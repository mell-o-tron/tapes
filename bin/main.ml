open Ssr_typechecker.Ast
open Ssr_typechecker.Typecheck
(*open Js_of_ocaml*)

  
  
(* Given a list of circuits, forms a polynomial with those circuits as monomials *)
let sum_monomials (l) = List.fold_left (fun acc m -> Ssr_typechecker.Tapes.Oplus(acc, m)) (TId0) l
    
(* Turns a string diagram identity into a circuit identity *)
let id_to_circuit l = List.fold_left (fun acc s -> Ssr_typechecker.Tapes.Otimes(acc, CId(s))) (CId1) l
    
let rec clean_circuit (c : Ssr_typechecker.Tapes.circuit) = match c with
    | CCompose (c1, c2) -> Ssr_typechecker.Tapes.CCompose (clean_circuit c1, clean_circuit c2)
    | Otimes (c1, CId1) -> clean_circuit c1
    | Otimes (CId1, c2) -> clean_circuit c2
    | Otimes (c1, c2)   -> Otimes (clean_circuit c1, clean_circuit c2)
    | _ -> c
  
let rec clean_tape (t : Ssr_typechecker.Tapes.tape) = match t with
    | TCompose (t1, t2) -> Ssr_typechecker.Tapes.TCompose (clean_tape t1, clean_tape t2)
    | Oplus (t1, TId0)  -> clean_tape t1
    | Oplus (TId0, t2)  -> clean_tape t2
    | Oplus (t1, t2)    -> Oplus (clean_tape t1, clean_tape t2)
    | Tape (c)          -> Tape(clean_circuit c)
    | _ -> t
    
let rec unwrap_swaptimes_circuit (l1 : sort list) (l2 : sort list) = match (l1, l2) with
  | [], u       -> id_to_circuit (u)
  | w, []       -> id_to_circuit (w)
  | a::w, b::u  -> CCompose(
                    CCompose(
                      Ssr_typechecker.Tapes.Otimes(
                        CId(a), 
                        unwrap_swaptimes_circuit(w) (b::u)),
                      Ssr_typechecker.Tapes.Otimes(
                        Ssr_typechecker.Tapes.SwapTimes(a, b), 
                        id_to_circuit(u@w))), 
                      Ssr_typechecker.Tapes.Otimes(
                        Ssr_typechecker.Tapes.Otimes(
                          CId(b), 
                          unwrap_swaptimes_circuit([a]) (u)), 
                        id_to_circuit(w)))

(* Turns an identity into the corresponding tape *)
let id_to_tape (l1 : sort list list) = List.fold_left (fun acc m -> Ssr_typechecker.Tapes.Oplus (acc, Tape(id_to_circuit m))) (TId0) l1                         

let rec unwrap_swapplus_tape (l1 : sort list list) (l2 : sort list list) = match (l1, l2) with
  | [], u       -> id_to_tape (u)
  | w, []       -> id_to_tape (w)
  | a::w, b::u  -> TCompose(
                    TCompose(
                      Ssr_typechecker.Tapes.Oplus(
                        TId([a]), 
                        unwrap_swapplus_tape(w) (b::u)),
                      Ssr_typechecker.Tapes.Oplus(
                        Ssr_typechecker.Tapes.SwapPlus(a, b), 
                        id_to_tape(u@w))), 
                      Ssr_typechecker.Tapes.Oplus(
                        Ssr_typechecker.Tapes.Oplus(
                          TId([b]), 
                          unwrap_swapplus_tape([a]) (u)), 
                        id_to_tape(w)))

    
let swapplus_to_tape (p : sort list list) (q : sort list list) = unwrap_swapplus_tape p q


let rec ldistr_to_tape (p : sort list list) (q : sort list list) (r : sort list list) = match p with
  | []      -> Ssr_typechecker.Tapes.TId0
  | u :: p1 -> TCompose (
      Ssr_typechecker.Tapes.Oplus (
        id_to_tape (obj_to_polynomial (Obtimes( obj_of_polynomial ([u]), Obplus (obj_of_polynomial (q), obj_of_polynomial (r))))),
        ldistr_to_tape p1 q r
      ),
      
      Ssr_typechecker.Tapes.Oplus (
        Ssr_typechecker.Tapes.Oplus (
          id_to_tape (obj_to_polynomial (Obtimes (obj_of_polynomial ([u]), obj_of_polynomial (q)))),
          swapplus_to_tape (obj_to_polynomial(Obtimes (obj_of_polynomial ([u]), obj_of_polynomial (r)))) (obj_to_polynomial(Obtimes (obj_of_polynomial (p1), obj_of_polynomial (q))))
        ),
        id_to_tape (obj_to_polynomial (Obtimes (obj_of_polynomial (p1), obj_of_polynomial (r))))
      )
  )

let rec swaptimes_to_tape (p : sort list list) (q : sort list list) = match q with
  | []      ->  Ssr_typechecker.Tapes.TId0
  | m :: q1  -> let sum_of_swaps = sum_monomials (List.map(fun ui ->  Ssr_typechecker.Tapes.Tape(unwrap_swaptimes_circuit ui m)) p)
                  in TCompose(ldistr_to_tape p [m] q1, Ssr_typechecker.Tapes.Oplus (sum_of_swaps, swaptimes_to_tape p q1))
  
  

let otimes_to_tape (_t : term) = failwith("otimes_to_tape not yet implemented")

(* converts term into tape (when possible) *)
let rec _to_tape (t : term) = match t with
  | Id (l)              -> id_to_tape (l)
  | SwapTimes (p, q)    -> swaptimes_to_tape p q
  | SwapPlus (p, q)     -> swapplus_to_tape p q
  | Ldistr (p, q, r)    -> ldistr_to_tape p q r
  | Otimes (_, _)       -> otimes_to_tape (t)
  | Oplus (t1, t2)      -> Ssr_typechecker.Tapes.Oplus(_to_tape(t1), _to_tape(t2))
  | Compose (t1, t2)    -> Ssr_typechecker.Tapes.TCompose(_to_tape(t1), _to_tape(t2))
  | Gen (_, _, _)       -> failwith("cannot convert SSR generator to tape")

  
let inchn = open_in Sys.argv.(1)

let ast_of_channel inchn =
  let lexbuf = Sedlexing.Latin1.from_channel inchn in
  let lexer = Sedlexing.with_tokenizer Ssr_typechecker.Lexer.token lexbuf in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Ssr_typechecker.Parser.main
  in
  try parser lexer
  with Ssr_typechecker.Parser.Error ->
    raise
      (Syntax_error
         ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
           (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
           - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol,
           "Syntax error" ))
           

(* prints the result of the computation *)
let main () = let ast =  ast_of_channel inchn in
  let ar = arity ast in 
  let coar = coarity ast in
  print_string "Arity:\t\t"; _print_type( ar ) ;
  print_string "Coarity:\t"; _print_type( coar ) ;
  
  print_string "To Tape:\t";print_endline (Ssr_typechecker.Tapes.pp_tape (clean_tape (_to_tape ast))) ;

  let tc = typecheck ast in
  print_endline ("Result of typecheck: " ^ (string_of_bool (tc)) ^ if tc then " ✅" else " ❌");
  
  if tc then Ssr_typechecker.Draw.draw_tape ((clean_tape (_to_tape ast)))
  else ();
  
  ; print_endline "\n"
;;


main()
