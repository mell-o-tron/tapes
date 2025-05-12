(* open Ssr_typechecker.Terms *)
open Ssr_typechecker.Typecheck
open Ssr_typechecker.Term_to_tape
open Ssr_typechecker.Tapes
open Ssr_typechecker.Ast
open Ssr_typechecker.Errors
open ANSITerminal

(*open Js_of_ocaml*)


let inchn = open_in Sys.argv.(1)

let ast_of_channel inchn =
  let lexbuf = Sedlexing.Latin1.from_channel inchn in
  let lexer = Sedlexing.with_tokenizer Ssr_typechecker.Lexer.token lexbuf in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Ssr_typechecker.Parser.main
  in
  try parser lexer
  with
    | ParseError s -> raise
      (Syntax_error
         ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
           (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
           - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol,
           s ))
    | Ssr_typechecker.Parser.Error ->
    raise
      (Syntax_error
         ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
           (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
           - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol,
           "Generic Syntax Error" ))


let sorts = ref []
let env = Hashtbl.create 10
let gens = Hashtbl.create 10
let settings = Hashtbl.create 10

let rec typecheck_command (e : expr) = match e with
  | Tape (t) -> printf [] "Tape typecheck result:\t%s\n" (if tape_typecheck t then (sprintf [green] "true ✅") else (sprintf [red; Bold] ("false ❌")))
  | Term (t) -> printf [] "Term typecheck result:\t%s\n" (if typecheck t then (sprintf [green] "true ✅") else (sprintf [red; Bold] "false ❌"))
  | Var (id) -> if Hashtbl.mem env id then typecheck_command (Hashtbl.find env id) else raise (RuntimeError (Printf.sprintf "Variable %s not found" id))

let rec draw_command (e : expr) (path : string) = (match e with
  | Tape (t) -> let tc = tape_typecheck t in if tc then Ssr_typechecker.Draw.draw_tape ((deep_clean_tape (t))) path else (raise (RuntimeError "Cannot draw tape: does not typecheck."))
  | Term (t) -> let tc = typecheck t in if tc then Ssr_typechecker.Draw.draw_tape ((deep_clean_tape (_to_tape t))) path else (raise (RuntimeError "Cannot draw term: does not typecheck."))
  | Var (id) -> if Hashtbl.mem env id then draw_command (Hashtbl.find env id) path else raise (RuntimeError (Printf.sprintf "Variable %s not found" id)))

let rec subst_gen_name_term (v : string) (t : Ssr_typechecker.Terms.term) : Ssr_typechecker.Terms.term = match t with
  | GenVar v -> if Hashtbl.mem gens v then Hashtbl.find gens v else raise (RuntimeError (Printf.sprintf "generator %s has not been defined prior to its use" v)) 
  | Otimes (t1, t2)       -> Otimes(subst_gen_name_term v t1, subst_gen_name_term v t2)
  | Oplus (t1, t2)      -> Oplus(subst_gen_name_term v t1, subst_gen_name_term v t2)
  | Compose (t1, t2)    -> Compose(subst_gen_name_term v t1, subst_gen_name_term v t2)
  | _ -> t

let subst_gen_name (v : string) (e : expr) = match e with
  | Term t -> Term (subst_gen_name_term v t)
  | _ -> e

let populate_genvars (e : expr) = Hashtbl.fold (fun k _ e1 -> subst_gen_name k e1) gens e 

let rec exec (p : program) = match p with
  | Comm (c) -> (match c with
      | Check e -> typecheck_command (populate_genvars e)
      | Draw (e, path) -> draw_command (populate_genvars e) path
  )
  | Decl (d) -> (match d with
      | ExprDecl (id, _typ, e) -> Hashtbl.add env id (populate_genvars e)
      | SortDecl id -> sorts := (id :: !sorts)
      | GenDecl (id, ob1, ob2) -> Hashtbl.add gens id (Gen (id, Ssr_typechecker.Terms.obj_to_polynomial ob1, Ssr_typechecker.Terms.obj_to_polynomial ob2))
  )
  | Seq(p1, p2) -> (exec p1) ; (exec p2)
  | Set (id, f) -> Hashtbl.add settings id f ; match id with 
      | "otimesdist" -> Ssr_typechecker.Draw.otimes_dist := f
      | "oplusdist" -> Ssr_typechecker.Draw.oplus_dist := f
      | "paddingdist" -> Ssr_typechecker.Draw.tape_padding := f
      | _ -> ()


(* prints the result of the computation *)

let main () = try let p = ast_of_channel inchn in exec (p)
with
  | TypeError s -> printf [Bold; red] "Type Error: %s.\n" s
  | RuntimeError s -> printf [Bold; red] "Runtime Error: %s.\n" s
  | Syntax_error (a, b, s) -> printf [Bold; red] "Syntax Error at (%d, %d): %s.\n" a b s

  ;;
(*  | _definded_sorts, Some ast, None ->
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
  | _definded_sorts, None, Some tape ->
    let ar = tape_arity tape in
    let coar = tape_coarity tape in
    print_string "Arity:\t\t"; _print_type( ar ) ;
    print_string "Coarity:\t"; _print_type( coar ) ;

    let tc = tape_typecheck tape in
    print_endline ("Result of typecheck: " ^ (string_of_bool (tc)) ^ if tc then " ✅" else " ❌");

    if tc then Ssr_typechecker.Draw.draw_tape ((clean_tape (tape)))
    else ();

    ; print_endline "\n"
  | _ -> failwith "CULO"
;;*)


main()
