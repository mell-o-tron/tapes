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

let rec typecheck_command (e : expr) = match e with
  | Tape (t) -> printf [] "Tape typecheck result:\t%s\n" (if tape_typecheck t then (sprintf [green] "true ✅") else (sprintf [red; Bold] ("false ❌")))
  | Term (t) -> printf [] "Term typecheck result:\t%s\n" (if typecheck t then (sprintf [green] "true ✅") else (sprintf [red; Bold] "false ❌"))
  | Var (id) -> if Hashtbl.mem env id then typecheck_command (Hashtbl.find env id) else raise (RuntimeError (Printf.sprintf "Variable %s not found" id))

let rec draw_command (e : expr) (path : string) = (match e with
  | Tape (t) -> let tc = tape_typecheck t in if tc then Ssr_typechecker.Draw.draw_tape ((clean_tape (t))) path else (raise (RuntimeError "Cannot draw tape: does not typecheck."))
  | Term (t) -> let tc = typecheck t in if tc then Ssr_typechecker.Draw.draw_tape ((clean_tape (_to_tape t))) path else (raise (RuntimeError "Cannot draw term: does not typecheck."))
  | Var (id) -> if Hashtbl.mem env id then draw_command (Hashtbl.find env id) path else raise (RuntimeError (Printf.sprintf "Variable %s not found" id)))



let rec exec (p : program) = match p with
  | Comm (c) -> (match c with
      | Check e -> typecheck_command e
      | Draw (e, path) -> draw_command e path
  )
  | Decl (d) -> (match d with
      | ExprDecl (id, _typ, e) -> Hashtbl.add env id (e)
      | SortDecl id -> sorts := (id :: !sorts)
  )
  | Seq(p1, p2) -> (exec p1) ; (exec p2)


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
