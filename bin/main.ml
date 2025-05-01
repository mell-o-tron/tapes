open Ssr_typechecker.Ast
open Ssr_typechecker.Typecheck
open Ssr_typechecker.Term_to_tape
open Ssr_typechecker.Tapes

(*open Js_of_ocaml*)

  
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
let main () = match ast_of_channel inchn with
  | _definded_sorts, Some ast, None ->
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
;;


main()
