
(* The type of tokens. *)

type token = 
  | Zero
  | SwapTimes
  | SwapPlus
  | STRING of (string)
  | RPAREN
  | RBRACKET
  | Otimes
  | Oplus
  | One
  | Ldistr
  | LPAREN
  | LBRACKET
  | Id
  | Gen
  | EOF
  | Compose
  | COMMA
  | COLON

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.term)
