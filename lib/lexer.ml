open Parser

let digit = [%sedlex.regexp? '0' .. '9']
let hexNumber = [%sedlex.regexp? Plus hex_digit]
let number = [%sedlex.regexp? Plus digit | "0x", hexNumber]
let character = [%sedlex.regexp? 0x20 .. 0x7E]
let quoted_string = [%sedlex.regexp? '"', Star (Compl (Chars "\"")), '"']
let variable_name = [%sedlex.regexp? ('a'..'z' | 'A'..'Z' | '_'), Star ('a'..'z' | 'A'..'Z' | '0'..'9' | '_')]

let rec token lexbuf =
  match%sedlex lexbuf with
  | "id"               -> Id
  | "s*" | "σ⊗"       -> SwapTimes
  | "s+" | "σ⊕"       -> SwapPlus
  | "*"  | "⊗"        -> Otimes
  | "+"  | "⊕"        -> Oplus
  | "dl" | "δl"        -> Ldistr
  | "gen"             -> Gen
  | "0"               -> Zero
  | "1"               -> One
  | "(" -> LPAREN
  | ")" -> RPAREN
  | "[" -> LBRACKET
  | "]" -> RBRACKET
  | ";" -> SEMICOLON
  | ":" -> COLON
  | "=" -> EQUALS
  | "." -> DOT
  | "sort" -> Sort
  | "let" -> Let
  | "term" -> Term
  | "tape" -> Tape
  | "draw" -> Draw
  | "check" -> Check
  | "to" -> To
  | white_space -> token lexbuf
  | "," -> COMMA
(*  | number -> INT (int_of_string (Sedlexing.Latin1.lexeme lexbuf))*)
  | variable_name ->
      let lexeme = Sedlexing.Latin1.lexeme lexbuf in STRING lexeme
  | quoted_string ->
      let lexeme = Sedlexing.Latin1.lexeme lexbuf in QSTRING lexeme
  | "//" -> comment lexbuf
  | eof -> EOF
  | any ->
      failwith
        (Printf.sprintf "Unrecognised character: \'%s\'"
           (Sedlexing.Latin1.lexeme lexbuf))
  | _ -> failwith "Impossible!"

and comment lexbuf =
  match%sedlex lexbuf with
  | "**" -> token lexbuf
  | "\n" -> token lexbuf
  | eof -> EOF
  | any -> comment lexbuf
  | _ -> failwith "Impossible!"


let tokenize (lexbuf : Sedlexing.lexbuf) = token lexbuf
