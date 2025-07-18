open Parser

let digit = [%sedlex.regexp? '0' .. '9']
let hexNumber = [%sedlex.regexp? Plus hex_digit]
let number = [%sedlex.regexp? Plus digit]
let floatnum = [%sedlex.regexp? Plus digit, '.', Plus digit]
let character = [%sedlex.regexp? 0x20 .. 0x7E]
let quoted_string = [%sedlex.regexp? '"', Star (Compl (Chars "\"")), '"']

let variable_name =
  [%sedlex.regexp?
    ( ('a' .. 'z' | 'A' .. 'Z' | '_'),
      Star ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') )]

let rec token lexbuf =
  match%sedlex lexbuf with
  | "id" -> Id
  | "s*" | "σ⊗" -> SwapTimes
  | "s+" | "σ⊕" -> SwapPlus
  | "*" | "⊗" -> Otimes
  | "+" | "⊕" -> Oplus
  | "dl" | "δl" -> Ldistr
  | "gen" -> Gen
  | "0" -> Zero
  | "1" -> One
  | "(" -> LPAREN
  | ")" -> RPAREN
  | "[" -> LBRACKET
  | "]" -> RBRACKET
  | ";" -> SEMICOLON
  | ":" -> COLON
  | "=" -> EQUALS
  | "." -> DOT
  | "->" -> ARROW
  | "sort" -> Sort
  | "let" -> Let
  | "term" -> Term
  | "tape" -> Tape
  | "trace" -> Trace
  | "draw" -> Draw
  | "draw_matrix" -> DrawMatrix
  | "check" -> Check
  | "to" -> To
  | "to_tape" -> ToTape
  | "split" -> Split
  | "cut" -> Cut
  | "join" -> Join
  | "spawn" -> Spawn
  | "copy" -> Copy
  | "multicopy" -> MultiCopy
  | "cocopy" -> CoCopy
  | "discard" -> Discard
  | "codiscard" -> CoDiscard
  | "set" -> Set
  | "BEGIN_IMP" -> BEGIN_IMP
  | "END_IMP" -> END_IMP
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "while" -> WHILE
  | "do" -> DO
  | "skip" -> SKIP
  | "abort" -> ABORT
  | ":=" -> ASSIGN
  | "and" -> AND
  | "or" -> OR
  | "not" -> NOT
  | "true" -> TRUE
  | "false" -> FALSE
  | "{" -> OPEN_BRACE
  | "}" -> CLOSED_BRACE
  | "path" -> PATH
  | white_space -> token lexbuf
  | "," -> COMMA
  (*  | number -> INT (int_of_string (Sedlexing.Latin1.lexeme lexbuf))*)
  | variable_name ->
      let lexeme = Sedlexing.Latin1.lexeme lexbuf in
      STRING lexeme
  | quoted_string ->
      let lexeme = Sedlexing.Latin1.lexeme lexbuf in
      QSTRING lexeme
  | floatnum ->
      let lexeme = Sedlexing.Latin1.lexeme lexbuf in
      FLOAT (float_of_string lexeme)
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
