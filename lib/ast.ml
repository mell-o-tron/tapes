type iden = string [@@deriving show]


type exprtype = TermType | TapeType

type expr = Var of iden | Tape of Tapes.tape | Term of Terms.term
type command = Draw of expr * string | Check of expr
type decl = ExprDecl of iden * exprtype * expr | SortDecl of iden
type program = Comm of command | Decl of decl | Seq of program * program



(*******************)


