type iden = string [@@deriving show]
type sort = string [@@deriving show]

type exprtype =
  | TermType
  | TapeType

type expr =
  | Var of iden
  | Tape of Tapes.tape
  | Term of Terms.term

type command =
  | Draw of expr * string
  | Check of expr

type decl =
  | ExprDecl of iden * exprtype * expr
  | SortDecl of iden
  | GenDecl of iden * Terms.obj * Terms.obj

type program =
  | Comm of command
  | Decl of decl
  | Set of iden * float
  | Seq of program * program

(*******************)
