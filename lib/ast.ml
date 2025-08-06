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
  | DrawMatrix of expr * string
  | DrawNF of expr * string
  | DrawTraceNF of expr * string
  | CheckInclusion of expr * expr
  | CheckInclusionInvariant of expr * expr * expr

type decl =
  | ExprDecl of iden * exprtype * expr
  | SortDecl of iden
  | GenDecl of iden * Terms.obj * Terms.obj * Terms.gen_kind

type program =
  | Comm of command
  | Decl of decl
  | Set of iden * float
  | Seq of program * program

(*******************)
