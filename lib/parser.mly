
%{
(*   open Ast (* Define the abstract syntax tree in Ast.ml *) *)
let remove_first_last s =
  let len = String.length s in
  if len <= 2 then
    ""  (* or raise an error if preferred *)
  else
    String.sub s 1 (len - 2)
%}

%token Id SwapTimes SwapPlus Otimes Oplus Ldistr Gen Zero One Split Cut Join Spawn Copy
%token LPAREN RPAREN LBRACKET RBRACKET COLON SEMICOLON COMMA EOF EQUALS Term Tape DOT Let Sort Draw Check To ToTape ARROW Set

%token <string> STRING QSTRING
%token <float> FLOAT

%left SEMICOLON
%left Tensor

%start <Ast.program> main
%%

main:
  | p=program EOF {p}

program:
  | c=command {Ast.Comm(c)}
  | d=decl {d}
  | s = setting {s}
  | p1=program DOT p2=program {Ast.Seq(p1, p2)}
  | program DOT error {raise (Errors.ParseError "further commands expected after \".\". There should be no dot at the end of a program")}

setting:
  | Set LPAREN s=STRING COLON f=FLOAT RPAREN {Ast.Set(s, f)}

command:
  | Draw e=expr To qs=QSTRING {Ast.Draw(e, remove_first_last qs)}
  | Check e=expr {Ast.Check(e)}
  | Draw expr error    {raise (Errors.ParseError "did not specify path of draw")}
  | Draw expr To error {raise (Errors.ParseError "did not specify path of draw")}
  | error {raise (Errors.ParseError "command expected")}

decl:
  | Let s=STRING COLON o1 = object_type ARROW o2 = object_type {Ast.Decl(Ast.GenDecl(s, o1, o2))}
  | Let s=STRING COLON Tape EQUALS e=tape_expr {Ast.Decl(Ast.ExprDecl(s, Ast.TapeType, e))}
  | Let s=STRING COLON Term EQUALS e=term_expr {Ast.Decl(Ast.ExprDecl(s, Ast.TermType, e))}
  | Let s=STRING COLON Sort {Ast.Decl(Ast.SortDecl(s))}
  | Let STRING COLON error {raise (Errors.ParseError "expression type (tape or term) expected")}
  | Let STRING EQUALS error {raise (Errors.ParseError "error in declaration. Maybe you forgot to specify the type of the expression")}
  | error {raise (Errors.ParseError "declaration expected")}

expr:
  | e = tape_expr {e}
  | e = term_expr {e}

tape_expr:
  | s=STRING {Ast.Var(s)}
  | t=tape {Ast.Tape(t)}
  | ToTape t=term {Ast.Tape(Term_to_tape._to_tape(t))}
  | error {raise (Errors.ParseError "tape expression expected")}

term_expr:
  | s=STRING {Ast.Var(s)}
  | t=term {Ast.Term(t)}
  | error {raise (Errors.ParseError "term expression expected")}

term:
  | Id LPAREN t1 = object_type RPAREN                                                                   {Terms.Id(Terms.obj_to_polynomial(t1)) }
  | SwapTimes LPAREN t1 = object_type COMMA t2 = object_type RPAREN                                     {Terms.SwapTimes(Terms.obj_to_polynomial(t1), Terms.obj_to_polynomial(t2)) }
  | SwapPlus LPAREN t1 = object_type COMMA t2 = object_type RPAREN                                      {Terms.SwapPlus(Terms.obj_to_polynomial(t1), Terms.obj_to_polynomial(t2)) }
  | Gen LPAREN s = STRING COMMA t1 = object_type COMMA t2 = object_type RPAREN                          {Terms.Gen(s, Terms.obj_to_polynomial(t1), Terms.obj_to_polynomial(t2))}
  | Ldistr LPAREN t1 = object_type COMMA t2 = object_type COMMA t3 = object_type RPAREN                 {Terms.Ldistr(Terms.obj_to_polynomial(t1), Terms.obj_to_polynomial(t2), Terms.obj_to_polynomial(t3))}
  | t1 = term Otimes t2 = term                                                                          {Terms.Otimes(t1, t2)}
  | t1 = term Oplus t2 = term                                                                           {Terms.Oplus(t1, t2)}
  | t1 = term SEMICOLON t2 = term                                                                       {Terms.Compose(t1, t2)}
  | Cut LPAREN t = object_type RPAREN                                                                   { Terms.Cut (Terms.obj_to_polynomial t)}
  | Split LPAREN t = object_type RPAREN                                                                 { Terms.Split (Terms.obj_to_polynomial t)}
  | Spawn LPAREN t = object_type RPAREN                                                                 { Terms.Spawn (Terms.obj_to_polynomial t)}
  | Join LPAREN t = object_type RPAREN                                                                  { Terms.Join (Terms.obj_to_polynomial t)}
  | Copy LPAREN t = object_type RPAREN                                                                  { Terms.Copy (Terms.obj_to_polynomial t)}
  | LPAREN t = term RPAREN                                                                              {t}
  | s = STRING {Terms.GenVar(s)}
  | error {raise (Errors.ParseError "term expected")}

circuit:
  | One                                                                                                 { Tapes.CId1 }
  | Id LPAREN s = STRING RPAREN                                                                         { Tapes.CId (s) }
  | Gen LPAREN s = STRING COMMA t1 = object_type COMMA t2 = object_type RPAREN                          { Tapes.Gen  (s, Terms.sort_prod_to_list t1, Terms.sort_prod_to_list t2) }
  | SwapTimes LPAREN s1 = STRING COMMA s2 = STRING RPAREN                                               { Tapes.SwapTimes (s1, s2) }
  | c1 = circuit Otimes c2 = circuit                                                                    { Tapes.Otimes    (c1, c2) }
  | c1 = circuit SEMICOLON c2 = circuit                                                                 { Tapes.CCompose (c1, c2) }
  | LPAREN c = circuit RPAREN                                                                           { c }
  | error {raise (Errors.ParseError "circuit expected")}

tape:
  | Zero                                                                                                { Tapes.TId0 }
  | Id LPAREN t = object_type RPAREN                                                                    { Tapes.TId      (Terms.obj_to_polynomial t) }
  | Cut LPAREN t = object_type RPAREN                                                                   { Tapes.Cut (Terms.obj_to_monomial t)}
  | Split LPAREN t = object_type RPAREN                                                                 { Tapes.Split (Terms.obj_to_monomial t)}
  | Spawn LPAREN t = object_type RPAREN                                                                 { Tapes.Spawn (Terms.obj_to_monomial t)}
  | Join LPAREN t = object_type RPAREN                                                                  { Tapes.Join (Terms.obj_to_monomial t)}
  | LBRACKET c = circuit RBRACKET                                                                       { Tapes.Tape     c }
  | t1 = tape Oplus t2 = tape                                                                           { Tapes.Oplus    (t1, t2) }
  | SwapPlus LPAREN t1 = object_type COMMA t2 = object_type RPAREN                                      { Tapes.SwapPlus (Terms.sort_prod_to_list t1, Terms.sort_prod_to_list t2) }
  | t1 = tape SEMICOLON t2 = tape                                                                       { Tapes.TCompose(t1, t2) }
  | LPAREN t = tape RPAREN
  { t }
  | error {raise (Errors.ParseError "tape expected")}

object_type:
  | LPAREN o = object_type RPAREN                                 {o}
  | s = STRING                                                    {(Terms.S(s))}
  | o1 = object_type Otimes o2 = object_type                      {(Terms.Obtimes(o1, o2))}
  | o1 = object_type Oplus o2 = object_type                       {(Terms.Obplus(o1, o2))}
  | Zero                                                          {(Terms.Ob0)}
  | One                                                           {(Terms.Ob1)}
  | error {raise (Errors.ParseError "object type expected")}


/*type_list_list:
  | LBRACKET RBRACKET                                             {[]}
  | LBRACKET l = type_list_list_inner RBRACKET                         {l}

type_list_list_inner:
  | l = type_list                                                    {[l]}
  | l1 = type_list COMMA l2 = type_list_list_inner                         {l1 :: l2}
  
type_list:
  | LBRACKET RBRACKET                                             {[]}
  | LBRACKET l = type_list_inner RBRACKET                         {l}
  
type_list_inner:
  | s = STRING                                                    {[s]}
  | s = STRING COMMA t = type_list_inner                                {s ::t}*/
