
%{
(*   open Ast (* Define the abstract syntax tree in Ast.ml *) *)
open Matrix
open Term_to_tape

let remove_first_last s =
  let len = String.length s in
  if len <= 2 then
    ""  (* or raise an error if preferred *)
  else
    String.sub s 1 (len - 2)
%}

%token Id SwapTimes SwapPlus Otimes Oplus Ldistr Gen Fun Zero One Split Cut Join Spawn Copy MultiCopy CoCopy Discard CoDiscard
%token LPAREN RPAREN LBRACKET RBRACKET COLON SEMICOLON COMMA EOF EQUALS Term Tape Trace DOT Let Sort Draw Check DrawMatrix DrawNF DrawTraceNF To ToTape ARROW Set REF
%token BEGIN_IMP END_IMP IF THEN ELSE WHILE DO SKIP ABORT ASSIGN AND OR NOT TRUE FALSE OPEN_BRACE CLOSED_BRACE PATH NORMALIZE NORMALIZETERM NORMALIZETRACE CHECKINCLUSION WITH INVARIANT BEGIN_TEST END_TEST
%token AXIOMS FORALL EXISTS IMPLIES IFF DELETEPATH REMEMPTIES DrawCospan OfMonomial DrawCircuit
%token UNION INTERSECTION OP STAR EMPTY TOP OfRelation

%token <string> STRING QSTRING
%token <float> FLOAT

%left SEMICOLON
%left Oplus
%left Otimes
%left DOT

%left OR
%left AND
%nonassoc NOT

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
  | DrawMatrix e=expr To qs=QSTRING {Ast.DrawMatrix(e, remove_first_last qs)}
  | DrawNF e=expr To qs=QSTRING {Ast.DrawNF(e, remove_first_last qs)}
  | DrawTraceNF e=expr To qs=QSTRING {Ast.DrawTraceNF(e, remove_first_last qs)}
  | CHECKINCLUSION LPAREN e1=expr COMMA e2=expr RPAREN WITH INVARIANT e3=expr {Ast.CheckInclusionInvariant(e1, e2, e3)}
  | CHECKINCLUSION LPAREN e1=expr COMMA e2=expr RPAREN {Ast.CheckInclusion(e1, e2)}
  | AXIOMS LBRACKET fl=separated_nonempty_list(COMMA, fol_formula) RBRACKET {Ast.SetAxioms(fl)}
  | DrawCospan c=circuit To qs=QSTRING {Ast.DrawCospan(c, remove_first_last qs)}
  | DrawCircuit c=circuit To qs=QSTRING {Ast.DrawCircuit(c, remove_first_last qs)}
  | Draw expr error    {raise (Errors.ParseError "did not specify path of draw")}
  | Draw expr To error {raise (Errors.ParseError "did not specify path of draw")}

decl:
  | Let s=STRING COLON o1 = object_type ARROW o2 = object_type {Ast.Decl(Ast.GenDecl(s, o1, o2, Relation))} (* TODO add functions et al *)
  | Let s=STRING COLON Tape EQUALS e=tape_expr {Ast.Decl(Ast.ExprDecl(s, Ast.TapeType, e))}
  | Let s=STRING COLON Term EQUALS e=term_expr {Ast.Decl(Ast.ExprDecl(s, Ast.TermType, e))}
  | Let s=STRING COLON Sort {Ast.Decl(Ast.SortDecl(s))}
  | Let STRING COLON error {raise (Errors.ParseError "expression type (tape or term) expected")}
  | Let STRING EQUALS error {raise (Errors.ParseError "error in declaration. Maybe you forgot to specify the type of the expression")}

expr:
  | e = tape_expr {e}
  | e = term_expr {e}

tape_expr:
  | s=STRING {Ast.Var(s)}
  | t=tape {Ast.Tape(t)}
  | ToTape t=term {Ast.Tape(Term_to_tape._to_tape(t))}
  | error {raise (Errors.ParseError "tape expression expected")}

term_expr:
  | t=term {Ast.Term(t)}
  | error {raise (Errors.ParseError "term expression expected")}

term:
  | Id LPAREN t1 = object_type RPAREN                                                                   {Terms.Id(Terms.obj_to_polynomial(t1)) }
  | SwapTimes LPAREN t1 = object_type COMMA t2 = object_type RPAREN                                     {Terms.SwapTimes(Terms.obj_to_polynomial(t1), Terms.obj_to_polynomial(t2)) }
  | SwapPlus LPAREN t1 = object_type COMMA t2 = object_type RPAREN                                      {Terms.SwapPlus(Terms.obj_to_polynomial(t1), Terms.obj_to_polynomial(t2)) }
  | Gen LPAREN s = STRING COMMA t1 = object_type COMMA t2 = object_type RPAREN                          {Terms.Gen(s, Terms.obj_to_polynomial(t1), Terms.obj_to_polynomial(t2), Terms.Relation)}
  | Fun LPAREN s = STRING COMMA t1 = object_type COMMA t2 = object_type RPAREN                          {Terms.Gen(s, Terms.obj_to_polynomial(t1), Terms.obj_to_polynomial(t2), Terms.Function)}
  | Ldistr LPAREN t1 = object_type COMMA t2 = object_type COMMA t3 = object_type RPAREN                 {Terms.Ldistr(Terms.obj_to_polynomial(t1), Terms.obj_to_polynomial(t2), Terms.obj_to_polynomial(t3))}
  | t1 = term Otimes t2 = term                                                                          {Terms.Otimes(t1, t2)}
  | t1 = term Oplus t2 = term                                                                           {Terms.Oplus(t1, t2)}
  | t1 = term SEMICOLON t2 = term                                                                       {Terms.Compose(t1, t2)}
  | Cut LPAREN t = object_type RPAREN                                                                   { Terms.Cut (Terms.obj_to_polynomial t)}
  | Split LPAREN t = object_type RPAREN                                                                 { Terms.Split (Terms.obj_to_polynomial t)}
  | Spawn LPAREN t = object_type RPAREN                                                                 { Terms.Spawn (Terms.obj_to_polynomial t)}
  | Join LPAREN t = object_type RPAREN                                                                  { Terms.Join (Terms.obj_to_polynomial t)}
  | Copy LPAREN t = object_type RPAREN                                                                  { Terms.Copy (Terms.obj_to_polynomial t)}
  | MultiCopy LPAREN n = FLOAT COMMA t = object_type RPAREN                                             { Terms.multi_copy (int_of_float n) (Terms.obj_to_polynomial t)}
  | CoCopy LPAREN t = object_type RPAREN                                                                { Terms.CoCopy (Terms.obj_to_polynomial t)}
  | Discard LPAREN t = object_type RPAREN                                                               { Terms.Discard (Terms.obj_to_polynomial t)}
  | CoDiscard LPAREN t = object_type RPAREN                                                             { Terms.CoDiscard (Terms.obj_to_polynomial t)}
  | Trace LPAREN l = object_type COMMA t = term RPAREN                                                  { Terms.Trace(Terms.obj_to_polynomial l, t) }
  | LPAREN t = term RPAREN                                                                              {t}
  | s = STRING {Terms.GenVar(s)}
  | BEGIN_IMP LBRACKET RBRACKET i = imp_command END_IMP {Imp.eval_command [] i}
  | BEGIN_IMP LBRACKET ctx=context RBRACKET i = imp_command END_IMP {let x = Imp.eval_command ctx i in (*print_endline (Tapes.pp_tape(Term_to_tape._to_tape x )) ;*) x }
  | BEGIN_TEST LBRACKET RBRACKET p = imp_pred END_TEST {Imp.corefl [] p}
  | BEGIN_TEST LBRACKET ctx=context RBRACKET p = imp_pred END_TEST {Imp.corefl ctx p}
  | NORMALIZETRACE t1 = term {let res = Rewrite.trace_normal_form t1 in Printf.printf "term: %s\n" (Terms.show_term res); res}
  | OfRelation r = relation {Relations.term_of_rel r}
  | error {raise (Errors.ParseError "term expected")}

context:
  | x = STRING COLON s = STRING {[(x, s)]}
  | x = STRING COLON s = STRING COMMA a1 = context {(x, s) :: a1}

circuit:
  | One                                                                                                 { Tapes.CId1 }
  | Id LPAREN s = STRING RPAREN                                                                         { Tapes.CId (s) }
  | Gen LPAREN s = STRING COMMA t1 = object_type COMMA t2 = object_type RPAREN                          { Tapes.Gen  (s, Terms.sort_prod_to_list t1, Terms.sort_prod_to_list t2, Terms.Relation) }
  | Fun LPAREN s = STRING COMMA t1 = object_type COMMA t2 = object_type RPAREN                          { Tapes.Gen  (s, Terms.sort_prod_to_list t1, Terms.sort_prod_to_list t2, Terms.Function) }
  | SwapTimes LPAREN s1 = STRING COMMA s2 = STRING RPAREN                                               { Tapes.SwapTimes (s1, s2) }
  | c1 = circuit Otimes c2 = circuit                                                                    { Tapes.Otimes    (c1, c2) }
  | c1 = circuit SEMICOLON c2 = circuit                                                                 { Tapes.CCompose (c1, c2) }
  | Copy LPAREN s = STRING RPAREN {Tapes.Gen("copy", [s], [s;s], Terms.Relation)}
  | CoCopy LPAREN s = STRING RPAREN {Tapes.Gen("cocopy", [s;s], [s], Terms.Relation)}
  | Discard LPAREN s = STRING RPAREN {Tapes.Gen("discard", [s], [], Terms.Relation)}
  | CoDiscard LPAREN s = STRING RPAREN {Tapes.Gen("codiscard", [], [s], Terms.Relation)}
  | LPAREN c = circuit RPAREN                                                                           { c }
  | OfMonomial LPAREN t = tape RPAREN {Fol_encoding.circuit_of_monomial (Tapes.deep_clean_tape t)}
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
  | Trace LPAREN l = object_type COMMA t = tape RPAREN                                                  { Tapes.Trace(Terms.obj_to_monomial l, t) }
  | PATH LPAREN f=FLOAT COMMA t = term RPAREN LBRACKET f1=FLOAT RBRACKET                                { snd (List.nth (linearize (int_of_float f) (_to_tape t)) (int_of_float f1))}
  | LPAREN t = tape RPAREN { t }
  | NORMALIZE t1 = tape {Matrix.normalize (t1)}
  | NORMALIZETERM t1 = term {Matrix.term_to_normalized_tape t1}
  | DELETEPATH idx = FLOAT c1 = circuit 
    {let elimd = fst (Rewrite.eliminate_path_from_left (int_of_float idx) c1) |> Tapes.deep_clean_circuit
     in Printf.printf "taped elimd: %s\n" (Tapes.pp_tape (Tapes.Tape(elimd))); Tapes.Tape(elimd)}
  // | REMEMPTIES LPAREN t1 = tape COMMA ob = object_type RPAREN 
  //   {Rewrite.eliminate_empty_paths_tape (Terms.obj_to_monomial ob) (t1|> Rewrite.merge_embedded_circuits |> Tapes.deep_clean_tape
  //   |> Rewrite.reduce_circuits_tape)}
  | ToTape LPAREN t = term RPAREN {(Term_to_tape._to_tape(t))}
  | error {raise (Errors.ParseError "tape expected")}

object_type:
  | LPAREN o = object_type RPAREN                                 {o}
  | s = STRING                                                    {(Terms.S(s))}
  | o1 = object_type Otimes o2 = object_type                      {(Terms.Obtimes(o1, o2))}
  | o1 = object_type Oplus o2 = object_type                       {(Terms.Obplus(o1, o2))}
  | Zero                                                          {(Terms.Ob0)}
  | One                                                           {(Terms.Ob1)}
  | error {raise (Errors.ParseError "object type expected")}

imp_expr:
  | s = STRING {Imp.Var s}
  | f = STRING LPAREN RPAREN COLON s = STRING {Imp.Func(f, [], s)}
  | f = STRING LPAREN a = args RPAREN COLON s = STRING {Imp.Func(f, a, s)}
  | f = QSTRING LPAREN RPAREN COLON s = STRING {Imp.Func(remove_first_last f, [], s)}
  | f = QSTRING LPAREN a = args RPAREN COLON s = STRING {Imp.Func(remove_first_last f, a, s)}
  | LPAREN e = imp_expr RPAREN {e}

imp_pred:
  | r = STRING LPAREN RPAREN {Imp.Rel(r, [], true)}
  | r = STRING LPAREN a = args RPAREN {Imp.Rel(r, a, true)}
  | r = QSTRING LPAREN RPAREN {Imp.Rel(remove_first_last r, [], true)}
  | r = QSTRING LPAREN a = args RPAREN {Imp.Rel(remove_first_last r, a, true)}
  | TRUE {Imp.Top}
  | FALSE {Imp.Bottom}
  | p1 = imp_pred OR p2 = imp_pred {Imp.Or(p1, p2)}
  | p1 = imp_pred AND p2 = imp_pred {Imp.And(p1, p2)}
  | NOT p1 = imp_pred {Imp.negate p1}
  | LPAREN p = imp_pred RPAREN {p}
  
args:
  | e = imp_expr {[e]}
  | e = imp_expr COMMA a1 = args {e :: a1}

imp_command:
  | ABORT {Imp.Abort}
  | SKIP {Imp.Skip}
  | IF p = imp_pred THEN OPEN_BRACE c1 = imp_command CLOSED_BRACE ELSE OPEN_BRACE c2 = imp_command CLOSED_BRACE {Imp.IfThenElse(p, c1, c2)}
  | WHILE p = imp_pred OPEN_BRACE c1 = imp_command CLOSED_BRACE {Imp.WhileDo (p, c1)}
  | c1 = imp_command SEMICOLON c2 = imp_command {Imp.Seq(c1, c2)}
  | x = STRING ASSIGN e = imp_expr {Imp.Assign(x, e)}

fol_formula:
  | TRUE {Fol_encoding.Top}
  | FALSE {Fol_encoding.Bot}
  | s=STRING LPAREN RPAREN {Fol_encoding.Lit(Pos(Pred(s, [])))}
  | s=STRING LPAREN l=fol_term_list RPAREN {Fol_encoding.Lit(Pos(Pred(s, l)))}
  | t1=fol_term EQUALS t2=fol_term {Fol_encoding.Lit(Pos(Equals(t1, t2)))}
  | f1=fol_formula OR f2=fol_formula {Fol_encoding.Or(f1, f2)}
  | f1=fol_formula AND f2=fol_formula {Fol_encoding.And(f1, f2)}
  | NOT f1=fol_formula {Fol_encoding.Not(f1)}
  | FORALL s=STRING DOT f=fol_formula {Fol_encoding.Forall((s, 0), f)}
  | EXISTS s=STRING DOT f=fol_formula {Fol_encoding.Exists((s, 0), f)}
  | f1=fol_formula IMPLIES f2=fol_formula {Fol_encoding.Or(Fol_encoding.Not(f1), f2)}
  | f1=fol_formula IFF f2=fol_formula {Fol_encoding.And(Fol_encoding.Or(Fol_encoding.Not(f1), f2),Fol_encoding.Or(Fol_encoding.Not(f2), f1))}
  | LPAREN f = fol_formula RPAREN {f}

fol_term:
  | s = STRING {Fol_encoding.Var (s, 0)}
  | s = STRING LPAREN  RPAREN {Fol_encoding.Func(s, [])}
  | s = STRING LPAREN l=fol_term_list RPAREN {Fol_encoding.Func(s, l)}
  | LPAREN t =fol_term RPAREN {t}

fol_term_list:
  | t = fol_term {[t]}
  | t = fol_term COMMA r = fol_term_list {t :: r}

relation:
  | Gen LPAREN s = STRING COMMA t1 = object_type COMMA t2 = object_type RPAREN {Relations.RelGen(s, Terms.obj_to_monomial t1, Terms.obj_to_monomial t2)}
  | UNION LPAREN r1=relation COMMA r2=relation RPAREN {Relations.Union (r1, r2)}
  | INTERSECTION LPAREN r1=relation COMMA r2=relation RPAREN {Relations.Intersection (r1, r2)}
  | OP LPAREN r1=relation RPAREN {Relations.Op (r1)}
  | STAR LPAREN r1=relation RPAREN {Relations.Star (r1)}
  | Id LPAREN t1 = object_type RPAREN {Relations.IdRel (Terms.obj_to_monomial t1)}
  | TOP LPAREN t1 = object_type RPAREN {Relations.TopRel (Terms.obj_to_monomial t1)}
  | EMPTY LPAREN t1 = object_type RPAREN {Relations.BotRel (Terms.obj_to_monomial t1)}
  | r1=relation SEMICOLON r2=relation {Relations.Compose (r1, r2)}
  | LPAREN r = relation RPAREN {r}