
%{
(*   open Ast (* Define the abstract syntax tree in Ast.ml *) *)
%}

%token Id SwapTimes SwapPlus Otimes Oplus Ldistr Gen Zero One
%token LPAREN RPAREN LBRACKET RBRACKET COLON SEMICOLON COMMA EOF Term Tape Newsort

%token <string> STRING

%left SEMICOLON
%left Tensor

%start <Ast.sort list * Ast.term option * Tapes.tape option> main
%%

main:
  | l = separated_list (SEMICOLON, sortdef) Term t=term EOF                                    { l, Some t , None}
  | l = separated_list (SEMICOLON, sortdef) Tape t=tape EOF                                    { l, None, Some t}


sortdef:
  | Newsort s=STRING                                                                                    {s}

term:
  | Id LPAREN t1 = object_type RPAREN                                                                   {Ast.Id(Ast.obj_to_polynomial(t1)) }
  | SwapTimes LPAREN t1 = object_type COMMA t2 = object_type RPAREN                                     {Ast.SwapTimes(Ast.obj_to_polynomial(t1), Ast.obj_to_polynomial(t2)) }
  | SwapPlus LPAREN t1 = object_type COMMA t2 = object_type RPAREN                                      {Ast.SwapPlus(Ast.obj_to_polynomial(t1), Ast.obj_to_polynomial(t2)) }
  | Gen LPAREN s = STRING COMMA t1 = object_type COMMA t2 = object_type RPAREN                          {Ast.Gen(s, Ast.obj_to_polynomial(t1), Ast.obj_to_polynomial(t2))}
  | Ldistr LPAREN t1 = object_type COMMA t2 = object_type COMMA t3 = object_type RPAREN                 {Ast.Ldistr(Ast.obj_to_polynomial(t1), Ast.obj_to_polynomial(t2), Ast.obj_to_polynomial(t3))}
  | t1 = term Otimes t2 = term                                                                          {Ast.Otimes(t1, t2)}
  | t1 = term Oplus t2 = term                                                                           {Ast.Oplus(t1, t2)}
  | t1 = term SEMICOLON t2 = term                                                                       {Ast.Compose(t1, t2)}
  | LPAREN t = term RPAREN                                                                              {t}

circuit:
  | One                                                                                                 { Tapes.CId1 }
  | Id LPAREN s = STRING RPAREN                                                                         { Tapes.CId (s) }
  | Gen LPAREN s = STRING COMMA t1 = object_type COMMA t2 = object_type RPAREN                          { Tapes.Gen  (s, Ast.sort_prod_to_list t1, Ast.sort_prod_to_list t2) }
  | SwapTimes LPAREN s1 = STRING COMMA s2 = STRING RPAREN                                               { Tapes.SwapTimes (s1, s2) }
  | c1 = circuit Otimes c2 = circuit                                                                    { Tapes.Otimes    (c1, c2) }
  | c1 = circuit SEMICOLON c2 = circuit                                                                 { Tapes.CCompose (c1, c2) }
  | LPAREN c = circuit RPAREN                                                                           { c }

tape:
  | Zero                                                                                                { Tapes.TId0 }
  | Id LPAREN t = object_type RPAREN                                                                    { Tapes.TId      (Ast.obj_to_polynomial t) }
  | LBRACKET c = circuit RBRACKET                                                                       { Tapes.Tape     c }
  | t1 = tape Oplus t2 = tape                                                                           { Tapes.Oplus    (t1, t2) }
  | SwapPlus LPAREN t1 = object_type COMMA t2 = object_type RPAREN                                      { Tapes.SwapPlus (Ast.sort_prod_to_list t1, Ast.sort_prod_to_list t2) }
  | Ldistr   LPAREN t1 = object_type COMMA t2 = object_type COMMA t3 = object_type RPAREN               { Tapes.Ldistr  (Ast.sort_prod_to_list t1, Ast.sort_prod_to_list t2, Ast.sort_prod_to_list t3) }
  | t1 = tape SEMICOLON t2 = tape                                                                       { Tapes.TCompose(t1, t2) }
  | LPAREN t = tape RPAREN                                                                              { t }

object_type:
  | LPAREN o = object_type RPAREN                                 {o}
  | s = STRING                                                    {(Ast.S(s))}
  | o1 = object_type Otimes o2 = object_type                      {(Ast.Obtimes(o1, o2))}
  | o1 = object_type Oplus o2 = object_type                       {(Ast.Obplus(o1, o2))}
  | Zero                                                          {(Ast.Ob0)}
  | One                                                           {(Ast.Ob1)}


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
