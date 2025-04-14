
%{
(*   open Ast (* Define the abstract syntax tree in Ast.ml *) *)
%}

%token Id SwapTimes SwapPlus Otimes Oplus Compose Ldistr Gen Zero One
%token LPAREN RPAREN LBRACKET RBRACKET COLON COMMA EOF

%token <string> STRING

%left Compose
%left Tensor

%start <Ast.term> main
%%

main:
  | t=term EOF { t }

term:
  | Id LPAREN t1 = object_type RPAREN                                                                   {Ast.Id(Ast.obj_to_polynomial(t1)) }
  | SwapTimes LPAREN t1 = object_type COMMA t2 = object_type RPAREN                                     {Ast.SwapTimes(Ast.obj_to_polynomial(t1), Ast.obj_to_polynomial(t2)) }
  | SwapPlus LPAREN t1 = object_type COMMA t2 = object_type RPAREN                                      {Ast.SwapPlus(Ast.obj_to_polynomial(t1), Ast.obj_to_polynomial(t2)) }
  | Gen LPAREN s = STRING COMMA t1 = object_type COMMA t2 = object_type RPAREN                          {Ast.Gen(s, Ast.obj_to_polynomial(t1), Ast.obj_to_polynomial(t2))}
  | Ldistr LPAREN t1 = object_type COMMA t2 = object_type COMMA t3 = object_type RPAREN                 {Ast.Ldistr(Ast.obj_to_polynomial(t1), Ast.obj_to_polynomial(t2), Ast.obj_to_polynomial(t3))}
  | t1 = term Otimes t2 = term                                                                          {Ast.Otimes(t1, t2)}
  | t1 = term Oplus t2 = term                                                                           {Ast.Oplus(t1, t2)}
  | t1 = term Compose t2 = term                                                                         {Ast.Compose(t1, t2)}
  | LPAREN t = term RPAREN                                                                              {t}

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
