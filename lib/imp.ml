open Terms
open Errors
(* open Typecheck *)

type iden = string

(** Expression of the imperative language *)
type imp_expr =
  | Var of iden
  | Func of iden * imp_expr list * sort

(** Predicate of the imperative language *)
type imp_pred =
  | Rel of iden * imp_expr list * bool
      (** Given a context, returns the monomial representing it. *)
  | Top
  | Bottom
  | Or of imp_pred * imp_pred
  | And of imp_pred * imp_pred

(** Command of the imperative language *)
type imp_comm =
  | Abort
  | Skip
  | IfThenElse of imp_pred * imp_comm * imp_comm
  | WhileDo of imp_pred * imp_comm
  | Seq of imp_comm * imp_comm
  | Assign of iden * imp_expr

type context = (iden * iden) list
(** List of (name, sort)*)

type split_context_t = {
  context_before : context;
  context_after : context;
}
(** Record representing what's before and after a certain variable in a context
*)

(** Given a context, returns the monomial representing it. *)
let eval_context (c : context) = List.map snd c

(** Given a context, an identifier and its supposed sort, splits the context at
    the position of the variable if it is found with the correct sort, otherwise
    raises an ImpError *)
let split_context (c : context) (x : iden) (a : sort) =
  let count =
    List.fold_left
      (fun acc elt -> if fst elt = x && snd elt = a then acc + 1 else acc)
      0 c
  in
  if count <> 1 then
    raise (ImpError (Printf.sprintf "variable %s not found in context" x))
  else
    (* Now do a single pass to split at the first occurrence of x *)
    let rec aux rev_before = function
      | [] ->
          (* impossible, since count = 1 *)
          failwith "unreachable"
      | hd :: tl ->
          if fst hd = x && snd hd = a then
            (* found it: before = rev_before reversed, after = tl *)
            { context_before = List.rev rev_before; context_after = tl }
          else aux (hd :: rev_before) tl
    in
    aux [] c

let print_context c =
  List.iter (fun (x, s) -> print_string (x ^ " ⊢ " ^ s ^ ", ")) c;
  print_endline ""

(** Given a context and a variable, looks up the sort of the variable in the
    context *)
let rec get_sort (c : context) (x : iden) =
  match c with
  | [] -> raise (ImpError (Printf.sprintf "variable %s not found in context" x))
  | (x1, s) :: rest -> if x = x1 then s else get_sort rest x

(** Given a context and an expression, returns the (return) type of the
    expression in the context *)
let get_type (c : context) (e : imp_expr) =
  match e with Var x -> [ get_sort c x ] | Func (_f, _l, coar) -> [ coar ]

(** Given a context and an expression, returns the tape corresponding to the
    expression *)
let rec eval_expr (c : context) (e : imp_expr) =
  match e with
  | Var x ->
      let a = get_sort c x in
      let sc = split_context c x a in
      let gamma, delta = (sc.context_before, sc.context_after) in
      Otimes
        ( Discard [ eval_context gamma ],
          Otimes (Id [ [ a ] ], Discard [ eval_context delta ]) )
  | Func (f, l, coar) ->
      let args =
        List.map (eval_expr c) l
        |> List.fold_left
             (fun acc a -> Otimes (acc, a))
             (Id (obj_to_polynomial Ob1))
      in
      Compose
        ( multi_copy (List.length l) [ eval_context c ],
          Compose
            ( args,
              Gen
                ( Printf.sprintf "$%s$" f,
                  [ List.map (fun x -> get_type c x) l |> List.flatten ],
                  [ [ coar ] ],
                  Function ) ) )

let rec eval_pred (c : context) (p : imp_pred) =
  match p with
  | Rel (name, l, s) ->
      let args =
        List.map (eval_expr c) l
        |> List.fold_left (fun acc a -> Otimes (acc, a)) (Id [ [] ])
      in
      Compose
        ( multi_copy (List.length l) [ eval_context c ],
          Compose
            ( args,
              Gen
                ( (if s then Printf.sprintf "$%s$" name
                   else Printf.sprintf "$\\overline{%s}$" name),
                  [ List.map (fun x -> get_type c x) l |> List.flatten ],
                  obj_to_polynomial Ob1,
                  Relation ) ) )
  | Top -> Discard [ eval_context c ]
  | Bottom -> Compose (Cut [ eval_context c ], Spawn (obj_to_polynomial Ob1))
  | Or (pred1, pred2) ->
      Compose
        ( Split [ eval_context c ],
          Compose
            ( Oplus (eval_pred c pred1, eval_pred c pred2),
              Join (obj_to_polynomial Ob1) ) )
  | And (pred1, pred2) ->
      Compose
        (Copy [ eval_context c ], Otimes (eval_pred c pred1, eval_pred c pred2))

let kleene_star (t : term) =
  let ar = Typecheck.arity t in
  let coar = Typecheck.coarity t in
  if ar = coar then
    Trace (ar, Compose (Join ar, Compose (Split ar, Oplus (t, Id ar))))
  else
    raise
      (ImpError
         (Printf.sprintf
            "Tried to apply kleene star to term with ar != coar.\n\
             Ar = %s\t Coar = %s"
            (Typecheck.string_of_sort_list_list ar)
            (Typecheck.string_of_sort_list_list coar)))

let union (t1 : term) (t2 : term) =
  let art1 = Typecheck.arity t1 in
  let art2 = Typecheck.arity t2 in
  let coart1 = Typecheck.coarity t1 in
  let coart2 = Typecheck.coarity t2 in
  if art1 = art2 && coart1 = coart2 then
    Compose (Compose (Split art1, Oplus (t1, t2)), Join coart1)
  else failwith "cannot apply union: arities or coarities don't match"

let corefl (c : context) (p : imp_pred) =
  let t = eval_pred c p in
  let ar = Typecheck.arity t in
  Compose (Copy ar, Otimes (t, Id ar))

let rec negate (p : imp_pred) =
  match p with
  | Rel (r, l, s) -> Rel (r, l, not s)
  | Top -> Bottom
  | Bottom -> Top
  | Or (p1, p2) -> And (negate p1, negate p2)
  | And (p1, p2) -> Or (negate p1, negate p2)

let rec eval_command (c : context) (com : imp_comm) =
  match com with
  | Abort -> Compose (Cut [ eval_context c ], Spawn [ eval_context c ])
  | Skip -> Id [ eval_context c ]
  | Seq (com1, com2) -> Compose (eval_command c com1, eval_command c com2)
  | IfThenElse (p, com1, com2) ->
      union
        (Compose (corefl c p, eval_command c com1))
        (Compose (corefl c (negate p), eval_command c com2))
  | WhileDo (p, com1) ->
      Printf.printf "corefl:\tAr : %s,\tCoar : %s\n"
        (Typecheck.arity (corefl c p) |> Typecheck.string_of_sort_list_list)
        (Typecheck.coarity (corefl c p) |> Typecheck.string_of_sort_list_list);
      Printf.printf "command:\tAr : %s,\tCoar : %s\n"
        (Typecheck.arity (eval_command c com1)
        |> Typecheck.string_of_sort_list_list)
        (Typecheck.coarity (eval_command c com1)
        |> Typecheck.string_of_sort_list_list);

      Compose
        ( kleene_star (Compose (corefl c p, eval_command c com1)),
          corefl c (negate p) )
  | Assign (x, e) ->
      let a = get_sort c x in
      let sc = split_context c x a in
      let gamma, delta = (sc.context_before, sc.context_after) in
      Compose
        ( Otimes
            ( Otimes (Copy [ eval_context gamma ], Id [ [ a ] ]),
              Copy [ eval_context delta ] ),
          Otimes
            ( Id [ eval_context gamma ],
              Otimes (eval_expr c e, Id [ eval_context delta ]) ) )

(* from section 11 of tapes with traces (bottom of page 42) *)
