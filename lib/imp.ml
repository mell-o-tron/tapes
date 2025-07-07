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
  | [] -> failwith "not found"
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
      print_string "Gamma: ";
      print_context gamma;
      print_string "Delta: ";
      print_context delta;
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
                ( f,
                  [ List.map (fun x -> get_type c x) l |> List.flatten ],
                  [ [ coar ] ] ) ) )

let rec eval_pred (c : context) (p : imp_pred) =
  match p with
  | Rel (name, l, s) ->
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
                ( (if s then Printf.sprintf "$ %s$" name
                   else Printf.sprintf "$\\overline{%s}$" name),
                  [ List.map (fun x -> get_type c x) l |> List.flatten ],
                  obj_to_polynomial Ob1 ) ) )
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

(* from section 11 of tapes with traces (bottom of page 42) *)
