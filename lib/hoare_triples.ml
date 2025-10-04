open Terms
open Imp
(* open Common_defs *)

(* open Tape_inclusion *)
open Rewrite
(* open Term_to_tape *)

type hoare_triple = {
  precond : imp_pred;
  com : imp_comm;
  postcond : imp_pred;
}
(** Type representing a Hoare triple: precondition, command, and postcondition.
*)

type relational_hoare_quadruple = {
  precond : imp_pred;
  com1 : imp_comm;
  com2 : imp_comm;
  postcond : imp_pred;
}
(** Type representing a relational Hoare quadruple: precondition, two commands,
    and postcondition. *)

(** Constructs a Hoare triple from a precondition, command, and postcondition.
*)
let make_triple precond com postcond = { precond; com; postcond }

(** Constructs a relational Hoare quadruple from a precondition, two commands,
    and postcondition. *)
let make_rel_triple precond com1 com2 postcond =
  { precond; com1; com2; postcond }

(** Constructs the two sides of the inequality corresponding to a Hoare triple.
*)
let check_triple (c : context) (t : hoare_triple) (inv : term) =
  let prec_term = eval_pred c t.precond in
  let post_term = eval_pred c t.postcond in
  let com_term = eval_command c t.com in

  let t1 = Compose (term_inverse prec_term, com_term) |> trace_normal_form in
  let untraced_t1 = match t1 with Trace (_, t) -> t | t -> t in
  let inv_t1 = Compose (Oplus (term_inverse inv, Id [ [] ]), untraced_t1) in
  let inv_t2 =
    Compose
      ( Join [ [] ],
        Compose (Split [ [] ], Oplus (term_inverse inv, term_inverse post_term))
      )
  in
  (inv_t1, inv_t2)

(** Constructs the two sides of the inequality corresponding to a Rel. Hoare
    quadruple. *)
let check_relational_triple (c1 : context) (c2 : context)
    (t : relational_hoare_quadruple) (inv : term) =
  let prec_term = eval_pred (c1 @ c2) t.precond in
  let post_term = eval_pred (c1 @ c2) t.postcond in
  let com1_term = eval_command c1 t.com1 in
  let com2_term = eval_command c2 t.com2 in

  let t1 =
    Compose (term_inverse prec_term, Otimes (com1_term, com2_term))
    |> trace_normal_form
  in
  let untraced_t1 =
    match t1 with
    | Trace (_, t) -> t
    | _ ->
        raise
          (Errors.TypeError
             "Triples can currently be checked only in programs with cycles")
  in
  let inv_t1 = Compose (Oplus (inv, Id [ [] ]), untraced_t1) in
  let inv_t2 =
    Compose
      (Join [ [] ], Compose (Split [ [] ], Oplus (inv, term_inverse post_term)))
  in
  (inv_t1, inv_t2)
