open Terms
open Imp

(* open Tape_inclusion *)
open Rewrite
(* open Term_to_tape *)

type hoare_triple = {
  precond : imp_pred;
  com : imp_comm;
  postcond : imp_pred;
}

let make_triple precond com postcond = { precond; com; postcond }

let check_triple (c : context) (t : hoare_triple) (inv : term) =
  let prec_term = eval_pred c t.precond in
  let post_term = eval_pred c t.postcond in
  let com_term = eval_command c t.com in

  let t1 = Compose (term_inverse prec_term, com_term) |> trace_normal_form in
  let untraced_t1 =
    match t1 with Trace (_, t) -> t | _ -> failwith "program has no cycle"
  in
  let inv_t1 = untraced_t1 in
  (* let inv_t1 = Compose (inv, untraced_t1) in *)
  let inv_t2 =
    Compose
      (Join [ [] ], Compose (Split [ [] ], Oplus (inv, term_inverse post_term)))
  in
  (inv_t1, inv_t2)
