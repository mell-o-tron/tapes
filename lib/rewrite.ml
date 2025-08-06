open Terms
open Tapes
open Typecheck

let rec tape_to_summand_list (t : tape) : tape list =
  match t with
  | Oplus (t1, t2) -> tape_to_summand_list t1 @ tape_to_summand_list t2
  | _ -> [ t ]

(* turns a circuit to product form *)
let rec circuit_to_product (t : circuit) =
  match t with
  | CCompose (Otimes (t1, t2), Otimes (t3, t4)) ->
      if
        circuit_typecheck (CCompose (t1, t3))
        && circuit_typecheck (CCompose (t2, t4))
      then Otimes (CCompose (t1, t3), CCompose (t2, t4))
      else
        CCompose
          ( circuit_to_product (Otimes (t1, t2)),
            circuit_to_product (Otimes (t3, t4)) )
  | CCompose (t1, t2) -> CCompose (circuit_to_product t1, circuit_to_product t2)
  | Otimes (t1, t2) -> Otimes (circuit_to_product t1, circuit_to_product t2)
  | _ -> t

let consume_sum_in_list l =
  match l with
  | [] -> []
  | [ t ] -> [ t ]
  | t1 :: t2 :: rest -> Oplus (t1, t2) :: rest

let tape_list_to_sum (l : tape list) =
  List.fold_right (fun t1 t2 -> Oplus (t1, t2)) l TId0 |> deep_clean_tape

let rec match_tapes_by_interface (l1 : tape list) (l2 : tape list) =
  match (l1, l2) with
  | [], [] -> []
  | [ t1 ], l2 ->
      let t2 = tape_list_to_sum l2 in
      if tape_typecheck (TCompose (t1, t2)) then [ TCompose (t1, t2) ]
      else failwith "incompatible interfaces in matching"
  | l1, [ t2 ] ->
      let t1 = tape_list_to_sum l1 in
      if tape_typecheck (TCompose (t1, t2)) then [ TCompose (t1, t2) ]
      else failwith "incompatible interfaces in matching"
  | t1 :: r1, t2 :: r2 ->
      let coar1 = tape_coarity t1 in
      let ar2 = tape_arity t2 in

      if coar1 = [] then t1 :: match_tapes_by_interface r1 l2
      else if ar2 = [] then t2 :: match_tapes_by_interface l1 r2
      else if tape_typecheck (TCompose (t1, t2)) then
        TCompose (t1, t2) :: match_tapes_by_interface r1 r2
      else if coar1 > ar2 then
        match_tapes_by_interface l1 (consume_sum_in_list l2)
      else match_tapes_by_interface (consume_sum_in_list l1) l2
  | _ -> failwith "should be unreachable"

(* turns a tape to sum form *)
let rec tape_to_sum_new (t : tape) =
  let rec tape_to_sum_aux (t : tape) =
    match t with
    | TCompose (Oplus (t1, t2), Oplus (t3, t4)) ->
        match_tapes_by_interface
          (Oplus (t1, t2) |> tape_to_summand_list)
          (Oplus (t3, t4) |> tape_to_summand_list)
        |> tape_list_to_sum
    | TCompose (t1, t2) -> TCompose (tape_to_sum_aux t1, tape_to_sum_aux t2)
    | Oplus (t1, t2) -> Oplus (tape_to_sum_aux t1, tape_to_sum_aux t2)
    | Trace (l1, t1) -> Trace (l1, tape_to_sum_aux t1)
    | _ -> t
  in
  (* this is probably not needed, but "meglio ave' paura che buscanne" *)
  let res = tape_to_sum_aux t in
  if res = t then res else tape_to_sum_new res

let rec tape_to_sum_old (t : tape) =
  let rec tape_to_sum_aux (t : tape) =
    match t with
    | TCompose (Oplus (t1, t2), Oplus (t3, t4)) ->
        if
          tape_typecheck (TCompose (t1, t3))
          && tape_typecheck (TCompose (t2, t4))
        then Oplus (TCompose (t1, t3), TCompose (t2, t4))
        else
          TCompose
            (tape_to_sum_aux (Oplus (t1, t2)), tape_to_sum_aux (Oplus (t3, t4)))
    | TCompose (t1, t2) -> TCompose (tape_to_sum_aux t1, tape_to_sum_aux t2)
    | Oplus (t1, t2) -> Oplus (tape_to_sum_aux t1, tape_to_sum_aux t2)
    | _ -> t
  in
  (* this is probably not needed, but "meglio ave' paura che buscanne" *)
  let res = tape_to_sum_aux t in
  if res = t then t else tape_to_sum_old res

let tape_to_sum = tape_to_sum_new

let rec tape_to_seq (t : tape) =
  let rec tape_to_seq_aux (t : tape) =
    match t with
    | Oplus (TCompose (t1, t2), TCompose (t3, t4)) ->
        TCompose (Oplus (t1, t3), Oplus (t2, t4))
    | TCompose (t1, t2) -> TCompose (tape_to_seq_aux t1, tape_to_seq_aux t2)
    | Oplus (t1, t2) -> Oplus (tape_to_seq_aux t1, tape_to_seq_aux t2)
    | Trace (l1, t1) -> Trace (l1, tape_to_seq_aux t1)
    | _ -> t
  in
  let res = tape_to_seq_aux t in
  if res = t then t else tape_to_seq res

let rec group_composition_right (t : tape) =
  let rec group_composition_right_aux (t : tape) =
    match t with
    | TCompose (TCompose (t1, t2), t3) -> TCompose (t1, TCompose (t2, t3))
    | TCompose (t1, t2) ->
        TCompose (group_composition_right_aux t1, group_composition_right_aux t2)
    | Oplus (t1, t2) ->
        Oplus (group_composition_right_aux t1, group_composition_right_aux t2)
    | Trace (u, t1) -> Trace (u, group_composition_right_aux t1)
    | _ -> t
  in
  let res = group_composition_right_aux t in
  if t = res then res else group_composition_right res

let rec add_empty_traces t =
  match t with
  | Compose (t1, t2) -> Compose (add_empty_traces t1, add_empty_traces t2)
  | Oplus (t1, t2) -> Oplus (add_empty_traces t1, add_empty_traces t2)
  | Trace _ -> t
  | _ -> Trace ([], t)

let rec trace_normal_form (t : term) : term =
  let t = add_empty_traces t in
  match t with
  | Compose (Trace (l1, t1), Trace (l2, t2)) ->
      let s1 = Terms.SwapPlus (l1, l2) in
      let s2 = Terms.SwapPlus (l2, l1) in
      let id1 = Id (remainder_of_prefix l1 (arity t1)) in
      let id2 = Id (remainder_of_prefix l1 (coarity t1)) in
      let t3 =
        Compose
          ( Oplus (s1, id1),
            Compose
              (Oplus (Id l2, t1), Compose (Oplus (s2, id2), Oplus (Id l1, t2)))
          )
      in
      Trace (l2 @ l1, t3)
  | Compose (t1, t2) ->
      trace_normal_form (Compose (trace_normal_form t1, trace_normal_form t2))
  | Oplus (Trace (l1, t1), Trace (l2, t2)) ->
      let sl = Terms.SwapPlus (l2, remainder_of_prefix l1 (arity t1)) in
      (* Printf.printf "sl: %s\n" (show_term sl); *)
      let sr = Terms.SwapPlus (remainder_of_prefix l1 (coarity t1), l2) in
      (* Printf.printf "sr: %s\n" (show_term sr); *)
      let idl1 = Id l1 in
      (* Printf.printf "idl1: %s\n" (show_term idl1); *)
      let idl2 = Id (remainder_of_prefix l2 (arity t2)) in
      (* Printf.printf "idl2: %s\n" (show_term idl2); *)
      let idr1 = Id l1 in
      (* Printf.printf "idr1: %s\n" (show_term idr1); *)
      let idr2 = Id (remainder_of_prefix l2 (coarity t2)) in
      (* Printf.printf "idr2: %s\n" (show_term idr2); *)
      let t3 =
        Compose
          ( Oplus (idl1, Oplus (sl, idl2)),
            Compose (Oplus (t1, t2), Oplus (idr1, Oplus (sr, idr2))) )
      in
      Trace (l2 @ l1, t3)
  | Oplus (t1, t2) ->
      trace_normal_form (Oplus (trace_normal_form t1, trace_normal_form t2))
  | Trace _ -> t
  | _ -> failwith "should not happen"
