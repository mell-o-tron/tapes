open Terms
open Tapes
open Typecheck
(* open Common_defs *)

(** transforms a tape of the form [Oplus_i (t_i)] to the list containing the
    [t_i]. *)
let rec tape_to_summand_list (t : tape) : tape list =
  match t with
  | Oplus (t1, t2) -> tape_to_summand_list t1 @ tape_to_summand_list t2
  | _ -> [ t ]

(** transforms a circuit of the form [Otimes (c_i)] to the list containing the
    [c_i]. *)
let rec circuit_to_factor_list (c : circuit) : circuit list =
  match c with
  | Otimes (c1, c2) -> circuit_to_factor_list c1 @ circuit_to_factor_list c2
  | _ -> [ c ]

(** transforms a list of circuits into one circuits that is their product *)
let circuit_list_to_prod (l : circuit list) =
  List.fold_right (fun t1 t2 -> Otimes (t1, t2)) l CId1 |> deep_clean_circuit

(** takes the first two elements of a circuit list and replaces them with their
    product. *)
let consume_prod_in_list l =
  match l with
  | [] -> []
  | [ t ] -> [ t ]
  | t1 :: t2 :: rest -> Otimes (t1, t2) :: rest

(** Given two lists of circuits, produces a list of their composition, matched
    by interface. *)
let rec match_circuits_by_interface (l1 : circuit list) (l2 : circuit list) =
  match (l1, l2) with
  | [], [] -> []
  | [ c1 ], l2 ->
      let c2 = circuit_list_to_prod l2 in
      if circuit_typecheck (CCompose (c1, c2)) then [ CCompose (c1, c2) ]
      else failwith "incompatible interfaces in matching"
  | l1, [ c2 ] ->
      let c1 = circuit_list_to_prod l1 in
      if circuit_typecheck (CCompose (c1, c2)) then [ CCompose (c1, c2) ]
      else failwith "incompatible interfaces in matching"
  | c1 :: r1, c2 :: r2 ->
      let coar1 = circuit_coarity c1 in
      let ar2 = circuit_arity c2 in

      if coar1 = [] then c1 :: match_circuits_by_interface r1 l2
      else if ar2 = [] then c2 :: match_circuits_by_interface l1 r2
      else if circuit_typecheck (CCompose (c1, c2)) then
        CCompose (c1, c2) :: match_circuits_by_interface r1 r2
      else if coar1 > ar2 then
        match_circuits_by_interface l1 (consume_prod_in_list l2)
      else match_circuits_by_interface (consume_prod_in_list l1) l2
  | _ -> failwith "should be unreachable"

(** turns a circuit to product form -- incomplete method *)
let rec circuit_to_product_old (t : circuit) =
  match t with
  | CCompose (Otimes (t1, t2), Otimes (t3, t4)) ->
      if
        circuit_typecheck (CCompose (t1, t3))
        && circuit_typecheck (CCompose (t2, t4))
      then Otimes (CCompose (t1, t3), CCompose (t2, t4))
      else
        CCompose
          ( circuit_to_product_old (Otimes (t1, t2)),
            circuit_to_product_old (Otimes (t3, t4)) )
  | CCompose (t1, t2) ->
      CCompose (circuit_to_product_old t1, circuit_to_product_old t2)
  | Otimes (t1, t2) ->
      Otimes (circuit_to_product_old t1, circuit_to_product_old t2)
  | _ -> t

(** turns a circuit to product form -- complete method *)
let rec circuit_to_prod_new (c : circuit) =
  let rec circ_to_sum_aux (c : circuit) =
    match c with
    | CCompose (Otimes (c1, c2), Otimes (c3, c4)) ->
        match_circuits_by_interface
          (Otimes (c1, c2) |> circuit_to_factor_list)
          (Otimes (c3, c4) |> circuit_to_factor_list)
        |> circuit_list_to_prod
    | CCompose (c1, c2) -> CCompose (circ_to_sum_aux c1, circ_to_sum_aux c2)
    | Otimes (c1, c2) -> Otimes (circ_to_sum_aux c1, circ_to_sum_aux c2)
    | _ -> c
  in
  (* this is probably not needed, but "meglio ave' paura che buscanne" *)
  let res = circ_to_sum_aux c in
  if res = c then res else circuit_to_prod_new res

(** turns a circuit to product form *)
let circuit_to_product = circuit_to_prod_new

(** takes the first two elements of a tape list and replaces them with their
    sum. *)
let consume_sum_in_list l =
  match l with
  | [] -> []
  | [ t ] -> [ t ]
  | t1 :: t2 :: rest -> Oplus (t1, t2) :: rest

(** given a list of tapes, produces a tape which is its sum. *)
let tape_list_to_sum (l : tape list) =
  List.fold_right (fun t1 t2 -> Oplus (t1, t2)) l TId0 |> deep_clean_tape

(** Given two lists of tapes, produces a list of their composition, matched by
    interface. *)
let rec match_tapes_by_interface (l1 : tape list) (l2 : tape list) =
  match (l1, l2) with
  | [], [] -> []
  | [ t1 ], l2 ->
      let t2 = tape_list_to_sum l2 in
      if tape_typecheck (TCompose (t1, t2)) then [ TCompose (t1, t2) ]
      else (
        Printf.printf "\t%s \n\n \t%s\n" (pp_tape t1) (pp_tape t2);
        failwith (Printf.sprintf "incompatible interfaces in matching"))
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

(** turns a tape to sum form -- complete method *)
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

(** turns a tape to sum form -- incomplete method *)
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

(** turns a tape to sum form *)
let tape_to_sum = tape_to_sum_new

(** turns a tape to sequence-first form *)
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

(** groups composition right in a tape. *)
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

(** given a TERM, adds empty traces to every generator; auxiliary function for
    [trace_normal_form] *)
let rec add_empty_traces t =
  match t with
  | Compose (t1, t2) -> Compose (add_empty_traces t1, add_empty_traces t2)
  | Oplus (t1, t2) -> Oplus (add_empty_traces t1, add_empty_traces t2)
  | Trace _ -> t
  | _ -> Trace ([], t)

(** trace-normal-form of a term, as described in Zanasi PhD thesis (prop 4.25)
*)
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
      let sr = Terms.SwapPlus (remainder_of_prefix l1 (coarity t1), l2) in
      let idl1 = Id l1 in
      let idl2 = Id (remainder_of_prefix l2 (arity t2)) in
      let idr1 = Id l1 in
      let idr2 = Id (remainder_of_prefix l2 (coarity t2)) in
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

(** Given a list of sorts, produces the identity circuit for that sort. *)
let rec bigcid m =
  match m with
  | [] -> CId1
  | [ s ] -> CId s
  | s :: rest -> Otimes (CId s, bigcid rest)

(** checks if a circuit is a product of ids (and can thus be eliminated if
    composed with something)*)
let rec is_prod_of_ids (c : circuit) =
  match c with
  | CId _ -> true
  | CId1 -> true
  | CCompose (c1, c2) -> is_prod_of_ids c1 && is_prod_of_ids c2
  | Otimes (c1, c2) -> is_prod_of_ids c1 && is_prod_of_ids c2
  | _ -> false

(** removes redundant identities in a circuit *)
let rec remove_redundant_ids (c : circuit) =
  let rec aux (c : circuit) =
    let c = circuit_to_product c in
    match c with
    | CCompose (cid, c1) when is_prod_of_ids cid ->
        (* Printf.printf "applied rule!\n"; *)
        c1
    | CCompose (c1, cid) when is_prod_of_ids cid ->
        (* Printf.printf "applied rule!\n"; *)
        c1
    | CCompose (c1, c2) -> CCompose (aux c1, aux c2)
    | Otimes (c1, c2) -> Otimes (aux c1, aux c2)
    | _ -> c
  in

  let res = aux c in
  if res = c then res else remove_redundant_ids res

(** merges composed taped circuits in tapes *)
let rec merge_embedded_circuits (t : tape) =
  let rec aux t =
    let t = tape_to_sum t in
    match t with
    | TCompose (Tape c1, Tape c2) ->
        Tape (CCompose (c1 |> deep_clean_circuit, c2 |> deep_clean_circuit))
    | TCompose (TId [ u ], Tape c2) when circuit_arity c2 = u -> Tape c2
    | TCompose (Tape c1, TId [ u ]) when circuit_coarity c1 = u -> Tape c1
    | TCompose (TId p, t1) when p = tape_arity t1 -> t1
    | TCompose (t1, TId p) when p = tape_coarity t1 -> t1
    | TCompose (t1, t2) -> TCompose (aux t1, aux t2)
    | Oplus (t1, t2) -> Oplus (aux t1, aux t2)
    | Trace (u, t) -> Trace (u, aux t)
    | _ -> t
  in
  let res = aux t in
  if t = res then res else merge_embedded_circuits res

(** brings a circuit to sequence-first form *)
let rec circuit_to_seq (c : circuit) =
  let rec aux (c : circuit) =
    match c with
    | Otimes (CCompose (t1, t2), CCompose (t3, t4)) ->
        CCompose (Otimes (t1, t3), Otimes (t2, t4))
    | CCompose (t1, t2) -> CCompose (aux t1, aux t2)
    | Otimes (t1, t2) -> Otimes (aux t1, aux t2)
    | _ -> c
  in
  let res = aux c in
  if res = c then c else circuit_to_seq res

(** lifts [remove_redundant_ids] to tapes *)
let rec reduce_circuits_tape (t : tape) =
  match t with
  | Tape c -> Tape (remove_redundant_ids c)
  | Oplus (t1, t2) -> Oplus (reduce_circuits_tape t1, reduce_circuits_tape t2)
  | TCompose (t1, t2) ->
      TCompose (reduce_circuits_tape t1, reduce_circuits_tape t2)
  | Trace (u, t) -> Trace (u, reduce_circuits_tape t)
  | _ -> t

(** wraps all embedded circuits in identities, for more pleasant graphical
    display. *)
let rec wrap_embeds_in_ids (t : tape) =
  match t with
  | Tape c ->
      TCompose
        (TCompose (TId [ circuit_arity c ], Tape c), TId [ circuit_coarity c ])
  | Oplus (t1, t2) -> Oplus (wrap_embeds_in_ids t1, wrap_embeds_in_ids t2)
  | TCompose (t1, t2) -> TCompose (wrap_embeds_in_ids t1, wrap_embeds_in_ids t2)
  | Trace (u, t) -> Trace (u, wrap_embeds_in_ids t)
  | _ -> t
