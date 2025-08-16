open Terms
open Tapes
open Typecheck
open Common_defs

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

(** Given a list of sorts, produces the identity circuit for that sort. *)
let rec bigcid m =
  match m with
  | [] -> CId1
  | [ s ] -> CId s
  | s :: rest -> Otimes (CId s, bigcid rest)

(** Given an index and a circuit, gets the list of paths that are obtained by
    starting at the idx-th 'input pin' starting from the top, and moving to the
    right. The return type is [(int * circuit) list * circuit option], because
    the following is returned:
    - a list of pairs (output pin index, path)
    - an optional "continuation", returned in the cases in which the path is
      interrupted with a `discard` and then continued (e.g. [---o  o---] )

    Note that this function only returns paths up to the first box it
    encounters. *)
let rec get_circuit_path (idx : int) (c : circuit) :
    (int * circuit) list * circuit option =
  let c = c |> clean_circuit in
  match c with
  | CId _ ->
      if idx = 0 then ([ (idx, c) ], None)
      else failwith "id: index was not zero"
  | Gen ("cocopy", _, [ u ], _) ->
      ( [
          (if idx < 2 then (0, CId u) else failwith "out of bounds index in join");
        ],
        None )
  | Gen ("copy", [ u ], _, _) ->
      if idx = 0 then ([ (0, CId u); (1, CId u) ], None)
      else failwith "split: index was not zero."
  | SwapTimes (x, y) ->
      ( [
          (if idx = 0 then (1, CId x)
           else if idx = 1 then (0, CId y)
           else failwith "wahoo");
        ],
        None )
  | Gen ("codiscard", _, _, _) -> ([], None)
  | Gen ("discard", _, _, _) -> ([], None)
  (* every other generator stops the path, behaving as an id*)
  | Gen (_, ar, _, _) -> get_circuit_path idx (bigcid ar)
  | CId1 -> ([], None)
  | Otimes (c1, c2) ->
      let art1size = List.length (circuit_arity c1) in
      let coart1size = List.length (circuit_coarity c1) in
      let art2size = List.length (circuit_arity c2) in

      if idx < art1size then get_circuit_path idx c1
      else if idx < art1size + art2size then
        let lin, cont = get_circuit_path (idx - art1size) c2 in
        (List.map (fun (i, t) -> (i + coart1size, t)) lin, cont)
      else
        failwith (Printf.sprintf "out of bounds monomial index %d in oplus" idx)
  | CCompose (c1, c2) ->
      let l1 = get_circuit_path idx c1 in

      let continuation = if List.length (fst l1) = 0 then Some c2 else None in

      let indices = List.map fst (fst l1) |> List.sort_uniq Int.compare in
      (* group the paths of c1 by destination index *)
      let part =
        List.map (fun i -> List.filter (fun (j, _) -> j = i) (fst l1)) indices
      in
      (* use each dest index of l1 as a source index for l2 *)
      let l2 = List.map (fun i -> get_circuit_path i c2) indices in

      (* compose each path in t1 with every path in c2 with matching destination and source *)
      let composed =
        List.map2
          (fun a b ->
            List.map
              (fun (_, ta) ->
                List.map (fun (ib, tb) -> (ib, CCompose (ta, tb))) b)
              a
            |> List.flatten)
          part (List.map fst l2)
        |> List.flatten
      in

      (composed, continuation)

(** removes the ith path of a circuit, from the left *)
let rec eliminate_path_from_left (idx : int) (c : circuit) =
  let c = c |> clean_circuit in
  match c with
  | CId _ ->
      if idx = 0 then (CId1, [ 0 ]) else failwith "id: index was not zero"
  | Gen ("cocopy", _, [ _ ], _) ->
      failwith "cocopy elimination not implemented yet"
  | Gen ("copy", [ _ ], _, _) ->
      if idx < 2 then (CId1, [ 0; 1 ])
      else failwith "out of bounds index in copy"
  | SwapTimes (x, y) ->
      if idx = 0 then (CId y, [ 1 ])
      else if idx = 1 then (CId x, [ 0 ])
      else failwith "wahoo"
  | Gen ("codiscard", _, _, _) ->
      failwith "codiscard elimination not implemented yet"
  | Gen ("discard", _, _, _) -> (CId1, [])
  | Gen (_, _, _, _) -> failwith "cannot eliminate generators"
  | CId1 -> (CId1, [])
  | Otimes (c1, c2) ->
      let art1size = List.length (circuit_arity c1) in
      let coart1size = List.length (circuit_coarity c1) in
      let art2size = List.length (circuit_arity c2) in

      if idx < art1size then
        let c1_elim, lis = eliminate_path_from_left idx c1 in
        (Otimes (c1_elim, c2), lis)
      else if idx < art1size + art2size then
        let elimd, indices = eliminate_path_from_left (idx - art1size) c2 in
        (Otimes (c1, elimd), List.map (fun i -> i + coart1size) indices)
      else failwith (Printf.sprintf "out of bounds pin index %d in otimes" idx)
  | CCompose (c1, c2) ->
      let c1_elim, l1 = eliminate_path_from_left idx c1 in

      let c2_elim, out_pins = eliminate_many_from_left l1 c2 in
      if c1_elim = CId1 then (c2_elim, out_pins)
      else (CCompose (c1_elim, c2_elim), out_pins)

(** removes a list of paths of a circuit, from the left *)
and eliminate_many_from_left l1 c =
  let c2_elim = ref c in
  let out_pins = ref [] in
  for i = 0 to List.length l1 - 1 do
    (* List.nth l1 i - i because i is the number of previously eliminated indices *)
    let el, lis = eliminate_path_from_left (List.nth l1 i - i) !c2_elim in
    c2_elim := el;
    out_pins := !out_pins @ lis
  done;
  (!c2_elim, !out_pins)

(** Checks whether a path is empty, returning a boolean outcome and an optional
    continuation. *)
let is_path_empty idx c =
  let paths, cont = get_circuit_path idx c in
  if List.length paths = 0 then (true, cont) else (false, cont)

(* BEGIN DEAD CODE : *)

let compose_continuation_below (c1 : circuit) (c2 : circuit option) =
  match c2 with None -> c1 | Some c2 -> Otimes (c1, c2)

let compose_continuation_above (c1 : circuit) (c2 : circuit option) =
  match c2 with None -> c1 | Some c2 -> Otimes (c2, c1)

let compose_continuations (c1 : circuit option) (c2 : circuit option)
    (ar : sort) =
  match (c1, c2) with
  | None, None -> Gen ("discard", [ ar ], [], Relation)
  | Some c1, Some c2 -> Otimes (c1, c2)
  | Some c1, None -> c1
  | None, Some c2 -> c2

(* : END DEAD CODE *)

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

(* let hardcoded_simplify_copier (n : int) (paths : (bool * circuit option) list) =
  match n with
  | 2 -> (
      let p1 = List.nth paths 0 in
      let p2 = List.nth paths 1 in
      let p3 = List.nth paths 2 in
      let p4 = List.nth paths 3 in
      match (p1, p2, p3, p4) with
      | (false, _), (false, _), (false, _), (false, _) -> failwith ""
      | (false, _), (false, _), (false, _), (true, _) -> failwith ""
      | (false, _), (false, _), (true, _), (false, _) -> failwith ""
      | (false, _), (false, _), (true, _), (true, _) -> failwith ""
      | (false, _), (true, _), (false, _), (false, _) -> failwith ""
      | (false, _), (true, _), (false, _), (true, _) -> failwith ""
      | (false, _), (true, _), (true, _), (false, _) -> failwith ""
      | (false, _), (true, _), (true, _), (true, _) -> failwith ""
      | (true, _), (false, _), (false, _), (false, _) -> failwith ""
      | (true, _), (false, _), (false, _), (true, _) -> failwith ""
      | (true, _), (false, _), (true, _), (false, _) -> failwith ""
      | (true, _), (false, _), (true, _), (true, _) -> failwith ""
      | (true, _), (true, _), (false, _), (false, _) -> failwith ""
      | (true, _), (true, _), (false, _), (true, _) -> failwith ""
      | (true, _), (true, _), (true, _), (false, _) -> failwith ""
      | (true, _), (true, _), (true, _), (true, _) -> failwith "")
  | _ -> failwith "not implemented yet" *)

(** simplifies rules through the comonoid rules, eliminating empty paths that
    come out of copiers *)
(* let rec eliminate_empty_paths (program_type : sort list) (c : circuit) =
  (* Printf.printf "circuit: %s\n" (show_circuit c); *)
  let target =
    copy_monomial_to_circuit program_type
    |> deep_clean_circuit |> remove_redundant_ids
  in
  (* 
  Printf.printf "target: %s\n" (show_circuit target); *)

  (* let c = circuit_to_product c in *)
  match c with
  | CCompose (c1, rest) when compare_circuit c1 target = 0 -> (
      Printf.printf "found_match\n";

      let l = List.init (List.length (circuit_arity rest)) (fun i -> i) in
      let paths = List.map (fun i -> is_path_empty i rest) l in
      printf [] "how many empties?\n";
      List.iter
        (fun (b, _) ->
          if b then printf [ green ] "found empty\n"
          else printf [ red ] "found nonempty\n")
        paths;
      printf [] "=================\n";
      try
        let empties = List.mapi (fun i (b, _) -> (i, b)) paths in
        let empties = List.filter (fun (_, b) -> b) empties in
        let empties = List.map (fun (i, _) -> i) empties in

        CCompose
          ( hardcoded_simplify_copier (List.length paths) paths,
            fst (eliminate_many_from_left empties rest) )
      with _ -> CCompose (c1, rest))
  | Otimes (c1, c2) ->
      Otimes
        ( eliminate_empty_paths program_type c1,
          eliminate_empty_paths program_type c2 )
  | CCompose (c1, c2) ->
      CCompose
        ( eliminate_empty_paths program_type c1,
          eliminate_empty_paths program_type c2 )
  | _ -> c

(* lifts [eliminate_empty_paths] to tapes *)
let rec eliminate_empty_paths_tape (program_ctx : sort list) (t : tape) =
  match t with
  | Tape c -> Tape (eliminate_empty_paths program_ctx c)
  | TCompose (t1, t2) ->
      TCompose
        ( eliminate_empty_paths_tape program_ctx t1,
          eliminate_empty_paths_tape program_ctx t2 )
  | Oplus (t1, t2) ->
      Oplus
        ( eliminate_empty_paths_tape program_ctx t1,
          eliminate_empty_paths_tape program_ctx t2 )
  | Trace (u, t1) -> Trace (u, eliminate_empty_paths_tape program_ctx t1)
  | _ -> t *)

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
