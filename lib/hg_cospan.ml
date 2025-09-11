open Common_defs
open Finset_conspan
open Ppx_compare_lib.Builtin
open Tapes

(** transforms an int option to string, rendering it as [-] if None *)
let int_opt_to_string (opt : int option) : string =
  match opt with Some x -> string_of_int x | None -> "-"

(** This module is used as input of the Make functor of Finset_cospan. Its base
    type is a numbered sort, along with an optional tag used internally for
    computing disjoint unions. *)
module TaggedType = struct
  type t = (int * sort) * int option [@@deriving compare]

  let compare (e1 : t) (e2 : t) = compare e1 e2
  let compare_val _ _ = failwith "not implemented"

  let get_tag ((_, s) : t) =
    try Option.get s with _ -> failwith "tag is empty, could not get tag."

  let set_tag ((v, _) : t) (n : int) : t = (v, Some n)
  let discard_tag ((v, _) : t) : t = (v, None)

  let to_string (((i, v), s) : t) =
    Printf.sprintf "(%s%d, %s)" v i (int_opt_to_string s)
end

module TaggedTypeCospan = Make (TaggedType)
module Taggedset = TaggedTypeCospan.Taggedset
module Taggedmap = TaggedTypeCospan.Taggedmap

type hyperedge_kind =
  | Function
  | Relation
  | NegRelation
[@@deriving show]

type hyperedge = {
  name : string;
  arity : sort list;
  kind : hyperedge_kind;
}

let pp_hyperedge { name; arity; kind } =
  Printf.sprintf "%s(%s; %s)\n" (show_hyperedge_kind kind) name
    (pp_sort_list arity)

let pp_hyperedge_list l = List.map pp_hyperedge l |> String.concat ", "

type hg_cospan = TaggedTypeCospan.t * hyperedge list

let pp_hg_cospan ((cos, l) : hg_cospan) =
  Printf.sprintf "(%s, [%s])"
    (TaggedTypeCospan.to_string cos)
    (pp_hyperedge_list l)

(** the cospan structure uses tags for computing disjoint unions, so the indices
    of our tagged sorts may end up jumbled up. This function realigns them. *)
let recompute_indices (cos : TaggedTypeCospan.t) : TaggedTypeCospan.t =
  (* Printf.printf "recomputing indices of %s\n" (TaggedTypeCospan.to_string cos); *)
  let hash_a = Hashtbl.create 10 in
  let hash_b = Hashtbl.create 10 in
  let hash_c = Hashtbl.create 10 in
  let a =
    List.mapi
      (fun i ((i_old, v), t) ->
        Hashtbl.add hash_a ((i_old, v), t) ((i, v), t);
        ((i, v), t))
      (Taggedset.to_list cos.a)
    |> Taggedset.of_list
  in
  let b =
    List.mapi
      (fun i ((i_old, v), t) ->
        (* Printf.printf "adding %s%d to hash_b\n" v i_old; *)
        Hashtbl.add hash_b ((i_old, v), t) ((i, v), t);
        ((i, v), t))
      (Taggedset.to_list cos.b)
    |> Taggedset.of_list
  in
  let c =
    List.mapi
      (fun i ((i_old, v), t) ->
        Hashtbl.add hash_c ((i_old, v), t) ((i, v), t);
        ((i, v), t))
      (Taggedset.to_list cos.c)
    |> Taggedset.of_list
  in

  let l =
    List.map
      (fun (((i1, v1), t1), ((i2, v2), t2)) ->
        (* Printf.printf "Looking for %s and %s in A and B\n"
          (TaggedType.to_string ((i1, v1), t1))
          (TaggedType.to_string ((i2, v2), t2)); *)
        (Hashtbl.find hash_a ((i1, v1), t1), Hashtbl.find hash_b ((i2, v2), t2)))
      (Taggedmap.to_list cos.l)
    |> Taggedmap.of_list
  in

  let r =
    List.map
      (fun (((i1, v1), t1), ((i2, v2), t2)) ->
        (* Printf.printf "looking for %s in hash_b\n"
          (TaggedType.to_string ((i2, v2), t2)); *)
        (Hashtbl.find hash_c ((i1, v1), t1), Hashtbl.find hash_b ((i2, v2), t2)))
      (Taggedmap.to_list cos.r)
    |> Taggedmap.of_list
  in

  { a; b; c; l; r }

(** shifts all the indices of elements *)
let shift_indices (ia : int) (ib : int) (ic : int) (cos : TaggedTypeCospan.t) :
    TaggedTypeCospan.t =
  let a =
    List.map (fun ((i, v), t) -> ((i + ia, v), t)) (Taggedset.to_list cos.a)
    |> Taggedset.of_list
  in
  let b =
    List.map (fun ((i, v), t) -> ((i + ib, v), t)) (Taggedset.to_list cos.b)
    |> Taggedset.of_list
  in
  let c =
    List.map (fun ((i, v), t) -> ((i + ic, v), t)) (Taggedset.to_list cos.c)
    |> Taggedset.of_list
  in

  let l =
    List.map
      (fun (((i1, v1), t1), ((i2, v2), t2)) ->
        (((i1 + ia, v1), t1), ((i2 + ib, v2), t2)))
      (Taggedmap.to_list cos.l)
    |> Taggedmap.of_list
  in

  let r =
    List.map
      (fun (((i1, v1), t1), ((i2, v2), t2)) ->
        (((i1 + ic, v1), t1), ((i2 + ib, v2), t2)))
      (Taggedmap.to_list cos.r)
    |> Taggedmap.of_list
  in

  { a; b; c; l; r }

(** performs the tensor product of two taggedtype cospans *)
let cospan_tensor (c1 : TaggedTypeCospan.t) (c2 : TaggedTypeCospan.t) =
  let c2 =
    shift_indices (Taggedset.cardinal c1.a) (Taggedset.cardinal c1.b)
      (Taggedset.cardinal c1.c) c2
  in
  (* Printf.printf "tensor of:\n%s\nand\n%s\n"
    (TaggedTypeCospan.to_string c1)
    (TaggedTypeCospan.to_string c2); *)

  let res = TaggedTypeCospan.tensor c1 c2 |> recompute_indices in
  (* Printf.printf "result is %s\n" (TaggedTypeCospan.to_string res); *)
  res

(** performs the composition of two taggedtype cospans *)
let cospan_compose c1 c2 =
  (* Printf.printf "composing:\n%s\nand\n%s\n"
    (TaggedTypeCospan.to_string c1)
    (TaggedTypeCospan.to_string c2); *)
  let res =
    TaggedTypeCospan.compose c1 c2
    |> recompute_indices |> TaggedTypeCospan.drop_tags
  in
  (* Printf.printf "result is %s\n" (TaggedTypeCospan.to_string res); *)
  res

(** performs the tensor product of two hypergraph cospans *)
let hg_cospan_tensor ((c1, l1) : hg_cospan) ((c2, l2) : hg_cospan) : hg_cospan =
  (cospan_tensor c1 c2, l1 @ l2)

(** performs the composition of two taggedtype cospans *)
let hg_cospan_compose (c : TaggedTypeCospan.t) ((c2, l2) : hg_cospan) :
    hg_cospan =
  (cospan_compose c c2, l2)

(** given a type, returns a cospan of the form: {s1 -> s1 <- s1, ..., sn -> sn <- sn}*)
let identity_cospan (t : sort list) =
  let index = ref (-1) in
  List.fold_left
    (fun cos s ->
      index := !index + 1;
      cos
      |> TaggedTypeCospan.add_elem_a ((!index, s), None)
      |> TaggedTypeCospan.add_elem_b ((!index, s), None)
      |> TaggedTypeCospan.add_elem_c ((!index, s), None)
      |> TaggedTypeCospan.add_pairing_l ((!index, s), None) ((!index, s), None)
      |> TaggedTypeCospan.add_pairing_r ((!index, s), None) ((!index, s), None))
    (TaggedTypeCospan.create ())
    t

(** given two types, produces a cospan corresponding to the string diagram in
    which these are swapped. *)
let swap_cospan (t1 : sort list) (t2 : sort list) =
  let t1len = List.length t1 in
  let t2len = List.length t2 in

  let ar = t1 @ t2 in
  let coar = t2 @ t1 in
  let index = ref (-1) in
  List.fold_left
    (fun cos _ ->
      index := !index + 1;

      let index_r = if !index < t2len then !index + t1len else !index - t2len in
      cos
      |> TaggedTypeCospan.add_elem_a ((!index, List.nth ar !index), None)
      |> TaggedTypeCospan.add_elem_b ((!index, List.nth ar !index), None)
      |> TaggedTypeCospan.add_elem_c ((!index, List.nth coar !index), None)
      |> TaggedTypeCospan.add_pairing_l
           ((!index, List.nth ar !index), None)
           ((!index, List.nth ar !index), None)
      |> TaggedTypeCospan.add_pairing_r
           ((!index, List.nth coar !index), None)
           ((index_r, List.nth coar !index), None))
    (TaggedTypeCospan.create ())
    ar

(** given a type, produces a cospan corresponding to discard(t) ; copy(t) *)
let discard_and_copy_cospan (t : sort list) =
  let n = List.length t in
  let index = ref (-1) in
  List.fold_left
    (fun cos s ->
      index := !index + 1;
      cos
      |> TaggedTypeCospan.add_elem_b ((!index, s), None)
      |> TaggedTypeCospan.add_elem_c ((!index, s), None)
      |> TaggedTypeCospan.add_elem_c ((!index + n, s), None)
      |> TaggedTypeCospan.add_pairing_r ((!index, s), None) ((!index, s), None)
      |> TaggedTypeCospan.add_pairing_r
           ((!index + n, s), None)
           ((!index, s), None))
    (TaggedTypeCospan.create ())
    t

(** Given a string diagram A -> B, produces a cospan A * B -> 1 that corresponds
    to it.*)
let rec cospan_of_circuit (c : circuit) =
  (* Printf.printf "converting to cospan: \n%s\n" (pp_circuit c); *)
  match c with
  | CId1 -> (TaggedTypeCospan.create (), [])
  | CId s ->
      ( TaggedTypeCospan.create ()
        |> TaggedTypeCospan.add_elem_a ((0, s), None)
        |> TaggedTypeCospan.add_elem_a ((1, s), None)
        |> TaggedTypeCospan.add_elem_b ((0, s), None)
        |> TaggedTypeCospan.add_pairing_l ((0, s), None) ((0, s), None)
        |> TaggedTypeCospan.add_pairing_l ((1, s), None) ((0, s), None),
        [] )
  | SwapTimes (s1, s2) ->
      ( TaggedTypeCospan.create ()
        |> TaggedTypeCospan.add_elem_a ((0, s1), None)
        |> TaggedTypeCospan.add_elem_a ((1, s2), None)
        |> TaggedTypeCospan.add_elem_a ((2, s2), None)
        |> TaggedTypeCospan.add_elem_a ((3, s1), None)
        |> TaggedTypeCospan.add_elem_b ((0, s2), None)
        |> TaggedTypeCospan.add_elem_b ((1, s1), None)
        |> TaggedTypeCospan.add_pairing_l ((0, s1), None) ((1, s1), None)
        |> TaggedTypeCospan.add_pairing_l ((1, s2), None) ((0, s2), None)
        |> TaggedTypeCospan.add_pairing_l ((2, s2), None) ((0, s2), None)
        |> TaggedTypeCospan.add_pairing_l ((3, s1), None) ((1, s1), None),
        [] )
  | Gen ("copy", [ s ], [ s1; s2 ], Relation)
  | Gen ("cocopy", [ s1; s2 ], [ s ], Relation)
    when s = s1 && s1 = s2 ->
      ( TaggedTypeCospan.create ()
        |> TaggedTypeCospan.add_elem_a ((0, s), None)
        |> TaggedTypeCospan.add_elem_a ((1, s), None)
        |> TaggedTypeCospan.add_elem_a ((2, s), None)
        |> TaggedTypeCospan.add_elem_b ((0, s), None)
        |> TaggedTypeCospan.add_pairing_l ((0, s), None) ((0, s), None)
        |> TaggedTypeCospan.add_pairing_l ((1, s), None) ((0, s), None)
        |> TaggedTypeCospan.add_pairing_l ((2, s), None) ((0, s), None),
        [] )
  | Gen ("discard", [ s ], [], Relation) | Gen ("codiscard", [], [ s ], Relation)
    ->
      ( TaggedTypeCospan.create ()
        |> TaggedTypeCospan.add_elem_a ((0, s), None)
        |> TaggedTypeCospan.add_elem_b ((0, s), None)
        |> TaggedTypeCospan.add_pairing_l ((0, s), None) ((0, s), None),
        [] )
  | Gen (name, ar, coar, k) ->
      let arcoar = ar @ coar in
      let idcospan = identity_cospan arcoar in
      ( idcospan,
        [
          {
            name;
            arity = arcoar;
            kind =
              (match k with
              | Function -> Function
              | NegRelation -> NegRelation
              | _ -> Relation);
          };
        ] )
  | Otimes (c1, c2) ->
      let n1 = Typecheck.circuit_arity c1 in
      let m1 = Typecheck.circuit_coarity c1 in
      let n2 = Typecheck.circuit_arity c2 in
      let m2 = Typecheck.circuit_coarity c2 in
      let idn1 = identity_cospan n1 in
      let swap = swap_cospan n2 m1 in
      let m2 = identity_cospan m2 in
      let c = cospan_tensor (cospan_tensor idn1 swap) m2 in
      hg_cospan_compose c
        (hg_cospan_tensor (cospan_of_circuit c1) (cospan_of_circuit c2))
  | CCompose (c1, c2) ->
      let n = Typecheck.circuit_arity c1 in
      let o1 = Typecheck.circuit_coarity c1 in
      let o2 = Typecheck.circuit_arity c2 in
      let m = Typecheck.circuit_coarity c2 in
      if o1 = o2 then
        let idn = identity_cospan n in
        let dco = discard_and_copy_cospan o1 in
        let idm = identity_cospan m in
        let c = cospan_tensor (cospan_tensor idn dco) idm in
        hg_cospan_compose c
          (hg_cospan_tensor (cospan_of_circuit c1) (cospan_of_circuit c2))
      else
        raise
          (Errors.TypeError
             "composition with non-matching arity and coarity in cospan \
              translation")
