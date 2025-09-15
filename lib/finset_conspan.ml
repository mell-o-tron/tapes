(** The type of an element equipped with a tag *)
module type TaggedType = sig
  type t

  val compare : t -> t -> int
  val get_tag : t -> int
  val set_tag : t -> int -> t
  val discard_tag : t -> t
  val to_string : t -> string
end

module type Cospan = sig
  type elt
  type t

  val create : unit -> t
  val add_elem_a : elt -> t -> t
  val add_elem_b : elt -> t -> t
  val add_elem_c : elt -> t -> t
  val add_pairing_l : elt -> elt -> t -> t
  val add_pairing_r : elt -> elt -> t -> t
  val tensor : t -> t -> t
  val compose : t -> t -> t
  val equal : t -> t -> bool
end

module Make (Tagged : TaggedType) = struct
  type elt = Tagged.t

  module Taggedset = Set.Make (Tagged)
  module Taggedmap = Map.Make (Tagged)
  module UF = UnionFind.Make (UnionFind.StoreRef)

  type t = {
    a : Taggedset.t;
    b : Taggedset.t;
    c : Taggedset.t;
    l : elt Taggedmap.t;
    r : elt Taggedmap.t;
  }

  let string_of_elem_list l = List.map Tagged.to_string l |> String.concat ", "

  let string_of_pair_list l =
    List.map
      (fun (a, b) -> "(" ^ Tagged.to_string a ^ ", " ^ Tagged.to_string b ^ ")")
      l
    |> String.concat ","

  (** Creates a string that represents the cospan *)
  let to_string t =
    Printf.sprintf
      "{\n\
      \    \t a = {%s}\n\
      \    \t b = {%s}\n\
      \    \t c = {%s}\n\
      \    \t l = {%s}\n\
      \    \t r = {%s}\n\
       }"
      (string_of_elem_list (Taggedset.to_list t.a))
      (string_of_elem_list (Taggedset.to_list t.b))
      (string_of_elem_list (Taggedset.to_list t.c))
      (string_of_pair_list (Taggedmap.to_list t.l))
      (string_of_pair_list (Taggedmap.to_list t.r))

  (** Creates an empty cospan *)
  let create : unit -> t =
   fun () ->
    {
      a = Taggedset.empty;
      b = Taggedset.empty;
      c = Taggedset.empty;
      l = Taggedmap.empty;
      r = Taggedmap.empty;
    }

  (** Adds an element to the leftmost set. *)
  let add_elem_a : elt -> t -> t =
   fun elem cosp ->
    { cosp with a = Taggedset.add (Tagged.discard_tag elem) cosp.a }

  (** Adds an element to the center set. *)
  let add_elem_b : elt -> t -> t =
   fun elem cosp ->
    { cosp with b = Taggedset.add (Tagged.discard_tag elem) cosp.b }

  (** Adds an element to the rightmost set. *)
  let add_elem_c : elt -> t -> t =
   fun elem cosp ->
    { cosp with c = Taggedset.add (Tagged.discard_tag elem) cosp.c }

  (** Adds a pairing to the left interface. *)
  let add_pairing_l : elt -> elt -> t -> t =
   fun elem1 elem2 cosp ->
    {
      cosp with
      l =
        Taggedmap.add (Tagged.discard_tag elem1) (Tagged.discard_tag elem2)
          cosp.l;
    }

  (** Adds a pairing to the right interface. *)
  let add_pairing_r : elt -> elt -> t -> t =
   fun elem1 elem2 cosp ->
    {
      cosp with
      r =
        Taggedmap.add (Tagged.discard_tag elem1) (Tagged.discard_tag elem2)
          cosp.r;
    }

  (** Performs the disjoint union of to sets of tagged elements, using the tags
      to preserve the origin of each element. *)
  let disjoint_union s1 s2 =
    let labeled_set_1 = Taggedset.map (fun x -> Tagged.set_tag x 0) s1 in
    let labeled_set_2 = Taggedset.map (fun x -> Tagged.set_tag x 1) s2 in
    Taggedset.union labeled_set_1 labeled_set_2

  let discard_set_tags s =
    Taggedset.fold
      (fun x acc -> Taggedset.add (Tagged.discard_tag x) acc)
      s Taggedset.empty

  let drop_tags (cosp : t) : t =
    let drop_set_tags s =
      Taggedset.fold
        (fun x acc -> Taggedset.add (Tagged.discard_tag x) acc)
        s Taggedset.empty
    in
    let drop_map_tags m =
      Taggedmap.fold
        (fun k v acc ->
          Taggedmap.add (Tagged.discard_tag k) (Tagged.discard_tag v) acc)
        m Taggedmap.empty
    in
    {
      a = drop_set_tags cosp.a;
      b = drop_set_tags cosp.b;
      c = drop_set_tags cosp.c;
      l = drop_map_tags cosp.l;
      r = drop_map_tags cosp.r;
    }

  (** Vertical composition of cospans. Discards tags after performing the
      computation; make sure that there are no overlapping elements in the sets
      of the two cospans, otherwise spurious interfaces shall arise. *)
  let tensor (t1 : t) (t2 : t) =
    let funsum (domain : Taggedset.t) (f1 : elt Taggedmap.t)
        (f2 : elt Taggedmap.t) : elt Taggedmap.t =
      let assign_sum_value el =
        let tag = Tagged.get_tag el in
        try
          (* Printf.printf "looking for %s in %s or %s\n" (Tagged.to_string el)
            (string_of_pair_list (Taggedmap.to_list f1))
            (string_of_pair_list (Taggedmap.to_list f2)); *)
          if tag = 0 then Taggedmap.find (Tagged.discard_tag el) f1
          else Taggedmap.find (Tagged.discard_tag el) f2
        with Not_found ->
          failwith "map element was not found in assign_sum_value"
      in
      Taggedset.fold
        (fun elem (acc : elt Taggedmap.t) ->
          Taggedmap.add elem (assign_sum_value elem) acc)
        domain Taggedmap.empty
    in

    let a1 = disjoint_union t1.a t2.a in
    let b1 = disjoint_union t1.b t2.b in
    let c1 = disjoint_union t1.c t2.c in
    let l = funsum a1 t1.l t2.l in
    let r = funsum c1 t1.r t2.r in

    {
      a = a1 |> Taggedset.map Tagged.discard_tag;
      b = b1 |> Taggedset.map Tagged.discard_tag;
      c = c1 |> Taggedset.map Tagged.discard_tag;
      l =
        l |> Taggedmap.to_list
        |> List.map (fun (x, y) -> (Tagged.discard_tag x, Tagged.discard_tag y))
        |> Taggedmap.of_list;
      r =
        r |> Taggedmap.to_list
        |> List.map (fun (x, y) -> (Tagged.discard_tag x, Tagged.discard_tag y))
        |> Taggedmap.of_list;
    }

  (** equality check for cospans *)
  let equal (t1 : t) (t2 : t) =
    Taggedmap.equal (fun x y -> Tagged.compare x y = 0) t1.r t2.r
    && Taggedmap.equal (fun x y -> Tagged.compare x y = 0) t1.l t2.l
    && Taggedset.equal t1.a t2.a && Taggedset.equal t1.b t2.b
    && Taggedset.equal t1.c t2.c

  let seq_to_string (to_string : 'a -> string) (s : 'a Seq.t) : string =
    let strs = Seq.map to_string s |> List.of_seq in
    "[" ^ String.concat "; " strs ^ "]"

  (** horizontal composition of cospans. Does NOT discard the tags after the
      computation; make sure to discard them after calling this function. *)
  let compose (t1 : t) (t2 : t) =
    (* check for composability c1 == a2*)
    if not (Taggedset.equal t1.c t2.a) then
      raise
        (Errors.TypeError "incompatible interfaces, cannot compose cospans.")
    else
      let uf = UF.new_store () in
      (* A1 -> B1 <- X      X -> B2 <- C2. Take disjoint union of B1 and B2 *)
      let d_lis = disjoint_union t1.b t2.b |> Taggedset.to_list in
      (* Printf.printf "disjoint union: %s\n" (string_of_elem_list d_lis); *)
      let d_hash : (elt, elt UF.rref) Hashtbl.t =
        Hashtbl.create (List.length d_lis)
      in
      (* add each element of d to the union_find *)
      List.iter (fun x -> Hashtbl.add d_hash x (UF.make uf x)) d_lis;
      let t1_lis = Taggedset.to_list t1.c in

      (* for each element el of t1.c perform the union between (push_tag g( pop_tag el) 0) and (push_tag h( pop_tag el) 1) *)
      List.iter
        (fun el ->
          (* Printf.printf "looking for %s in cospan 1, right map: %s\n"
            (Tagged.to_string (Tagged.discard_tag el))
            (string_of_pair_list (Taggedmap.to_list t1.r)); *)
          (* an element coming from the left is tagged with zero *)
          let x1 =
            Tagged.set_tag (Taggedmap.find (Tagged.discard_tag el) t1.r) 0
          in
          (* Printf.printf "looking for %s in cospan 2, left map: %s\n"
            (Tagged.to_string (Tagged.discard_tag el))
            (string_of_pair_list (Taggedmap.to_list t2.l)); *)
          (* an element coming from the right is tagged with one *)
          let x2 =
            Tagged.set_tag (Taggedmap.find (Tagged.discard_tag el) t2.l) 1
          in
          (* let keys = Hashtbl.to_seq_keys d_hash in
          Printf.printf "looking for %s and %s; keys in the hashtable: %s\n"
            (Tagged.to_string x1) (Tagged.to_string x2)
            (seq_to_string Tagged.to_string keys); *)
          let _ =
            UF.union uf (Hashtbl.find d_hash x1) (Hashtbl.find d_hash x2)
          in
          ())
        t1_lis;

      (* add the representatives of each equivalence class to the pushout *)
      let pushout = ref Taggedset.empty in
      Hashtbl.iter
        (fun _ r ->
          pushout := Taggedset.add (UF.get uf (UF.find uf r)) !pushout)
        d_hash;

      (* we now need to adjust the left and right maps by substituting each value with its representative *)
      let newl =
        Taggedmap.map
          (fun a ->
            (* an element coming from the left is tagged with zero *)
            let v = Tagged.set_tag a 0 in
            let r = Hashtbl.find d_hash v in
            UF.get uf (UF.find uf r))
          t1.l
      in

      let newr =
        Taggedmap.map
          (fun a ->
            (* an element coming from the right is tagged with one *)
            let v = Tagged.set_tag a 1 in
            let r = Hashtbl.find d_hash v in
            UF.get uf (UF.find uf r))
          t2.r
      in

      { a = t1.a; b = !pushout; c = t2.c; l = newl; r = newr }
end
