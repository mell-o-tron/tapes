open Ppx_compare_lib.Builtin
open Common_defs

type gen_kind = Terms.gen_kind [@@deriving show, compare]

(** type of string diagrams *)
type circuit =
  | CId of sort
  | CId1
  | Gen of string * sort list * sort list * gen_kind
  | CCompose of circuit * circuit
  | Otimes of circuit * circuit
  | SwapTimes of sort * sort
[@@deriving show, compare]

(** type of tape diagrams *)
type tape =
  | TId of sort list list
  | TId0
  | Tape of circuit
  | TCompose of tape * tape
  | Oplus of tape * tape
  | Trace of sort list * tape
  | SwapPlus of (sort list * sort list)
  | Cut of sort list
  | Split of sort list
  | Spawn of sort list
  | Join of sort list
[@@deriving show]

(** Pretty-print a circuit *)
let rec pp_circuit (c : circuit) : string =
  match c with
  | CId s -> Printf.sprintf "Id(\"%s\")" s
  | CId1 -> "Id1"
  | Gen (name, args, rets, kind) ->
      Printf.sprintf "%s(%s, [%s], [%s])" (show_gen_kind kind) name
        (String.concat ", " args) (String.concat ", " rets)
  | CCompose (c1, c2) ->
      Printf.sprintf "(%s ; %s)" (pp_circuit c1) (pp_circuit c2)
  | Otimes (c1, c2) ->
      Printf.sprintf "(%s ⊗  %s)" (pp_circuit c1) (pp_circuit c2)
  | SwapTimes (s1, s2) -> Printf.sprintf "σ⊗(\"%s\", \"%s\")" s1 s2

(** Pretty-print a tape *)
let rec pp_tape (t : tape) : string =
  match t with
  | TId lsts ->
      (* Each element of lsts is a list of sorts *)
      Printf.sprintf "Id(%s)" (String.concat "; " (List.map pp_sort_list lsts))
  | TId0 -> "Id0"
  | Tape c -> Printf.sprintf "Tape(%s)" (pp_circuit c)
  | TCompose (t1, t2) -> Printf.sprintf "(%s ; %s)" (pp_tape t1) (pp_tape t2)
  | Oplus (t1, t2) -> Printf.sprintf "(%s ⊕  %s)" (pp_tape t1) (pp_tape t2)
  | SwapPlus (lst1, lst2) ->
      Printf.sprintf "σ⊕(%s, %s)" (pp_sort_list lst1) (pp_sort_list lst2)
  | Cut lst -> Printf.sprintf "Cut(%s)" (pp_sort_list lst)
  | Split lst -> Printf.sprintf "Split(%s)" (pp_sort_list lst)
  | Spawn lst -> Printf.sprintf "Spawn(%s)" (pp_sort_list lst)
  | Join lst -> Printf.sprintf "Join(%s)" (pp_sort_list lst)
  | Trace (lst, t) ->
      Printf.sprintf "Tr_{%s}(%s)" (pp_sort_list lst) (pp_tape t)

(** debug utility *)
let find_and_print_gen name (t : tape) : unit =
  let rec circuit_find (c : circuit) : gen_kind option =
    match c with
    | CId _ | CId1 -> None
    | Gen (gname, _, _, kind) -> if gname = name then Some kind else None
    | CCompose (c1, c2) | Otimes (c1, c2) -> (
        match circuit_find c1 with Some k -> Some k | None -> circuit_find c2)
    | SwapTimes _ -> None
  in
  let rec tape_find (t : tape) : gen_kind option =
    match t with
    | TId _ | TId0 | Cut _ | Split _ | Spawn _ | Join _ -> None
    | Tape c -> circuit_find c
    | TCompose (t1, t2) | Oplus (t1, t2) -> (
        match tape_find t1 with Some k -> Some k | None -> tape_find t2)
    | Trace (_, t') -> tape_find t'
    | SwapPlus _ -> None
  in
  match tape_find t with
  | Some kind -> print_endline (show_gen_kind kind)
  | None -> print_endline "not found"

(* basic circuit simplification *)
let rec clean_circuit (c : circuit) =
  match c with
  | CCompose (c1, c2) -> CCompose (clean_circuit c1, clean_circuit c2)
  | Otimes (c1, CId1) -> clean_circuit c1
  | Otimes (CId1, c2) -> clean_circuit c2
  | Otimes (c1, c2) -> Otimes (clean_circuit c1, clean_circuit c2)
  | _ -> c

(** simplifies a circuit recursively and until reaching a fixpoint.*)
let deep_clean_circuit (c : circuit) =
  let rec aux (c : circuit) =
    let c1 = clean_circuit c in
    if c = c1 then c else aux c1
  in
  aux c

(* let rec clean_tape (t : tape) =
  match t with
  | TCompose (TId0, TId0) -> TId0
  | TCompose (t1, t2) -> TCompose (clean_tape t1, clean_tape t2)
  | Oplus (t1, TId0) -> clean_tape t1
  | Oplus (TId0, t2) -> clean_tape t2
  | Oplus (t1, t2) -> Oplus (clean_tape t1, clean_tape t2)
  | Tape c -> Tape (clean_circuit c)
  | Trace (l, t) -> Trace (l, clean_tape t)
  | _ -> t *)

(** Simplifies a tape recursively. Use [deep_clean_tape] for the fixpointed
    version *)
let rec clean_tape (t : tape) =
  match t with
  | TCompose (TId0, TId0) -> TId0
  (* | TCompose (TId0, t1) -> t1
  | TCompose (t1, TId0) -> t1 *)
  | TCompose (t1, t2) -> TCompose (clean_tape t1, clean_tape t2)
  | Oplus (t1, TId0) -> clean_tape t1
  | Oplus (TId0, t2) -> clean_tape t2
  | Oplus (t1, t2) -> Oplus (clean_tape t1, clean_tape t2)
  | Tape c -> Tape (deep_clean_circuit c)
  | Trace ([], t1) -> clean_tape t1
  | Trace (l, t) -> Trace (l, clean_tape t)
  (* | SwapPlus ([], []) ->
      Printf.printf "boom!\n";
      TId0
  | SwapPlus ([], l) -> TId [ l ]
  | SwapPlus (l, []) -> TId [ l ] *)
  | TId [] -> TId0
  | _ -> t

(** Simplifies a tape recursively and until reaching a fixpoint. *)
let deep_clean_tape (t : tape) =
  let rec fix_ids (t : tape) =
    match t with
    | TCompose (t1, t2) -> TCompose (fix_ids t1, fix_ids t2)
    | Oplus (t1, t2) -> Oplus (fix_ids t1, fix_ids t2)
    | Trace (l, t1) -> Trace (l, fix_ids t1)
    (* | Tape (CId l) -> TId [ [ l ] ] *)
    | Tape CId1 -> TId [ [] ]
    | _ -> t
  in

  let rec aux (t : tape) =
    let t1 = clean_tape t in
    if t = t1 then t else aux t1
  in
  fix_ids (aux t)

(** Given a list of circuits, forms a polynomial with those circuits as
    monomials *)
let sum_monomials l = List.fold_left (fun acc m -> Oplus (acc, m)) TId0 l

(** Given a sort list, generates a circuit identity *)
let id_to_circuit l = List.fold_left (fun acc s -> Otimes (acc, CId s)) CId1 l

(** constructs generalized circuit swap *)
let rec unwrap_swaptimes_circuit (l1 : sort list) (l2 : sort list) =
  match (l1, l2) with
  | [], u -> id_to_circuit u
  | w, [] -> id_to_circuit w
  | a :: w, b :: u ->
      CCompose
        ( CCompose
            ( Otimes (CId a, unwrap_swaptimes_circuit w (b :: u)),
              Otimes (SwapTimes (a, b), id_to_circuit (u @ w)) ),
          Otimes
            (Otimes (CId b, unwrap_swaptimes_circuit [ a ] u), id_to_circuit w)
        )

(** Turns an identity into the corresponding tape *)
let id_to_tape (l1 : sort list list) =
  List.fold_left (fun acc m -> Oplus (acc, Tape (id_to_circuit m))) TId0 l1

(** constructs generalized tape swap*)
let rec unwrap_swapplus_tape (l1 : sort list list) (l2 : sort list list) =
  match (l1, l2) with
  | [], u -> id_to_tape u
  | w, [] -> id_to_tape w
  | a :: w, b :: u ->
      TCompose
        ( TCompose
            ( Oplus (TId [ a ], unwrap_swapplus_tape w (b :: u)),
              Oplus (SwapPlus (a, b), id_to_tape (u @ w)) ),
          Oplus (Oplus (TId [ b ], unwrap_swapplus_tape [ a ] u), id_to_tape w)
        )

(** constructs polynomial tape swap, alias for [unwrap_swapplus_tape]*)
let swapplus_to_tape (p : sort list list) (q : sort list list) =
  let res = unwrap_swapplus_tape p q in
  (* Printf.printf "%s\n" (pp_tape res); *)
  res

(* given a [string list list], it constructs the corresponding split, using coherence axioms of commutative monoids - FP1 *)
let rec split_to_tape (l1 : sort list list) =
  match l1 with
  | [] -> TId0
  | x :: xs ->
      TCompose
        ( Oplus (Split x, split_to_tape xs),
          Oplus (TId [ x ], Oplus (swapplus_to_tape [ x ] xs, id_to_tape xs)) )

(* given a [string list list], it constructs the corresponding join, using coherence axioms of cocommutative comonoids - FC1 *)
let rec join_to_tape (l1 : sort list list) =
  match l1 with
  | [] -> TId0
  | x :: xs ->
      TCompose
        ( Oplus (TId [ x ], Oplus (swapplus_to_tape xs [ x ], id_to_tape xs)),
          Oplus (Join x, join_to_tape xs) )

(** given a [string list list], it constructs the corresponding cut, using
    coherence axioms of commutative monoids - FP2 *)
let rec cut_to_tape (l1 : sort list list) =
  match l1 with [] -> TId0 | x :: xs -> Oplus (Cut x, cut_to_tape xs)

(** given a [string list list], it constructs the corresponding spawn, using
    coherence axioms of cocommutative comonoids - FC2 *)
let rec spawn_to_tape (l1 : sort list list) =
  match l1 with [] -> TId0 | x :: xs -> Oplus (Spawn x, spawn_to_tape xs)

(** n-ary split *)
let rec multi_split n u =
  if n = 0 then Cut u
  else if n = 1 then TId [ u ]
  else if n = 2 then Split u
  else TCompose (Split u, Oplus (TId [ u ], multi_split (n - 1) u))

(** n-ary join *)
let rec multi_join n u =
  if n = 0 then Spawn u
  else if n = 1 then TId [ u ]
  else if n = 2 then Join u
  else TCompose (Oplus (TId [ u ], multi_join (n - 1) u), Join u)

(** n-ary join, for polynomials *)
let rec multi_join_pol n (p : string list list) =
  if n = 0 then spawn_to_tape p
  else if n = 1 then id_to_tape p
  else if n = 2 then join_to_tape p
  else TCompose (Oplus (id_to_tape p, multi_join_pol (n - 1) p), join_to_tape p)

let invert_generator s ar coar (kind : gen_kind) =
  let t0 = Otimes (id_to_circuit coar, Gen ("codiscard", [], ar, Relation)) in
  let t1 = Otimes (id_to_circuit coar, Gen ("copy", ar, ar @ ar, Relation)) in
  let t2 =
    Otimes
      ( Otimes
          ( id_to_circuit coar,
            Gen
              ( s,
                ar,
                coar,
                match kind with
                | NegRelation -> NegRelation
                | Function -> Function
                | _ -> Relation ) ),
        id_to_circuit ar )
  in
  let t3 =
    Otimes (Gen ("cocopy", coar @ coar, coar, Relation), id_to_circuit ar)
  in
  let t4 = Otimes (Gen ("discard", coar, [], Relation), id_to_circuit ar) in
  CCompose (t0, CCompose (t1, CCompose (t2, CCompose (t3, t4))))

(** Computes the inverse of a circuit *)
let rec circuit_inverse (c : circuit) =
  match c with
  | CId _ | CId1 -> c
  (* the inverse of a function is, in general, a relation. *)
  | Gen (s, ar, coar, kind) -> invert_generator s ar coar kind
  | CCompose (c1, c2) -> CCompose (circuit_inverse c2, circuit_inverse c1)
  | Otimes (c1, c2) -> Otimes (circuit_inverse c1, circuit_inverse c2)
  | SwapTimes (s1, s2) -> SwapTimes (s2, s1)

(** Computes the inverse of a tape*)
let rec tape_inverse (t : tape) =
  match t with
  | TId0 | TId _ -> t
  | Tape c -> Tape (circuit_inverse c)
  | SwapPlus (a, b) -> SwapPlus (b, a)
  | TCompose (t1, t2) -> TCompose (tape_inverse t2, tape_inverse t1)
  | Oplus (t1, t2) -> Oplus (tape_inverse t1, tape_inverse t2)
  | Spawn l -> Cut l
  | Cut l -> Spawn l
  | Join l -> Split l
  | Split l -> Join l
  | Trace (l, t1) -> Trace (l, tape_inverse t1)
