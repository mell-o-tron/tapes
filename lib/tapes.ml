type gen_kind = Terms.gen_kind [@@deriving show]

type circuit =
  | CId of Terms.sort
  | CId1
  | Gen of string * Terms.sort list * Terms.sort list * gen_kind
  | CCompose of circuit * circuit
  | Otimes of circuit * circuit
  | SwapTimes of Terms.sort * Terms.sort
[@@deriving show]

type tape =
  | TId of Terms.sort list list (* da vedere *)
  | TId0
  | Tape of circuit
  | TCompose of tape * tape
  | Oplus of tape * tape
  | Trace of Terms.sort list * tape
  | SwapPlus of (Terms.sort list * Terms.sort list)
  | Cut of Terms.sort list
  | Split of Terms.sort list
  | Spawn of Terms.sort list
  | Join of Terms.sort list
[@@deriving show]

type sort = string

(* Pretty-print a list of sorts, e.g. ["a"; "b"] becomes: ["a", "b"] *)
let pp_sort_list (lst : string list) : string =
  "[" ^ String.concat ", " (List.map (fun s -> "\"" ^ s ^ "\"") lst) ^ "]"

(* Pretty-print a circuit *)
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

(* Pretty-print a tape *)
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

(* remove redundant identities *)
let rec clean_circuit (c : circuit) =
  match c with
  | CCompose (c1, c2) -> CCompose (clean_circuit c1, clean_circuit c2)
  | Otimes (c1, CId1) -> clean_circuit c1
  | Otimes (CId1, c2) -> clean_circuit c2
  | Otimes (c1, c2) -> Otimes (clean_circuit c1, clean_circuit c2)
  | _ -> c

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

let rec clean_tape (t : tape) =
  match t with
  | TCompose (TId0, TId0) -> TId0
  (* | TCompose (TId0, t1) -> t1
  | TCompose (t1, TId0) -> t1 *)
  | TCompose (t1, t2) -> TCompose (clean_tape t1, clean_tape t2)
  | Oplus (t1, TId0) -> clean_tape t1
  | Oplus (TId0, t2) -> clean_tape t2
  | Oplus (t1, t2) -> Oplus (clean_tape t1, clean_tape t2)
  | Tape c -> Tape (clean_circuit c)
  | Trace ([], t1) -> clean_tape t1
  | Trace (l, t) -> Trace (l, clean_tape t)
  | SwapPlus ([], []) -> TId0
  | SwapPlus ([], l) -> TId [ l ]
  | SwapPlus (l, []) -> TId [ l ]
  | TId [] -> TId0
  | _ -> t

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

(* Given a list of circuits, forms a polynomial with those circuits as monomials *)
let sum_monomials l = List.fold_left (fun acc m -> Oplus (acc, m)) TId0 l

(* Turns a string diagram identity into a circuit identity *)
let id_to_circuit l = List.fold_left (fun acc s -> Otimes (acc, CId s)) CId1 l

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

(* Turns an identity into the corresponding tape *)
let id_to_tape (l1 : sort list list) =
  List.fold_left (fun acc m -> Oplus (acc, Tape (id_to_circuit m))) TId0 l1

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

let swapplus_to_tape (p : sort list list) (q : sort list list) =
  unwrap_swapplus_tape p q

(* Using coherence axioms of commutative monoids - FP1 *)
let rec split_to_tape (l1 : sort list list) =
  match l1 with
  | [] -> TId0
  | x :: xs ->
      TCompose
        ( Oplus (Split x, split_to_tape xs),
          Oplus (TId [ x ], Oplus (swapplus_to_tape [ x ] xs, id_to_tape xs)) )

(* Using coherence axioms of cocommutative comonoids - FC1 *)
let rec join_to_tape (l1 : sort list list) =
  match l1 with
  | [] -> TId0
  | x :: xs ->
      TCompose
        ( Oplus (TId [ x ], Oplus (swapplus_to_tape xs [ x ], id_to_tape xs)),
          Oplus (Join x, join_to_tape xs) )

(* Using coherence axioms of commutative monoids - FP2 *)
let rec cut_to_tape (l1 : sort list list) =
  match l1 with [] -> TId0 | x :: xs -> Oplus (Cut x, cut_to_tape xs)

(* Using coherence axioms of cocommutative comonoids - FC2 *)
let rec spawn_to_tape (l1 : sort list list) =
  match l1 with [] -> TId0 | x :: xs -> Oplus (Spawn x, spawn_to_tape xs)

let rec multi_split n u =
  if n = 0 then Cut u
  else if n = 1 then TId [ u ]
  else if n = 2 then Split u
  else TCompose (Split u, Oplus (TId [ u ], multi_split (n - 1) u))

let rec multi_join n u =
  if n = 0 then Spawn u
  else if n = 1 then TId [ u ]
  else if n = 2 then Join u
  else TCompose (Oplus (TId [ u ], multi_join (n - 1) u), Join u)

let rec multi_join_pol n (p : string list list) =
  if n = 0 then spawn_to_tape p
  else if n = 1 then id_to_tape p
  else if n = 2 then join_to_tape p
  else TCompose (Oplus (id_to_tape p, multi_join_pol (n - 1) p), join_to_tape p)
