open Terms
open Tapes

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

(* Definition 4.5 of technical report *)
let rec ldistr_to_tape (p : sort list list) (q : sort list list)
    (r : sort list list) =
  match p with
  | [] -> TId0
  | u :: p1 ->
      TCompose
        ( Oplus
            ( id_to_tape
                (obj_to_polynomial
                   (Obtimes
                      ( obj_of_polynomial [ u ],
                        Obplus (obj_of_polynomial q, obj_of_polynomial r) ))),
              ldistr_to_tape p1 q r ),
          Oplus
            ( Oplus
                ( id_to_tape
                    (obj_to_polynomial
                       (Obtimes (obj_of_polynomial [ u ], obj_of_polynomial q))),
                  swapplus_to_tape
                    (obj_to_polynomial
                       (Obtimes (obj_of_polynomial [ u ], obj_of_polynomial r)))
                    (obj_to_polynomial
                       (Obtimes (obj_of_polynomial p1, obj_of_polynomial q))) ),
              id_to_tape
                (obj_to_polynomial
                   (Obtimes (obj_of_polynomial p1, obj_of_polynomial r))) ) )

(* Definition 4.6 of technical report *)
let rec swaptimes_to_tape (p : sort list list) (q : sort list list) =
  match q with
  | [] -> TId0
  | m :: q1 ->
      let sum_of_swaps =
        sum_monomials
          (List.map (fun ui -> Tape (unwrap_swaptimes_circuit ui m)) p)
      in
      TCompose
        (ldistr_to_tape p [ m ] q1, Oplus (sum_of_swaps, swaptimes_to_tape p q1))

let rec left_whiskering_mon (u : sort list) (t : tape) =
  match t with
  | TId0 -> TId0
  | TId s ->
      TId (List.map (fun x -> u @ x) s)
      (* CHECK this case is missing in tech report *)
  | Tape c -> Tape (Otimes (id_to_circuit u, c))
  | SwapPlus (v, w) -> SwapPlus (u @ v, u @ w)
  | TCompose (t1, t2) ->
      TCompose (left_whiskering_mon u t1, left_whiskering_mon u t2)
  | Oplus (t1, t2) -> Oplus (left_whiskering_mon u t1, left_whiskering_mon u t2)
  | Split v -> Split (u @ v)
  | Join v -> Join (u @ v)
  | Cut v -> Cut (u @ v)
  | Spawn v -> Spawn (u @ v)
  | Trace t -> Trace (left_whiskering_mon u t)

let rec right_whiskering_mon (u : sort list) (t : tape) =
  match t with
  | TId0 -> TId0
  | TId s ->
      TId (List.map (fun x -> x @ u) s)
      (* CHECK this case is missing in tech report *)
  | Tape c -> Tape (Otimes (c, id_to_circuit u))
  | SwapPlus (v, w) -> SwapPlus (v @ u, w @ u)
  | TCompose (t1, t2) ->
      TCompose (right_whiskering_mon u t1, right_whiskering_mon u t2)
  | Oplus (t1, t2) ->
      Oplus (right_whiskering_mon u t1, right_whiskering_mon u t2)
  | Split v -> Split (v @ u)
  | Join v -> Join (v @ u)
  | Cut v -> Cut (v @ u)
  | Spawn v -> Spawn (v @ u)
  | Trace t -> Trace (right_whiskering_mon u t)

let rec left_whiskering (u : sort list list) (t : tape) =
  match u with
  | [] -> TId0
  | a :: rest -> Oplus (left_whiskering_mon a t, left_whiskering rest t)

let rec circuit_inverse (c : circuit) =
  match c with
  | CId _ | CId1 -> c
  | Gen (s, ar, coar) -> Gen (s ^ "$^{-1}$", coar, ar)
  | CCompose (c1, c2) -> CCompose (circuit_inverse c2, circuit_inverse c1)
  | Otimes (c1, c2) -> Otimes (circuit_inverse c1, circuit_inverse c2)
  | SwapTimes (s1, s2) -> SwapTimes (s2, s1)

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
  | Trace t1 -> Trace (tape_inverse t1)

let rec right_whiskering (u : sort list list) (t : tape) =
  let p = Typecheck.tape_arity t in
  let q = Typecheck.tape_coarity t in
  let dl w s = ldistr_to_tape p [ w ] s in
  let inv_dl w s = tape_inverse (ldistr_to_tape q [ w ] s) in
  match u with
  | [] -> TId0
  | a :: rest ->
      TCompose
        ( dl a rest,
          TCompose
            ( Oplus (right_whiskering_mon a t, right_whiskering rest t),
              inv_dl a rest ) )

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

let otimes_to_tape (t1 : tape) (t2 : tape) =
  let p = Typecheck.tape_arity t1 in
  let s = Typecheck.tape_coarity t2 in
  TCompose (left_whiskering p t2, right_whiskering s t1)

let rec copy_monomial_to_circuit (l : sort list) : circuit =
  match l with
  | [] -> CId1
  | x :: xs ->
      CCompose
        ( Otimes (Gen ("copy", [ x ], [ x; x ]), copy_monomial_to_circuit xs),
          Otimes
            (CId x, Otimes (unwrap_swaptimes_circuit [ x ] xs, id_to_circuit xs))
        )

let rec copy_to_tape (l : sort list list) : tape =
  match l with
  | [] -> TId0
  | u :: p1 ->
      Oplus
        ( Oplus
            ( Tape (copy_monomial_to_circuit u),
              spawn_to_tape (times_on_objects [ u ] p1) ),
          TCompose
            ( Oplus (spawn_to_tape (times_on_objects p1 [ u ]), copy_to_tape p1),
              tape_inverse (ldistr_to_tape p1 [ u ] p1) ) )

let rec cocopy_monomial_to_circuit (l : sort list) : circuit =
  match l with
  | [] -> CId1
  | x :: xs ->
      CCompose
        ( Otimes
            (CId x, Otimes (unwrap_swaptimes_circuit xs [ x ], id_to_circuit xs)),
          Otimes (Gen ("cocopy", [ x; x ], [ x ]), cocopy_monomial_to_circuit xs)
        )

let rec cocopy_to_tape (l : sort list list) : tape =
  match l with
  | [] -> TId0
  | u :: p1 ->
      Oplus
        ( Oplus
            ( Tape (cocopy_monomial_to_circuit u),
              cut_to_tape (times_on_objects [ u ] p1) ),
          TCompose
            ( ldistr_to_tape p1 [ u ] p1,
              Oplus (cut_to_tape (times_on_objects p1 [ u ]), cocopy_to_tape p1)
            ) )

let discard_to_tape_mon (l : sort list) : circuit =
  let l = List.map (fun x -> Gen ("discard", [ x ], [])) l in
  List.fold_right (fun x y -> Otimes (x, y)) l CId1

let discard_to_tape (l : sort list list) : tape =
  let l = List.map (fun x -> Tape (discard_to_tape_mon x)) l in
  List.fold_right (fun x y -> Oplus (x, y)) l TId0 |> deep_clean_tape

let codiscard_to_tape_mon (l : sort list) : circuit =
  let l = List.map (fun x -> Gen ("codiscard", [], [ x ])) l in
  List.fold_right (fun x y -> Otimes (x, y)) l CId1

let codiscard_to_tape (l : sort list list) : tape =
  let l = List.map (fun x -> Tape (codiscard_to_tape_mon x)) l in
  List.fold_right (fun x y -> Oplus (x, y)) l TId0 |> deep_clean_tape

(* converts term into tape (when possible) *)
let rec _to_tape (t : term) =
  match t with
  | Id l -> id_to_tape l
  | SwapTimes (p, q) -> swaptimes_to_tape p q
  | SwapPlus (p, q) -> swapplus_to_tape p q
  | Ldistr (p, q, r) -> ldistr_to_tape p q r
  | Otimes (t1, t2) -> otimes_to_tape (_to_tape t1) (_to_tape t2)
  | Oplus (t1, t2) -> Oplus (_to_tape t1, _to_tape t2)
  | Compose (t1, t2) -> TCompose (_to_tape t1, _to_tape t2)
  | Gen (s, ar, coar) -> (
      match (ar, coar) with
      | [], [] -> Tape (Gen (s, [], []))
      | [ a ], [ b ] -> Tape (Gen (s, a, b))
      | [ a ], [] -> Tape (Gen (s, a, []))
      | [], [ b ] -> Tape (Gen (s, [], b))
      | _ ->
          raise
            (Errors.RuntimeError
               "cannot transform non-monomial generator to a tape"))
  | GenVar v ->
      raise (Errors.RuntimeError (Printf.sprintf "generator %s not found" v))
  | Split l -> split_to_tape l
  | Join l -> join_to_tape l
  | Spawn l -> spawn_to_tape l
  | Cut l -> cut_to_tape l
  | Copy l -> copy_to_tape l
  | CoCopy l -> cocopy_to_tape l
  | Trace t -> Trace (_to_tape t)
  | Discard l -> discard_to_tape l
  | CoDiscard l -> codiscard_to_tape l

(*  copy & discard etc
      Definitions (20), (21) under theorem 7.3 (for polynomials)
      for monomials, use coherence axioms
  *)

(*
      split, join, cut, spawn
  *)
