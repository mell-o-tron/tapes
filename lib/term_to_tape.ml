open Terms
open Tapes
open Common_defs

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
  | TId s -> TId (List.map (fun x -> u @ x) s)
  | Tape c -> Tape (Otimes (id_to_circuit u, c))
  | SwapPlus (v, w) -> SwapPlus (u @ v, u @ w)
  | TCompose (t1, t2) ->
      TCompose (left_whiskering_mon u t1, left_whiskering_mon u t2)
  | Oplus (t1, t2) -> Oplus (left_whiskering_mon u t1, left_whiskering_mon u t2)
  | Split v -> Split (u @ v)
  | Join v -> Join (u @ v)
  | Cut v -> Cut (u @ v)
  | Spawn v -> Spawn (u @ v)
  | Trace (v, t) -> Trace (u @ v, left_whiskering_mon u t)

let rec right_whiskering_mon (u : sort list) (t : tape) =
  match t with
  | TId0 -> TId0
  | TId s -> TId (List.map (fun x -> x @ u) s)
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
  | Trace (v, t) -> Trace (v @ u, right_whiskering_mon u t)

let rec left_whiskering (u : sort list list) (t : tape) =
  match u with
  | [] -> TId0
  | a :: rest -> Oplus (left_whiskering_mon a t, left_whiskering rest t)

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

let otimes_to_tape (t1 : tape) (t2 : tape) =
  let p = Typecheck.tape_arity t1 in
  let s = Typecheck.tape_coarity t2 in
  TCompose (left_whiskering p t2, right_whiskering s t1)

let rec copy_monomial_to_circuit (l : sort list) : circuit =
  match l with
  | [] -> CId1
  | x :: xs ->
      CCompose
        ( Otimes
            ( Gen ("copy", [ x ], [ x; x ], Relation),
              copy_monomial_to_circuit xs ),
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
          Otimes
            ( Gen ("cocopy", [ x; x ], [ x ], Relation),
              cocopy_monomial_to_circuit xs ) )

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
  let l = List.map (fun x -> Gen ("discard", [ x ], [], Relation)) l in
  List.fold_right (fun x y -> Otimes (x, y)) l CId1

let discard_to_tape (l : sort list list) : tape =
  let l = List.map (fun x -> Tape (discard_to_tape_mon x)) l in
  let res = List.fold_right (fun x y -> Oplus (x, y)) l TId0 in
  res |> deep_clean_tape

let codiscard_to_tape_mon (l : sort list) : circuit =
  let l = List.map (fun x -> Gen ("codiscard", [], [ x ], Relation)) l in
  List.fold_right (fun x y -> Otimes (x, y)) l CId1

let codiscard_to_tape (l : sort list list) : tape =
  let l = List.map (fun x -> Tape (codiscard_to_tape_mon x)) l in
  List.fold_right (fun x y -> Oplus (x, y)) l TId0 |> deep_clean_tape

(** Transforms polynomial trace of term to nested monomial trace of tape *)
let iterate_trace (l : sort list list) (t : tape) : tape =
  List.fold_left (fun t1 u -> Trace (u, t1)) t (List.rev l)

(** converts term into tape (when possible) *)
let rec _to_tape (t : term) =
  match t with
  | Id l -> id_to_tape l
  | SwapTimes (p, q) -> swaptimes_to_tape p q
  | SwapPlus (p, q) -> swapplus_to_tape p q
  | Ldistr (p, q, r) -> ldistr_to_tape p q r
  | Otimes (t1, t2) -> otimes_to_tape (_to_tape t1) (_to_tape t2)
  | Oplus (t1, t2) -> Oplus (_to_tape t1, _to_tape t2)
  | Compose (t1, t2) -> TCompose (_to_tape t1, _to_tape t2)
  | Gen (s, ar, coar, kind) -> (
      match (ar, coar) with
      | [], [] -> Tape (Gen (s, [], [], kind))
      | [ a ], [ b ] -> Tape (Gen (s, a, b, kind))
      | [ a ], [] -> Tape (Gen (s, a, [], kind))
      | [], [ b ] -> Tape (Gen (s, [], b, kind))
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
  | Trace (l, t) -> iterate_trace l (_to_tape t)
  | Discard l -> discard_to_tape l
  | CoDiscard l -> codiscard_to_tape l
