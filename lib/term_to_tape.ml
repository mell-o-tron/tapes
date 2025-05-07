open Terms
open Tapes

(* Given a list of circuits, forms a polynomial with those circuits as monomials *)
let sum_monomials (l) = List.fold_left (fun acc m -> Oplus(acc, m)) (TId0) l

(* Turns a string diagram identity into a circuit identity *)
let id_to_circuit l = List.fold_left (fun acc s -> Otimes(acc, CId(s))) (CId1) l

let rec unwrap_swaptimes_circuit (l1 : sort list) (l2 : sort list) = match (l1, l2) with
  | [], u       -> id_to_circuit (u)
  | w, []       -> id_to_circuit (w)
  | a::w, b::u  -> CCompose(
                    CCompose(
                      Otimes(
                        CId(a),
                        unwrap_swaptimes_circuit(w) (b::u)),
                      Otimes(
                        SwapTimes(a, b),
                        id_to_circuit(u@w))),
                      Otimes(
                        Otimes(
                          CId(b),
                          unwrap_swaptimes_circuit([a]) (u)),
                        id_to_circuit(w)))

(* Turns an identity into the corresponding tape *)
let id_to_tape (l1 : sort list list) = List.fold_left (fun acc m -> Oplus (acc, Tape(id_to_circuit m))) (TId0) l1

let rec unwrap_swapplus_tape (l1 : sort list list) (l2 : sort list list) = match (l1, l2) with
  | [], u       -> id_to_tape (u)
  | w, []       -> id_to_tape (w)
  | a::w, b::u  -> TCompose(
                    TCompose(
                      Oplus(
                        TId([a]),
                        unwrap_swapplus_tape(w) (b::u)),
                      Oplus(
                        SwapPlus(a, b),
                        id_to_tape(u@w))),
                      Oplus(
                        Oplus(
                          TId([b]),
                          unwrap_swapplus_tape([a]) (u)),
                        id_to_tape(w)))


let swapplus_to_tape (p : sort list list) (q : sort list list) = unwrap_swapplus_tape p q

(* Definition 4.5 of technical report *)
let rec ldistr_to_tape (p : sort list list) (q : sort list list) (r : sort list list) = match p with
  | []      -> TId0
  | u :: p1 -> TCompose (
      Oplus (
        id_to_tape (obj_to_polynomial (Obtimes( obj_of_polynomial ([u]), Obplus (obj_of_polynomial (q), obj_of_polynomial (r))))),
        ldistr_to_tape p1 q r
      ),

      Oplus (
        Oplus (
          id_to_tape (obj_to_polynomial (Obtimes (obj_of_polynomial ([u]), obj_of_polynomial (q)))),
          swapplus_to_tape (obj_to_polynomial(Obtimes (obj_of_polynomial ([u]), obj_of_polynomial (r)))) (obj_to_polynomial(Obtimes (obj_of_polynomial (p1), obj_of_polynomial (q))))
        ),
        id_to_tape (obj_to_polynomial (Obtimes (obj_of_polynomial (p1), obj_of_polynomial (r))))
      )
  )

(* Definition 4.6 of technical report *)
let rec swaptimes_to_tape (p : sort list list) (q : sort list list) = match q with
  | []      ->  TId0
  | m :: q1  -> let sum_of_swaps = sum_monomials (List.map(fun ui ->  Tape(unwrap_swaptimes_circuit ui m)) p)
                  in TCompose(ldistr_to_tape p [m] q1, Oplus (sum_of_swaps, swaptimes_to_tape p q1))

let rec left_whiskering_mon (u : sort list) (t : tape) = match t with
                  | TId0 -> TId0
                  | TId s -> TId (List.map (fun x -> u@x) s)  (* CHECK this case is missing in tech report *)
                  | Tape(c) -> Tape(Otimes(id_to_circuit u, c))
                  | SwapPlus (v, w) -> SwapPlus (u@v, u@w)
                  | TCompose(t1, t2) -> TCompose (left_whiskering_mon u t1, left_whiskering_mon u t2)
                  | Oplus (t1, t2) -> Oplus (left_whiskering_mon u t1, left_whiskering_mon u t2)
                  | _ -> failwith "not yet implemented (left whiskering)"


let rec right_whiskering_mon (u : sort list) (t : tape) = match t with
                  | TId0 -> TId0
                  | TId s -> TId (List.map (fun x -> x@u) s) (* CHECK this case is missing in tech report *)
                  | Tape(c) -> Tape(Otimes(c, id_to_circuit u))
                  | SwapPlus (v, w) -> SwapPlus (v@u, w@u)
                  | TCompose(t1, t2) -> TCompose (right_whiskering_mon u t1, right_whiskering_mon u t2)
                  | Oplus (t1, t2) -> Oplus (right_whiskering_mon u t1, right_whiskering_mon u t2)
                  | _ -> failwith "not yet implemented (right whiskering)"

let rec left_whiskering (u : sort list list) (t : tape) = match u with
  | [] -> TId0
  | a :: rest -> Oplus (left_whiskering_mon a t, left_whiskering rest t)

let rec circuit_inverse (c : circuit) = match c with
  | CId _ | CId1 -> c
  | Gen (s, ar, coar) -> Gen(s ^ "$^{-1}$", coar, ar)
  | CCompose (c1, c2) -> CCompose(circuit_inverse c2, circuit_inverse c1)
  | Otimes (c1, c2) -> Otimes (circuit_inverse c1, circuit_inverse c2)
  | SwapTimes (s1, s2) -> SwapTimes (s2, s1)

let rec tape_inverse (t : tape) = match t with 
  | TId0 | TId _ -> t
  | Tape c -> Tape(circuit_inverse c)
  | SwapPlus (a, b) -> SwapPlus (b, a)
  | TCompose (t1, t2) -> TCompose(tape_inverse t2, tape_inverse t1)
  | Oplus (t1, t2) -> Oplus (tape_inverse t1, tape_inverse t2)
  | _ -> failwith "inverse not yet implemented"

let rec right_whiskering (u : sort list list) (t : tape) = 
  let p = Typecheck.tape_arity t in
  let q = Typecheck.tape_coarity t in
  let dl w s = ldistr_to_tape p [w] s in
  let inv_dl w s = tape_inverse (ldistr_to_tape q [w] s) in
  match u with
  | [] -> TId0
  | a :: rest -> TCompose(dl a rest, TCompose(Oplus (right_whiskering_mon a t, right_whiskering rest t), inv_dl a rest))


let rec otimes_to_tape (t1 : term) (t2 : term) = 
  let t1 = _to_tape t1 in
  let t2 = _to_tape t2 in
  let p = Typecheck.tape_arity t1 in
  let s = Typecheck.tape_coarity t2 in
  TCompose (left_whiskering p t2, right_whiskering s t1)

(* converts term into tape (when possible) *)
and _to_tape (t : term) = match t with
  | Id (l)              -> id_to_tape (l)
  | SwapTimes (p, q)    -> swaptimes_to_tape p q
  | SwapPlus (p, q)     -> swapplus_to_tape p q
  | Ldistr (p, q, r)    -> ldistr_to_tape p q r
  | Otimes (t1, t2)       -> otimes_to_tape t1 t2
  | Oplus (t1, t2)      -> Oplus(_to_tape(t1), _to_tape(t2))
  | Compose (t1, t2)    -> TCompose(_to_tape(t1), _to_tape(t2))
  | Gen (_, _, _)       -> failwith("cannot convert SSR generator to tape")
