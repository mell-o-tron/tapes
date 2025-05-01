open Ast
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

let rec swaptimes_to_tape (p : sort list list) (q : sort list list) = match q with
  | []      ->  TId0
  | m :: q1  -> let sum_of_swaps = sum_monomials (List.map(fun ui ->  Tape(unwrap_swaptimes_circuit ui m)) p)
                  in TCompose(ldistr_to_tape p [m] q1, Oplus (sum_of_swaps, swaptimes_to_tape p q1))



let otimes_to_tape (_t : term) = failwith("otimes_to_tape not yet implemented")



(* converts term into tape (when possible) *)
let rec _to_tape (t : term) = match t with
  | Id (l)              -> id_to_tape (l)
  | SwapTimes (p, q)    -> swaptimes_to_tape p q
  | SwapPlus (p, q)     -> swapplus_to_tape p q
  | Ldistr (p, q, r)    -> ldistr_to_tape p q r
  | Otimes (_, _)       -> otimes_to_tape (t)
  | Oplus (t1, t2)      -> Oplus(_to_tape(t1), _to_tape(t2))
  | Compose (t1, t2)    -> TCompose(_to_tape(t1), _to_tape(t2))
  | Gen (_, _, _)       -> failwith("cannot convert SSR generator to tape")
