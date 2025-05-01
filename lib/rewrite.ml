open Tapes
open Typecheck

(* turns a circuit to product form *)
let rec circuit_to_product (t : circuit) = match t with
    | CCompose(Otimes(t1, t2), Otimes(t3, t4)) -> if circuit_typecheck(CCompose (t1, t3)) && circuit_typecheck(CCompose (t2, t4)) then
        Otimes(CCompose(t1, t3), CCompose(t2, t4))
      else
        CCompose(circuit_to_product (Otimes(t1, t2)), circuit_to_product (Otimes(t3, t4)))
    | CCompose(t1, t2) -> CCompose(circuit_to_product t1, circuit_to_product t2)
    | Otimes(t1, t2) -> Otimes(circuit_to_product t1, circuit_to_product t2)
    | _ -> t


(* turns a tape to sum form *)
let rec tape_to_sum (t : tape) = match t with
  | TCompose(Oplus(t1, t2), Oplus(t3, t4)) -> if tape_typecheck(TCompose (t1, t3)) && tape_typecheck(TCompose (t2, t4)) then
        Oplus(TCompose(t1, t3), TCompose(t2, t4))
      else
        TCompose(tape_to_sum (Oplus(t1, t2)), tape_to_sum (Oplus(t3, t4)))
  | TCompose (t1, t2) -> TCompose(tape_to_sum t1, tape_to_sum t2)
  | Oplus(t1, t2) -> Oplus(tape_to_sum t1, tape_to_sum t2)
  | _ -> t

