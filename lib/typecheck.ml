open Ast
open Tapes

exception Syntax_error of int * int * string

(* returns the arity of a term *)
let rec arity (t : term) = match t with
  | Id (l1)             -> l1
  | Gen (_, l1, _)      -> l1
  | SwapTimes (l1, l2)  -> obj_to_polynomial ( Obtimes (obj_of_polynomial l1, obj_of_polynomial l2) ) 
  | SwapPlus (l1, l2)   -> l1 @ l2
  | Oplus (t1, t2)      -> arity (t1) @ arity (t2)
  | Otimes (t1, t2)     -> times_on_objects (arity(t1)) (arity(t2))
  | Compose (t1, _)     -> arity(t1)
  | Ldistr (l1, l2, l3) -> obj_to_polynomial (
      Obtimes(
        obj_of_polynomial(l1), 
        Obplus(obj_of_polynomial(l2), obj_of_polynomial(l3))
      )
  )

  
(*  | _ -> failwith("arity not yet implemented")*)

(* returns the coarity of a term *)
let rec coarity (t : term) = match t with
| Id (l1)             -> l1
| Gen (_, _, l2)      -> l2
| SwapTimes (l1, l2)  -> obj_to_polynomial ( Obtimes (obj_of_polynomial l2, obj_of_polynomial l1) ) 
| SwapPlus (l1, l2)   -> l2 @ l1
| Oplus (t1, t2)      -> coarity (t1) @ coarity (t2)
| Otimes (t1, t2)     -> times_on_objects (coarity(t1)) (coarity(t2))
| Compose (_, t2)     -> coarity(t2)
| Ldistr (l1, l2, l3) -> obj_to_polynomial (
    Obplus(
        Obtimes (obj_of_polynomial(l1), obj_of_polynomial(l2)),
        Obtimes (obj_of_polynomial(l1), obj_of_polynomial(l3))
    )
)

(* checks if arity and coarity match in compositions *)
let rec typecheck (t : term) = match t with
  | Compose (t1, t2)  -> arity(t2) = coarity(t1)
  | Oplus (t1, t2)    -> typecheck(t1) && typecheck(t2)
  | Otimes (t1, t2)   -> typecheck(t1) && typecheck(t2)
  | _                 -> true



let rec circuit_arity (c : circuit) = match c with 
  | CId s -> [s]
  | CId1 -> []
  | Gen (_, ar, _coar) -> ar
  | CCompose (c1, _c2) -> circuit_arity c1
  | Otimes (c1, c2) -> circuit_arity c1 @ circuit_arity c2
  | SwapTimes (s1, s2) -> [s1 ; s2]

let rec circuit_coarity (c : circuit) = match c with 
  | CId s -> [s]
  | CId1 -> []
  | Gen (_, _ar, coar) -> coar
  | CCompose (_c1, c2) -> circuit_coarity c2
  | Otimes (c1, c2) -> circuit_coarity c1 @ circuit_coarity c2
  | SwapTimes (s1, s2) -> [s2 ; s1]

let rec tape_arity (t : tape) = match t with 
  | TId sll -> sll
  | TId0 -> []
  | Tape c -> [circuit_arity c]
  | TCompose (t1, _t2) -> tape_arity t1
  | Oplus (t1, t2) -> tape_arity t1 @ tape_arity t2
  | SwapPlus (sl1, sl2) -> [sl1 ; sl2]

  (* not in tech report -- should include?? *)
  | Ldistr (sl1, sl2, sl3) -> obj_to_polynomial (
    Obtimes(
      obj_of_polynomial([sl1]), 
      Obplus(obj_of_polynomial([sl2]), obj_of_polynomial([sl3]))
    )
  )

  | Discard _ -> failwith "not yet implemented"
  | Copy _ -> failwith "not yet implemented"
  | CoDiscard _ -> failwith "not yet implemented"
  | CoCopy _ -> failwith "not yet implemented"

  let rec tape_coarity (t : tape) = match t with 
  | TId sll -> sll
  | TId0 -> []
  | Tape c -> [circuit_coarity c]
  | TCompose (_t1, t2) -> tape_coarity t2
  | Oplus (t1, t2) -> tape_coarity t1 @ tape_coarity t2
  | SwapPlus (sl1, sl2) -> [sl2 ; sl1]

  (* not in tech report -- should include?? *)
  | Ldistr (sl1, sl2, sl3) -> obj_to_polynomial (
    Obplus(
        Obtimes (obj_of_polynomial([sl1]), obj_of_polynomial([sl2])),
        Obtimes (obj_of_polynomial([sl1]), obj_of_polynomial([sl3]))
    )
)

  | Discard _ -> failwith "not yet implemented"
  | Copy _ -> failwith "not yet implemented"
  | CoDiscard _ -> failwith "not yet implemented"
  | CoCopy _ -> failwith "not yet implemented"

  (* checks if arity and coarity match in compositions *)
let rec circuit_typecheck (t : circuit) = match t with
  | CCompose (t1, t2)  -> circuit_arity(t2) = circuit_coarity(t1)
  | Otimes (t1, t2)    -> circuit_typecheck(t1) && circuit_typecheck(t2)
  | _                 -> true

  (* checks if arity and coarity match in compositions *)
let rec tape_typecheck (t : tape) = match t with
  | TCompose (t1, t2)  -> tape_arity(t2) = tape_coarity(t1)
  | Oplus (t1, t2)    -> tape_typecheck(t1) && tape_typecheck(t2)
  | Tape (c1)         -> circuit_typecheck c1
  | _                 -> true
