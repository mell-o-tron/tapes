open Tapes
open Typecheck

type tape_atom =
  | ATId of Terms.sort list list (* da vedere *)
  | ATId0
  | ATape of circuit
  | ASwapPlus of (Terms.sort list * Terms.sort list)
  | ACut of Terms.sort list
  | ASplit of Terms.sort list
  | ASpawn of Terms.sort list
  | AJoin of Terms.sort list
  | ATrace of assoc_tape

and comp = Comp of tape_atom list
and assoc_tape = Sum of comp list

(* turns a circuit to product form *)
let rec circuit_to_product (t : circuit) =
  match t with
  | CCompose (Otimes (t1, t2), Otimes (t3, t4)) ->
      if
        circuit_typecheck (CCompose (t1, t3))
        && circuit_typecheck (CCompose (t2, t4))
      then Otimes (CCompose (t1, t3), CCompose (t2, t4))
      else
        CCompose
          ( circuit_to_product (Otimes (t1, t2)),
            circuit_to_product (Otimes (t3, t4)) )
  | CCompose (t1, t2) -> CCompose (circuit_to_product t1, circuit_to_product t2)
  | Otimes (t1, t2) -> Otimes (circuit_to_product t1, circuit_to_product t2)
  | _ -> t

(* turns a tape to sum form *)
let rec tape_to_sum (t : tape) =
  let rec tape_to_sum_aux (t : tape) =
    match t with
    | TCompose (Oplus (t1, t2), Oplus (t3, t4)) ->
        if
          tape_typecheck (TCompose (t1, t3))
          && tape_typecheck (TCompose (t2, t4))
        then Oplus (TCompose (t1, t3), TCompose (t2, t4))
        else
          TCompose
            (tape_to_sum_aux (Oplus (t1, t2)), tape_to_sum_aux (Oplus (t3, t4)))
    | TCompose (t1, t2) -> TCompose (tape_to_sum_aux t1, tape_to_sum_aux t2)
    | Oplus (t1, t2) -> Oplus (tape_to_sum_aux t1, tape_to_sum_aux t2)
    | Trace t1 -> Trace (tape_to_sum_aux t1)
    | _ -> t
  in
  (* this is probably not needed, but "meglio ave' paura che buscanne" *)
  let res = tape_to_sum_aux t in
  if res = t then res else tape_to_sum res

let rec atom_of_tape (t : tape) =
  match t with
  | TId l -> ATId l
  | TId0 -> ATId0
  | Tape c -> ATape c
  | SwapPlus l -> ASwapPlus l
  | Cut l -> ACut l
  | Split l -> ASplit l
  | Spawn l -> ASpawn l
  | Join l -> AJoin l
  | Trace t -> ATrace (assoc_tape_of_tape t)
  | _ -> failwith "atom or trace expected"

and comp_of_tape (t : tape) =
  match t with
  | TCompose (t1, t2) ->
      let (Comp res1) = comp_of_tape t1 in
      let (Comp res2) = comp_of_tape t2 in
      Comp (res1 @ res2)
  | _ -> Comp [ atom_of_tape t ]

and assoc_tape_of_tape (t : tape) =
  let t = tape_to_sum t in
  match t with
  | Oplus (t1, t2) ->
      let (Sum res1) = assoc_tape_of_tape t1 in
      let (Sum res2) = assoc_tape_of_tape t2 in
      Sum (res1 @ res2)
  | _ -> Sum [ comp_of_tape t ]
