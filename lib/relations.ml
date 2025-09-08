open Common_defs
open Terms
open Typecheck

type relation =
  | RelGen of string * sort list * sort list
  | Union of relation * relation
  | Intersection of relation * relation
  | Op of relation
  | Star of relation
  | IdRel of sort list
  | TopRel of sort list
  | BotRel of sort list
  | Compose of relation * relation

let rec pp_relation = function
  | RelGen (name, l1, l2) ->
      Printf.sprintf "%s ⊆ (%s) × (%s)" name (pp_sort_list l1) (pp_sort_list l2)
  | Union (r1, r2) ->
      Printf.sprintf "(%s) ∪ (%s)" (pp_relation r1) (pp_relation r2)
  | Intersection (r1, r2) ->
      Printf.sprintf "(%s) ∩ (%s)" (pp_relation r1) (pp_relation r2)
  | Op r1 -> Printf.sprintf "(%s)†" (pp_relation r1)
  | Star r1 -> Printf.sprintf "(%s)*" (pp_relation r1)
  | IdRel l1 -> Printf.sprintf "id (%s)" (pp_sort_list l1)
  | TopRel _ -> "⊤"
  | BotRel _ -> "⊥"
  | Compose (r1, r2) ->
      Printf.sprintf "(%s) ; (%s)" (pp_relation r1) (pp_relation r2)

(** given a term representing a relation R, returns the term representing R* *)
let kleene_star (t : term) =
  let ar = arity t in
  let coar = coarity t in
  if ar = coar then
    Trace (ar, Compose (Join ar, Compose (Split ar, Oplus (t, Id ar))))
  else
    raise
      (Errors.TypeError
         (Printf.sprintf
            "Tried to apply kleene star to term with ar != coar.\n\
             Ar = %s\t Coar = %s"
            (string_of_sort_list_list ar)
            (string_of_sort_list_list coar)))

let rec term_of_rel (rel : relation) =
  match rel with
  | RelGen (name, l1, l2) -> Gen (name, [ l1 ], [ l2 ], Relation)
  | Union (r1, r2) ->
      let t1 = term_of_rel r1 in
      let t2 = term_of_rel r2 in
      let ar1 = arity t1 in
      let ar2 = arity t2 in
      let coar1 = coarity t1 in
      let coar2 = coarity t2 in
      if ar1 = ar2 && coar1 = coar2 then
        Compose (Split ar1, Compose (Oplus (t1, t2), Join coar1))
      else raise (Errors.TypeError "incompatible relations in union")
  | Intersection (r1, r2) ->
      let t1 = term_of_rel r1 in
      let t2 = term_of_rel r2 in
      let ar1 = arity t1 in
      let ar2 = arity t2 in
      let coar1 = coarity t1 in
      let coar2 = coarity t2 in
      if ar1 = ar2 && coar1 = coar2 then
        Compose (Copy ar1, Compose (Otimes (t1, t2), CoCopy coar1))
      else raise (Errors.TypeError "incompatible relations in intersection")
  | Op r1 -> term_inverse (term_of_rel r1)
  | Star r1 -> kleene_star (term_of_rel r1)
  | IdRel l1 -> Id [ l1 ]
  | TopRel l1 -> Compose (Cut [ l1 ], Spawn [ l1 ])
  | BotRel l1 -> Compose (Spawn [ l1 ], Cut [ l1 ])
  | Compose (r1, r2) ->
      let t1 = term_of_rel r1 in
      let t2 = term_of_rel r2 in
      let ar2 = arity t2 in
      let coar1 = coarity t1 in
      if ar2 = coar1 then Compose (t1, t2)
      else failwith "incompatible relations in composition"
