type sort = string [@@deriving show]

(*type term = Id of (sort)
            | Gen of string * (sort list list) * (sort list list) 
            | SwapTimes of (sort * sort)
            | SwapPlus of (sort list * sort list)
            | Oplus of term * term
            | Otimes of term * term
            | Compose of term * term 
            | Id0
            | Id1
            [@@deriving show]*)

            
type obj  = S of sort | Obtimes of obj * obj | Obplus of obj * obj | Ob0 | Ob1 [@@deriving show]


type term = Id of (sort list list)
            | Gen of string * (sort list list) * (sort list list)
            | SwapTimes of (sort list list * sort list list)
            | SwapPlus of (sort list list * sort list list)
            | Oplus of term * term
            | Otimes of term * term
            | Compose of term * term
            | Ldistr of (sort list list * sort list list * sort list list)
            [@@deriving show]


let _print_type t = print_string "[" ; List.iter(fun l1 -> print_string "[" ; List.iter(fun x -> print_string " " ; print_string x ; print_string " ") l1 ; print_string "]") t ; print_string "]\n"

(* Otimes on objects, Technical report page 24, section 4.1 *)
let times_on_objects p q =
  List.concat_map (fun ui ->
    List.map (fun vj -> ui @ vj) q
  ) p

(* turns an object into a normalized polynomial in S** representation *)
let rec obj_to_polynomial ob = match ob with
  | S (e)             -> [[e]]
  | Obplus (o1, o2)   -> obj_to_polynomial(o1) @ obj_to_polynomial(o2)
  | Obtimes (o1, o2)  -> times_on_objects (obj_to_polynomial(o1)) (obj_to_polynomial(o2))
  | Ob0               -> []
  | Ob1               -> [[]]

(* turns a polynomial in S** representation into a polynomial *)
let obj_of_polynomial poly =
  List.fold_left (fun acc mon ->
    (* multiply individual atoms *)
    let term =
      List.fold_left (fun acc_e e -> Obtimes(acc_e, S(e))) Ob1 mon
    in
    (* sum monomials *)
    Obplus(acc, term)
  ) Ob0 poly

(* reduces an object into its normal form *)
let _object_to_normal_form ob = obj_of_polynomial(obj_to_polynomial ob)


(* transforms an object of the form ∏_i (s_i) to [s_1, s_2, ...]  *)
let rec sort_prod_to_list (ob : obj) = match ob with
  | S(e) -> [e]
  | Ob1 -> []
  | Obtimes (t1, t2) -> (sort_prod_to_list t1) @ (sort_prod_to_list t2)
  | Obplus _ | Ob0 -> failwith "tried to transform non-prod type to prod list"


(*
 - def 4.5 distributore
 - 4.6 swap del * (su polinomi - su monomi è come il multiSwap di prima)
 - funzione multiswap -> swap polimorfa
 - 
*)

