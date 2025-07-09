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

type obj =
  | S of sort
  | Obtimes of obj * obj
  | Obplus of obj * obj
  | Ob0
  | Ob1
[@@deriving show]

let rec clean_obj = function
  | Obtimes (x, y) -> (
      let x = clean_obj x and y = clean_obj y in
      match (x, y) with _, Ob1 -> x | Ob1, _ -> y | _ -> Obtimes (x, y))
  | Obplus (x, y) -> (
      let x = clean_obj x and y = clean_obj y in
      match (x, y) with _, Ob0 -> x | Ob0, _ -> y | _ -> Obplus (x, y))
  | S s -> S s
  | Ob0 -> Ob0
  | Ob1 -> Ob1

let rec pp_object ob =
  let ob = clean_obj ob in
  match ob with
  | S s -> s
  | Ob0 -> "0"
  | Ob1 -> "1"
  | Obtimes (a, b) -> "(" ^ pp_object a ^ " ⊗  " ^ pp_object b ^ ")"
  | Obplus (a, b) -> "(" ^ pp_object a ^ " ⊕  " ^ pp_object b ^ ")"

type term =
  | Id of sort list list
  | GenVar of string
  | Gen of string * sort list list * sort list list
  | SwapTimes of (sort list list * sort list list)
  | SwapPlus of (sort list list * sort list list)
  | Oplus of term * term
  | Otimes of term * term
  | Compose of term * term
  | Ldistr of (sort list list * sort list list * sort list list)
  | Cut of sort list list
  | Split of sort list list
  | Spawn of sort list list
  | Join of sort list list
  | Copy of sort list list
  | Discard of sort list list
  | CoCopy of sort list list
  | CoDiscard of sort list list
  | Trace of sort list list * term
[@@deriving show]

let defined_terms : (string, term) Hashtbl.t = Hashtbl.create 10

let _print_type t =
  print_string "[";
  List.iter
    (fun l1 ->
      print_string "[";
      List.iter
        (fun x ->
          print_string " ";
          print_string x;
          print_string " ")
        l1;
      print_string "]")
    t;
  print_string "]\n"

(* Otimes on objects, Technical report page 24, section 4.1 *)
let times_on_objects p q =
  List.concat_map (fun ui -> List.map (fun vj -> ui @ vj) q) p

(* turns an object into a normalized polynomial in S** representation *)
let rec obj_to_polynomial ob =
  match ob with
  | S e -> [ [ e ] ]
  | Obplus (o1, o2) -> obj_to_polynomial o1 @ obj_to_polynomial o2
  | Obtimes (o1, o2) ->
      times_on_objects (obj_to_polynomial o1) (obj_to_polynomial o2)
  | Ob0 -> []
  | Ob1 -> [ [] ]

let rec obj_to_monomial ob =
  match ob with
  | S e -> [ e ]
  | Obtimes (o1, o2) -> obj_to_monomial o1 @ obj_to_monomial o2
  | Ob1 -> []
  | Ob0 | Obplus _ ->
      raise
        (Errors.TypeError
           "used non-monomial type where monomial type was needed")

(* turns a polynomial in S** representation into a polynomial *)
let obj_of_polynomial poly =
  List.fold_left
    (fun acc mon ->
      (* multiply individual atoms *)
      let term = List.fold_left (fun acc_e e -> Obtimes (acc_e, S e)) Ob1 mon in
      (* sum monomials *)
      Obplus (acc, term))
    Ob0 poly

(* turns a monomial in S* representation into a polynomial *)
let obj_of_monomial (mon : sort list) =
  List.fold_left (fun acc_e e -> Obtimes (acc_e, S e)) Ob1 mon

(* reduces an object into its normal form *)
let _object_to_normal_form ob = obj_of_polynomial (obj_to_polynomial ob)

(* transforms an object of the form ∏_i (s_i) to [s_1, s_2, ...]  *)
let rec sort_prod_to_list (ob : obj) =
  match ob with
  | S e -> [ e ]
  | Ob1 -> []
  | Obtimes (t1, t2) -> sort_prod_to_list t1 @ sort_prod_to_list t2
  | Obplus _ | Ob0 ->
      raise (Errors.TypeError "expected product of sorts, got non-product type")

let rec multi_copy n u =
  if n = 0 then Discard u
  else Compose (Copy u, Otimes (multi_copy (n - 1) u, Id u))
