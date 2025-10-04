open Common_defs

(** Type representing the kinds of generators. *)
type gen_kind =
  | Relation
  | OpRelation
  | NegRelation
  | Function
  | Corefl
[@@deriving show, compare]

(** type of objects *)
type obj =
  | S of sort
  | Obtimes of obj * obj
  | Obplus of obj * obj
  | Ob0
  | Ob1
[@@deriving show]

(** Cleans objects by removing neutral elements. *)
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

(** Pretty-prints objects as strings. *)
let rec pp_object ob =
  let ob = clean_obj ob in
  match ob with
  | S s -> s
  | Ob0 -> "0"
  | Ob1 -> "1"
  | Obtimes (a, b) -> "(" ^ pp_object a ^ " ⊗  " ^ pp_object b ^ ")"
  | Obplus (a, b) -> "(" ^ pp_object a ^ " ⊕  " ^ pp_object b ^ ")"

(** Type representing sesquistrict-rig terms. *)
type term =
  | Id of sort list list
  | GenVar of string
  | Gen of string * sort list list * sort list list * gen_kind
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

(** Hashtable for looking up which terms have been named so far. *)
let defined_terms : (string, term) Hashtbl.t = Hashtbl.create 10

(** Prints a type in polynomial form to stdout. *)
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

(** Computes the tensor product of two objects as polynomials. *)
let times_on_objects p q =
  List.concat_map (fun ui -> List.map (fun vj -> ui @ vj) q) p

(** Converts an object to a normalized polynomial in S** representation. *)
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

(** Converts a polynomial in S** representation to an object. *)
let obj_of_polynomial poly =
  List.fold_left
    (fun acc mon ->
      (* multiply individual atoms *)
      let term = List.fold_left (fun acc_e e -> Obtimes (acc_e, S e)) Ob1 mon in
      (* sum monomials *)
      Obplus (acc, term))
    Ob0 poly

(** Converts a monomial in S* representation to an object. *)
let obj_of_monomial (mon : sort list) =
  List.fold_left (fun acc_e e -> Obtimes (acc_e, S e)) Ob1 mon

(** Reduces an object into its normal form. *)
let _object_to_normal_form ob = obj_of_polynomial (obj_to_polynomial ob)

(** Transforms an object of the form ∏_i (s_i) to [s_1, s_2, ...]. *)
let rec sort_prod_to_list (ob : obj) =
  match ob with
  | S e -> [ e ]
  | Ob1 -> []
  | Obtimes (t1, t2) -> sort_prod_to_list t1 @ sort_prod_to_list t2
  | Obplus _ | Ob0 ->
      raise (Errors.TypeError "expected product of sorts, got non-product type")

(** Constructs an n-ary copy term. *)
let rec multi_copy n u =
  if n = 0 then Discard u
  else if n = 1 then Id u
  else if n = 2 then Copy u
  else Compose (Copy u, Otimes (multi_copy (n - 1) u, Id u))

(* Definition 4.5 of technical report *)
let rec decompose_ldistr (p : sort list list) (q : sort list list)
    (r : sort list list) =
  match p with
  | [] -> Id []
  | u :: p1 ->
      Compose
        ( Oplus
            ( Id
                (obj_to_polynomial
                   (Obtimes
                      ( obj_of_polynomial [ u ],
                        Obplus (obj_of_polynomial q, obj_of_polynomial r) ))),
              decompose_ldistr p1 q r ),
          Oplus
            ( Oplus
                ( Id
                    (obj_to_polynomial
                       (Obtimes (obj_of_polynomial [ u ], obj_of_polynomial q))),
                  SwapPlus
                    ( obj_to_polynomial
                        (Obtimes (obj_of_polynomial [ u ], obj_of_polynomial r)),
                      obj_to_polynomial
                        (Obtimes (obj_of_polynomial p1, obj_of_polynomial q)) )
                ),
              Id
                (obj_to_polynomial
                   (Obtimes (obj_of_polynomial p1, obj_of_polynomial r))) ) )

(** Inverts a generator. *)
let invert_generator s ar coar (kind : gen_kind) =
  let t0 = Otimes (Id coar, CoDiscard ar) in
  let t1 = Otimes (Id coar, Copy ar) in
  let t2 =
    Otimes
      ( Otimes
          ( Id coar,
            Gen
              ( s,
                ar,
                coar,
                match kind with
                | NegRelation -> NegRelation
                | Function -> Function
                | _ -> Relation ) ),
        Id ar )
  in
  let t3 = Otimes (CoCopy coar, Id ar) in
  let t4 = Otimes (Discard coar, Id ar) in
  Compose (t0, Compose (t1, Compose (t2, Compose (t3, t4))))

(** Computes the inverse of a term. *)
let rec term_inverse (t : term) =
  match t with
  | Id x -> Id x
  | SwapPlus (a, b) -> SwapPlus (b, a)
  | SwapTimes (a, b) -> SwapTimes (b, a)
  | Compose (t1, t2) -> Compose (term_inverse t2, term_inverse t1)
  | Oplus (t1, t2) -> Oplus (term_inverse t1, term_inverse t2)
  | Otimes (t1, t2) -> Otimes (term_inverse t1, term_inverse t2)
  | Spawn l -> Cut l
  | Cut l -> Spawn l
  | Join l -> Split l
  | Split l -> Join l
  | Trace (l, t1) -> Trace (l, term_inverse t1)
  | Gen (s, ar, coar, kind) -> invert_generator s ar coar kind
  | Ldistr (l1, l2, l3) -> term_inverse (decompose_ldistr l1 l2 l3)
  | Copy l1 -> CoCopy l1
  | CoCopy l1 -> Copy l1
  | Discard l1 -> CoDiscard l1
  | CoDiscard l1 -> Discard l1
  | GenVar _ -> failwith "variable should be resolved by now"
