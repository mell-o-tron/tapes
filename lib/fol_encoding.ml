open Tapes
open Hg_cospan
open Common_defs

type term =
  | Var of string * int
  | Func of string * term list

type atom =
  | Equals of term * term
  | Pred of string * term list

type literal =
  | Pos of atom
  | Neg of atom

type formula =
  | Top
  | Bot
  | Lit of literal
  | And of formula * formula
  | Or of formula * formula
  | Not of formula
  | Exists of (string * int) * formula
  | Forall of (string * int) * formula

(** basic simplification for FOL formulas *)
let rec simplify_formula (f : formula) : formula =
  match f with
  | Top -> Top
  | Bot -> Top
  | Lit _ -> f
  | And (f1, Top) -> f1
  | And (Top, f1) -> f1
  | Or (_, Top) -> Top
  | Or (Top, _) -> Top
  | And (_, Bot) -> Bot
  | And (Bot, _) -> Bot
  | Or (f1, Bot) -> f1
  | Or (Bot, f1) -> f1
  | And (f1, f2) -> And (simplify_formula f1, simplify_formula f2)
  | Or (f1, f2) -> Or (simplify_formula f1, simplify_formula f2)
  | Not Top -> Bot
  | Not Bot -> Top
  | Not f -> Not (simplify_formula f)
  | Exists (_, Top) -> Top
  | Exists (z, f) -> Exists (z, simplify_formula f)
  | Forall (z, f) -> Forall (z, simplify_formula f)

let string_of_var (v, i) = v ^ string_of_int i

(** transforms a term into a SPASS-compatible term *)
let rec spassify_term (t : term) =
  match t with
  | Var (x, i) -> string_of_var (x, i)
  | Func (name, l) ->
      Printf.sprintf "%s(%s)" name
        (List.map spassify_term l |> String.concat ",")

(** transforms an atom into a SPASS-compatible atom *)
let spassify_atom (a : atom) =
  match a with
  | Equals (t1, t2) ->
      Printf.sprintf "equal(%s, %s)" (spassify_term t1) (spassify_term t2)
  | Pred (name, l) ->
      Printf.sprintf "%s(%s)" name
        (List.map spassify_term l |> String.concat ",")

(** transforms a formula into a SPASS-compatible formula *)
let rec spassify (f : formula) =
  let f = simplify_formula f in
  match f with
  | Exists (v, f1) ->
      Printf.sprintf "exists([%s], %s)" (string_of_var v) (spassify f1)
  | Forall (v, f1) ->
      Printf.sprintf "forall([%s], %s)" (string_of_var v) (spassify f1)
  | And (f1, f2) -> Printf.sprintf "and(%s, %s)" (spassify f1) (spassify f2)
  | Or (f1, f2) -> Printf.sprintf "or(%s, %s)" (spassify f1) (spassify f2)
  | Not f1 -> Printf.sprintf "not(%s)" (spassify f1)
  | Lit (Pos atom) -> spassify_atom atom
  | Lit (Neg atom) -> Printf.sprintf "not (%s)" (spassify_atom atom)
  | Top -> Printf.sprintf "true"
  | Bot -> Printf.sprintf "false"

let current_axioms : formula list ref = ref []

type judgment = string list * string list * formula
(** arity, coarity |- formula *)

let eq_vars (s1, n1) (s2, n2) = Lit (Pos (Equals (Var (s1, n1), Var (s2, n2))))
let eq_terms (t1, t2) = Lit (Pos (Equals (t1, t2)))
let var_counter = ref 0

(** generates a fresh variable *)
let fresh_var () =
  var_counter := !var_counter + 1;
  ("z", !var_counter)

let fresh_vars n = List.init n (fun _ -> fresh_var ())
let xs n = List.init n (fun i -> ("x", i))
let ys n = List.init n (fun i -> ("y", i))
let zs n = List.init n (fun i -> ("z", i))

(** crude approach for removing latex from the names of generators*)
let strip_of_latex s =
  let re = Str.regexp_string in
  let s = Str.global_replace (re "\\overline") "" s in
  let s = Str.global_replace (re "$") "" s in
  let s = Str.global_replace (re "{") "" s in
  let s = Str.global_replace (re "}") "" s in
  (* TODO what to do? *)
  let s = Str.global_replace (re "^\\dagger") "_inv" s in
  s

(** counts the occurrences of a string in a substring *)
let count_occurrences_str big sub =
  let re = Str.regexp_string sub in
  (* escape special regex chars *)
  let rec aux idx count =
    try
      let pos = Str.search_forward re big idx in
      aux (pos + 1) (count + 1)
      (* +1 for overlapping matches; use +String.length sub for non-overlapping *)
    with Not_found -> count
  in
  aux 0 0

(** crude way to check the sign of a generator *)
let sign_of_latex s = count_occurrences_str s "\\overline" mod 2 = 0

(** adds nested exists from a variable list [l] to a formula [f] *)
let rec nested_exists l f =
  match l with
  | [] -> failwith "exists with no variable"
  | [ v ] -> Exists (v, f)
  | v :: rest -> Exists (v, nested_exists rest f)

(** Applies substitution to terms *)
let rec subst_term t (z, nz) (x, nx) =
  match t with
  | Var (v, nv) -> if v = x && nv = nx then Var (z, nz) else t
  | Func (name, l) ->
      Func (name, List.map (fun t -> subst_term t (z, nz) (x, nx)) l)

(** Applies substitution to atoms *)
let subst_atom a z x =
  match a with
  | Equals (t1, t2) -> Equals (subst_term t1 z x, subst_term t2 z x)
  | Pred (name, l) -> Pred (name, List.map (fun t -> subst_term t z x) l)

(** Applies substitution to formulas *)
let rec subst f z x =
  match f with
  | Exists (v, f1) -> if v = x then f else Exists (v, subst f1 z x)
  | Forall (v, f1) -> if v = x then f else Forall (v, subst f1 z x)
  | And (f1, f2) -> And (subst f1 z x, subst f2 z x)
  | Or (f1, f2) -> Or (subst f1 z x, subst f2 z x)
  | Not f1 -> Not (subst f1 z x)
  | Lit (Pos atom) -> Lit (Pos (subst_atom atom z x))
  | Lit (Neg atom) -> Lit (Pos (subst_atom atom z x))
  | Top | Bot -> f

(** Gathers the free variables from a term *)
let rec free_vars_term (t : term) bound_list =
  match t with
  | Var (s, i) -> if List.mem (s, i) bound_list then [] else [ (s, i) ]
  | Func (_, tl) ->
      List.map (fun t -> free_vars_term t bound_list) tl |> List.flatten

(** Gathers the free variables from an atom *)
let free_vars_atom (a : atom) bound_list =
  match a with
  | Equals (t1, t2) ->
      free_vars_term t1 bound_list @ free_vars_term t2 bound_list
  | Pred (_, tl) ->
      List.map (fun t -> free_vars_term t bound_list) tl |> List.flatten

(** Gathers the free variables from a formula *)
let rec free_vars f bound_list =
  match f with
  | Exists (v, f1) -> free_vars f1 (v :: bound_list)
  | Forall (v, f1) -> free_vars f1 (v :: bound_list)
  | And (f1, f2) -> free_vars f1 bound_list @ free_vars f2 bound_list
  | Or (f1, f2) -> free_vars f1 bound_list @ free_vars f2 bound_list
  | Not f1 -> free_vars f1 bound_list
  | Lit (Pos atom) -> free_vars_atom atom bound_list
  | Lit (Neg atom) -> free_vars_atom atom bound_list
  | Top | Bot -> []

(** universally quantifies the free variables in a formula *)
let universally_quantify_fvs (f : formula) =
  let fv = free_vars f [] |> List.sort_uniq compare in
  List.fold_right (fun a b -> Forall (a, b)) fv f

(** existentially quantifies a list of vars *)
let existentially_quantify_vars (f : formula) (l : (string * int) list) =
  List.fold_right (fun a b -> Exists (a, b)) l f

(** performs multi-variable substitutions in a formula *)
let rec substs f zs xs =
  match (xs, zs) with
  | [], [] -> f
  | x :: xs, z :: zs -> substs (subst f z x) zs xs
  | _ -> failwith "substitution has different number of variables"

(** increases the indices of x-variables *)
let shift_xs shift_amount ar_len f =
  let xs = xs ar_len in
  substs f (List.map (fun (v, i) -> (v, i + shift_amount)) xs) xs

(** increases the indices of y-variables (TODO why does this exist)*)
let shift_ys shift_amount coar_len f =
  let ys = ys coar_len in
  substs f (List.map (fun (v, i) -> (v, i + shift_amount)) ys) ys

(** given a circuit, produces a formula *)
let rec judgment_of_circuit_direct (c : circuit) : judgment =
  match c with
  | CId a -> ([ a ], [ a ], eq_vars ("x", 0) ("y", 0))
  | CId1 -> ([], [], Top)
  | Gen ("copy", ar, coar, _) ->
      (ar, coar, And (eq_vars ("x", 0) ("y", 0), eq_vars ("x", 0) ("y", 1)))
  | Gen ("cocopy", ar, coar, _) ->
      (ar, coar, And (eq_vars ("x", 0) ("y", 0), eq_vars ("x", 1) ("y", 0)))
  | Gen ("discard", ar, coar, _) | Gen ("codiscard", ar, coar, _) ->
      (ar, coar, Top)
  (* EQ does not seem to be translated *)
  | Gen ("$eq$", ar, coar, Relation) when coar = [] && List.length ar = 2 ->
      (ar, coar, Lit (Pos (Equals (Var ("x", 0), Var ("x", 1)))))
  | Gen ("$eq$", _, _, _) ->
      raise
        (Errors.TypeError
           "eq should be a relation symbol with arity 2 and coarity 0")
  | Gen ("$\\overline{eq}$", ar, coar, Relation) ->
      (ar, coar, Lit (Neg (Equals (Var ("x", 0), Var ("x", 1)))))
  | Gen (name, ar, coar, Relation) ->
      let l =
        List.mapi (fun i _ -> Var ("x", i)) ar
        @ List.mapi (fun i _ -> Var ("y", i)) coar
      in
      if sign_of_latex name then
        (ar, coar, Lit (Pos (Pred (strip_of_latex name, l))))
      else (ar, coar, Lit (Neg (Pred (strip_of_latex name, l))))
  | Gen (name, ar, coar, Function) ->
      let l = List.mapi (fun i _ -> Var ("x", i)) ar in
      (ar, coar, eq_terms (Func (strip_of_latex name, l), Var ("y", 0)))
  | SwapTimes (a, b) ->
      ( [ a; b ],
        [ b; a ],
        And (eq_vars ("x", 0) ("y", 1), eq_vars ("x", 1) ("y", 0)) )
  | CCompose (c1, c2) ->
      let ar1, coar1, f1 = judgment_of_circuit_direct c1 in
      let ar2, coar2, f2 = judgment_of_circuit_direct c2 in
      let zs = fresh_vars (List.length coar1) in
      ( ar1,
        coar2,
        if List.is_empty zs then And (f1, f2)
        else
          nested_exists zs
            (And
               ( substs f1 zs (ys (List.length coar1)),
                 substs f2 zs (xs (List.length ar2)) )) )
  | Otimes (c1, c2) ->
      let ar1, coar1, f1 = judgment_of_circuit_direct c1 in
      let ar2, coar2, f2 = judgment_of_circuit_direct c2 in
      let f2 =
        f2
        |> shift_xs (List.length ar2) (List.length ar1)
        |> shift_ys (List.length coar2) (List.length coar1)
      in
      (ar1 @ ar2, coar1 @ coar2, And (f1, f2))
  | _ -> failwith "not yet implemented"

let formula_of_circuit_direct c =
  let _, _, f = judgment_of_circuit_direct c in
  f

let split_last lst =
  match List.rev lst with
  | [] -> failwith "empty list has no last element"
  | last :: rev_rest -> (List.rev rev_rest, last)

let formula_of_hg_cospan ((cos, l) : hg_cospan) =
  Printf.printf "formula of cospan: %s" (pp_hg_cospan (cos, l));

  let he_formuals =
    let acc_ar = ref 0 in
    List.map
      (fun { name; arity; kind } ->
        let res =
          match kind with
          | Relation ->
              if strip_of_latex name = "eq" && List.length arity = 2 then
                Lit
                  (Pos (Equals (Var ("y", !acc_ar + 0), Var ("y", !acc_ar + 1))))
              else
                let vars =
                  List.mapi (fun i _ -> Var ("y", !acc_ar + i)) arity
                in
                Lit (Pos (Pred (strip_of_latex name, vars)))
          | Function ->
              Printf.printf "vars (as relation) are: %s\n" (pp_sort_list arity);
              let ins, _ = split_last arity in
              Printf.printf "vars (as function) are: %s\n" (pp_sort_list ins);
              let vars = List.mapi (fun i _ -> Var ("y", !acc_ar + i)) ins in
              Lit
                (Pos
                   (Equals
                      ( Func (strip_of_latex name, vars),
                        Var ("y", !acc_ar + List.length arity - 1) )))
        in
        acc_ar := !acc_ar + List.length arity;
        res)
      l
  in
  let he_formula =
    List.fold_left (fun a b -> And (a, b)) Top he_formuals |> simplify_formula
  in
  Printf.printf "hyper_edges: %s\n" (spassify he_formula);
  let g (i : int) =
    (* Printf.printf "looking for %d in %s\n" i
      (Taggedset.to_list cos.c |> TaggedTypeCospan.string_of_elem_list); *)
    let elem =
      List.find (fun ((j, _), _) -> i = j) (cos.c |> Taggedset.to_list)
    in
    let (j, _), _ = Taggedmap.find elem cos.r in
    j
  in
  let y_vars = ys (Taggedset.cardinal cos.c) in
  let z_vars = List.map (fun (_, i) -> ("z", g i)) y_vars in
  let substituted_he_formula = substs he_formula z_vars y_vars in
  Printf.printf "substituted hyper_edges: %s\n" (spassify he_formula);
  let f (i : int) =
    let elem =
      List.find (fun ((j, _), _) -> i = j) (cos.a |> Taggedset.to_list)
    in
    let (j, _), _ = Taggedmap.find elem cos.l in
    j
  in
  let x_vars = xs (Taggedset.cardinal cos.a) in
  let preimages =
    List.map
      (fun (_, i) -> List.filter (fun (_, j) -> f j = i) x_vars)
      (zs (Taggedset.cardinal cos.b))
  in
  let existentially_quantified = ref [] in
  let to_subst = ref [] in
  let with_subst = ref [] in
  let preim_formulas =
    List.mapi
      (fun i l ->
        if List.length l = 0 then
          let _ =
            existentially_quantified := !existentially_quantified @ [ ("z", i) ]
          in
          Top
        else
          let s, j = List.hd l in
          to_subst := !to_subst @ [ ("z", i) ];
          with_subst := !with_subst @ [ (s, j) ];
          List.fold_left
            (fun a (_, m) ->
              And (a, Lit (Pos (Equals (Var ("x", m), Var (s, j))))))
            Top l)
      preimages
  in
  let preim_formula =
    List.fold_left (fun a b -> And (a, b)) Top preim_formulas
  in
  existentially_quantify_vars
    (substs (And (preim_formula, substituted_he_formula)) !with_subst !to_subst)
    !existentially_quantified

let formula_of_circuit_cospan (c : circuit) =
  Printf.printf "formula of circuit: %s\n" (pp_circuit c);
  let cos = cospan_of_circuit c in
  formula_of_hg_cospan cos

let formula_of_circuit = formula_of_circuit_cospan

let rec list_disj l =
  match l with [] -> Top | [ x ] -> x | x :: rest -> Or (x, list_disj rest)

let rec bigcid m =
  match m with
  | [] -> CId1
  | [ s ] -> CId s
  | s :: rest -> Otimes (CId s, bigcid rest)

let rec circuit_of_monomial (t : tape) =
  match t with
  | Tape c -> c
  | TId [ s ] -> bigcid s
  | TCompose (t1, t2) ->
      CCompose (circuit_of_monomial t1, circuit_of_monomial t2)
  | _ ->
      raise
        (Errors.TypeError
           "Tried to obtain circuit from tape that is not monomial or is traced")

let formula_of_monomial (t : tape) = circuit_of_monomial t |> formula_of_circuit

let fol_matrix_of_tape (t : tape) =
  let m = Matrix.get_matrix t in
  Matrix.matrix_mapij
    (fun _ _ l -> List.map formula_of_monomial l |> list_disj)
    m

(** Helper: unique-ify a list of (string * int) pairs *)
let uniq_pairs lst = lst |> List.sort_uniq compare

(** Traverse a term to collect all function symbols *)
let rec functions_of_term (t : term) : (string * int) list =
  match t with
  | Var _ -> []
  | Func (name, args) ->
      let here = (strip_of_latex name, List.length args) in
      here :: List.concat_map functions_of_term args

(** Traverse an atom to collect all function symbols *)
let functions_of_atom = function
  | Equals (t1, t2) -> functions_of_term t1 @ functions_of_term t2
  | Pred (_, args) -> List.concat_map functions_of_term args

(** Traverse a literal to collect function symbols *)
let functions_of_literal = function Pos a | Neg a -> functions_of_atom a

(** Main: collect and dedup from a formula *)
let get_all_functions (fml : formula) : (string * int) list =
  let open List in
  let rec aux f =
    match f with
    | Top | Bot -> []
    | Lit lit -> functions_of_literal lit
    | And (f1, f2) | Or (f1, f2) -> aux f1 @ aux f2
    | Not f' -> aux f'
    | Exists (_, f') | Forall (_, f') -> aux f'
  in
  uniq_pairs (aux fml)

(** Traverse an atom to collect predicate symbols *)
let predicates_of_atom = function
  | Equals _ -> []
  | Pred (name, args) -> [ (strip_of_latex name, List.length args) ]

(** Traverse a literal to collect predicate symbols *)
let predicates_of_literal = function Pos a | Neg a -> predicates_of_atom a

(** Main: collect and dedup from a formula *)
let get_all_predicates (fml : formula) : (string * int) list =
  let open List in
  let rec aux f =
    match f with
    | Top | Bot -> []
    | Lit lit -> predicates_of_literal lit
    | And (f1, f2) | Or (f1, f2) -> aux f1 @ aux f2
    | Not f' -> aux f'
    | Exists (_, f') | Forall (_, f') -> aux f'
  in
  uniq_pairs (aux fml)

(** gathers SPASS-compatible list of functions *)
let spassify_functions (fml : formula) =
  let funs = get_all_functions fml in
  List.map (fun (name, ar) -> Printf.sprintf "(%s, %d)" name ar) funs
  |> String.concat ","

(** gathers SPASS-compatible list of predicates *)
let spassify_preds (fml : formula) =
  let preds = get_all_predicates fml in
  List.map (fun (name, ar) -> Printf.sprintf "(%s, %d)" name ar) preds
  |> String.concat ","

let big_and (fl : formula list) =
  List.fold_left (fun f1 f2 -> And (f1, f2)) Top fl

(** Given two formulas, generate the problem
    [axioms |- forall [freevars] . f1 => f2 ] *)
let generate_implication_problem f1 f2 =
  let axioms = List.map spassify !current_axioms in
  let axioms =
    List.mapi (fun i x -> Printf.sprintf "formula(%s, %d)." x (i + 1)) axioms
    |> String.concat "\n"
  in

  Printf.sprintf
    "\n\
     begin_problem(Implication).\n\n\
     list_of_descriptions.\n\
     name({*Implication*}).\n\
     author({*Lorenzo Pace*}).\n\
     status(unsatisfiable).\n\
     description({* Implication problem, generated by the tapes tool. *}).\n\
     end_of_list.\n\
    \ \n\
     list_of_symbols.\n\
    \ %s\n\
    \ %s\n\
     end_of_list.\n\
    \ \n\
     list_of_formulae(axioms).\n\n\
     %s\n\
     end_of_list.\n\n\
     list_of_formulae(conjectures).\n\
    \ \n\
     formula(%s,%d).\n\
    \ \n\
     end_of_list.\n\
    \ \n\
     end_problem.  \n\n\
    \  "
    (let fs =
       spassify_functions (And (And (f1, f2), big_and !current_axioms))
     in
     if fs = "" then "" else Printf.sprintf "functions[%s]." fs)
    (let fs = spassify_preds (And (And (f1, f2), big_and !current_axioms)) in
     if fs = "" then "" else Printf.sprintf "predicates[%s]." fs)
    axioms
    (spassify (Or (Not f1, f2) |> universally_quantify_fvs))
    (List.length !current_axioms + 1)
