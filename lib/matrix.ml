open Tapes
open Rewrite
open Typecheck
open Term_to_tape

type 'a matrix = 'a list list

let rec linearize (idx : int) (t : tape) : (int * tape) list =
  let t = tape_to_seq t |> group_composition_right |> deep_clean_tape in
  match t with
  | Tape _ ->
      if idx = 0 then [ (idx, t) ] else failwith "tape: index was not zero"
  | TId _ -> if idx = 0 then [ (idx, t) ] else failwith "id: index was not zero"
  | Join u ->
      [
        (if idx < 2 then (0, TId [ u ])
         else failwith "out of bounds index in join");
      ]
  | Split u ->
      if idx = 0 then [ (0, TId [ u ]); (1, TId [ u ]) ]
      else failwith "split: index was not zero."
  | SwapPlus (x, y) ->
      (* Printf.printf "idx: %d\n" idx; *)
      [
        (if idx = 0 then (1, TId [ x ])
         else if idx = 1 then (0, TId [ y ])
         else failwith "wahoo");
      ]
  | Spawn _ -> []
  | Cut _ -> []
  | TId0 -> []
  | Oplus (t1, t2) ->
      (* Printf.printf "oplus: %s + %s\n" (show_tape t1) (show_tape t2); *)
      let num_monomials_art1 = List.length (tape_arity t1) in
      let num_monomials_coart1 = List.length (tape_coarity t1) in
      let num_monomials_art2 = List.length (tape_arity t2) in

      if idx < num_monomials_art1 then linearize idx t1
      else if idx < num_monomials_art1 + num_monomials_art2 then
        List.map
          (fun (i, t) -> (i + num_monomials_coart1, t))
          (linearize (idx - num_monomials_art1) t2)
      else
        failwith (Printf.sprintf "out of bounds monomial index %d in oplus" idx)
  | TCompose (t1, t2) ->
      let l1 = linearize idx t1 in
      let indices = List.map fst l1 |> List.sort_uniq Int.compare in
      (* Printf.printf "indices: [%s]\n"
        (List.map string_of_int indices |> String.concat ", ");
      flush stdout; *)
      let part =
        List.map (fun i -> List.filter (fun (j, _) -> j = i) l1) indices
      in
      (* Printf.printf "part: [%s]\n"
        (List.map
           (fun x ->
             List.map
               (fun (i, t) -> Printf.sprintf "(%d, %s)" i (show_tape t))
               x)
           part
        |> List.map (String.concat ";")
        |> String.concat "|"); 
      flush stdout;*)

      let l2 = List.map (fun i -> linearize i t2) indices in
      (* Printf.printf "l2: [%s]\n"
        (List.map
           (fun x ->
             List.map
               (fun (i, t) -> Printf.sprintf "(%d, %s)" i (show_tape t))
               x)
           l2
        |> List.map (String.concat ";")
        |> String.concat "|");
      flush stdout; *)
      let composed =
        List.map2
          (fun a b ->
            List.map
              (fun (_, ta) ->
                List.map (fun (ib, tb) -> (ib, TCompose (ta, tb))) b)
              a
            |> List.flatten)
          part l2
        |> List.flatten
      in
      (* Printf.printf "composed: [%s]\n"
        (List.map
           (fun (i, t) -> Printf.sprintf "(%d, %s)" i (show_tape t))
           composed
        |> String.concat ";");
      flush stdout; *)
      composed
  | Trace _ -> failwith "traces not allowed in linearization"
(* | _ -> failwith (Printf.sprintf "failed, met %s.\n" (show_tape t)) *)

let transpose matrix =
  let rec transpose_aux acc = function
    | [] -> List.rev acc
    | [] :: _ -> List.rev acc
    | rows ->
        let heads = List.map List.hd rows in
        let tails = List.map List.tl rows in
        transpose_aux (heads :: acc) tails
  in
  transpose_aux [] matrix

let matrix_dimensions (matrix : 'a matrix) : int * int =
  match matrix with
  | [] -> (0, 0) (* No rows *)
  | row :: _ -> (List.length matrix, List.length row)

(** Each entry of the matrix is a multiset (represented here as a list) of tapes*)
let get_matrix (t : tape) =
  let ar = tape_arity t in
  let coar = tape_coarity t in
  let paths = List.mapi (fun i _ -> linearize i t) ar in

  (* Printf.printf "num paths: %s\n"
    (List.map List.length paths |> List.map string_of_int |> String.concat ", "); *)
  let matrix =
    List.map
      (fun l ->
        List.mapi
          (fun i _ ->
            List.filter (fun (j, _) -> i = j) l |> List.map (fun (_, t) -> t))
          coar)
      paths
  in
  transpose matrix

let get_matrix_entry (m : 'a matrix) (i : int) (j : int) =
  List.nth (List.nth m i) j

let matrix_mapij f = List.mapi (fun i r -> List.mapi (fun j x -> f i j x) r)

let get_column_arity (c : tape list list) =
  try
    (* further checks here? *)
    let ars = List.map (fun t -> List.hd (tape_arity t)) (List.flatten c) in
    let fst = List.hd ars in
    if List.for_all (fun ar -> ar = fst) ars then fst
    else failwith "trying to get arity of non-uniform column"
  with Not_found -> failwith "error in get_column_arity"

let get_row_coarity (r : tape list list) =
  try
    (* further checks here? *)
    let coars = List.map (fun t -> List.hd (tape_coarity t)) (List.flatten r) in
    let fst = List.hd coars in
    if List.for_all (fun ar -> ar = fst) coars then fst
    else failwith "trying to get arity of non-uniform column"
  with Not_found -> failwith "error in get_column_arity"

let string_of_annotated_matrix
    (m : (tape list * string list * string list) list list) =
  let string_of_tapes l = List.map pp_tape l |> String.concat ", " in
  List.map
    (List.map (fun (l, ar, coar) ->
         Printf.sprintf "(%s, %s, %s)" (string_of_tapes l) (pp_sort_list ar)
           (pp_sort_list coar)))
    m
  |> List.map (String.concat ", ")
  |> String.concat "\n"

let rec bigoplus (i : int) (f : int -> tape) =
  match i with 0 -> f 0 | n -> Oplus (bigoplus (n - 1) f, f n)

let tape_of_matrix (m : tape list matrix) (arities : sort list list)
    (coarities : sort list list) =
  (* let m_dag = transpose m in *)
  (* let coarities = List.map get_row_coarity m in
  Printf.printf "coarities: %s\n"
    (List.map pp_sort_list coarities |> String.concat ", ");
  let arities = List.map get_column_arity m_dag in
  Printf.printf "arities: %s\n"
    (List.map pp_sort_list arities |> String.concat ", "); *)

  (* transform matrix of multiset to matrix of tapes by wrapping in split and join *)
  let wrap_with_split_join ar coar l =
    match l with
    | [] -> TCompose (Cut ar, Spawn coar)
    | _ ->
        let x = List.fold_left (fun a b -> Oplus (a, b)) TId0 l in
        TCompose
          ( multi_split (List.length l) ar,
            TCompose (x, multi_join (List.length l) coar) )
  in

  let mat =
    matrix_mapij
      (fun i j l ->
        let ar = List.nth arities j in
        let coar = List.nth coarities i in
        wrap_with_split_join ar coar l)
      m
  in
  (* t1 = Otimes_i (multisplit (#coarities) arities[i])
     t2 = Otimes_i (Otimes_j m_{ij})
     t3 = multijoin (#arities) (Otimes_j coarities[j]) *)
  let n = List.length arities in
  let m = List.length coarities in
  let t1 = bigoplus (n - 1) (fun i -> multi_split m (List.nth arities i)) in
  let t2 =
    bigoplus (n - 1) (fun i ->
        bigoplus (m - 1) (fun j -> get_matrix_entry mat j i))
  in
  let t3 = multi_join_pol n coarities in
  TCompose (t1, TCompose (t2, t3)) |> deep_clean_tape

let scalar_mult (t : tape) (m : tape list matrix) =
  matrix_mapij (fun _ _ l -> List.map (fun t1 -> otimes_to_tape t t1) l) m

let mat_mult_generic ~(zero : 'c) ~(add : 'c -> 'c -> 'c)
    ~(mul : 'a -> 'b -> 'c) (a : 'a matrix) (b : 'b matrix) : 'c matrix =
  (* transpose B to make column-access easy *)
  let b_t = transpose b in
  (* dot product of row and column *)
  let dot row col =
    try List.fold_left2 (fun acc x y -> add acc (mul x y)) zero row col
    with _ -> failwith "tried to take dot product of incompatible vectors"
  in
  List.map (fun row -> List.map (fun col -> dot row col) b_t) a

(* TODO test *)
let mat_mult (a : tape list matrix) (b : tape list matrix) =
  let zero = [] in
  let add x y = x @ y in
  let mul x y =
    List.map (fun t1 -> List.map (fun t2 -> TCompose (t1, t2)) y) x
    |> List.flatten
  in
  mat_mult_generic ~zero ~add ~mul a b

let normalize (t : tape) =
  let normalized =
    tape_of_matrix (t |> get_matrix) (tape_arity t) (tape_coarity t)
  in
  normalized |> deep_clean_tape

let term_to_normalized_tape (t : Terms.term) =
  let trace_normalized = Rewrite.trace_normal_form t in
  match trace_normalized with
  | Trace (l, t') -> iterate_trace l (normalize (_to_tape t'))
  | _ -> normalize (_to_tape t)
