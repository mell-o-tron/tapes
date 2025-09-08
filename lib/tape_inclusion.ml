open Tapes
open Matrix
open Fol_encoding
open ANSITerminal
open Typecheck

(** Given two tapes, generates the problems that, collectively, represent the
    inclusion t1 <= t2*)
let generate_implication_problems (t1 : tape) (t2 : tape) =
  let m1 = fol_matrix_of_tape t1 in
  let m2 = fol_matrix_of_tape t2 in
  if matrix_dimensions m1 = matrix_dimensions m2 then (
    let m =
      Matrix.matrix_mapij
        (fun i j x ->
          generate_implication_problem x (Matrix.get_matrix_entry m2 i j))
        m1
    in
    let dir = "./problems" in
    if not (Sys.file_exists dir && Sys.is_directory dir) then
      Unix.mkdir dir 0o755
    else
      Sys.readdir dir
      |> Array.iter (fun x -> Sys.remove (Filename.concat dir x));
    let problems = List.flatten m in
    List.iteri
      (fun i x ->
        try
          let oc = open_out (Printf.sprintf "%s/%d" dir i) in
          Printf.fprintf oc "%s\n" x;
          close_out oc
        with Sys_error e -> eprintf [ red ] "System error: \"%s\"\n" e)
      problems;
    Printf.printf "SPASS files generated in ./problems directory.\n")
  else
    failwith
      "matrix dimensions are not matching: cannot generate implication problems"

(** given two tapes with matching traces and an invariant, produces the
    inclusion problems TODO remove this*)
let inclusion_by_invariant (t1 : tape) (t2 : tape) (invariant : tape) =
  match (t1, t2) with
  | Trace (sl1, t1), Trace (sl2, t2) when sl1 = sl2 ->
      let ari = tape_arity invariant in
      let coari = tape_coarity invariant in
      if ari = coari && ari = [ sl1 ] then
        let inv_l =
          Oplus (invariant, TId (remainder_of_prefix ari (tape_arity t1)))
          |> deep_clean_tape
        in
        let inv_r =
          Oplus (invariant, TId (remainder_of_prefix ari (tape_coarity t2)))
          |> deep_clean_tape
        in

        let t1 = TCompose (inv_l, t1) in
        let t2 = TCompose (t2, inv_r) in
        (* 
        printf [] "arity t1: %s\t coarity t1 %s\n"
          (string_of_sort_list_list (tape_arity t1))
          (string_of_sort_list_list (tape_coarity t1));
        printf [] "arity t2: %s\t coarity t2 %s\n"
          (string_of_sort_list_list (tape_arity t2))
          (string_of_sort_list_list (tape_coarity t2)); *)

        generate_implication_problems t1 t2
      else failwith "non-matching invariant interfaces."
  | _ -> failwith "non-traced terms or non matching trace arities"
