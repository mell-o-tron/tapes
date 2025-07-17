open Tapes
open Rewrite
open Typecheck

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
      Printf.printf "idx: %d\n" idx;
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
      Printf.printf "indices: [%s]\n"
        (List.map string_of_int indices |> String.concat ", ");
      flush stdout;
      let part =
        List.map (fun i -> List.filter (fun (j, _) -> j = i) l1) indices
      in
      Printf.printf "part: [%s]\n"
        (List.map
           (fun x ->
             List.map
               (fun (i, t) -> Printf.sprintf "(%d, %s)" i (show_tape t))
               x)
           part
        |> List.map (String.concat ";")
        |> String.concat "|");
      flush stdout;

      let l2 = List.map (fun i -> linearize i t2) indices in
      Printf.printf "l2: [%s]\n"
        (List.map
           (fun x ->
             List.map
               (fun (i, t) -> Printf.sprintf "(%d, %s)" i (show_tape t))
               x)
           l2
        |> List.map (String.concat ";")
        |> String.concat "|");
      flush stdout;
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
      Printf.printf "composed: [%s]\n"
        (List.map
           (fun (i, t) -> Printf.sprintf "(%d, %s)" i (show_tape t))
           composed
        |> String.concat ";");
      flush stdout;
      composed
  | Trace _ -> failwith "traces not allowed in linearization"
(* | _ -> failwith (Printf.sprintf "failed, met %s.\n" (show_tape t)) *)
