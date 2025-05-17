open Tapes

type circuit_draw_interface =
  | EmptyCircuit
  | CircuitTens of circuit_draw_interface * circuit_draw_interface
  | CircuitPin of float * float
[@@deriving show]

type tape_draw_interface =
  | EmptyInterface of (float * float) option * (float * float) option
  | EmptyTape of (float * float) * (float * float)
  | TapeTens of tape_draw_interface * tape_draw_interface
  | TapeInterface of (float * float) * (float * float) * circuit_draw_interface
[@@deriving show]

type circuit_block =
  | BMeasure of {
      fresh_name : string;
      len : float;
      pos : float * float;
    }
  | BId of {
      fresh_name : string;
      pos : float * float;
      len : float;
      sort : string;
    }
  | BSwap of {
      fresh_name : string;
      pos : float * float;
      scaley : float;
      sorts : string * string;
    }
  | BCopy of {
      fresh_name : string;
      pos : float * float;
      scaley : float;
      sort : string;
    }
  | BDiscard of {
      fresh_name : string;
      pos : float * float;
      sort : string;
    }
  | BCoCopy of {
      fresh_name : string;
      pos : float * float;
      scaley : float;
      sort : string;
    }
  | BCoDiscard of {
      fresh_name : string;
      pos : float * float;
      sort : string;
    }
  | BGen of {
      fresh_name : string;
      pos : float * float;
      arity : int;
      coarity : int;
      name : string;
      otimesdist : float;
      sorts : string list * string list;
    }
  | Connector of {
      pos1 : float * float;
      pos2 : float * float;
    }
[@@deriving show]

type circuit_geometry =
  | CircGeo of {
      tikz : circuit_block list;
      height : float;
      length : float;
      left_interface : circuit_draw_interface;
      right_interface : circuit_draw_interface;
    }
[@@deriving show]

type tape_geometry =
  | TapeGeo of {
      tikz : string;
      height : float;
      length : float;
      left_interface : tape_draw_interface;
      right_interface : tape_draw_interface;
    }
[@@deriving show]

(********************************************************************************************************)

(* settings: these can be controlled from the language *)
let oplus_dist = ref 0.25
let otimes_dist = ref 0.5
let tape_padding = ref 0.25
let align_summands = ref true
(* Circuit interface utils *)

(* Flatten the circuit tree into a list of CircuitPin nodes *)
let rec flatten_circuit (c : circuit_draw_interface) : (float * float) list =
  match c with
  | EmptyCircuit -> []
  | CircuitPin (x, y) -> [ (x, y) ]
  | CircuitTens (c1, c2) -> flatten_circuit c1 @ flatten_circuit c2

(* Rebuild the circuit tree from a list of pins in left-associated form *)
let rec rebuild_circuit (pins : (float * float) list) : circuit_draw_interface =
  match pins with
  | [] -> EmptyCircuit
  | (x, y) :: [] -> CircuitPin (x, y)
  | (x, y) :: rest -> CircuitTens (CircuitPin (x, y), rebuild_circuit rest)

(* The normalization function: flatten then rebuild *)
let circuit_interface_normalize (c : circuit_draw_interface) :
    circuit_draw_interface =
  let pins = flatten_circuit c in
  rebuild_circuit pins

(* circuit operations: init, rev *)
let rec circuit_interface_init (n : int) (f : int -> circuit_draw_interface) =
  if n = 0 then EmptyCircuit
  else
    CircuitTens
      (circuit_interface_init (n - 1) f, circuit_interface_normalize (f n))

let rec circuit_interface_rev (c : circuit_draw_interface) =
  circuit_interface_normalize
    (match c with
    | EmptyCircuit -> EmptyCircuit
    | CircuitPin _ -> c
    | CircuitTens (c1, c2) ->
        CircuitTens (circuit_interface_rev c2, circuit_interface_rev c1))

(* map for circuits *)
let rec circuit_interface_map
    (f : circuit_draw_interface -> circuit_draw_interface)
    (c : circuit_draw_interface) =
  let c = circuit_interface_normalize c in
  circuit_interface_normalize
    (match c with
    | EmptyCircuit -> c
    | CircuitPin _ -> f c
    | CircuitTens (c1, c2) -> CircuitTens (f c1, circuit_interface_map f c2))

(* creates a list by mapping over the elements of a circuit interface *)
let rec circuit_interface_to_list_map (f : circuit_draw_interface -> 'a list)
    (c : circuit_draw_interface) =
  let c = circuit_interface_normalize c in
  match c with
  | EmptyCircuit | CircuitPin _ -> f c
  | CircuitTens (c1, c2) -> f c1 @ circuit_interface_to_list_map f c2

(* returns the lowest position of a circuit *)
let top_of_circuit_interface c =
  match circuit_interface_rev c with
  | EmptyCircuit -> None
  | CircuitPin (x, y) | CircuitTens (CircuitPin (x, y), _) -> Some (x, y)
  | _ -> failwith "malformed circuit interface 1"

(* returns the hightst position of a circuit *)
let rec base_of_circuit_interface c =
  match circuit_interface_normalize c with
  | EmptyCircuit -> None
  | CircuitTens (_, c2) -> base_of_circuit_interface c2
  | CircuitPin (x, y) -> Some (x, y)

(* returns lowest y value of two circuit interfaces *)
let y_base_of_circuit_interfaces c1 c2 =
  match (base_of_circuit_interface c1, base_of_circuit_interface c2) with
  | None, None -> None
  | None, Some (_, y) -> Some y
  | Some (_, y), None -> Some y
  | Some (_, y1), Some (_, y2) -> Some (min y1 y2)

(* transforms a list into a circuit interface *)
let rec circuit_interface_of_list l =
  match l with
  | [] -> EmptyCircuit
  | (x, y) :: xs -> CircuitTens (CircuitPin (x, y), circuit_interface_of_list xs)

let rec circuit_interface_height (c : circuit_draw_interface) =
  match circuit_interface_normalize c with
  | EmptyCircuit | CircuitPin _ -> 0.
  | CircuitTens (t1, t2) ->
      circuit_interface_height t1 +. circuit_interface_height t2 +. !otimes_dist

(********************************************************************************************************)
(* Tape interface utils *)

(* this gets rid of all empty interfaces. Is this correct? *)
let rec clean_tape_interface t =
  match t with
  | TapeTens (EmptyInterface _, t1) -> clean_tape_interface t1
  | TapeTens (t1, EmptyInterface _) -> clean_tape_interface t1
  | _ -> t

let rec deep_clean_interface t =
  let ct = clean_tape_interface t in
  if t == ct then ct else deep_clean_interface ct

let rec list_of_tape_interface t =
  match t with
  | EmptyTape _ | TapeInterface _ | EmptyInterface _ -> [ t ]
  | TapeTens (t1, t2) -> list_of_tape_interface t1 @ list_of_tape_interface t2

let rec tape_interface_of_list t =
  match t with
  | t1 :: [] -> t1
  | t1 :: rest -> TapeTens (t1, tape_interface_of_list rest)
  | [] -> EmptyInterface (None, None)

(* bring a tape interface to a list-like normal form *)
let tape_interface_normalize (t : tape_draw_interface) =
  t |> deep_clean_interface |> list_of_tape_interface |> tape_interface_of_list
  |> deep_clean_interface

(* NOTE: the empty interface also has information, so we want to apply the function to it as well.*)
let rec tape_interface_map (f : tape_draw_interface -> tape_draw_interface)
    (t : tape_draw_interface) =
  tape_interface_normalize
    (let t = tape_interface_normalize t in
     match t with
     | EmptyTape _ -> f t
     | TapeInterface _ -> f t
     | EmptyInterface _ -> f t
     | TapeTens (t1, t2) -> TapeTens (f t1, tape_interface_map f t2))

(* returns the lowest position of a tape interface *)
let rec base_of_tape_interface t =
  match tape_interface_normalize t with
  | EmptyTape ((x, y), _) -> (x, y)
  | TapeTens (_, t2) -> base_of_tape_interface t2
  | TapeInterface ((x, y), _, _) -> (x, y)
  | EmptyInterface (Some (x, y), _) -> (x, y)
  | _ -> failwith "tried to get base of empty interface"

(* returns the highest position of a tape interface *)
let rec top_of_tape_interface t =
  match tape_interface_normalize t with
  | EmptyTape (_, (x, y)) -> (x, y)
  | TapeTens (t1, _) -> top_of_tape_interface t1
  | TapeInterface (_, (x, y), _) -> (x, y)
  | EmptyInterface (_, Some (x, y)) -> (x, y)
  | _ -> failwith "tried to get top of empty interface"

(* returns the lowest position of a pair of tape interfaces *)
let base_of_tape_interfaces t1 t2 =
  let x1, y1 = base_of_tape_interface t1 in
  let x2, y2 = base_of_tape_interface t2 in
  if y1 > y2 then (x1, y1) else (x2, y2)

let rec tape_interface_to_string_map2
    (f : tape_draw_interface -> tape_draw_interface -> string)
    (t1 : tape_draw_interface) (t2 : tape_draw_interface) =
  let t1, t2 =
    ( t1 |> deep_clean_interface |> tape_interface_normalize,
      t2 |> deep_clean_interface |> tape_interface_normalize )
  in
  match (t1, t2) with
  | EmptyTape _, EmptyTape _ | TapeInterface _, TapeInterface _ -> f t1 t2
  | TapeTens (t11, t21), TapeTens (t12, t22) ->
      f t11 t12 ^ tape_interface_to_string_map2 f t21 t22
  | EmptyInterface _, EmptyInterface _ -> "" (* TODO is this right?*)
  | _ ->
      print_endline
        ("failure: \n"
        ^ show_tape_draw_interface t1
        ^ "\n\n"
        ^ show_tape_draw_interface t2);

      failwith
        "tape_interface_to_string_map2 could not be applied, args have \
         different sizes."

let rec tape_interface_height (t : tape_draw_interface) =
  match tape_interface_normalize t with
  | EmptyTape ((_, y1), (_, y2)) -> abs_float (y1 -. y2)
  | EmptyInterface (Some (_, y1), Some (_, y2)) -> abs_float (y1 -. y2)
  | EmptyInterface (None, None) -> 0.
  | TapeTens (t1, t2) -> (
      match (t1, t2) with
      | EmptyInterface (None, None), EmptyInterface (None, None) -> 0.
      | EmptyInterface (None, None), _ -> tape_interface_height t2
      | _, EmptyInterface (None, None) -> tape_interface_height t1
      | EmptyInterface (Some _, Some _), EmptyInterface (Some _, Some _) | _ ->
          let _, y_top = top_of_tape_interface t1 in
          let _, y_bot = base_of_tape_interface t2 in
          abs_float (y_top -. y_bot))
  | TapeInterface ((_, y1), (_, y2), _) -> abs_float (y1 -. y2)
  | _ -> failwith "malformed empty interface?"

(********************************************************************************************************)
(* counters used in generating fresh ids for the tikz elements *)

let id_counter = ref 0
let swap_counter = ref 0
let gen_counter = ref 0
let meas_counter = ref 0

let fresh_id () =
  id_counter := !id_counter + 1;
  string_of_int !id_counter

let fresh_swap () =
  swap_counter := !swap_counter + 1;
  string_of_int !swap_counter

let fresh_gen () =
  gen_counter := !gen_counter + 1;
  string_of_int !gen_counter

let fresh_meas () =
  meas_counter := !meas_counter + 1;
  string_of_int !meas_counter

(********************************************************************************************************)
(* Drawing Circuits *)

let rec get_circuit_height (c : circuit) =
  match c with
  | CId _ | CId1 -> 0.0
  | SwapTimes _ -> !otimes_dist
  | Otimes (t1, t2) ->
      get_circuit_height t1 +. get_circuit_height t2 +. !otimes_dist
  | CCompose (t1, t2) -> max (get_circuit_height t1) (get_circuit_height t2)
  | Gen (_, ar, coar) ->
      let ar_size = List.length ar in
      let coar_size = List.length coar in
      float_of_int (max (max (ar_size - 1) (coar_size - 1)) 0) *. !otimes_dist

let rec get_circuit_left_interface_height (c : circuit) =
  match c with
  | CId _ | CId1 | SwapTimes _ -> get_circuit_height c
  | Gen (_, ar, _) ->
      print_float (max 0. (float_of_int (List.length ar - 1)) *. !otimes_dist);
      max 0. (float_of_int (List.length ar - 1)) *. !otimes_dist
  | Otimes (t1, t2) ->
      get_circuit_left_interface_height t1
      +. get_circuit_left_interface_height t2
      +. !otimes_dist
  | CCompose (t1, _t2) -> get_circuit_left_interface_height t1

let rec get_circuit_within_tape_left_interface_height (c : circuit) =
  match c with
  | CId _ | CId1 | SwapTimes _ -> get_circuit_height c
  | Gen _ -> get_circuit_height c
  | Otimes (t1, t2) ->
      get_circuit_within_tape_left_interface_height t1
      +. get_circuit_within_tape_left_interface_height t2
      +. !otimes_dist
  | CCompose (t1, _t2) -> get_circuit_within_tape_left_interface_height t1

let rec circuit_connect_interfaces (ina : circuit_draw_interface)
    (inb : circuit_draw_interface) =
  match (circuit_interface_normalize ina, circuit_interface_normalize inb) with
  | EmptyCircuit, EmptyCircuit -> ""
  | CircuitPin (x1, y1), CircuitPin (x2, y2) ->
      Printf.sprintf "\\draw [in=180, out=0] (%f , %f) to (%f , %f);\n" x1 y1 x2
        y2
  | ( CircuitTens (CircuitPin (x1, y1), ina1),
      CircuitTens (CircuitPin (x2, y2), inb1) ) ->
      Printf.sprintf "\\draw [in=180, out=0] (%f , %f) to (%f , %f);\n" x1 y1 x2
        y2
      ^ circuit_connect_interfaces ina1 inb1
  | _ ->
      Printf.printf "======\nina: %s\n////\ninb: %s\n======="
        (show_circuit_draw_interface ina)
        (show_circuit_draw_interface inb);
      failwith "trying to connect incompatible circuit interfaces"

let circuit_align_interfaces ri1 ri2 =
  let ri1, ri2 =
    (circuit_interface_normalize ri1, circuit_interface_normalize ri2)
  in
  match (ri1, ri2) with
  | EmptyCircuit, EmptyCircuit -> (ri1, ri2)
  | ( CircuitTens (CircuitPin (posx1, _posy1), _),
      CircuitTens (CircuitPin (posx2, _posy2), _) )
  | CircuitTens (CircuitPin (posx1, _posy1), _), CircuitPin (posx2, _posy2)
  | CircuitPin (posx1, _posy1), CircuitTens (CircuitPin (posx2, _posy2), _)
  | CircuitPin (posx1, _posy1), CircuitPin (posx2, _posy2) ->
      let max_x = max posx1 posx2 in
      let f =
       fun c ->
        match c with CircuitPin (_, y) -> CircuitPin (max_x, y) | _ -> c
      in
      (circuit_interface_map f ri1, circuit_interface_map f ri2)
  | _ -> failwith "malformed circuit interface 2"

(********************************************************************************************************)
(* Drawing Tapes *)

(* transforms a tape identity into ⊕_i (Tape(⊗_j (circuit ids))) *)
let tid_to_normal_form (l : Terms.sort list list) =
  let to_tape (l1 : Terms.sort list) =
    Tape (List.fold_left (fun acc x -> Otimes (acc, CId x)) CId1 l1)
  in
  deep_clean_tape
    (List.fold_left (fun acc x -> Oplus (acc, x)) TId0 (List.map to_tape l))

let rec is_tape_identity (t : tape) =
  match t with
  | TId0 | Tape CId1 | Tape (CId _) -> true
  | Oplus (t1, t2) -> is_tape_identity t1 && is_tape_identity t2
  | Tape (Otimes (c1, c2)) ->
      is_tape_identity (Tape c1) && is_tape_identity (Tape c2)
  | _ -> false

(* returns height of tape *)
let rec get_tape_height (t : tape) =
  match t with
  | TId l -> get_tape_height (tid_to_normal_form l)
  | TId0 -> !tape_padding *. 2.
  | Tape c -> get_circuit_height c +. (2. *. !tape_padding)
  | TCompose (t1, t2) -> max (get_tape_height t1) (get_tape_height t2)
  | Oplus (t1, t2) -> get_tape_height t1 +. get_tape_height t2 +. !oplus_dist
  | SwapPlus (l1, l2) ->
      let len1, len2 = (List.length l1, List.length l2) in
      let h1 =
        (float_of_int (len1 - 1) *. !otimes_dist) +. (!tape_padding *. 2.)
      in
      let h2 =
        (float_of_int (len2 - 1) *. !otimes_dist) +. (!tape_padding *. 2.)
      in
      h1 +. h2 +. !oplus_dist
  | Cut l1 ->
      let n = List.length l1 in
      (!tape_padding *. 2.) +. (float_of_int (max 0 (n - 1)) *. !otimes_dist)
  | Split l1 ->
      let n = List.length l1 in
      (!tape_padding *. 4.)
      +. (2. *. float_of_int (max 0 (n - 1)) *. !otimes_dist)
      +. !oplus_dist
  | Spawn l1 ->
      let n = List.length l1 in
      (!tape_padding *. 2.) +. (float_of_int (max 0 (n - 1)) *. !otimes_dist)
  | Join l1 ->
      let n = List.length l1 in
      (!tape_padding *. 4.)
      +. (2. *. float_of_int (max 0 (n - 1)) *. !otimes_dist)
      +. !oplus_dist

let rec get_tape_left_interface_height (t : tape) =
  match t with
  | Tape c ->
      get_circuit_within_tape_left_interface_height c +. (2. *. !tape_padding)
  | TCompose (t1, _) -> get_tape_left_interface_height t1
  | Oplus (t1, t2) ->
      get_tape_left_interface_height t1
      +. get_tape_left_interface_height t2
      +. !oplus_dist
  | _ -> get_tape_height t

(* counts summands in left interface *)
let rec get_tape_left_interface_size (t : tape) =
  match t with
  | TId0 -> 1
  | Tape _ -> 1
  | SwapPlus _ -> 2
  | Cut _ -> 1
  | Split _ -> 1
  | Spawn _ -> 0
  | Join _ -> 2
  | TId l -> max 1 (List.length l)
  | TCompose (t1, _) -> get_tape_left_interface_size t1
  | Oplus (t1, t2) ->
      get_tape_left_interface_size t1 + get_tape_left_interface_size t2

let rec get_max_x_tape t =
  match t with
  | EmptyTape (_, (x, _)) -> x
  | TapeInterface (_, (x, _), _) -> x
  | TapeTens (t1, t2) -> max (get_max_x_tape t1) (get_max_x_tape t2)
  | EmptyInterface (Some (x, _), _) -> x
  | EmptyInterface (None, None) -> -.infinity
  | _ -> failwith "malformed empty interface?"

let rec get_summand_list t =
  match t with
  | Oplus (t1, t2) -> get_summand_list t1 @ get_summand_list t2
  | _ -> [ t ]

let align_circuit_interface_to_x c x =
  circuit_interface_map
    (function
      | CircuitPin (_, y) -> CircuitPin (x, y)
      | _ -> failwith "should not be called 1")
    c

let align_tape_interface_to_x t x =
  tape_interface_map
    (function
      | EmptyTape ((_, y1), (_, y2)) -> EmptyTape ((x, y1), (x, y2))
      | TapeInterface ((_, y1), (_, y2), c) ->
          TapeInterface ((x, y1), (x, y2), align_circuit_interface_to_x c x)
      | EmptyInterface (Some (_, y1), Some (_, y2)) ->
          EmptyInterface (Some (x, y1), Some (x, y2))
      | EmptyInterface _ -> t
      | _ -> failwith "should not be called 2")
    t

(* same as circuit align interfaces but for tapes. *)
let tape_align_interfaces ri1 ri2 =
  let ri1 = ri1 |> deep_clean_interface |> tape_interface_normalize in
  let ri2 = ri2 |> deep_clean_interface |> tape_interface_normalize in
  let max_1 = get_max_x_tape ri1 in
  let max_2 = get_max_x_tape ri2 in
  let max_x = max max_1 max_2 in
  (align_tape_interface_to_x ri1 max_x, align_tape_interface_to_x ri2 max_x)

(*   | _ -> print_endline ("failure: \n" ^ (show_tape_draw_interface ri1) ^ "\n" ^ (show_tape_draw_interface ri2)) ; failwith ("invalid tape interfaces") *)

(* debug util that draws colored circles on the interfaces *)
let rec tape_highlight_interfaces (col : string)
    (ina : (float * float) list list) =
  match ina with
  | [] -> ""
  | l1 :: ina1 ->
      String.concat ""
        (List.map
           (fun x ->
             Printf.sprintf
               "\\node [circle, fill=%s, draw] () at (%f, %f) {};\n" col (fst x)
               (snd x))
           l1)
      ^ tape_highlight_interfaces col ina1

let construct_tape_between _pos_bot1 _pos_top1 _pos_bot2 _pos_top2 =
  match (_pos_bot1, _pos_top1, _pos_bot2, _pos_top2) with
  | ( (_pos_bot1x, _pos_bot1y),
      (_pos_top1x, _pos_top1y),
      (_pos_bot2x, _pos_bot2y),
      (_pos_top2x, _pos_top2y) ) ->
      Printf.sprintf "\\freestyletape {%f} {%f} {%f} {%f} {%f} {%f} {%f} {%f}"
        _pos_bot1x _pos_bot1y _pos_top1x _pos_top1y _pos_bot2x _pos_bot2y
        _pos_top2x _pos_top2y

let rec list_take n = function
  | _ when n <= 0 -> []
  | [] -> []
  | x :: xs -> x :: list_take (n - 1) xs

let rec list_drop n = function
  | l when n <= 0 -> l
  | [] -> []
  | _ :: xs -> list_drop (n - 1) xs

let rec list_sum = function [] -> 0 | x :: xs -> x + list_sum xs

(* returns a list of pairs (interface, tape), matched by size *)
let rec pair_intfs_tapes (is : tape_draw_interface list) (ts : tape list) =
  (* Printf.printf "====\nis:\n" ; List.iter (fun x -> print_endline (show_tape_draw_interface x)) is ;
  Printf.printf "\nts:\n====" ; List.iter (fun x -> print_endline (show_tape x)) ts ; *)
  if list_sum (List.map get_tape_left_interface_size ts) = List.length is then
    match ts with
    | [] -> []
    | t :: xs ->
        let n = get_tape_left_interface_size t in
        if n != 0 then
          (tape_interface_of_list (list_take n is), t)
          :: pair_intfs_tapes (list_drop n is) xs
        else
          raise
            (Errors.RuntimeError
               "Used alignment procedure on non-alignable summands -- TODO \
                make all summands alignable")
  else
    raise
      (Errors.TypeError
         (Printf.sprintf
            "trying to pair incompatible interface and type (lengths %d vs %d) \
             -- should be impossible"
            (List.length is)
            (list_sum (List.map get_tape_left_interface_size ts))))

(* connects two tape interfaces *)
let tape_connect_interfaces (ina : tape_draw_interface)
    (inb : tape_draw_interface) =
  let ina = deep_clean_interface ina in
  let inb = deep_clean_interface inb in
  (* printf []
    "\n====================\nAlign: \n%s\n with\n %s\n====================\n"
    (show_tape_draw_interface ina)
    (show_tape_draw_interface inb); *)
  let res =
    tape_interface_to_string_map2
      (fun t1 t2 ->
        match (t1, t2) with
        | EmptyTape _, EmptyTape _ -> ""
        | ( TapeInterface (_pos_bot1, _pos_top1, c1),
            TapeInterface (_pos_bot2, _pos_top2, c2) ) ->
            construct_tape_between _pos_bot1 _pos_top1 _pos_bot2 _pos_top2
            ^ circuit_connect_interfaces c1 c2
        | _ ->
            Printf.printf "==========\n%s\n//\n%s\n========== "
              (show_tape_draw_interface ina)
              (show_tape_draw_interface inb);
            failwith "trying to connect incompatible tape interfaces")
      ina inb
  in
  (* printf [] "\n-----\noutput: %s\n-----\n" res; *)
  res

let rec list_max (l : float list) =
  match l with [] -> -.infinity | a :: rest -> max a (list_max rest)

let max_x_in_diags ds =
  List.map (fun (_d, _h, _l, _li, ri) -> get_max_x_tape ri) ds |> list_max

let diag_adjust_height (d, h, l, li, ri) =
  let hl = tape_interface_height li in
  let hr = tape_interface_height ri in
  (d, max h (max hl hr), l, li, ri)

let stack_diagrams (i, (da, ha, la, lia, ria)) (_, db, hb, lb, optlib, optrib) =
  match (optlib, optrib) with
  | Some lib, Some rib ->
      let ria_aligned, rib_aligned = tape_align_interfaces ria rib in
      ( i,
        da ^ db,
        ha +. hb +. !oplus_dist,
        max la lb,
        Some (TapeTens (lia, lib)),
        Some (TapeTens (ria_aligned, rib_aligned)) )
  | _ -> (i, da, ha, la, Some lia, Some ria)
