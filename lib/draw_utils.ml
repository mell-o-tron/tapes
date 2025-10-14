open Tapes
open Ppx_compare_lib.Builtin

type circuit_draw_interface =
  | EmptyCircuit
  | EmptyCircuitPin of float * float
  | CircuitTens of circuit_draw_interface * circuit_draw_interface
  | CircuitPin of float * float
[@@deriving show, compare]

type tape_draw_interface =
  | EmptyInterface of (float * float) option * (float * float) option
  | EmptyTape of (float * float) * (float * float)
  | TapeTens of tape_draw_interface * tape_draw_interface
  | TapeInterface of (float * float) * (float * float) * circuit_draw_interface
[@@deriving show, compare]

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
      style : string;
    }
  | Connector of { positions : (float * float) list }
  | EmptyBlock
  | CircuitDebugNode of {
      pos : float * float;
      text : string;
    }
[@@deriving show, compare]

type tape_block =
  | EmptyTBlock
  | BTape of {
      pos : float * float;
      width : float;
      height : float;
    }
  | BFreeStyleTape of {
      posll : float * float;
      poslu : float * float;
      posrl : float * float;
      posru : float * float;
    }
  | BAdapter of {
      pos : float * float;
      height1 : float;
      height2 : float;
    }
  | BSwapTape of {
      pos : float * float;
      n1 : int;
      n2 : int;
      oplusdist : float;
      otimesdist : float;
      tapepadding : float;
      width : float;
    }
  | BSplitTape of {
      pos : float * float;
      n : int;
      len : float;
      tapepadding : float;
      otimesdist : float;
      oplusdist : float;
    }
  | BCutTape of {
      pos : float * float;
      n : int;
      tapepadding : float;
      otimesdist : float;
    }
  | BJoinTape of {
      pos : float * float;
      n : int;
      len : float;
      tapepadding : float;
      otimesdist : float;
      oplusdist : float;
    }
  | BSpawnTape of {
      pos : float * float;
      n : int;
      tapepadding : float;
      otimesdist : float;
    }
  | BTraceTape of {
      pos_l : float * float;
      pos_r : float * float;
      n : int;
      len : int;
      tapepadding : float;
      otimesdist : float;
      oplusdist : float;
      max_y : float;
    }
  | TapeDebugNode of {
      pos : float * float;
      text : string;
    }
[@@deriving show, compare]

type block =
  | CB of circuit_block
  | TB of tape_block
[@@deriving show, compare]

type circuit_geometry =
  | CircGeo of {
      tikz : circuit_block list;
      height : float;
      length : float;
      left_interface : circuit_draw_interface;
      right_interface : circuit_draw_interface;
    }
[@@deriving show, compare]

type tape_geometry =
  | TapeGeo of {
      tikz : block list;
      height : float;
      length : float;
      left_interface : tape_draw_interface;
      right_interface : tape_draw_interface;
    }
[@@deriving show]

(* settings: these can be controlled from the language *)

(** Global value for the vertical distance between summands in tape diagrams. *)
let oplus_dist = ref 0.25

(** Global value for the vertical distance between tensor products in tape
    diagrams. *)
let otimes_dist = ref 0.5

(** Global value for the padding around tape diagrams. *)
let tape_padding = ref 0.25

(** Controls whether summands should be aligned in tape diagrams. *)
let align_summands = ref true

(** Controls whether identity morphisms should have zero length. *)
let zero_len_ids = ref false

(** Controls whether to use the old alignment algorithm for tape diagrams. *)
let old_alignment = ref false

(** Controls whether to wrap traces in identities (should always be true). *)
let wrap_trace_ids = ref true

(** Scaling factor for the x-axis in diagrams. *)
let scale_x = ref 1.

(** Scaling factor for the y-axis in diagrams. *)
let scale_y = ref 1.

(** Controls whether wires should be joined in diagrams. *)
let join_wires = ref false

(** Controls whether wires should be drawn with rounded corners. *)
let rounded_wires = ref true

(** Counter for generating fresh identity names. *)
let id_counter = ref 0

(** Counter for generating fresh swap names. *)
let swap_counter = ref 0

(** Counter for generating fresh generator names. *)
let gen_counter = ref 0

(** Counter for generating fresh measurement names. *)
let meas_counter = ref 0

(********** Tape utils ***********)

(** Wraps a tape in identity morphisms on both sides. *)
let wrap_in_ids (t : tape) =
  let ar = Typecheck.tape_arity t in
  let coar = Typecheck.tape_coarity t in
  TCompose (TCompose (TId ar, t), TId coar)

(********* List utils ***********)

(** Returns the maximum value in a list of floats. *)
let rec list_max (l : float list) =
  match l with [] -> -.infinity | a :: rest -> max a (list_max rest)

(** Returns the minimum value in a list of floats. *)
let rec list_min (l : float list) : float =
  match l with [] -> infinity | a :: rest -> min a (list_min rest)

(********* BLOCK LOGIC **********)

(** Returns the height of a tape block. *)
let tape_block_height (tb : tape_block) =
  match tb with
  | EmptyTBlock -> 0.
  | BTape { height; _ } -> height
  | BFreeStyleTape { posll; poslu; posrl; posru } ->
      max (snd poslu) (snd posru) -. min (snd posll) (snd posrl)
  | BAdapter { height1; height2; _ } -> max height1 height2
  | BSwapTape { n1; n2; oplusdist; otimesdist; tapepadding; _ } ->
      (tapepadding *. 4.)
      +. (float_of_int (max 0 (n1 + n2 - 2)) *. otimesdist)
      +. oplusdist
  | BSplitTape { n; oplusdist; otimesdist; tapepadding; _ } ->
      let h =
        (2. *. tapepadding) +. (float_of_int (max 0 (n - 1)) *. otimesdist)
      in
      (2. *. h) +. oplusdist
  | BCutTape { n; tapepadding; otimesdist; _ } ->
      (float_of_int (max 0 n - 1) *. otimesdist) +. (2. *. tapepadding)
  | BJoinTape { n; oplusdist; otimesdist; tapepadding; _ } ->
      let h =
        (2. *. tapepadding) +. (float_of_int (max 0 (n - 1)) *. otimesdist)
      in
      (2. *. h) +. oplusdist
  | BSpawnTape { n; tapepadding; otimesdist; _ } ->
      (float_of_int (max 0 n - 1) *. otimesdist) +. (2. *. tapepadding)
  | BTraceTape { n; oplusdist; otimesdist; tapepadding; max_y; pos_r; pos_l; _ }
    ->
      (* THIS IS WRONG!! *)
      let h =
        (2. *. tapepadding) +. (float_of_int (max 0 (n - 1)) *. otimesdist)
      in
      max_y +. oplusdist +. h -. min (snd pos_r) (snd pos_l)
  | TapeDebugNode _ -> 0.

(** Returns the y-position of a tape block. *)
let y_pos_of_tape_block (tb : tape_block) : float =
  match tb with
  | EmptyTBlock -> failwith "tried to get position of empty block"
  | BTape { pos; _ }
  | BAdapter { pos; _ }
  | BSwapTape { pos; _ }
  | BSplitTape { pos; _ }
  | BCutTape { pos; _ }
  | BJoinTape { pos; _ }
  | BSpawnTape { pos; _ } ->
      snd pos
  | BFreeStyleTape { posll; posrl; _ } -> min (snd posll) (snd posrl)
  | BTraceTape { pos_l; pos_r; _ } -> min (snd pos_l) (snd pos_r)
  | TapeDebugNode { pos; _ } -> snd pos

(** Returns the y-position of a circuit block. *)
let y_pos_of_circuit_block (tb : circuit_block) : float =
  match tb with
  | EmptyBlock -> failwith "tried to get position of empty block"
  | BCopy { pos; _ }
  | BDiscard { pos; _ }
  | BCoCopy { pos; _ }
  | BCoDiscard { pos; _ }
  | BGen { pos; _ }
  | BSwap { pos; _ }
  | BId { pos; _ } ->
      snd pos
  | CircuitDebugNode { pos; _ } -> snd pos
  | _ -> failwith "not yet implemented"

(** Returns the x-position of a tape block. *)
let x_pos_of_tape_block (tb : tape_block) : float =
  match tb with
  | EmptyTBlock -> failwith "tried to get position of empty block"
  | BTape { pos; _ }
  | BAdapter { pos; _ }
  | BSwapTape { pos; _ }
  | BSplitTape { pos; _ }
  | BCutTape { pos; _ }
  | BJoinTape { pos; _ }
  | BSpawnTape { pos; _ } ->
      fst pos
  | BFreeStyleTape { posll; _ } -> fst posll
  | BTraceTape { pos_l; _ } -> fst pos_l
  | TapeDebugNode { pos; _ } -> fst pos

(** Returns the top y-position of a tape block. *)
let tape_block_top_y (t : tape_block) =
  y_pos_of_tape_block t +. tape_block_height t

(** Returns the base (lowest y) of a list of circuit blocks. *)
let base_of_circuit_blocks (t : circuit_block list) =
  let t =
    List.filter
      (fun x ->
        match x with
        | EmptyBlock | BMeasure _ | Connector _ -> false
        | _ -> true)
      t
  in
  let l = List.map (fun x -> y_pos_of_circuit_block x) t in
  list_min l

(** Returns the base (lowest y) of a list of tape blocks. *)
let base_of_tape_blocks (t : tape_block list) =
  let t = List.filter (fun x -> x != EmptyTBlock) t in
  let l = List.map (fun x -> y_pos_of_tape_block x) t in
  list_min l

(** Returns the top (highest y) of a list of tape blocks. *)
let top_of_tape_blocks (t : tape_block list) =
  let t = List.filter (fun x -> x != EmptyTBlock) t in
  let l = List.map (fun x -> tape_block_top_y x) t in
  list_max l

(** Extracts tape blocks from a list of blocks. *)
let get_tape_blocks (t : block list) =
  List.filter (function TB _ -> true | _ -> false) t
  |> List.map (fun x -> match x with TB x -> x | _ -> failwith "unreachable")

(** Extracts circuit blocks from a list of blocks. *)
let get_circuit_blocks (t : block list) =
  List.filter (function CB _ -> true | _ -> false) t
  |> List.map (fun x -> match x with CB x -> x | _ -> failwith "unreachable")

(** Converts a circuit block to its TikZ representation. *)
let tikz_of_circuit_block (cb : circuit_block) : string =
  match cb with
  | BMeasure { fresh_name; len; pos = x, y } ->
      Printf.sprintf "\\measuretape {%s} {%f} {%f} {%f}\n" fresh_name len x y
  | BId { fresh_name; pos = x, y; len; sort = _ } ->
      let zero_flag = if len = 0. then 0 else 1 in
      Printf.sprintf "\\id{%s}{%f}{%f}{%d}\n" fresh_name x y zero_flag
  | BSwap { fresh_name; pos = x, y; scaley; sorts = _ } ->
      Printf.sprintf "\\swap{%s}{%f}{%f}{%f}\n" fresh_name x y scaley
  | BCopy { fresh_name; pos = x, y; scaley; sort = _ } ->
      Printf.sprintf "\\copycirc {%s}{%f}{%f}{%f}\n" fresh_name x y scaley
  | BDiscard { fresh_name; pos = x, y; sort = _ } ->
      Printf.sprintf "\\discardcirc {%s}{%f}{%f}\n" fresh_name x y
  | BCoCopy { fresh_name; pos = x, y; scaley; sort = _ } ->
      Printf.sprintf "\\cocopycirc {%s}{%f}{%f}{%f}\n" fresh_name x y scaley
  | BCoDiscard { fresh_name; pos = x, y; sort = _ } ->
      Printf.sprintf "\\codiscardcirc {%s}{%f}{%f}\n" fresh_name x y
  | BGen
      {
        fresh_name;
        pos = x, y;
        arity;
        coarity;
        name;
        otimesdist;
        sorts = _;
        style;
      } ->
      Printf.sprintf "\\gen {%s}{%f}{%f}{%d}{%d}{%s}{%f}{%s}\n" fresh_name x y
        arity coarity name otimesdist style
  | Connector { positions = l } ->
      if List.length l = 0 then ""
      else
        let s =
          List.map (fun (x, y) -> Printf.sprintf "(%f,%f)" x y) l
          |> String.concat " to "
        in
        Printf.sprintf "\\draw %s %s;\n"
          (if !rounded_wires then "[in=180, out=0]" else "")
          s
  | EmptyBlock -> ""
  | CircuitDebugNode { pos = x, y; text = t } ->
      Printf.sprintf "\\node () at (%f, %f) {%s};\n" x y t

(** Converts a tape block to its TikZ representation. *)
let tikz_of_tape_block (tb : tape_block) (debug : bool) : string =
  let debug_bounds tb =
    if debug then
      let x = x_pos_of_tape_block tb in
      let top = top_of_tape_blocks [ tb ] in
      let base = base_of_tape_blocks [ tb ] in
      Printf.sprintf
        "\\node () at (%f, %f) {\\color{green}$-$};\n\
        \ \\node () at (%f, %f) {\\color{blue}$-$};"
        x top x base
    else ""
  in

  (match tb with
  | EmptyTBlock -> ""
  | BTape { pos = x, y; width; height } ->
      Printf.sprintf "\\tape {%f} {%f} {%f} {%f}\n" x y width height
  | BFreeStyleTape
      { posll = x1, y1; poslu = x2, y2; posrl = x3, y3; posru = x4, y4 } ->
      Printf.sprintf "\\%s {%f} {%f} {%f} {%f} {%f} {%f} {%f} {%f}\n"
        (if !rounded_wires then "roundedfreestyletape" else "freestyletape")
        x1 y1 x2 y2 x3 y3 x4 y4
  | BAdapter { pos = x, y; height1; height2 } ->
      Printf.sprintf "\\adapter {%f} {%f} {%f} {%f}\n" x y height1 height2
  | BSwapTape { pos = x, y; n1; n2; oplusdist; otimesdist; tapepadding; width }
    ->
      Printf.sprintf "\\swaptape {%f} {%f} {%d} {%d} {%f} {%f} {%f} {%f}\n" x y
        n1 n2 oplusdist otimesdist tapepadding width
  | BSplitTape { pos = x, y; n; len = l; tapepadding; otimesdist; oplusdist } ->
      Printf.sprintf "\\splittape {%f} {%f} {%d} {%f} {%f} {%f} {%f}\n" x y n l
        tapepadding otimesdist oplusdist
  | BCutTape { pos = x, y; n; tapepadding; otimesdist } ->
      Printf.sprintf "\\cuttape {%f} {%f} {%d} {%f} {%f}\n" x y n tapepadding
        otimesdist
  | BSpawnTape { pos = x, y; n; tapepadding; otimesdist } ->
      Printf.sprintf "\\spawntape {%f} {%f} {%d} {%f} {%f}\n" x y n tapepadding
        otimesdist
  | BJoinTape { pos = x, y; n; len = l; tapepadding; otimesdist; oplusdist } ->
      Printf.sprintf "\\jointape {%f} {%f} {%d} {%f} {%f} {%f} {%f}\n" x y n l
        tapepadding otimesdist oplusdist
  | BTraceTape
      {
        pos_l = xl, yl;
        pos_r = xr, yr;
        n;
        len = _;
        tapepadding;
        otimesdist;
        oplusdist;
        max_y;
      } ->
      Printf.sprintf "\\trace{%f}{%f}{%f}{%f}{%n}{%f}{%f}{%f}{%f}" xl yl xr yr n
        max_y tapepadding otimesdist oplusdist
  | TapeDebugNode { pos = x, y; text = t } ->
      Printf.sprintf "\\node () at (%f, %f) {%s};\n" x y t)
  ^ debug_bounds tb

(** Sorts a list of blocks, tape blocks first, then circuit blocks. *)
let sort_block_list (b : block list) : block list =
  let rec aux (b : block list) : block list * block list =
    match b with
    | [] -> ([], [])
    | TB tb :: b1 ->
        let ts, bs = aux b1 in
        (TB tb :: ts, bs)
    | CB cb :: b1 ->
        let ts, bs = aux b1 in
        (ts, CB cb :: bs)
  in
  let ts, bs = aux b in
  ts @ bs

(** Converts an identity block to a connector block. *)
let id_to_connector (b : circuit_block) =
  match b with
  | BId { pos = x, y; len; _ } ->
      Connector { positions = [ (x, y); (x +. len, y) ] }
  | _ -> b

(** Converts swap blocks to connector blocks in a list. *)
let rec swaps_to_connectors (l : circuit_block list) =
  match l with
  | BSwap { pos = x, y; scaley; _ } :: rest ->
      Connector { positions = [ (x, y); (x +. 1., y +. scaley) ] }
      :: Connector { positions = [ (x, y +. scaley); (x +. 1., y) ] }
      :: swaps_to_connectors rest
  | x :: l1 -> x :: swaps_to_connectors l1
  | [] -> []

(** Converts identity blocks to connector blocks in a list. *)
let ids_to_connectors (l : circuit_block list) = List.map id_to_connector l

(** Checks if a tape block has nonzero width. *)
let is_tape_block_nonzero_width (b : tape_block) =
  match b with
  | EmptyTBlock -> false
  | BTape { width; _ } -> not (width = 0.)
  | BFreeStyleTape
      { posll = x1, _; poslu = x2, _; posrl = x3, _; posru = x4, _ } ->
      (not (x1 = x3)) || not (x2 = x4)
  | _ -> true

(** Checks if a circuit block has nonzero width. *)
let is_circuit_block_nonzero_width (b : circuit_block) =
  match b with
  | Connector { positions = (x, _) :: l } ->
      List.for_all (fun (x1, _) -> not (x -. x1 = 0.)) l
  | EmptyBlock -> false
  | _ -> true

(** Gets all connector blocks from a list of circuit blocks. *)
let get_connectors (l : circuit_block list) =
  List.filter (fun x -> match x with Connector _ -> true | _ -> false) l

(** Gets all non-connector blocks from a list of circuit blocks. *)
let get_non_connectors (l : circuit_block list) =
  List.filter (fun x -> match x with Connector _ -> false | _ -> true) l

(** Sorts the positions in a connector block by x-coordinate. *)
let orient_connector (l : circuit_block) =
  match l with
  | Connector { positions = l } ->
      Connector
        { positions = List.sort (fun (x1, _) (x2, _) -> compare x1 x2) l }
  | _ -> failwith "orient_connector applied on non-connector"

(** Concatenates two connector blocks. *)
let concat_connectors (b : circuit_block) (c : circuit_block) =
  match (b, c) with
  | Connector { positions = l1 }, Connector { positions = _ :: l2 } ->
      Connector { positions = l1 @ l2 }
  | _ ->
      failwith
        "concat_connectors expects two connectors, the second of which non \
         empty"

(** Finds a connector block in a list that matches the end of another connector.
*)
let find_matching_connector (b : circuit_block) (l : circuit_block list) =
  let l = get_connectors l in
  let matching_block =
    match b with
    | Connector { positions = pl } ->
        let x2, y2 =
          try List.hd (List.rev pl)
          with _ -> failwith "expected nonempty list"
        in
        List.find
          (function
            | Connector { positions = (x1, y1) :: _ } -> x1 = x2 && y1 = y2
            | _ ->
                failwith
                  "find_matching_connector expects nonempty connector list")
          l
    | _ -> failwith "find_matching_connector expects connector"
  in
  ( List.filter (fun x -> not (x = matching_block)) l,
    concat_connectors b matching_block )

(** Sorts connector blocks. *)
let sort_connectors = List.sort compare_circuit_block

(** Matches and merges connector blocks in a list. *)
let rec match_connectors (l : circuit_block list) =
  let rec aux (l : circuit_block list) =
    match l with
    | Connector c :: l1 ->
        let newl, newc =
          try find_matching_connector (Connector c) l1
          with Not_found -> (l1, Connector c)
        in
        newc :: aux newl
    | [] -> []
    | _ -> failwith "match_connectors expects list of connectors"
  in
  let l = sort_connectors l in
  let res = aux l in
  if List.length res = List.length l then res else match_connectors res

(** Prints a list of blocks to stdout. *)
let print_blocks (l : block list) =
  List.iter (fun x -> Printf.printf "%s\n" (show_block x)) l

(** Converts a list of blocks to their TikZ representation. *)
let rec tikz_of_block_list (b : block list) : string =
  let b = sort_block_list b in
  match b with
  | [] -> ""
  | TB tb :: b1 -> tikz_of_tape_block tb false ^ "\n" ^ tikz_of_block_list b1
  | CB cb :: b1 -> tikz_of_circuit_block cb ^ "\n" ^ tikz_of_block_list b1

(********************************************************************************************************)

(* Circuit interface utils *)

(** Flatten a circuit interface tree into a list of positions. IMPORTANT: this
    considers empty pins as regular pins, losing the information. If you wish to
    preserve it, use [flatten_circuit_preserve_empty].*)
let rec flatten_circuit (c : circuit_draw_interface) : (float * float) list =
  match c with
  | EmptyCircuit -> []
  | CircuitPin (x, y) -> [ (x, y) ]
  | CircuitTens (c1, c2) -> flatten_circuit c1 @ flatten_circuit c2
  | EmptyCircuitPin (x, y) -> [ (x, y) ]

(** Flatten a circuit interface, preserving information about which circuit pins
    are empty (the boolean flag will be set to false for those.) *)
let rec flatten_circuit_preserve_empty (c : circuit_draw_interface) :
    (float * float * bool) list =
  match c with
  | EmptyCircuit -> []
  | CircuitPin (x, y) -> [ (x, y, true) ]
  | CircuitTens (c1, c2) ->
      flatten_circuit_preserve_empty c1 @ flatten_circuit_preserve_empty c2
  | EmptyCircuitPin (x, y) -> [ (x, y, false) ]

(** Flatten non-empty components of the circuit interface *)
let rec flatten_circuit_nonempty (c : circuit_draw_interface) :
    (float * float) list =
  match c with
  | EmptyCircuit | EmptyCircuitPin _ -> []
  | CircuitPin (x, y) -> [ (x, y) ]
  | CircuitTens (c1, c2) ->
      flatten_circuit_nonempty c1 @ flatten_circuit_nonempty c2

(** Rebuild the circuit interface from a list of positions in left-associated
    form. IMPORTANT: this treats all positions as nonempty. If there were empty
    pins, use [rebuild_circuit_preserve]. *)
let rec rebuild_circuit (pins : (float * float) list) : circuit_draw_interface =
  match pins with
  | [] -> EmptyCircuit
  | (x, y) :: [] -> CircuitPin (x, y)
  | (x, y) :: rest -> CircuitTens (CircuitPin (x, y), rebuild_circuit rest)

(** Rebuilds the circuit interface from a list of positions with emptyness
    information in left-associated form.*)
let rec rebuild_circuit_preserve (pins : (float * float * bool) list) :
    circuit_draw_interface =
  match pins with
  | [] -> EmptyCircuit
  | (x, y, b) :: [] -> if b then CircuitPin (x, y) else EmptyCircuitPin (x, y)
  | (x, y, b) :: rest ->
      CircuitTens
        ( (if b then CircuitPin (x, y) else EmptyCircuitPin (x, y)),
          rebuild_circuit_preserve rest )

(** Removes empty circuit interfaces. Note that these are distinct from empty
    circuit pins, which still hold a position.*)
let rec clean_circuit_interface (c : circuit_draw_interface) =
  match c with
  | CircuitTens (EmptyCircuit, c1) -> clean_circuit_interface c1
  | CircuitTens (c1, EmptyCircuit) -> clean_circuit_interface c1
  | CircuitTens (c1, c2) ->
      CircuitTens (clean_circuit_interface c1, clean_circuit_interface c2)
  | CircuitPin _ | EmptyCircuit | EmptyCircuitPin _ -> c

let rec deep_clean_circuit_interface (c : circuit_draw_interface) =
  let cleaned = clean_circuit_interface c in
  if c = cleaned then cleaned else deep_clean_circuit_interface cleaned

(* The normalization function: flatten then rebuild *)
let circuit_interface_normalize (c : circuit_draw_interface) :
    circuit_draw_interface =
  let pins = flatten_circuit_preserve_empty c in
  rebuild_circuit_preserve pins |> deep_clean_circuit_interface

(* circuit operations: init, rev *)
let rec circuit_interface_init (n : int) (f : int -> circuit_draw_interface) =
  if n = 0 then EmptyCircuit
  else
    CircuitTens
      (circuit_interface_init (n - 1) f, circuit_interface_normalize (f n))

(** inverts a circuit interface *)
let rec circuit_interface_rev (c : circuit_draw_interface) =
  circuit_interface_normalize
    (match c with
    | EmptyCircuit -> EmptyCircuit
    | CircuitPin _ | EmptyCircuitPin _ -> c
    | CircuitTens (c1, c2) ->
        CircuitTens (circuit_interface_rev c2, circuit_interface_rev c1))

(** map for circuit interfaces *)
let rec circuit_interface_map
    (f : circuit_draw_interface -> circuit_draw_interface)
    (c : circuit_draw_interface) =
  let c = circuit_interface_normalize c in
  circuit_interface_normalize
    (match c with
    | EmptyCircuit -> c
    | CircuitPin _ -> f c
    | EmptyCircuitPin _ -> f c
    | CircuitTens (c1, c2) -> CircuitTens (f c1, circuit_interface_map f c2))

(** creates a list by mapping over the elements of a circuit interface *)
let rec circuit_interface_to_list_map (f : circuit_draw_interface -> 'a list)
    (c : circuit_draw_interface) =
  let c = circuit_interface_normalize c in
  match c with
  | EmptyCircuit | CircuitPin _ | EmptyCircuitPin _ -> f c
  | CircuitTens (c1, c2) -> f c1 @ circuit_interface_to_list_map f c2

(** returns the highest position of a circuit *)
let top_of_circuit_interface c =
  let l = flatten_circuit c in
  if List.length l = 0 then None
  else
    Some
      (let maxy = List.map (fun (_, y) -> y) l |> list_max in
       List.find (fun (_, y) -> y = maxy) l)

(** returns the hightst position of a circuit *)
let base_of_circuit_interface c =
  let l = flatten_circuit c in
  if List.length l = 0 then None
  else
    Some
      (let miny = List.map (fun (_, y) -> y) l |> list_min in
       List.find (fun (_, y) -> y = miny) l)

(*** returns lowest y value of two circuit interfaces *)
let y_base_of_circuit_interfaces c1 c2 =
  match (base_of_circuit_interface c1, base_of_circuit_interface c2) with
  | None, None -> None
  | None, Some (_, y) -> Some y
  | Some (_, y), None -> Some y
  | Some (_, y1), Some (_, y2) -> Some (min y1 y2)

(** transforms a list into a circuit interface *)
let rec circuit_interface_of_list l =
  match l with
  | [] -> EmptyCircuit
  | (x, y) :: xs -> CircuitTens (CircuitPin (x, y), circuit_interface_of_list xs)

(** returns the height of a circuit interface *)
let circuit_interface_height (c : circuit_draw_interface) =
  (* Printf.printf "getting height of intf: %s\n" (show_circuit_draw_interface c); *)
  try
    let top = top_of_circuit_interface c in
    let bot = base_of_circuit_interface c in
    (* Printf.printf "top: %f, bottom: %f\n"
      (snd (Option.get top))
      (snd (Option.get bot)); *)
    snd (Option.get top) -. snd (Option.get bot)
  with _ -> 0.
(* match circuit_interface_normalize c with
  | EmptyCircuit | CircuitPin _ -> 0.
  | CircuitTens (t1, t2) ->
      circuit_interface_height t1 +. circuit_interface_height t2 +. !otimes_dist *)

(********************************************************************************************************)
(* Tape interface utils *)

let rec is_empty_circ c =
  match c with
  | EmptyCircuit -> true
  | EmptyCircuitPin _ -> true
  | CircuitTens (c1, c2) -> is_empty_circ c1 && is_empty_circ c2
  | _ -> false

(** Cleans a tape interface by removing empty circuit interfaces. *)
let rec clean_tape_interface t =
  match t with
  | TapeInterface (p1, p2, c) when is_empty_circ c -> EmptyTape (p1, p2)
  | TapeTens (EmptyInterface _, t1) -> clean_tape_interface t1
  | TapeTens (t1, EmptyInterface _) -> clean_tape_interface t1
  | TapeTens (t1, t2) ->
      TapeTens (clean_tape_interface t1, clean_tape_interface t2)
  | _ -> t

(** this gets rid of all empty interfaces. *)
let deep_clean_interface t = clean_tape_interface t
(* let ct = clean_tape_interface t in
  if t == ct then ct else deep_clean_interface ct *)

(** given a tape interface, turns it into a list *)
let rec list_of_tape_interface t =
  match t with
  | EmptyTape _ | TapeInterface _ | EmptyInterface _ -> [ t ]
  | TapeTens (t1, t2) -> list_of_tape_interface t1 @ list_of_tape_interface t2

(** given a list of tape interfaces, returns their tensor.*)
let rec tape_interface_of_list t =
  match t with
  | t1 :: [] -> t1
  | t1 :: rest -> TapeTens (t1, tape_interface_of_list rest)
  | [] -> EmptyInterface (None, None)

(** checks if a circuit interface is empty *)
let circuit_intf_is_empty cintf =
  let flattened_noempty = flatten_circuit_nonempty cintf in
  List.length flattened_noempty = 0

(** destroys empty interfaces in a tape interface *)
let destroy_empty_interfaces t =
  let l = list_of_tape_interface t in
  List.filter (fun t -> match t with EmptyInterface _ -> false | _ -> true) l
  |> tape_interface_of_list

(** bring a tape interface to a list-like normal form *)
let tape_interface_normalize (t : tape_draw_interface) =
  t |> deep_clean_interface |> list_of_tape_interface |> tape_interface_of_list
  |> deep_clean_interface

(** NOTE: the empty interface also has information, so we want to apply the
    function to it as well.*)
let rec tape_interface_map (f : tape_draw_interface -> tape_draw_interface)
    (t : tape_draw_interface) =
  tape_interface_normalize
    (let t = tape_interface_normalize t in
     match t with
     | EmptyTape _ -> f t
     | TapeInterface _ -> f t
     | EmptyInterface _ -> f t
     | TapeTens (t1, t2) -> TapeTens (f t1, tape_interface_map f t2))

(** returns the lowest position of a tape interface *)
let rec base_of_tape_interface t =
  match tape_interface_normalize t with
  | EmptyTape ((x, y), _) -> (x, y)
  | TapeTens (_, t2) -> base_of_tape_interface t2
  | TapeInterface ((x, y), _, _) -> (x, y)
  | EmptyInterface (Some (x, y), _) -> (x, y)
  | _ -> failwith "tried to get base of empty interface"

(** Gets the highest nonempty interface from a tape interface. *)
let get_highest_nonempty_interface t =
  let l =
    list_of_tape_interface t
    |> List.filter (function EmptyInterface _ -> false | _ -> true)
  in
  match l with [] -> failwith "no highest interface" | a :: _ -> a

(** Gets the lowest nonempty interface from a tape interface. *)
let get_lowest_nonempty_interface t =
  let t = list_of_tape_interface t |> List.rev |> tape_interface_of_list in
  get_highest_nonempty_interface t

(** Checks if a tape interface is nonempty. *)
let is_interface_nonempty (ti : tape_draw_interface) =
  match ti with
  | EmptyInterface _ ->
      (* print_endline ("false: " ^ show_tape_draw_interface ti); *)
      false
  | _ ->
      (* print_endline ("true: " ^ show_tape_draw_interface ti); *)
      true

(** Removes the highest nonempty interface from a tape interface. *)
let rec chop_off_highest_nonempty_intf t =
  let l = list_of_tape_interface t in
  match l with
  | [] -> failwith "no highest interface to chop off"
  | a :: rest ->
      if is_interface_nonempty a then rest |> tape_interface_of_list
      else
        TapeTens
          (a, chop_off_highest_nonempty_intf (rest |> tape_interface_of_list))
        |> tape_interface_normalize

(** Returns the top position of a tape interface. *)
let rec top_of_tape_interface t =
  match tape_interface_normalize t with
  | EmptyTape (_, (x, y)) -> (x, y)
  | TapeTens (t1, _) -> top_of_tape_interface t1
  | TapeInterface (_, (x, y), _) -> (x, y)
  | EmptyInterface (_, Some (x, y)) -> (x, y)
  | _ -> failwith "tried to get top of empty interface"

(** Returns the base position of two tape interfaces (lowest y). *)
let base_of_tape_interfaces t1 t2 =
  let x1, y1 = base_of_tape_interface t1 in
  let x2, y2 = base_of_tape_interface t2 in
  if y1 > y2 then (x1, y1) else (x2, y2)

(** Maps a function over two tape interfaces, producing a list of blocks. *)
let rec tape_interface_to_block_map2
    (f : tape_draw_interface -> tape_draw_interface -> block list)
    (t1 : tape_draw_interface) (t2 : tape_draw_interface) : block list =
  let t1, t2 =
    ( t1 |> deep_clean_interface |> tape_interface_normalize,
      t2 |> deep_clean_interface |> tape_interface_normalize )
  in
  match (t1, t2) with
  | EmptyTape _, EmptyTape _ | TapeInterface _, TapeInterface _ -> f t1 t2
  | TapeTens (t11, t21), TapeTens (t12, t22) ->
      f t11 t12 @ tape_interface_to_block_map2 f t21 t22
  | EmptyInterface _, EmptyInterface _
  | TapeInterface (_, _, EmptyCircuit), EmptyTape _
  | EmptyTape _, TapeInterface (_, _, EmptyCircuit) ->
      [] (* TODO is this right?*)
  | _ ->
      print_endline
        ("failure: \n"
        ^ show_tape_draw_interface t1
        ^ "\n\n"
        ^ show_tape_draw_interface t2);

      failwith
        "tape_interface_to_block_map2 could not be applied, args have \
         different sizes."

(** Returns the height of a tape interface. *)
let rec tape_interface_height (t : tape_draw_interface) =
  (* used NORMALIZED t, but this would eliminate all empty interfaces. *)
  match t with
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
  | Gen (_, ar, coar, _) ->
      let ar_size = List.length ar in
      let coar_size = List.length coar in
      float_of_int (max (max (ar_size - 1) (coar_size - 1)) 0) *. !otimes_dist

let rec get_circuit_left_interface_height (c : circuit) =
  match c with
  | CId _ | CId1 | SwapTimes _ -> get_circuit_height c
  | Gen (_, ar, _, _) ->
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

let remove_empty_pins i =
  let l = flatten_circuit_nonempty i in
  circuit_interface_of_list l

let circuit_connect_interfaces (ina : circuit_draw_interface)
    (inb : circuit_draw_interface) : circuit_block list =
  let l1 = flatten_circuit_nonempty ina in
  let l2 = flatten_circuit_nonempty inb in

  match (l1, l2) with
  | [], _ | _, [] -> []
  | _ ->
      List.map2
        (fun (x1, y1) (x2, y2) ->
          Connector { positions = [ (x1, y1); (x2, y2) ] })
        l1 l2

let circuit_align_interfaces ri1 ri2 =
  let ri1, ri2 =
    (circuit_interface_normalize ri1, circuit_interface_normalize ri2)
  in

  let l1 = flatten_circuit_nonempty ri1 in
  let l2 = flatten_circuit_nonempty ri2 in

  match (l1, l2) with
  | [], [] -> (ri1, ri2)
  | (posx1, _) :: _, (posx2, _) :: _ ->
      let max_x = max posx1 posx2 in
      let f =
       fun c ->
        match c with CircuitPin (_, y) -> CircuitPin (max_x, y) | _ -> c
      in
      (circuit_interface_map f ri1, circuit_interface_map f ri2)
  | (posx1, _) :: _, [] ->
      let max_x = posx1 in
      let f =
       fun c ->
        match c with CircuitPin (_, y) -> CircuitPin (max_x, y) | _ -> c
      in
      (circuit_interface_map f ri1, circuit_interface_map f ri2)
  | [], (posx1, _) :: _ ->
      let max_x = posx1 in
      let f =
       fun c ->
        match c with CircuitPin (_, y) -> CircuitPin (max_x, y) | _ -> c
      in
      (circuit_interface_map f ri1, circuit_interface_map f ri2)

(********************************************************************************************************)
(* Drawing Tapes *)

(* transforms a tape identity into ⊕_i (Tape(⊗_j (circuit ids))) *)
let tid_to_normal_form (l : Common_defs.sort list list) =
  let to_tape (l1 : Common_defs.sort list) =
    Tape (List.fold_left (fun acc x -> Otimes (acc, CId x)) CId1 l1)
  in
  deep_clean_tape
    (List.fold_left (fun acc x -> Oplus (acc, x)) TId0 (List.map to_tape l))

let rec is_tape_identity (t : tape) =
  match t with
  | TId0 | Tape CId1 | Tape (CId _) | TId [ [] ] -> true
  | Oplus (t1, t2) -> is_tape_identity t1 && is_tape_identity t2
  | Tape (Otimes (c1, c2)) ->
      is_tape_identity (Tape c1) && is_tape_identity (Tape c2)
  | _ -> false

(* returns height of tape *)
and get_tape_height (t : tape) =
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
  | Trace (l, t) ->
      let n = l |> List.length in
      get_tape_height t
      +. (float_of_int (n - 1) *. !otimes_dist)
      +. (2. *. !tape_padding) +. !oplus_dist

let rec get_tape_left_interface_height (t : tape) =
  match t with
  | Tape c ->
      get_circuit_within_tape_left_interface_height c +. (2. *. !tape_padding)
  | TCompose (t1, _) -> get_tape_left_interface_height t1
  | Oplus (t1, t2) ->
      get_tape_left_interface_height t1
      +. get_tape_left_interface_height t2
      +. !oplus_dist
  | _ -> get_tape_height t (* TODO this is wrong! *)

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
  | Trace (l, t1) ->
      let n = l |> List.length in
      get_tape_left_interface_size t1 - n

let rec get_max_x_tape t =
  match t with
  | EmptyTape (_, (x, _)) -> x
  | TapeInterface (_, (x, _), _) -> x
  | TapeTens (t1, t2) -> max (get_max_x_tape t1) (get_max_x_tape t2)
  | EmptyInterface (_, Some (x, _)) -> x
  | EmptyInterface (None, None) -> -.infinity
  | _ -> failwith "malformed empty interface?"

let rec get_min_x_tape t =
  match t with
  | EmptyTape ((x, _), _) -> x
  | TapeInterface ((x, _), _, _) -> x
  | TapeTens (t1, t2) -> min (get_min_x_tape t1) (get_min_x_tape t2)
  | EmptyInterface (Some (x, _), _) -> x
  | EmptyInterface (None, None) -> infinity
  | _ -> failwith "malformed empty interface?"

let rec get_summand_list t =
  match t with
  | Oplus (t1, t2) -> get_summand_list t1 @ get_summand_list t2
  | _ -> [ t ]

let align_circuit_interface_to_x c x =
  circuit_interface_map
    (function
      | CircuitPin (_, y) -> CircuitPin (x, y)
      | EmptyCircuitPin (_, y) -> EmptyCircuitPin (x, y)
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

let rec flatten_tape (t : tape_draw_interface) =
  match t with
  | EmptyTape _ -> []
  | TapeInterface (_, _, c) -> flatten_circuit c
  | TapeTens (t1, t2) -> flatten_tape t1 @ flatten_tape t2
  | EmptyInterface _ -> []

let rec flatten_tape_nonempty (t : tape_draw_interface) =
  match t with
  | EmptyTape _ -> []
  | TapeInterface (_, _, c) -> flatten_circuit_nonempty c
  | TapeTens (t1, t2) -> flatten_tape_nonempty t1 @ flatten_tape_nonempty t2
  | EmptyInterface _ -> []

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
      [
        TB
          (BFreeStyleTape
             {
               posll = (_pos_bot1x, _pos_bot1y);
               poslu = (_pos_top1x, _pos_top1y);
               posrl = (_pos_bot2x, _pos_bot2y);
               posru = (_pos_top2x, _pos_top2y);
             });
      ]

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
  (* Printf.printf "====\nis:\n";
  List.iter (fun x -> print_endline (show_tape_draw_interface x)) is;
  Printf.printf "\nts:\n====";
  List.iter (fun x -> print_endline (show_tape x)) ts; *)
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
    failwith
      (Printf.sprintf
         "trying to pair incompatible interface and type (lengths %d vs %d) -- \
          there might be some undetected empty interfaces"
         (List.length is)
         (list_sum (List.map get_tape_left_interface_size ts)))

(** connects two tape interfaces *)
let tape_connect_interfaces (ina : tape_draw_interface)
    (inb : tape_draw_interface) =
  let ina = destroy_empty_interfaces ina in
  let inb = destroy_empty_interfaces inb in
  (* printf []
    "\n====================\nAlign: \n%s\n with\n %s\n====================\n"
    (show_tape_draw_interface ina)
    (show_tape_draw_interface inb); *)
  let res =
    tape_interface_to_block_map2
      (fun t1 t2 ->
        match (t1, t2) with
        | EmptyTape (_pos_bot1, _pos_top1), EmptyTape (_pos_bot2, _pos_top2) ->
            construct_tape_between _pos_bot1 _pos_top1 _pos_bot2 _pos_top2
        | ( TapeInterface (_pos_bot1, _pos_top1, c1),
            TapeInterface (_pos_bot2, _pos_top2, c2) ) ->
            construct_tape_between _pos_bot1 _pos_top1 _pos_bot2 _pos_top2
            @ List.map (fun x -> CB x) (circuit_connect_interfaces c1 c2)
        | _ ->
            Printf.printf "==========\n%s\n//\n%s\n========== "
              (show_tape_draw_interface ina)
              (show_tape_draw_interface inb);
            failwith "trying to connect incompatible tape interfaces")
      ina inb
  in

  res

let max_x_in_diags (ds : tape_geometry list) =
  ds
  |> List.map (fun (TapeGeo { right_interface; _ }) ->
         get_max_x_tape right_interface)
  |> list_max

let min_x_in_diags (ds : tape_geometry list) =
  ds
  |> List.map (fun (TapeGeo { left_interface; _ }) ->
         get_min_x_tape left_interface)
  |> list_min

let diag_adjust_height
    (TapeGeo { tikz; height; length; left_interface; right_interface }) =
  let hl = tape_interface_height left_interface in
  let hr = tape_interface_height right_interface in
  TapeGeo
    {
      tikz;
      height = max height (max hl hr);
      length;
      left_interface;
      right_interface;
    }

let stack_diagrams
    ( i,
      TapeGeo
        {
          tikz = da;
          height = ha;
          length = la;
          left_interface = lia;
          right_interface = ria;
        } ) (_, db, hb, lb, optlib, optrib) =
  match (optlib, optrib) with
  | Some lib, Some rib ->
      let ria_aligned, rib_aligned = tape_align_interfaces ria rib in
      ( i,
        da @ db,
        ha +. hb +. !oplus_dist,
        max la lb,
        Some (TapeTens (lia, lib)),
        Some (TapeTens (ria_aligned, rib_aligned)) )
  | _ ->
      (* fallback when one side is missing *)
      (i, da, ha, la, Some lia, Some ria)

(** adds debug elements in drawing for circuit interfaces. arguments are
    interface and latex color name*)
let debug_get_circuit_interface (ci : circuit_draw_interface) (color : string) :
    circuit_block list =
  circuit_interface_to_list_map
    (fun c ->
      match c with
      | EmptyCircuit -> []
      | CircuitPin (x, y) ->
          [
            CircuitDebugNode
              {
                pos = (x, y);
                text = Printf.sprintf "\\color{%s} $\\bullet$" color;
              };
          ]
      | EmptyCircuitPin (x, y) ->
          [
            CircuitDebugNode
              {
                pos = (x, y);
                text = Printf.sprintf "\\color{%s} $\\star$" color;
              };
          ]
      | _ -> failwith "should not happen I guess")
    ci

(** Adds debug elements for a tape interface. *)
let debug_get_tape_interface (ti : tape_draw_interface) (color : string) :
    block list =
  List.map
    (function
      | EmptyTape _ | EmptyInterface _ -> []
      | TapeInterface (pos1, pos2, _) ->
          [
            TB
              (TapeDebugNode
                 {
                   pos = pos1;
                   text = Printf.sprintf "\\color{%s} $\\bullet$" color;
                 });
            TB
              (TapeDebugNode
                 {
                   pos = pos2;
                   text = Printf.sprintf "\\color{%s} $\\bullet$" color;
                 });
          ]
      | _ -> failwith "should not happen")
    (list_of_tape_interface ti)
  |> List.concat

(** Moves a circuit block by a given offset. *)
let move_circuit_block (cb : circuit_block) (dx, dy) =
  let move (x, y) = (x +. dx, y +. dy) in
  match cb with
  | BMeasure b -> BMeasure { b with pos = move b.pos }
  | BId b -> BId { b with pos = move b.pos }
  | BSwap b -> BSwap { b with pos = move b.pos }
  | BCopy b -> BCopy { b with pos = move b.pos }
  | BDiscard b -> BDiscard { b with pos = move b.pos }
  | BCoCopy b -> BCoCopy { b with pos = move b.pos }
  | BCoDiscard b -> BCoDiscard { b with pos = move b.pos }
  | BGen b -> BGen { b with pos = move b.pos }
  | Connector c -> Connector { positions = List.map move c.positions }
  | EmptyBlock -> cb
  | CircuitDebugNode b -> CircuitDebugNode { b with pos = move b.pos }

(** Moves a tape block by a given offset. *)
let move_tape_block (tb : tape_block) (dx, dy) =
  let move (x, y) = (x +. dx, y +. dy) in
  match tb with
  | EmptyTBlock -> EmptyTBlock
  | BTape b -> BTape { b with pos = move b.pos }
  | BFreeStyleTape b ->
      BFreeStyleTape
        {
          posll = move b.posll;
          poslu = move b.poslu;
          posrl = move b.posrl;
          posru = move b.posru;
        }
  | BAdapter b -> BAdapter { b with pos = move b.pos }
  | BSwapTape b -> BSwapTape { b with pos = move b.pos }
  | BSplitTape b -> BSplitTape { b with pos = move b.pos }
  | BCutTape b -> BCutTape { b with pos = move b.pos }
  | BJoinTape b -> BJoinTape { b with pos = move b.pos }
  | BSpawnTape b -> BSpawnTape { b with pos = move b.pos }
  | BTraceTape b ->
      BTraceTape
        {
          b with
          pos_r = move b.pos_r;
          pos_l = move b.pos_l;
          max_y = b.max_y +. dy;
        }
  | TapeDebugNode b -> TapeDebugNode { b with pos = move b.pos }

(** Moves a block (circuit or tape) by a given offset. *)
let move_block (b : block) (dx, dy) : block =
  match b with
  | CB cb -> CB (move_circuit_block cb (dx, dy))
  | TB tb -> TB (move_tape_block tb (dx, dy))

let rec move_circuit_interface (dx, dy) = function
  | EmptyCircuit -> EmptyCircuit
  | CircuitTens (i1, i2) ->
      CircuitTens
        (move_circuit_interface (dx, dy) i1, move_circuit_interface (dx, dy) i2)
  | CircuitPin (x, y) -> CircuitPin (x +. dx, y +. dy)
  | EmptyCircuitPin (x, y) -> EmptyCircuitPin (x +. dx, y +. dy)

let rec move_tape_interface (dx, dy) = function
  | EmptyInterface (opt1, opt2) ->
      let move_opt = function
        | None -> None
        | Some (x, y) -> Some (x +. dx, y +. dy)
      in
      EmptyInterface (move_opt opt1, move_opt opt2)
  | EmptyTape ((x1, y1), (x2, y2)) ->
      EmptyTape ((x1 +. dx, y1 +. dy), (x2 +. dx, y2 +. dy))
  | TapeTens (t1, t2) ->
      TapeTens (move_tape_interface (dx, dy) t1, move_tape_interface (dx, dy) t2)
  | TapeInterface ((x1, y1), (x2, y2), ci) ->
      TapeInterface
        ( (x1 +. dx, y1 +. dy),
          (x2 +. dx, y2 +. dy),
          move_circuit_interface (dx, dy) ci )

let move_circuit_geometry (CircGeo cg) (dx, dy) : circuit_geometry =
  let move_cb_list = List.map (fun cb -> move_circuit_block cb (dx, dy)) in
  CircGeo
    {
      cg with
      tikz = move_cb_list cg.tikz;
      left_interface = move_circuit_interface (dx, dy) cg.left_interface;
      right_interface = move_circuit_interface (dx, dy) cg.right_interface;
    }

let move_tape_geometry (TapeGeo tg) (dx, dy) : tape_geometry =
  let move_block_list = List.map (fun b -> move_block b (dx, dy)) in
  TapeGeo
    {
      tg with
      tikz = move_block_list tg.tikz;
      left_interface = move_tape_interface (dx, dy) tg.left_interface;
      right_interface = move_tape_interface (dx, dy) tg.right_interface;
    }

let max_by_y pairs =
  match pairs with
  | [] -> invalid_arg "max_by_y: empty list"
  | hd :: tl ->
      List.fold_left
        (fun ((_, y_max) as best) ((_, y) as p) ->
          if y > y_max then p else best)
        hd tl

let min_by_y pairs =
  match pairs with
  | [] -> invalid_arg "min_by_y: empty list"
  | hd :: tl ->
      List.fold_left
        (fun ((_, y_min) as best) ((_, y) as p) ->
          if y < y_min then p else best)
        hd tl

let nonempty_interface_top (ti : tape_draw_interface) =
  let l = list_of_tape_interface ti in
  l
  |> List.filter is_interface_nonempty
  |> List.map top_of_tape_interface
  |> max_by_y

let nonempty_interface_base (ti : tape_draw_interface) =
  let l = list_of_tape_interface ti in
  l
  |> List.filter is_interface_nonempty
  |> List.map base_of_tape_interface
  |> min_by_y

let nonempty_interface_center (ti : tape_draw_interface) =
  let x_top, y_top = nonempty_interface_top ti in
  let _, y_bot = nonempty_interface_base ti in
  (x_top, (y_top +. y_bot) /. 2.)

let nonempty_interface_diff_centers (ti1 : tape_draw_interface)
    (ti2 : tape_draw_interface) =
  let _, y1 = nonempty_interface_center ti1 in
  let _, y2 = nonempty_interface_center ti2 in
  y1 -. y2

let is_interface_binary (ti : tape_draw_interface) =
  let l = ti |> list_of_tape_interface |> List.filter is_interface_nonempty in
  let summand_number = l |> List.length in
  summand_number = 2

let get_space_between_nonempty_summands (ti : tape_draw_interface) =
  let l = ti |> list_of_tape_interface |> List.filter is_interface_nonempty in
  if is_interface_binary ti then
    let base0 = List.nth l 0 |> nonempty_interface_base in
    let top1 = List.nth l 1 |> nonempty_interface_top in
    snd base0 -. snd top1
  else failwith "Interface has invalid number of summands"
