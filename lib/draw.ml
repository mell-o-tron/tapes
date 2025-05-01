open Tapes
open Typecheck
open Rewrite

type circuit_draw_interface = 
  | EmptyCircuit 
  | CircuitTens of circuit_draw_interface * circuit_draw_interface 
  | CircuitPin of float * float
  [@@deriving show]
  
type tape_draw_interface = 
  | EmptyTape of (float * float) * (float * float) 
  | TapeTens of tape_draw_interface * tape_draw_interface 
  | TapeInterface of (float * float) * (float * float) * circuit_draw_interface
  [@@deriving show]

(********************************************************************************************************)

let oplus_dist = 0.5
let otimes_dist = 1.
let tape_padding = 0.5

(* Circuit interface utils *)
  
(* Flatten the circuit tree into a list of CircuitPin nodes *)
let rec flatten_circuit (c : circuit_draw_interface) : (float * float) list =
  match c with
  | EmptyCircuit -> []
  | CircuitPin (x, y) -> [(x, y)]
  | CircuitTens (c1, c2) ->
      flatten_circuit c1 @ flatten_circuit c2

(* Rebuild the circuit tree from a list of pins in left-associated form *)
let rec rebuild_circuit (pins : (float * float) list) : circuit_draw_interface =
  match pins with
  | [] -> EmptyCircuit
  | (x, y) :: [] -> CircuitPin (x, y)
  | (x, y) :: rest -> CircuitTens (CircuitPin(x, y), rebuild_circuit rest)

(* The normalization function: flatten then rebuild *)
let circuit_interface_normalize (c : circuit_draw_interface) : circuit_draw_interface =
  print_endline ("NORMALIZING: ===============\n" ^ show_circuit_draw_interface(c) ^ "\n============================");
  let pins = flatten_circuit c in
  print_endline ("NORMALIZED: ================\n" ^ show_circuit_draw_interface(rebuild_circuit pins) ^ "\n============================"); rebuild_circuit pins
  
(* circuit operations: init, rev *)
let rec circuit_interface_init (n : int) (f : int -> circuit_draw_interface) =
  if n = 0 then EmptyCircuit else CircuitTens(circuit_interface_init (n-1) f, circuit_interface_normalize(f(n)))
let rec circuit_interface_rev (c : circuit_draw_interface) = circuit_interface_normalize(match c with
  | EmptyCircuit -> EmptyCircuit
  | CircuitPin _ -> c
  | CircuitTens (c1, c2) -> CircuitTens(circuit_interface_rev c2, circuit_interface_rev c1))

(* map for circuits *)
let rec circuit_interface_map (f : circuit_draw_interface -> circuit_draw_interface) (c : circuit_draw_interface) = 
  print_endline ("MAP: =====================\n" ^ show_circuit_draw_interface(c) ^ "\n============================");
  let c = circuit_interface_normalize c in
  circuit_interface_normalize(match (c) with
  | EmptyCircuit -> c
  | CircuitPin _ -> f(c)
  | CircuitTens (c1, c2) -> CircuitTens(f(c1),  (circuit_interface_map f c2)))
  
(* creates a list by mapping over the elements of a circuit interface *)
let rec circuit_interface_to_list_map (f : circuit_draw_interface -> 'a list) (c : circuit_draw_interface) = 
  let c = circuit_interface_normalize c in match (c) with
  | EmptyCircuit | CircuitPin _ -> f(c)
  | CircuitTens (c1, c2) -> f(c1) @ (circuit_interface_to_list_map f c2)
  
(* returns the lowest position of a circuit *)
let top_of_circuit_interface c = match circuit_interface_rev c with
  | EmptyCircuit -> None
  | CircuitPin (x, y) | CircuitTens(CircuitPin (x, y), _) -> Some (x, y)
  | _ -> failwith "malformed circuit interface"

(* returns the hightst position of a circuit *)
let rec base_of_circuit_interface c = match circuit_interface_normalize c with
  | EmptyCircuit -> None
  | CircuitTens(_, c2) -> base_of_circuit_interface c2
  | CircuitPin (x, y) -> Some (x, y)

(* returns lowest y value of two circuit interfaces *)
let y_base_of_circuit_interfaces c1 c2 = match base_of_circuit_interface c1, base_of_circuit_interface c2 with
  | None, None -> None
  | None, Some (_, y) -> Some y
  | Some (_, y), None -> Some y
  | Some (_, y1), Some (_, y2) -> Some(min y1 y2)

(* transforms a list into a circuit interface *)
let rec circuit_interface_of_list l = match l with
  | [] -> EmptyCircuit
  | (x, y)::xs -> CircuitTens(CircuitPin(x, y), circuit_interface_of_list xs)
  
(********************************************************************************************************)
(* Tape interface utils *)

let rec list_of_tape t = match t with
  | EmptyTape _ | TapeInterface _ -> [t]
  | TapeTens (t1, t2) ->  list_of_tape t1 @ list_of_tape t2

let rec tape_of_list t = match t with
  | t1 :: [] -> t1
  | t1 :: rest -> TapeTens (t1, tape_of_list rest)
  | [] -> failwith "an empty list is not a valid tape"

(* bring a tape interface to a list-like normal form *)
let tape_interface_normalize (t : tape_draw_interface) = t |> list_of_tape |> tape_of_list

(* NOTE: the empty interface also has information, so we want to apply the function to it as well.*)
let rec tape_interface_map (f : tape_draw_interface -> tape_draw_interface) (t : tape_draw_interface) = tape_interface_normalize(match tape_interface_normalize t with
  | EmptyTape _ -> f(t)
  | TapeInterface _ -> f(t)
  | TapeTens (t1, t2) -> TapeTens(f(t1),  (tape_interface_map f t2)))
  
(* returns the lowest position of a tape interface *)
let rec base_of_tape_interface t = match tape_interface_normalize t with
  | EmptyTape ((x, y), _) -> (x, y)
  | TapeTens(_, t2) -> base_of_tape_interface t2
  | TapeInterface ((x, y), _, _) ->  (x, y)

(* returns the lowest position of a pair of tape interfaces *)
let base_of_tape_interfaces t1 t2 =
  let x1, y1 = base_of_tape_interface t1 in
  let x2, y2 = base_of_tape_interface t2 in
  if y1 > y2 then (x1, y1) else x2, y2


let rec tape_interface_to_string_map2 (f : tape_draw_interface -> tape_draw_interface -> string) (t1 : tape_draw_interface) (t2 : tape_draw_interface) =

print_endline ("MAP2: \n" ^ show_tape_draw_interface t1 ^ "\n\n" ^ show_tape_draw_interface t2) ; 

(match tape_interface_normalize t1, tape_interface_normalize t2 with
  | (EmptyTape _), (EmptyTape _)  | (TapeInterface _), (TapeInterface _) -> (f t1 t2)
  | TapeTens (t11, t21), TapeTens (t12, t22) -> (f t11 t12) ^ (tape_interface_to_string_map2 f t21 t22)
  | _ -> print_endline ("failure: \n" ^ show_tape_draw_interface t1 ^ "\n\n" ^ show_tape_draw_interface t2);
  
  failwith "tape_interface_to_string_map2 could not be applied, args have different sizes."
)

let rec tape_interface_height (t : tape_draw_interface) = match tape_interface_normalize t with
  | EmptyTape ((_, y1), (_, y2)) -> abs_float(y1 -. y2)
  | TapeTens(t1, t2) -> tape_interface_height t1 +. tape_interface_height t2 +. oplus_dist
  | TapeInterface ((_, y1), (_, y2), _) ->  abs_float(y1 -. y2)
    
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

let rec get_circuit_height (c : circuit) = match c with
  | CId _ | CId1 -> 0.0
  | SwapTimes (_) -> 1.0
  | Otimes (t1, t2) -> get_circuit_height t1 +. get_circuit_height t2 +. otimes_dist
  | CCompose (t1, t2)  -> max (get_circuit_height t1) (get_circuit_height t2) (* probably wrong *)
  | Gen (_, ar, coar) -> let ar_size =  (List.length ar) in let coar_size =  (List.length coar) in (float_of_int)(max (max (ar_size - 1) (coar_size - 1)) (0)) *. otimes_dist


let rec circuit_connect_interfaces (ina: circuit_draw_interface) (inb: circuit_draw_interface) =  match (circuit_interface_normalize ina, circuit_interface_normalize inb) with
  | EmptyCircuit, EmptyCircuit  -> ""
  | CircuitPin (x1, y1), CircuitPin (x2, y2) -> (Printf.sprintf "\\draw [in=180, out=0] (%f , %f) to (%f , %f);\n" x1 y1 x2 y2)
  | CircuitTens (CircuitPin (x1, y1), ina1), CircuitTens (CircuitPin (x2, y2), inb1) 
    -> (Printf.sprintf "\\draw [in=180, out=0] (%f , %f) to (%f , %f);\n" x1 y1 x2 y2) ^ (circuit_connect_interfaces ina1 inb1)
  | _ -> failwith "trying to connect incompatible interfaces"
  
  
let circuit_align_interfaces ri1 ri2 = 
  let ri1, ri2 = circuit_interface_normalize(ri1), circuit_interface_normalize(ri2) in
  match ri1, ri2 with
  | EmptyCircuit, EmptyCircuit -> ri1, ri2
  | CircuitTens(CircuitPin (posx1, _posy1), _), CircuitTens(CircuitPin (posx2, _posy2), _)
  | CircuitPin (posx1, _posy1), CircuitPin (posx2, _posy2) ->
      let max_x = max posx1 posx2 in 
      let f = fun c -> (match c with CircuitPin (_, y) -> CircuitPin(max_x, y) | _ -> c) 
      in (circuit_interface_map f ri1, circuit_interface_map f ri2)
  
  | _ -> failwith "malformed circuit interface"
  
let rec tikz_of_circuit_meas (t:circuit) (posx:float) (posy:float) (debug : bool)= 
  
  if not debug then
    tikz_of_circuit t posx posy debug 
  else let diag, h, l, li, ri = tikz_of_circuit t posx posy debug in 
  
  let base_diagl = match base_of_circuit_interface li with
    | None -> posy
    | Some (_, y) -> y
  
  in
  
  let base_diagr = match base_of_circuit_interface ri with
    | None -> posy
    | Some (_, y) -> y
  
  in
  
  let base_of_diagram = min base_diagl base_diagr in
  
  (   diag ^ (Printf.sprintf "\\measuretape {%s} {%f} {%f} {%f}\n" (fresh_meas ()) h (-. 1. -. float_of_int(!meas_counter)) base_of_diagram ),
      h,
      l,
      li,
      ri
  )

  
(* returns a string of LaTeX macros that represents the string diagram corresponding to circuit t. *)
and tikz_of_circuit (t:circuit) (posx:float) (posy:float) (debug:bool) = match t with
  | CId   (_)          -> ( Printf.sprintf "\\id{%s}{%f}{%f}\n" (fresh_id()) posx posy, 
                            0.,    (* height *)
                            1.,    (* length *)
                            CircuitPin(posx, posy),         (* left interface *)
                            CircuitPin(posx +. 1., posy)    (* right interface *)
                          )
                          
  | SwapTimes (_)     -> let swapheight = otimes_dist in ( Printf.sprintf "\\swap{%s}{%f}{%f}{%f}\n" (fresh_swap()) posx posy 0.5,
                            swapheight, 
                            1.,
                            CircuitTens(CircuitPin(posx, posy +. swapheight), CircuitPin(posx, posy)),
                            CircuitTens(CircuitPin(posx +. 1., posy +. swapheight), CircuitPin(posx +. 1., posy))
                          )
  
  | Otimes (CId1, t2) -> tikz_of_circuit_meas t2 posx posy debug
  | Otimes (t1, CId1) -> tikz_of_circuit_meas t1 posx posy debug
  
  | Otimes (t1, t2)   ->  let diag2, h2, l2, li2, ri2 = tikz_of_circuit_meas t2 posx posy debug in
                          let diag1, h1, l1, li1, ri1 = tikz_of_circuit_meas t1 posx (posy +. h2 +. otimes_dist) debug in
                          
                          let ri1_aligned, ri2_aligned = circuit_align_interfaces ri1 ri2 in 
                          
                          (* TODO perform check to avoid redundant connections *)
                          ( diag1 ^ diag2 ^ "% adjusting misaligned tensors:\n" ^ (circuit_connect_interfaces ri1 ri1_aligned) ^ (circuit_connect_interfaces ri2 ri2_aligned), 
                            h1 +. h2 +. otimes_dist,
                            max l1 l2,
                            CircuitTens(li1, li2),
                            CircuitTens(ri1_aligned, ri2_aligned)
                          ) 
                          
  | CCompose (t1, t2)  -> let diag1, h1, l1, li1, ri1 = tikz_of_circuit_meas t1 posx posy debug in 
                          let base_diag1 = (match base_of_circuit_interface ri1 with Some(_, y) -> y | None -> posy) in
                          let diag2, h2, l2, li2, ri2 = tikz_of_circuit_meas t2 (posx +. l1) (base_diag1) debug in 
                          
                          ( diag1 ^ diag2 ^ "% composing interfaces:\n" ^ (circuit_connect_interfaces ri1 li2), 
                            max h1 h2 +. posy -. base_diag1, 
                            l1 +. l2, 
                            li1, ri2
                          )
  
  | Gen (name, ar, coar) -> let ar_size =  (List.length ar) in let coar_size =  (List.length coar) in
  
                            let height = (float_of_int)(max (max (ar_size - 1) (coar_size - 1)) (0)) *. otimes_dist in
                            let arshift = if ar_size < coar_size then float_of_int(coar_size - ar_size) /. 2. *. otimes_dist else 0. in
                            let coarshift = if ar_size > coar_size then float_of_int(ar_size - coar_size) /. 2. *. otimes_dist else 0. in
                            
                            ( Printf.sprintf "\\gen {%s}{%f}{%f}{%d}{%d}{%s}{%f}\n" (fresh_gen()) posx (posy) (ar_size - 1) (coar_size - 1) name otimes_dist,
                              height,
                              2.,
                              circuit_interface_rev (circuit_interface_init (ar_size)   (fun i -> CircuitPin(posx, (float_of_int (i - 1) *. otimes_dist) +. posy +. arshift))) |> circuit_interface_normalize,
                              circuit_interface_rev (circuit_interface_init (coar_size) (fun i -> CircuitPin(posx +. 2., (float_of_int (i - 1) *. otimes_dist) +. posy +. coarshift))) |> circuit_interface_normalize
                            )
                            
  | CId1                 ->  ("", 0., 0., EmptyCircuit, EmptyCircuit) 

  
(********************************************************************************************************)
(* Drawing Tapes *)

(* returns height of tape *)
let rec get_tape_height (t : tape) = match t with
  | TId _ -> tape_padding *. 2.
  | TId0 -> tape_padding *. 2.
  | Tape c -> get_circuit_height c +. 2. *. tape_padding
  | TCompose (t1, t2) -> max (get_tape_height t1) (get_tape_height t2)
  | Oplus (t1, t2) -> (get_tape_height t1) +. (get_tape_height t2) +. oplus_dist

  | SwapPlus (l1, l2) -> let len1, len2 = (List.length l1), (List.length l2) in (* TODO check if this works correctly *)
    let h1 =  float_of_int(len1) *. otimes_dist +. tape_padding in
    let h2 =  float_of_int(len2) *. otimes_dist +. tape_padding in
    h1 +. h2 +. oplus_dist
  | Ldistr (_, _, _) -> failwith "not yet implemented"
  | Discard _ -> failwith "not yet implemented"
  | Copy _ -> failwith "not yet implemented"
  | CoDiscard _ -> failwith "not yet implemented"
  | CoCopy _ ->  failwith "not yet implemented"

(* transforms a tape identity into ⊕_i (Tape(⊗_j (circuit ids))) *)
let tid_to_normal_form (l : Ast.sort list list) = 
  let to_tape (l1 : Ast.sort list) = Tape (List.fold_left (fun acc x -> Otimes(acc, CId(x))) CId1 l1 ) in
  List.fold_left (fun acc x -> Oplus(acc, x)) TId0 (List.map to_tape l)
  
let rec get_max_x_tape t = match t with 
  | EmptyTape (_, (x, _)) -> x
  | TapeInterface (_, (x, _), _) -> x
  | TapeTens (t1, t2) -> max (get_max_x_tape t1) (get_max_x_tape t2)
  

(* same as circuit align interfaces but for tapes. *)
let tape_align_interfaces ri1 ri2 = 
    let max_1 = get_max_x_tape ri1 in
    let max_2 = get_max_x_tape ri2 in
    let max_x = max max_1 max_2 in
    let fc c = match c with
      | CircuitPin (_, y) -> CircuitPin(max_x, y)
      | _ -> print_endline (show_circuit_draw_interface c); failwith "tried to apply circuit map function to non-circuit pin" in
    let f t = match t with 
        | EmptyTape ((_, y1), (_, y2)) ->  EmptyTape ((max_x, y1), (max_x, y2))
        | TapeInterface ((_, y1), (_, y2), c) -> TapeInterface ((max_x, y1), (max_x, y2), circuit_interface_map fc c)
        | _ -> failwith "tried to apply tape map function to tensor" in
    tape_interface_map f ri1, tape_interface_map f ri2

    
(*   | _ -> print_endline ("failure: \n" ^ (show_tape_draw_interface ri1) ^ "\n" ^ (show_tape_draw_interface ri2)) ; failwith ("invalid tape interfaces") *)

(* debug util that draws colored circles on the interfaces *)
let rec tape_highlight_interfaces (col : string) (ina: (float*float)list list) = (match (ina) with
  | []  -> ""
  | l1 :: ina1  -> 
    String.concat "" (List.map(fun x -> Printf.sprintf "\\node [circle, fill=%s, draw] () at (%f, %f) {};\n" col (fst x) (snd x)) l1) ^ tape_highlight_interfaces (col) ina1)

  
let construct_tape_between _pos_bot1 _pos_top1 _pos_bot2 _pos_top2 = match (_pos_bot1, _pos_top1, _pos_bot2, _pos_top2) with
    (_pos_bot1x, _pos_bot1y), (_pos_top1x, _pos_top1y), (_pos_bot2x, _pos_bot2y), (_pos_top2x, _pos_top2y)
      ->  Printf.sprintf "\\freestyletape {%f} {%f} {%f} {%f} {%f} {%f} {%f} {%f}" _pos_bot1x _pos_bot1y _pos_top1x _pos_top1y _pos_bot2x _pos_bot2y _pos_top2x _pos_top2y
  
(* connects two tape interfaces *)
let tape_connect_interfaces (ina: tape_draw_interface) (inb: tape_draw_interface) = 
  print_endline ("===============\n" ^ show_tape_draw_interface ina) ; print_endline ("-------------\n" ^ show_tape_draw_interface inb ^ "\n====================\n") ;
  tape_interface_to_string_map2 (fun t1 t2 -> match t1, t2 with
    | TapeInterface (_pos_bot1, _pos_top1, c1), TapeInterface (_pos_bot2, _pos_top2, c2) -> 
      (construct_tape_between _pos_bot1 _pos_top1 _pos_bot2 _pos_top2) ^ circuit_connect_interfaces c1 c2
    | _ -> failwith "trying to connect incompatible interfaces"
  ) ina inb
   
(* returns a string of latex macros representing the tape diagram *)
let rec tikz_of_tape (t:tape)(posx:float) (posy:float) (debug : bool) = match t with

  | TId l -> tikz_of_tape (tid_to_normal_form l)(posx) (posy) debug 
  | TId0 -> ("", 0., 0., EmptyTape ((posx, posy), (posx, posy)), EmptyTape ((posx, posy), (posx, posy)))
  | Tape c ->
    let diag, h, l, li, ri  = tikz_of_circuit_meas c (posx ) (tape_padding +. posy ) debug in
              ((Printf.sprintf "\\tape {%f} {%f} {%f} {%f}\n" (posx ) posy (l) (h +. 2. *. tape_padding)) ^ diag,
                h +. 2. *. tape_padding,
                l,
                TapeInterface ((posx , posy), (posx , posy +. h +. 2. *. tape_padding), li),
                TapeInterface ((posx +. l , posy), (posx +. l , posy +. h +. 2. *. tape_padding), ri)
              )
              
  | TCompose (t1, t2) -> 
    let offset = 1. in
    let diag1, h1, l1, li1, ri1 = tikz_of_tape t1 posx posy debug in
    let diag2, h2, l2, li2, ri2 = tikz_of_tape t2 (posx +. l1 +. offset) (snd(base_of_tape_interface ri1) +. ((tape_interface_height ri1) /.2.) -. (get_tape_height t2)/.2.) debug in
    ( tape_connect_interfaces ri1 li2 ^ diag1 ^ diag2,
      max h1 h2,
      l1 +. l2 +. offset,
      li1,
      ri2
    )

  | Oplus (TId0, t2) -> tikz_of_tape(t2)(posx) (posy) (debug)
  | Oplus (t1, TId0) -> tikz_of_tape(t1)(posx) (posy) (debug)
  
  | Oplus (t1, t2) -> 
    let diag2, h2, l2, li2, ri2 = tikz_of_tape t2 posx posy debug in
    let diag1, h1, l1, li1, ri1 = tikz_of_tape t1 posx (posy +. h2 +. oplus_dist) debug in
    
    let ri1_aligned, ri2_aligned = tape_align_interfaces ri1 ri2 in 
    
    ( diag1 ^ diag2 ^ tape_connect_interfaces ri1 ri1_aligned ^ tape_connect_interfaces ri2 ri2_aligned,
      h1 +. h2 +. oplus_dist,
      max l1 l2,
      TapeTens(li1, li2),
      TapeTens(ri1_aligned, ri2_aligned)
    )
  
  | SwapPlus (l1, l2) -> let len1, len2 = (List.length l1), (List.length l2) in (* TODO check if this works correctly *)
  
    let i1l = (List.mapi (fun i _ -> (posx, posy +. 2. *. tape_padding +. (float_of_int i) *. otimes_dist +. oplus_dist +. (float_of_int len2) *. otimes_dist)) l1) in
    let i2l = (List.mapi (fun i _ -> (posx, posy +. tape_padding +. (float_of_int i) *. otimes_dist)) l2) in
    let i1r = (List.mapi (fun i _ -> (2. +. posx, 2. *. tape_padding +. posy +. (float_of_int i) *. otimes_dist +. oplus_dist +. (float_of_int len1) *. otimes_dist)) l2) in
    let i2r = (List.mapi (fun i _ -> (2. +. posx, posy +. tape_padding +. (float_of_int i) *. otimes_dist)) l1) in
    
    let h1 =  float_of_int(len1) *. otimes_dist +. tape_padding in
    let h2 =  float_of_int(len2) *. otimes_dist +. tape_padding in

    (
      Printf.sprintf "\\swaptape {%f} {%f} {%d} {%d} {%f} {%f} {%f} {%f}" posx posy len1 len2 oplus_dist otimes_dist tape_padding 2.0,
      h1 +. h2 +. oplus_dist,
      2.0,
      TapeTens( TapeInterface ((posx, posy +. h2  +. oplus_dist), (posx, posy +. h1 +. h2 +. oplus_dist), circuit_interface_of_list (List.rev i1l)),
                TapeInterface ((posx, posy), (posx, posy +. h2), circuit_interface_of_list (List.rev i2l))) |> tape_interface_normalize,

      TapeTens( TapeInterface ((posx +. 2., posy +. h2 +. oplus_dist), (posx +. 2., posy +. h2 +. h1 +. oplus_dist), circuit_interface_of_list (List.rev i1r)),
                TapeInterface ((posx +. 2., posy), (posx +. 2., posy +. h2), circuit_interface_of_list (List.rev i2r))) |> tape_interface_normalize
    )
    
  | Ldistr (_, _, _) -> failwith "not yet implemented"
  | Discard _ -> failwith "not yet implemented"
  | Copy _ -> failwith "not yet implemented"
  | CoDiscard _ -> failwith "not yet implemented"
  | CoCopy _ ->  failwith "not yet implemented"
  
  
(********************************************************************************************************)

let rec flatten_tape (t : tape_draw_interface) = match t with
    | EmptyTape _ -> []
    | TapeInterface (_, _, c) -> flatten_circuit c
    | TapeTens (t1, t2) -> flatten_tape t1 @ flatten_tape t2 

let label_tape (ri : tape_draw_interface) (li : tape_draw_interface) (ar : string list list) (coar : string list list) = 
  let ri_flattened = List.map (fun (x, y) -> (x -. 0.5, y)) (flatten_tape ri) in print_endline (String.concat "\t" (List.map (fun (x, y) -> Printf.sprintf "(%f, %f)" x y) ri_flattened)) ;
  let li_flattened = List.map (fun (x, y) -> (x +. 0.5, y)) (flatten_tape li) in print_endline (String.concat "\t" (List.map (fun (x, y) -> Printf.sprintf "(%f, %f)" x y) li_flattened)) ;
  let ar_flattened = List.flatten ar in print_endline (String.concat "\t" ar_flattened);
  let coar_flattened = List.flatten coar in print_endline (String.concat "\t" coar_flattened) ;
  
  Printf.printf "%d, %d, %d, %d\n" (List.length ri_flattened) (List.length li_flattened) (List.length ar_flattened) (List.length coar_flattened);

  if ((List.length ri_flattened) != (List.length ar_flattened) || (List.length li_flattened) != (List.length coar_flattened)) then 
        failwith "Interfaces don't match with arity and coarity of term"
else
  let f ar_or_coar i (x, y) =  Printf.sprintf "\\node () at (%f, %f) {%s};\n" x y (List.nth ar_or_coar i) in

  String.concat "" (List.mapi (f ar_flattened) ri_flattened @ List.mapi (f coar_flattened) li_flattened)

let draw_circuit (ast:circuit) =
    match (tikz_of_circuit (circuit_to_product ast) 0. 0. false) with
      | s, _, _, _, _ -> (* Write message to file *)
                    let oc = open_out "figure.txt" in
                    (* create or truncate file, return channel *)
                    Printf.fprintf oc "%s\n" s;
                    (* write something *)
                    close_out oc;;
                    
let draw_tape (ast:tape) = (print_endline ("AST:\n"^(pp_tape(ast)))) ; (print_endline ("AST TO SUM\n"^(pp_tape(tape_to_sum ast)))) ;
    match (tikz_of_tape (tape_to_sum ast) 0. 0. true) with
      | s, _, _, li, ri -> (* Write message to file *)
                    let oc = open_out "figure.txt" in
                    (* create or truncate file, return channel *)
                    Printf.fprintf oc "%s\n%s\n" s (label_tape li ri (tape_arity ast) (tape_coarity ast));
                    (* write something *)
                    close_out oc;;
