open Tapes
open Typecheck
open Rewrite
open ANSITerminal
open Draw_utils

(* Drawing circuits *)

(* adds debug measures to circuit drawings *)
let rec tikz_of_circuit_meas (t : circuit) (posx : float) (posy : float)
    (debug : bool) (id_zero_width : bool) : circuit_geometry =
  if not debug then tikz_of_circuit t posx posy debug id_zero_width
  else
    let (CircGeo
           {
             tikz = diag;
             height = h;
             length = l;
             left_interface = li;
             right_interface = ri;
           }) =
      tikz_of_circuit t posx posy debug id_zero_width
    in

    let base_diagl =
      match base_of_circuit_interface li with None -> posy | Some (_, y) -> y
    in

    let base_diagr =
      match base_of_circuit_interface ri with None -> posy | Some (_, y) -> y
    in

    let base_of_diagram = min base_diagl base_diagr in

    CircGeo
      {
        tikz =
          diag
          ^ Printf.sprintf "\\measuretape {%s} {%f} {%f} {%f}\n" (fresh_meas ())
              h
              (-1. -. float_of_int !meas_counter)
              base_of_diagram;
        height = h;
        length = l;
        left_interface = li;
        right_interface = ri;
      }

(* returns a string of LaTeX macros that represents the string diagram corresponding to circuit t. *)
and tikz_of_circuit (t : circuit) (posx : float) (posy : float) (debug : bool)
    (id_zero_width : bool) : circuit_geometry =
  match t with
  | CId _ ->
      let l = if id_zero_width then 0. else 1. in
      CircGeo
        {
          tikz =
            Printf.sprintf "\\id{%s}{%f}{%f}{%d}\n" (fresh_id ()) posx posy
              (if id_zero_width then 0 else 1);
          height = 0.;
          length = l;
          left_interface = CircuitPin (posx, posy);
          right_interface = CircuitPin (posx +. l, posy);
        }
  | SwapTimes _ ->
      let swapheight = !otimes_dist in
      CircGeo
        {
          tikz =
            Printf.sprintf "\\swap{%s}{%f}{%f}{%f}\n" (fresh_swap ()) posx posy
              !otimes_dist;
          height = swapheight;
          length = 1.;
          left_interface =
            CircuitTens
              (CircuitPin (posx, posy +. swapheight), CircuitPin (posx, posy));
          right_interface =
            CircuitTens
              ( CircuitPin (posx +. 1., posy +. swapheight),
                CircuitPin (posx +. 1., posy) );
        }
  | Otimes (CId1, t2) -> tikz_of_circuit_meas t2 posx posy debug id_zero_width
  | Otimes (t1, CId1) -> tikz_of_circuit_meas t1 posx posy debug id_zero_width
  | CCompose (CId _, c2) -> tikz_of_circuit c2 posx posy debug id_zero_width
  | CCompose (c2, CId _) -> tikz_of_circuit c2 posx posy debug id_zero_width
  | Otimes (t1, t2) ->
      let (CircGeo
             {
               tikz = diag2;
               height = h2;
               length = l2;
               left_interface = li2;
               right_interface = ri2;
             }) =
        tikz_of_circuit_meas t2 posx posy debug id_zero_width
      in
      let (CircGeo
             {
               tikz = diag1;
               height = h1;
               length = l1;
               left_interface = li1;
               right_interface = ri1;
             }) =
        tikz_of_circuit_meas t1 posx
          (posy +. h2 +. !otimes_dist)
          debug id_zero_width
      in

      let ri1_aligned, ri2_aligned = circuit_align_interfaces ri1 ri2 in

      (* TODO perform check to avoid redundant connections *)
      CircGeo
        {
          tikz =
            diag1 ^ diag2 ^ "% adjusting misaligned tensors:\n"
            ^ circuit_connect_interfaces ri1 ri1_aligned
            ^ circuit_connect_interfaces ri2 ri2_aligned;
          height = h1 +. h2 +. !otimes_dist;
          length = max l1 l2;
          left_interface = CircuitTens (li1, li2);
          right_interface = CircuitTens (ri1_aligned, ri2_aligned);
        }
  | CCompose (t1, t2) ->
      let (CircGeo
             {
               tikz = diag1;
               height = h1;
               length = l1;
               left_interface = li1;
               right_interface = ri1;
             }) =
        tikz_of_circuit_meas t1 posx posy debug id_zero_width
      in
      let base_diag1 =
        match base_of_circuit_interface ri1 with
        | Some (_, y) -> y
        | None -> posy
      in
      let (CircGeo
             {
               tikz = diag2;
               height = h2;
               length = l2;
               left_interface = li2;
               right_interface = ri2;
             }) =
        tikz_of_circuit_meas t2 (posx +. l1)
          (base_diag1
          +. ((circuit_interface_height ri1 /. 2.)
             -. (get_circuit_height t2 /. 2.)))
          debug id_zero_width
      in
      CircGeo
        {
          tikz =
            diag1 ^ diag2 ^ "% composing interfaces:\n"
            ^ circuit_connect_interfaces ri1 li2;
          height = max h1 h2;
          length = l1 +. l2;
          left_interface = li1;
          right_interface = ri2;
        }
  | Gen (name, ar, coar) -> (
      match name with
      | "copy" when coar = ar @ ar ->
          CircGeo
            {
              tikz =
                Printf.sprintf "\\copycirc {%s}{%f}{%f}{%f}\n" (fresh_gen ())
                  posx posy !otimes_dist;
              height = !otimes_dist;
              length = 1.;
              left_interface = CircuitPin (posx, posy +. (!otimes_dist /. 2.));
              right_interface =
                CircuitTens
                  ( CircuitPin (posx +. 1., posy +. !otimes_dist),
                    CircuitPin (posx +. 1., posy) );
            }
      | "cocopy" when ar = coar @ coar ->
          CircGeo
            {
              tikz =
                Printf.sprintf "\\cocopycirc {%s}{%f}{%f}{%f}\n" (fresh_gen ())
                  posx posy !otimes_dist;
              height = !otimes_dist;
              length = 1.;
              left_interface =
                CircuitTens
                  ( CircuitPin (posx, posy +. !otimes_dist),
                    CircuitPin (posx, posy) );
              right_interface =
                CircuitPin (posx +. 1., posy +. (!otimes_dist /. 2.));
            }
      | _ ->
          let ar_size = List.length ar in
          let coar_size = List.length coar in

          let height =
            float_of_int (max (max (ar_size - 1) (coar_size - 1)) 0)
            *. !otimes_dist
          in
          let arshift =
            if ar_size < coar_size then
              float_of_int (coar_size - ar_size) /. 2. *. !otimes_dist
            else 0.
          in
          let coarshift =
            if ar_size > coar_size then
              float_of_int (ar_size - coar_size) /. 2. *. !otimes_dist
            else 0.
          in

          CircGeo
            {
              tikz =
                Printf.sprintf "\\gen {%s}{%f}{%f}{%d}{%d}{%s}{%f}\n"
                  (fresh_gen ()) posx posy ar_size coar_size name !otimes_dist;
              height;
              length = 2.;
              left_interface =
                circuit_interface_rev
                  (circuit_interface_init ar_size (fun i ->
                       CircuitPin
                         ( posx,
                           (float_of_int (i - 1) *. !otimes_dist)
                           +. posy +. arshift )))
                |> circuit_interface_normalize;
              right_interface =
                circuit_interface_rev
                  (circuit_interface_init coar_size (fun i ->
                       CircuitPin
                         ( posx +. 2.,
                           (float_of_int (i - 1) *. !otimes_dist)
                           +. posy +. coarshift )))
                |> circuit_interface_normalize;
            })
  | CId1 ->
      CircGeo
        {
          tikz = "";
          height = 0.;
          length = 0.;
          left_interface = EmptyCircuit;
          right_interface = EmptyCircuit;
        }

(* returns a string of latex macros representing the tape diagram *)
let rec tikz_of_tape (t : tape) (posx : float) (posy : float) (debug : bool) :
    tape_geometry =
  match t with
  | TId l -> tikz_of_tape (tid_to_normal_form l) posx posy debug
  | TId0 ->
      TapeGeo
        {
          tikz = "";
          height = 0.;
          length = 0.;
          left_interface = EmptyTape ((posx, posy), (posx, posy));
          right_interface = EmptyTape ((posx, posy), (posx, posy));
        }
  | Tape c ->
      let (CircGeo
             {
               tikz = diag;
               height = h;
               length = l;
               left_interface = li;
               right_interface = ri;
             }) =
        tikz_of_circuit_meas c posx (!tape_padding +. posy) debug false
      in
      TapeGeo
        {
          tikz =
            Printf.sprintf "\\tape {%f} {%f} {%f} {%f}\n" posx posy l
              (h +. (2. *. !tape_padding))
            ^ diag;
          height = h +. (2. *. !tape_padding);
          length = l;
          left_interface =
            TapeInterface
              ((posx, posy), (posx, posy +. h +. (2. *. !tape_padding)), li);
          right_interface =
            TapeInterface
              ( (posx +. l, posy),
                (posx +. l, posy +. h +. (2. *. !tape_padding)),
                ri );
        }
  | TCompose (t1, t2) ->
      if is_tape_identity t1 then tikz_of_tape t2 posx posy debug
      else if is_tape_identity t2 then tikz_of_tape t1 posx posy debug
      else
        let offset = 0.5 in
        (* draw the left diagram *)
        let (TapeGeo
               {
                 tikz = diag1;
                 height = h1;
                 length = l1;
                 left_interface = li1;
                 right_interface = ri1;
               }) =
          tikz_of_tape t1 posx posy debug
        in

        (* bring the right tape to sum form, then get a list of the summands*)
        let t2_to_sum = deep_clean_tape (tape_to_sum t2) in
        let summand_list = get_summand_list t2_to_sum in

        (* check if the summand list is alignable, i.e. (for now) if all summands have non-void arity *)
        let alignable =
          List.for_all (fun x -> tape_arity x != []) summand_list
        in
        if alignable && !align_summands then
          let _ = printf [ blue ] "debug: aligned composition\n" in

          let rintf_list = list_of_tape_interface ri1 in
          let intf_tape_pair = pair_intfs_tapes rintf_list summand_list in

          (* assigns summand index with associated horizontal offset *)
          let offset_tbl = Hashtbl.create 10 in

          let draw_on_intf (i : int) pair =
            (* the distance between the componends is proportional to the difference between the two tape heights *)
            let offset_multiplier =
              abs_float
                (tape_interface_height ri1 -. get_tape_left_interface_height t2)
              +. 0.25
            in
            (* Adds offset to offset table - this is used to compute the length later *)
            Hashtbl.add offset_tbl i (offset *. offset_multiplier);
            (* ri1 is the right interface to which we want to align the drawing of tape t *)
            let ri1, t = pair in
            let (TapeGeo
                   {
                     tikz = d;
                     height = h;
                     length = l;
                     left_interface = li;
                     right_interface = ri;
                   }) =
              tikz_of_tape t
                (posx +. l1 +. (offset *. offset_multiplier))
                (* some interfaces don't have a base, e.g. the empty ones. TODO they should right? *)
                ((try snd (base_of_tape_interface ri1) with Failure _ -> posx)
                +. (tape_interface_height ri1 /. 2.)
                -. (get_tape_left_interface_height t /. 2.))
                debug
            in

            (tape_connect_interfaces ri1 li ^ d, h, l, li, ri)
          in

          (* draw each summand separately *)
          let diags =
            List.mapi (fun i x -> (i, draw_on_intf i x)) intf_tape_pair
          in
          let max_x_d = max_x_in_diags (List.map snd diags) in
          let diags =
            List.map
              (fun (i, (d, h, l, li, ri)) ->
                let ri_aligned = align_tape_interface_to_x ri max_x_d in
                ( i,
                  diag_adjust_height
                    (d ^ tape_connect_interfaces ri ri_aligned, h, l, li, ri) ))
              diags
          in
          match
            List.fold_right stack_diagrams diags (69, "", 0., 0., None, None)
          with
          | i, diag, h, l, Some _li, Some ri ->
              let offset = Hashtbl.find offset_tbl i in
              TapeGeo
                {
                  tikz = diag1 ^ diag;
                  height = max h1 h;
                  length = l1 +. l +. offset;
                  left_interface = li1;
                  right_interface = ri;
                }
          | _ -> failwith "There has been some mistake idk"
        else
          let _ = printf [ red ] "debug: NON aligned composition\n" in
          let offset_multiplier =
            abs_float
              (tape_interface_height ri1 -. get_tape_left_interface_height t2)
            +. 0.25
          in
          let (TapeGeo
                 {
                   tikz = diag2;
                   height = h2;
                   length = l2;
                   left_interface = li2;
                   right_interface = ri2;
                 }) =
            tikz_of_tape t2
              (posx +. l1 +. (offset *. offset_multiplier))
              ((try snd (base_of_tape_interface ri1) with Failure _ -> posx)
              +. (tape_interface_height ri1 /. 2.)
              -. (get_tape_left_interface_height t2 /. 2.))
              debug
          in

          TapeGeo
            {
              tikz = tape_connect_interfaces ri1 li2 ^ diag1 ^ diag2 ^ "\n";
              height = max h1 h2;
              length = l1 +. l2 +. offset;
              left_interface = li1;
              right_interface = ri2;
            }
  | Oplus (TId0, t2) -> tikz_of_tape t2 posx posy debug
  | Oplus (t1, TId0) -> tikz_of_tape t1 posx posy debug
  | Oplus (t1, t2) ->
      let (TapeGeo
             {
               tikz = diag2;
               height = h2;
               length = l2;
               left_interface = li2;
               right_interface = ri2;
             }) =
        tikz_of_tape t2 posx posy debug
      in
      let (TapeGeo
             {
               tikz = diag1;
               height = h1;
               length = l1;
               left_interface = li1;
               right_interface = ri1;
             }) =
        tikz_of_tape t1 posx (posy +. h2 +. !oplus_dist) debug
      in

      let ri1_aligned, ri2_aligned = tape_align_interfaces ri1 ri2 in

      TapeGeo
        {
          tikz =
            diag1 ^ diag2
            ^ tape_connect_interfaces ri1 ri1_aligned
            ^ tape_connect_interfaces ri2 ri2_aligned;
          height = h1 +. h2 +. !oplus_dist;
          length = max l1 l2;
          left_interface = TapeTens (li1, li2);
          right_interface = TapeTens (ri1_aligned, ri2_aligned);
        }
  | SwapPlus (l1, l2) ->
      let len1, len2 = (List.length l1, List.length l2) in
      (* TODO check if this works correctly *)

      let i1l =
        List.mapi
          (fun i _ ->
            ( posx,
              posy +. (3. *. !tape_padding)
              +. (float_of_int i *. !otimes_dist)
              +. !oplus_dist
              +. (float_of_int (len2 - 1) *. !otimes_dist) ))
          l1
      in
      let i2l =
        List.mapi
          (fun i _ ->
            (posx, posy +. !tape_padding +. (float_of_int i *. !otimes_dist)))
          l2
      in
      let i1r =
        List.mapi
          (fun i _ ->
            ( 2. +. posx,
              (3. *. !tape_padding) +. posy
              +. (float_of_int i *. !otimes_dist)
              +. !oplus_dist
              +. (float_of_int (len1 - 1) *. !otimes_dist) ))
          l2
      in
      let i2r =
        List.mapi
          (fun i _ ->
            ( 2. +. posx,
              posy +. !tape_padding +. (float_of_int i *. !otimes_dist) ))
          l1
      in

      let h1 =
        (float_of_int (len1 - 1) *. !otimes_dist) +. (!tape_padding *. 2.)
      in
      let h2 =
        (float_of_int (len2 - 1) *. !otimes_dist) +. (!tape_padding *. 2.)
      in

      TapeGeo
        {
          tikz =
            Printf.sprintf "\\swaptape {%f} {%f} {%d} {%d} {%f} {%f} {%f} {%f}"
              posx posy len1 len2 !oplus_dist !otimes_dist !tape_padding 2.0;
          height = h1 +. h2 +. !oplus_dist;
          length = 2.0;
          left_interface =
            TapeTens
              ( TapeInterface
                  ( (posx, posy +. h2 +. !oplus_dist),
                    (posx, posy +. h1 +. h2 +. !oplus_dist),
                    circuit_interface_of_list (List.rev i1l) ),
                TapeInterface
                  ( (posx, posy),
                    (posx, posy +. h2),
                    circuit_interface_of_list (List.rev i2l) ) )
            |> tape_interface_normalize;
          right_interface =
            TapeTens
              ( TapeInterface
                  ( (posx +. 2., posy +. h1 +. !oplus_dist),
                    (posx +. 2., posy +. h2 +. h1 +. !oplus_dist),
                    circuit_interface_of_list (List.rev i1r) ),
                TapeInterface
                  ( (posx +. 2., posy),
                    (posx +. 2., posy +. h1),
                    circuit_interface_of_list (List.rev i2r) ) )
            |> tape_interface_normalize;
        }
  | Cut l1 ->
      let n = List.length l1 in
      let h =
        (!tape_padding *. 2.) +. (float_of_int (max 0 (n - 1)) *. !otimes_dist)
      in
      let l = if n > 0 then float_of_int n *. !otimes_dist else 1. in
      let il =
        List.mapi
          (fun i _ ->
            (posx, posy +. (float_of_int i *. !otimes_dist) +. !tape_padding))
          l1
        |> List.rev
      in
      let _debug_interfaces =
        String.concat ""
          (List.map
             (fun (x, y) ->
               Printf.sprintf "\\node () at (%f, %f) {$\\bullet$};\n" x y)
             il)
      in
      TapeGeo
        {
          tikz =
            Printf.sprintf "\\cuttape {%f} {%f} {%d} {%f} {%f}" posx posy n
              !tape_padding !otimes_dist;
          height = h;
          length = l;
          left_interface =
            TapeInterface
              ((posx, posy), (posx, posy +. h), circuit_interface_of_list il);
          right_interface =
            EmptyInterface (Some (posx +. l, posy), Some (posx +. l, posy +. h));
        }
  | Spawn l1 ->
      let n = List.length l1 in
      let h =
        (!tape_padding *. 2.) +. (float_of_int (max 0 (n - 1)) *. !otimes_dist)
      in
      let l = if n > 0 then float_of_int n *. !otimes_dist else 1. in
      let il =
        List.mapi
          (fun i _ ->
            ( posx +. l,
              posy +. (float_of_int i *. !otimes_dist) +. !tape_padding ))
          l1
        |> List.rev
      in
      let _debug_interfaces =
        String.concat ""
          (List.map
             (fun (x, y) ->
               Printf.sprintf "\\node () at (%f, %f) {$\\bullet$};\n" x y)
             il)
      in
      TapeGeo
        {
          tikz =
            Printf.sprintf "\\spawntape {%f} {%f} {%d} {%f} {%f}\n" posx posy n
              !tape_padding !otimes_dist;
          height = h;
          length = l;
          left_interface =
            EmptyInterface (Some (posx, posy), Some (posx, posy +. h));
          right_interface =
            TapeInterface
              ( (posx +. l, posy),
                (posx +. l, posy +. h),
                circuit_interface_of_list il );
        }
  | Split l1 ->
      let n = List.length l1 in
      let h =
        (2. *. !tape_padding) +. (float_of_int (max 0 (n - 1)) *. !otimes_dist)
      in
      let base_left = (h /. 2.) +. (!oplus_dist /. 2.) in
      let toth =
        (!tape_padding *. 4.)
        +. (2. *. float_of_int (max 0 (n - 1)) *. !otimes_dist)
        +. !oplus_dist
      in
      let l = (toth /. 2.) +. 1. in
      let lil =
        List.mapi
          (fun i _ ->
            ( posx,
              posy
              +. (float_of_int i *. !otimes_dist)
              +. !tape_padding +. base_left ))
          l1
        |> List.rev
      in
      let ril1 =
        List.mapi
          (fun i _ ->
            ( posx +. l,
              posy +. (float_of_int i *. !otimes_dist) +. !tape_padding ))
          l1
        |> List.rev
      in
      let ril2 =
        List.mapi
          (fun i _ ->
            ( posx +. l,
              posy
              +. (float_of_int i *. !otimes_dist)
              +. !tape_padding +. h +. !oplus_dist ))
          l1
        |> List.rev
      in
      TapeGeo
        {
          tikz =
            Printf.sprintf "\\splittape {%f} {%f} {%d} {%f} {%f} {%f}" posx posy
              n !tape_padding !otimes_dist !oplus_dist;
          height = toth;
          length = l;
          left_interface =
            TapeInterface
              ( (posx, posy +. base_left),
                (posx, posy +. h +. base_left),
                circuit_interface_of_list lil );
          right_interface =
            TapeTens
              ( TapeInterface
                  ( (posx +. l, posy +. h +. !oplus_dist),
                    (posx +. l, posy +. (2. *. h) +. !oplus_dist),
                    circuit_interface_of_list ril2 ),
                TapeInterface
                  ( (posx +. l, posy),
                    (posx +. l, posy +. h),
                    circuit_interface_of_list ril1 ) );
        }
  | Join l1 ->
      let n = List.length l1 in
      let h =
        (2. *. !tape_padding) +. (float_of_int (max 0 (n - 1)) *. !otimes_dist)
      in
      let base_left = (h /. 2.) +. (!oplus_dist /. 2.) in
      let toth =
        (!tape_padding *. 4.)
        +. (2. *. float_of_int (max 0 (n - 1)) *. !otimes_dist)
        +. !oplus_dist
      in
      let l = (toth /. 2.) +. 1. in
      let lil =
        List.mapi
          (fun i _ ->
            ( posx +. l,
              posy
              +. (float_of_int i *. !otimes_dist)
              +. !tape_padding +. base_left ))
          l1
        |> List.rev
      in
      let ril1 =
        List.mapi
          (fun i _ ->
            (posx, posy +. (float_of_int i *. !otimes_dist) +. !tape_padding))
          l1
        |> List.rev
      in
      let ril2 =
        List.mapi
          (fun i _ ->
            ( posx,
              posy
              +. (float_of_int i *. !otimes_dist)
              +. !tape_padding +. h +. !oplus_dist ))
          l1
        |> List.rev
      in
      TapeGeo
        {
          tikz =
            Printf.sprintf "\\jointape {%f} {%f} {%d} {%f} {%f} {%f}" posx posy
              n !tape_padding !otimes_dist !oplus_dist;
          height = toth;
          length = l;
          left_interface =
            TapeTens
              ( TapeInterface
                  ( (posx, posy +. h +. !oplus_dist),
                    (posx, posy +. (2. *. h) +. !oplus_dist),
                    circuit_interface_of_list ril2 ),
                TapeInterface
                  ( (posx, posy),
                    (posx, posy +. h),
                    circuit_interface_of_list ril1 ) );
          right_interface =
            TapeInterface
              ( (posx +. l, posy +. base_left),
                (posx +. l, posy +. h +. base_left),
                circuit_interface_of_list lil );
        }

(********************************************************************************************************)

(* loses tape information, gets only circuit pins *)
let rec flatten_tape (t : tape_draw_interface) =
  match t with
  | EmptyTape _ -> []
  | TapeInterface (_, _, c) -> flatten_circuit c
  | TapeTens (t1, t2) -> flatten_tape t1 @ flatten_tape t2
  | EmptyInterface _ -> []

let label_tape (ri : tape_draw_interface) (li : tape_draw_interface)
    (ar : string list list) (coar : string list list) =
  let ri_flattened = List.map (fun (x, y) -> (x -. 0.5, y)) (flatten_tape ri) in
  let li_flattened = List.map (fun (x, y) -> (x +. 0.5, y)) (flatten_tape li) in
  let ar_flattened = List.flatten ar in
  let coar_flattened = List.flatten coar in

  Printf.printf "%d, %d, %d, %d\n" (List.length ri_flattened)
    (List.length li_flattened) (List.length ar_flattened)
    (List.length coar_flattened);

  if
    List.length ri_flattened != List.length ar_flattened
    || List.length li_flattened != List.length coar_flattened
  then failwith "Interfaces don't match with arity and coarity of term"
  else
    let f ar_or_coar i (x, y) =
      Printf.sprintf "\\node () at (%f, %f) {%s};\n" x y (List.nth ar_or_coar i)
    in

    String.concat ""
      (List.mapi (f ar_flattened) ri_flattened
      @ List.mapi (f coar_flattened) li_flattened)

let draw_circuit (ast : circuit) (path : string) =
  match tikz_of_circuit (circuit_to_product ast) 0. 0. false false with
  | CircGeo { tikz = s; _ } -> (
      (* Write message to file *)
      try
        let oc = open_out path in
        (* create or truncate file, return channel *)
        Printf.fprintf oc "%s\n" s;
        (* write something *)
        close_out oc
      with Sys_error e -> eprintf [ red ] "System error: \"%s\"\n" e)

let draw_tape (ast : tape) (path : string) =
  match tikz_of_tape (tape_to_sum ast) 0. 0. false with
  | TapeGeo { tikz = s; left_interface = li; right_interface = ri; _ } -> (
      (* Write message to file *)
      try
        let oc = open_out path in
        (* create or truncate file, return channel *)
        Printf.fprintf oc "%s\n%s\n" s
          (label_tape li ri (tape_arity ast) (tape_coarity ast));
        Printf.printf "Drawing saved at path: \t'%s'\n"
          (sprintf [ green ] "%s" path);
        (* write something *)
        close_out oc
      with Sys_error e -> eprintf [ red; Bold ] "System error: \"%s\"\n" e)
