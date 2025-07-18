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
          @ [
              BMeasure
                {
                  fresh_name = fresh_meas ();
                  len = h;
                  pos = (-1. -. float_of_int !meas_counter, base_of_diagram);
                };
            ];
        height = h;
        length = l;
        left_interface = li;
        right_interface = ri;
      }

(* returns a string of LaTeX macros that represents the string diagram corresponding to circuit t. *)
and tikz_of_circuit (t : circuit) (posx : float) (posy : float) (debug : bool)
    (id_zero_width : bool) : circuit_geometry =
  match t with
  | CId s ->
      let l = if id_zero_width then 0. else 1. in
      CircGeo
        {
          tikz =
            [
              BId
                {
                  fresh_name = fresh_id ();
                  pos = (posx, posy);
                  len = (if id_zero_width then 0. else 1.);
                  sort = s;
                };
            ];
          height = 0.;
          length = l;
          left_interface = CircuitPin (posx, posy);
          right_interface = CircuitPin (posx +. l, posy);
        }
  | SwapTimes (s1, s2) ->
      let swapheight = !otimes_dist in
      CircGeo
        {
          tikz =
            [
              BSwap
                {
                  fresh_name = fresh_swap ();
                  pos = (posx, posy);
                  scaley = !otimes_dist;
                  sorts = (s1, s2);
                };
            ];
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

      (* print_endline "===";
      print_endline (pp_circuit t1);
      print_endline "---";
      print_endline (pp_circuit t2);
      print_endline "==="; *)
      let rec is_all_empty intf =
        match intf with
        | EmptyCircuit -> true
        | CircuitTens (intf1, EmptyCircuit) -> is_all_empty intf1
        | CircuitTens (EmptyCircuit, intf1) -> is_all_empty intf1
        | CircuitTens (intf1, intf2) -> is_all_empty intf1 && is_all_empty intf2
        | _ -> false
      in

      let ri1_aligned, ri2_aligned =
        if (not (is_all_empty ri1)) && not (is_all_empty ri2) then
          circuit_align_interfaces ri1 ri2
        else (ri1, ri2)
      in
      let connections =
        if (not (is_all_empty ri1)) && not (is_all_empty ri2) then
          circuit_connect_interfaces ri1 ri1_aligned
          @ circuit_connect_interfaces ri2 ri2_aligned
        else []
      in
      (* TODO perform check to avoid redundant connections *)
      CircGeo
        {
          tikz = diag1 @ diag2 @ connections;
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
          tikz = diag1 @ diag2 @ circuit_connect_interfaces ri1 li2;
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
                [
                  BCopy
                    {
                      fresh_name = fresh_gen ();
                      pos = (posx, posy);
                      scaley = !otimes_dist;
                      sort = "TODO";
                    };
                ];
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
                [
                  BCoCopy
                    {
                      fresh_name = fresh_gen ();
                      pos = (posx, posy);
                      scaley = !otimes_dist;
                      sort = "TODO";
                    };
                ];
              height = !otimes_dist;
              length = 1.;
              left_interface =
                CircuitTens
                  ( CircuitPin (posx, posy +. !otimes_dist),
                    CircuitPin (posx, posy) );
              right_interface =
                CircuitPin (posx +. 1., posy +. (!otimes_dist /. 2.));
            }
      | "discard" when coar = [] ->
          CircGeo
            {
              tikz =
                [
                  BDiscard
                    {
                      fresh_name = fresh_gen ();
                      pos = (posx, posy);
                      sort = "TODO";
                    };
                ];
              height = 0.;
              length = 1.;
              left_interface = CircuitPin (posx, posy);
              right_interface = EmptyCircuit;
            }
      | "codiscard" when ar = [] ->
          CircGeo
            {
              tikz =
                [
                  BCoDiscard
                    {
                      fresh_name = fresh_gen ();
                      pos = (posx, posy);
                      sort = "TODO";
                    };
                ];
              height = 0.;
              length = 1.;
              left_interface = EmptyCircuit;
              right_interface = CircuitPin (posx +. 1., posy);
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
                [
                  BGen
                    {
                      fresh_name = fresh_gen ();
                      pos = (posx, posy);
                      arity = ar_size;
                      coarity = coar_size;
                      name;
                      otimesdist = !otimes_dist;
                      sorts = (ar, coar);
                    };
                ];
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
          tikz = [ EmptyBlock ];
          height = 0.;
          length = 0.;
          left_interface = EmptyCircuit;
          right_interface = EmptyCircuit;
        }

let rec draw_oplus t1 t2 posx posy max_len debug =
  let (TapeGeo geo2) = tikz_of_tape t2 posx posy max_len debug in
  let (TapeGeo geo1) =
    tikz_of_tape t1 posx (posy +. geo2.height +. !oplus_dist) max_len debug
  in

  let get_tape_blocks bl = match bl with TB tb -> Some tb | _ -> None in

  let top_of_below =
    if !old_alignment then
      max
        (snd (top_of_tape_interface geo2.left_interface))
        (snd (top_of_tape_interface geo2.right_interface))
    else top_of_tape_blocks (List.filter_map get_tape_blocks geo2.tikz)
  in
  let base_of_above =
    if !old_alignment then
      min
        (snd (base_of_tape_interface geo1.left_interface))
        (snd (base_of_tape_interface geo1.right_interface))
    else base_of_tape_blocks (List.filter_map get_tape_blocks geo1.tikz)
  in

  (* check for intersection of drawn diagrams *)
  let (TapeGeo geo1) =
    if top_of_below +. !oplus_dist > base_of_above then
      let adj = top_of_below +. !oplus_dist -. base_of_above in
      move_tape_geometry (TapeGeo geo1) (0., adj)
    else TapeGeo geo1
  in

  let ri1_aligned, ri2_aligned =
    tape_align_interfaces geo1.right_interface geo2.right_interface
  in

  TapeGeo
    {
      tikz =
        geo1.tikz @ geo2.tikz
        @ tape_connect_interfaces geo1.right_interface ri1_aligned
        @ tape_connect_interfaces geo2.right_interface ri2_aligned;
      height = geo1.height +. geo2.height +. !oplus_dist;
      length = max geo1.length geo2.length;
      left_interface = TapeTens (geo1.left_interface, geo2.left_interface);
      right_interface = TapeTens (ri1_aligned, ri2_aligned);
    }

and draw_tape_composition t1 t2 posx posy max_len debug =
  if is_tape_identity t1 then tikz_of_tape t2 posx posy max_len debug
  else if is_tape_identity t2 then tikz_of_tape t1 posx posy max_len debug
  else
    let offset = 0.5 in
    (* draw the left diagram *)
    let (TapeGeo geo1) = tikz_of_tape t1 posx posy max_len debug in

    (* bring the right tape to sum form, then get a list of the summands*)
    let t2_to_sum = deep_clean_tape (tape_to_sum t2) in
    let summand_list = get_summand_list t2_to_sum in

    (* check if the summand list is alignable, i.e. (for now) if all summands have non-void arity *)
    let alignable = List.for_all (fun x -> tape_arity x != []) summand_list in

    if alignable && !align_summands then (
      try
        aligned_tape_composition posy (TapeGeo geo1) offset summand_list max_len
          debug
      with Failure _ ->
        printf [ yellow ]
          "Could not apply aligned composition, reverting to non-aligned.\n";
        non_aligned_tape_composition t1 t2 posx posy (TapeGeo geo1) offset
          max_len debug)
    else
      non_aligned_tape_composition t1 t2 posx posy (TapeGeo geo1) offset max_len
        debug
(* let _ = printf [ red ] "debug: NON aligned composition\n" in *)

and align_interface_centers intf1 intf2 posx =
  (try snd (base_of_tape_interface intf1) with Failure _ -> posx)
  +. (tape_interface_height intf1 /. 2.)
  -. (tape_interface_height intf2 /. 2.)

and aligned_tape_composition posy (TapeGeo geo1) offset summand_list max_len
    debug =
  let _ = printf [ blue ] "debug: aligned composition\n" in

  let rintf_list = list_of_tape_interface geo1.right_interface in
  let intf_tape_pair = pair_intfs_tapes rintf_list summand_list in
  let intf_geo_pair =
    List.map
      (fun (intf, t) ->
        (intf, tikz_of_tape t (get_max_x_tape intf) 0. max_len debug))
      intf_tape_pair
  in

  let offset_multipliers =
    List.map
      (fun (intf, TapeGeo geo) ->
        abs_float
          (tape_interface_height intf
          -. tape_interface_height geo.left_interface)
        +. 0.25)
      intf_geo_pair
  in

  let max_in_float_list = List.fold_left (fun m1 m2 -> max m1 m2) min_float in
  let min_in_float_list = List.fold_left (fun m1 m2 -> min m1 m2) max_float in

  let offset_multiplier = max_in_float_list offset_multipliers in

  let intf_geo_pair =
    List.map
      (fun (intf, TapeGeo geo) ->
        ( intf,
          move_tape_geometry (TapeGeo geo)
            ( offset_multiplier,
              align_interface_centers intf geo.left_interface posy ) ))
      intf_geo_pair
  in

  let diag_top =
    List.map
      (fun (_, TapeGeo geo) -> top_of_tape_blocks (get_tape_blocks geo.tikz))
      intf_geo_pair
    |> max_in_float_list
  in

  let diag_bot =
    List.map
      (fun (_, TapeGeo geo) -> base_of_tape_blocks (get_tape_blocks geo.tikz))
      intf_geo_pair
    |> min_in_float_list
  in

  let diag_height = diag_top -. diag_bot in

  let diag_len =
    List.map (fun (_, TapeGeo geo) -> geo.length) intf_geo_pair
    |> min_in_float_list
  in

  let diag_rintf =
    List.map (fun (_, TapeGeo geo) -> geo.right_interface) intf_geo_pair
    |> tape_interface_of_list
  in

  (* ADD INTERSECTION CHECK HERE *)
  TapeGeo
    {
      tikz =
        (List.map
           (fun (intf, TapeGeo geo) ->
             tape_connect_interfaces intf geo.left_interface)
           intf_geo_pair
        |> List.flatten)
        @ geo1.tikz
        @ (List.map (fun (_, TapeGeo geo) -> geo.tikz) intf_geo_pair
          |> List.flatten);
      height = max geo1.height diag_height;
      length = geo1.length +. diag_len +. (offset *. offset_multiplier);
      left_interface = geo1.left_interface;
      right_interface = diag_rintf;
    }

and non_aligned_tape_composition t1 t2 posx posy (TapeGeo geo1) offset max_len
    debug =
  (* draw geo2 in dummy position, to gather its interface height *)
  let (TapeGeo geo2) = tikz_of_tape t2 0.0 0.0 max_len debug in

  let offset_multiplier =
    abs_float
      (tape_interface_height geo1.right_interface
      -. tape_interface_height geo2.left_interface)
    +. 0.25
  in

  (* redraw geo2, with correct tape alignment *)
  let (TapeGeo geo2) =
    tikz_of_tape t2
      (posx +. geo1.length +. (offset *. offset_multiplier))
      (align_interface_centers geo1.right_interface geo2.left_interface posy)
      max_len debug
  in

  (* align spacing between binary tape: case spacing(t1) < spacing(t2) *)
  let (TapeGeo geo1) =
    if
      is_interface_binary geo1.right_interface
      && is_interface_binary geo2.left_interface
      && get_space_between_nonempty_summands geo1.right_interface
         < get_space_between_nonempty_summands geo2.left_interface
    then (
      let prev_opldist = !oplus_dist in
      oplus_dist := get_space_between_nonempty_summands geo2.left_interface;
      let res = tikz_of_tape t1 posx posy max_len debug in
      (* Constrain len to <= geo1.length ? *)
      oplus_dist := prev_opldist;
      res)
    else TapeGeo geo1
  in

  (* align spacing between binary tape: case spacing(t1) > spacing(t2) *)
  let (TapeGeo geo2) =
    if
      is_interface_binary geo1.right_interface
      && is_interface_binary geo2.left_interface
      && get_space_between_nonempty_summands geo1.right_interface
         > get_space_between_nonempty_summands geo2.left_interface
    then (
      let prev_opldist = !oplus_dist in
      oplus_dist := get_space_between_nonempty_summands geo1.right_interface;
      let res =
        tikz_of_tape t2
          (posx +. geo1.length +. (offset *. offset_multiplier))
          ((try snd (base_of_tape_interface geo1.right_interface)
            with Failure _ -> posx)
          +. (tape_interface_height geo1.right_interface /. 2.)
          -. (get_tape_left_interface_height t2 /. 2.))
          max_len debug
      in
      oplus_dist := prev_opldist;
      res)
    else TapeGeo geo2
  in

  (* horizontal adjustment *)
  let diff =
    min_x_in_diags [ TapeGeo geo1 ]
    -. min_x_in_diags [ TapeGeo geo2 ]
    +. geo1.length
  in
  let diff = if diff > 0. then diff else 0. in
  let (TapeGeo geo2) = move_tape_geometry (TapeGeo geo2) (diff, 0.0) in

  (* vertical adjustment *)
  let adj =
    if
      is_interface_nonempty geo1.right_interface
      && is_interface_nonempty geo1.left_interface
    then
      nonempty_interface_diff_centers geo1.right_interface geo2.left_interface
    else 0.
  in
  let (TapeGeo geo2) = move_tape_geometry (TapeGeo geo2) (0., adj) in
  TapeGeo
    {
      tikz =
        tape_connect_interfaces geo1.right_interface geo2.left_interface
        @ geo1.tikz @ geo2.tikz;
      height = max geo1.height geo2.height;
      length = geo1.length +. geo2.length +. (offset *. offset_multiplier);
      left_interface = geo1.left_interface;
      right_interface = geo2.right_interface;
    }

and tikz_of_tape (t : tape) (posx : float) (posy : float) (max_len : float)
    (debug : bool) : tape_geometry =
  let t = tape_to_sum t in
  match t with
  | TId l -> tikz_of_tape (tid_to_normal_form l) posx posy max_len debug
  | TId0 ->
      TapeGeo
        {
          tikz = [ TB EmptyTBlock ];
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
        tikz_of_circuit_meas c posx (!tape_padding +. posy) debug !zero_len_ids
      in
      TapeGeo
        {
          tikz =
            TB
              (BTape
                 {
                   pos = (posx, posy);
                   width = l;
                   height = h +. (2. *. !tape_padding);
                 })
            :: List.map (fun cb -> CB cb) diag;
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
  | TCompose (t1, t2) -> draw_tape_composition t1 t2 posx posy max_len debug
  | Oplus (TId0, t2) -> tikz_of_tape t2 posx posy max_len debug
  | Oplus (t1, TId0) -> tikz_of_tape t1 posx posy max_len debug
  | Oplus (t1, t2) -> draw_oplus t1 t2 posx posy max_len debug
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
            [
              TB
                (BSwapTape
                   {
                     pos = (posx, posy);
                     n1 = len1;
                     n2 = len2;
                     oplusdist = !oplus_dist;
                     otimesdist = !otimes_dist;
                     tapepadding = !tape_padding;
                     width = 2.0;
                   });
            ];
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
            [
              TB
                (BCutTape
                   {
                     pos = (posx, posy);
                     n;
                     tapepadding = !tape_padding;
                     otimesdist = !otimes_dist;
                   });
            ];
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
            [
              TB
                (BSpawnTape
                   {
                     pos = (posx, posy);
                     n;
                     tapepadding = !tape_padding;
                     otimesdist = !otimes_dist;
                   });
            ];
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
      let toth = (2. *. h) +. !oplus_dist in
      let l = min ((toth /. 2.) +. 1.) max_len in
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
            [
              TB
                (BSplitTape
                   {
                     pos = (posx, posy);
                     n;
                     len = l -. 1.;
                     tapepadding = !tape_padding;
                     otimesdist = !otimes_dist;
                     oplusdist = !oplus_dist;
                   });
            ];
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
      let l = min ((toth /. 2.) +. 1.) max_len in
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
            [
              TB
                (BJoinTape
                   {
                     pos = (posx, posy);
                     n;
                     len = l -. 1.;
                     tapepadding = !tape_padding;
                     otimesdist = !otimes_dist;
                     oplusdist = !oplus_dist;
                   });
            ];
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
  | Trace (_l, t) ->
      let (TapeGeo diag) =
        tikz_of_tape
          (if !wrap_trace_ids then wrap_in_ids t else t)
          posx posy max_len debug
      in

      let posl =
        get_highest_nonempty_interface diag.left_interface
        |> base_of_tape_interface
      in
      let posr =
        get_highest_nonempty_interface diag.right_interface
        |> base_of_tape_interface
      in
      let maxy = top_of_tape_blocks (get_tape_blocks diag.tikz) in
      let n =
        get_highest_nonempty_interface diag.left_interface
        |> flatten_tape |> List.length
      in
      let h =
        (2. *. !tape_padding) +. (float_of_int (max (n - 1) 0) *. !otimes_dist)
      in
      let trace_block =
        BTraceTape
          {
            pos_l = posl;
            pos_r = posr;
            n;
            len = 6969;
            tapepadding = !tape_padding;
            otimesdist = !otimes_dist;
            oplusdist = !oplus_dist;
            max_y = maxy;
          }
      in
      let radius_right = (maxy +. !oplus_dist +. h -. snd posr) /. 2. in
      let radius_left = (maxy +. !oplus_dist +. h -. snd posl) /. 2. in
      let ri_chopped = chop_off_highest_nonempty_intf diag.right_interface in
      let li_chopped = chop_off_highest_nonempty_intf diag.left_interface in
      let ri_aligned =
        align_tape_interface_to_x ri_chopped (fst posr +. radius_right)
      in
      let li_aligned =
        align_tape_interface_to_x li_chopped (fst posl -. radius_left)
      in

      let res =
        TapeGeo
          {
            diag with
            tikz =
              diag.tikz @ [ TB trace_block ]
              @ tape_connect_interfaces ri_chopped ri_aligned
              @ tape_connect_interfaces li_aligned li_chopped;
            left_interface = li_aligned;
            right_interface = ri_aligned;
            length = diag.length +. radius_left +. radius_right;
          }
      in
      (* print_endline (show_tape_geometry res); *)
      move_tape_geometry res (0., 0.)

(********************************************************************************************************)

(* loses tape information, gets only circuit pins *)

let label_tape (ri : tape_draw_interface) (li : tape_draw_interface)
    (ar : string list list) (coar : string list list) =
  let ri_flattened =
    List.map (fun (x, y) -> (x -. (0.5 /. !scale_x), y)) (flatten_tape ri)
  in
  let li_flattened =
    List.map (fun (x, y) -> (x +. (0.5 /. !scale_x), y)) (flatten_tape li)
  in
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
        Printf.fprintf oc "%s\n"
          (String.concat "\n" (List.map tikz_of_circuit_block s));
        (* write something *)
        close_out oc
      with Sys_error e -> eprintf [ red ] "System error: \"%s\"\n" e)

let tikzpicture_of_ast (ast : tape) (posx : float) (posy : float) =
  (* print_endline (pp_tape ast); *)
  match
    tikz_of_tape (ast |> tape_to_sum |> tape_to_sum) posx posy infinity false
  with
  | TapeGeo { tikz = s; left_interface = li; right_interface = ri; _ } ->
      let header =
        Printf.sprintf
          "\\def\\xscale{%f}\n\
           \\def\\yscale{%f}\n\
           \\begin{tikzpicture}[inner sep=0,outer sep=0, xscale = \\xscale, \
           yscale = \\yscale]"
          !scale_x !scale_y
      in
      let footer = "\\end{tikzpicture}" in

      let blocks =
        if !join_wires then
          let cblocks =
            get_circuit_blocks s |> ids_to_connectors |> swaps_to_connectors
            |> List.filter is_circuit_block_nonzero_width
          in
          let tblocks =
            get_tape_blocks s |> List.filter is_tape_block_nonzero_width
          in
          let connectors =
            cblocks |> get_connectors |> List.map orient_connector
            |> match_connectors
          in
          let cblocks = (cblocks |> get_non_connectors) @ connectors in

          (tblocks |> List.map (fun x -> TB x))
          @ (cblocks |> List.map (fun x -> CB x))
        else s
      in
      let tikz_string = tikz_of_block_list blocks in
      Printf.sprintf "%s\n%s\n%s\n%s\n" header tikz_string
        (label_tape li ri (tape_arity ast) (tape_coarity ast))
        footer

let latex_of_tape_matrix t =
  let matrix = Matrix.get_matrix t in

  let matrix_format =
    Printf.sprintf
      "\\setlength{\\arraycolsep}{0.5cm}\\renewcommand{\\arraystretch}{5}\n\
       $\\begin{bmatrix}\n\
       %s\n\
       \\end{bmatrix}$\n"
  in

  let matrix_strings =
    List.map (List.map (List.map (fun x -> tikzpicture_of_ast x 0. 0.))) matrix
  in

  let s = (List.map (List.map (fun x -> String.concat "," x))) matrix_strings in
  let s =
    (List.map (fun x ->
         String.concat "&"
           (List.map
              (fun y ->
                if y = "" then "\\emptyset"
                else Printf.sprintf "\\left | \\vcenter{\\hbox{%s}} \\right |" y)
              x)))
      s
  in
  let s = String.concat "\\\\" s in
  matrix_format s

let draw_tape_matrix t path =
  try
    let oc = open_out path in
    let s = latex_of_tape_matrix t in
    Printf.fprintf oc "%s" s;
    Printf.printf "Drawing saved at path: \t'%s'\n"
      (sprintf [ green ] "%s" path)
  with Sys_error e -> eprintf [ red; Bold ] "System error: \"%s\"\n" e

let draw_tape_and_matrix t path =
  let t_drawing = tikzpicture_of_ast t 0. 0. in
  let matrix = latex_of_tape_matrix t in
  try
    let oc = open_out path in
    Printf.fprintf oc
      "\\textbf{Tape}\n\n\
       $\\vcenter{\\hbox{%s}}$\\hspace{2cm}\n\n\
       \\textbf{Matrix}:\n\n\
       %s\n"
      t_drawing matrix
  with Sys_error e -> eprintf [ red; Bold ] "System error: \"%s\"\n" e

let draw_tape_at_pos (ast : tape) (path : string) (posx : float) (posy : float)
    =
  try
    let oc = open_out path in

    Printf.fprintf oc "%s" (tikzpicture_of_ast ast posx posy);
    Printf.printf "Drawing saved at path: \t'%s'\n"
      (sprintf [ green ] "%s" path);
    (* write something *)
    close_out oc
  with Sys_error e -> eprintf [ red; Bold ] "System error: \"%s\"\n" e

let draw_tape (ast : tape) (path : string) = draw_tape_at_pos ast path 0. 0.
