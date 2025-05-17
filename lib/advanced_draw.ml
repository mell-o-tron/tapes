open Tapes

type sort = string

let oplus_dist = ref 0.25
let otimes_dist = ref 0.5
let tape_padding = ref 0.25

type intf_node = (float * float) ref option

type circuit_graph =
  | GCId of {
      pos : float * float;
      len : float;
      sort : sort;
      prev : intf_node;
      next : intf_node;
    }
  | GCSwap of {
      pos1 : float * float;
      pos2 : float * float;
      len : float;
      sort1 : sort;
      sort2 : sort;
      prev1 : intf_node;
      prev2 : intf_node;
      next1 : intf_node;
      next2 : intf_node;
    }
  | GGen of {
      name : string;
      pos : float * float;
      prev : intf_node list;
      next : intf_node list;
    }
  | EmptyGraph

let rec get_graph_right_interface (g : circuit_graph list) =
  match g with
  | [ GCId x ] -> [ x.next ]
  | [ GCSwap x ] -> [ x.next1; x.next2 ]
  | [ GGen x ] -> x.next
  | [ EmptyGraph ] -> []
  | l ->
      List.map (fun x -> [ x ]) l
      |> List.map get_graph_right_interface
      |> List.concat

let rec get_graph_left_interface (g : circuit_graph list) =
  match g with
  | [ GCId x ] -> [ x.prev ]
  | [ GCSwap x ] -> [ x.prev1; x.prev2 ]
  | [ GGen x ] -> x.prev
  | [ EmptyGraph ] -> []
  | l ->
      List.map (fun x -> [ x ]) l
      |> List.map get_graph_left_interface
      |> List.concat

let connect_graphs (_g1 : circuit_graph list) (_g2 : circuit_graph list) =
  failwith ""

let rec circuit_to_graph_list (c : circuit) (pos : float * float) =
  let posx, posy = pos in
  match c with
  | CId1 -> [ EmptyGraph ]
  | CId s -> [ GCId { pos; len = 1.; sort = s; prev = None; next = None } ]
  | Gen (name, ar, coar) ->
      [
        GGen
          {
            name;
            pos;
            prev = List.map (fun _ -> None) ar;
            next = List.map (fun _ -> None) coar;
          };
      ]
  | SwapTimes (s1, s2) ->
      [
        GCSwap
          {
            pos1 = (posx, posy +. !otimes_dist);
            pos2 = pos;
            len = 1.;
            sort1 = s1;
            sort2 = s2;
            prev1 = None;
            prev2 = None;
            next1 = None;
            next2 = None;
          };
      ]
  | Otimes (c1, c2) ->
      circuit_to_graph_list c1 (posx, posy +. !otimes_dist)
      @ circuit_to_graph_list c2 pos
  | CCompose (c1, c2) -> (
      match (c1, c2) with
      | CId1, _ -> circuit_to_graph_list c2 pos
      | _, CId1 -> circuit_to_graph_list c1 pos
      | _ -> failwith "composition not yet implemented")
