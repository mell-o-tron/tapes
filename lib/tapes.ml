type circuit = CId of Ast.sort
            |  CId1
            |  Gen of string * (Ast.sort list) * (Ast.sort list)
            |  CCompose of circuit * circuit
            |  Otimes of circuit * circuit
            |  SwapTimes of Ast.sort * Ast.sort
            [@@deriving show]
            
type tape =   TId of (Ast.sort list list)
            | TId0
            | Tape of circuit
            | TCompose of tape * tape
            | Oplus of tape * tape
            | SwapPlus of (Ast.sort list * Ast.sort list)
            
            (* not in tech report -- should include?? *)
            | Ldistr of (Ast.sort list * Ast.sort list * Ast.sort list)
            
            | Discard of Ast.sort list
            | Copy of Ast.sort list
            | CoDiscard of Ast.sort list
            | CoCopy of Ast.sort list
            [@@deriving show]


(* Pretty-print a list of sorts, e.g. ["a"; "b"] becomes: ["a", "b"] *)
let pp_sort_list (lst : string list) : string =
  "[" ^ (String.concat ", " (List.map (fun s -> "\"" ^ s ^ "\"") lst)) ^ "]"

(* Pretty-print a circuit *)
let rec pp_circuit (c : circuit) : string =
  match c with
  | CId s ->
      Printf.sprintf "Id(\"%s\")" s
  | CId1 ->
      "Id1"
  | Gen (name, args, rets) ->
      Printf.sprintf "Gen(%s, [%s], [%s])"
        name
        (String.concat ", " args)
        (String.concat ", " rets)
  | CCompose (c1, c2) ->
      Printf.sprintf "(%s ; %s)" (pp_circuit c1) (pp_circuit c2)
  | Otimes (c1, c2) ->
      Printf.sprintf "(%s ⊗  %s)" (pp_circuit c1) (pp_circuit c2)
  | SwapTimes (s1, s2) ->
      Printf.sprintf "σ⊗(\"%s\", \"%s\")" s1 s2
      

(* Pretty-print a tape *)
let rec pp_tape (t : tape) : string =
  match t with
  | TId lsts ->
      (* Each element of lsts is a list of sorts *)
      Printf.sprintf "Id(%s)"
        (String.concat "; " (List.map pp_sort_list lsts))
  | TId0 ->
      "Id0"
  | Tape c ->
      Printf.sprintf "Tape(%s)" (pp_circuit c)
  | TCompose (t1, t2) ->
      Printf.sprintf "(%s ; %s)" (pp_tape t1) (pp_tape t2)
  | Oplus (t1, t2) ->
      Printf.sprintf "(%s ⊕  %s)" (pp_tape t1) (pp_tape t2)
  | SwapPlus (lst1, lst2) ->
      Printf.sprintf "σ⊕(%s, %s)" (pp_sort_list lst1) (pp_sort_list lst2)
  | Ldistr (lst1, lst2, lst3) ->
      Printf.sprintf "δl(%s, %s, %s)"
        (pp_sort_list lst1)
        (pp_sort_list lst2)
        (pp_sort_list lst3)
  | Discard lst ->
      Printf.sprintf "Discard(%s)" (pp_sort_list lst)
  | Copy lst ->
      Printf.sprintf "Copy(%s)" (pp_sort_list lst)
  | CoDiscard lst ->
      Printf.sprintf "CoDiscard(%s)" (pp_sort_list lst)
  | CoCopy lst ->
      Printf.sprintf "CoCopy(%s)" (pp_sort_list lst)

