type circuit =
  | CId of Terms.sort
  | CId1
  | Gen of string * Terms.sort list * Terms.sort list
  | CCompose of circuit * circuit
  | Otimes of circuit * circuit
  | SwapTimes of Terms.sort * Terms.sort
[@@deriving show]

type tape =
  | TId of Terms.sort list list (* da vedere *)
  | TId0
  | Tape of circuit
  | TCompose of tape * tape
  | Oplus of tape * tape
  | Trace of Terms.sort list * tape
  | SwapPlus of (Terms.sort list * Terms.sort list)
  | Cut of Terms.sort list
  | Split of Terms.sort list
  | Spawn of Terms.sort list
  | Join of Terms.sort list
[@@deriving show]

(* Pretty-print a list of sorts, e.g. ["a"; "b"] becomes: ["a", "b"] *)
let pp_sort_list (lst : string list) : string =
  "[" ^ String.concat ", " (List.map (fun s -> "\"" ^ s ^ "\"") lst) ^ "]"

(* Pretty-print a circuit *)
let rec pp_circuit (c : circuit) : string =
  match c with
  | CId s -> Printf.sprintf "Id(\"%s\")" s
  | CId1 -> "Id1"
  | Gen (name, args, rets) ->
      Printf.sprintf "Gen(%s, [%s], [%s])" name (String.concat ", " args)
        (String.concat ", " rets)
  | CCompose (c1, c2) ->
      Printf.sprintf "(%s ; %s)" (pp_circuit c1) (pp_circuit c2)
  | Otimes (c1, c2) ->
      Printf.sprintf "(%s ⊗  %s)" (pp_circuit c1) (pp_circuit c2)
  | SwapTimes (s1, s2) -> Printf.sprintf "σ⊗(\"%s\", \"%s\")" s1 s2

(* Pretty-print a tape *)
let rec pp_tape (t : tape) : string =
  match t with
  | TId lsts ->
      (* Each element of lsts is a list of sorts *)
      Printf.sprintf "Id(%s)" (String.concat "; " (List.map pp_sort_list lsts))
  | TId0 -> "Id0"
  | Tape c -> Printf.sprintf "Tape(%s)" (pp_circuit c)
  | TCompose (t1, t2) -> Printf.sprintf "(%s ; %s)" (pp_tape t1) (pp_tape t2)
  | Oplus (t1, t2) -> Printf.sprintf "(%s ⊕  %s)" (pp_tape t1) (pp_tape t2)
  | SwapPlus (lst1, lst2) ->
      Printf.sprintf "σ⊕(%s, %s)" (pp_sort_list lst1) (pp_sort_list lst2)
  | Cut lst -> Printf.sprintf "Cut(%s)" (pp_sort_list lst)
  | Split lst -> Printf.sprintf "Split(%s)" (pp_sort_list lst)
  | Spawn lst -> Printf.sprintf "Spawn(%s)" (pp_sort_list lst)
  | Join lst -> Printf.sprintf "Join(%s)" (pp_sort_list lst)
  | Trace (lst, t) ->
      Printf.sprintf "Tr_{%s}(%s)" (pp_sort_list lst) (pp_tape t)

(* remove redundant identities *)
let rec clean_circuit (c : circuit) =
  match c with
  | CCompose (c1, c2) -> CCompose (clean_circuit c1, clean_circuit c2)
  | Otimes (c1, CId1) -> clean_circuit c1
  | Otimes (CId1, c2) -> clean_circuit c2
  | Otimes (c1, c2) -> Otimes (clean_circuit c1, clean_circuit c2)
  | _ -> c

let rec clean_tape (t : tape) =
  match t with
  | TCompose (TId0, TId0) -> TId0
  | TCompose (t1, t2) -> TCompose (clean_tape t1, clean_tape t2)
  | Oplus (t1, TId0) -> clean_tape t1
  | Oplus (TId0, t2) -> clean_tape t2
  | Oplus (t1, t2) -> Oplus (clean_tape t1, clean_tape t2)
  | Tape c -> Tape (clean_circuit c)
  | Trace (l, t) -> Trace (l, clean_tape t)
  | _ -> t

let rec deep_clean_tape (t : tape) =
  let t1 = clean_tape t in
  if t = t1 then t else deep_clean_tape t1
