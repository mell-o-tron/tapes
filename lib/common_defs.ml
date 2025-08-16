open Ppx_compare_lib.Builtin

type sort = string [@@deriving show, compare]

(** Pretty-print a list of sorts *)
let pp_sort_list (lst : string list) : string =
  "[" ^ String.concat ", " (List.map (fun s -> "\"" ^ s ^ "\"") lst) ^ "]"
