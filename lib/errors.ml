exception TypeError of string
(** Exception raised for type errors. *)

exception RuntimeError of string
(** Exception raised for runtime errors. *)

exception ParseError of string
(** Exception raised for parse errors. *)

exception Syntax_error of int * int * string
(** Exception raised for syntax errors, with line and column information. *)

exception ImpError of string
(** Exception raised for errors in the imperative language. *)
