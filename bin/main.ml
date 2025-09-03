(* open Ssr_typechecker.Terms *)
open Ssr_typechecker.Typecheck
open Ssr_typechecker.Term_to_tape
open Ssr_typechecker.Tapes
open Ssr_typechecker.Terms
open Ssr_typechecker.Ast
open Ssr_typechecker.Environment

(* open Ssr_typechecker.Imp *)
open Ssr_typechecker.Errors
open ANSITerminal

let inchn = open_in Sys.argv.(1)

(** given an input channel, returns an AST *)
let ast_of_channel inchn =
  let lexbuf = Sedlexing.Latin1.from_channel inchn in
  let lexer = Sedlexing.with_tokenizer Ssr_typechecker.Lexer.token lexbuf in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Ssr_typechecker.Parser.main
  in
  try parser lexer with
  | ParseError s ->
      raise
        (Syntax_error
           ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
             (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
             - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol,
             s ))
  | Ssr_typechecker.Parser.Error ->
      raise
        (Syntax_error
           ( (fst (Sedlexing.lexing_positions lexbuf)).pos_lnum,
             (fst (Sedlexing.lexing_positions lexbuf)).pos_cnum
             - (fst (Sedlexing.lexing_positions lexbuf)).pos_bol,
             "Generic Syntax Error" ))

let settings = Hashtbl.create 10

(** typechecks an expression *)
let rec typecheck_command (e : expr) =
  match e with
  | Tape t ->
      printf [] "Tape typecheck result:\t%s\n"
        (if tape_typecheck t then
           sprintf [ green ] "true ✅ -- %s 🡒 %s"
             (tape_arity t |> obj_of_polynomial |> pp_object)
             (tape_coarity t |> obj_of_polynomial |> pp_object)
         else sprintf [ red; Bold ] "false ❌\n%s" (show_tape t))
  | Term t ->
      printf [] "Term typecheck result:\t%s\n"
        (if typecheck t then
           sprintf [ green ] "true ✅ -- %s 🡒 %s"
             (arity t |> obj_of_polynomial |> pp_object)
             (coarity t |> obj_of_polynomial |> pp_object)
         else sprintf [ red; Bold ] "false ❌")
  | Var id ->
      if Hashtbl.mem env id then typecheck_command (Hashtbl.find env id)
      else raise (RuntimeError (Printf.sprintf "Variable %s not found" id))

(** draws a tape / term *)
let rec draw_command (e : expr) (path : string) =
  match e with
  | Tape t ->
      let tc = tape_typecheck t in
      if tc then Ssr_typechecker.Draw.draw_tape (deep_clean_tape t) path
      else
        raise
          (RuntimeError
             (Printf.sprintf "Cannot draw tape: does not typecheck.\n%s "
                (pp_tape t)))
  | Term t ->
      let tc = typecheck t in
      if tc then
        Ssr_typechecker.Draw.draw_tape (deep_clean_tape (_to_tape t)) path
      else raise (RuntimeError "Cannot draw term: does not typecheck.")
  | Var id ->
      if Hashtbl.mem env id then draw_command (Hashtbl.find env id) path
      else raise (RuntimeError (Printf.sprintf "Variable %s not found" id))

(** draws a diagram and the corresponding matrix *)
let draw_matrix_command (e : expr) (path : string) : unit =
  let rec get_tape e =
    match e with
    | Tape t -> t
    | Term t -> _to_tape t
    | Var id ->
        if Hashtbl.mem env id then get_tape (Hashtbl.find env id)
        else raise (RuntimeError (Printf.sprintf "Variable %s not found" id))
  in

  let t : tape = get_tape e in
  Ssr_typechecker.Draw.draw_tape_and_matrix t path

(** draws the normal form of a diagram*)
let draw_normal_command (e : expr) (path : string) : unit =
  let rec get_tape e =
    match e with
    | Tape t -> t
    | Term t -> _to_tape t
    | Var id ->
        if Hashtbl.mem env id then get_tape (Hashtbl.find env id)
        else raise (RuntimeError (Printf.sprintf "Variable %s not found" id))
  in

  let t : tape = get_tape e in
  Ssr_typechecker.Draw.draw_tape_matrix_and_normalform t path

(** draws a diagram and its trace-normal form*)
let draw_trace_nf_command (e : expr) (path : string) : unit =
  let rec get_term e =
    match e with
    | Tape _ -> failwith "can only take trace normal form of term"
    | Term t -> t
    | Var id ->
        if Hashtbl.mem env id then get_term (Hashtbl.find env id)
        else raise (RuntimeError (Printf.sprintf "Variable %s not found" id))
  in

  let t : term = get_term e in
  Ssr_typechecker.Draw.draw_term_trace_normalform t path

(** Draws the finite set cospan obtained by translating a circuit and discarding
    the generators *)
let draw_cospan_command (c : circuit) (path : string) : unit =
  let cos = Ssr_typechecker.Hg_cospan.cospan_of_circuit c in
  Ssr_typechecker.Draw.draw_hg_cospan cos path

let draw_circuit_command (c : circuit) (path : string) : unit =
  Ssr_typechecker.Draw.draw_circuit c path

(** checks if e1 <= e2*)
let check_inclusion_command (e1 : expr) (e2 : expr) : unit =
  let rec get_tape e =
    match e with
    | Tape t -> t
    | Term t -> _to_tape t
    | Var id ->
        if Hashtbl.mem env id then get_tape (Hashtbl.find env id)
        else raise (RuntimeError (Printf.sprintf "Variable %s not found" id))
  in

  let t1 : tape = get_tape e1 in
  let t2 : tape = get_tape e2 in
  Ssr_typechecker.Tape_inclusion.generate_implication_problems t1 t2

(** checks if e1 <= e2 with an invariant (e3)*)
let check_inclusion_inv_command (e1 : expr) (e2 : expr) (e3 : expr) : unit =
  let rec get_tape e =
    match e with
    | Tape _ ->
        failwith "cannot check invariant inclusion of bare tape: use term"
    | Term t -> _to_tape (t |> Ssr_typechecker.Rewrite.trace_normal_form)
    | Var id ->
        if Hashtbl.mem env id then get_tape (Hashtbl.find env id)
        else raise (RuntimeError (Printf.sprintf "Variable %s not found" id))
  in

  let t1 : tape = get_tape e1 in
  let t2 : tape = get_tape e2 in
  let inv : tape = get_tape e3 in
  Ssr_typechecker.Tape_inclusion.inclusion_by_invariant t1 t2 inv

let check_triple_command ctx (t : Ssr_typechecker.Hoare_triples.hoare_triple)
    (inv : expr) =
  let rec get_term e =
    match e with
    | Tape _ -> failwith "invariant should be a term, not a tape"
    | Term t -> t
    | Var id ->
        if Hashtbl.mem env id then get_term (Hashtbl.find env id)
        else raise (RuntimeError (Printf.sprintf "Variable %s not found" id))
  in
  let t1, t2 =
    Ssr_typechecker.Hoare_triples.check_triple ctx t (get_term inv)
  in
  Printf.printf "begun drawing\n";
  draw_command (Term t1) "lhs";
  Printf.printf "done drawing\n";
  Printf.printf "begun drawing\n";
  draw_command (Term t2) "rhs";
  Printf.printf "done drawing\n"
(* let t1, t2 =
    ( Ssr_typechecker.Matrix.term_to_normalized_tape t1,
      Ssr_typechecker.Matrix.term_to_normalized_tape t2 )
  in
  draw_command (Tape t1) "lhs";
  draw_command (Tape t2) "rhs" *)

(** executes a .tapes program *)
let rec exec (p : program) =
  match p with
  | Comm c -> (
      match c with
      | Check e -> typecheck_command (populate_genvars e)
      | Draw (e, path) -> draw_command (populate_genvars e) path
      | DrawMatrix (e, path) -> draw_matrix_command (populate_genvars e) path
      | DrawNF (e, path) -> draw_normal_command (populate_genvars e) path
      | DrawTraceNF (e, path) -> draw_trace_nf_command (populate_genvars e) path
      | CheckInclusion (e1, e2) ->
          check_inclusion_command (populate_genvars e1) (populate_genvars e2)
      | CheckInclusionInvariant (e1, e2, e3) ->
          check_inclusion_inv_command (populate_genvars e1)
            (populate_genvars e2) (populate_genvars e3)
      | SetAxioms fl -> Ssr_typechecker.Fol_encoding.current_axioms := fl
      | DrawCospan (c, path) -> draw_cospan_command c path
      | DrawCircuit (c, path) -> draw_circuit_command c path
      | CheckTriple (ctx, t, inv) -> check_triple_command ctx t inv)
  | Decl d -> (
      match d with
      | ExprDecl (id, _typ, e) -> (
          match e with Term t -> Hashtbl.add defined_terms id t | _ -> ())
      | SortDecl id -> sorts := id :: !sorts
      | GenDecl (id, ob1, ob2, kind) ->
          Hashtbl.add gens id
            (Gen
               ( id,
                 Ssr_typechecker.Terms.obj_to_polynomial ob1,
                 Ssr_typechecker.Terms.obj_to_polynomial ob2,
                 kind )))
  | Seq (p1, p2) ->
      exec p1;
      exec p2
  | Set (id, f) -> (
      Hashtbl.add settings id f;
      match id with
      | "otimesdist" -> Ssr_typechecker.Draw_utils.otimes_dist := f
      | "oplusdist" -> Ssr_typechecker.Draw_utils.oplus_dist := f
      | "paddingdist" -> Ssr_typechecker.Draw_utils.tape_padding := f
      | "alignsummands" ->
          Ssr_typechecker.Draw_utils.align_summands := not (f = 0.)
      | "zerolenids" -> Ssr_typechecker.Draw_utils.zero_len_ids := not (f = 0.)
      | "oldalignment" ->
          Ssr_typechecker.Draw_utils.old_alignment := not (f = 0.)
      | "scalex" -> Ssr_typechecker.Draw_utils.scale_x := f
      | "scaley" -> Ssr_typechecker.Draw_utils.scale_y := f
      | "wrap_trace_ids" ->
          Ssr_typechecker.Draw_utils.wrap_trace_ids := not (f = 0.)
      | "rounded_wires" ->
          Ssr_typechecker.Draw_utils.rounded_wires := not (f = 0.)
      | "join_wires" -> Ssr_typechecker.Draw_utils.join_wires := not (f = 0.)
      | _ -> ())

(** entry-point *)
let main () =
  try
    let p = ast_of_channel inchn in
    exec p
  with
  | TypeError s -> printf [ Bold; red ] "Type Error: %s.\n" s
  | RuntimeError s -> printf [ Bold; red ] "Runtime Error: %s.\n" s
  | ImpError s -> printf [ Bold; red ] "Imp Error: %s.\n" s
  | Syntax_error (a, b, s) ->
      printf [ Bold; red ] "Syntax Error at (%d, %d): %s.\n" a b s
;;

main ()
