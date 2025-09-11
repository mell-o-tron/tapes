open Ast
open Terms
open Errors

let sorts : string list ref = ref []
let env : (string, expr) Hashtbl.t = Hashtbl.create 10
let gens : (string, term) Hashtbl.t = Hashtbl.create 10

(** replaces the generator names with terms *)
let rec subst_gen_name_term (v : string) (t : term) tbl : term =
  match t with
  | GenVar v ->
      if Hashtbl.mem tbl v then Hashtbl.find tbl v
      else
        raise
          (RuntimeError
             (Printf.sprintf
                "generator %s has not been defined prior to its use" v))
  | Otimes (t1, t2) ->
      Otimes (subst_gen_name_term v t1 tbl, subst_gen_name_term v t2 tbl)
  | Oplus (t1, t2) ->
      Oplus (subst_gen_name_term v t1 tbl, subst_gen_name_term v t2 tbl)
  | Compose (t1, t2) ->
      Compose (subst_gen_name_term v t1 tbl, subst_gen_name_term v t2 tbl)
  | Trace (a, t1) -> Trace (a, subst_gen_name_term v t1 tbl)
  | _ -> t

let subst_gen_name (v : string) (e : expr) tbl =
  Printf.printf "%s\n" v;
  match e with Term t -> Term (subst_gen_name_term v t tbl) | _ -> e

(** populates the generator variables *)
let populate_genvars (e : expr) =
  e |> Hashtbl.fold (fun k _ e1 -> subst_gen_name k e1 gens) gens

let is_term = function Term _ -> true | _ -> false

let populate_vars_in_term (t : term) =
  let rec get_term e =
    match e with
    | Tape _ -> failwith "is not term"
    | Term t -> t
    | Var id ->
        if Hashtbl.mem env id then get_term (Hashtbl.find env id)
        else raise (RuntimeError (Printf.sprintf "Variable %s not found" id))
  in
  let t_env = Hashtbl.create 10 in
  Hashtbl.iter
    (fun x y -> if is_term y then Hashtbl.add t_env x (get_term y))
    env;
  t |> Hashtbl.fold (fun k _ e1 -> subst_gen_name_term k e1 t_env) t_env
