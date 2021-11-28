
open Format
open Lib
open Ast
open Tast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

let error loc e = raise (Error (loc, e))

(* on utilise une map *)
type struct_env = structure M.t
type fun_env = function_ M.t

(* ptyp -> typ *)
let rec type_type = function
  | PTident { id = "int" } -> Tint
  | PTident { id = "bool" } -> Tbool
  | PTident { id = "string" } -> Tstring
  | PTptr ty -> Tptr (type_type ty)
  | _ -> error dummy_loc ("unknown struct ") (* TODO type structure *)

let rec eq_type ty1 ty2 = match ty1, ty2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct s1, Tstruct s2 -> s1 == s2
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | _ -> false
    (* TODO autres types *)

let fmt_used = ref false
let fmt_imported = ref false

let evar v = { expr_desc = TEident v; expr_typ = v.v_typ }

let new_var =
  let id = ref 0 in
  fun x loc ?(used=false) ty ->
    incr id;
    { v_name = x; v_id = !id; v_loc = loc; v_typ = ty; v_used = used; v_addr = false }

module Env = struct
  module M = Map.Make(String)
  type t = var M.t
  let empty = M.empty
  let find = M.find
  let add env v = M.add v.v_name v env

  let all_vars = ref []
  let check_unused () =
    let check v =
      if v.v_name <> "_" && (* TODO used *) true then error v.v_loc "unused variable" in
    List.iter check !all_vars


  let var x loc ?used ty env =
    let v = new_var x loc ?used ty in
    all_vars := v :: !all_vars;
    add env v, v

  (* TODO type () et vecteur de types *)
end

let tvoid = Tmany []
let make d ty = { expr_desc = d; expr_typ = ty }
let stmt d = make d tvoid (* statement *)

let rec expr env e =
 let e, ty, rt = expr_desc env e.pexpr_loc e.pexpr_desc in
  { expr_desc = e; expr_typ = ty }, rt

(* à un environnement, une localisation, une expression
   associe: une expression typée (epxr_desc), un type d'expression (typ),
   et si oui ou non l'expression renvoie quelque chose *)
and expr_desc env loc = function
  | PEskip ->
     TEskip, tvoid, false
  | PEconstant c -> begin
      match c with
      | Cint _ -> TEconstant c, Tint, false
      | Cstring _ -> TEconstant c, Tstring, false
      | Cbool _ -> TEconstant c, Tbool, false
    end
  | PEbinop (op, e1, e2) ->
    (* TODO TODO TODO *) assert false
  | PEunop (Uamp, e1) ->
    (* TODO *) assert false
  | PEunop (Uneg | Unot | Ustar as op, e1) ->
    (* TODO *) assert false
  | PEcall ({id = "fmt.Print"}, el) ->
    (* TODO *) TEprint [], tvoid, false
  | PEcall ({id="new"}, [{pexpr_desc=PEident {id}}]) ->
     let ty = match id with
       | "int" -> Tint | "bool" -> Tbool | "string" -> Tstring
       | _ -> (* TODO *) error loc ("no such type " ^ id) in
     TEnew ty, Tptr ty, false
  | PEcall ({id="new"}, _) ->
     error loc "new expects a type"
  | PEcall (id, el) ->
     (* TODO *) assert false
  | PEfor (e, b) ->
     (* TODO *) assert false
  | PEif (e1, e2, e3) ->
     (* TODO *) assert false
  | PEnil ->
     (* TODO *) assert false
  | PEident {id=id} ->
     (* TODO *) (try let v = Env.find id env in TEident v, v.v_typ, false
      with Not_found -> error loc ("unbound variable " ^ id))
  | PEdot (e, id) ->
     (* TODO *) assert false
  | PEassign (lvl, el) ->
     (* TODO *) TEassign ([], []), tvoid, false 
  | PEreturn el ->
     (* TODO *) TEreturn [], tvoid, true
  | PEblock el ->
     (* TODO *) TEblock [], tvoid, false
  | PEincdec (e, op) ->
     (* TODO *) assert false
  | PEvars _ ->
     (* TODO *) assert false 

let found_main = ref false

(* 1. declare structures *)
(* builds a *typed* structure environment, with at first no field *)
let phase1 structures = function
  | PDstruct ({ps_name = {id; loc}; _} as s) ->
      if M.mem id structures
        then raise (Error (loc, "structure '" ^ id ^ "' already defined"))
        else M.add id {s_name=id; s_fields=(Hashtbl.create 5)} structures
  | PDfunction _ -> structures

let sizeof = function
  | Tint | Tbool | Tstring | Tptr _ -> 8
  | _ -> (* TODO *) assert false 


(* associate precise type if valid in a given context *)
let rec find_type structures = function
  | PTptr ptyp -> Tptr (find_type structures ptyp)
  | PTident {id; loc} -> begin match id with
    | "int" -> Tint
    | "bool" -> Tbool
    | "string" -> Tstring
    | _ -> if M.mem id structures
        then Tstruct (M.find id structures)
        else raise (Error (loc, "unknown type '" ^ id ^ "'"))
    end

(* returns a list of typed parameters for a given function, as a list of vars *)
let rec build_parameters structures f_name used_names = function
  | [] -> []
  | ({id; loc}, ptyp) :: q -> if List.mem id used_names
      then raise (Error (loc, "function '" ^ f_name ^
        "': redefinition of parameter '" ^ id ^ "'"))
      else begin
        let typ = find_type structures ptyp in
        (* NOTE v_id may need to be changed in the future *)
        let v = {v_name=id; v_id=0; v_loc=loc; v_typ=typ; v_used=false; v_addr=false} in
        v :: build_parameters structures f_name (id :: used_names) q
      end

(* 2. declare functions and type fields *)
(* only creates function mappings while editing structure fields *)
let phase2 structures functions = function
  | PDfunction {pf_name={id; loc}; pf_params=pl; pf_typ=tyl} ->
      if id = "main" then found_main := true;
      if M.mem id functions
        then raise (Error (loc, "function '" ^ id ^ "' already defined"));
      let fn_params = build_parameters structures id [] pl in
      let fn_typ = List.map (find_type structures) tyl in
      M.add id {fn_name=id; fn_params; fn_typ} functions

  | PDstruct {ps_name = {id=id_s; _}; ps_fields} ->
      let s = M.find id_s structures in
      List.iter (function ({id; loc}, ptyp) ->
        let typ = find_type structures ptyp in
        Hashtbl.add s.s_fields id {f_name=id; f_typ=typ; f_ofs=0}) ps_fields;
      functions


(* 3. type check function bodies *)
let decl = function
  | PDfunction { pf_name={id; loc}; pf_body = e; pf_typ=tyl } ->
    (* TODO check name and type *) 
    let f = { fn_name = id; fn_params = []; fn_typ = []} in
    let e, rt = expr Env.empty e in
    TDfunction (f, e)
  | PDstruct {ps_name={id}} ->
    (* TODO *) let s = { s_name = id; s_fields = Hashtbl.create 5 } in
     TDstruct s

(* local variables (functions, structures) are used to represent context *)
let file ~debug:b (imp, dl) =
  debug := b;
  (* fmt_imported := imp; *)
  let structures = List.fold_left phase1 M.empty dl in
  let functions = List.fold_left (phase2 structures) M.empty dl in
  if not !found_main then raise (Error (dummy_loc, "missing method main"));
  let dl = List.map decl dl in
  Env.check_unused (); (* TODO variables non utilisees *)
  if imp && not !fmt_used then raise (Error (dummy_loc, "fmt imported but not used"));
  dl
