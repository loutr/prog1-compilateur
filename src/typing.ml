
open Format
open Lib
open Ast
open Tast

let debug = ref false
let found_main = ref false
let fmt_used = ref false
let fmt_imported = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos
exception Error of Ast.location * string


let tvoid = Tmany []

let rec printable_type = function
  | Tint -> "int"
  | Tbool -> "bool"
  | Tstring -> "string"
  | Tstruct structure -> structure.s_name
  | Tptr Tmany [] -> "*nil"
  | Tptr typ -> "*" ^ (printable_type typ)
  | Tmany [] -> "instruction"
  | Tmany [t] -> printable_type t
  | Tmany (t :: q) -> (printable_type t) ^ " & " ^ (printable_type (Tmany q))

let type_error_message expected received =
  "This expression has type '" ^ (printable_type received)
  ^ "' but an expression of type '" ^ (printable_type expected) ^ "' was expected"

(* context types *)
type struct_env = structure M.t
type fun_env = function_ M.t



(* associate precise type if valid in a given structure context *)
(* replacement for the original 'type_type' function *)
let rec find_type structures = function
  | PTptr ptyp -> Tptr (find_type structures ptyp)
  | PTident {id = "int"; loc} -> Tint
  | PTident {id = "bool"; loc} -> Tbool
  | PTident {id = "string"; loc} -> Tstring
  | PTident {id; loc} -> if M.mem id structures
      then Tstruct (M.find id structures)
      else raise (Error (loc, "unknown type '" ^ id ^ "'"))

let rec eq_type ty1 ty2 = match ty1, ty2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct s1, Tstruct s2 -> s1 == s2
  | Tptr Tmany [], Tptr _ -> true
  | Tptr _, Tptr Tmany [] -> true
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | Tmany [], Tmany [] -> true
  | Tmany (t1 :: q1), Tmany (t2 :: q2) -> (eq_type t1 t2) && (eq_type (Tmany q1) (Tmany q2))
  | _ -> false

let argument_call_correspondance loc expr_typ {v_typ} =
    if not (eq_type expr_typ v_typ)
      then raise (Error (loc, type_error_message v_typ expr_typ))

let rec assignation_correspondance loc lvl_typ el_typ = match lvl_typ, el_typ with
  | [], [] -> ()
  | _, [] -> raise (Error (loc, "not enough values to unpack"))
  | [], _ -> raise (Error (loc, "too many values to unpack"))
  | (lt :: lq), (et :: eq) -> if eq_type lt et
      then assignation_correspondance loc lq eq
      else raise (Error (loc, type_error_message lt et))

let rec lvalue_test {expr_desc} = match expr_desc with
  | TEdot (e, x) -> lvalue_test e
  | TEident _ -> true
  | TEunop (Ustar, _) -> true
  | _ -> false

(* Tmany characterises instructions and multiple-return functions *)
let is_expression = function
  | Tmany _ -> false
  | _ -> true


(* expression associated with a variable *)
let evar v = { expr_desc = TEident v; expr_typ = v.v_typ }

let new_var =
  let id = ref 0 in
  fun x loc ?(used=false) ty ->
    incr id;
    { v_name = x; v_id = !id; v_loc = loc; v_typ = ty; v_used = used; v_addr = false }

module Env = struct
  module M = Map.Make(String)
  type t = var M.t
  type e = {
    vars: t;
    functions: fun_env;
    structures: struct_env;
    return_values: typ list;
    returns: bool;
  }

  let find id env = M.find id env.vars
  let find_opt id env = M.find_opt id env.vars
  let mem id env = M.mem id env.vars
  let add env v = {env with vars=(M.add v.v_name v env.vars)}
  let add_vars = List.fold_left add

  let new_env functions structures f = add_vars {
    vars=M.empty;
    functions; structures;
    return_values=f.fn_typ;
    returns=false
  } f.fn_params

  let all_vars = ref []
  let check_unused () =
    let check v =
      if v.v_name <> "_" && (* TODO used *) true then raise (Error (v.v_loc, "unused variable")) in
    List.iter check !all_vars

  let var x loc ?used ty env =
    let v = new_var x loc ?used ty in
    all_vars := v :: !all_vars;
    add env v, v

  (* TODO type () et vecteur de types *)
end

let make d ty = { expr_desc = d; expr_typ = ty }
let stmt d = {expr_desc=d; expr_typ=tvoid} (* statement *)


(* types an expression and indicates whether it returns something *)
let rec expr (env: Env.e) e =
  let e, ty, env' = expr_desc env e.pexpr_loc e.pexpr_desc in
  { expr_desc = e; expr_typ = ty }, env'

and expr_desc env loc = function
  | PEskip -> TEskip, tvoid, env

  | PEconstant c -> begin match c with
      | Cint _ -> TEconstant c, Tint, env
      | Cstring _ -> TEconstant c, Tstring, env
      | Cbool _ -> TEconstant c, Tbool, env
    end

  | PEbinop (op, e1, e2) ->
      let e1', _ = expr env e1 and e2', _ = expr env e2 in
      let {expr_typ=t1} = e1' and {expr_typ=t2} = e2' in
      begin match op with
        | Badd | Bsub | Bmul | Bdiv | Bmod ->
            begin match t1, t2 with
              | Tint, Tint -> TEbinop (op, e1', e2'), Tint, env
              | Tint, _ -> raise (Error (e2.pexpr_loc, type_error_message Tint t2))
              | _, _ -> raise (Error (e1.pexpr_loc, type_error_message Tint t1))
            end
        | Beq | Bne -> if eq_type t1 t2
            then TEbinop (op, e1', e2'), Tbool, env
            else raise (Error (loc, "expressions do not have the same type"))
        | Blt | Ble | Bgt | Bge ->
            begin match t1, t2 with
              | Tint, Tint -> TEbinop (op, e1', e2'), Tbool, env
              | Tint, _ -> raise (Error (e2.pexpr_loc, type_error_message Tint t2))
              | _, _ -> raise (Error (e1.pexpr_loc, type_error_message Tint t1))
            end
        | Band | Bor ->
            begin match t1, t2 with
              | Tbool, Tbool -> TEbinop (op, e1', e2'), Tbool, env
              | Tbool, _ -> raise (Error (e2.pexpr_loc, type_error_message Tbool t2))
              | _, _ -> raise (Error (e1.pexpr_loc, type_error_message Tbool t1))
            end
      end
    
  | PEunop (Uamp, e1) ->
      let e1', _ = expr env e1 in if lvalue_test e1'
        then TEunop (Uamp, e1'), Tptr e1'.expr_typ, env
        else raise (Error (e1.pexpr_loc, "lvalue required as operand of '&'"))
      
  | PEunop (Uneg | Unot | Ustar as op, e1) ->
      let e1', _ = expr env e1 in begin match op, e1'.expr_typ with
        | Uneg, Tint -> TEunop (Uneg, e1'), Tint, env
        | Uneg, t -> raise (Error (e1.pexpr_loc, type_error_message Tint t))
        | Unot, Tbool -> TEunop (Unot, e1'), Tbool, env
        | Unot, t -> raise (Error (e1.pexpr_loc, type_error_message Tbool t))
        | Ustar, Tptr Tmany [] -> raise (Error (e1.pexpr_loc, "cannot dereference nil"))
        | Ustar, Tptr t -> TEunop (Ustar, e1'), t, env
        | _ -> raise (Error (e1.pexpr_loc, "cannot dereference this"))
      end  

  | PEcall ({id = "fmt.Print"; loc}, el) ->
      let el', typ_list = expr_list env el in
      List.iter (function
        | Tmany _ -> raise (Error (loc, "this is not a correct expression, it cannot be printed"))
        | _ -> ()) typ_list;
      TEprint el', tvoid, env

  | PEcall ({id="new"}, [{pexpr_desc=PEident ident}]) ->
      let t = find_type env.structures (PTident ident) in TEnew t, Tptr t, env
  | PEcall ({id="new"}, _) -> raise (Error (loc, "new expects a type"))

  | PEcall ({id; loc}, el) -> begin match M.find_opt id env.functions with
      | Some callee -> 
          let el', typ_list = expr_list env el in
          List.iter2 (argument_call_correspondance loc)
          typ_list callee.fn_params;
          TEcall (callee, el'), Tmany callee.fn_typ, env
      | None -> raise (Error (loc, "unknown function '" ^ id ^ "'"))     
    end

  | PEfor (e, b) ->
      let e', _ = expr env e and b', _ = expr env b in
      begin match e'.expr_typ, b'.expr_typ with
        | Tmany [], Tbool -> TEfor (e', b'), tvoid, env
        | _, Tbool -> raise (Error (loc, "syntax error"))
        | _, _ -> raise (Error (b.pexpr_loc, type_error_message Tbool b'.expr_typ))
      end

  | PEif (b, e1, e2) ->
      let e1', env1 = expr env e1
      and e2', env2 = expr env e2
      and b', _ = expr env b in
      begin match b'.expr_typ, e1'.expr_typ, e2'.expr_typ with
        | Tbool, Tmany [], Tmany [] -> TEif (b', e1', e2'), tvoid,
            {env with returns = env1.returns && env2.returns}
        | Tbool, _, _ -> raise (Error (loc, "syntax error"))
        | _, _, _ -> raise (Error (b.pexpr_loc, type_error_message Tbool b'.expr_typ))
      end

  | PEnil -> TEnil, Tptr tvoid, env
      
  | PEident {id} -> begin match Env.find_opt id env with
      | Some v -> v.v_used <- true; TEident v, v.v_typ, env
      | None -> raise (Error (loc, "unknown variable '" ^ id ^ "'"))
    end

  | PEdot (e, {id; loc=id_loc}) ->
      let e', _ = expr env e in begin match e'.expr_typ with
        | Tptr (Tstruct structure) | Tstruct structure ->
            begin match Hashtbl.find_opt structure.s_fields id with
              | Some field -> TEdot (e', field), field.f_typ, env
              | None -> raise (Error (id_loc, "structure '" ^ structure.s_name
                  ^ "' has no field named '" ^ id ^ "'"))
            end
        | _ -> raise (Error (e.pexpr_loc,
            "this is not a valid structure nor a pointer to a structure"))
      end

  | PEassign (lvl, el) ->
      let lvl', lvl_typ = expr_list env lvl
      and el', el_typ = expr_list env el in
      List.iter (fun e -> if not (lvalue_test e)
        then raise (Error (loc, "this is not an lvalue, it cannot be assigned a value"))) lvl';
      assignation_correspondance loc lvl_typ el_typ;
      TEassign (lvl', el'), tvoid, env

  | PEreturn el ->
      let el', el_typ = expr_list env el in
      if not (eq_type (Tmany el_typ) (Tmany env.return_values))
        then raise (Error (loc, type_error_message (Tmany env.return_values) (Tmany el_typ)))
        else TEreturn el', tvoid, {env with returns = true}

  | PEblock el ->
     (* TODO *) TEblock [], tvoid, env
  | PEincdec (e, op) ->
     (* TODO *) assert false
  | PEvars _ ->
     (* TODO *) assert false 

and expr_list env = function
  | [({pexpr_desc=PEcall _} as call)] ->
      let e, _ = expr env call in
      begin match e.expr_typ with
        | Tmany l -> [e], l
        | t -> [e], [t]
      end
  | l -> let l' = List.map
      (fun e -> let e', _ = expr env e in e', e'.expr_typ) l in
      List.split l'

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


(* returns a list of typed parameters for a given function, as a list of vars *)
let rec build_parameters structures f_name used_names = function
  | [] -> []
  | ({id; loc}, ptyp) :: q -> if List.mem id used_names
      then raise (Error (loc, "function '" ^ f_name ^
        "': redefinition of parameter '" ^ id ^ "'"))
      else begin
        let typ = find_type structures ptyp in
        let v = new_var id loc typ in
        v :: build_parameters structures f_name (id :: used_names) q
      end

(* type and add a list of fields to a given structure *)
let rec add_fields structure_context structure used_names = function
  | [] -> ()
  | ({id; loc}, ptyp) :: q -> if List.mem id used_names
      then raise (Error (loc, "structure '" ^ structure.s_name ^
        "': redefinition of field '" ^ id ^ "'"))
      else begin
        let typ = find_type structure_context ptyp in
        Hashtbl.add structure.s_fields id {f_name=id; f_typ=typ; f_ofs=0};
        add_fields structure_context structure (id :: used_names) q
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

  | PDstruct {ps_name = {id; _}; ps_fields} ->
      let s = M.find id structures in
      add_fields structures s [] ps_fields;
      functions


(* 3. type check function bodies *)
let decl structures functions = function
  | PDfunction { pf_name={id; loc}; pf_body = e; pf_typ=tyl } ->
    (* TODO check name and type *) 
    let f = { fn_name = id; fn_params = []; fn_typ = []} in
    let env = Env.new_env functions structures f in
    let e, env' = expr env e in
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
  let dl = List.map (decl structures functions) dl in
  Env.check_unused (); (* TODO variables non utilisees *)
  if imp && not !fmt_used then raise (Error (dummy_loc, "fmt imported but not used"));
  dl
