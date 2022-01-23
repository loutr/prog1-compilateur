open Format
open Ast
open Tast
open X86_64

exception Anomaly of string

let debug = ref false

let strings = Hashtbl.create 32
let alloc_string =
  let r = ref 0 in
  fun s ->
    incr r;
    let l = "S_" ^ string_of_int !r in
    Hashtbl.add strings l s;
    l

let new_label =
  let r = ref 0 in fun () -> incr r; "L_" ^ string_of_int !r

let ampersand_s = alloc_string "&"
let space_s = alloc_string " "

let printf_frame = inline
"        pushq %rbp
        movq %rsp, %rbp
        andb $0xF0, %spl
        xorq %rax, %rax
        call printf
        leave
        ret
"

let print_correspondance = function
  | Tstring -> "P_string"
  | Tbool -> "P_bool"
  | Tint -> "P_int"
  | Tptr _ | Tptrnil -> "P_ptr"
  | Tstruct s -> "P_" ^ s.s_name
  | _ -> raise (Anomaly "could not yield print function for the given type")

(* Builds the print function for a given type. *)
(* the functions expect their arguments in %rsi *)
let print_functions = Hashtbl.create 5
let rec add_print_capability typ = if not (Hashtbl.mem print_functions (print_correspondance typ)) then
  Hashtbl.add print_functions (print_correspondance typ) (match typ with
    | Tstring -> let s_string = alloc_string "%s" in
        movq (ilab s_string) (reg rdi) ++ printf_frame

    | Tint -> let s_int = alloc_string "%ld" in
        movq (ilab s_int) (reg rdi) ++ printf_frame

    | Tbool -> add_print_capability Tstring;
        let true_s = alloc_string "true" and false_s = alloc_string "false"
        and l = new_label () and l' = new_label () in
        cmpq (imm 0) (reg rsi) ++ je l
        ++ movq (ilab true_s) (reg rsi) ++ jmp l'
        ++ label l ++ movq (ilab false_s) (reg rsi)
        ++ label l' ++ call "P_string" ++ ret

    | Tstruct s -> add_print_capability Tstring;
        let ocb_s = alloc_string "{" and ccb_s = alloc_string "}" in
        movq (reg rsi) (reg r10) ++ movq (ilab ocb_s) (reg rsi) ++ call "P_string" ++
        (Hashtbl.fold (fun _ f d -> add_print_capability f.f_typ;
          (match f.f_typ with
            | Tbool -> movq (ind ~ofs:f.f_ofs r10) (reg rsi) ++ call "P_bool"
            | Tint -> movq (ind ~ofs:f.f_ofs r10) (reg rsi) ++ call "P_int"
            | Tptr _ | Tptrnil -> movq (ind ~ofs:f.f_ofs r10) (reg rsi) ++ call "P_ptr"
            | Tstring -> leaq (ind ~ofs:f.f_ofs r10) rsi ++ call "P_string"
            | Tstruct s -> leaq (ind ~ofs:f.f_ofs r10) rsi ++ call ("P_" ^ s.s_name)
            | _ -> raise (Anomaly "could not yield print function for the given type")
          ) ++
          if d = nop then nop else movq (ilab space_s) (reg rsi) ++ call "P_string"
        ) s.s_fields nop)
        ++ movq (ilab ccb_s) (reg rsi) ++ call "P_string" ++ ret

    | Tptr _ | Tptrnil -> add_print_capability Tstring;
        let nil_s = alloc_string "<nil>" and s_addr = alloc_string "0x%08x"
        and l = new_label () in
        cmpq (imm 0) (reg rsi) ++ jne l
        ++ movq (ilab nil_s) (reg rsi) ++ call "P_string" ++ ret
        ++ label l ++ movq (ilab s_addr) (reg rdi) ++ printf_frame

    | _ -> raise (Anomaly "could not build print function for the given type")
  )

let malloc n = movq (imm n) (reg rdi) ++ call "malloc"
let sizeof = Typing.sizeof

type env = {
  ofs_this: int;
  mutable nb_locals: int;
}

let addv env = env.nb_locals <- env.nb_locals + 1
let copy { ofs_this; nb_locals } = { ofs_this; nb_locals }
let empty_env nargs = { nb_locals = 0; ofs_this = nargs }


(* code to compare values. One of them must be
   within a register (cmpq cannot operate on two
   non-register values) *)
let compare typ val1 val2 = match typ with
  | Tstruct s ->
      leaq val1 rsi ++ leaq val2 rdi ++
      movq (imm s.s_size) (reg rdx) ++ call "memcp"
  | Tstring ->
      leaq val1 rsi ++ leaq val2 rdi ++
      call "strcmp"
  | _ -> cmpq val1 val2 ++ sete (reg al) ++ movzbq (reg al) rax


(* code to print a list of expressions provided in order on the stack *)
(* 2 specific behaviours to match Go's:
  * strings do not yield surrounding spaces, other types do
  * pointers to structures are of the form &<print result of the associated structure>
*)
(* NOTE this version of the function does not handle functions *)
(* IDEA: separating part of expr_stack which does stack substraction as a separate function,
   to be used here. Justification: this is the only "ad-hoc" "function" which generates command
   on the fly based on parameter list *)
let rec expr_print left_spacing env = function
  | [] -> nop
  | ({ expr_typ = Tstring } as e) :: el -> expr env e
      ++ movq (reg rax) (reg rsi) ++ call "P_string"
      ++ expr_print false env el
  | e :: el -> add_print_capability e.expr_typ;
      (if left_spacing then movq (ilab space_s) (reg rsi) ++ call "P_string" else nop)
      ++ expr env e
      ++ movq (reg rax) (reg rsi) ++ call (print_correspondance e.expr_typ) ++
      expr_print true env el
      

(* associates an *l-value* to its runtime address *)
and expr_address env { expr_desc=desc; expr_typ=typ } = match desc, typ with
  | TEident v, (Tstruct _ | Tstring) ->
      movq (ind ~ofs:v.v_addr rbp) (reg rax)
  | TEident v, _ -> leaq (ind ~ofs:v.v_addr rbp) rax
  | TEdot (e, f), _ -> begin match e.expr_typ with
      | Tptr _ -> expr env e
      | _ -> expr_address env e
    end ++ addq (imm f.f_ofs) (reg rax)
  | TEunop (Ustar, e), _ -> expr env e
  | _ -> raise (Anomaly "trying to find the runtime address of something\
      which is not an l-value")


and expr env e = match e.expr_desc with
  | TEskip -> nop
  | TEconstant (Cbool true) -> movq (imm 1) (reg rax)
  | TEconstant (Cbool false) -> movq (imm 0) (reg rax)
  | TEconstant (Cint x) -> movq (imm64 x) (reg rax)
  | TEnil -> xorq (reg rax) (reg rax)
  | TEconstant (Cstring s) ->
      let label = alloc_string s in
      movq (ilab label) (reg rax)

  | TEbinop (Band | Bor as op, e1, e2) ->
      expr env e1 ++ pushq (reg rax) ++ expr env e2 ++
      (if op = Band then andq else orq)
        (ind rsp) (reg rax) ++
      popq rcx

  | TEbinop (Blt | Ble | Bgt | Bge as op, e1, e2) ->
      expr env e1 ++ pushq (reg rax) ++ expr env e2 ++
      cmpq (reg rax) (ind rsp) ++
      (function
        | Blt -> setl | Ble -> setle
        | Bgt -> setg | _ -> setge) op
        (reg al) ++ movzbq (reg al) rax ++
      popq rcx
      
  | TEbinop (Badd | Bsub | Bmul as op, e1, e2) ->
      expr env e2 ++ pushq (reg rax) ++ expr env e1 ++ 
      (function
        | Badd -> addq | Bsub -> subq
        | _ -> imulq) op (ind rsp) (reg rax) ++
      popq rcx

  | TEbinop (Bdiv | Bmod as op, e1, e2) ->
      expr env e1 ++ pushq (reg rax) ++ expr env e2 ++ movq (reg rax) (reg rax) ++
      cqto ++ idivq (ind rsp) ++
      movq (reg (if Bdiv = op then rdx else rax)) (reg rax)

  | TEbinop (Beq | Bne as op, e1, e2) ->
      expr env e1 ++ pushq (reg rax) ++
      expr env e2 ++ movq (reg rax) (reg rbx) ++
      compare e1.expr_typ (reg rbx) (ind rsp) ++
      (if op = Beq then nop else notq (reg rax) ++ andq (imm 1) (reg rax)) ++
      popq rcx

  | TEunop (Uneg, e1) -> expr env e1 ++ negq (reg rax)
  | TEunop (Unot, e1) -> expr env e1 ++ notq (reg rax) ++ andq (imm 1) (reg rax)

  | TEunop (Uamp, e1) -> expr_address env e1 

  | TEunop (Ustar, e1) ->
      expr env e1 ++ (match e1.expr_typ with
        | Tptr (Tstruct _) -> nop (* NOTE should string* be considered there? I don't think so *)
        | _ -> movq (ind rax) (reg rax)
      )

  | TEprint el -> add_print_capability Tstring; expr_print false env el
      
  | TEident x -> movq (ind ~ofs:x.v_addr rbp) (reg rax)

  | TEassign (lvl, el) -> expr_stack env el ++ List.fold_left (fun d lv -> (match lv.expr_typ with
        | Tstruct s -> expr_address env lv  ++ popq rsi ++ movq (reg rax) (reg rdi) ++
            movq (imm s.s_size) (reg rdx) ++ call "memcpy"
        | Twild -> popq rsi
        | _ -> expr_address env lv  ++ popq rsi ++ movq (reg rsi) (ind rax)
      ) ++ d)
        nop lvl
      (* NOTE what about using free to remove useless temporary structures ? *)
      
  | TEblock el -> let env' = copy env and nb_glob = env.nb_locals in
      let t1 = List.fold_left (++) nop (List.map (expr env') el) in
      t1 ++ (* haha! evaluation order is not left-to-right! *)
      (if env'.nb_locals > nb_glob
        then addq (imm ((env'.nb_locals - nb_glob) * 8)) (reg rsp)
        else nop)

  | TEif (b, e1, e2) -> let l_end = new_label () and l_false = new_label () in
      expr env b ++ cmpq (imm 0) (reg rax) ++ je l_false ++
      expr env e1 ++ jmp l_end ++
      label l_false ++ expr env e2 ++ label l_end

  | TEfor (b, e) -> let l_cond = new_label () and l_begin = new_label () in
      jmp l_cond ++ label l_begin ++ expr env e ++
      label l_cond ++ expr env b ++ cmpq (imm 1) (reg rax) ++ je l_begin

  | TEnew ty -> malloc (sizeof ty) 

  | TEcall (f, el) -> ev_fun env (f, el) ++ begin match (List.length f.fn_typ) with
        | 0 -> nop
        | 1 -> popq rax
        | _ -> raise (Anomaly "trying to evaluate invalid function within an expression")
      end

  | TEdot _ -> expr_address env e ++ (match e.expr_typ with
        | Tstruct _ -> nop
        | _ -> movq (ind rax) (reg rax)
      )

  | TEvars (varlist, initlist) ->
      List.iter (fun var -> addv env; var.v_addr <- -8 * env.nb_locals) varlist;
      expr_stack env (List.rev initlist)

  | TEreturn el -> 
      (* NOTE this is incorrect when the evaluations have side-effects, because the elements
      are evaluated in reverse order, which does not match the semantics *)
      expr_stack env el ++ (List.fold_left (++) nop (List.mapi (fun i _ ->
        popq rax ++ movq (reg rax) (ind ~ofs:(env.ofs_this + 8 * i) rbp)) el)) ++ leave ++ ret

  | TEincdec (e1, op) -> expr_address env e1 ++
      (if op = Inc then incq else decq) (ind rax)
    (* NOTE optimisations are possible when e1 is more specific, like TEident.
       In that case, a command like incq -16(%rbp). This has not been done here
       and is beyond the scope of this project. *)


(* evaluates a list of expressions and put the results on the stack in reverse order of appearance.
   This function is used within TEassign, TEvars and function calls *)
and expr_stack env = function
  | [] -> nop
  | { expr_desc = TEcall (f, arglist) } :: el' -> 
      expr_stack env el' ++ ev_fun env (f, arglist)
  | e :: el' -> expr_stack env el' ++ expr env e ++ (match e.expr_typ with
      | Tstruct s -> (* structures need to be copied when evaluated, as to allow x, y = y, x *)
          movq (reg rax) (reg r10) ++ malloc (s.s_size) ++ 
          movq (reg rax) (reg rdi) ++ movq (reg r10) (reg rsi) ++
          movq (imm s.s_size) (reg rdx) ++ call "memcpy"
      | _ -> nop) ++ pushq (reg rax)

(* calls a function and leave the results on the stack *)
and ev_fun env (f, arglist) =
  subq (imm (8 * (List.length f.fn_typ))) (reg rsp) ++
  expr_stack env arglist ++ call ("F_" ^ f.fn_name) ++
  (* function arguments must be removed from the stack once the call is done *)
  (if arglist = [] then nop else addq (imm (8 * (List.length arglist))) (reg rsp))




let function_ f e =
  if !debug then eprintf "function %s:@." f.fn_name;
  (* why 2? both return address and previous %rbp value are on the stack *)
  let env = empty_env (8 * (2 + (List.length f.fn_params))) in
  label ("F_" ^ f.fn_name) ++ pushq (reg rbp) ++ movq (reg rsp) (reg rbp) ++
  expr env e ++ (if f.fn_typ = [] then leave ++ ret else nop)

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code

let file ?debug:(b=false) dl =
  debug := b;
  let funs = List.fold_left decl nop dl in
  let print_funs = Hashtbl.fold (fun l s d -> label l ++ s ++ d) print_functions nop in
  { text =
      globl "main" ++ label "main" ++
      call "F_main" ++
      xorq (reg rax) (reg rax) ++
      ret ++
      funs ++ print_funs
    ;
    data = (Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings nop)
    ;
  }
