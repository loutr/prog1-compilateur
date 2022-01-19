(* étiquettes
     F_function      entrée fonction
     E_function      sortie fonction
     L_xxx           sauts
     S_xxx           chaîne

   expression calculée avec la pile si besoin, résultat final dans %rdi

   fonction : arguments sur la pile, résultat dans %rax ou sur la pile

            res k
            ...
            res 1
            arg n
            ...
            arg 1
            adr. retour
   rbp ---> ancien rbp
            ...
            var locales
            ...
            calculs
   rsp ---> ...

*)

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
let rec add_print_capability typ = if not (Hashtbl.mem print_functions typ) then
  Hashtbl.add print_functions typ (match typ with
    | Tstring -> let s_string = alloc_string "%s" in
        label "P_string" ++ movq (ilab s_string) (reg rdi) ++ printf_frame

    | Tint -> let s_int = alloc_string "%ld" in
        label "P_int" ++ movq (ilab s_int) (reg rdi) ++ printf_frame

    | Tbool -> add_print_capability Tstring;
        let true_s = alloc_string "true" and false_s = alloc_string "false"
        and l = new_label () and l' = new_label () in
        label "P_bool" ++ cmpq (imm 0) (reg rsi) ++ je l
        ++ movq (ilab true_s) (reg rsi) ++ jmp l'
        ++ label l ++ movq (ilab false_s) (reg rsi)
        ++ label l' ++ call "P_string" ++ ret

    | Tstruct s -> add_print_capability Tstring;
        let ocb_s = alloc_string "{" and ccb_s = alloc_string "}" in
        label ("P_" ^ s.s_name) ++ movq (reg rsi) (reg r10)
        ++ movq (ilab ocb_s) (reg rsi) ++ call "P_string" ++
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
        label "P_ptr" ++ cmpq (imm 0) (reg rsi) ++ jne l
        ++ movq (ilab nil_s) (reg rsi) ++ call "P_string" ++ ret
        ++ label l ++ movq (ilab s_addr) (reg rdi) ++ printf_frame

    | _ -> raise (Anomaly "could not build print function for the given type")
  )

let malloc n = movq (imm n) (reg rdi) ++ call "malloc"
let sizeof = Typing.sizeof

(* type proposé dans le sujet
type env = {
  exit_label: string;
  ofs_this: int;
  mutable nb_locals: int; (* maximum *)
  next_local: int; (* 0, 1, ... *)
}
*)

type env = {
  ofs_this: int;
  mutable nb_locals: int;
}

let addv env = env.nb_locals <- env.nb_locals + 1

let empty_env nargs =
  { nb_locals = 0; ofs_this = nargs }

(* f reçoit le label correspondant à ``renvoyer vrai'' *)
(* what is the purpose of this function? *)
let compile_bool f =
  let l_true = new_label () and l_end = new_label () in
  f l_true ++
  movq (imm 0) (reg rdi) ++ jmp l_end ++
  label l_true ++ movq (imm 1) (reg rdi) ++ label l_end

(* code to compare values. One of them must be
   within a register (cmpq cannot operate on two
   non-register values) *)
let compare typ val1 val2 = match typ with
  | Tstruct s ->
      leaq val1 rsi ++ leaq val2 rdi ++
      movl (imm s.s_size) (reg edx) ++ call "memcp"
  | Tstring ->
      leaq val1 rsi ++ leaq val2 rdi ++
      call "strcmp"
  | _ -> cmpq val1 val2 ++ sete (reg al) ++ movzbq (reg al) rax


(* code to print a list of expressions *)
(* 2 specific behaviours to match Go's:
  * strings do not yield surrounding spaces, other types do
  * pointers to structures are of the form &<print result of the associated structure>
*)
(* NOTE this version of the function does not handle functions with multiple return values *)
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
      
      

(* associates a *l-value* to its runtime address *)
and expr_address env {expr_desc=desc; expr_typ=typ} = match desc, typ with
  | TEident v, (Tstruct _ | Tstring) ->
      movq (ind ~ofs:(8 * v.v_addr) rbp) (reg rax)
  | TEident v, _ -> leaq (ind ~ofs:(8 * v.v_addr) rbp) rax
  | TEdot (e, f), _ -> expr_address env e ++ addq (imm f.f_ofs) (reg rax)
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
      expr env e1 ++ pushq (reg rax) ++
      expr env e2 ++
      (if op = Band then andq else orq)
        (ind rsp) (reg rax) ++
      popq rcx

  | TEbinop (Blt | Ble | Bgt | Bge as op, e1, e2) ->
      expr env e1 ++ pushq (reg rax) ++
      expr env e2 ++
      cmpq (reg rax) (ind rsp) ++
      (function
        | Blt -> setl | Ble -> setle
        | Bgt -> setg | _ -> setge) op
        (reg al) ++ movzbq (reg al) rax ++
      popq rcx
      
  | TEbinop (Badd | Bsub | Bmul as op, e1, e2) ->
      expr env e2 ++ pushq (reg rax) ++
      expr env e1 ++ 
      (function
        | Badd -> addq | Bsub -> subq
        | _ -> imulq) op (ind rsp) (reg rax) ++
      popq rcx

  | TEbinop (Bdiv | Bmod as op, e1, e2) ->
      expr env e1 ++ pushq (reg rax) ++
      expr env e2 ++ movq (reg rax) (reg rax) ++
      cqto ++ idivq (ind rsp) ++
      movq (reg (if Bdiv = op then rdx else rax)) (reg rax)

  | TEbinop (Beq | Bne as op, e1, e2) ->
      expr env e1 ++ pushq (reg rax) ++
      expr env e2 ++ movq (reg rax) (reg rbx) ++
      compare e1.expr_typ (reg rbx) (ind rsp) ++
      (if op = Beq then nop else notq (reg rax) ++ andq (imm 1) (reg rax)) ++
      popq rcx

  | TEunop (Uneg, e1) -> expr env e1 ++ negq (reg rax)
  | TEunop (Unot, e1) -> expr env e1 ++ notq (reg rax)

  | TEunop (Uamp, e1) -> expr_address env e1 

  | TEunop (Ustar, e1) ->
      expr env e1 ++ (match e1.expr_typ with
        | Tptr (Tstruct _ | Tstring) -> nop
        | _ -> movq (ind rax) (reg rax)
      )

  | TEprint el -> add_print_capability Tstring;
      expr_print false env el
      
  | TEident x ->
    (* TODO code pour x *) assert false 
  | TEassign (lvl, el) ->
     assert false
  | TEblock el ->  (* NOTE temporary *)
      expr_list env el
  | TEif (e1, e2, e3) ->
     (* TODO code pour if *) assert false
  | TEfor (e1, e2) ->
     (* TODO code pour for *) assert false
  | TEnew ty ->
     (* TODO code pour new S *) assert false
  | TEcall (f, el) ->
      subq (imm (List.length f.fn_typ)) (reg rsp) ++
      expr_list env el ++ call ("F_" ^ f.fn_name)
  | TEdot (e1, {f_ofs=ofs}) ->
     (* TODO code pour e.f *) assert false
  | TEvars _ ->
     assert false (* fait dans block *)
  | TEreturn el ->
    (* TODO code pour return e *) assert false
  | TEincdec (e1, op) ->
    (* TODO code pour return e++, e-- *) assert false
    (* NOTE si on pouvait s'attendre facilement à ce que ce soit une variable sur la pile,
       on pourrait faire directement opquiincremente (%registreapproprié)
       mais la l-value opérande peut avoir une téte bizarre, qu'il est nécessaire de
       déterminer en plusieurs étapes (ou alors il faut sérieusement optimiser la chose) *)

(* evaluates a list of expression and puts them successively on the stack (in reverse order).
   For instance, it is used to prepare function arguments or variable assignation. *)
and expr_list env = function
  | [] -> nop
  | e :: el' -> expr env e ++ expr_list env el' 

and expr_list_rev env = function
  | [] -> nop
  | e :: el' -> expr_list env el' ++ expr env e

let function_ f e =
  if !debug then eprintf "function %s:@." f.fn_name;
  let env = empty_env (List.length f.fn_params) in
  label ("F_" ^ f.fn_name) ++ expr env e

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e ++ ret
  | TDstruct _ -> code

let file ?debug:(b=false) dl =
  debug := b;
  let funs = List.fold_left decl nop dl in
  let print_funs = Hashtbl.fold (fun _ s d -> s ++ d) print_functions nop in
  { text =
      globl "main" ++ label "main" ++
      call "F_main" ++
      xorq (reg rax) (reg rax) ++
      ret ++
      funs ++ print_funs
; (* TODO print pour d'autres valeurs *)
   (* TODO appel malloc de stdlib *)
    data =
      (Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings nop)
    ;
  }
