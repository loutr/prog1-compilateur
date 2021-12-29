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

let malloc n = movq (imm n) (reg rdi) ++ call "malloc"
let allocz n = movq (imm n) (reg rdi) ++ call "allocz"

let sizeof = Typing.sizeof

let new_label =
  let r = ref 0 in fun () -> incr r; "L_" ^ string_of_int !r

type env = {
  exit_label: string;
  ofs_this: int;
  mutable nb_locals: int; (* maximum *)
  next_local: int; (* 0, 1, ... *)
}

let empty_env =
  { exit_label = ""; ofs_this = -1; nb_locals = 0; next_local = 0 }

let mk_bool d = { expr_desc = d; expr_typ = Tbool }

(* f reçoit le label correspondant à ``renvoyer vrai'' *)
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

(* associates a *l-value* to its runtime address *)
let rec expr_address {expr_desc=desc; expr_typ=typ} = match desc, typ with
  | TEident v, (Tstruct _ | Tstring) ->
      movq (ind ~ofs:(8 * v.v_addr) rbp) (reg rax)
  | TEident v, _ -> leaq (ind ~ofs:(8 * v.v_addr) rbp) rax
  | TEdot (e, f), _ -> expr_address e ++ addq (imm f.f_ofs) (reg rax)
  | TEunop (Ustar, e), _ -> expr_address e ++ movq (ind rax) (reg rax)
  | _ -> raise (Anomaly "trying to find the runtime address of something\
      which is not an l-value")


let rec expr env e = match e.expr_desc with
  | TEskip -> nop
  | TEconstant (Cbool true) -> movq (imm 1) (reg rax)
  | TEconstant (Cbool false) -> movq (imm 0) (reg rax)
  | TEconstant (Cint x) -> movq (imm64 x) (reg rax)
  | TEnil -> xorq (reg rax) (reg rax)
  | TEconstant (Cstring s) ->
      let label = alloc_string s in
      movq (lab label) (reg rax)

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
      movq (reg ((function Bdiv -> rdx | _ -> rax) op)) (reg rax)

  | TEbinop (Beq | Bne as op, e1, e2) ->
      expr env e1 ++ pushq (reg rax) ++
      expr env e2 ++ compare e1.expr_typ (reg rax) (ind rsp) ++
      popq rcx

  | TEunop (Uneg, e1) -> expr env e1 ++ negq (reg rax)
  | TEunop (Unot, e1) -> expr env e1 ++ notq (reg rax)

  | TEunop (Uamp, e1) -> expr_address e1 

  | TEunop (Ustar, e1) ->
    (* TODO code pour * *) assert false 
  | TEprint el ->
    (* TODO code pour Print *) assert false 
  | TEident x ->
    (* TODO code pour x *) assert false 
  | TEassign (lvl, el) ->
     assert false
  | TEblock el ->
     (* TODO code pour block *) assert false
  | TEif (e1, e2, e3) ->
     (* TODO code pour if *) assert false
  | TEfor (e1, e2) ->
     (* TODO code pour for *) assert false
  | TEnew ty ->
     (* TODO code pour new S *) assert false
  | TEcall (f, el) ->
     (* TODO code pour appel fonction *) assert false
  | TEdot (e1, {f_ofs=ofs}) ->
     (* TODO code pour e.f *) assert false
  | TEvars _ ->
     assert false (* fait dans block *)
  | TEreturn [] ->
    (* TODO code pour return e *) assert false
  | TEreturn [e1] ->
    (* TODO code pour return e1,... *) assert false
  | TEreturn _ ->
     assert false
  | TEincdec (e1, op) ->
    (* TODO code pour return e++, e-- *) assert false

let function_ f e =
  if !debug then eprintf "function %s:@." f.fn_name;
  (* TODO code pour fonction *) let s = f.fn_name in label ("F_" ^ s) 

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code

let file ?debug:(b=false) dl =
  debug := b;
  (* TODO calcul offset champs *)
  (* TODO code fonctions *) let funs = List.fold_left decl nop dl in
  { text =
      globl "main" ++ label "main" ++
      call "F_main" ++
      xorq (reg rax) (reg rax) ++
      ret ++
      funs ++
      inline "
print_int:
        movq    %rdi, %rsi
        movq    $S_int, %rdi
        xorq    %rax, %rax
        call    printf
        ret
"; (* TODO print pour d'autres valeurs *)
   (* TODO appel malloc de stdlib *)
    data =
      label "S_int" ++ string "%ld" ++
      (Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings nop)
    ;
  }
