
open Format
open X86_64
open Tast


(* phase 1 : allocation des variables *)

exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

module Smap = Map.Make(String)

type local_env = int Smap.t

(*let rec alloc_expr (env: local_env) (fpcur: int) = function
  | TEConst c ->
    TEConst i, fpcur  

  | TEVar x ->
    (try LVar (Smap.find x env)
    with Not_found -> if Hashtbl.mem genv x then GVar x
      else raise (VarUndef x)), fpcur

  | PBinop (o, e1, e2) ->
    let ee1, fp1 = alloc_expr env fpcur e1 in
    let ee2, fp2 = alloc_expr env fpcur e2 in
    Binop (o, ee1, ee2), max fp1 fp2

  | PLetin (x, e1, e2) ->
    let d = -fpcur - 8 in
    let ee1, fp1 = alloc_expr env fpcur e1 in
    let ee2, fp2 = alloc_expr (Smap.add x d env) (fpcur + 8) e2 in
    Letin (d, ee1, ee2), max fp1 fp2

  | PCall (f, l) ->
    let ll, fpmax =
      List.fold_left
        (fun (ll, fpmax) e ->
          let ee, fpmax' = alloc_expr env fpcur e in
          ee::ll, max fpmax fpmax') ([], fpcur) l
    in
    Call (f, ll), fpmax

and alloc_stmt = function
  | PSet (x, e) ->
    Hashtbl.replace genv x ();
    let ee, fp = alloc_expr Smap.empty 0 e in
    Set (x, ee, fp)

  | PFun (f, l, e) ->
    let env, _ = List.fold_left
      (fun (env, f) x -> 
        let ff = f + 8 in
        Smap.add x ff env, ff) 
      (Smap.empty, 8) l in
    let ee, fp = alloc_expr env 0 e in
    Fun (f, ee, fp)
    
  | PPrint e ->
    let e, fpmax = alloc_expr Smap.empty 0 e in
    Print (e, fpmax)

let alloc = List.map alloc_stmt *)

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let mkbool r =
  pushq (imm 2) ++
  call "_malloc" ++
	popn 8 ++
  movb (imm 1) (ind rax) ++
  movb r (ind ~ofs:8 rax) 
let mkint r =
  pushq (imm 9) ++
  call "_malloc" ++
  popn 8 ++
  movb (imm 2) (ind rax) ++
  movq r (ind ~ofs:8 rax) 

let new_label =
  let x = ref 0 in
  fun () -> incr x;
    "_lab_" ^ string_of_int !x

let rec compile_expr = function
  | TEConst (CBoolean b) ->
    mkbool (imm (if b then 1 else 0)) ++
    pushq !%rax
  | TEConst (CNumber i) ->
    mkint (imm i) ++
    pushq !%rax
  | TEConst (CString s) ->
    let l = String.length s in
    pushq (imm (l + 2)) ++
    call "_malloc" ++
    popn 8 ++
    movb (imm 3) (ind rax) ++
    let t, _ = String.fold_left (fun (code, i) c -> code ++
      movb (imm (Char.code c)) (ind ~ofs:(8 * i) rax), i + 1) 
      (movb (imm 0) (ind ~ofs:(l + 1) rax), 1) s in
    t ++
    pushq !%rax

  | TECall ((TCVar v), l) -> 
    let v = if v = "num-modulo" then "_num_modulo" else v in
    List.fold_left (fun code e -> code ++ compile_expr e.e) nop l ++
    call v ++ popn (8 * List.length l) ++ pushq !%rax

  | TEOp (o, l)->
    let opcode = begin match o with
    | BEq -> 
      cmpq (ind rbx) (ind rax) ++
      sete !%cl ++
      mkbool !%cl
    | BNeq -> 
      cmpq (ind rbx) (ind rax) ++
      setne !%cl ++
      mkbool !%cl
    | BLt -> 
      movq (ind ~ofs:8 rax) !%rcx ++
      cmpq (ind ~ofs:8 rbx) !%rcx ++
      setl !%cl ++
      mkbool !%cl
    | BLeq -> 
      movq (ind ~ofs:8 rax) !%rcx ++
      cmpq (ind ~ofs:8 rbx) !%rcx ++
      setle !%cl ++
      mkbool !%cl
    | BGt -> 
      movq (ind ~ofs:8 rax) !%rcx ++
      cmpq (ind ~ofs:8 rbx) !%rcx ++
      setg !%cl ++
      mkbool !%cl
    | BGeq -> 
      movq (ind ~ofs:8 rax) !%rcx ++
      cmpq (ind ~ofs:8 rbx) !%rcx ++
      setge !%cl ++
      mkbool !%cl
    | BAdd ->
      movq (ind ~ofs:8 rax) !%rcx ++
      addq (ind ~ofs:8 rbx) !%rcx ++
      mkint !%rcx ++
      pushq !%rax
    | BSub ->
      movq (ind ~ofs:8 rax) !%rcx ++
      subq (ind ~ofs:8 rbx) !%rcx ++
      mkint !%rcx ++
      pushq !%rax
    | BMul ->
      imulq (ind ~ofs:8 rbx) (ind ~ofs:8 rax) ++
      movq !%rax !%rcx ++
      mkint !%rcx ++
      pushq !%rax
    | BDiv ->
      movq (ind ~ofs:8 rax) !%rax ++
      cqto ++ 
      idivq !%rbx ++
      movq !%rax !%rcx ++
      mkint !%rcx ++
      pushq !%rax
    | BAnd -> 
      testb (ind ~ofs:8 rbx) (ind ~ofs:8 rax) ++
      setne !%cl ++
      mkbool !%cl
    | BOr -> 
      movb (ind ~ofs:8 rax) !%r8b ++
      notb !%r8b ++
      movb (ind ~ofs:8 rbx) !%r9b ++
      notb !%r9b ++
      testb !%r8b !%r9b ++
      sete !%cl ++
      mkbool !%cl end in
    List.fold_left (fun acc e ->
      acc ++
      compile_expr e.e ++
      popq rbx ++
      opcode) 
      (compile_expr (List.hd l).e ++
        popq rax)
      (List.tl l) ++
    pushq !%rax

  | TEIf (l, eb) ->
    let endif = new_label () in
    List.fold_left (fun acc (c, cb) ->
      let clab = new_label () in
      acc ++
      compile_expr c.e ++
      popq rax ++
      testq (ind ~ofs:8 rax) (ind ~ofs:8 rax) ++
      je clab ++
      compile_block cb ++
      jmp endif ++
      label clab
      ) nop l ++
    compile_block eb ++
    label endif

  | TEBlock b ->
    compile_block b

  | _ -> failwith "todo" 
    (* LVar fp_x ->
      pushq (ind ~ofs:fp_x rbp)

  | GVar x ->
      pushq (lab x)


  | Letin (ofs, e1, e2) ->
      compile_expr e1 ++
      popq rax ++ movq !%rax (ind ~ofs rbp) ++
      compile_expr e2

  | Call (f, l) ->
    List.fold_left (fun code e -> code ++ compile_expr e) nop l ++
    call f ++ popn (8 * List.length l) ++ pushq !%rax *)

and compile_stmt = function
  | TSExpr e -> 
    compile_expr e.e
  | _ -> failwith "todo"
  (* | Set (x, e, fpmax) ->
    let code =
      pushn fpmax ++
      compile_expr e ++
      popq rax ++ movq !%rax (lab x) ++
      popn fpmax
    in
    codefun, codemain ++ code

  | Fun (f, e, fpmax) ->
    let code =
      label f ++
      pushq !%rbp ++
      movq !%rsp !%rbp ++ pushn fpmax ++
      compile_expr e ++ popq rax ++
      popn fpmax ++ popq rbp ++ ret
    in
    codefun ++ code, codemain

  | Print (e, fpmax) ->
    let code =
      pushn fpmax ++
      compile_expr e ++
      popq rdi ++
      popn fpmax ++
      call "print_number"
    in
    codefun, codemain ++ code *)
and compile_block b =
  List.fold_left (fun acc s -> 
    acc ++ 
    popq rax ++ 
    compile_stmt s) (pushq (imm 0)) b

let code p =
  (* let p = alloc p in 
  Format.eprintf "%a@." print p; *)
  let code = compile_block p in
  { 
    text =
      globl "main" ++ label "main" ++
      movq !%rsp !%rbp ++
      code ++
      popq rax ++
      movq (imm 0) !%rax ++ (* exit *)
      ret ++

      label "_malloc" ++
      pushq !%rbp ++
      movq !%rsp !%rbp ++
      andq (imm (-16)) !%rsp ++
      movq (ind ~ofs:16 rbp) !%rdi ++
      call "malloc" ++
      movq !%rbp !%rsp ++
      popq rbp ++
      ret ++

      label "_printf" ++
      pushq !%rbp ++
      movq !%rsp !%rbp ++
      andq (imm (-16)) !%rsp ++
      movq (ind ~ofs:24 rbp) !%rsi ++
      movq (ind ~ofs:16 rbp) !%rdi ++
      call "printf" ++
      movq !%rbp !%rsp ++
      popq rbp ++
      ret ++

      label "print" ++
      popq rsi ++
      pushq (ind ~ofs:8 rsi) ++
      movq !%rsi !%rax ++
      cmpb (imm 0) (ind rsi) ++
      je "print_nothing" ++
      cmpb (imm 1) (ind rsi) ++
      je "print_boolean" ++
      cmpb (imm 2) (ind rsi) ++
      je "print_number" ++
      cmpb (imm 3) (ind rsi) ++
      je "print_string" ++
      cmpb (imm 4) (ind rsi) ++
      je "print_empty" ++
      cmpb (imm 5) (ind rsi) ++
      je "print_link" ++
      cmpb (imm 6) (ind rsi) ++
      je "print_function" ++

      label "print_nothing" ++
      pushq (ilab ".Sprint_nothing") ++
      call "_printf" ++
      jmp "end_print" ++

      label "print_boolean" ++
      cmpq (imm 0) !%rdi ++
      je "print_false" ++
      pushq (ilab ".Sprint_true") ++
      jmp "end_print_bool" ++
      label "print_false" ++
      pushq (ilab ".Sprint_false") ++
      label "end_print_bool" ++
      call "_printf" ++
      jmp "end_print" ++

      label "print_number" ++
      movq !%rdi !%rsi ++
      pushq (ilab ".Sprint_number") ++
      call "_printf" ++
      jmp "end_print" ++

      label "print_string" ++
      movq !%rdi !%rsi ++
      pushq (ilab ".Sprint_string") ++
      call "_printf" ++
      jmp "end_print" ++
      
      label "print_empty" ++
      pushq (ilab ".Sprint_empty") ++
      call "_printf" ++
      jmp "end_print" ++

      label "print_link" ++
      pushq (ilab ".Sprint_link") ++
      call "_printf" ++
      jmp "end_print" ++

      label "print_function" ++
      pushq (ilab ".Sprint_function") ++
      call "_printf" ++
      jmp "end_print" ++

      label "end_print" ++
      addq (imm 8) !%rsp ++
      ret ++

      label "_num_modulo" ++
      popq rax ++
      popq rbx ++
      cqto ++ 
      idivq !%rbx ++
      movq !%rdx !%rax ++
      ret;
    data =
      label ".Sprint_number" ++ string "%d\n" ++
      label ".Sprint_string" ++ string "%s\n" ++
      label ".Sprint_nothing" ++ string "nothing\n" ++
      label ".Sprint_true" ++ string "true\n" ++
      label ".Sprint_false" ++ string "false\n" ++
      label ".Sprint_empty" ++ string "empty\n" ++
      label ".Sprint_link" ++ string "link\n" ++
      label ".Sprint_function" ++ string "function\n" ++
      Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv nop
  }