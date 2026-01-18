
open Format
open X86_64
open Tast
open Code

exception VarUndef of string
module SMap = Map.Make(String)
module VSet = Set.Make(String)

type vartype =  Local | Clos | Arg
type block = stmt list
and expr =
| AEConst of Ast.const
| AEBlock of block
| AEOp of Ast.binop * expr list
| AEIf of (expr * block) list * block
| AEVar of vartype * int
| AECall of expr * expr list
| AELam of clos * block
| AECases of expr * expr * expr
and stmt = 
| ASExpr of expr
| ASDeclC of expr
| ASDeclV of expr
| ASAssign of vartype * int * expr

let rec alloc_expr (env: local_env) (fpcur: int) = function
| TEConst c ->
  AEConst c, fpcur
| TEBlock b -> 
  let nenv = {
    locals = SMap.empty; 
    args = SMap.empty; 
    oldlocals = SMap.union (fun _ n _ -> Some n) 
      (SMap.map (fun v -> v + 8 * (1 + SMap.cardinal env.locals)) env.oldlocals) env.locals;
    captured = env.captured} in
  let ab, fp = alloc_block nenv fpcur b in
  AEBlock ab, fp
| TEOp (BAdd, ({ e=_; t=TTString } as e)::a::l) -> 
  alloc_expr env fpcur (
    TECall (TCVar "strconcat", [e; { e=TEOp (BAdd, a::l); t = TTString }]))
| TEOp (BEq, ({ e=_; t=TTList _ } as e)::a::l) -> 
  alloc_expr env fpcur (
    TECall (TCVar "list_eq", [e; { e=TEOp (BEq, a::l); t = TTList TTAny }]))
| TEOp (BNeq, ({ e=_; t=TTList _ } as e)::a::l) -> 
  alloc_expr env fpcur (
    TECall (TCVar "list_neq", [e; { e=TEOp (BNeq, a::l); t = TTList TTAny }]))
| TEOp (o, l) -> 
  let ll = List.map (fun e -> alloc_expr env fpcur e.e) l in
  AEOp (o, List.map fst ll), List.fold_left max fpcur (List.map snd ll)
| TEIf (l, eb) -> 
  let ll = List.map (fun (c, cb) -> 
    let c, fpc = alloc_expr env fpcur c.e in
    let cb, fpcb = alloc_block env fpcur cb in
    (c, cb), max fpc fpcb) l in
  let eb, fpe = alloc_block env fpcur eb in
  AEIf (List.map fst ll, eb), List.fold_left max fpe (List.map snd ll)
| TEVar x ->
  (try AEVar (Local, SMap.find x env.locals) 
  with Not_found -> 
    (try AEVar (Arg, SMap.find x env.args)
    with Not_found -> 
      (try AEVar (Clos, fst (Hashtbl.find env.captured x))
      with Not_found -> 
        let i = Hashtbl.length env.captured in
        Hashtbl.add env.captured x (9 + 8 * i, SMap.find x env.oldlocals);
        AEVar (Clos, 9 + 8 * i)))), fpcur
| TELam (p, b) -> 
  let a, _ = List.fold_left (fun (m, i) (x, _) -> (SMap.add x (8*i + 16) m, i + 1)) (SMap.empty, 1) p in
  let fsize = 8 * (SMap.cardinal a + 3 + SMap.cardinal env.locals) in
  let nenv = {
    locals = SMap.empty; 
    args = a; 
    oldlocals = SMap.union (fun _ n _ -> Some n) env.locals 
    (SMap.map (fun v -> v - fsize) 
      (SMap.union (fun _ n _ -> Some n) 
        env.oldlocals 
        env.args));
    captured = Hashtbl.create 17} in
  let b, fp = alloc_block nenv fpcur b in
  Hashtbl.iter (fun k v -> if not (SMap.mem k env.locals || SMap.mem k env.args) then 
    Hashtbl.add env.captured k v) nenv.captured;
  let clos = { fp = fp; captured = nenv.captured; frame_size = fsize } in
  AELam (clos, b), fpcur
| TECall (TCVar v, l) -> 
  let v = if v = "num-modulo" then "_num_modulo" else v in
  let l = List.mapi (fun i e -> alloc_expr env (fpcur + i) e.e) l in
  let x, _ = alloc_expr env fpcur (TEVar v) in
  AECall (x, List.map fst l), List.fold_left max fpcur (List.map snd l)
| TECases (e,  ["empty", [], eb; "link", [x; y], lb])
| TECases (e,  ["link", [x; y], lb; "empty", [], eb]) -> 
  let e, fp = alloc_expr env fpcur e.e in
  let ef, fpe = alloc_expr env fp (TELam ([], eb)) in
  let lf, fpl = alloc_expr env fpe (TELam ([x, TTAny; y, TTAny], lb)) in
  AECases(e, AECall (ef, []), AECall (lf, [])), fpl
| _ -> failwith "TODO"
and alloc_block env fpcur = function 
| [] -> [], fpcur
| s::ss -> 
  begin match s with 
  | TSExpr e ->
    let e, fp = alloc_expr env fpcur e.e in
    let ss, fpss = alloc_block env fp ss in
    ASExpr e::ss, fpss
  | TSDecl (b, v, e) ->
    let addr = SMap.fold (fun _ i a -> min i a) env.locals 0 - 8 in
    let env = { env with locals = SMap.add v addr env.locals} in
    let e, fpe = alloc_expr env (fpcur + 1) e.e in
    let ss, fpss = alloc_block env fpe ss in
    (if b then ASDeclV e else ASDeclC e)::ss, fpss
  | TSFun (f, _, l) -> 
    alloc_block env fpcur (TSDecl(false, f, { e=TELam l; t=TTAny })::ss)
  | TSAssign (x, e) -> 
    let vartype, addr = (try Local, SMap.find x env.locals
      with Not_found -> try Arg, SMap.find x env.args
        with Not_found -> Clos, try fst (Hashtbl.find env.captured x)
          with Not_found -> 
            let i = Hashtbl.length env.captured in
            Hashtbl.add env.captured x (i, SMap.find x env.oldlocals);
            9 + 8*i) in
    let e, fp = alloc_expr env fpcur e.e in
    let ss, fpss = alloc_block env fp ss in
    ASAssign (vartype, addr, e)::ss, fpss end

let codefun = ref nop 
let rec compile_expr = function
| AEConst (CBoolean b) ->
  mkbool (imm (if b then 1 else 0)) ++
  pushq !%rax
| AEConst (CNumber i) ->
  mkint (imm i) ++
  pushq !%rax
| AEConst (CString s) ->
  let l = String.length s in
  pushq (imm (l + 2)) ++
  call "_malloc" ++
  popn 8 ++
  movb (imm 3) (ind rax) ++
  let t, _ = String.fold_left (fun (code, i) c -> code ++
    movb (imm (Char.code c)) (ind ~ofs:i rax), i + 1) 
    (movb (imm 0) (ind ~ofs:(l + 1) rax), 1) (Scanf.unescaped s) in
  t ++
  pushq !%rax

| AEVar (Clos, i) ->
  movq (ind ~ofs:16 rbp) !%rcx ++
  movq (ind ~ofs:i rcx) !%rax ++
  readptr_rax () ++
  pushq !%rax
| AEVar (_, i) ->
  movq (ind ~ofs:i rbp) !%rax ++
  readptr_rax () ++
  pushq !%rax

| AECall (f, l) ->
  List.fold_left (fun code e -> compile_expr e ++ code) nop l ++
  compile_expr f ++
  movq (ind rsp) !%rcx ++
  call_star (ind ~ofs:1 rcx) ++
  popn (8 * List.length l + 8) ++ 
  pushq !%rax

| AEOp (o, l)->
  let errfunc = 
    let end_err = new_label () in
    movb (ind rbx) !%r14b ++
    cmpb (imm 6) !%r14b ++
    sete !%r13b ++
    movb (ind rax) !%r14b ++
    cmpb (imm 6) !%r14b ++
    sete !%r12b ++
    testb !%r12b !%r13b ++ 
    je end_err ++
    compile_expr (AEConst (CString ("can't compare functions"))) ++
    pushq (ilab "raise") ++
    call_star (ind rsp) ++
    label end_err in
  let getb e = 
    compile_expr e ++
    popq rbx ++
    popq rax in
  let opcode e = begin match o with
  | BEq -> 
    getb e ++
    errfunc ++
    movq (ind rax) !%rcx ++
    cmpq (ind rbx) !%rcx ++
    sete !%r14b ++
    mkbool !%r14b
  | BNeq -> 
    getb e ++
    errfunc ++
    movq (ind rax) !%rcx ++
    cmpq (ind rbx) !%rcx ++
    setne !%r14b ++
    mkbool !%r14b
  | BLt -> 
    getb e ++
    movq (ind ~ofs:1 rax) !%r14 ++
    cmpq (ind ~ofs:1 rbx) !%r14 ++
    setl !%r14b ++
    mkbool !%r14b
  | BLeq -> 
    getb e ++
    movq (ind ~ofs:1 rax) !%r14 ++
    cmpq (ind ~ofs:1 rbx) !%r14 ++
    setle !%r14b ++
    mkbool !%r14b
  | BGt -> 
    getb e ++
    movq (ind ~ofs:1 rax) !%r14 ++
    cmpq (ind ~ofs:1 rbx) !%r14 ++
    setg !%r14b ++
    mkbool !%r14b
  | BGeq -> 
    getb e ++
    movq (ind ~ofs:1 rax) !%r14 ++
    cmpq (ind ~ofs:1 rbx) !%r14 ++
    setge !%r14b ++
    mkbool !%r14b
  | BAdd ->
    getb e ++
    movq (ind ~ofs:1 rax) !%r14 ++
    addq (ind ~ofs:1 rbx) !%r14 ++
    mkint !%r14
  | BSub ->
    getb e ++
    movq (ind ~ofs:1 rax) !%r14 ++
    subq (ind ~ofs:1 rbx) !%r14 ++
    mkint !%r14 
  | BMul ->
    getb e ++
    movq (ind ~ofs:1 rax) !%r14 ++
    imulq (ind ~ofs:1 rbx) !%r14 ++
    movq !%rax !%r14 ++
    mkint !%r14 
  | BDiv ->
    getb e ++
    movq (ind ~ofs:1 rax) !%rax ++
    cqto ++ 
    idivq !%rbx ++
    movq !%rax !%r14 ++
    mkint !%r14
  | BAnd -> 
    let l, a = new_label (), new_label () in
    movb (ind ~ofs:1 rax) !%r8b ++
    testb !%r8b !%r8b ++
    je l ++
    getb e ++
    testb (ind ~ofs:1 rbx) !%r8b ++
    setne !%r14b ++
    jmp a ++
    label l ++
    movb (imm 0) !%r14b ++
    label a ++
    mkbool !%r14b
  | BOr -> 
    let l, a = new_label (), new_label () in
    movb (ind ~ofs:1 rax) !%r8b ++
    testb !%r8b !%r8b ++
    jne l ++
    getb e ++
    notb !%r8b ++
    movb (ind ~ofs:1 rbx) !%r9b ++
    notb !%r9b ++
    testb !%r8b !%r9b ++
    sete !%r14b ++
    jmp a ++
    label l ++
    movb (imm 1) !%r14b ++
    label a ++
    mkbool !%r14b end in
  if List.length l = 1 then 
    compile_expr (List.hd l)
  else begin List.fold_left (fun acc e ->
    acc ++
    pushq !%rax ++
    opcode e) 
    (compile_expr (List.hd l) ++
      popq rax)
    (List.tl l) ++
  pushq !%rax end

| AEIf (l, eb) ->
  let endif = new_label () in
  List.fold_left (fun acc (c, cb) ->
    let clab = new_label () in
    acc ++
    compile_expr c ++
    popq rax ++
    movb (ind ~ofs:1 rax) !%cl ++
    testb (ind ~ofs:1 rax) !%cl ++
    je clab ++
    compile_block cb ++
    jmp endif ++
    label clab
    ) nop l ++
  compile_block eb ++
  label endif

| AEBlock b -> (* create new scope !!! *)
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  compile_block b ++
  popq rbp

| AELam (clos, b) ->
  let l = new_label () in
  codefun := !codefun ++
    label l ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++ 
    compile_block b ++
    popq rax ++
    popq rbp ++ 
    ret;
  pushq (imm (9 + 8*(Hashtbl.length clos.captured))) ++
  call "_malloc" ++
  popn 8 ++
  movb (imm 6) (ind rax) ++
  movq (ilab l) (ind ~ofs:1 rax) ++
  pushq !%rax ++
  Hashtbl.fold (fun _ (i, ofs) acc ->
    acc ++
    movetoheap (ofs (* - clos.frame_size*)) !%rcx ++
    movq !%rcx (ind ~ofs:i rax))
    clos.captured nop

| AECases (e, ef, lf) -> 
  let l, lend = new_label (), new_label () in
  compile_expr e ++
  popq rax ++
  cmpb (imm 4) (ind rax) ++
  jne l ++
  compile_expr ef ++
  jmp lend ++
  label l ++
  pushq (ind ~ofs:9 rax) ++
  pushq (ind ~ofs:1 rax) ++
  compile_expr lf ++
  popn 16 ++
  label lend

and compile_stmt = function
| ASExpr e -> 
  compile_expr e
| ASDeclC e -> 
  compile_expr e ++
  popq rax ++
  pushq !%rax ++
  pushq !%rax
| ASDeclV e -> 
  compile_expr e ++
  popq r14 ++
  mkptr !%r14 ++
  pushq !%rax ++
  pushq !%rax
| ASAssign (Clos, a, e) ->
  compile_expr e ++
  popq rax ++
  movq (ind ~ofs:16 rbp) !%rcx ++
  movq (ind ~ofs:a rcx) !%rcx ++
  movq !%rax (ind ~ofs:1 rcx) ++
  pushq !%rcx
| ASAssign (_, a, e) ->
  compile_expr e ++
  popq rax ++
  movq (ind ~ofs:a rbp) !%rcx ++
  movq !%rax (ind ~ofs:1 rcx) ++
  pushq !%rcx
and compile_block b =
  List.fold_left (fun acc s -> 
    acc ++ 
    popq rax ++ 
    compile_stmt s) (pushq (imm 0)) b

let code f =
  let f, fp = alloc_block default_env (List.length loc) f in
  compile_block f ++
  popq rax ++
  popn (8 * fp) |>
  make !codefun