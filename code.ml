
module SMap = Map.Make(String)
open X86_64

type local_env = {
  locals: int SMap.t; 
  oldlocals: int SMap.t;
  captured: (string, int * int) Hashtbl.t; 
  args: int SMap.t 
}

let loc = [
  "nothing", -8; 
  "strconcat", -16; 
  "_num_modulo", -24; 
  "raise", -32;
  "print", -40
]
let default_env = 
{ oldlocals = SMap.empty; 
  captured = Hashtbl.create 17; 
  args = SMap.empty; 
  locals = List.fold_left (fun m (k, v) -> SMap.add k v m) SMap.empty loc }
type clos = { fp: int; captured: (string, int * int) Hashtbl.t }

let new_label =
  let x = ref 0 in
  fun () -> incr x;
    "_lab_" ^ string_of_int !x

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let alloc_default =
  pushq (imm 1) ++
  call "_malloc" ++
  popn 8 ++
  movb (imm 0) (ind rax) ++
  pushq !%rax ++
  List.fold_left (fun acc n -> acc ++ 
    pushq (imm 9) ++
    call "_malloc" ++
    popn 8 ++ 
    movq (imm 6) (ind rax) ++
    movq (ilab n) (ind ~ofs:1 rax) ++
    pushq !%rax)
    nop (List.tl (List.map fst loc))

let readptr_rax () =
  let l = new_label () in
  cmpb (imm 7) (ind rax) ++
  jne l ++
  movq (ind ~ofs:1 rax) !%rax ++
  label l

let mkbool r =
  pushq (imm 2) ++
  call "_malloc" ++
  popn 8 ++
  movb (imm 1) (ind rax) ++
  movb r (ind ~ofs:1 rax) 
let mkint r =
  pushq (imm 9) ++
  call "_malloc" ++
  popn 8 ++
  movb (imm 2) (ind rax) ++
  movq r (ind ~ofs:1 rax) 
let mkptr r = 
  pushq (imm 9) ++
  call "_malloc" ++
  popn 8 ++
  movb (imm 7) (ind rax) ++
  movq r (ind ~ofs:1 rax) 
let mkfun r = 
  pushq (imm 9) ++
  call "_malloc" ++
  popn 8 ++
  movb (imm 6) (ind rax) ++
  movq r (ind ~ofs:1 rax) 

let make codefun codemain =
{
  text =
    globl "main" ++ label "main" ++
    movq !%rsp !%rbp ++
    movq !%rbp !%r15 ++
    alloc_default ++
    codemain ++
    movq (imm 0) !%rax ++ (* exit *)
    label "exit" ++
    movq !%r15 !%rbp ++
    movq !%r15 !%rsp ++
    ret ++
    
    codefun ++

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
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    movq (ind ~ofs:24 rbp) !%rsi ++
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
    pushq (imm 0) ++
    pushq (ilab ".Sprint_nothing") ++
    call "_printf" ++
    jmp "end_print" ++

    label "print_boolean" ++
    pushq (imm 0) ++
    movq (ind ~ofs:1 rsi) !%rdi ++
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
    pushq (ind ~ofs:1 rsi) ++
    pushq (ilab ".Sprint_number") ++
    call "_printf" ++
    jmp "end_print" ++

    label "print_string" ++
    addq (imm 1) !%rsi ++
    pushq !%rsi ++
    pushq (ilab ".Sprint_string") ++
    call "_printf" ++
    jmp "end_print" ++
    
    label "print_empty" ++
    pushq (imm 0) ++
    pushq (ilab ".Sprint_empty") ++
    call "_printf" ++
    jmp "end_print" ++

    label "print_link" ++
    pushq (imm 0) ++
    pushq (ilab ".Sprint_link") ++
    call "_printf" ++
    jmp "end_print" ++

    label "print_function" ++
    pushq (imm 0) ++
    pushq (ilab ".Sprint_function") ++
    call "_printf" ++
    jmp "end_print" ++

    label "end_print" ++
    addq (imm 16) !%rsp ++
    movq (ind ~ofs:24 rbp) !%rax ++
    popq rbp ++ 
    ret ++

    label "_num_modulo" ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    movq (ind ~ofs:24 rbp) !%rax ++
    movq (ind ~ofs:32 rbp) !%rbx ++
    cqto ++ 
    idivq !%rbx ++
    movq !%rdx !%r14 ++
    mkint !%r14 ++
    popq rbp ++ 
    ret ++

    label "strconcat" ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    movq (ind ~ofs:24 rbp) !%r13 ++
    movq (ind ~ofs:32 rbp) !%r14 ++
    movq !%rax !%r11 ++
    addq (imm (-1)) !%r11 ++
    movq !%rbx !%r12 ++
    addq (imm (-1)) !%r12 ++
    movq (imm (-1)) !%rcx ++
    label "w1" ++
    addq (imm 1) !%rcx ++
    addq (imm 1) !%r11 ++
    cmpb (imm 0) (ind r11) ++
    jne "w1" ++
    label "w2" ++
    addq (imm 1) !%rcx ++
    addq (imm 1) !%r12 ++
    cmpb (imm 0) (ind r12) ++
    jne "w2" ++
    addq (imm 9) !%rcx ++
    pushq !%rcx ++
    call "_malloc" ++
    addq (imm 8) !%rsp ++
    movb (imm 3) (ind rax) ++
    movq !%rax !%rcx ++
    addq (imm (-1)) !%r13 ++
    addq (imm (-1)) !%r14 ++
    label "w3" ++
    addq (imm 1) !%rcx ++
    addq (imm 1) !%r13 ++
    cmpb (imm 0) (ind r13) ++
    movb (ind rcx) !%r8b ++
    movb !%r8b (ind r13) ++
    jne "w3" ++
    addq (imm (-1)) !%rcx ++
    label "w4" ++
    addq (imm 1) !%rcx ++
    addq (imm 1) !%r14 ++
    cmpb (imm 0)  (ind r14)++
    movb (ind rcx) !%r8b ++
    movb !%r8b (ind r14) ++
    jne "w4" ++
    popq rbp ++ 
    ret ++

    label "raise" ++
    movq !%rsp !%rbp ++ 
    pushq (ind ~ofs:24 rbp) ++
    pushq (ilab ".Sprint_string") ++
    call "_printf" ++
    movq (imm 1) !%rax ++
    jmp "exit" ++

    label "nothing";
  data =
    label ".Sprint_number" ++ string "%d" ++
    label ".Sprint_string" ++ string "%s" ++
    label ".Sprint_nothing" ++ string "nothing" ++
    label ".Sprint_true" ++ string "true" ++
    label ".Sprint_false" ++ string "false" ++
    label ".Sprint_empty" ++ string "empty" ++
    label ".Sprint_link" ++ string "link" ++
    label ".Sprint_function" ++ string "<function>"
}
