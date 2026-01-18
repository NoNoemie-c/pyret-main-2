
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
  "empty", -16;
  "strconcat", -24; 
  "_num_modulo", -32; 
  "raise", -40;
  "print", -48;
  "link", -56 (*; 
  "each", -64;
  "fold", -72;
  "print_list_4242", -80;
  "list_eq", -88;
  "list_neq", -96*)
]
let default_env = 
{ oldlocals = SMap.empty; 
  captured = Hashtbl.create 17; 
  args = SMap.empty; 
  locals = List.fold_left (fun m (k, v) -> SMap.add k v m) SMap.empty loc }
type clos = { fp: int; captured: (string, int * int) Hashtbl.t; frame_size: int }

let new_label =
  let x = ref 0 in
  fun () -> incr x;
    "_lab_" ^ string_of_int !x

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let rec skip n l = 
  if n <= 0 then l 
  else skip (n - 1) (List.tl l)
let alloc_default =
  pushq (imm 1) ++
  call "_malloc" ++
  popn 8 ++
  movb (imm 0) (ind rax) ++
  pushq !%rax ++
  pushq (imm 1) ++
  call "_malloc" ++
  popn 8 ++
  movb (imm 4) (ind rax) ++
  pushq !%rax ++
  List.fold_left (fun acc n -> acc ++ 
    pushq (imm 9) ++
    call "_malloc" ++
    popn 8 ++ 
    movq (imm 6) (ind rax) ++
    movq (ilab n) (ind ~ofs:1 rax) ++
    pushq !%rax)
    nop (skip 2 (List.map fst loc))

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

(* maybe not necessary ? *)
let movetoheap i d = 
  movq !%rax !%r13  ++
  movq (ind ~ofs:i rbp) !%r14 ++
  mkptr !%r14 ++
  movq !%rax (ind ~ofs:i rbp) ++
  movq (ind ~ofs:1 rax) d ++
  movq !%r13 !%rax

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
    pushq !%rsi ++
    (*pushq (ilab "print_list_4242") ++*)
    call_star (ind rsp) ++
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

    label "link" ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    pushq (imm 17) ++
    call "_malloc" ++
    popn 8 ++
    movb (imm 5) (ind rax) ++
    movq (ind ~ofs:24 rbp) !%rcx ++
    movq !%rcx (ind ~ofs:1 rax) ++
    movq (ind ~ofs:32 rbp) !%rcx ++
    movq !%rcx (ind ~ofs:9 rax) ++
    popq rbp ++ 
    ret ++

    label "_num_modulo" ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    movq (imm 0) !%r12 ++
    movq (imm 0) !%r13 ++
    movq (ind ~ofs:24 rbp) !%rax ++
    movq (ind ~ofs:32 rbp) !%rbx ++
    movq (ind ~ofs:1 rax) !%rax ++
    movq (ind ~ofs:1 rbx) !%rbx ++ 
    cmpq (imm 0) !%rax ++
    setl !%r12b ++ 
    cmpq (imm 0) !%rbx ++
    setl !%r13b ++ 
    shlq (imm 8) !%r13 ++
    addq !%r13 !%r12 ++
    cqto ++ 
    idivq !%rbx ++
    movq !%rdx !%r14 ++
    cmpw (imm 1) !%r12w ++
    je "t1" ++
    cmpw (imm 256) !%r12w ++
    je "t2" ++
    jmp "tend" ++
    label "t1" ++
    addq !%rbx !%r14 ++
    jmp "tend" ++
    label "t2" ++
    addq !%rbx !%r14 ++
    label "tend" ++
    mkint !%r14 ++
    popq rbp ++ 
    ret ++

    label "strconcat" ++
    pushq !%rbp ++
    movq !%rsp !%rbp ++
    movq (ind ~ofs:24 rbp) !%r13 ++
    movq (ind ~ofs:32 rbp) !%r14 ++
    movq !%r13 !%r11 ++
    movq !%r14 !%r12 ++
    movq (imm 0) !%rcx ++
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
    pushq !%rcx ++
    call "_malloc" ++
    addq (imm 8) !%rsp ++
    movb (imm 3) (ind rax) ++
    movq !%rax !%rcx ++
    label "w3" ++
    addq (imm 1) !%rcx ++
    addq (imm 1) !%r13 ++
    cmpb (imm 0) (ind r13) ++
    movb (ind r13) !%r8b ++
    movb !%r8b (ind rcx) ++
    jne "w3" ++
    addq (imm (-1)) !%rcx ++
    label "w4" ++
    addq (imm 1) !%rcx ++
    addq (imm 1) !%r14 ++
    cmpb (imm 0)  (ind r14)++
    movb (ind r14) !%r8b ++
    movb !%r8b (ind rcx) ++
    jne "w4" ++
    popq rbp ++ 
    ret ++

    label "raise" ++
    movq !%rsp !%rbp ++ 
    pushq (ind ~ofs:24 rbp) ++
    pushq (ilab ".Sprint_string") ++
    call "_printf" ++
    movq (imm 1) !%rax ++
    jmp "exit";
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