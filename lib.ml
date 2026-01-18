open X86_64

(* compiled from lib.arr and edited to fit here *)
let code = 
  label "_lx_4" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:24 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_6" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_6" ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_5" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_5" ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 16) !%rsp ++
	pushq !%rax ++
	popq rax ++
	movq (ind ~ofs:32 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_9" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_9" ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_8" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_8" ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:17 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_7" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_7" ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 24) !%rsp ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_10" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_11" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_11" ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "list_eq" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:32 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_12" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_12" ++
	pushq !%rax ++
	popq rax ++
	cmpb (imm 4) (ind ~ofs:0 rax) ++
	jne "_lx_3" ++
	pushq (imm 17) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_10") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-24) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-24) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-24) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	jmp "_lx_2" ++
  label "_lx_3" ++
	pushq (ind ~ofs:9 rax) ++
	pushq (ind ~ofs:1 rax) ++
	pushq (imm 25) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_4") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-96) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-96) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-96) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:17 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-8) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-8) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-8) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	addq (imm 16) !%rsp ++
  label "_lx_2" ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_16" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:32 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_22" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_22" ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:17 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_21" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_21" ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_20" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_20" ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:25 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_19" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_19" ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 32) !%rsp ++
	pushq !%rax ++
	movq (ind ~ofs:24 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_18" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_18" ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_17" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_17" ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 24) !%rsp ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_23" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_24" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_24" ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "list_neq" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:40 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_25" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_25" ++
	pushq !%rax ++
	popq rax ++
	cmpb (imm 4) (ind ~ofs:0 rax) ++
	jne "_lx_15" ++
	pushq (imm 17) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_23") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:16 rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:16 rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	jmp "_lx_14" ++
  label "_lx_15" ++
	pushq (ind ~ofs:9 rax) ++
	pushq (ind ~ofs:1 rax) ++
	pushq (imm 33) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_16") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-104) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-104) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-104) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:25 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-8) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-8) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-8) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:0 rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:0 rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:0 rbp) !%rcx ++
	movq !%rcx (ind ~ofs:17 rax) ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	addq (imm 16) !%rsp ++
  label "_lx_14" ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_35" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	pushq (imm 4) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 3) (ind ~ofs:0 rax) ++
	movb (imm 0) (ind ~ofs:3 rax) ++
	movb (imm 44) (ind ~ofs:1 rax) ++
	movb (imm 32) (ind ~ofs:2 rax) ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_36" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_36" ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 16) !%rsp ++
	pushq !%rax ++
	popq rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:17 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_38" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_38" ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:25 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_37" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_37" ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 16) !%rsp ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_39" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_40" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_40" ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_30" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:24 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_32" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_32" ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_31" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_31" ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 16) !%rsp ++
	pushq !%rax ++
	popq rax ++
	movq (ind ~ofs:32 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_41" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_41" ++
	pushq !%rax ++
	popq rax ++
	cmpb (imm 4) (ind ~ofs:0 rax) ++
	jne "_lx_34" ++
	pushq (imm 17) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_39") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-88) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-88) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-88) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	jmp "_lx_33" ++
  label "_lx_34" ++
	pushq (ind ~ofs:9 rax) ++
	pushq (ind ~ofs:1 rax) ++
	pushq (imm 33) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_35") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-136) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-136) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-136) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:8 rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:8 rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:8 rbp) !%rcx ++
	movq !%rcx (ind ~ofs:17 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-64) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-64) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-64) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:25 rax) ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	addq (imm 16) !%rsp ++
  label "_lx_33" ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_42" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_43" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_43" ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_27" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:24 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_44" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_44" ++
	pushq !%rax ++
	popq rax ++
	cmpb (imm 4) (ind ~ofs:0 rax) ++
	jne "_lx_29" ++
	pushq (imm 17) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_42") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-56) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-56) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-56) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	jmp "_lx_28" ++
  label "_lx_29" ++
	pushq (ind ~ofs:9 rax) ++
	pushq (ind ~ofs:1 rax) ++
	pushq (imm 49) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_30") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-136) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-136) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-136) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-112) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-112) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-112) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-88) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-88) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-88) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:8 rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:8 rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:8 rbp) !%rcx ++
	movq !%rcx (ind ~ofs:17 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-64) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-64) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-64) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:25 rax) ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	addq (imm 16) !%rsp ++
  label "_lx_28" ++
	popq rax ++
	popq rbp ++
	ret ++
  label "each" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	pushq (imm 57) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_27") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-112) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-112) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-112) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-136) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-136) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-136) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-88) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-88) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-88) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-56) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-56) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-56) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:8 rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:8 rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:8 rbp) !%rcx ++
	movq !%rcx (ind ~ofs:17 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-64) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-64) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-64) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:25 rax) ++
	popq rax ++
	pushq !%rax ++
	pushq !%rax ++
	popq rax ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 3) (ind ~ofs:0 rax) ++
	movb (imm 0) (ind ~ofs:8 rax) ++
	movb (imm 91) (ind ~ofs:1 rax) ++
	movb (imm 108) (ind ~ofs:2 rax) ++
	movb (imm 105) (ind ~ofs:3 rax) ++
	movb (imm 115) (ind ~ofs:4 rax) ++
	movb (imm 116) (ind ~ofs:5 rax) ++
	movb (imm 58) (ind ~ofs:6 rax) ++
	movb (imm 32) (ind ~ofs:7 rax) ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_45" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_45" ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 16) !%rsp ++
	pushq !%rax ++
	popq rax ++
	movq (ind ~ofs:24 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_47" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_47" ++
	pushq !%rax ++
	movq (ind ~ofs:(-8) rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_46" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_46" ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 16) !%rsp ++
	pushq !%rax ++
	popq rax ++
	pushq (imm 3) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 3) (ind ~ofs:0 rax) ++
	movb (imm 0) (ind ~ofs:2 rax) ++
	movb (imm 93) (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_48" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_48" ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 16) !%rsp ++
	pushq !%rax ++
	popq rax ++
	movq (ind ~ofs:24 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_49" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_49" ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_56" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_59" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_59" ++
	pushq !%rax ++
	popq rax ++
	pushq !%rax ++
	movq (ind ~ofs:24 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_60" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_60" ++
	pushq !%rax ++
	popq rbx ++
	popq rax ++
	movb (ind ~ofs:0 rbx) !%r14b ++
	cmpb (imm 6) !%r14b ++
	sete !%r13b ++
	movb (ind ~ofs:0 rax) !%r14b ++
	cmpb (imm 6) !%r14b ++
	sete !%r12b ++
	testb !%r12b !%r13b ++
	je "_lx_58" ++
	pushq (imm 25) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 3) (ind ~ofs:0 rax) ++
	movb (imm 0) (ind ~ofs:24 rax) ++
	movb (imm 99) (ind ~ofs:1 rax) ++
	movb (imm 97) (ind ~ofs:2 rax) ++
	movb (imm 110) (ind ~ofs:3 rax) ++
	movb (imm 39) (ind ~ofs:4 rax) ++
	movb (imm 116) (ind ~ofs:5 rax) ++
	movb (imm 32) (ind ~ofs:6 rax) ++
	movb (imm 99) (ind ~ofs:7 rax) ++
	movb (imm 111) (ind ~ofs:8 rax) ++
	movb (imm 109) (ind ~ofs:9 rax) ++
	movb (imm 112) (ind ~ofs:10 rax) ++
	movb (imm 97) (ind ~ofs:11 rax) ++
	movb (imm 114) (ind ~ofs:12 rax) ++
	movb (imm 101) (ind ~ofs:13 rax) ++
	movb (imm 32) (ind ~ofs:14 rax) ++
	movb (imm 102) (ind ~ofs:15 rax) ++
	movb (imm 117) (ind ~ofs:16 rax) ++
	movb (imm 110) (ind ~ofs:17 rax) ++
	movb (imm 99) (ind ~ofs:18 rax) ++
	movb (imm 116) (ind ~ofs:19 rax) ++
	movb (imm 105) (ind ~ofs:20 rax) ++
	movb (imm 111) (ind ~ofs:21 rax) ++
	movb (imm 110) (ind ~ofs:22 rax) ++
	movb (imm 115) (ind ~ofs:23 rax) ++
	pushq !%rax ++
	pushq (ilab "raise") ++
	call_star (ind ~ofs:0 rsp) ++
  label "_lx_58" ++
	movq (ind ~ofs:0 rax) !%rcx ++
	cmpq (ind ~ofs:0 rbx) !%rcx ++
	sete !%r14b ++
	pushq (imm 2) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 1) (ind ~ofs:0 rax) ++
	movb !%r14b (ind ~ofs:1 rax) ++
	pushq !%rax ++
	popq rax ++
	pushq !%rax ++
	movb (ind ~ofs:1 rax) !%r8b ++
	testb !%r8b !%r8b ++
	je "_lx_62" ++
	movq (ind ~ofs:32 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_65" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_65" ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:17 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_64" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_64" ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:25 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_63" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_63" ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 24) !%rsp ++
	pushq !%rax ++
	popq rbx ++
	popq rax ++
	testb (ind ~ofs:1 rbx) !%r8b ++
	setne !%r14b ++
	jmp "_lx_61" ++
  label "_lx_62" ++
	movb (imm 0) !%r14b ++
  label "_lx_61" ++
	pushq (imm 2) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 1) (ind ~ofs:0 rax) ++
	movb !%r14b (ind ~ofs:1 rax) ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_66" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	pushq (imm 2) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 1) (ind ~ofs:0 rax) ++
	movb (imm 0) (ind ~ofs:1 rax) ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_53" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_67" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_67" ++
	pushq !%rax ++
	popq rax ++
	cmpb (imm 4) (ind ~ofs:0 rax) ++
	jne "_lx_55" ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_66") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	jmp "_lx_54" ++
  label "_lx_55" ++
	pushq (ind ~ofs:9 rax) ++
	pushq (ind ~ofs:1 rax) ++
	pushq (imm 33) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_56") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-152) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-152) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-152) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:25 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:0 rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:0 rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:0 rbp) !%rcx ++
	movq !%rcx (ind ~ofs:17 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-8) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-8) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-8) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	addq (imm 16) !%rsp ++
  label "_lx_54" ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_71" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	pushq (imm 2) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 1) (ind ~ofs:0 rax) ++
	movb (imm 0) (ind ~ofs:1 rax) ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_72" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	pushq (imm 2) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 1) (ind ~ofs:0 rax) ++
	movb (imm 1) (ind ~ofs:1 rax) ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_68" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_73" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_73" ++
	pushq !%rax ++
	popq rax ++
	cmpb (imm 4) (ind ~ofs:0 rax) ++
	jne "_lx_70" ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_72") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	jmp "_lx_69" ++
  label "_lx_70" ++
	pushq (ind ~ofs:9 rax) ++
	pushq (ind ~ofs:1 rax) ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_71") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	addq (imm 16) !%rsp ++
  label "_lx_69" ++
	popq rax ++
	popq rbp ++
	ret ++
  label "fold" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:24 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_74" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_74" ++
	pushq !%rax ++
	popq rax ++
	cmpb (imm 4) (ind ~ofs:0 rax) ++
	jne "_lx_52" ++
	pushq (imm 17) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_68") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:16 rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:16 rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	jmp "_lx_51" ++
  label "_lx_52" ++
	pushq (ind ~ofs:9 rax) ++
	pushq (ind ~ofs:1 rax) ++
	pushq (imm 41) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_53") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-152) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-152) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-152) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:25 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:0 rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:0 rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:0 rbp) !%rcx ++
	movq !%rcx (ind ~ofs:17 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:0 rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:0 rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:0 rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-8) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-8) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-8) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	addq (imm 16) !%rsp ++
  label "_lx_51" ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_81" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_84" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_84" ++
	pushq !%rax ++
	popq rax ++
	pushq !%rax ++
	movq (ind ~ofs:24 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_85" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_85" ++
	pushq !%rax ++
	popq rbx ++
	popq rax ++
	movb (ind ~ofs:0 rbx) !%r14b ++
	cmpb (imm 6) !%r14b ++
	sete !%r13b ++
	movb (ind ~ofs:0 rax) !%r14b ++
	cmpb (imm 6) !%r14b ++
	sete !%r12b ++
	testb !%r12b !%r13b ++
	je "_lx_83" ++
	pushq (imm 25) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 3) (ind ~ofs:0 rax) ++
	movb (imm 0) (ind ~ofs:24 rax) ++
	movb (imm 99) (ind ~ofs:1 rax) ++
	movb (imm 97) (ind ~ofs:2 rax) ++
	movb (imm 110) (ind ~ofs:3 rax) ++
	movb (imm 39) (ind ~ofs:4 rax) ++
	movb (imm 116) (ind ~ofs:5 rax) ++
	movb (imm 32) (ind ~ofs:6 rax) ++
	movb (imm 99) (ind ~ofs:7 rax) ++
	movb (imm 111) (ind ~ofs:8 rax) ++
	movb (imm 109) (ind ~ofs:9 rax) ++
	movb (imm 112) (ind ~ofs:10 rax) ++
	movb (imm 97) (ind ~ofs:11 rax) ++
	movb (imm 114) (ind ~ofs:12 rax) ++
	movb (imm 101) (ind ~ofs:13 rax) ++
	movb (imm 32) (ind ~ofs:14 rax) ++
	movb (imm 102) (ind ~ofs:15 rax) ++
	movb (imm 117) (ind ~ofs:16 rax) ++
	movb (imm 110) (ind ~ofs:17 rax) ++
	movb (imm 99) (ind ~ofs:18 rax) ++
	movb (imm 116) (ind ~ofs:19 rax) ++
	movb (imm 105) (ind ~ofs:20 rax) ++
	movb (imm 111) (ind ~ofs:21 rax) ++
	movb (imm 110) (ind ~ofs:22 rax) ++
	movb (imm 115) (ind ~ofs:23 rax) ++
	pushq !%rax ++
	pushq (ilab "raise") ++
	call_star (ind ~ofs:0 rsp) ++
  label "_lx_83" ++
	movq (ind ~ofs:0 rax) !%rcx ++
	cmpq (ind ~ofs:0 rbx) !%rcx ++
	setne !%r14b ++
	pushq (imm 2) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 1) (ind ~ofs:0 rax) ++
	movb !%r14b (ind ~ofs:1 rax) ++
	pushq !%rax ++
	popq rax ++
	pushq !%rax ++
	movb (ind ~ofs:1 rax) !%r8b ++
	testb !%r8b !%r8b ++
	jne "_lx_87" ++
	movq (ind ~ofs:32 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_90" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_90" ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:17 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_89" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_89" ++
	pushq !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:25 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_88" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_88" ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 24) !%rsp ++
	pushq !%rax ++
	popq rbx ++
	popq rax ++
	notb !%r8b ++
	movb (ind ~ofs:1 rbx) !%r9b ++
	notb !%r9b ++
	testb !%r8b !%r9b ++
	sete !%r14b ++
	jmp "_lx_86" ++
  label "_lx_87" ++
	movb (imm 1) !%r14b ++
  label "_lx_86" ++
	pushq (imm 2) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 1) (ind ~ofs:0 rax) ++
	movb !%r14b (ind ~ofs:1 rax) ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_91" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	pushq (imm 2) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 1) (ind ~ofs:0 rax) ++
	movb (imm 1) (ind ~ofs:1 rax) ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_78" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_92" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_92" ++
	pushq !%rax ++
	popq rax ++
	cmpb (imm 4) (ind ~ofs:0 rax) ++
	jne "_lx_80" ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_91") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	jmp "_lx_79" ++
  label "_lx_80" ++
	pushq (ind ~ofs:9 rax) ++
	pushq (ind ~ofs:1 rax) ++
	pushq (imm 33) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_81") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-160) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-160) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-160) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:25 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:0 rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:0 rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:0 rbp) !%rcx ++
	movq !%rcx (ind ~ofs:17 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-8) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-8) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-8) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	addq (imm 16) !%rsp ++
  label "_lx_79" ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_96" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	pushq (imm 2) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 1) (ind ~ofs:0 rax) ++
	movb (imm 1) (ind ~ofs:1 rax) ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_97" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	pushq (imm 2) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 1) (ind ~ofs:0 rax) ++
	movb (imm 0) (ind ~ofs:1 rax) ++
	pushq !%rax ++
	popq rax ++
	popq rbp ++
	ret ++
  label "_lx_93" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq (ind ~ofs:9 rcx) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_98" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_98" ++
	pushq !%rax ++
	popq rax ++
	cmpb (imm 4) (ind ~ofs:0 rax) ++
	jne "_lx_95" ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_97") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	jmp "_lx_94" ++
  label "_lx_95" ++
	pushq (ind ~ofs:9 rax) ++
	pushq (ind ~ofs:1 rax) ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_96") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	addq (imm 16) !%rsp ++
  label "_lx_94" ++
	popq rax ++
	popq rbp ++
	ret ++
  label "print_list_4242" ++
	pushq !%rbp ++
	movq !%rsp !%rbp ++
	pushq (imm 0) ++
	popq rax ++
	movq (ind ~ofs:24 rbp) !%rax ++
	cmpb (imm 7) (ind ~ofs:0 rax) ++
	jne "_lx_99" ++
	movq (ind ~ofs:1 rax) !%rax ++
  label "_lx_99" ++
	pushq !%rax ++
	popq rax ++
	cmpb (imm 4) (ind ~ofs:0 rax) ++
	jne "_lx_77" ++
	pushq (imm 17) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_93") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:16 rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:16 rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:16 rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	jmp "_lx_76" ++
  label "_lx_77" ++
	pushq (ind ~ofs:9 rax) ++
	pushq (ind ~ofs:1 rax) ++
	pushq (imm 41) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 6) (ind ~ofs:0 rax) ++
	movq (ilab "_lx_78") (ind ~ofs:1 rax) ++
	pushq !%rax ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-160) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-160) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-160) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:25 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:0 rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:0 rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:0 rbp) !%rcx ++
	movq !%rcx (ind ~ofs:17 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:0 rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:0 rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:0 rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq !%rax !%r13 ++
	movq (ind ~ofs:(-8) rbp) !%r14 ++
	pushq (imm 9) ++
	call "_malloc" ++
	addq (imm 8) !%rsp ++
	movb (imm 7) (ind ~ofs:0 rax) ++
	movq !%r14 (ind ~ofs:1 rax) ++
	movq !%rax (ind ~ofs:(-8) rbp) ++
	movq (ind ~ofs:1 rax) !%rcx ++
	movq !%r13 !%rax ++
	movq (ind ~ofs:(-8) rbp) !%rcx ++
	movq !%rcx (ind ~ofs:9 rax) ++
	movq (ind ~ofs:0 rsp) !%rcx ++
	call_star (ind ~ofs:1 rcx) ++
	addq (imm 8) !%rsp ++
	pushq !%rax ++
	addq (imm 16) !%rsp ++
  label "_lx_76" ++
	popq rax ++
	popq rbp ++
	ret

