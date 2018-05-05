	.file	"examples/basic_graphics.ll"
	.section	.rodata.cst16,"aM",@progbits,16
	.align	16
.LCPI0_0:
	.zero	16
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6:
	.cfi_def_cfa_offset 32
	subq	$96, %rsp
.Ltmp7:
	.cfi_def_cfa_offset 128
.Ltmp8:
	.cfi_offset %rbx, -32
.Ltmp9:
	.cfi_offset %r14, -24
.Ltmp10:
	.cfi_offset %r15, -16
	movl	$12, %edi
	callq	malloc
	movl	$200, (%rax)
	movl	$200, 4(%rax)
	movl	$200, 8(%rax)
	movq	%rax, 88(%rsp)
	movl	$32, %edi
	callq	malloc
	movl	$0, (%rax)
	movq	$0, 8(%rax)
	movl	$0, 16(%rax)
	movl	$0, 20(%rax)
	movq	$0, 24(%rax)
	movq	%rax, 80(%rsp)
	movq	88(%rsp), %rcx
	movl	$2, (%rax)
	movq	$0, 8(%rax)
	movl	$0, 16(%rax)
	movl	$200, 20(%rax)
	movq	%rcx, 24(%rax)
	movl	$32, %edi
	callq	malloc
	movl	$0, (%rax)
	movq	$0, 8(%rax)
	movl	$0, 16(%rax)
	movl	$0, 20(%rax)
	movq	$0, 24(%rax)
	movq	%rax, 72(%rsp)
	movq	88(%rsp), %rcx
	movl	$1, (%rax)
	movq	$0, 8(%rax)
	movl	$100, 16(%rax)
	movl	$200, 20(%rax)
	movq	%rcx, 24(%rax)
	movl	$32, %edi
	callq	malloc
	movl	$0, (%rax)
	movq	$0, 8(%rax)
	movl	$0, 16(%rax)
	movl	$0, 20(%rax)
	movq	$0, 24(%rax)
	movq	%rax, 64(%rsp)
	movq	88(%rsp), %rcx
	movl	$3, (%rax)
	movq	$0, 8(%rax)
	movl	$100, 16(%rax)
	movl	$200, 20(%rax)
	movq	%rcx, 24(%rax)
	movl	$32, %edi
	callq	malloc
	movl	$0, (%rax)
	movq	$0, 8(%rax)
	movl	$0, 16(%rax)
	movl	$0, 20(%rax)
	movq	$0, 24(%rax)
	movq	%rax, 56(%rsp)
	movl	$4, (%rax)
	movq	$.Ltmp, 8(%rax)
	movabsq	$858993459400, %rcx     # imm = 0xC8000000C8
	movq	%rcx, 16(%rax)
	movq	64(%rsp), %rbx
	movl	$24, %edi
	callq	malloc
	movq	%rbx, (%rax)
	movl	$100, 8(%rax)
	movl	$300, 12(%rax)          # imm = 0x12C
	movl	$3, 16(%rax)
	movl	$5, 20(%rax)
	movq	%rax, 48(%rsp)
	movq	56(%rsp), %rbx
	movl	$24, %edi
	callq	malloc
	movq	%rbx, (%rax)
	movl	$200, 8(%rax)
	movl	$300, 12(%rax)          # imm = 0x12C
	movl	$3, 16(%rax)
	movl	$5, 20(%rax)
	movq	%rax, 40(%rsp)
	movq	80(%rsp), %rbx
	movl	$24, %edi
	callq	malloc
	movq	%rbx, (%rax)
	movl	$300, 8(%rax)           # imm = 0x12C
	movl	$300, 12(%rax)          # imm = 0x12C
	movl	$3, 16(%rax)
	movl	$5, 20(%rax)
	movq	%rax, 32(%rsp)
	movq	72(%rsp), %rbx
	movl	$24, %edi
	callq	malloc
	movq	%rbx, (%rax)
	movl	$400, 8(%rax)           # imm = 0x190
	movl	$300, 12(%rax)          # imm = 0x12C
	movl	$3, 16(%rax)
	movl	$5, 20(%rax)
	movq	%rax, 24(%rsp)
	movq	80(%rsp), %rbx
	movl	$24, %edi
	callq	malloc
	movq	%rbx, (%rax)
	movl	$500, 8(%rax)           # imm = 0x1F4
	movl	$300, 12(%rax)          # imm = 0x12C
	movl	$3, 16(%rax)
	movl	$5, 20(%rax)
	movq	%rax, 16(%rsp)
	movq	80(%rsp), %rbx
	movl	$24, %edi
	callq	malloc
	movq	%rbx, (%rax)
	movl	$600, 8(%rax)           # imm = 0x258
	movl	$300, 12(%rax)          # imm = 0x12C
	movl	$3, 16(%rax)
	movl	$5, 20(%rax)
	movq	%rax, 8(%rsp)
	movl	$48, %edi
	callq	malloc
	movq	%rax, (%rsp)
	movl	$16, %edi
	callq	malloc
	movq	%rax, %rbx
	vxorps	%xmm0, %xmm0, %xmm0
	vmovups	%xmm0, (%rbx)
	movl	$8, %edi
	callq	malloc
	movq	%rbx, (%rax)
	movq	(%rsp), %rcx
	movq	%rax, (%rcx)
	movl	$16, %edi
	callq	malloc
	movq	%rax, %rbx
	vxorps	%xmm0, %xmm0, %xmm0
	vmovups	%xmm0, (%rbx)
	movl	$8, %edi
	callq	malloc
	movq	%rbx, (%rax)
	movq	(%rsp), %rcx
	movq	%rax, 8(%rcx)
	movl	$16, %edi
	callq	malloc
	movq	%rax, %rbx
	vxorps	%xmm0, %xmm0, %xmm0
	vmovups	%xmm0, (%rbx)
	movl	$8, %edi
	callq	malloc
	movq	%rbx, (%rax)
	movq	(%rsp), %rcx
	movq	%rax, 16(%rcx)
	movl	$16, %edi
	callq	malloc
	movq	%rax, %rbx
	vxorps	%xmm0, %xmm0, %xmm0
	vmovups	%xmm0, (%rbx)
	movl	$8, %edi
	callq	malloc
	movq	%rbx, (%rax)
	movq	(%rsp), %rcx
	movq	%rax, 24(%rcx)
	movl	$16, %edi
	callq	malloc
	movq	%rax, %rbx
	vxorps	%xmm0, %xmm0, %xmm0
	vmovups	%xmm0, (%rbx)
	movl	$8, %edi
	callq	malloc
	movq	%rbx, (%rax)
	movq	(%rsp), %rcx
	movq	%rax, 32(%rcx)
	movl	$16, %edi
	callq	malloc
	movq	%rax, %rbx
	vxorps	%xmm0, %xmm0, %xmm0
	vmovups	%xmm0, (%rbx)
	movl	$8, %edi
	callq	malloc
	movq	%rbx, (%rax)
	movq	(%rsp), %rcx
	movq	%rax, 40(%rcx)
	movq	(%rsp), %rax
	movq	(%rax), %rbx
	movq	48(%rsp), %r14
	movq	(%rbx), %r15
	movl	$16, %edi
	callq	malloc
	movq	%r15, (%rax)
	movq	%r14, 8(%rax)
	movq	%rax, (%rbx)
	movq	(%rsp), %rax
	movq	8(%rax), %rbx
	movq	40(%rsp), %r14
	movq	(%rbx), %r15
	movl	$16, %edi
	callq	malloc
	movq	%r15, (%rax)
	movq	%r14, 8(%rax)
	movq	%rax, (%rbx)
	movq	(%rsp), %rax
	movq	16(%rax), %rbx
	movq	32(%rsp), %r14
	movq	(%rbx), %r15
	movl	$16, %edi
	callq	malloc
	movq	%r15, (%rax)
	movq	%r14, 8(%rax)
	movq	%rax, (%rbx)
	movq	(%rsp), %rax
	movq	24(%rax), %rbx
	movq	24(%rsp), %r14
	movq	(%rbx), %r15
	movl	$16, %edi
	callq	malloc
	movq	%r15, (%rax)
	movq	%r14, 8(%rax)
	movq	%rax, (%rbx)
	movq	(%rsp), %rax
	movq	32(%rax), %rbx
	movq	16(%rsp), %r14
	movq	(%rbx), %r15
	movl	$16, %edi
	callq	malloc
	movq	%r15, (%rax)
	movq	%r14, 8(%rax)
	movq	%rax, (%rbx)
	movq	(%rsp), %rax
	movq	40(%rax), %rbx
	movq	8(%rsp), %r14
	movq	(%rbx), %r15
	movl	$16, %edi
	callq	malloc
	movq	%r15, (%rax)
	movq	%r14, 8(%rax)
	movq	%rax, (%rbx)
	movq	(%rsp), %rsi
	movl	$6, %edi
	movl	$1, %edx
	movl	$800, %ecx              # imm = 0x320
	movl	$600, %r8d              # imm = 0x258
	xorl	%eax, %eax
	callq	render
	movl	%eax, %ecx
	movl	$.Lfmt, %edi
	xorl	%eax, %eax
	movl	%ecx, %esi
	callq	printf
	addq	$96, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	ret
.Ltmp11:
	.size	main, .Ltmp11-main
	.cfi_endproc

	.type	.Ltmp,@object           # @tmp
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Ltmp:
	.asciz	"images/shrek.png"
	.size	.Ltmp, 17

	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4


	.section	".note.GNU-stack","",@progbits
