	.text
	.file	"example.ll"
	.globl	putchar                         # -- Begin function putchar
	.p2align	4, 0x90
	.type	putchar,@function
putchar:                                # @putchar
	.cfi_startproc
# %bb.0:
	movl	%edi, %eax
	retq
.Lfunc_end0:
	.size	putchar, .Lfunc_end0-putchar
	.cfi_endproc
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$42, %edi
	callq	putchar@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
