	.file	"bug-relative.c"
	.version	"01.01"
gcc2_compiled.:
.globl glueball
.data
	.balign 4
	.type	 glueball,@object
	.size	 glueball,4
glueball:
	.long 1234
.text
	.balign 4
.globl bumpy
# arg_size=0x8 frame_size=0x0 aligned size=0x60
# varargs=0 stdarg=0 rserved area size=0x200
bumpy:
# Function prologue
	.using	.,r15
	B	.LFENT000001
	.long	96
	.long	.LPGT0
	.drop	r15
	.balign 2
.LFENT000001:
	STM	r13,r12,8(r11)
	LR	r13,r11
	A	r11,4(,r15)
	L	r4,8(,r15)
	BASR	r3,0
	.using	.,r3
.LPG0:
	ST	r4,0(r13)
# Function code
	L	r1,=A(glueball)
	MVC	0(4,r1),88(r13)
	L	r1,=A(glueball)
	MVC	0(4,r1),=F'888'
	L	r1,=A(glueball)
	L	r0,88(r13)
	LR	r2,r0
	LR	r0,r2
	SLL	r0,1
	ST	r0,0(r1)
	L	r1,=A(glueball)
	MVC	0(4,r1),=F'444'
	L	r1,=A(glueball)
	L	r0,0(r1)
	LR	r15,r0
	B	.L2
.L2:
# Function epilogue
	L	r14,12(,r13)
	LM	2,12,28(r13)
	L	r13,8(,r13)
	BASR	r1,r14
# Function literal pool
	.balign	4
	.ltorg
# Function page table
	.balign	4
.LPGT0:
	.long	.LPG0
.Lfe1:
	.size	 bumpy,.Lfe1-bumpy
	.balign 4
.globl funky
# arg_size=0x8 frame_size=0x10 aligned size=0x70
# varargs=0 stdarg=0 rserved area size=0x200
funky:
# Function prologue
	.using	.,r15
	B	.LFENT000002
	.long	112
	.long	.LPGT1
	.drop	r15
	.balign 2
.LFENT000002:
	STM	r13,r12,8(r11)
	LR	r13,r11
	A	r11,4(,r15)
	L	r4,8(,r15)
	BASR	r3,0
	.using	.,r3
.LPG1:
	ST	r4,0(r13)
# Function code
	L	r0,88(r13)
	A	r0,=F'44'
	ST	r0,96(r13)
	L	r0,92(r13)
	A	r0,=F'999'
	ST	r0,100(r13)
	L	r1,88(r13)
	M	r0,92(r13)
	LR	r2,r1
	L	r1,=A(glueball)
	LR	r0,r2
	A	r0,0(r1)
	ST	r0,104(r13)
	L	r0,104(r13)
	LR	r15,r0
	B	.L3
.L3:
# Function epilogue
	L	r14,12(,r13)
	LM	2,12,28(r13)
	L	r13,8(,r13)
	BASR	r1,r14
# Function literal pool
	.balign	4
	.ltorg
# Function page table
	.balign	4
.LPGT1:
	.long	.LPG1
.Lfe2:
	.size	 funky,.Lfe2-funky
	.ident	"GCC: (GNU) 2.96 19991229 (experimental)"
