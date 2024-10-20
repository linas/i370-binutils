#
# Demo of (fixed) bug for bignum, float and double support.
#
# Assemble with
# ../build/gas/as-new -a=bug-bignum.lst -o bug-bignum.obj bug-bignum.s
#
.text
.globl main
.using .,r7

         MVC   76(4,r13),=XL4'cacad0d0'
# E and EH are both IBM floating point Hex
# EB is "floating point binary" aka IEEE float
         MVC   84(4,r13),=E'0.0'
         MVC   84(4,r13),=EB'-0.0'
         MVC   84(4,r13),=E'1.0'
         MVC   84(4,r13),=EB'1.0'
         MVC   84(4,r13),=E'-1.0'
         MVC   84(4,r13),=EB'-1.0'
         MVC   84(4,r13),=E'2.0'
         MVC   84(4,r13),=E'-2.0'
         MVC   84(4,r13),=EB'+inf'
         MVC   84(4,r13),=EB'-inf'
         MVC   84(4,r13),=EB'0.0314159E2'
         MVC   84(4,r13),=EH'0.0314159E2'
         MVC   84(4,r13),=EB'-3.14159'
         MVC   84(4,r13),=EH'3.14159'
         MVC   84(4,r13),=E'3.1415926'
         MVC   84(4,r13),=E'3.141592653'
         MVC   84(4,r13),=E'3.14159265348979'
         MVC   84(4,r13),=E'27.1828'
         MVC   84(4,r13),=E'314.159'
         MVC   84(4,r13),=E'314.159E-30'
         MVC   84(4,r13),=E'314.159E+30'
         MVC   92(8,r13),=XL8'C1C2C3C4C5C6C7C8'
# D and DH are both IBM double-precision floating point Hex
# DB is "double floating point binary" aka IEEE float
         MVC   136(4,r13),=D'0.0'
         MVC   136(4,r13),=DB'0.0'
         MVC   136(4,r13),=DB'-0.0'
         MVC   136(4,r13),=D'1.0'
         MVC   136(4,r13),=DB'1.0'
         MVC   136(4,r13),=D'-1.0'
         MVC   136(4,r13),=DB'-1.0'
         MVC   136(4,r13),=D'2.0'
         MVC   136(4,r13),=DB'2.0'
         MVC   136(4,r13),=D'-2.0'
         MVC   136(4,r13),=DB'-2.0'
         MVC   116(8,r13),=D'0.314159E1'
         MVC   124(8,r13),=XL8'C2C2C2C2C2C2C2C2'
.balign 4
.ltorg
