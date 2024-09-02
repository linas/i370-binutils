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
         MVC   84(4,r13),=E'0.0'
         MVC   84(4,r13),=E'-0.0'
         MVC   84(4,r13),=E'1.0'
         MVC   84(4,r13),=E'-1.0'
         MVC   84(4,r13),=E'2.0'
         MVC   84(4,r13),=E'-2.0'
         MVC   84(4,r13),=E'+inf'
         MVC   84(4,r13),=E'-inf'
         MVC   84(4,r13),=E'0.0314159E2'
         MVC   84(4,r13),=E'-3.14159'
         MVC   92(8,r13),=XL8'C1C2C3C4C5C6C7C8'
         MVC   116(8,r13),=D'0.314159E1'
         MVC   116(8,r13),=D'1.0'
         MVC   124(8,r13),=XL8'C2C2C2C2C2C2C2C2'
.balign 4
.ltorg
