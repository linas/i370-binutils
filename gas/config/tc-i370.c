/* tc-i370.c -- Assembler for the IBM 360/370/390 instruction set.
   Loosely based on the ppc files by Linas Vepstas <linas@linas.org> 1998, 99
   Copyright (C) 1994-2018 Free Software Foundation, Inc.
   Written by Ian Lance Taylor, Cygnus Support.

   This file is part of GAS, the GNU Assembler.

   GAS is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GAS is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GAS; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street - Fifth Floor, Boston, MA
   02110-1301, USA.  */

/* This assembler implements the mashup of ELF and HLASM emited by the
   gcc i370 compiler. The main difference between HLASM and ELF is that
   ELF uses a dot in from of the pseudo-ops, while HLASM does not. The
   -mhlasm flag is uses to control this. */

#include "as.h"
#include "safe-ctype.h"
#include "subsegs.h"
#include "dwarf2dbg.h"

#include "opcode/i370.h"

#ifdef OBJ_ELF
#include "elf/i370.h"
#endif

/* true only if ch is a numeric digit */
#define ISNUM(ch) (ISALNUM (ch) && !ISALPHA (ch))

/* true if ch is a binary digit -- zero or one. */
#define ISBDIGIT(ch) ((ch) == '0' || (ch) == '1')

/* This is the assembler for the System/390 Architecture.  */

/* Size of the literal pool/ltorg to print; #include "gas/listing.h" */
extern int listing_lhs_cont_lines;

bool i370_no_pseudo_dot = false;
bool i370_labels_without_colons = false;
bool i370_convert_dc_to_ebcdic = false;


/* Generic assembler global variables which must be defined by all
   targets.  */

#ifdef OBJ_ELF
/* This string holds the chars that always start a comment.  If the
   pre-processor is disabled, these aren't very useful.  The macro
   tc_comment_chars points to this.  We use this, rather than the
   usual comment_chars, so that we can switch for Solaris conventions.  */
static const char i370_eabi_comment_chars[] = "#";

const char *i370_comment_chars = i370_eabi_comment_chars;
#else
const char comment_chars[] = "#";
#endif

/* Characters which start a comment at the beginning of a line.  */
const char line_comment_chars[] = "#*";

/* Characters which may be used to separate multiple commands on a
   single line.  */
const char line_separator_chars[] = ";";

/* Characters which are used to indicate an exponent in a floating
   point number.  */
const char EXP_CHARS[] = "eE";

/* Characters which mean that a number is a floating point constant,
   as in 0d1.0.  */
const char FLT_CHARS[] = "dD";

void
md_show_usage (FILE *stream)
{
  fprintf (stream, "\
S/370 options:\n\
-mhlasm         	Operate in HLASM-compatible mode\n\
-mebcdic         	Convert DC C'foo' to EBCDIC before output\n\
-mregnames      	Allow symbolic names for registers\n\
-mno-regnames   	Do not allow symbolic names for registers\n\
-u              	ignored\n");

#ifdef OBJ_ELF
  fprintf (stream, "\
-mrelocatable        	support for GCC's -mrelocatble option\n\
-mrelocatable-lib       support for GCC's -mrelocatble-lib option\n\
-V        		print assembler version number\n");
#endif
}

/* Whether to use user friendly register names.  */
#define TARGET_REG_NAMES_P true

static bool reg_names_p = TARGET_REG_NAMES_P;


/* Predefined register names if -mregnames
   In general, there are lots of them, in an attempt to be compatible
   with a number of assemblers.  */

/* Structure to hold information about predefined registers.  */
struct pd_reg
  {
    const char *name;
    int value;
  };

/* List of registers that are pre-defined:

   Each general register has predefined names of the form:
   1. r<reg_num> which has the value <reg_num>.
   2. r.<reg_num> which has the value <reg_num>.

   Each floating point register has predefined names of the form:
   1. f<reg_num> which has the value <reg_num>.
   2. f.<reg_num> which has the value <reg_num>.

   There are only four floating point registers, and these are
   commonly labelled 0,2,4 and 6.  Thus, there is no f1, f3, etc.

   There are individual registers as well:
   rbase or r.base has the value  3  (base register)
   rpgt or r.pgt   has the value  4  (page origin table pointer)
   rarg or r.arg   has the value 11  (argument pointer)
   rtca or r.tca   has the value 12  (table of contents pointer)
   rtoc or r.toc   has the value 12  (table of contents pointer)
   sp or r.sp      has the value 13  (stack pointer)
   dsa or r.dsa    has the value 13  (stack pointer)
   lr              has the value 14  (link reg)

   The table is sorted. Suitable for searching by a binary search.  */

static const struct pd_reg pre_defined_registers[] =
{
  { "arg", 11 },   /* Argument Pointer.  */
  { "base", 3 },   /* Base Reg.  */

  { "dsa", 13 },   /* Stack pointer */

  { "f.0", 0 },    /* Floating point registers.  */
  { "f.2", 2 },
  { "f.4", 4 },
  { "f.6", 6 },

  { "f0", 0 },
  { "f2", 2 },
  { "f4", 4 },
  { "f6", 6 },

  { "lr", 14 },    /* Link Register.  */
  { "pgt", 4 },    /* Page Origin Table Pointer.  */

  { "r.0", 0 },    /* General Purpose Registers.  */
  { "r.1", 1 },
  { "r.10", 10 },
  { "r.11", 11 },
  { "r.12", 12 },
  { "r.13", 13 },
  { "r.14", 14 },
  { "r.15", 15 },
  { "r.2", 2 },
  { "r.3", 3 },
  { "r.4", 4 },
  { "r.5", 5 },
  { "r.6", 6 },
  { "r.7", 7 },
  { "r.8", 8 },
  { "r.9", 9 },

  { "r.arg", 11 },  /* Argument Pointer.  */
  { "r.base", 3 },  /* Base Reg.  */
  { "r.dsa", 13 },  /* Stack Pointer.  */
  { "r.pgt", 4 },   /* Page Origin Table Pointer.  */
  { "r.sp", 13 },   /* Stack Pointer.  */

  { "r.tca", 12 },  /* Pointer to the table of contents.  */
  { "r.toc", 12 },  /* Pointer to the table of contents.  */

#if !defined(HOST_EBCDIC)
  /* ASCII sort order, numbers < chars */
  { "r0", 0 },      /* More general purpose registers.  */
  { "r1", 1 },
  { "r10", 10 },
  { "r11", 11 },
  { "r12", 12 },
  { "r13", 13 },
  { "r14", 14 },
  { "r15", 15 },
  { "r2", 2 },
  { "r3", 3 },
  { "r4", 4 },
  { "r5", 5 },
  { "r6", 6 },
  { "r7", 7 },
  { "r8", 8 },
  { "r9", 9 },
#endif

  { "rbase", 3 },  /* Base Reg.  */

  { "rtca", 12 },  /* Pointer to the table of contents.  */
  { "rtoc", 12 },  /* Pointer to the table of contents.  */

#if defined(HOST_EBCDIC)
  /* EBCDIC sort order, numbers > chars */
  { "r0", 0 },      /* More general purpose registers.  */
  { "r1", 1 },
  { "r10", 10 },
  { "r11", 11 },
  { "r12", 12 },
  { "r13", 13 },
  { "r14", 14 },
  { "r15", 15 },
  { "r2", 2 },
  { "r3", 3 },
  { "r4", 4 },
  { "r5", 5 },
  { "r6", 6 },
  { "r7", 7 },
  { "r8", 8 },
  { "r9", 9 },
#endif

  { "sp", 13 },   /* Stack Pointer.  */
};

#define REG_NAME_CNT        (sizeof (pre_defined_registers) / sizeof (struct pd_reg))

/* Given NAME, find the register number associated with that name, return
   the integer value associated with the given name or -1 on failure.  */

static int
reg_name_search (const struct pd_reg *regs,
		 int regcount,
		 const char *name)
{
  int middle, low, high;
  int cmp;

  low = 0;
  high = regcount - 1;

  do
    {
      middle = (low + high) / 2;
      cmp = strcasecmp (name, regs[middle].name);
      if (cmp < 0)
        high = middle - 1;
      else if (cmp > 0)
        low = middle + 1;
      else
        return regs[middle].value;
    }
  while (low <= high);

  return -1;
}

/* register_name(): parse expression containing a register name.

   in:  Input_line_pointer points to 1st char of operand.

   out: An expressionS.

   The operand may have been a register: in this case, X_op == O_register,
   X_add_number is set to the register number, and truth is returned.
   Input_line_pointer->(next non-blank) char after operand, or is in its
   original state.

   A variety of RS-form instructions accept masks where R2 should be,
   for example CLM Compare Logical Characters Under Mask. For these
   cases, we just pretend the mask is a register; it still has to be
   between 0 and 15.  Nothing else in the assembler cares about this.
  */

static bool
register_name (expressionS *expressionP)
{
  int reg_number;
  char *name;
  char *start;
  char c;

  /* Allow percent sign, so %r13 and %13 as register names.  */
  start = name = input_line_pointer;
  if (name[0] == '%' && ISALPHA (name[1]))
    name = ++input_line_pointer;

  /* else if (!reg_names_p) return false; */

  while (' ' == *name)
    name = ++input_line_pointer;

  /* If it's a number, treat it as a number.  If it's alpha, then either
   * its a binary mask B'0101' or it's a register name in the register
   * table.  */
  if (!ISALPHA (name[0]))
    reg_number = get_single_number ();
  else if ('B' == name[0])
    {
      int i;
      /* Brute-force binary */
      reg_number = 0;
      for (i=0; i<4; i++)
	if ('1' == name[i+2])
	  reg_number |= (1 << (3-i));
	else if ('0' != name[i+2])
	  as_bad(_("expecting binary digit"));
      if (name[1] != name[6])
	  as_bad(_("expecting quoted 4-bit binary bitmask"));
      input_line_pointer += 7;  /* Total length of B'0101' */
    }
  else
    {
      c = get_symbol_name (&name);
      reg_number = reg_name_search (pre_defined_registers, REG_NAME_CNT, name);

      /* HLASM compat: user might stated that `MYREG6 EQU 6` and then
       * used this in an insn: `L MYREG6,=A(foo)`, So we look up the
       * name and use that value, if we find it.  */
      if (reg_number < 0)
	{
	  symbolS* equP;
	  equP = symbol_find(name);
	  if (equP)
	    reg_number = S_GET_VALUE(equP);
	}

      /* Put back the delimiting char.  */
      (void) restore_line_pointer (c);
    }

  /* If numeric, make sure it's not out of bounds.  */
  if ((0 <= reg_number) && (16 >= reg_number))
    {
      expressionP->X_op = O_register;
      expressionP->X_add_number = reg_number;

      /* Make the rest nice.  */
      expressionP->X_add_symbol = NULL;
      expressionP->X_op_symbol = NULL;
      return true;
    }

  /* Reset the line as if we had not done anything.  */
  input_line_pointer = start;
  return false;
}

/* Local variables.  */

/* The type of processor we are assembling for.  This is one or more
   of the I370_OPCODE flags defined in opcode/i370.h.  */
static int i370_cpu = 0;

/* The base register to use for opcode with optional operands.
   We define two of these: "text" and "other".  Normally, "text"
   would get used in the .text section for branches, while "other"
   gets used in the .data section for address constants.

   The idea of a second base register in a different section
   is foreign to the usual HLASM-style semantics; however, it
   allows us to provide support for dynamically loaded libraries,
   by allowing us to place address constants in a section other
   than the text section. The "other" section need not be the
   .data section, it can be any section that isn't the .text section.

   Note that HLASM defines a multiple, concurrent .using semantic
   that we do not: in calculating offsets, it uses either the most
   recent .using directive, or the one with the smallest displacement.
   This allows HLASM to support a quasi-block-scope-like behaviour.
   Handy for people writing assembly by hand ... but not supported
   by us.  */
static int i370_using_text_regno = -1;
static int i370_using_other_regno = -1;

/* The base address for address literals.  */
static expressionS i370_using_text_baseaddr;
static expressionS i370_using_other_baseaddr;

/* the "other" section, used only for syntax error detection.  */
static segT i370_other_section = undefined_section;

/* Opcode hash table.  */
static htab_t i370_hash = NULL;

/* Macro hash table.  */
static htab_t i370_macro_hash = NULL;

#ifdef OBJ_ELF
/* What type of shared library support to use.  */
static enum { SHLIB_NONE, SHLIB_PIC, SHILB_MRELOCATABLE } shlib = SHLIB_NONE;
#endif

/* Flags to set in the elf header.  */
static flagword i370_flags = 0;

#ifndef WORKING_DOT_WORD
int md_short_jump_size = 4;
int md_long_jump_size = 4;
#endif

#ifdef OBJ_ELF
const char *md_shortopts = "l:um:K:VQ:";
#else
const char *md_shortopts = "um:";
#endif
struct option md_longopts[] =
{
  {NULL, no_argument, NULL, 0}
};
size_t md_longopts_size = sizeof (md_longopts);

int
md_parse_option (int c, const char *arg)
{
  switch (c)
    {
    case 'u':
      /* -u means that any undefined symbols should be treated as
         external, which is the default for gas anyhow.  */
      break;

#ifdef OBJ_ELF
    case 'K':
      /* Recognize -K PIC */
      if (strcmp (arg, "PIC") == 0 || strcmp (arg, "pic") == 0)
        {
          shlib = SHLIB_PIC;
          i370_flags |= EF_I370_RELOCATABLE_LIB;
        }
      else
        return 0;

      break;
#endif

    case 'm':
      /* -mhlasm means means HLASM compatibility mode.  */
      if (strcmp (arg, "hlasm") == 0)
	{
	  reg_names_p = false;
	  i370_no_pseudo_dot = true;
	  i370_labels_without_colons = true;

	  /* Current default is ascii */
	  i370_convert_dc_to_ebcdic = false;
	}

      else if (strcmp (arg, "ebcdic") == 0)
	{
	  i370_convert_dc_to_ebcdic = true;
	}

      /* -m360 means to assemble for the ancient 360 architecture.  */
      else if (strcmp (arg, "360") == 0 || strcmp (arg, "i360") == 0)
	i370_cpu = I370_OPCODE_360;
      /* -mxa means to assemble for the IBM 370 XA.  */
      else if (strcmp (arg, "xa") == 0)
	i370_cpu = I370_OPCODE_370_XA;
      /* -many means to assemble for any architecture (370/XA).  */
      else if (strcmp (arg, "any") == 0)
	i370_cpu = I370_OPCODE_370;

      else if (strcmp (arg, "regnames") == 0)
	reg_names_p = true;

      else if (strcmp (arg, "no-regnames") == 0)
	reg_names_p = false;

#ifdef OBJ_ELF
      /* -mrelocatable/-mrelocatable-lib -- warn about
	 initializations that require relocation.  */
      else if (strcmp (arg, "relocatable") == 0)
        {
          shlib = SHILB_MRELOCATABLE;
          i370_flags |= EF_I370_RELOCATABLE;
        }
      else if (strcmp (arg, "relocatable-lib") == 0)
        {
          shlib = SHILB_MRELOCATABLE;
          i370_flags |= EF_I370_RELOCATABLE_LIB;
        }
#endif
      else
        {
          as_bad (_("invalid switch -m%s"), arg);
          return 0;
        }
      break;

#ifdef OBJ_ELF
      /* -V: SVR4 argument to print version ID.  */
    case 'V':
      print_version_id ();
      break;

      /* -Qy, -Qn: SVR4 arguments controlling whether a .comment section
         should be emitted or not.  FIXME: Not implemented.  */
    case 'Q':
      break;

#endif

    default:
      return 0;
    }

  return 1;
}


/* Set i370_cpu if it is not already set.
   Currently defaults to the reasonable superset;
   but can be made more fine grained if desired.  */

static void
i370_set_cpu (void)
{
  const char *default_os  = TARGET_OS;
  const char *default_cpu = TARGET_CPU;

  /* Override with the superset for the moment.  */
  i370_cpu = I370_OPCODE_ZARCH_SUPERSET;
  if (i370_cpu == 0)
    {
      if (strcmp (default_cpu, "i360") == 0)
        i370_cpu = I370_OPCODE_360;
      else if (strcmp (default_cpu, "i370") == 0)
        i370_cpu = I370_OPCODE_370;
      else if (strcmp (default_cpu, "XA") == 0)
        i370_cpu = I370_OPCODE_370_XA;
      else if (strcmp (default_cpu, "ESA") == 0)
        i370_cpu = I370_OPCODE_ESA390;
      else if (strcmp (default_cpu, "ESA390") == 0)
        i370_cpu = I370_OPCODE_ESA390;
      else if (strcmp (default_cpu, "zArch") == 0)
        i370_cpu = I370_OPCODE_ZARCH;
      else
        as_fatal ("Unknown default cpu = %s, os = %s", default_cpu, default_os);
    }
}

/* Figure out the BFD architecture to use.
   FIXME: specify the different 370 architectures.  */

enum bfd_architecture
i370_arch (void)
{
   return bfd_arch_i370;
}

/* This function is called when the assembler starts up.  It is called
   after the options have been parsed and the output file has been
   opened.  */

void
md_begin (void)
{
  const struct i370_opcode *op;
  const struct i370_opcode *op_end;
  const struct i370_macro *macro;
  const struct i370_macro *macro_end;
  bool dup_insn = false;

  i370_set_cpu ();

#ifdef OBJ_ELF
  /* Set the ELF flags if desired.  */
  if (i370_flags)
    bfd_set_private_flags (stdoutput, i370_flags);
#endif

  /* Insert the opcodes into a hash table.  */
  i370_hash = str_htab_create ();

   op_end = i370_opcodes + i370_num_opcodes;
   for (op = i370_opcodes; op < op_end; op++)
     {
       know ((op->opcode.i[0] & op->mask.i[0]) == op->opcode.i[0]
	     && (op->opcode.i[1] & op->mask.i[1]) == op->opcode.i[1]);

       if ((op->flags & i370_cpu) != 0)
         {
           if (str_hash_insert (i370_hash, op->name, (void *) op, 0) != NULL)
             {
               as_bad (_("Internal assembler error for instruction %s"), op->name);
               dup_insn = true;
             }
         }
     }

  /* Insert the macros into a hash table.  */
  i370_macro_hash = str_htab_create ();

  macro_end = i370_macros + i370_num_macros;
  for (macro = i370_macros; macro < macro_end; macro++)
    {
      if ((macro->flags & i370_cpu) != 0)
        {
          if (str_hash_insert (i370_macro_hash, macro->name, (void *) macro, 0) != NULL)
            {
              as_bad (_("Internal assembler error for macro %s"), macro->name);
              dup_insn = true;
            }
        }
    }

  if (dup_insn)
    abort ();
}

/* Insert an operand value into an instruction.  */

static i370_insn_t
i370_insert_operand (i370_insn_t insn,
		     const struct i370_operand *operand,
		     offsetT val)
{
  if (operand->insert)
    {
      const char *errmsg;

      /* Used for 48-bit insn's.  */
      errmsg = NULL;
      insn = (*operand->insert) (insn, (long) val, &errmsg);
      if (errmsg)
        as_bad ("%s", errmsg);
    }
  else
    /* This is used only for 16, 32 bit insn's.  */
    insn.i[0] |= (((long) val & ((1 << operand->bits) - 1))
		  << operand->shift);

  return insn;
}


#ifdef OBJ_ELF_SUFFIX
/* Parse @got, etc. and return the desired relocation.
   Currently, i370 does not support (don't really need to support) any
   of these fancier markups ... for example, no one is going to
   write 'L 6,=V(bogus)@got' it just doesn't make sense (at least to me).
   So basically, we could get away with this routine returning
   BFD_RELOC_UNUSED in all circumstances.  However, I'll leave
   in for now in case someone ambitious finds a good use for this stuff ...
   this routine was pretty much just copied from the powerpc code ...  */

static bfd_reloc_code_real_type
i370_elf_suffix (char **str_p, expressionS *exp_p)
{
  struct map_bfd
  {
    const char *string;
    int length;
    bfd_reloc_code_real_type reloc;
  };

  char ident[20];
  char *str = *str_p;
  char *str2;
  int ch;
  int len;
  struct map_bfd *ptr;

#define MAP(str,reloc) { str, sizeof (str) - 1, reloc }

  static struct map_bfd mapping[] =
  {
    /* warnings with -mrelocatable.  */
    MAP ("fixup",	BFD_RELOC_CTOR),
    { (char *)0, 0,	BFD_RELOC_UNUSED }
  };

  if (*str++ != '@')
    return BFD_RELOC_UNUSED;

  for (ch = *str, str2 = ident;
       (str2 < ident + sizeof (ident) - 1
        && (ISALNUM (ch) || ch == '@'));
       ch = *++str)
    *str2++ = TOLOWER (ch);

  *str2 = '\0';
  len = str2 - ident;

  ch = ident[0];
  for (ptr = &mapping[0]; ptr->length > 0; ptr++)
    if (ch == ptr->string[0]
        && len == ptr->length
        && memcmp (ident, ptr->string, ptr->length) == 0)
      {
        if (exp_p->X_add_number != 0
            && (ptr->reloc == BFD_RELOC_16_GOTOFF
        	|| ptr->reloc == BFD_RELOC_LO16_GOTOFF
        	|| ptr->reloc == BFD_RELOC_HI16_GOTOFF
        	|| ptr->reloc == BFD_RELOC_HI16_S_GOTOFF))
          as_warn (_("identifier+constant@got means identifier@got+constant"));

        /* Now check for identifier@suffix+constant */
        if (*str == '-' || *str == '+')
          {
            char *orig_line = input_line_pointer;
            expressionS new_exp;

            input_line_pointer = str;
            expression (&new_exp);
            if (new_exp.X_op == O_constant)
              {
        	exp_p->X_add_number += new_exp.X_add_number;
        	str = input_line_pointer;
              }

            if (&input_line_pointer != str_p)
              input_line_pointer = orig_line;
          }

        *str_p = str;
        return ptr->reloc;
      }

  return BFD_RELOC_UNUSED;
}

/* Like normal .long/.short/.word, except support @got, etc.
   Clobbers input_line_pointer, checks end-of-line.  */

static void
i370_elf_cons (int nbytes)   /* 1=.byte, 2=.word, 4=.long.  */
{
  expressionS xexp;
  bfd_reloc_code_real_type reloc;

  if (is_it_end_of_statement ())
    {
      demand_empty_rest_of_line ();
      return;
    }

  do
    {
      expression (&xexp);
      if (xexp.X_op == O_symbol
          && *input_line_pointer == '@'
          && (reloc = i370_elf_suffix (&input_line_pointer, &xexp)) != BFD_RELOC_UNUSED)
        {
          reloc_howto_type *reloc_howto = bfd_reloc_type_lookup (stdoutput, reloc);
          int size = bfd_get_reloc_size (reloc_howto);

          if (size > nbytes)
	    as_bad (ngettext ("%s relocations do not fit in %u byte",
			      "%s relocations do not fit in %u bytes",
			      nbytes),
		    reloc_howto->name, nbytes);
          else
            {
              char *p = frag_more ((int) nbytes);
              int offset = nbytes - size;

              fix_new_exp (frag_now, p - frag_now->fr_literal + offset, size, &xexp, 0, reloc);
            }
        }
      else
        emit_expr (&xexp, (unsigned int) nbytes);
    }
  while (*input_line_pointer++ == ',');

  input_line_pointer--;        	/* Put terminator back into stream.  */
  demand_empty_rest_of_line ();
}
#endif /* OBJ_ELF_SUFFIX */

#ifdef OBJ_ELF


/* ASCII to EBCDIC conversion table.  */
static unsigned char ascebc[256] =
{
 /*00  NL    SH    SX    EX    ET    NQ    AK    BL */
     0x00, 0x01, 0x02, 0x03, 0x37, 0x2D, 0x2E, 0x2F,
 /*08  BS    HT    LF    VT    FF    CR    SO    SI */
     0x16, 0x05, 0x15, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
 /*10  DL    D1    D2    D3    D4    NK    SN    EB */
     0x10, 0x11, 0x12, 0x13, 0x3C, 0x3D, 0x32, 0x26,
 /*18  CN    EM    SB    EC    FS    GS    RS    US */
     0x18, 0x19, 0x3F, 0x27, 0x1C, 0x1D, 0x1E, 0x1F,
 /*20  SP     !     "     #     $     %     &     ' */
     0x40, 0x5A, 0x7F, 0x7B, 0x5B, 0x6C, 0x50, 0x7D,
 /*28   (     )     *     +     ,     -    .      / */
     0x4D, 0x5D, 0x5C, 0x4E, 0x6B, 0x60, 0x4B, 0x61,
 /*30   0     1     2     3     4     5     6     7 */
     0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,
 /*38   8     9     :     ;     <     =     >     ? */
     0xF8, 0xF9, 0x7A, 0x5E, 0x4C, 0x7E, 0x6E, 0x6F,
 /*40   @     A     B     C     D     E     F     G */
     0x7C, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,
 /*48   H     I     J     K     L     M     N     O */
     0xC8, 0xC9, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6,
 /*50   P     Q     R     S     T     U     V     W */
     0xD7, 0xD8, 0xD9, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6,
 /*58   X     Y     Z     [     \     ]     ^     _ */
     0xE7, 0xE8, 0xE9, 0xAD, 0xE0, 0xBD, 0x5F, 0x6D,
 /*60   `     a     b     c     d     e     f     g */
     0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
 /*68   h     i     j     k     l     m     n     o */
     0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96,
 /*70   p     q     r     s     t     u     v     w */
     0x97, 0x98, 0x99, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6,
 /*78   x     y     z     {     |     }     ~    DL */
     0xA7, 0xA8, 0xA9, 0xC0, 0x4F, 0xD0, 0xA1, 0x07,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F,
     0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0x3F, 0xFF
};

/* EBCDIC to ASCII conversion table.  */
unsigned char ebcasc[256] =
{
 /*00  NU    SH    SX    EX    PF    HT    LC    DL */
     0x00, 0x01, 0x02, 0x03, 0x00, 0x09, 0x00, 0x7F,
 /*08              SM    VT    FF    CR    SO    SI */
     0x00, 0x00, 0x00, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
 /*10  DE    D1    D2    TM    RS    NL    BS    IL */
     0x10, 0x11, 0x12, 0x13, 0x14, 0x0A, 0x08, 0x00,
 /*18  CN    EM    CC    C1    FS    GS    RS    US */
     0x18, 0x19, 0x00, 0x00, 0x1C, 0x1D, 0x1E, 0x1F,
 /*20  DS    SS    FS          BP    LF    EB    EC */
     0x00, 0x00, 0x00, 0x00, 0x00, 0x0A, 0x17, 0x1B,
 /*28              SM    C2    EQ    AK    BL       */
     0x00, 0x00, 0x00, 0x00, 0x05, 0x06, 0x07, 0x00,
 /*30              SY          PN    RS    UC    ET */
     0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04,
 /*38                    C3    D4    NK          SU */
     0x00, 0x00, 0x00, 0x00, 0x14, 0x15, 0x00, 0x1A,
 /*40  SP                                           */
     0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*48                     .     <     (     +     | */
     0x00, 0x00, 0x00, 0x2E, 0x3C, 0x28, 0x2B, 0x7C,
 /*50   &                                           */
     0x26, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*58               !     $     *     )     ;     ^ */
     0x00, 0x00, 0x21, 0x24, 0x2A, 0x29, 0x3B, 0x5E,
 /*60   -     /                                     */
     0x2D, 0x2F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*68                     ,     %     _     >     ? */
     0x00, 0x00, 0x00, 0x2C, 0x25, 0x5F, 0x3E, 0x3F,
 /*70                                               */
     0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*78         `     :     #     @     '     =     " */
     0x00, 0x60, 0x3A, 0x23, 0x40, 0x27, 0x3D, 0x22,
 /*80         a     b     c     d     e     f     g */
     0x00, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
 /*88   h     i           {                         */
     0x68, 0x69, 0x00, 0x7B, 0x00, 0x00, 0x00, 0x00,
 /*90         j     k     l     m     n     o     p */
     0x00, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F, 0x70,
 /*98   q     r           }                         */
     0x71, 0x72, 0x00, 0x7D, 0x00, 0x00, 0x00, 0x00,
 /*A0         ~     s     t     u     v     w     x */
     0x00, 0x7E, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
 /*A8   y     z                       [             */
     0x79, 0x7A, 0x00, 0x00, 0x00, 0x5B, 0x00, 0x00,
 /*B0                                               */
     0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*B8                                 ]             */
     0x00, 0x00, 0x00, 0x00, 0x00, 0x5D, 0x00, 0x00,
 /*C0   {     A     B     C     D     E     F     G */
     0x7B, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
 /*C8   H     I                                     */
     0x48, 0x49, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*D0   }     J     K     L     M     N     O     P */
     0x7D, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F, 0x50,
 /*D8   Q     R                                     */
     0x51, 0x52, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*E0   \           S     T     U     V     W     X */
     0x5C, 0x00, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
 /*E8   Y     Z                                     */
     0x59, 0x5A, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
 /*F0   0     1     2     3     4     5     6     7 */
     0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
 /*F8   8     9                                     */
     0x38, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF
};

/* EBCDIC translation tables needed for 3270 support.  */

static void
i370_rebcdic (int do_convert, int nrepeat)
{
  int i;
  char *p, *ps, *end;
  char delim = 0;
  size_t nbytes;

  delim = *input_line_pointer;
  if (('\'' == delim) || ('\"' == delim)) input_line_pointer++;
  end = input_line_pointer;
  while (('\r' != *end) && ('\n' != *end)) end++;
  end --;
  if (delim != *end) end++;
  *end = '\0';
  nbytes = end - input_line_pointer;

  p = frag_more (nbytes * nrepeat);
  ps = p;
  while (end > input_line_pointer)
    {
      if (do_convert)
	*p = ascebc [(unsigned char) (*input_line_pointer)];
      else
	*p = *input_line_pointer;

      ++p; ++input_line_pointer;
    }
  ++input_line_pointer;

  /* Now, repeat, if needed */
  for (i=1; i<nrepeat; i++)
    {
      memcpy(p, ps, nbytes);
      p += nbytes;
    }
}

static void
i370_ebcdic (int do_convert)
{
  i370_rebcdic(do_convert, 1);
}


/* Stub out a couple of routines.  */

static void
i370_rmode (int unused ATTRIBUTE_UNUSED)
{
  as_tsktsk ("rmode ignored");
}

/* DSECT Data Section is (more or less) the same thing an
   ELF .data section.  */

static void
i370_dsect (int sect)
{
  char *save_line = input_line_pointer;
  static char section[] = ".data\n";

  /* Just pretend this is .section .data.  */
  input_line_pointer = section;
  obj_elf_section (sect);

  input_line_pointer = save_line;
}

/* CSECT Code Section is (more or less) the same thing an
   ELF .text section.  */

static void
i370_csect (int sect)
{
  char *save_line = input_line_pointer;
  static char section[] = ".text\n";

  /* Just pretend this is .section .text.  */
  input_line_pointer = section;
  obj_elf_section (sect);

  input_line_pointer = save_line;
}


/* HLASM externals start with two @@. Whenever we see one of these,
   convert the @@ to underscores, and downcase the rest of the name.
   FIXME: We probably shouldn't do this, if the target isn't HLASM...  */

char * i370_canonicalize_symbol_name (char *name)
{
  size_t i, namelen;
  if ('@' == name[0])
    {
      name[0] = '_';
      if ('@' == name[1])
	name[1] = '_';

      /* Downcase, also */
      namelen = strlen(name);
      for (i=1; i<namelen; i++)
	name[i] = TOLOWER(name[i]);
    }
  return name;
}

/* Provide minimal support for HLASM externals.

   For example,
       ENTRY @@CRT0
   should be translated to
       ENTRY __crt0
   Note the "@" is translated to "_" and the rest of the external
   identifier is translated to lowercase. This is done by
   i370_canonicalize_symbol_name() above.

   This is meant to be more or less the same as the elf `.globl __crt0`
   See the function s_globl() in read.c for a fancy example.
*/

static void
i370_entry (int unused ATTRIBUTE_UNUSED)
{
  char *name;
  char *end;
  char sav;
  symbolS *symbolP;

  name = input_line_pointer;
  end = strpbrk(input_line_pointer, " \r\n");
  if (NULL == end)
    as_bad(_("enexpected end of file"));

  sav = *end;
  *end = 0x0;
  input_line_pointer = end;

  symbolP = symbol_find_or_make (name);
  S_SET_EXTERNAL (symbolP);

  *end = sav;
  demand_empty_rest_of_line ();

  dwarf2_emit_label(symbolP);
}


#define BIGNUM_CACHE 8    /* Eight LITTLENUM_TYPE's so 16 bytes total */

/* Create IBM Floating Point Decimal
   TODO: Implement me!  */
static void
gen_to_decfloat_words (LITTLENUM_TYPE *words, int type ATTRIBUTE_UNUSED)
{
  memset(words, 0, BIGNUM_CACHE * sizeof(LITTLENUM_TYPE));

  as_bad (_("IBM Decimal floats currently not supported"));
}

/* Create IBM Floating Point Hex. This is probably wrong,
   Its a crude approximation for what it should be.
 */
static void
gen_to_hexfloat_words (LITTLENUM_TYPE *words, int type)
{
  memset(words, 0, BIGNUM_CACHE * sizeof(LITTLENUM_TYPE));

  /* The below is incorrect, but vaguely approximate for
     the IBM hex format. It generates 8-bit exponents and
     24 or 56-bit mantissas.
     See atof-ieee.c gen_to_words() for example.
     It's ... complicated. */
  if ('E' == type)
    {
      gen_to_words(words, 2, 8);
    }
  else if ('D' == type)
    {
      gen_to_words(words, 4, 8);
    }
  else if ('L' == type)
    {
      as_bad("long double precision not supported");
      // gen_to_words(words, 4, 8);
    }
}

static int
i370_parse_const (expressionS *xexp)
{
  char *name;
  char delim;
  int cons_len = 0;

  name = input_line_pointer;
  switch (name[0])
    {
    case 'A': /* A == address-of.  */
    case 'V': /* V == extern.  */
      ++input_line_pointer; /* Move past open-paren before parsing */
      expression (xexp);
      cons_len = strcspn (name, ", \n\r"); /* len includes foo */
      return cons_len;

    case 'B':  /* Binary bitstring */
    case 'H':
    case 'F':
    case 'X':
    case 'E':  /* Single-precision float point.  */
    case 'D':  /* Double-precision float point.  */
    case 'L':  /* 128-bit float point.  */
      break;
    default:
      as_bad (_("Invalid constant expression"));
    }

    /* H == 16-bit decimal fixed-point const; expression must be const.  */
    /* F == decimal fixed-point const; expression must be const.  */
    /* X == hex fixed-point const; expression must be const.  */
    if ('H' == name[0]) cons_len = 2;
    else if ('F' == name[0]) cons_len = 4;
    else if ('X' == name[0]) cons_len = -1;
    else if ('B' == name[0]) cons_len = -1;
    else if ('E' == name[0]) cons_len = 4;
    else if ('D' == name[0]) cons_len = 8;
    else if ('L' == name[0]) cons_len = 16;

    /* Extract length, if it is present;
       FIXME: assume single-digit length.  */
    if ('L' == name[1])
      {
	/* Should work for ASCII and EBCDIC.  */
	cons_len = name[2] - '0';
	input_line_pointer += 2;
      }

    /* Advance past subtype modifier for float point */
    if (('E' == name[0] || 'D' == name[0] || 'L' == name[0]) &&
	('B' == name[1] || 'H' == name[1] || 'D' == name[1]))
      input_line_pointer++;

    ++input_line_pointer;

    /* Get rid of pesky quotes.  */
    delim = *input_line_pointer;
    if (('\'' == delim) || ('\"' == delim))
      {
	char * clse;

	++input_line_pointer;
	clse = strchr (input_line_pointer, delim);
	if (clse)
	  *clse= ' ';
	else
	  as_bad (_("missing end-quote"));
      }

    if (('X' == name[0]) || ('E' == name[0]) ||
        ('D' == name[0]) || ('L' == name[0]))
      {
#define TMPSZ 50
	char tmp[TMPSZ];
	char *save;

	/* The length of hex constants is specified directly with L,
	   or implied through the number of hex digits. For example:
	   X'AB'       one byte
	   X'abcd'     two bytes
	   X'000000AB' four bytes
	   XL4'AB'     four bytes, left-padded with zero.  */
	if (('X' == name[0]) && (0 > cons_len))
	  {
	    int hex_len = 0;
	    save = input_line_pointer;
	    while (*save && ISXDIGIT (*save))
	      {
		hex_len++;
		save++;
	      }
	    cons_len = (hex_len+1) / 2;
	  }

	/* Just like above, but for bitstrings. For example:
	   B'1100'     one byte 0x0c
	   B'01011100' one byte 0x5c */
	if (('B' == name[0]) && (0 > cons_len))
	  {
	    int bit_len = 0;
	    save = input_line_pointer;
	    while (*save && ISBDIGIT (*save))
	      {
		bit_len++;
		save++;
	      }
	    cons_len = (bit_len+7) / 8;
	  }

	/* I believe this works even for XL8'dada0000beeebaaa'
	   which should parse out to X_op == O_big
	   Note that floats and doubles get represented as
	   0d3.14159265358979  or 0f 2.7.  */
	tmp[0] = '0';
	tmp[1] = name[0];
	tmp[2] = 0;
	strncat (tmp, input_line_pointer, TMPSZ-5);
	save = input_line_pointer;
	input_line_pointer = tmp;
	expression (xexp);
	input_line_pointer = save + (input_line_pointer-tmp-2);

	/* Fix up lengths for floats and doubles.  */
	if (O_big == xexp->X_op)
	  {
	    LITTLENUM_TYPE fltnum[BIGNUM_CACHE];
	    xexp->X_add_number = cons_len / CHARS_PER_LITTLENUM;

	    /* Convert generic_floating_point_number to bignum.
	     * The add_to_lit_pool will cache this, for later playback. */
	    if ('E' == name[0]) /* 32-bit float */
	      {
	        if ('B' == name[1]) /* IEEE float */
		  gen_to_words(fltnum, 2, 8);
	        else if ('D' == name[1]) /* Decimal */
		  gen_to_decfloat_words(fltnum, name[0]);
	        else /* Assume "Floating Point Hex" aka old-style IBM */
		  gen_to_hexfloat_words(fltnum, name[0]);

	        generic_bignum[0] = fltnum[1];
	        generic_bignum[1] = fltnum[0];
	      }
	    else if ('D' == name[0]) /* 64-bit double */
	      {
	        if ('B' == name[1]) /* IEEE double */
		  gen_to_words(fltnum, 4, 11);
	        else if ('D' == name[1]) /* Decimal */
		  gen_to_decfloat_words(fltnum, name[0]);
	        else /* Assume "Floating Point Hex" aka old-style IBM */
		  gen_to_hexfloat_words(fltnum, name[0]);

	        /* This is correct if host is LE, but what if host is BE? */
	        generic_bignum[0] = fltnum[3];
	        generic_bignum[1] = fltnum[2];
	        generic_bignum[2] = fltnum[1];
	        generic_bignum[3] = fltnum[0];
	      }
	    else if ('L' == name[0]) /* 128-bit double */
	      {
	        if ('B' == name[1]) /* IEEE double */
		  gen_to_words(fltnum, 8, 15);
	        else if ('D' == name[1]) /* Decimal */
		  gen_to_decfloat_words(fltnum, name[0]);
	        else /* Assume "Floating Point Hex" aka old-style IBM */
		  gen_to_hexfloat_words(fltnum, name[0]);

	        /* This is correct if host is LE, but what if host is BE? */
	        generic_bignum[0] = fltnum[7];
	        generic_bignum[1] = fltnum[6];
	        generic_bignum[2] = fltnum[5];
	        generic_bignum[3] = fltnum[4];
	        generic_bignum[4] = fltnum[3];
	        generic_bignum[5] = fltnum[2];
	        generic_bignum[6] = fltnum[1];
	        generic_bignum[7] = fltnum[0];
	      }
	  }
      }
    else
      expression (xexp);

    /* O_big occurs when more than 4 bytes worth gets parsed.  */
    if ((xexp->X_op != O_constant) && (xexp->X_op != O_big))
      as_bad (_("expression not a constant"));

  return cons_len;
}

static void
i370_dc_align (char type)
{
  int align2 = 0;  /* Power of two alignment. */

  switch (type)
    {
    case 'C':  /* No alignment */
      align2 = 0;
      break;

    case 'H':  /* 16-bit decimal */
      align2 = 1;
      break;

    case 'F':  /* 32-bit decimal */
    case 'E':  /* 32-bit float */
    case 'A':  /* Address of label */
    case 'V':  /* External Address */
      align2 = 2;
      break;

    case 'X':  /* variable length hex */
      align2 = 0; /* Not aligned. Used for strings */
      break;

    case 'D':  /* 64-bit double */
    case 'L':  /* 128-bit long double */
      align2 = 3;
      break;

    default:
      as_bad (_("unsupported DC type"));
      return;
    }

  if (0 < align2)
    {
      frag_align (align2, 0, 0);
      record_alignment (now_seg, align2);
    }
}

/* DC Define Const.
   For sample code on handling other constants, look at i370_elf_cons()

   This code handles pseudo-ops of the style
   DC   D'3.141592653'   # in sysv4, .double 3.14159265
   DC   F'1'             # in sysv4, .long   1.
   DC   X'FOOD'          # hexadecimal, length 2
   DC   XL4'FOOD'        # hexadecimal, length 4
   DC   472X'OO'         # hexadecimal, length 472

   The D and F types get aligned to 4 byte boundaries. H gets aligned
   to 2-byte boundaries. The X-types are not aligned, as they are
   often used for strings.  Those with a repeat count get aligned
   on a boundary suitable for the repeated type.
 */
static void
i370_dc (int unused ATTRIBUTE_UNUSED)
{
  expressionS xexp;
  int nbytes;
  char type;
  char *p;
  char *rpeat;
  int i, npeat = 1;

  if (is_it_end_of_statement ())
    {
      demand_empty_rest_of_line ();
      return;
    }

  /* Look for a repeat count */
  rpeat = input_line_pointer;
  while (ISNUM(*input_line_pointer)) input_line_pointer++;
  if (rpeat != input_line_pointer)
    {
      type = *input_line_pointer;
      *input_line_pointer = 0;
      npeat = atoi(rpeat);
      *input_line_pointer = type;
    }

  type = *input_line_pointer;
  if ('C' == type) /* String constant */
    {
      ++input_line_pointer;

      if (i370_convert_dc_to_ebcdic)
	i370_rebcdic(1, npeat);
      else
	i370_rebcdic(0, npeat);
      return;
    }

  i370_dc_align(type);

  nbytes = i370_parse_const(&xexp);

  switch (type)
    {
    case 'A':  /* Address of label */
    case 'V':  /* External Address */
      /* Address constants are of length 4 */
      for (i=0; i<npeat; i++) emit_expr (&xexp, 4);
      break;

    case 'B':  /* binary bitstring */
    case 'H':  /* 16-bit decimal */
    case 'F':  /* 32-bit decimal */
    case 'X':  /* variable length hex */
      for (i=0; i<npeat; i++) emit_expr (&xexp, nbytes);
      break;

    case 'E':  /* 32-bit float */
    case 'D':  /* 64-bit double */
    case 'L':  /* 128-bit long double */
      p = frag_more (nbytes * npeat);

      for (i=0; i<npeat; i++)
	{
	  /* XXX I'm not sure this is right ...? */
	  memcpy (p, generic_bignum, nbytes);
	  p += nbytes;
	}
      break;

    default:
      as_bad (_("unsupported DC type"));
      return;
    }

  demand_empty_rest_of_line ();
}


/* Provide minimal support for DS Define Storage.
   DS 0H should align on a half-word
   DS 4000C should increment 4000 bytes (i.e. same as .space 4000)

   See the function s_space() in read.c for examples on how to
   handle other section types (absolute, common, etc.) and how
   to parse more complex expressions. */

static void
i370_ds (int unused ATTRIBUTE_UNUSED)
{
  expressionS val;
  addressT total;
  int mult = 1;
  int alignment = 0;  /* Left shift 1 << align.  */
  char alchar = 'C';
  char *palchar;

  /* The expression() below tries to interpret the suffix as a radix.
     We don't want that; we just want ordinary base-10. */
  palchar = input_line_pointer;
  for ( ; ISNUM (*palchar); palchar++) {}
  if (ISALPHA (*palchar))
    {
      alchar = *palchar;
      *palchar = ' ';
    }

  expression (&val);
  if (val.X_op != O_constant)
    as_bad(_("Unsupported DS format"));

  switch (alchar)
  {
  case 'C':  /* 8-bit */
    mult = 1;
    break;
  case 'H':  /* 16-bit */
    alignment = 1;
    mult = 2;
    break;
  case 'F':  /* 32-bit */
    alignment = 2;
    mult = 4;
    break;
  case 'D':  /* 64-bit */
    alignment = 3;
    mult = 8;
    break;
  case 'L':  /* 128-bit, but 64-bit aligned */
    alignment = 3;
    mult = 16;
    break;
  default:
    as_bad (_("Unsupported DS alignment"));
    return;
  }

  /* Align, if needed. */
  if (0 < alignment)
    {
      frag_align (alignment, 0, 0);
      record_alignment (now_seg, alignment);
    }

  /* Expressions such as DS 0H or DS 0F or DS 0D force alignment only;
   * no space is reserved.  */
  if (0 < val.X_add_number)
    {
      total = mult * val.X_add_number;
      frag_var (rs_fill, 1, 1, (relax_substateT) 0, (symbolS *) 0,
		(offsetT) total, (char *) 0);
    }

  demand_empty_rest_of_line ();
}


/* Attempt at supporting general EQU statements.
 * Expressions must evalute to a constant.
 * Currently supported forms:
 *    FOO EQU 8
 *    FOO EQU 42 - 21
 */
static void do_equ(char* token)
{
  struct expressionS ex;

  /* Maybe use i370_parse_const for F'1234' type expressions? */
  /* i370_parse_const(&ex); */
  expression(&ex);

  if (ex.X_op != O_constant)
    {
      as_bad(_("Expressions in EQU must be constant"));
      return;
    }

  /* reg_section isn't written to the object file */
  local_symbol_make(token, reg_section, &zero_address_frag, ex.X_add_number);

  /* Local symbols cannot hold expressions. */
  /* symbol_set_value_expression(symp, &ex); */

  while ('\n' != *input_line_pointer) input_line_pointer++;
  input_line_pointer--;
}

/* Support for DS and DC occuring on the same line as a label.
 * In such a case, alginment must be done *before* the label is issued.
 */
bool i370_align_label(char * line_start)
{
  /* Look for DC or DS followed by whitespace */
  if (('D' == *(input_line_pointer+1)) &&
      (ISSPACE(*(input_line_pointer+3))))
    {
      char dtype = *(input_line_pointer+2);
      if ('S' != dtype && 'C' != dtype)
	{
	  as_bad(_("Unexpected label alignment."));
	  return true;
	}

      char* palch = input_line_pointer + 4;
      while (ISSPACE(*palch) || ISNUM(*palch)) palch ++;
      char atype = *palch;
      i370_dc_align(atype);

      /* We don't input_line_pointer++ here, that is done by DC/DS. */
      return true;
    }

  /* Look for EQU followed by stuff. */
  if (0 == strncmp(input_line_pointer+1, "EQU", 3))
    {
      input_line_pointer += 4;
      while (ISSPACE (*input_line_pointer)) input_line_pointer++;
      if ('*' == *input_line_pointer)
	return true;

      do_equ(line_start);
      return false;
    }

  return true;
}

/* Solaris pseudo op to change to the .rodata section.  */

static void
i370_elf_rdata (int sect)
{
  char *save_line = input_line_pointer;
  static char section[] = ".rodata\n";

  /* Just pretend this is .section .rodata.  */
  input_line_pointer = section;
  obj_elf_section (sect);

  input_line_pointer = save_line;
}

/* Pseudo op to make file scope bss items.  */

static void
i370_elf_lcomm (int unused ATTRIBUTE_UNUSED)
{
  char *name;
  char c;
  char *p;
  offsetT size;
  symbolS *symbolP;
  offsetT align;
  segT old_sec;
  int old_subsec;
  char *pfrag;
  int align2;

  c = get_symbol_name (&name);

  /* Just after name is now '\0'.  */
  p = input_line_pointer;
  (void) restore_line_pointer (c);
  SKIP_WHITESPACE ();
  if (*input_line_pointer != ',')
    {
      as_bad (_("Expected comma after symbol-name: rest of line ignored."));
      ignore_rest_of_line ();
      return;
    }

  /* Skip ','.  */
  input_line_pointer++;
  if ((size = get_absolute_expression ()) < 0)
    {
      as_warn (_(".COMMon length (%ld.) <0! Ignored."), (long) size);
      ignore_rest_of_line ();
      return;
    }

  /* The third argument to .lcomm is the alignment.  */
  if (*input_line_pointer != ',')
    align = 8;
  else
    {
      ++input_line_pointer;
      align = get_absolute_expression ();
      if (align <= 0)
        {
          as_warn (_("ignoring bad alignment"));
          align = 8;
        }
    }

  *p = 0;
  symbolP = symbol_find_or_make (name);
  *p = c;
  dwarf2_emit_label(symbolP);

  if (S_IS_DEFINED (symbolP) && ! S_IS_COMMON (symbolP))
    {
      as_bad (_("Ignoring attempt to re-define symbol `%s'."),
              S_GET_NAME (symbolP));
      ignore_rest_of_line ();
      return;
    }

  if (S_GET_VALUE (symbolP) && S_GET_VALUE (symbolP) != (valueT) size)
    {
      as_bad (_("Length of .lcomm \"%s\" is already %ld. Not changed to %ld."),
              S_GET_NAME (symbolP),
              (long) S_GET_VALUE (symbolP),
              (long) size);

      ignore_rest_of_line ();
      return;
    }

  /* Allocate_bss:  */
  old_sec = now_seg;
  old_subsec = now_subseg;
  if (align)
    {
      /* Convert to a power of 2 alignment.  */
      for (align2 = 0; (align & 1) == 0; align >>= 1, ++align2)
	;
      if (align != 1)
        {
          as_bad (_("Common alignment not a power of 2"));
          ignore_rest_of_line ();
          return;
        }
    }
  else
    align2 = 0;

  record_alignment (bss_section, align2);
  subseg_set (bss_section, 0);
  if (align2)
    frag_align (align2, 0, 0);
  if (S_GET_SEGMENT (symbolP) == bss_section)
    symbol_get_frag (symbolP)->fr_symbol = 0;
  symbol_set_frag (symbolP, frag_now);
  pfrag = frag_var (rs_org, 1, 1, (relax_substateT) 0, symbolP, size,
        	    (char *) 0);
  *pfrag = 0;
  S_SET_SIZE (symbolP, size);
  S_SET_SEGMENT (symbolP, bss_section);
  subseg_set (old_sec, old_subsec);
  demand_empty_rest_of_line ();
}
#endif /* OBJ_ELF */


#define LITERAL_POOL_SUPPORT
#ifdef LITERAL_POOL_SUPPORT
/* Provide support for literal pools within the text section.
   Loosely based on similar code from tc-arm.c.
   We will use four symbols to locate four parts of the literal pool.
   These four sections contain 64,32,16 and 8-bit constants; we use
   four sections so that all memory access can be appropriately aligned.
   That is, we want to avoid mixing these together so that we don't
   waste space padding out to alignments.  The four pointers
   longlong_poolP, word_poolP, etc. point to a symbol labeling the
   start of each pool part.

   lit_pool_num increments from zero to infinity and uniquely id's
     -- it's used to generate the *_poolP symbol name.  */

#define MAX_LITERAL_POOL_SIZE 1024

typedef struct literalS
{
  struct expressionS  xexp;
  char * sym_name;
  char size;  /* 1,2,4,8 or 16 */
  short offset;

  /* Cache for generic_floating_point_number and generic_bignum */
  LITTLENUM_TYPE bignum[BIGNUM_CACHE];

#ifdef OBJ_ELF
  struct dwarf2_line_info loc;
#endif
} literalT;

literalT literals[MAX_LITERAL_POOL_SIZE];
int next_literal_pool_place = 0; /* Next free entry in the pool.  */

static symbolS *longlong_poolP = NULL;   /* 64-bit pool entries.  */
static symbolS *word_poolP = NULL;       /* 32-bit pool entries.  */
static symbolS *short_poolP = NULL;      /* 16-bit pool entries.  */
static symbolS *byte_poolP = NULL;       /* 8-bit  pool entries.  */

static int lit_pool_num = 1;

/* Create a new, empty symbol.  */
static symbolS *
symbol_make_empty (void)
{
  return symbol_create (FAKE_LABEL_NAME, undefined_section,
  			&zero_address_frag, 0);
}

/* Make the first argument an address-relative expression
   by subtracting the second argument.  */

static void
i370_make_relative (expressionS *exx, expressionS *baseaddr)
{
  if (O_constant == baseaddr->X_op)
    {
       exx->X_op = O_symbol;
       exx->X_add_number -= baseaddr->X_add_number;
    }
  else if (O_symbol == baseaddr->X_op)
    {
       exx->X_op = O_subtract;
       exx->X_op_symbol = baseaddr->X_add_symbol;
       exx->X_add_number -= baseaddr->X_add_number;
    }
  else if (O_uminus == baseaddr->X_op)
    {
       exx->X_op = O_add;
       exx->X_op_symbol = baseaddr->X_add_symbol;
       exx->X_add_number += baseaddr->X_add_number;
    }
  else
    as_bad (_("Missing or bad .using directive"));
}

/* Add an expression to the literal pool.  */
static  void
add_to_lit_pool (expressionS *exx, char *name, int sz)
{
  int lit_count = 0;
  int offset_in_pool = 0;

  /* Start a new pool, if necessary.  */
  if ((8 == sz || 16 == sz) && NULL == longlong_poolP)
    longlong_poolP = symbol_make_empty ();
  else if (4 == sz && NULL == word_poolP)
    word_poolP = symbol_make_empty ();
  else if (2 == sz && NULL == short_poolP)
    short_poolP = symbol_make_empty ();
  else if (1 == sz && NULL == byte_poolP)
    byte_poolP = symbol_make_empty ();

  /* Check if this literal value is already in the pool.
     FIXME: We should probably be checking expressions
            of type O_symbol as well.
     FIXME: This is probably(certainly?) broken for O_big,
            which includes 64-bit long-longs.  */
  while (lit_count < next_literal_pool_place)
    {
      if (exx->X_op == O_constant
          && literals[lit_count].xexp.X_op == exx->X_op
          && literals[lit_count].xexp.X_add_number == exx->X_add_number
          && literals[lit_count].xexp.X_unsigned == exx->X_unsigned
          && literals[lit_count].size == sz)
        break;
      else if (literals[lit_count].sym_name
	       && name
	       && !strcmp (name, literals[lit_count].sym_name))
        break;
      if (sz == literals[lit_count].size)
	offset_in_pool += sz;
      lit_count ++;
    }

  if (lit_count == next_literal_pool_place) /* new entry */
    {
      if (next_literal_pool_place > MAX_LITERAL_POOL_SIZE)
	as_bad (_("Literal Pool Overflow"));

      literals[next_literal_pool_place].xexp = *exx;
      literals[next_literal_pool_place].size = sz;
      literals[next_literal_pool_place].offset = offset_in_pool;
      if (name)
	literals[next_literal_pool_place].sym_name = xstrdup (name);
      else
	literals[next_literal_pool_place].sym_name = NULL;

      /* Cache the value of generic_bignum; we need this later,
       * when issueing the ltorg. Cache 16 bytes total.  */
      if (exx->X_op == O_big)
	memcpy (literals[next_literal_pool_place].bignum,
	       generic_bignum, BIGNUM_CACHE * sizeof(LITTLENUM_TYPE));

#ifdef OBJ_ELF
      /* Record the location of the first source line to reference
         this entry in the literal pool.  If it turns out during
         linking that the symbol does not exist we will be able
         to give an accurate line number for the (first use of the)
         missing reference.  */
      if (debug_type == DEBUG_DWARF2)
	dwarf2_where (&literals[next_literal_pool_place].loc);
#endif
      next_literal_pool_place++;
    }

  /* ???_poolP points to the beginning of the literal pool.
     X_add_number is the offset from the beginning of the
     literal pool to this expr minus the location of the most
     recent .using directive.  Thus, the grand total value of the
     expression is the distance from .using to the literal.  */
  if (8 == sz || 16 == sz)
    exx->X_add_symbol = longlong_poolP;
  else if (4 == sz)
    exx->X_add_symbol = word_poolP;
  else if (2 == sz)
    exx->X_add_symbol = short_poolP;
  else if (1 == sz)
    exx->X_add_symbol = byte_poolP;
  exx->X_add_number = offset_in_pool;
  exx->X_op_symbol = NULL;

  /* If the user has set up a base reg in another section,
     use that; otherwise use the text section.  */
  if (0 < i370_using_other_regno)
    i370_make_relative (exx, &i370_using_other_baseaddr);
  else
    i370_make_relative (exx, &i370_using_text_baseaddr);
}

/* The symbol setup for the literal pool is done in two steps.  First,
   a symbol that represents the start of the literal pool is created,
   above, in the add_to_pool() routine. This is sym ???_poolP.
   However, we don't know what fragment it's in until a bit later.
   So we defer the frag_now thing, and the symbol name, until .ltorg time.  */

/* Can't use symbol_new here, so have to create a symbol and then at
   a later date assign it a value. That's what these functions do.  */

static void
symbol_locate (symbolS *symbolP,
	       const char *name,	/* It is copied, the caller can modify.  */
	       segT segment,		/* Segment identifier (SEG_<something>).  */
	       valueT valu,		/* Symbol value.  */
	       fragS *frag)		/* Associated fragment.  */
{
  size_t name_length;
  char *preserved_copy_of_name;

  name_length = strlen (name) + 1;      /* +1 for \0 */
  obstack_grow (&notes, name, name_length);
  preserved_copy_of_name = obstack_finish (&notes);

  S_SET_NAME (symbolP, preserved_copy_of_name);

  S_SET_SEGMENT (symbolP, segment);
  S_SET_VALUE (symbolP, valu);
  symbol_clear_list_pointers (symbolP);

  symbol_set_frag (symbolP, frag);

  /* Link to end of symbol chain.  */
  {
    extern int symbol_table_frozen;

    if (symbol_table_frozen)
      abort ();
  }

  symbol_append (symbolP, symbol_lastP, &symbol_rootP, &symbol_lastP);

  obj_symbol_new_hook (symbolP);

#ifdef tc_symbol_new_hook
  tc_symbol_new_hook (symbolP);
#endif

#define DEBUG_SYMS
#ifdef DEBUG_SYMS
  verify_symbol_chain(symbol_rootP, symbol_lastP);
#endif /* DEBUG_SYMS */
}

/* i370_addr_offset() will convert operand expressions
   that appear to be absolute into their base-register
   relative form.  These expressions come in two types:

   (1) of the form "* + const" * where "*" means
   relative offset since the last using
   i.e. "*" means ".-using_baseaddr"

   (2) labels, which are never absolute, but are always
   relative to the last "using".  Anything with an alpha
   character is considered to be a label (since symbols
   can never be operands), and since we've already handled
   register operands. For example, "BL .L33" branch low
   to .L33 RX form insn frequently terminates for-loops.

   Local labels can start with underscores; this is for
   MVS/HLASM compatibility.
  */

static bool
i370_addr_offset (expressionS *exx)
{
  char *dot, *lab;
  int islabel = 0;
  int all_digits = 0;

  /* Search for a label; anything with an alpha char will do.
     Local labels consist of N digits followed by either b or f.  */
  lab = input_line_pointer;
  while (*lab && (',' != *lab) && ('(' != *lab))
    {
      if (ISDIGIT (*lab))
	all_digits = 1;
      else if (ISALPHA (*lab) || '_' == *lab || '@' == *lab)
	{
	  if (!all_digits)
	    {
	      islabel = 1;
	      break;
	    }
	  else if (('f' == *lab) || ('b' == *lab))
	    {
	      islabel = 1;
	      break;
	    }
	  if (all_digits)
	    break;
	}
      else if ('.' != *lab)
        break;
      ++lab;
    }

  /* See if operand has a * in it.  */
  dot = strchr (input_line_pointer, '*');

  if (!dot && !islabel)
    return false;

  /* Replace * with . and let expr munch on it.  */
  if (dot)
    *dot = '.';
  expression (exx);

  /* OK, now we have to subtract the "using" location.
     Normally branches appear in the text section only.  */
  if (0 == strncmp (now_seg->name, ".text", 5) || 0 > i370_using_other_regno)
    i370_make_relative (exx, &i370_using_text_baseaddr);
  else
    i370_make_relative (exx, &i370_using_other_baseaddr);

  /* Put the * back.  */
  if (dot)
    *dot = '*';

  return true;
}


/* Handle address constants of various sorts.  */
/* The currently supported types are
      =A(some_symb)
      =V(some_extern)
      =X'deadbeef'    hexadecimal
      =F'1234'        32-bit const int
      =H'1234'        16-bit const int.  */

static bool
i370_addr_cons (expressionS *xexp)
{
  char *name;
  char *sym_name, delim;
  int name_len;
  int cons_len = 0;

  name = input_line_pointer;
  sym_name = input_line_pointer;
  /* Find the spelling of the operand.  */
  if (name[0] == '=' && ISALPHA (name[1]))
    name = ++input_line_pointer;
  else
    return false;

  switch (name[0])
    {
    case 'A': /* A == address-of.  */
    case 'V': /* V == extern.  */
      ++input_line_pointer; /* Skip open paren */
      expression (xexp);

      /* The sym_name includes the = sign, so `=V(some_extern)` */
      name_len = strcspn (sym_name, ", \r\n");
      delim = *(sym_name + name_len);
      *(sym_name + name_len) = 0x0;
      add_to_lit_pool (xexp, sym_name, 4);
      *(sym_name + name_len) = delim;

      break;
    case 'H':  /* 16-bit decimal */
    case 'F':  /* 32-bit decimal */
    case 'X':  /* hex, variable length */
    case 'E':  /* Single-precision float point.  */
    case 'D':  /* Double-precision float point.  */
    case 'L':  /* 128-bit float point.  */
      cons_len = i370_parse_const(xexp);
      add_to_lit_pool (xexp, 0x0, cons_len);
      break;

    default:
      as_bad (_("Unknown/unsupported address literal type"));
      return false;
    }

  return true;
}


/* Dump the contents of the literal pool that we've accumulated so far.
   This aligns the pool to the size of the largest literal in the pool.  */

static void
i370_ltorg (int ignore ATTRIBUTE_UNUSED)
{
  int litsize;
  int lit_count = 0;
  int byte_count = 0;
  int biggest_literal_size = 0;
  int biggest_align = 0;
  char pool_name[28];

  if (strncmp (now_seg->name, ".text", 5))
    {
      if (i370_other_section == undefined_section)
	as_bad (_(".ltorg without prior .using in section %s"),
		now_seg->name);

      if (i370_other_section != now_seg)
	as_bad (_(".ltorg in section %s paired to .using in section %s"),
		now_seg->name, i370_other_section->name);
    }

  if (! longlong_poolP
      && ! word_poolP
      && ! short_poolP
      && ! byte_poolP)
    /* Nothing to do.  */
    return;

  /* Find largest literal .. 2 4 or 8.  */
  lit_count = 0;
  while (lit_count < next_literal_pool_place)
    {
      if (biggest_literal_size < literals[lit_count].size)
	biggest_literal_size = literals[lit_count].size;
      lit_count ++;
    }
  if (1 == biggest_literal_size) biggest_align = 0;
  else if (2 == biggest_literal_size) biggest_align = 1;
  else if (4 == biggest_literal_size) biggest_align = 2;
  else if (8 == biggest_literal_size) biggest_align = 3;
  else as_bad (_("bad alignment of %d bytes in literal pool"), biggest_literal_size);
  if (0 == biggest_align) biggest_align = 1;

  /* Align pool for short, word, double word accesses.  */
  frag_align (biggest_align, 0, 0);
  record_alignment (now_seg, biggest_align);

  /* Output largest literals first, then the smaller ones.  */
  for (litsize=8; litsize; litsize /=2)
    {
      symbolS *current_poolP = NULL;
      switch (litsize)
	{
	case 8:
	  current_poolP = longlong_poolP; break;
	case 4:
	  current_poolP = word_poolP; break;
	case 2:
	  current_poolP = short_poolP; break;
	case 1:
	  current_poolP = byte_poolP; break;
	default:
	  as_bad (_("bad literal size\n"));
	}
      if (NULL == current_poolP)
	continue;
      sprintf (pool_name, ".LITP%01d%06d", litsize, lit_pool_num);
      symbol_locate (current_poolP, pool_name, now_seg,
		     (valueT) frag_now_fix (), frag_now);
      symbol_table_insert (current_poolP);

      lit_count = 0;
      while (lit_count < next_literal_pool_place)
	{
	  if (litsize == literals[lit_count].size)
	    {
	      /* Emit address constant symbols if the user gave the
	         -L, --keep-locals flag. This is kind of useless,
	         except for debugging.  */
	      if (flag_keep_locals && literals[lit_count].sym_name)
		{
		  symbolS * symP = symbol_make_empty ();
		  symbol_locate (symP, literals[lit_count].sym_name, now_seg,
		                 (valueT) frag_now_fix (), frag_now);
		  symbol_table_insert (symP);
		}

	      /* Restore bignum from cache, where emit_expr can find it. */
	      if (literals[lit_count].xexp.X_op == O_big)
		{
		  generic_bignum[0] = literals[lit_count].bignum[0];
		  generic_bignum[1] = literals[lit_count].bignum[1];
		  generic_bignum[2] = literals[lit_count].bignum[2];
		  generic_bignum[3] = literals[lit_count].bignum[3];
		}

	      emit_expr (&(literals[lit_count].xexp), literals[lit_count].size);
	      byte_count += literals[lit_count].size;
#ifdef OBJ_ELF
	      if (debug_type == DEBUG_DWARF2)
		dwarf2_gen_line_info (frag_now_fix (), &literals[lit_count].loc);
#endif
	    }
	  lit_count ++;
	}
    }

  /* Print this many lines of the literal pool in the GAS listing. */
  listing_lhs_cont_lines = byte_count / 4;

  next_literal_pool_place = 0;
  longlong_poolP = NULL;
  word_poolP = NULL;
  short_poolP = NULL;
  byte_poolP = NULL;
  lit_pool_num++;
}

/* This fix_new is called by cons via TC_CONS_FIX_NEW.	*/

void
cons_fix_new_i370 (fragS *	frag,
		  int		where,
		  int		size,
		  expressionS * exp,
		  bfd_reloc_code_real_type reloc)
{
  int pcrel = 0;

  /* Pick a reloc. */
  switch (size)
    {
    case 1:
      reloc = BFD_RELOC_8;
      break;
    case 2:
      reloc = BFD_RELOC_16;
      break;
    case 4:
    default:
      reloc = BFD_RELOC_32;
      break;
    case 8:
      reloc = BFD_RELOC_64;
      break;
    }

  fix_new_exp (frag, where, size, exp, pcrel, reloc);
}

#endif /* LITERAL_POOL_SUPPORT */


/* Add support for the HLASM-like USING directive to indicate
   the base register to use ...  we don't support the full
   hlasm semantics for this ... we merely pluck a base address
   and a register number out.  We print a warning if using is
   called multiple times.  I suppose we should check to see
   if the regno is valid.  */

static void
i370_using (int ignore ATTRIBUTE_UNUSED)
{
  expressionS ex, baseaddr;
  int iregno;
  char *star;

  /* If "*" appears in a using, it means "."
     replace it with "." so that expr doesn't get confused.  */
  star = strchr (input_line_pointer, '*');
  if (star)
    *star = '.';

  /* The first arg to using will usually be ".", but it can
     be a more complex expression too.  */
  expression (&baseaddr);
  if (star)
    *star = '*';
  if (O_constant != baseaddr.X_op
      && O_symbol != baseaddr.X_op
      && O_uminus != baseaddr.X_op)
    as_bad (_(".using: base address expression illegal or too complex"));

  /* Skip past the comma.  */
  if (*input_line_pointer != '\0') ++input_line_pointer;

  /* The second arg to using had better be a register.  */
  register_name (&ex);
  demand_empty_rest_of_line ();
  iregno = ex.X_add_number;

  if (0 == strncmp (now_seg->name, ".text", 5))
    {
      i370_using_text_baseaddr = baseaddr;
      i370_using_text_regno = iregno;
    }
  else
    {
      i370_using_other_baseaddr = baseaddr;
      i370_using_other_regno = iregno;
      i370_other_section = now_seg;
    }
}

static void
i370_drop (int ignore ATTRIBUTE_UNUSED)
{
  expressionS ex;
  int iregno;

  register_name (&ex);
  demand_empty_rest_of_line ();
  iregno = ex.X_add_number;

  if (0 == strncmp (now_seg->name, ".text", 5))
    {
      if (iregno != i370_using_text_regno)
	as_bad (_("dropping register %d in section %s does not match using register %d"),
		iregno, now_seg->name, i370_using_text_regno);

      i370_using_text_regno = -1;
      i370_using_text_baseaddr.X_op = O_absent;
    }
  else
    {
      if (iregno != i370_using_other_regno)
	as_bad (_("dropping register %d in section %s does not match using register %d"),
		iregno, now_seg->name, i370_using_other_regno);

      if (i370_other_section != now_seg)
	as_bad (_("dropping register %d in section %s previously used in section %s"),
		iregno, now_seg->name, i370_other_section->name);

      i370_using_other_regno = -1;
      i370_using_other_baseaddr.X_op = O_absent;
      i370_other_section = undefined_section;
    }
}


/* We need to keep a list of fixups.  We can't simply generate them as
   we go, because that would require us to first create the frag, and
   that would screw up references to ``.''.  */

struct i370_fixup
{
  expressionS xexp;
  int opindex;
  bfd_reloc_code_real_type reloc;
};

#define MAX_INSN_FIXUPS 5

/* Handle a macro.  Gather all the operands, transform them as
   described by the macro, and call md_assemble recursively.  All the
   operands are separated by commas; we don't accept parentheses
   around operands here.  */

static void
i370_macro (char *str, const struct i370_macro *macro)
{
  char *operands[10];
  unsigned int count;
  char *s;
  unsigned int len;
  const char *format;
  int arg;
  char *send;
  char *complete;

  /* Gather the users operands into the operands array.  */
  count = 0;
  s = str;
  while (1)
    {
      if (count >= sizeof operands / sizeof operands[0])
        break;
      operands[count++] = s;
      s = strchr (s, ',');
      if (s == (char *) NULL)
        break;
      *s++ = '\0';
    }

  if (count != macro->operands)
    {
      as_bad (_("wrong number of operands"));
      return;
    }

  /* Work out how large the string must be (the size is unbounded
     because it includes user input).  */
  len = 0;
  format = macro->format;
  while (*format != '\0')
    {
      if (*format != '%')
        {
          ++len;
          ++format;
        }
      else
        {
          arg = strtol (format + 1, &send, 10);
          know (send != format && arg >= 0 && (unsigned) arg < count);
          len += strlen (operands[arg]);
          format = send;
        }
    }

  /* Put the string together.  */
  complete = s = XNEWVEC (char, len + 1);
  format = macro->format;
  while (*format != '\0')
    {
      if (*format != '%')
        *s++ = *format++;
      else
        {
          arg = strtol (format + 1, &send, 10);
          strcpy (s, operands[arg]);
          s += strlen (s);
          format = send;
        }
    }
  *s = '\0';

  /* Assemble the constructed instruction.  */
  md_assemble (complete);
  free (complete);
}

/* This routine is called for each instruction to be assembled.  */

void
md_assemble (char *str)
{
  char *s;
  const struct i370_opcode *opcode;
  i370_insn_t insn;
  const unsigned char *opindex_ptr;
  int have_optional_index, have_optional_basereg, have_optional_reg;
  int skip_optional_index, skip_optional_basereg, skip_optional_reg;
  int use_text=0, use_other=0;
  struct i370_fixup fixups[MAX_INSN_FIXUPS];
  int fc;
  char *f;
  int i;
#ifdef OBJ_ELF_SUFFIX
  bfd_reloc_code_real_type reloc;
#endif

  /* Get the opcode.  */
  for (s = str; *s != '\0' && ! ISSPACE (*s); s++)
    ;
  if (*s != '\0')
    *s++ = '\0';

  /* Look up the opcode in the hash table.  */
  opcode = (const struct i370_opcode *) str_hash_find (i370_hash, str);
  if (opcode == (const struct i370_opcode *) NULL)
    {
      const struct i370_macro *macro;

      gas_assert (i370_macro_hash);
      macro = (const struct i370_macro *) str_hash_find (i370_macro_hash, str);
      if (macro == (const struct i370_macro *) NULL)
        as_bad (_("Unrecognized opcode: `%s'"), str);
      else
	i370_macro (s, macro);

      return;
    }

  insn = opcode->opcode;

  str = s;
  while (ISSPACE (*str))
    ++str;

  /* I370 operands are either expressions or address constants.
     Many operand types are optional.  The optional operands
     are always surrounded by parens, and are used to denote the base
     register ... e.g. "A R1, D2" or "A R1, D2(,B2) as opposed to
     the fully-formed "A R1, D2(X2,B2)".  Note also the = sign,
     such as A R1,=A(i) where the address-of operator =A implies
     use of both a base register, and a missing index register.

     So, before we start seriously parsing the operands, we check
     to see if we have an optional operand, and, if we do, we count
     the number of commas to see which operand should be omitted.  */

  have_optional_index = have_optional_basereg = have_optional_reg = 0;
  for (opindex_ptr = opcode->operands; *opindex_ptr != 0; opindex_ptr++)
    {
      const struct i370_operand *operand;

      operand = &i370_operands[*opindex_ptr];
      if ((operand->flags & I370_OPERAND_INDEX) != 0)
	have_optional_index = 1;
      if ((operand->flags & I370_OPERAND_BASE) != 0)
	have_optional_basereg = 1;
      if ((operand->flags & I370_OPERAND_OPTIONAL) != 0)
	have_optional_reg = 1;
    }

  skip_optional_index = skip_optional_basereg = skip_optional_reg = 0;
  if (have_optional_index || have_optional_basereg)
    {
      unsigned int opcount, nwanted;

      /* There is an optional operand.  Count the number of
	 commas and open-parens in the input line.  */
      if (*str == '\0')
	opcount = 0;
      else
	{
	  opcount = 1;
	  s = str;
	  while ((s = strpbrk (s, ",(=")) != (char *) NULL)
	    {
	      ++opcount;
	      ++s;
	      if (',' == *s) ++s;  /* avoid counting things like (, */
	      if ('=' == *s) { ++s; --opcount; }
	    }
	}

      /* If there are fewer operands in the line then are called
	 for by the instruction, we want to skip the optional
	 operand.  */
      nwanted = strlen ((char *) opcode->operands);
      if (have_optional_index)
	{
	  if (opcount < nwanted)
	    skip_optional_index = 1;
	  if (have_optional_basereg && ((opcount+1) < nwanted))
	    skip_optional_basereg = 1;
	  if (have_optional_reg && ((opcount+1) < nwanted))
	    skip_optional_reg = 1;
	}
      else
	{
	  if (have_optional_basereg && (opcount < nwanted))
	    skip_optional_basereg = 1;
	  if (have_optional_reg && (opcount < nwanted))
	    skip_optional_reg = 1;
	}
    }

  /* Gather the operands.  */
  fc = 0;
  for (opindex_ptr = opcode->operands; *opindex_ptr != 0; opindex_ptr++)
    {
      const struct i370_operand *operand;
      char *hold;
      expressionS ex;

      operand = &i370_operands[*opindex_ptr];

      /* If this is an index operand, and we are skipping it,
	 just insert a zero.  */
      if (skip_optional_index &&
	  ((operand->flags & I370_OPERAND_INDEX) != 0))
        {
          insn = i370_insert_operand (insn, operand, 0);
          continue;
        }

      /* If this is the base operand, and we are skipping it,
	 just insert the current using basreg.  */
      if (skip_optional_basereg &&
          ((operand->flags & I370_OPERAND_BASE) != 0))
        {
          int basereg = -1;
          if (use_text)
            {
              if (0 == strncmp (now_seg->name, ".text", 5)
		  || 0 > i370_using_other_regno)
		basereg = i370_using_text_regno;
              else
		basereg = i370_using_other_regno;
            }
          else if (use_other)
            {
              if (0 > i370_using_other_regno)
		basereg = i370_using_text_regno;
              else
		basereg = i370_using_other_regno;
            }
          if (0 > basereg)
	    as_bad (_("not using any base register"));

          insn = i370_insert_operand (insn, operand, basereg);
          continue;
        }

      /* If this is an optional operand, and we are skipping it,
	 Use zero (since a non-zero value would denote a register)  */
      if (skip_optional_reg
	  && ((operand->flags & I370_OPERAND_OPTIONAL) != 0))
        {
          insn = i370_insert_operand (insn, operand, 0);
          continue;
        }

      /* Gather the operand.  */
      hold = input_line_pointer;
      input_line_pointer = str;

      /* Register names are only allowed where there are registers.  */
      if ((operand->flags & I370_OPERAND_GPR) != 0)
        {
          /* Quickie hack to get past things like (,r13).  */
          if (skip_optional_index && (',' == *input_line_pointer))
            {
              *input_line_pointer = ' ';
              input_line_pointer ++;
            }

          if (! register_name (&ex))
	    as_bad (_("expecting a register or mask for operand %d"),
		    (int) (opindex_ptr - opcode->operands + 1));
        }

      /* Check for an address constant expression.  */
      /* We will put PSW-relative addresses in the text section,
         and address literals in the .data (or other) section.  */
      else if (i370_addr_cons (&ex))
	use_other = 1;
      else if (i370_addr_offset (&ex))
	use_text = 1;
      else expression (&ex);

      str = input_line_pointer;
      input_line_pointer = hold;

      /* Length fields are one-based when printed, but are zero-based
         in machine code. So e.g. Length 1 to 16 are machine encoded
         zero to 15.  */
      if (operand->flags & I370_OPERAND_LENGTH)
	ex.X_add_number --;

      if (ex.X_op == O_illegal)
        as_bad (_("illegal operand"));
      else if (ex.X_op == O_absent)
        as_bad (_("missing operand"));
      else if (ex.X_op == O_register)
	insn = i370_insert_operand (insn, operand, ex.X_add_number);
      else if (ex.X_op == O_constant)
        {
#ifdef OBJ_ELF_SUFFIX
          /* Allow @HA, @L, @H on constants.
             Well actually, no we don't; these really don't make sense
             (at least not to me) for the i370.  However, this code is
             left here for any dubious future expansion reasons.  */
          char *orig_str = str;

          if ((reloc = i370_elf_suffix (&str, &ex)) != BFD_RELOC_UNUSED)
            switch (reloc)
              {
              default:
        	str = orig_str;
        	break;

              case BFD_RELOC_LO16:
        	/* X_unsigned is the default, so if the user has done
                   something which cleared it, we always produce a
                   signed value.  */
		ex.X_add_number = (((ex.X_add_number & 0xffff)
				    ^ 0x8000)
				   - 0x8000);
        	break;

              case BFD_RELOC_HI16:
        	ex.X_add_number = (ex.X_add_number >> 16) & 0xffff;
        	break;

              case BFD_RELOC_HI16_S:
        	ex.X_add_number = (((ex.X_add_number >> 16) & 0xffff)
        			   + ((ex.X_add_number >> 15) & 1));
        	break;
              }
#endif
          insn = i370_insert_operand (insn, operand, ex.X_add_number);
        }
#ifdef OBJ_ELF_SUFFIX
      else if ((reloc = i370_elf_suffix (&str, &ex)) != BFD_RELOC_UNUSED)
        {
          as_tsktsk ("md_assemble(): suffixed relocations not supported\n");

          /* We need to generate a fixup for this expression.  */
          if (fc >= MAX_INSN_FIXUPS)
            as_fatal ("too many fixups");
          fixups[fc].xexp = ex;
          fixups[fc].opindex = 0;
          fixups[fc].reloc = reloc;
          ++fc;
        }
#endif /* OBJ_ELF_SUFFIX */
      else
        {
          /* We need to generate a fixup for this expression.  */
          /* Typically, the expression will just be a symbol ...
             printf ("insn %s needs fixup for %s \n",
                     opcode->name, S_GET_NAME(ex.X_add_symbol));  */

          if (fc >= MAX_INSN_FIXUPS)
            as_fatal ("too many fixups");
          fixups[fc].xexp = ex;
          fixups[fc].opindex = *opindex_ptr;
          fixups[fc].reloc = BFD_RELOC_UNUSED;
          ++fc;
        }

      /* Skip over delimiter (close paren, or comma).  */
      if ((')' == *str) && (',' == *(str+1)))
	++str;
      if (*str != '\0')
	++str;
    }

  while (ISSPACE (*str))
    ++str;

  if (*str != '\0')
    as_bad (_("junk at end of line: `%s'"), str);

  /* Write out the instruction.  */
  f = frag_more (opcode->len);
  dwarf2_emit_insn (opcode->len);
  if (4 >= opcode->len)
    md_number_to_chars (f, insn.i[0], opcode->len);
  else
    {
      md_number_to_chars (f, insn.i[0], 4);

      if (6 == opcode->len)
	md_number_to_chars ((f + 4), ((insn.i[1])>>16), 2);
      else
	{
	  /* Not used --- don't have any 8 byte instructions.  */
	  as_bad (_("Internal Error: bad instruction length"));
	  md_number_to_chars ((f + 4), insn.i[1], opcode->len -4);
	}
    }

  /* Create any fixups.  At this point we do not use a
     bfd_reloc_code_real_type, but instead just use the
     BFD_RELOC_UNUSED plus the operand index.  This lets us easily
     handle fixups for any operand type, although that is admittedly
     not a very exciting feature.  We pick a BFD reloc type in
     md_apply_fix.  */
  for (i = 0; i < fc; i++)
    {
      if (fixups[i].reloc == BFD_RELOC_UNUSED)
	{
	  fix_new_exp (frag_now, f - frag_now->fr_literal, opcode->len,
		       &fixups[i].xexp, 0,
		       ((bfd_reloc_code_real_type)
			(fixups[i].opindex + (int) BFD_RELOC_UNUSED)));
	}
#ifdef OBJ_ELF_SUFFIX
      else
	{
	  reloc_howto_type *reloc_howto = bfd_reloc_type_lookup (stdoutput, fixups[i].reloc);
	  int size;
	  fixS *fixP;

	  if (!reloc_howto)
	    abort ();

	  size = bfd_get_reloc_size (reloc_howto);

	  if (size < 1 || size > 4)
	    abort ();

	  printf (" gwana do fixup %d \n", i);
	  fixP = fix_new_exp (frag_now, f - frag_now->fr_literal, size,
         		      &fixups[i].xexp, reloc_howto->pc_relative,
         		      fixups[i].reloc);

	  /* Turn off complaints that the addend is too large for things like
	     foo+100000@ha.  */
	  switch (fixups[i].reloc)
	    {
	    case BFD_RELOC_16_GOTOFF:
	    case BFD_RELOC_LO16:
	    case BFD_RELOC_HI16:
	    case BFD_RELOC_HI16_S:
	      fixP->fx_no_overflow = 1;
	      break;
	    default:
	      break;
	    }
	}
#endif /* OBJ_ELF_SUFFIX */
    }
}


/* Pseudo-op handling.  */

/* The .byte pseudo-op.  This is similar to the normal .byte
   pseudo-op, but it can also take a single ASCII string.  */

static void
i370_byte (int ignore ATTRIBUTE_UNUSED)
{
  if (*input_line_pointer != '\"')
    {
      cons (1);
      return;
    }

  /* Gather characters.  A real double quote is doubled.  Unusual
     characters are not permitted.  */
  ++input_line_pointer;
  while (1)
    {
      char c;

      c = *input_line_pointer++;

      if (c == '\"')
        {
        if (*input_line_pointer != '\"')
            break;
          ++input_line_pointer;
        }

      FRAG_APPEND_1_CHAR (c);
    }

  demand_empty_rest_of_line ();
}

/* The .tc pseudo-op.  This is used when generating XCOFF and ELF.
   This takes two or more arguments.

   When generating XCOFF output, the first argument is the name to
   give to this location in the toc; this will be a symbol with class
   TC.  The rest of the arguments are 4 byte values to actually put at
   this location in the TOC; often there is just one more argument, a
   relocatable symbol reference.

   When not generating XCOFF output, the arguments are the same, but
   the first argument is simply ignored.  */

static void
i370_tc (int ignore ATTRIBUTE_UNUSED)
{

  /* Skip the TOC symbol name.  */
  while (is_part_of_name (*input_line_pointer)
         || *input_line_pointer == '['
         || *input_line_pointer == ']'
         || *input_line_pointer == '{'
         || *input_line_pointer == '}')
    ++input_line_pointer;

  /* Align to a four byte boundary.  */
  frag_align (2, 0, 0);
  record_alignment (now_seg, 2);

  if (*input_line_pointer != ',')
    demand_empty_rest_of_line ();
  else
    {
      ++input_line_pointer;
      cons (4);
    }
}

const char *
md_atof (int type, char *litp, int *sizep)
{
  /* 360/370/390 have three different float formats:
     H Hex, which is the old-style, 24-bit or 56-bit mantissa
     B Binary, which is IEEE, 24-bit or 53-bit mantissa
     D Decimal, which is uhh, decimal.

     Support only "Binary" (IEEE).
     FIXME: Add support for Hex.
  */
  return ieee_md_atof (type, litp, sizep, true);
}

/* Align a section (I don't know why this is machine dependent).  */

valueT
md_section_align (asection *seg, valueT addr)
{
  int align = bfd_section_alignment (seg);

  /* valueT is bfd_vma is 64-bit */
  return (addr + (1UL << align) - 1) & ((~0UL) << align);
}

/* We don't have any form of relaxing.  */

int
md_estimate_size_before_relax (fragS *fragp ATTRIBUTE_UNUSED,
			       asection *seg ATTRIBUTE_UNUSED)
{
  abort ();
  return 0;
}

/* Convert a machine dependent frag.  We never generate these.  */

void
md_convert_frag (bfd *abfd ATTRIBUTE_UNUSED,
		 asection *sec ATTRIBUTE_UNUSED,
		 fragS *fragp ATTRIBUTE_UNUSED)
{
  abort ();
}

/* We have no need to default values of symbols.  */

symbolS *
md_undefined_symbol (char *name ATTRIBUTE_UNUSED)
{
  return 0;
}

/* Functions concerning relocs.  */

/* The location from which a PC relative jump should be calculated,
   given a PC relative reloc.  */

long
md_pcrel_from_section (fixS *fixp, segT sec ATTRIBUTE_UNUSED)
{
  return fixp->fx_frag->fr_address + fixp->fx_where;
}

#ifdef OBJ_ELF_RELOCATABLE
/* Validate any relocations emitted for -mrelocatable, possibly adding
   fixups for word relocations in writable segments, so we can adjust
   them at runtime.  */

static void
i370_elf_validate_fix (fixS *fixp, segT seg)
{
  if (fixp->fx_done || fixp->fx_pcrel)
    return;

  switch (shlib)
    {
    case SHLIB_NONE:
    case SHLIB_PIC:
      return;

    case SHILB_MRELOCATABLE:
      if (fixp->fx_r_type <= BFD_RELOC_UNUSED
          && fixp->fx_r_type != BFD_RELOC_16_GOTOFF
          && fixp->fx_r_type != BFD_RELOC_HI16_GOTOFF
          && fixp->fx_r_type != BFD_RELOC_LO16_GOTOFF
          && fixp->fx_r_type != BFD_RELOC_HI16_S_GOTOFF
          && fixp->fx_r_type != BFD_RELOC_32_BASEREL
          && fixp->fx_r_type != BFD_RELOC_LO16_BASEREL
          && fixp->fx_r_type != BFD_RELOC_HI16_BASEREL
          && fixp->fx_r_type != BFD_RELOC_HI16_S_BASEREL
          && strcmp (segment_name (seg), ".got2") != 0
          && strcmp (segment_name (seg), ".dtors") != 0
          && strcmp (segment_name (seg), ".ctors") != 0
          && strcmp (segment_name (seg), ".fixup") != 0
          && strcmp (segment_name (seg), ".stab") != 0
          && strcmp (segment_name (seg), ".gcc_except_table") != 0
          && strcmp (segment_name (seg), ".ex_shared") != 0)
        {
          if ((seg->flags & (SEC_READONLY | SEC_CODE)) != 0
              || fixp->fx_r_type != BFD_RELOC_CTOR)
	    as_bad_where (fixp->fx_file, fixp->fx_line,
			  "Relocation cannot be done when using -mrelocatable");
        }
      return;
    default:
      break;
    }
}
#endif /* OBJ_ELF_RELOCATABLE */

/* Apply a fixup to the object code.  This is called for all the
   fixups we generated by the call to fix_new_exp, above.  In the call
   above we used a reloc code which was the largest legal reloc code
   plus the operand index.  Here we undo that to recover the operand
   index.  At this point all symbol values should be fully resolved,
   and we attempt to completely resolve the reloc.  If we can not do
   that, we determine the correct reloc code and put it back in the
   fixup.

   See gas/cgen.c for more sample code and explanations of what's
   going on here.  */

void
md_apply_fix (fixS *fixP, valueT * valP, segT seg ATTRIBUTE_UNUSED)
{
  valueT value = * valP;

  if (fixP->fx_addsy == NULL)
    fixP->fx_done = 1;
  else if ((int) fixP->fx_r_type >= (int) BFD_RELOC_UNUSED)
    {
      fixP->fx_done = 1;

      /* Allow fixups of things like *+12 where * is relative to the
         most recent .using. For mystery reasons, gas assigns the name
         *ABS* to the * symbol.  At any rate, old gcc's generate this.
         Not a problem for HLASM, which treats * differently, anyway.  */
      if (strcmp(S_GET_NAME (fixP->fx_addsy), "*ABS*"))
	{
	    as_bad(_("Fixup of undefined symbol %s at 0x%lx (%s:%d) not allowed"),
		    S_GET_NAME (fixP->fx_addsy),
		    fixP->fx_frag->fr_address + fixP->fx_where,
		    fixP->fx_file, fixP->fx_line);
	    return;
	}
    }

#ifdef DEBUG
  else
    {
      printf ("\nmd_apply_fix: symbol %s at 0x%lx (%s:%d) val=0x%lx addend=0x%lx\n",
	      S_GET_NAME (fixP->fx_addsy),
	      fixP->fx_frag->fr_address + fixP->fx_where,
	      fixP->fx_file, fixP->fx_line,
	      S_GET_VALUE (fixP->fx_addsy), value);
    }
#endif

  /* Apply fixups to operands.  These are all of one kind: they fix
     base-register-relative offsets so that they point to the correct
     location in the literal pool.  There should be no relocations
     for any operands, since no instruction ever takes an operand
     that requires reloc.  */
  if ((int) fixP->fx_r_type >= (int) BFD_RELOC_UNUSED)
    {
      int opindex;
      const struct i370_operand *operand;
      char *where;
      i370_insn_t insn;

      opindex = (int) fixP->fx_r_type - (int) BFD_RELOC_UNUSED;
      operand = &i370_operands[opindex];

#ifdef DEBUG
      printf ("\nmd_apply_fix: fixup operand %s at 0x%lx in %s:%d addend=0x%lx\n",
	      operand->name,
	      fixP->fx_frag->fr_address + fixP->fx_where,
	      fixP->fx_file, fixP->fx_line,
	      value);
#endif
      /* Fetch the instruction, insert the fully resolved operand
         value, and stuff the instruction back again.
         fisxp->fx_size is the length of the instruction.  */
      where = fixP->fx_frag->fr_literal + fixP->fx_where;
      insn.i[0] = bfd_getb32 ((unsigned char *) where);

      if (6 <= fixP->fx_size)
	/* Deal with 48-bit insn's.  */
	insn.i[1] = bfd_getb32 (((unsigned char *) where)+4);

      insn = i370_insert_operand (insn, operand, (offsetT) value);
      bfd_putb32 ((bfd_vma) insn.i[0], (unsigned char *) where);

      if (6 <= fixP->fx_size)
	/* Deal with 48-bit insn's.  */
	bfd_putb32 ((bfd_vma) insn.i[1], (((unsigned char *) where)+4));

      /* We are done, right? right !!  */
      fixP->fx_done = 1;

      /* Nothing else to do here.  */
      return;
    }

  fixP->fx_addnumber = value;
  return;

#ifdef OBJ_ELF_RELOCATABLE
  /* Handle fixups in general storage. */
  /* At this time, there aren't any. */
  if ((int) fixP->fx_r_type < (int) BFD_RELOC_UNUSED)
    {
      i370_elf_validate_fix (fixP, seg);
#ifdef DEBUG
      printf ("md_apply_fix: reloc case %d in segment  %s %s:%d\n",
	      fixP->fx_r_type, segment_name (seg), fixP->fx_file, fixP->fx_line);
      printf ("\tcurrent fixup value is 0x%lx \n", value);
#endif
      switch (fixP->fx_r_type)
        {
        case BFD_RELOC_32:
        case BFD_RELOC_CTOR:
          if (fixP->fx_pcrel)
            fixP->fx_r_type = BFD_RELOC_32_PCREL;
	  /* Fall through.  */

        case BFD_RELOC_RVA:
        case BFD_RELOC_32_PCREL:
        case BFD_RELOC_32_BASEREL:
#ifdef DEBUG
          printf ("\t32 bit relocation at 0x%lx\n",
		  fixP->fx_frag->fr_address + fixP->fx_where);
#endif
          md_number_to_chars (fixP->fx_frag->fr_literal + fixP->fx_where,
        		      value, 4);
          break;

        case BFD_RELOC_LO16:
        case BFD_RELOC_16:
          if (fixP->fx_pcrel)
            as_bad_where (fixP->fx_file, fixP->fx_line,
        		  "cannot emit PC relative %s relocation%s%s",
        		  bfd_get_reloc_code_name (fixP->fx_r_type),
        		  fixP->fx_addsy != NULL ? " against " : "",
        		  (fixP->fx_addsy != NULL
        		   ? S_GET_NAME (fixP->fx_addsy)
        		   : ""));

          md_number_to_chars (fixP->fx_frag->fr_literal + fixP->fx_where,
        		      value, 2);
          break;

          /* This case happens when you write, for example,
             lis %r3,(L1-L2)@ha
             where L1 and L2 are defined later.  */
        case BFD_RELOC_HI16:
          if (fixP->fx_pcrel)
            abort ();
          md_number_to_chars (fixP->fx_frag->fr_literal + fixP->fx_where,
        		      value >> 16, 2);
          break;
        case BFD_RELOC_HI16_S:
          if (fixP->fx_pcrel)
            abort ();
          md_number_to_chars (fixP->fx_frag->fr_literal + fixP->fx_where,
        		      (value + 0x8000) >> 16, 2);
          break;

        case BFD_RELOC_8:
          if (fixP->fx_pcrel)
            abort ();

          md_number_to_chars (fixP->fx_frag->fr_literal + fixP->fx_where,
        		      value, 1);
          break;

        default:
          fprintf (stderr,
        	  "Gas failure, reloc value %d\n", fixP->fx_r_type);
          fflush (stderr);
          abort ();
        }
    }
#endif /* OBJ_ELF_RELOCATABLE */
}

/* Generate a reloc for a fixup.  */

arelent *
tc_gen_reloc (asection *seg ATTRIBUTE_UNUSED, fixS *fixp)
{
  arelent *reloc;

  reloc = XNEW (arelent);

  reloc->sym_ptr_ptr = XNEW (asymbol *);
  *reloc->sym_ptr_ptr = symbol_get_bfdsym (fixp->fx_addsy);
  reloc->address = fixp->fx_frag->fr_address + fixp->fx_where;
  reloc->howto = bfd_reloc_type_lookup (stdoutput, fixp->fx_r_type);
  if (reloc->howto == (reloc_howto_type *) NULL)
    {
      as_bad_where (fixp->fx_file, fixp->fx_line,
        	    "reloc %d not supported by object file format", (int)fixp->fx_r_type);
      return NULL;
    }
  reloc->addend = fixp->fx_addnumber;

#ifdef DEBUG
  printf ("\ngen_reloc(): sym %s (%s:%d) at addr 0x%lx addend=0x%lx\n",
	  S_GET_NAME(fixp->fx_addsy),
	  fixp->fx_file, fixp->fx_line,
	  reloc->address, reloc->addend);
#endif

  return reloc;
}

/* The target-specific pseudo-ops which we support.  */

const pseudo_typeS md_pseudo_table[] =
{
  /* Pseudo-ops which must be overridden.  */
  { "byte",     i370_byte,	0 },

  /* HLASM-style pseudo-ops */
  { "dc",       i370_dc,	0 },
  { "ds",       i370_ds,	0 },
  { "rmode",    i370_rmode,	0 },
  { "csect",    i370_csect,	0 },
  { "dsect",    i370_dsect,	0 },
  { "entry",    i370_entry,	0 },

  /* enable ebcdic strings e.g. for 3270 support */
  { "ebcdic",   i370_ebcdic,	1 },

#ifdef OBJ_ELF_SUFFIX
  { "long",     i370_elf_cons,	4 },
  { "word",     i370_elf_cons,	4 },
  { "short",    i370_elf_cons,	2 },
#endif
#ifdef OBJ_ELF
  /* Override {"word", cons, 2} in read.c */
  { "word",     cons,		4 },
  { "rdata",    i370_elf_rdata,	0 },
  { "rodata",   i370_elf_rdata,	0 },
  { "lcomm",    i370_elf_lcomm,	0 },

  /* These are used for dwarf2.  */
  { "file",     (void (*) (int)) dwarf2_directive_file, 0 },
  { "loc",      dwarf2_directive_loc, 0 },
  { "loc_mark_labels", dwarf2_directive_loc_mark_labels, 0 },
#endif

  /* This pseudo-op is used even when not generating XCOFF output.  */
  { "tc",       i370_tc,	0 },

  /* Dump the literal pool */
  { "ltorg",    i370_ltorg,	0 },

  /* Support the HLASM-style USING directive. */
  { "using",    i370_using,	0 },
  { "drop",     i370_drop,	0 },

  { NULL,       NULL,		0 }
};
