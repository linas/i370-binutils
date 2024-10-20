/* tc-i370.h -- Header file for tc-i370.c.
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

#define TC_I370

struct fix;

/* Set the endianness we are using.  Default to big endian.  */
#ifndef TARGET_BYTES_BIG_ENDIAN
#define TARGET_BYTES_BIG_ENDIAN 1
#endif

#define md_number_to_chars number_to_chars_bigendian

/* The target BFD architecture.  */
#define TARGET_ARCH (i370_arch ())
extern enum bfd_architecture i370_arch (void);

/* The target BFD format.  */
#define TARGET_FORMAT ("elf32-i370")

/* HLASM uses pseudo-ops that do not begin with a dot. */
extern bool i370_no_pseudo_dot;
#define NO_PSEUDO_DOT i370_no_pseudo_dot

/* HLASM uses labels without a trailing colon. */
extern bool i370_labels_without_colons;
#define LABELS_WITHOUT_COLONS i370_labels_without_colons

/* HLASM places alignment DC, DS after a label, not before it */
extern bool i370_align_label(char*);
#define TC_START_LABEL_WITHOUT_COLON(NUL_CHAR, NEXT_CHAR) \
  (!i370_labels_without_colons || i370_align_label(line_start))

/* HLASM allows symbols starting with two @@ */
#define LEX_AT (LEX_BEGIN_NAME | LEX_NAME) /* Can have @'s inside labels.  */

/* Rewrite the @@ in HLASM symbols */
extern char * i370_canonicalize_symbol_name(char *);
#define tc_canonicalize_symbol_name i370_canonicalize_symbol_name

/* Permit temporary numeric labels.  */
#define LOCAL_LABELS_FB 1

/* Single quotes are used for literals,
 * e.g. =E'3.14159' or =XL4'cacad0d0' */
#define SINGLE_QUOTE_STRINGS

/* $ is (not) used to refer to the current location.  */
/* #define DOLLAR_DOT */

/* foo-. gets turned into PC relative relocs.  */
#define DIFF_EXPR_OK

/* Values passed to md_apply_fix don't include the symbol value.  */
#define MD_APPLY_SYM_VALUE(FIX) 0

/* We don't need to handle .word strangely.  */
#define WORKING_DOT_WORD

/* Call md_pcrel_from_section, not md_pcrel_from.  */
#define MD_PCREL_FROM_SECTION(FIX, SEC) md_pcrel_from_section (FIX, SEC)
extern long md_pcrel_from_section (struct fix *, segT);

#define md_operand(x)

#define TC_CONS_FIX_NEW cons_fix_new_i370
extern void cons_fix_new_i370 (fragS *, int, int, expressionS *, bfd_reloc_code_real_type);

#define tc_comment_chars i370_comment_chars
extern const char *i370_comment_chars;

#define DWARF2_LINE_MIN_INSN_LENGTH     1
#define DWARF2_DEFAULT_RETURN_COLUMN    14  /* Link register is r14 */
#define DWARF2_CIE_DATA_ALIGNMENT       -4  /* 32-bit arch */
