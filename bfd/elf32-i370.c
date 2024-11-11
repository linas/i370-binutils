/* i370-specific support for 32-bit ELF
   Copyright (C) 1994-2018 Free Software Foundation, Inc.
   Written by Ian Lance Taylor, Cygnus Support.
   Hacked by Linas Vepstas for i370 linas@linas.org

   This file is part of BFD, the Binary File Descriptor library.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

/* This file is based on a preliminary PowerPC ELF ABI.
   But its been hacked on for the IBM 360/370 architectures.
   Basically, the 31bit relocation works, and just about everything
   else is a wild card.  In particular, don't expect shared libs or
   dynamic loading to work ...  its never been tested.  */

#include "sysdep.h"
#include "bfd.h"
#include "bfdlink.h"
#include "libbfd.h"
#include "elf-bfd.h"
#include "elf/i370.h"

static reloc_howto_type *i370_elf_howto_table[ (int)R_I370_max ];

static reloc_howto_type i370_elf_howto_raw[] =
{
  /* This reloc does nothing.  */
  HOWTO (R_I370_NONE,		/* type */
	 0,			/* rightshift */
	 0,			/* size (1 = byte, 2 = 16-bit, 4 = 32-bit) */
	 0,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_dont, /* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_I370_NONE",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* A standard 31 bit relocation.  */
  HOWTO (R_I370_ADDR31,		/* type */
	 0,			/* rightshift */
	 4,			/* size (1 = byte, 2 = 16-bit, 4 = 32-bit) */
	 31,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_I370_ADDR31",	/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xffffffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  /* A standard 32 bit relocation.  */
  HOWTO (R_I370_ADDR32,		/* type */
	 0,			/* rightshift */
	 4,			/* size (1 = byte, 2 = 16-bit, 4 = 32-bit) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_I370_ADDR32",	/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xffffffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  /* A standard 16 bit relocation.  */
  HOWTO (R_I370_ADDR16,		/* type */
	 0,			/* rightshift */
	 2,			/* size (1 = byte, 2 = 16-bit, 4 = 32-bit) */
	 16,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_I370_ADDR16",	/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xffff,		/* dst_mask */
	 false),		/* pcrel_offset */

  /* 31-bit PC relative.  */
  HOWTO (R_I370_REL31,		/* type */
	 0,			/* rightshift */
	 4,			/* size (1 = byte, 2 = 16-bit, 4 = 32-bit) */
	 31,			/* bitsize */
	 true,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_I370_REL31",	/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xffffffff,		/* dst_mask */
	 true),			/* pcrel_offset */

  /* 32-bit PC relative.  */
  HOWTO (R_I370_REL32,		/* type */
	 0,			/* rightshift */
	 4,			/* size (1 = byte, 2 = 16-bit, 4 = 32-bit) */
	 32,			/* bitsize */
	 true,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_I370_REL32",	/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xffffffff,		/* dst_mask */
	 true),			/* pcrel_offset */

  /* A standard 12 bit relocation.  */
  HOWTO (R_I370_ADDR12,		/* type */
	 0,			/* rightshift */
	 2,			/* size (1 = byte, 2 = 16-bit, 4 = 32-bit) */
	 12,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_I370_ADDR12",	/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xfff,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* 12-bit PC relative.  */
  HOWTO (R_I370_REL12,		/* type */
	 0,			/* rightshift */
	 2,			/* size (1 = byte, 2 = 16-bit, 4 = 32-bit) */
	 12,			/* bitsize */
	 true,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_I370_REL12",	/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xfff,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* A standard 8 bit relocation.  */
  HOWTO (R_I370_ADDR8,		/* type */
	 0,			/* rightshift */
	 1,			/* size (1 = byte, 2 = 16-bit, 4 = 32-bit) */
	 8,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_I370_ADDR8",	/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xff,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* 8-bit PC relative.  */
  HOWTO (R_I370_REL8,		/* type */
	 0,			/* rightshift */
	 1,			/* size (1 = byte, 2 = 16-bit, 4 = 32-bit) */
	 8,			/* bitsize */
	 true,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc,	/* special_function */
	 "R_I370_REL8",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xff,			/* dst_mask */
	 true),			/* pcrel_offset */

  /* This is used only by the dynamic linker.  The symbol should exist
     both in the object being run and in some shared library.  The
     dynamic linker copies the data addressed by the symbol from the
     shared library into the object, because the object being
     run has to have the data at some particular address.  */
  HOWTO (R_I370_COPY,		/* type */
	 0,			/* rightshift */
	 4,			/* size (1 = byte, 2 = 16-bit, 4 = 32-bit) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc,	 /* special_function */
	 "R_I370_COPY",		/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0,			/* dst_mask */
	 false),		/* pcrel_offset */

  /* Used only by the dynamic linker.  When the object is run, this
     longword is set to the load address of the object, plus the
     addend.  */
  HOWTO (R_I370_RELATIVE,	/* type */
	 0,			/* rightshift */
	 4,			/* size (1 = byte, 2 = 16-bit, 4 = 32-bit) */
	 32,			/* bitsize */
	 false,			/* pc_relative */
	 0,			/* bitpos */
	 complain_overflow_bitfield, /* complain_on_overflow */
	 bfd_elf_generic_reloc,	 /* special_function */
	 "R_I370_RELATIVE",	/* name */
	 false,			/* partial_inplace */
	 0,			/* src_mask */
	 0xffffffff,		/* dst_mask */
	 false),		/* pcrel_offset */

};

/* The i370 compiler emits the following glue for PIC functions:

   .section .data.pool
   funcname:
      ST r12,68(r11)       addr  0:  save r12 in frame
      L  r12,12(r15)       addr  4:  load addr of funcname$fent
      BR r12               addr  8:  branch to funcname$fent
      .short 0             addr 10:  padding
      .long funcname$fent  addr 12:  addr of function in text section
      .long funcname$pool  addr 16:  location of literal pool
      .long stacksize      addr 20:  size of stackframe
      .long funcname$pgt   addr 24:  location of page table (for branches)
      .long 0              addr 28:  unused; resereved

  It is a combined PLT & GOT: the instructions are PLT-like, and the
  three addresses are GOT-like. It's combined because r15 provides the
  indes into the GOT (which is why no additional GOT is needed.)

  Anything linking to `funcname` has to make a copy of this, including
  both the PLT trampoline, the three relocations, the pool table that
  follows, and the page table. Grand total is usually 48 to 160 bytes.

  To lessen debugging confusion, the copy goes into the .data.plink
  section. The (local) relocs that follow go into the .rela.pool
  section.
 */

/* Total size of of above entry. */
#define PLINK_ENTRY_SIZE 32

/* Total number of relocs in the entry. */
#define PLINK_RELOCS 3

#define PLINK_INTRO_SIZE 12
const bfd_byte plink_code[PLINK_INTRO_SIZE] = {
  0x50, 0xc0, 0xb0, 0x44, /* ST  r12,68(,r11) */
  0x58, 0xc0, 0xf0, 0x0c, /* L   r12,12(,r15) */
  0x07, 0xfc,             /* BR   r12 */
  0x07, 0x00              /* NOOP for alignment */
};


/* Initialize the i370_elf_howto_table, so that linear accesses can be done.  */

static void
i370_elf_howto_init (void)
{
  unsigned int i, type;

  for (i = 0; i < sizeof (i370_elf_howto_raw) / sizeof (i370_elf_howto_raw[0]); i++)
    {
      type = i370_elf_howto_raw[i].type;
      BFD_ASSERT (type < sizeof (i370_elf_howto_table) / sizeof (i370_elf_howto_table[0]));
      i370_elf_howto_table[type] = &i370_elf_howto_raw[i];
    }
}

static reloc_howto_type *
i370_elf_reloc_type_lookup (bfd *abfd ATTRIBUTE_UNUSED,
			    bfd_reloc_code_real_type code)
{
  enum i370_reloc_type i370_reloc = R_I370_NONE;

  if (!i370_elf_howto_table[ R_I370_ADDR31 ])
    /* Initialize howto table if needed.  */
    i370_elf_howto_init ();

  switch ((int) code)
    {
    default:
      return NULL;

    case BFD_RELOC_NONE:	i370_reloc = R_I370_NONE;	break;
    case BFD_RELOC_32:		i370_reloc = R_I370_ADDR31;	break;
    case BFD_RELOC_16:		i370_reloc = R_I370_ADDR16;	break;
    case BFD_RELOC_32_PCREL:	i370_reloc = R_I370_REL31;	break;
    case BFD_RELOC_CTOR:	i370_reloc = R_I370_ADDR31;	break;
    case BFD_RELOC_I370_D12:	i370_reloc = R_I370_ADDR12;	break;
    }

  return i370_elf_howto_table[ (int)i370_reloc ];
};

static reloc_howto_type *
i370_elf_reloc_name_lookup (bfd *abfd ATTRIBUTE_UNUSED,
			    const char *r_name)
{
  unsigned int i;

  for (i = 0;
       i < sizeof (i370_elf_howto_raw) / sizeof (i370_elf_howto_raw[0]);
       i++)
    if (i370_elf_howto_raw[i].name != NULL
	&& strcasecmp (i370_elf_howto_raw[i].name, r_name) == 0)
      return &i370_elf_howto_raw[i];

  return NULL;
}

/* The name of the dynamic interpreter.  This is put in the .interp
    section.  */

#define ELF_DYNAMIC_INTERPRETER "/lib/ld.so"

/* Set the howto pointer for an i370 ELF reloc.  */

static bool
i370_elf_info_to_howto (bfd *abfd ATTRIBUTE_UNUSED,
			arelent *cache_ptr,
			Elf_Internal_Rela *dst)
{
  unsigned int r_type;

  if (!i370_elf_howto_table[ R_I370_ADDR31 ])
    /* Initialize howto table.  */
    i370_elf_howto_init ();

  r_type = ELF32_R_TYPE (dst->r_info);
  if (r_type >= R_I370_max)
    {
      /* xgettext:c-format */
      _bfd_error_handler (_("%pB: unrecognised I370 reloc number: %d"),
			  abfd, r_type);
      bfd_set_error (bfd_error_bad_value);
      r_type = R_I370_NONE;
    }
  cache_ptr->howto = i370_elf_howto_table[r_type];
  return cache_ptr->howto != NULL;
}

/* Hack alert --  the following several routines look generic to me ...
   why are we bothering with them ?  */
/* Function to set whether a module needs the -mrelocatable bit set.  */

static bool
i370_elf_set_private_flags (bfd *abfd, flagword flags)
{
  BFD_ASSERT (!elf_flags_init (abfd)
	      || elf_elfheader (abfd)->e_flags == flags);

  elf_elfheader (abfd)->e_flags = flags;
  elf_flags_init (abfd) = true;
  return true;
}

/* Merge backend specific data from an object file to the output
   object file when linking.  */

static bool
i370_elf_merge_private_bfd_data (bfd *ibfd, struct bfd_link_info *info)
{
  bfd *obfd = info->output_bfd;
  flagword old_flags;
  flagword new_flags;

  if (bfd_get_flavour (ibfd) != bfd_target_elf_flavour
      || bfd_get_flavour (obfd) != bfd_target_elf_flavour)
    return true;

  new_flags = elf_elfheader (ibfd)->e_flags;
  old_flags = elf_elfheader (obfd)->e_flags;
  if (!elf_flags_init (obfd))	/* First call, no flags set.  */
    {
      elf_flags_init (obfd) = true;
      elf_elfheader (obfd)->e_flags = new_flags;
    }

  else if (new_flags == old_flags)	/* Compatible flags are ok.  */
    ;

  else					/* Incompatible flags.  */
    {
      _bfd_error_handler
	/* xgettext:c-format */
	(_("%pB: uses different e_flags (%#x) fields than previous modules (%#x)"),
	 ibfd, new_flags, old_flags);

      bfd_set_error (bfd_error_bad_value);
      return false;
    }

  return true;
}

/* Handle an i370-specific section when reading an object file.  This
   is called when elfcode.h finds a section with an unknown type.
   Currently this is never called!? Implementation below is what most
   other arches do... !? Maybe we can gut this out to nothing!? */

static bool
i370_elf_section_from_shdr (bfd *abfd,
			    Elf_Internal_Shdr *hdr,
			    const char *name,
			    int shindex)
{
  asection *newsect;
  flagword flags;

#ifdef DEBUG
  fprintf(stderr, "section_from_shdr called for %s (%d) in %s\n",
    name, shindex, bfd_get_filename(abfd));
#endif

  if (! _bfd_elf_make_section_from_shdr (abfd, hdr, name, shindex))
    return false;

  newsect = hdr->bfd_section;
  flags = bfd_section_flags (newsect);
  if (hdr->sh_type == SHT_ORDERED)
    flags |= SEC_SORT_ENTRIES;

  bfd_set_section_flags (newsect, flags);
  return true;
}

/* Set up any other section flags and such that may be necessary.  */

static bool
i370_elf_fake_sections (bfd *abfd ATTRIBUTE_UNUSED,
			Elf_Internal_Shdr *shdr,
			asection *asect)
{
#ifdef DEBUG
  fprintf(stderr, "fake_sections called for %s in %s\n",
    bfd_section_name(asect), bfd_get_filename(abfd));
#endif

  if ((asect->flags & (SEC_GROUP | SEC_EXCLUDE)) == SEC_EXCLUDE)
    shdr->sh_flags |= SHF_EXCLUDE;

  if ((asect->flags & SEC_SORT_ENTRIES) != 0)
    shdr->sh_type = SHT_ORDERED;

  return true;
}

/* Create the .data.plink section that will hold PLT entries.
   We give this a non-standard name for the moment, because the
   i370 glue is non-standard/unusual. But it does look like a PLT
   in the overall general sense.

   TODO: might also need .dynsbss and .rela.sbss and maybe other things
   stolen from _bfd_elf_create_dynamic_sections. Under construction,
   not everything works yet.
 */
static bool
i370_elf_create_dynamic_sections (bfd *abfd, struct bfd_link_info *info)
{
  struct elf_link_hash_table *htab = elf_hash_table (info);

  asection *s;
  flagword flags;
  flags = (SEC_ALLOC | SEC_CODE | SEC_LOAD | SEC_HAS_CONTENTS
	   | SEC_LINKER_CREATED);

#ifdef DEBUG
  fprintf(stderr, "i370_elf_create_dynamic_sections called\n");
#endif

  s = bfd_make_section_anyway_with_flags (abfd, ".data.plink", flags);
  if (s == NULL)
    return false;
  htab->splt = s;

  s = bfd_make_section_anyway_with_flags (abfd, ".rela.pool",
                                          flags | SEC_READONLY);
  if (s == NULL)
    return false;
  htab->srelplt = s;

  /* .rela.pool holds the relocations that are needed for .data.plink
     Specifically, the location of the page table and the pool table.  */
  elf_section_data (htab->splt)->sreloc = s;

  return true;
}

/* Look through the relocs for an input section, and allocate space
   for that reloc.  This is called for each input bfd, one by one,
   and so anything we see here might be provided by later sections.

   Under construction. Might not do the right thing.
 */
static bool
i370_elf_check_relocs (bfd *abfd,
		       struct bfd_link_info *info,
		       asection *sec,
		       const Elf_Internal_Rela *relocs)
{
  bfd *dynobj;
  Elf_Internal_Shdr *symtab_hdr;
  struct elf_link_hash_entry **sym_hashes;
  const Elf_Internal_Rela *rel;
  const Elf_Internal_Rela *rel_end;
  asection *sreloc;

#ifdef DEBUG
  fprintf(stderr,
      "check_relocs called for %s in %s is-reloc=%d is-pic=%d nrelocs=%u\n",
       bfd_section_name(sec), bfd_get_filename(abfd),
       bfd_link_relocatable (info), bfd_link_pic (info), sec->reloc_count);
#endif

  /* If not a relocatable object, there's nothing to do. */
  if (bfd_link_relocatable (info))
    return true;

  /* If we're not looking at a PIC object, there's nothing to do. */
  if (!bfd_link_pic (info))
    return true;

  /* If there aren't any relocations, there's nothing to do. */
  /* This is probably an error condition... */
  if (0 == sec->reloc_count)
    return true;

  dynobj = elf_hash_table (info)->dynobj;
  symtab_hdr = &elf_tdata (abfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (abfd);

  sreloc = _bfd_elf_make_dynamic_reloc_section
                   (sec, dynobj, 2, abfd, /*rela?*/ true);

  if (sreloc == NULL) return false;

  rel_end = relocs + sec->reloc_count;
  for (rel = relocs; rel < rel_end; rel++)
    {
      unsigned long r_symndx;
      struct elf_link_hash_entry *h;

      r_symndx = ELF32_R_SYM (rel->r_info);
      if (r_symndx < symtab_hdr->sh_info)
        h = NULL;
      else
	{
	  h = sym_hashes[r_symndx - symtab_hdr->sh_info];
	  while (h->root.type == bfd_link_hash_indirect
		 || h->root.type == bfd_link_hash_warning)
	    h = (struct elf_link_hash_entry *) h->root.u.i.link;
	}

#ifdef DEBUG
      fprintf (stderr,
	   "i370_elf_check_relocs: dynamic relocation needed for %s\n",
	   (h && h->root.root.string)
	   ? h->root.root.string : "<unknown>");
#endif
      sreloc->size += sizeof (Elf32_External_Rela);
    }

  return true;
}

/* Adjust a symbol defined by a dynamic object and referenced by a
   regular object.  The current definition is in some section of the
   dynamic object, but those sections are not being copied to the
   output bfd.  This function provides an opportunity to tweak the
   symbol as needed.  */

static bool
i370_elf_adjust_dynamic_symbol (struct bfd_link_info *info,
				struct elf_link_hash_entry *h)
{
  struct elf_link_hash_table *htab = elf_hash_table (info);
  bfd *dynobj = htab->dynobj;

#ifdef DEBUG
  /* h->root.u.def.section->vma points at the start of .data.pool
     (in the input shared object.) The h->root.u.def.value is the
     offset to the entry for the function.  */
  asection *sin = h->root.u.def.section;
  bfd_vma loco = sin->vma + h->root.u.def.value;

  fprintf (stderr,
     "i370_adjust_dynamic in %s at %lx size=%lx sym= %s\n",
      bfd_section_name(sin), loco, h->size, h->root.root.string);
#endif

  /* Make sure we know what is going on here.  */
  BFD_ASSERT (dynobj != NULL
	      && (h->needs_plt
		  || h->is_weakalias
		  || (h->def_dynamic
		      && h->ref_regular
		      && !h->def_regular)));

  asection *splt = bfd_get_linker_section (dynobj, ".data.plink");
  BFD_ASSERT (splt != NULL);
  BFD_ASSERT (splt == htab->splt);

  /* We'll be copying the dynamic objects .data.pool entry to
     the output objects "local" .data.plink section. FWIW these
     are both effectively .plt entries but are used in a rather
     non-conventional way, so get different names. */
  h->needs_plt = 1;

  /* The total size is a sum of the glue, plus $pool plus $pgt,
     typically 48 to maybe 160 bytes, depending. */
  h->plt.offset = splt->size;
  splt->size += h->size;

  return true;

#ifdef NOT_YET_MAYBE_NEVER
  /* If this is a weak symbol, and there is a real definition, the
     processor independent code will have arranged for us to see the
     real definition first, and we can just use the same value.  */
  if (h->is_weakalias)
    {
      struct elf_link_hash_entry *def = weakdef (h);
      BFD_ASSERT (def->root.type == bfd_link_hash_defined);
      h->root.u.def.section = def->root.u.def.section;
      h->root.u.def.value = def->root.u.def.value;
      return true;
    }

  /* This is a reference to a symbol defined by a dynamic object which
     is not a function.  */

  /* If we are creating a shared library, we must presume that the
     only references to the symbol are via the global offset table.
     For such cases we need not do anything here; the relocations will
     be handled correctly by relocate_section.  */
  if (bfd_link_pic (info))
    return true;

  /* We must allocate the symbol in our .dynbss section, which will
     become part of the .bss section of the executable.  There will be
     an entry for this symbol in the .dynsym section.  The dynamic
     object will contain position independent code, so all references
     from the dynamic object to this symbol will go through the global
     offset table.  The dynamic linker will use the .dynsym entry to
     determine the address it must put in the global offset table, so
     both the dynamic object and the regular object will refer to the
     same memory location for the variable.

     Of course, if the symbol is sufficiently small, we must instead
     allocate it in .sbss.  FIXME: It would be better to do this if and
     only if there were actually SDAREL relocs for that symbol.  */

  if (h->size <= elf_gp_size (dynobj))
    s = bfd_get_linker_section (dynobj, ".dynsbss");
  else
    s = bfd_get_linker_section (dynobj, ".dynbss");
  BFD_ASSERT (s != NULL);

  /* We must generate a R_I370_COPY reloc to tell the dynamic linker to
     copy the initial value out of the dynamic object and into the
     runtime process image.  We need to remember the offset into the
     .rela.bss section we are going to use.  */
  if ((h->root.u.def.section->flags & SEC_ALLOC) != 0 && h->size != 0)
    {
      asection *srel;

      if (h->size <= elf_gp_size (dynobj))
	srel = bfd_get_linker_section (dynobj, ".rela.sbss");
      else
	srel = bfd_get_linker_section (dynobj, ".rela.bss");
      BFD_ASSERT (srel != NULL);
      srel->size += sizeof (Elf32_External_Rela);
      h->needs_copy = 1;
    }

  return _bfd_elf_adjust_dynamic_copy (info, h, s);
#endif
}

/* Allocate space in .rela.pool for dynamic relocs.
   Called by late_size_sections.  */
static bool
allocate_dynrelocs (struct elf_link_hash_entry *h, void * goober)
{
  struct elf_link_hash_table *htab = goober;
  struct elf_dyn_relocs *p, **head;

  /* Don't bother if nothing is needed */
  if (!h->needs_plt)
    return true;

#ifdef DEBUG
  fprintf(stderr, "allocate_dynrelocs for %s %s\n",
          bfd_section_name(h->root.u.def.section), h->root.root.string);
#endif

  if (h->root.type == bfd_link_hash_indirect)
    return true;

  /* This should never happen. */
  if (0 == htab->dynamic_sections_created)
    return true;

  /* There are three relocs in the dynamic linkage. Allocate space
     for them.  XXX This is wrong, but a placeholder fow now. */
  size_t amt = sizeof(struct elf_dyn_relocs);
  p = ((struct elf_dyn_relocs *) bfd_alloc (htab->dynobj, amt));
  if (p == NULL) return false;

  head = &h->dyn_relocs;
  p->next = *head;
  *head = p;
  p->sec = htab->splt;
  p->count = PLINK_RELOCS;
  p->pc_count = 0;

  /* Allocate space.  */
  for (p = h->dyn_relocs; p != NULL; p = p->next)
    {
      asection *sreloc = elf_section_data (p->sec)->sreloc;
      sreloc->size += p->count * sizeof (Elf32_External_Rela);
    }

  return true;
}

#define is_i370_elf(bfd) \
  (bfd_get_flavour (bfd) == bfd_target_elf_flavour)

/* Set the sizes of the dynamic sections.
   Copied from elf_m68k_late_size_sections.  */

static bool
i370_elf_late_size_sections (bfd *output_bfd,
			     struct bfd_link_info *info)
{
  bfd *dynobj;
  asection *s;
  bool plt = false;
  bool relocs = false;
  bool reltext = false;
  bfd *ibfd;

#ifdef DEBUG
  fprintf(stderr, "i370_elf_late_size_sections entered for %s\n",
          bfd_get_filename(output_bfd));
#endif

  dynobj = elf_hash_table (info)->dynobj;
  if (dynobj == NULL)
    return true;

  if (elf_hash_table (info)->dynamic_sections_created)
    {
      /* Set the contents of the .interp section to the interpreter.  */
      if (bfd_link_executable (info) && !info->nointerp)
	{
	  s = bfd_get_linker_section (dynobj, ".interp");
	  BFD_ASSERT (s != NULL);
	  s->size = sizeof ELF_DYNAMIC_INTERPRETER;
	  s->contents = (unsigned char *) ELF_DYNAMIC_INTERPRETER;
	}
    }
  else
    {
      /* We may have created entries in the .rela.got, .rela.sdata, and
	 .rela.sdata2 sections.  However, if we are not creating the
	 dynamic sections, we will not actually use these entries.  Reset
	 the size of .rela.got, et al, which will cause it to get
	 stripped from the output file below.  */
      static char *rela_sections[] = { ".rela.got", ".rela.sdata",
				       ".rela.sdata2", ".rela.sbss",
				       NULL };
      char **p;

      for (p = rela_sections; *p != NULL; p++)
	{
	  s = bfd_get_linker_section (dynobj, *p);
	  if (s != NULL)
	    s->size = 0;
	}
    }

  /* Set up space for local dynamic relocs. */
  for (ibfd = info->input_bfds; ibfd != NULL; ibfd = ibfd->link.next)
    {
      asection *srel;

      for (s = ibfd->sections; s != NULL; s = s->next)
	{
	  struct elf_dyn_relocs *p;

	  if (!is_i370_elf (ibfd))
	    continue;

	  for (p = ((struct elf_dyn_relocs *)
		   elf_section_data (s)->local_dynrel);
	       p != NULL; p = p->next)
	    {
	      if (p->count != 0)
		{
		  srel = elf_section_data (p->sec)->sreloc;
		  srel->size += p->count * sizeof (Elf32_External_Rela);
		  if ((p->sec->output_section->flags & SEC_READONLY) != 0)
		    info->flags |= DF_TEXTREL;
		}
	    }
	}
    }

  /* Allocate space for global sym dynamic relocs. */
  struct elf_link_hash_table *htab = elf_hash_table (info);
  elf_link_hash_traverse (elf_hash_table (info), allocate_dynrelocs, htab);

  /* The check_relocs and adjust_dynamic_symbol entry points have
     determined the sizes of the various dynamic sections.  Allocate
     memory for them.  */
  for (s = dynobj->sections; s != NULL; s = s->next)
    {
      const char *name;

      if ((s->flags & SEC_LINKER_CREATED) == 0)
	continue;

      /* It's OK to base decisions on the section name, because none
         of the dynobj section names depend upon the input files.  */

      name = bfd_section_name (s);
      if (strcmp (name, ".data.plink") == 0)
	{
	  /* Remember whether there is a PLT.  */
	  plt = s->size != 0;
	}
      else if (startswith (name, ".rela"))
	{
	  if (s->size != 0)
	    {
	      asection *target;
	      const char *outname;

	      /* Remember whether there are any relocation sections.  */
	      relocs = true;

	      /* If this relocation section applies to a read only
		 section, then we probably need a DT_TEXTREL entry.  */
	      outname = bfd_section_name (s->output_section);
	      target = bfd_get_section_by_name (output_bfd, outname + 5);
	      if (target != NULL
		  && (target->flags & SEC_READONLY) != 0
		  && (target->flags & SEC_ALLOC) != 0)
		reltext = true;

	      /* We use the reloc_count field as a counter if we need
		 to copy relocs into the output file.  */
	      s->reloc_count = 0;
	    }
	}
      else if (strcmp (name, ".got") != 0
	       && strcmp (name, ".sdata") != 0
	       && strcmp (name, ".sdata2") != 0
	       && strcmp (name, ".dynbss") != 0
	       && strcmp (name, ".dynsbss") != 0)
	{
	  /* It's not one of our sections, so don't allocate space.  */
	  continue;
	}

      if (s->size == 0)
	{
	  /* If we don't need this section, strip it from the output
	     file.  This is for the .rela.bss and .rela.pool sections,
	     which might get created in create_dynamic_sections() but
	     are then never populated in adjust_dynamic_symbol().  */
	  s->flags |= SEC_EXCLUDE;
	  continue;
	}

      if ((s->flags & SEC_HAS_CONTENTS) == 0)
	continue;

      /* Allocate memory for the section contents.  */
      s->contents = (bfd_byte *) bfd_zalloc (dynobj, s->size);
      if (s->contents == NULL)
	return false;
    }

  /* Add some entries to the .dynamic section.  We fill in the
     values later, in i370_elf_finish_dynamic_sections, but we
     must add the entries now so that we get the correct size for
     the .dynamic section.  The DT_DEBUG entry is filled in by the
     dynamic linker and used by the debugger.  */
/* Currently unused/un-needed, due to unfinished implementation?! */
  return _bfd_elf_add_dynamic_tags (output_bfd, info, relocs|plt|reltext);
}

/* Copy the entire prolog glue from the PIC object to the .data.plink
   section. This is the PLT glue, the pointer into the text section,
   and the offsets to the pool and page tables. Generate relocs
   for the assorted entries. */
static bool
i370_plink_entry_copy(bfd * pic_bfd,
                      struct bfd_link_info *info,
                      struct elf_link_hash_entry *h)
{
  struct elf_link_hash_table *htab = elf_hash_table (info);
  asection *splt = htab->splt;

  bfd_vma offset = h->plt.offset;
  bfd_byte * to_loc = splt->contents + offset;

  /* Done previously? */
  if (to_loc[0]) return true;

  bfd_vma from_loc = h->root.u.def.value;
  asection *s = h->root.u.def.section;
#ifdef DEBUG
  /* h->root.u.def.section->vma points at the start of .data.pool
     in the input shared object where the function symbol is defined.
     The h->root.u.def.value is the offset to the entry for the
     function.  */
  printf("plink_entry_copy from %lx + %lx to %lx size %lx name= %s\n",
         s->vma, from_loc, offset, h->size, h->root.root.string);
  printf("from section s=%s in %s\n", bfd_section_name(s),
        bfd_get_filename(pic_bfd));
#endif

  return bfd_get_section_contents(pic_bfd, s, to_loc, from_loc, h->size);
}

/* The RELOCATE_SECTION function is called by the ELF backend linker
   to handle the relocations for a section.

   The relocs are always passed as Rela structures; if the section
   actually uses Rel structures, the r_addend field will always be
   zero.

   This function is responsible for adjusting the section contents as
   necessary, and (if using Rela relocs and generating a
   relocatable output file) adjusting the reloc addend as
   necessary.

   This function does not have to worry about setting the reloc
   address or the reloc symbol index.

   LOCAL_SYMS is a pointer to the swapped in local symbols.

   LOCAL_SECTIONS is an array giving the section in the input file
   corresponding to the st_shndx field of each local symbol.

   The global hash table entry for the global symbols can be found
   via elf_sym_hashes (input_bfd).

   When generating relocatable output, this function must handle
   STB_LOCAL/STT_SECTION symbols specially.  The output symbol is
   going to be the section symbol corresponding to the output
   section, which means that the addend must be adjusted
   accordingly.  */

static int
i370_elf_relocate_section (bfd *output_bfd,
			   struct bfd_link_info *info,
			   bfd *input_bfd,
			   asection *input_section,
			   bfd_byte *contents,
			   Elf_Internal_Rela *relocs,
			   Elf_Internal_Sym *local_syms,
			   asection **local_sections)
{
  Elf_Internal_Shdr *symtab_hdr = &elf_tdata (input_bfd)->symtab_hdr;
  struct elf_link_hash_entry **sym_hashes = elf_sym_hashes (input_bfd);
  Elf_Internal_Rela *rel = relocs;
  Elf_Internal_Rela *relend = relocs + input_section->reloc_count;
  asection *sreloc = NULL;
  bool ret = true;

#ifdef DEBUG
  fprintf(stderr, "relocate_section for %s with %u relocs in %s\n",
          bfd_section_name(input_section), input_section->reloc_count,
          bfd_get_filename(input_bfd));
#endif

  if (!i370_elf_howto_table[ R_I370_ADDR31 ])
    /* Initialize howto table if needed.  */
    i370_elf_howto_init ();

  for (; rel < relend; rel++)
    {
      enum i370_reloc_type r_type    = (enum i370_reloc_type) ELF32_R_TYPE (rel->r_info);
      bfd_vma offset		     = rel->r_offset;
      bfd_vma addend		     = rel->r_addend;
      bfd_reloc_status_type r	     = bfd_reloc_other;
      Elf_Internal_Sym *sym	     = NULL;
      asection *sec		     = NULL;
      struct elf_link_hash_entry * h = NULL;
      const char *sym_name	     = NULL;
      reloc_howto_type *howto;
      unsigned long r_symndx;
      bfd_vma relocation;

      /* Unknown relocation handling.  */
      if ((unsigned) r_type >= (unsigned) R_I370_max
	  || !i370_elf_howto_table[(int)r_type])
	{
	  /* xgettext:c-format */
	  _bfd_error_handler (_("%pB: unknown relocation type %d"),
			      input_bfd, (int) r_type);

	  bfd_set_error (bfd_error_bad_value);
	  ret = false;
	  continue;
	}

      howto = i370_elf_howto_table[(int) r_type];
      r_symndx = ELF32_R_SYM (rel->r_info);
      relocation = 0;

      if (r_symndx < symtab_hdr->sh_info)
	{
	  sym = local_syms + r_symndx;
	  sec = local_sections[r_symndx];
	  sym_name = "<local symbol>";

	  relocation = _bfd_elf_rela_local_sym (output_bfd, sym, & sec, rel);
	  addend = rel->r_addend;
	}
      else
	{
	  h = sym_hashes[r_symndx - symtab_hdr->sh_info];

	  if (info->wrap_hash != NULL
	      && (input_section->flags & SEC_DEBUGGING) != 0)
	    h = ((struct elf_link_hash_entry *)
		 unwrap_hash_lookup (info, input_bfd, &h->root));

	  while (h->root.type == bfd_link_hash_indirect
		 || h->root.type == bfd_link_hash_warning)
	    h = (struct elf_link_hash_entry *) h->root.u.i.link;

	  sym_name = h->root.root.string;
	  if (h->root.type == bfd_link_hash_defined
	      || h->root.type == bfd_link_hash_defweak)
	    {
	      sec = h->root.u.def.section;

	      /* If no output section, then this must still be the
	         input .data.pool. Swap it out for .data.plink. */
	      if (NULL == sec->output_section)
		{
		  if (h->needs_plt)
		    {
		      if (!i370_plink_entry_copy(sec->owner, info, h))
			{
			  _bfd_error_handler (
			      "%pA: failed plt copy for symbol %s",
			      sec, sym_name);
			  ret = false;
			}
		      sec = elf_hash_table (info)->splt;
		      h->root.u.def.section = sec;
		    }
		  else
		    _bfd_error_handler (
		      "%pA: not output for symbol %s",
		      sec, sym_name);
		}

	      /* In these cases, we don't need the relocation value. */
	      if (bfd_link_pic (info)
		  && ((! info->symbolic && h->dynindx != -1)
		      || !h->def_regular)
		  && (input_section->flags & SEC_ALLOC) != 0
		  && (r_type == R_I370_ADDR31
		      || r_type == R_I370_COPY
		      || r_type == R_I370_ADDR16
		      || r_type == R_I370_RELATIVE))
		{}
	      else
		relocation = (h->root.u.def.value
			      + sec->output_section->vma
			      + sec->output_offset);
	    }
	  else if (h->root.type == bfd_link_hash_undefweak)
	    ;
	  else if (info->unresolved_syms_in_objects == RM_IGNORE
		   && ELF_ST_VISIBILITY (h->other) == STV_DEFAULT)
	    ;
	  else if (!bfd_link_relocatable (info))
	    {
	      (*info->callbacks->undefined_symbol)
		(info, h->root.root.string, input_bfd,
		 input_section, rel->r_offset,
		 (info->unresolved_syms_in_objects == RM_DIAGNOSE
		  || ELF_ST_VISIBILITY (h->other)));
	      ret = false;
	      continue;
	    }
	}

      if (sec != NULL && discarded_section (sec))
	RELOC_AGAINST_DISCARDED_SECTION (info, input_bfd, input_section,
					 rel, 1, relend, howto, 0, contents);

      if (bfd_link_relocatable (info))
	continue;

      switch ((int) r_type)
	{
	default:
	  _bfd_error_handler
	    (_("%pB: unknown relocation type %d for symbol %s"),
	     input_bfd, (int) r_type, sym_name);

	  bfd_set_error (bfd_error_bad_value);
	  ret = false;
	  continue;

	case (int) R_I370_NONE:
	  continue;

	/* Relocations that may need to be propagated if this is a shared
	   object.  */
	case (int) R_I370_REL31:
	  /* If these relocations are not to a named symbol, they can be
	     handled right here, no need to bother the dynamic linker.  */
	  if (h == NULL
	      || strcmp (h->root.root.string, "_GLOBAL_OFFSET_TABLE_") == 0)
	    break;
	/* Fall through.  */

	/* Relocations that always need to be propagated if this is a shared
	   object.  */
	case (int) R_I370_ADDR31:
	case (int) R_I370_ADDR16:
	  if (bfd_link_pic (info)
	      && r_symndx != STN_UNDEF)
	    {
	      Elf_Internal_Rela outrel;
	      bfd_byte *loc;
	      int skip;

#ifdef DEBUG
	      fprintf (stderr,
		       "i370_elf_relocate_section needs to create PIC relocation for %s\n",
		       (h && h->root.root.string) ? h->root.root.string : "<unknown>");
#endif

	      /* When generating a shared object, these relocations
		 are copied into the output file to be resolved at run
		 time.  */

	      if (sreloc == NULL)
		{
		  sreloc = _bfd_elf_get_dynamic_reloc_section
		    (input_bfd, input_section, /*rela?*/ true);

		  /* Hack around a confusing situation. Some bug
		    somewhere. We are called with relocations in
		    .rela.debug_info but there's nowhere to put them
		    because sreloc wasn't created and because there's
		    no sreloc->contents for it. I think this is because
		    there are literals in the debug sections, but
		    i370_elf_check_relocs() was never called for those.
		    Either this is he right behavior, or we have to call
		    i370_elf_check_relocs() on stripped debug sections,
		    anyway. I dunno. grep for strip_debugger in
		    _bfd_elf_link_iterate_on_relocs() */
		  if (sreloc == NULL
		      && (input_section->flags & SEC_DEBUGGING))
		    continue;

		  if (sreloc == NULL)
		    return false;
#ifdef DEBUG
		  _bfd_error_handler ("Work with dynamic section %pA", sreloc);
#endif
		  if (sreloc->contents == NULL)
		    {
		      bfd_set_error (bfd_error_no_contents);
		      return false;
		    }
		}

	      skip = 0;

	      outrel.r_offset =
		_bfd_elf_section_offset (output_bfd, info, input_section,
					 rel->r_offset);
	      if (outrel.r_offset == (bfd_vma) -1
		  || outrel.r_offset == (bfd_vma) -2)
		skip = (int) outrel.r_offset;
	      outrel.r_offset += (input_section->output_section->vma
				  + input_section->output_offset);

	      if (skip)
		memset (&outrel, 0, sizeof outrel);
	      /* h->dynindx may be -1 if this symbol was marked to
		 become local.  */
	      else if (h != NULL
		       && ((! info->symbolic && h->dynindx != -1)
			   || !h->def_regular))
		{
		  BFD_ASSERT (h->dynindx != -1);
		  outrel.r_info = ELF32_R_INFO (h->dynindx, r_type);
		  outrel.r_addend = rel->r_addend;
		}
	      else
		{
		  if (r_type == R_I370_ADDR31)
		    {
		      outrel.r_info = ELF32_R_INFO (0, R_I370_RELATIVE);
		      outrel.r_addend = relocation + rel->r_addend;
		    }
		  else
		    {
		      long indx;

		      if (bfd_is_abs_section (sec))
			indx = 0;
		      else if (sec == NULL || sec->owner == NULL)
			{
			  bfd_set_error (bfd_error_bad_value);
			  return false;
			}
		      else
			{
			  asection *osec;

			  /* We are turning this relocation into one
			     against a section symbol.  It would be
			     proper to subtract the symbol's value,
			     osec->vma, from the emitted reloc addend,
			     but ld.so expects buggy relocs.  */
			  osec = sec->output_section;
			  indx = elf_section_data (osec)->dynindx;
			  if (indx == 0)
			    {
			      struct elf_link_hash_table *htab;
			      htab = elf_hash_table (info);
			      osec = htab->text_index_section;
			      indx = elf_section_data (osec)->dynindx;
			    }
			  BFD_ASSERT (indx != 0);
#ifdef DEBUG
			  if (indx <= 0)
			    {
			      printf ("indx=%ld section=%s flags=%08x name=%s\n",
				      indx, osec->name, osec->flags,
				      h->root.root.string);
			    }
#endif
			}

		      outrel.r_info = ELF32_R_INFO (indx, r_type);
		      outrel.r_addend = relocation + rel->r_addend;
		    }
		}

	      loc = sreloc->contents;
	      loc += sreloc->reloc_count++ * sizeof (Elf32_External_Rela);
	      bfd_elf32_swap_reloca_out (output_bfd, &outrel, loc);

	      /* This reloc will be computed at runtime, so there's no
		 need to do anything now, unless this is a RELATIVE
		 reloc in an unallocated section.  */
	      if (skip == -1
		  || (input_section->flags & SEC_ALLOC) != 0
		  || ELF32_R_TYPE (outrel.r_info) != R_I370_RELATIVE)
		continue;
	    }
	  break;

	case (int) R_I370_COPY:
	case (int) R_I370_RELATIVE:
	  _bfd_error_handler
	    /* xgettext:c-format */
	    (_("%pB: Relocation %s is not yet supported for symbol %s."),
	     input_bfd,
	     i370_elf_howto_table[(int) r_type]->name,
	     sym_name);

	  bfd_set_error (bfd_error_invalid_operation);
	  ret = false;
	  continue;
	}

#ifdef DEBUG
      fprintf (stderr, "   type= %s symndx= %ld off= %3lx add= %3lx sym= %s\n",
	       howto->name,
	       r_symndx,
	       (long) offset,
	       (long) addend,
	       sym_name);
#endif
      r = _bfd_final_link_relocate (howto, input_bfd, input_section, contents,
				    offset, relocation, addend);

      if (r != bfd_reloc_ok)
	{
	  ret = false;
	  switch (r)
	    {
	    default:
	      break;

	    case bfd_reloc_overflow:
	      {
		const char *name;

		if (h != NULL)
		  name = NULL;
		else
		  {
		    name = bfd_elf_string_from_elf_section (input_bfd,
							    symtab_hdr->sh_link,
							    sym->st_name);
		    if (name == NULL)
		      break;

		    if (*name == '\0')
		      name = bfd_section_name (sec);
		  }

		(*info->callbacks->reloc_overflow) (info,
						    (h ? &h->root : NULL),
						    name,
						    howto->name,
						    (bfd_vma) 0,
						    input_bfd,
						    input_section,
						    offset);
	      }
	      break;
	    }
	}
    }

#ifdef DEBUG
  fprintf (stderr, "\n");
#endif

  return ret;
}

/* Finish up dynamic symbol handling. Set the contents of the various
   dynamic sections.  */
static bool
i370_elf_finish_dynamic_symbol(bfd * output_bfd ATTRIBUTE_UNUSED,
                               struct bfd_link_info * info ATTRIBUTE_UNUSED,
                               struct elf_link_hash_entry * h,
                               Elf_Internal_Sym *sym ATTRIBUTE_UNUSED)
{
  if (!h->needs_plt) return true;

#ifdef DEBUG
  /* sym->st_shndx is the index of the section the symbol is in.
     It should usually be the index for .data.plink.
     sym->st_name is the index into the .dynsym section.
     sym->st_size == h->size when all is well.  */
  fprintf(stderr,
     "finish_dyn_symb at %lx size %lx dynidx %ld shndx %u in %s symb %s\n",
     sym->st_value, sym->st_size, sym->st_name, sym->st_shndx,
     bfd_get_filename(output_bfd), h->root.root.string);
#endif

  return true;
}

/* Finish up the dynamic sections.  */
/* XXX hack alert bogus This routine is mostly all junk and almost
   certainly does the wrong thing.  Its here simply because it does
   just enough to allow glibc-2.1 ld.so to compile & link.  */

static bool
i370_elf_finish_dynamic_sections (bfd *output_bfd,
				  struct bfd_link_info *info)
{
  asection *sdyn;
  bfd *dynobj = elf_hash_table (info)->dynobj;
  asection *sgot = elf_hash_table (info)->sgot;

#ifdef DEBUG
  fprintf (stderr, "i370_elf_finish_dynamic_sections called for %s\n",
           bfd_get_filename(output_bfd));
#endif

  sdyn = bfd_get_linker_section (dynobj, ".dynamic");

  if (elf_hash_table (info)->dynamic_sections_created)
    {
      asection *splt;
      Elf32_External_Dyn *dyncon, *dynconend;

      splt = elf_hash_table (info)->splt;
      BFD_ASSERT (splt != NULL && sdyn != NULL);

      dyncon = (Elf32_External_Dyn *) sdyn->contents;
      dynconend = (Elf32_External_Dyn *) (sdyn->contents + sdyn->size);
      for (; dyncon < dynconend; dyncon++)
	{
	  Elf_Internal_Dyn dyn;
	  asection *s;
	  bool size;

	  bfd_elf32_swap_dyn_in (dynobj, dyncon, &dyn);

	  switch (dyn.d_tag)
	    {
	    case DT_PLTGOT:
	      s = elf_hash_table (info)->splt;
	      size = false;
	      break;
	    case DT_PLTRELSZ:
	      s = elf_hash_table (info)->srelplt;
	      size = true;
	      break;
	    case DT_JMPREL:
	      s = elf_hash_table (info)->srelplt;
	      size = false;
	      break;
	    default:
	      continue;
	    }

	  if (s == NULL)
	    dyn.d_un.d_val = 0;
	  else
	    {
	      if (!size)
		dyn.d_un.d_ptr = s->output_section->vma + s->output_offset;
	      else
		dyn.d_un.d_val = s->size;
	    }
	  bfd_elf32_swap_dyn_out (output_bfd, &dyn, dyncon);
	}
    }

  if (sgot && sgot->size != 0)
    {
      unsigned char *contents = sgot->contents;

      if (sdyn == NULL)
	bfd_put_32 (output_bfd, (bfd_vma) 0, contents);
      else
	bfd_put_32 (output_bfd,
		    sdyn->output_section->vma + sdyn->output_offset,
		    contents);

      elf_section_data (sgot->output_section)->this_hdr.sh_entsize = 4;
    }

  if (bfd_link_pic (info))
    {
      asection *sdynsym;
      asection *s;
      Elf_Internal_Sym sym;
      int maxdindx = 0;

      /* Set up the section symbols for the output sections.  */

      sdynsym = bfd_get_linker_section (dynobj, ".dynsym");
      BFD_ASSERT (sdynsym != NULL);

      sym.st_size = 0;
      sym.st_name = 0;
      sym.st_info = ELF_ST_INFO (STB_LOCAL, STT_SECTION);
      sym.st_other = 0;
      sym.st_target_internal = 0;

      for (s = output_bfd->sections; s != NULL; s = s->next)
	{
	  int indx, dindx;
	  Elf32_External_Sym *esym;

	  sym.st_value = s->vma;

	  indx = elf_section_data (s)->this_idx;
	  dindx = elf_section_data (s)->dynindx;
	  // dindx is -1 if there are no dynamic symbols.
	  // dindx is 0 if there are symbols in debugging sections,
	  // but those are being stripped.
	  if (dindx > 0)
	    {
	      BFD_ASSERT(indx > 0);

	      if (dindx > maxdindx)
		maxdindx = dindx;

	      sym.st_shndx = indx;

	      esym = (Elf32_External_Sym *) sdynsym->contents + dindx;
	      bfd_elf32_swap_symbol_out (output_bfd, &sym, esym, NULL);
	    }
	}

      /* Set the sh_info field of the output .dynsym section to the
	 index of the first global symbol.  */
      elf_section_data (sdynsym->output_section)->this_hdr.sh_info =
	maxdindx + 1;
    }

  return true;
}

#define TARGET_BIG_SYM		i370_elf32_vec
#define TARGET_BIG_NAME		"elf32-i370"
#define ELF_ARCH		bfd_arch_i370
#define ELF_MACHINE_CODE	EM_S370
#ifdef EM_I370_OLD
#define ELF_MACHINE_ALT1	EM_I370_OLD
#endif
#define ELF_MAXPAGESIZE		0x1000
#define ELF_OSABI		ELFOSABI_GNU

#define elf_info_to_howto	i370_elf_info_to_howto

#define elf_backend_plt_not_loaded 1
#define elf_backend_rela_normal    1

#define bfd_elf32_bfd_reloc_type_lookup		i370_elf_reloc_type_lookup
#define bfd_elf32_bfd_reloc_name_lookup		i370_elf_reloc_name_lookup
#define bfd_elf32_bfd_set_private_flags		i370_elf_set_private_flags
#define bfd_elf32_bfd_merge_private_bfd_data	i370_elf_merge_private_bfd_data
#define elf_backend_relocate_section		i370_elf_relocate_section

/* Dynamic loader support is mostly broken; just enough here to be able to
   link glibc's ld.so without errors.  */
#define elf_backend_create_dynamic_sections	i370_elf_create_dynamic_sections
#define elf_backend_init_index_section		_bfd_elf_init_1_index_section
#define elf_backend_finish_dynamic_sections	i370_elf_finish_dynamic_sections
#define elf_backend_fake_sections		i370_elf_fake_sections
#define elf_backend_late_size_sections		i370_elf_late_size_sections
#define elf_backend_section_from_shdr		i370_elf_section_from_shdr
#define elf_backend_adjust_dynamic_symbol	i370_elf_adjust_dynamic_symbol
#define elf_backend_check_relocs		i370_elf_check_relocs
#define elf_backend_finish_dynamic_symbol i370_elf_finish_dynamic_symbol

#include "elf32-target.h"
