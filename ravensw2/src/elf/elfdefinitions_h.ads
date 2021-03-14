pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package elfdefinitions_h is

   --  unsupported macro: DT_DEPRECATED_SPARC_REGISTER DT_SPARC_REGISTER
   --  unsupported macro: ELFOSABI_LINUX ELFOSABI_GNU
   --  unsupported macro: EM_AMD64 EM_X86_64
   --  unsupported macro: EM_ARC_A5 EM_ARC_COMPACT
   EV_NONE : constant := 0;  --  /raven/include/elfdefinitions.h:852
   EV_CURRENT : constant := 1;  --  /raven/include/elfdefinitions.h:853

   GRP_COMDAT : constant := 16#1#;  --  /raven/include/elfdefinitions.h:858
   GRP_MASKOS : constant := 16#0ff00000#;  --  /raven/include/elfdefinitions.h:859
   GRP_MASKPROC : constant := 16#f0000000#;  --  /raven/include/elfdefinitions.h:860

   VERSYM_VERSION : constant := 16#7fff#;  --  /raven/include/elfdefinitions.h:865
   VERSYM_HIDDEN : constant := 16#8000#;  --  /raven/include/elfdefinitions.h:866
   --  unsupported macro: PT_ARM_UNWIND PT_ARM_EXIDX
   --  unsupported macro: PT_HISUNW PT_HIOS
   --  unsupported macro: PT_LOSUNW PT_SUNWBSS
   --  unsupported macro: SHT_GNU_verdef SHT_SUNW_verdef
   --  unsupported macro: SHT_GNU_verneed SHT_SUNW_verneed
   --  unsupported macro: SHT_GNU_versym SHT_SUNW_versym

   PN_XNUM : constant := 16#FFFF#;  --  /raven/include/elfdefinitions.h:2058
   --  unsupported macro: NT_FREEBSD_ABI_TAG NT_ABI_TAG
   --  unsupported macro: NT_GNU_ABI_TAG NT_ABI_TAG
   --  unsupported macro: NT_NETBSD_IDENT NT_ABI_TAG
   --  unsupported macro: NT_OPENBSD_IDENT NT_ABI_TAG
   --  arg-macro: function ELF32_M_SYM (I)
   --    return (I) >> 8;
   --  arg-macro: function ELF32_M_SIZE (I)
   --    return (unsigned char) (I);
   --  arg-macro: function ELF32_M_INFO (M, S)
   --    return ((M) << 8) + (unsigned char) (S);
   --  arg-macro: function ELF64_M_SYM (I)
   --    return (I) >> 8;
   --  arg-macro: function ELF64_M_SIZE (I)
   --    return (unsigned char) (I);
   --  arg-macro: function ELF64_M_INFO (M, S)
   --    return ((M) << 8) + (unsigned char) (S);
   --  arg-macro: function ELF32_ST_BIND (I)
   --    return (I) >> 4;
   --  arg-macro: function ELF32_ST_TYPE (I)
   --    return (I) and 16#F#;
   --  arg-macro: function ELF32_ST_INFO (B, T)
   --    return ((B) << 4) + ((T) and 16#F#);
   --  arg-macro: function ELF64_ST_BIND (I)
   --    return (I) >> 4;
   --  arg-macro: function ELF64_ST_TYPE (I)
   --    return (I) and 16#F#;
   --  arg-macro: function ELF64_ST_INFO (B, T)
   --    return ((B) << 4) + ((T) and 16#F#);
   --  arg-macro: function ELF32_ST_VISIBILITY (O)
   --    return (O) and 16#3#;
   --  arg-macro: function ELF64_ST_VISIBILITY (O)
   --    return (O) and 16#3#;
   --  arg-macro: function ELF32_R_SYM (I)
   --    return (I) >> 8;
   --  arg-macro: function ELF32_R_TYPE (I)
   --    return (unsigned char) (I);
   --  arg-macro: function ELF32_R_INFO (S, T)
   --    return ((S) << 8) + (unsigned char) (T);
   --  arg-macro: function ELF64_R_SYM (I)
   --    return (I) >> 32;
   --  arg-macro: function ELF64_R_TYPE (I)
   --    return (I) and 16#FFFFFFFF#;
   --  arg-macro: function ELF64_R_INFO (S, T)
   --    return ((S) << 32) + ((T) and 16#FFFFFFFF#);

  ---
  -- * Copyright (c) 2010 Joseph Koshy
  -- * All rights reserved.
  -- *
  -- * Redistribution and use in source and binary forms, with or without
  -- * modification, are permitted provided that the following conditions
  -- * are met:
  -- * 1. Redistributions of source code must retain the above copyright
  -- *    notice, this list of conditions and the following disclaimer.
  -- * 2. Redistributions in binary form must reproduce the above copyright
  -- *    notice, this list of conditions and the following disclaimer in the
  -- *    documentation and/or other materials provided with the distribution.
  -- *
  -- * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
  -- * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  -- * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  -- * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
  -- * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  -- * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  -- * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  -- * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  -- * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  -- * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  -- * SUCH DAMAGE.
  -- *
  -- * $Id: elfdefinitions.h 3403 2016-02-13 15:39:27Z jkoshy $
  --

  -- * These definitions are based on:
  -- * - The public specification of the ELF format as defined in the
  -- *   October 2009 draft of System V ABI.
  -- *   See: http://www.sco.com/developers/gabi/latest/ch4.intro.html
  -- * - The May 1998 (version 1.5) draft of "The ELF-64 object format".
  -- * - Processor-specific ELF ABI definitions for sparc, i386, amd64, mips,
  -- *   ia64, and powerpc processors.
  -- * - The "Linkers and Libraries Guide", from Sun Microsystems.

  -- * Types of capabilities.
  -- * Flags used with dynamic linking entries.
  -- * Dynamic linking entry types.
  -- * Flags used in the executable header (field: e_flags).
  -- * Offsets in the `ei_ident[]` field of an ELF executable header.
  -- * The ELF class of an object.
  -- * Endianness of data in an ELF object.
  -- * Values of the magic numbers used in identification array.
  -- * ELF OS ABI field.
  -- * ELF Machine types: (EM_*).

  -- Other synonyms.
  -- * ELF file types: (ET_*).

  -- ELF file format version numbers.
  -- * Flags for section groups.
  -- * Flags / mask for .gnu.versym sections.
  -- * Flags used by program header table entries.
  -- * Types of program header table entries.
  -- synonyms.
  -- * Section flags.
  -- * Special section indices.
  -- * Section types.
  -- Aliases for section types.
  -- * Symbol binding information.
  -- * Symbol types
  -- * Symbol binding.
  -- * Symbol visibility.
  -- * Symbol flags.
  -- * Version dependencies.
  -- * Version flags.
  -- * Version needs
  -- * Version numbers.
  -- ** Relocation types.
  --
  -- * These are the symbols used in the Sun ``Linkers and Loaders
  -- * Guide'', Document No: 817-1984-17.  See the X86_64 relocations list
  -- * below for the spellings used in the ELF specification.

  -- * Relocation definitions from the ARM ELF ABI, version "ARM IHI
  -- * 0044E" released on 30th November 2012.

  -- ** ELF Types.

  -- Program address.
   subtype Elf32_Addr is unsigned;  -- /raven/include/elfdefinitions.h:2064

  -- Unsigned tiny integer.
   subtype Elf32_Byte is unsigned_char;  -- /raven/include/elfdefinitions.h:2065

  -- Unsigned medium integer.
   subtype Elf32_Half is unsigned_short;  -- /raven/include/elfdefinitions.h:2066

  -- File offset.
   subtype Elf32_Off is unsigned;  -- /raven/include/elfdefinitions.h:2067

  -- Section index.
   subtype Elf32_Section is unsigned_short;  -- /raven/include/elfdefinitions.h:2068

  -- Signed integer.
   subtype Elf32_Sword is int;  -- /raven/include/elfdefinitions.h:2069

  -- Unsigned integer.
   subtype Elf32_Word is unsigned;  -- /raven/include/elfdefinitions.h:2070

  -- Unsigned long integer.
   subtype Elf32_Lword is unsigned_long;  -- /raven/include/elfdefinitions.h:2071

  -- Program address.
   subtype Elf64_Addr is unsigned_long;  -- /raven/include/elfdefinitions.h:2073

  -- Unsigned tiny integer.
   subtype Elf64_Byte is unsigned_char;  -- /raven/include/elfdefinitions.h:2074

  -- Unsigned medium integer.
   subtype Elf64_Half is unsigned_short;  -- /raven/include/elfdefinitions.h:2075

  -- File offset.
   subtype Elf64_Off is unsigned_long;  -- /raven/include/elfdefinitions.h:2076

  -- Section index.
   subtype Elf64_Section is unsigned_short;  -- /raven/include/elfdefinitions.h:2077

  -- Signed integer.
   subtype Elf64_Sword is int;  -- /raven/include/elfdefinitions.h:2078

  -- Unsigned integer.
   subtype Elf64_Word is unsigned;  -- /raven/include/elfdefinitions.h:2079

  -- Unsigned long integer.
   subtype Elf64_Lword is unsigned_long;  -- /raven/include/elfdefinitions.h:2080

  -- Unsigned long integer.
   subtype Elf64_Xword is unsigned_long;  -- /raven/include/elfdefinitions.h:2081

  -- Signed long integer.
   subtype Elf64_Sxword is long;  -- /raven/include/elfdefinitions.h:2082

  -- * Capability descriptors.
  --

  -- 32-bit capability descriptor.
  -- Type of entry.
  -- Integer value.
  -- Pointer value.
   type Elf32_Cap_c_un_union (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            c_val : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2093
         when others =>
            c_ptr : aliased Elf32_Addr;  -- /raven/include/elfdefinitions.h:2094
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Cap_c_un_union);
   pragma Unchecked_Union (Elf32_Cap_c_un_union);type Elf32_Cap is record
      c_tag : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2091
      c_un : aliased Elf32_Cap_c_un_union;  -- /raven/include/elfdefinitions.h:2095
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Cap);  -- /raven/include/elfdefinitions.h:2096

  -- 64-bit capability descriptor.
  -- Type of entry.
  -- Integer value.
  -- Pointer value.
   type Elf64_Cap_c_un_union (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            c_val : aliased Elf64_Xword;  -- /raven/include/elfdefinitions.h:2102
         when others =>
            c_ptr : aliased Elf64_Addr;  -- /raven/include/elfdefinitions.h:2103
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Cap_c_un_union);
   pragma Unchecked_Union (Elf64_Cap_c_un_union);type Elf64_Cap is record
      c_tag : aliased Elf64_Xword;  -- /raven/include/elfdefinitions.h:2100
      c_un : aliased Elf64_Cap_c_un_union;  -- /raven/include/elfdefinitions.h:2104
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Cap);  -- /raven/include/elfdefinitions.h:2105

  -- * MIPS .conflict section entries.
  --

  -- 32-bit entry.
   type Elf32_Conflict is record
      c_index : aliased Elf32_Addr;  -- /raven/include/elfdefinitions.h:2113
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Conflict);  -- /raven/include/elfdefinitions.h:2114

  -- 64-bit entry.
   type Elf64_Conflict is record
      c_index : aliased Elf64_Addr;  -- /raven/include/elfdefinitions.h:2118
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Conflict);  -- /raven/include/elfdefinitions.h:2119

  -- * Dynamic section entries.
  --

  -- 32-bit entry.
  -- Type of entry.
  -- Integer value.
  -- Pointer value.
   type Elf32_Dyn_d_un_union (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            d_val : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2129
         when others =>
            d_ptr : aliased Elf32_Addr;  -- /raven/include/elfdefinitions.h:2130
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Dyn_d_un_union);
   pragma Unchecked_Union (Elf32_Dyn_d_un_union);type Elf32_Dyn is record
      d_tag : aliased Elf32_Sword;  -- /raven/include/elfdefinitions.h:2127
      d_un : aliased Elf32_Dyn_d_un_union;  -- /raven/include/elfdefinitions.h:2131
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Dyn);  -- /raven/include/elfdefinitions.h:2132

  -- 64-bit entry.
  -- Type of entry.
  -- Integer value.
  -- Pointer value;
   type Elf64_Dyn_d_un_union (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            d_val : aliased Elf64_Xword;  -- /raven/include/elfdefinitions.h:2138
         when others =>
            d_ptr : aliased Elf64_Addr;  -- /raven/include/elfdefinitions.h:2139
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Dyn_d_un_union);
   pragma Unchecked_Union (Elf64_Dyn_d_un_union);type Elf64_Dyn is record
      d_tag : aliased Elf64_Sxword;  -- /raven/include/elfdefinitions.h:2136
      d_un : aliased Elf64_Dyn_d_un_union;  -- /raven/include/elfdefinitions.h:2140
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Dyn);  -- /raven/include/elfdefinitions.h:2141

  -- * The executable header (EHDR).
  --

  -- 32 bit EHDR.
  -- ELF identification.
  -- Object file type (ET_*).
  -- Machine type (EM_*).
  -- File format version (EV_*).
  -- Start address.
  -- File offset to the PHDR table.
  -- File offset to the SHDRheader.
  -- Flags (EF_*).
  -- Elf header size in bytes.
  -- PHDR table entry size in bytes.
  -- Number of PHDR entries.
  -- SHDR table entry size in bytes.
  -- Number of SHDR entries.
  -- Index of section name string table.
   type Elf32_Ehdr_e_ident_array is array (0 .. 15) of aliased unsigned_char;
   type Elf32_Ehdr is record
      e_ident : aliased Elf32_Ehdr_e_ident_array;  -- /raven/include/elfdefinitions.h:2150
      e_type : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2151
      e_machine : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2152
      e_version : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2153
      e_entry : aliased Elf32_Addr;  -- /raven/include/elfdefinitions.h:2154
      e_phoff : aliased Elf32_Off;  -- /raven/include/elfdefinitions.h:2155
      e_shoff : aliased Elf32_Off;  -- /raven/include/elfdefinitions.h:2156
      e_flags : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2157
      e_ehsize : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2158
      e_phentsize : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2159
      e_phnum : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2160
      e_shentsize : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2161
      e_shnum : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2162
      e_shstrndx : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2163
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Ehdr);  -- /raven/include/elfdefinitions.h:2164

  -- 64 bit EHDR.
  -- ELF identification.
  -- Object file type (ET_*).
  -- Machine type (EM_*).
  -- File format version (EV_*).
  -- Start address.
  -- File offset to the PHDR table.
  -- File offset to the SHDRheader.
  -- Flags (EF_*).
  -- Elf header size in bytes.
  -- PHDR table entry size in bytes.
  -- Number of PHDR entries.
  -- SHDR table entry size in bytes.
  -- Number of SHDR entries.
  -- Index of section name string table.
   type Elf64_Ehdr_e_ident_array is array (0 .. 15) of aliased unsigned_char;
   type Elf64_Ehdr is record
      e_ident : aliased Elf64_Ehdr_e_ident_array;  -- /raven/include/elfdefinitions.h:2169
      e_type : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2170
      e_machine : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2171
      e_version : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2172
      e_entry : aliased Elf64_Addr;  -- /raven/include/elfdefinitions.h:2173
      e_phoff : aliased Elf64_Off;  -- /raven/include/elfdefinitions.h:2174
      e_shoff : aliased Elf64_Off;  -- /raven/include/elfdefinitions.h:2175
      e_flags : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2176
      e_ehsize : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2177
      e_phentsize : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2178
      e_phnum : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2179
      e_shentsize : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2180
      e_shnum : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2181
      e_shstrndx : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2182
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Ehdr);  -- /raven/include/elfdefinitions.h:2183

  -- * Shared object information.
  --

  -- 32-bit entry.
  -- The name of a shared object.
  -- 32-bit timestamp.
  -- Checksum of visible symbols, sizes.
  -- Interface version string index.
  -- Flags (LL_*).
   type Elf32_Lib is record
      l_name : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2192
      l_time_stamp : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2193
      l_checksum : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2194
      l_version : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2195
      l_flags : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2196
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Lib);  -- /raven/include/elfdefinitions.h:2197

  -- 64-bit entry.
  -- The name of a shared object.
  -- 32-bit timestamp.
  -- Checksum of visible symbols, sizes.
  -- Interface version string index.
  -- Flags (LL_*).
   type Elf64_Lib is record
      l_name : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2201
      l_time_stamp : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2202
      l_checksum : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2203
      l_version : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2204
      l_flags : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2205
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Lib);  -- /raven/include/elfdefinitions.h:2206

  -- * Note tags
  --

  -- Aliases for the ABI tag.
  -- * Note descriptors.
  --

  -- Length of note's name.
  -- Length of note's value.
  -- Type of note.
   type Elf_Note is record
      n_namesz : aliased unsigned;  -- /raven/include/elfdefinitions.h:2271
      n_descsz : aliased unsigned;  -- /raven/include/elfdefinitions.h:2272
      n_type : aliased unsigned;  -- /raven/include/elfdefinitions.h:2273
   end record;
   pragma Convention (C_Pass_By_Copy, Elf_Note);  -- /raven/include/elfdefinitions.h:2274

  -- 32-bit note header.
   subtype Elf32_Nhdr is Elf_Note;  -- /raven/include/elfdefinitions.h:2276

  -- 64-bit note header.
   subtype Elf64_Nhdr is Elf_Note;  -- /raven/include/elfdefinitions.h:2277

  -- * MIPS ELF options descriptor header.
  --

  -- Type of options.
  -- Size of option descriptor.
  -- Index of section affected.
  -- Kind-specific information.
   type Elf_Options is record
      kind : aliased Elf64_Byte;  -- /raven/include/elfdefinitions.h:2284
      size : aliased Elf64_Byte;  -- /raven/include/elfdefinitions.h:2285
      section : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2286
      info : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2287
   end record;
   pragma Convention (C_Pass_By_Copy, Elf_Options);  -- /raven/include/elfdefinitions.h:2288

  -- * Option kinds.
  --

  -- * ODK_EXCEPTIONS info field masks.
  --

  -- * ODK_PAD info field masks.
  --

  -- * ODK_HWPATCH info field masks.
  --

  -- * ODK_HWAND/ODK_HWOR info field and hwp_flags[12] masks.
  --

  -- * ODK_IDENT/ODK_GP_GROUP info field masks.
  --

  -- * MIPS ELF register info descriptor.
  --

  -- 32 bit RegInfo entry.
  -- Mask of general register used.
  -- Mask of coprocessor register used.
  -- GP register value.
   type Elf32_RegInfo_ri_cprmask_array is array (0 .. 3) of aliased Elf32_Word;
   type Elf32_RegInfo is record
      ri_gprmask : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2421
      ri_cprmask : aliased Elf32_RegInfo_ri_cprmask_array;  -- /raven/include/elfdefinitions.h:2422
      ri_gp_value : aliased Elf32_Addr;  -- /raven/include/elfdefinitions.h:2423
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_RegInfo);  -- /raven/include/elfdefinitions.h:2424

  -- 64 bit RegInfo entry.
  -- Mask of general register used.
  -- Padding.
  -- Mask of coprocessor register used.
  -- GP register value.
   type Elf64_RegInfo_ri_cprmask_array is array (0 .. 3) of aliased Elf64_Word;
   type Elf64_RegInfo is record
      ri_gprmask : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2428
      ri_pad : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2429
      ri_cprmask : aliased Elf64_RegInfo_ri_cprmask_array;  -- /raven/include/elfdefinitions.h:2430
      ri_gp_value : aliased Elf64_Addr;  -- /raven/include/elfdefinitions.h:2431
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_RegInfo);  -- /raven/include/elfdefinitions.h:2432

  -- * Program Header Table (PHDR) entries.
  --

  -- 32 bit PHDR entry.
  -- Type of segment.
  -- File offset to segment.
  -- Virtual address in memory.
  -- Physical address (if relevant).
  -- Size of segment in file.
  -- Size of segment in memory.
  -- Segment flags.
  -- Alignment constraints.
   type Elf32_Phdr is record
      p_type : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2440
      p_offset : aliased Elf32_Off;  -- /raven/include/elfdefinitions.h:2441
      p_vaddr : aliased Elf32_Addr;  -- /raven/include/elfdefinitions.h:2442
      p_paddr : aliased Elf32_Addr;  -- /raven/include/elfdefinitions.h:2443
      p_filesz : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2444
      p_memsz : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2445
      p_flags : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2446
      p_align : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2447
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Phdr);  -- /raven/include/elfdefinitions.h:2448

  -- 64 bit PHDR entry.
  -- Type of segment.
  -- Segment flags.
  -- File offset to segment.
  -- Virtual address in memory.
  -- Physical address (if relevant).
  -- Size of segment in file.
  -- Size of segment in memory.
  -- Alignment constraints.
   type Elf64_Phdr is record
      p_type : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2452
      p_flags : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2453
      p_offset : aliased Elf64_Off;  -- /raven/include/elfdefinitions.h:2454
      p_vaddr : aliased Elf64_Addr;  -- /raven/include/elfdefinitions.h:2455
      p_paddr : aliased Elf64_Addr;  -- /raven/include/elfdefinitions.h:2456
      p_filesz : aliased Elf64_Xword;  -- /raven/include/elfdefinitions.h:2457
      p_memsz : aliased Elf64_Xword;  -- /raven/include/elfdefinitions.h:2458
      p_align : aliased Elf64_Xword;  -- /raven/include/elfdefinitions.h:2459
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Phdr);  -- /raven/include/elfdefinitions.h:2460

  -- * Move entries, for describing data in COMMON blocks in a compact
  -- * manner.
  --

  -- 32-bit move entry.
  -- Initialization value.
  -- Encoded size and index.
  -- Offset relative to symbol.
  -- Repeat count.
  -- Number of units to skip.
   type Elf32_Move is record
      m_value : aliased Elf32_Lword;  -- /raven/include/elfdefinitions.h:2470
      m_info : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2471
      m_poffset : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2472
      m_repeat : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2473
      m_stride : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2474
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Move);  -- /raven/include/elfdefinitions.h:2475

  -- 64-bit move entry.
  -- Initialization value.
  -- Encoded size and index.
  -- Offset relative to symbol.
  -- Repeat count.
  -- Number of units to skip.
   type Elf64_Move is record
      m_value : aliased Elf64_Lword;  -- /raven/include/elfdefinitions.h:2479
      m_info : aliased Elf64_Xword;  -- /raven/include/elfdefinitions.h:2480
      m_poffset : aliased Elf64_Xword;  -- /raven/include/elfdefinitions.h:2481
      m_repeat : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2482
      m_stride : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2483
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Move);  -- /raven/include/elfdefinitions.h:2484

  -- * Section Header Table (SHDR) entries.
  --

  -- 32 bit SHDR
  -- index of section name
  -- section type
  -- section flags
  -- in-memory address of section
  -- file offset of section
  -- section size in bytes
  -- section header table link
  -- extra information
  -- alignment constraint
  -- size for fixed-size entries
   type Elf32_Shdr is record
      sh_name : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2500
      sh_type : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2501
      sh_flags : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2502
      sh_addr : aliased Elf32_Addr;  -- /raven/include/elfdefinitions.h:2503
      sh_offset : aliased Elf32_Off;  -- /raven/include/elfdefinitions.h:2504
      sh_size : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2505
      sh_link : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2506
      sh_info : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2507
      sh_addralign : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2508
      sh_entsize : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2509
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Shdr);  -- /raven/include/elfdefinitions.h:2510

  -- 64 bit SHDR
  -- index of section name
  -- section type
  -- section flags
  -- in-memory address of section
  -- file offset of section
  -- section size in bytes
  -- section header table link
  -- extra information
  -- alignment constraint
  -- size for fixed-size entries
   type Elf64_Shdr is record
      sh_name : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2514
      sh_type : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2515
      sh_flags : aliased Elf64_Xword;  -- /raven/include/elfdefinitions.h:2516
      sh_addr : aliased Elf64_Addr;  -- /raven/include/elfdefinitions.h:2517
      sh_offset : aliased Elf64_Off;  -- /raven/include/elfdefinitions.h:2518
      sh_size : aliased Elf64_Xword;  -- /raven/include/elfdefinitions.h:2519
      sh_link : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2520
      sh_info : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2521
      sh_addralign : aliased Elf64_Xword;  -- /raven/include/elfdefinitions.h:2522
      sh_entsize : aliased Elf64_Xword;  -- /raven/include/elfdefinitions.h:2523
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Shdr);  -- /raven/include/elfdefinitions.h:2524

  -- * Symbol table entries.
  --

  -- index of symbol's name
  -- value for the symbol
  -- size of associated data
  -- type and binding attributes
  -- visibility
  -- index of related section
   type Elf32_Sym is record
      st_name : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2532
      st_value : aliased Elf32_Addr;  -- /raven/include/elfdefinitions.h:2533
      st_size : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2534
      st_info : aliased unsigned_char;  -- /raven/include/elfdefinitions.h:2535
      st_other : aliased unsigned_char;  -- /raven/include/elfdefinitions.h:2536
      st_shndx : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2537
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Sym);  -- /raven/include/elfdefinitions.h:2538

  -- index of symbol's name
  -- type and binding attributes
  -- visibility
  -- index of related section
  -- value for the symbol
  -- size of associated data
   type Elf64_Sym is record
      st_name : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2541
      st_info : aliased unsigned_char;  -- /raven/include/elfdefinitions.h:2542
      st_other : aliased unsigned_char;  -- /raven/include/elfdefinitions.h:2543
      st_shndx : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2544
      st_value : aliased Elf64_Addr;  -- /raven/include/elfdefinitions.h:2545
      st_size : aliased Elf64_Xword;  -- /raven/include/elfdefinitions.h:2546
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Sym);  -- /raven/include/elfdefinitions.h:2547

  -- * Syminfo descriptors, containing additional symbol information.
  --

  -- 32-bit entry.
  -- Entry index with additional flags.
  -- Flags.
   type Elf32_Syminfo is record
      si_boundto : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2566
      si_flags : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2567
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Syminfo);  -- /raven/include/elfdefinitions.h:2568

  -- 64-bit entry.
  -- Entry index with additional flags.
  -- Flags.
   type Elf64_Syminfo is record
      si_boundto : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2572
      si_flags : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2573
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Syminfo);  -- /raven/include/elfdefinitions.h:2574

  -- * Relocation descriptors.
  --

  -- location to apply relocation to
  -- type+section for relocation
   type Elf32_Rel is record
      r_offset : aliased Elf32_Addr;  -- /raven/include/elfdefinitions.h:2581
      r_info : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2582
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Rel);  -- /raven/include/elfdefinitions.h:2583

  -- location to apply relocation to
  -- type+section for relocation
  -- constant addend
   type Elf32_Rela is record
      r_offset : aliased Elf32_Addr;  -- /raven/include/elfdefinitions.h:2586
      r_info : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2587
      r_addend : aliased Elf32_Sword;  -- /raven/include/elfdefinitions.h:2588
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Rela);  -- /raven/include/elfdefinitions.h:2589

  -- location to apply relocation to
  -- type+section for relocation
   type Elf64_Rel is record
      r_offset : aliased Elf64_Addr;  -- /raven/include/elfdefinitions.h:2592
      r_info : aliased Elf64_Xword;  -- /raven/include/elfdefinitions.h:2593
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Rel);  -- /raven/include/elfdefinitions.h:2594

  -- location to apply relocation to
  -- type+section for relocation
  -- constant addend
   type Elf64_Rela is record
      r_offset : aliased Elf64_Addr;  -- /raven/include/elfdefinitions.h:2597
      r_info : aliased Elf64_Xword;  -- /raven/include/elfdefinitions.h:2598
      r_addend : aliased Elf64_Sxword;  -- /raven/include/elfdefinitions.h:2599
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Rela);  -- /raven/include/elfdefinitions.h:2600

  -- * Symbol versioning structures.
  --

  -- 32-bit structures.
  -- Index to name.
  -- Offset to next entry.
   type Elf32_Verdaux is record
      vda_name : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2618
      vda_next : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2619
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Verdaux);  -- /raven/include/elfdefinitions.h:2620

  -- Hash value of dependency name.
  -- Flags.
  -- Unused.
  -- Offset to dependency name.
  -- Offset to next vernaux entry.
   type Elf32_Vernaux is record
      vna_hash : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2624
      vna_flags : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2625
      vna_other : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2626
      vna_name : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2627
      vna_next : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2628
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Vernaux);  -- /raven/include/elfdefinitions.h:2629

  -- Version information.
  -- Flags.
  -- Index into the versym section.
  -- Number of aux entries.
  -- Hash value of name.
  -- Offset to aux entries.
  -- Offset to next version definition.
   type Elf32_Verdef is record
      vd_version : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2633
      vd_flags : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2634
      vd_ndx : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2635
      vd_cnt : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2636
      vd_hash : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2637
      vd_aux : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2638
      vd_next : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2639
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Verdef);  -- /raven/include/elfdefinitions.h:2640

  -- Version number.
  -- Number of aux entries.
  -- Offset of associated file name.
  -- Offset of vernaux array.
  -- Offset of next verneed entry.
   type Elf32_Verneed is record
      vn_version : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2644
      vn_cnt : aliased Elf32_Half;  -- /raven/include/elfdefinitions.h:2645
      vn_file : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2646
      vn_aux : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2647
      vn_next : aliased Elf32_Word;  -- /raven/include/elfdefinitions.h:2648
   end record;
   pragma Convention (C_Pass_By_Copy, Elf32_Verneed);  -- /raven/include/elfdefinitions.h:2649

   subtype Elf32_Versym is Elf32_Half;  -- /raven/include/elfdefinitions.h:2651

  -- 64-bit structures.
  -- Index to name.
  -- Offset to next entry.
   type Elf64_Verdaux is record
      vda_name : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2656
      vda_next : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2657
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Verdaux);  -- /raven/include/elfdefinitions.h:2658

  -- Hash value of dependency name.
  -- Flags.
  -- Unused.
  -- Offset to dependency name.
  -- Offset to next vernaux entry.
   type Elf64_Vernaux is record
      vna_hash : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2661
      vna_flags : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2662
      vna_other : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2663
      vna_name : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2664
      vna_next : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2665
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Vernaux);  -- /raven/include/elfdefinitions.h:2666

  -- Version information.
  -- Flags.
  -- Index into the versym section.
  -- Number of aux entries.
  -- Hash value of name.
  -- Offset to aux entries.
  -- Offset to next version definition.
   type Elf64_Verdef is record
      vd_version : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2669
      vd_flags : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2670
      vd_ndx : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2671
      vd_cnt : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2672
      vd_hash : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2673
      vd_aux : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2674
      vd_next : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2675
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Verdef);  -- /raven/include/elfdefinitions.h:2676

  -- Version number.
  -- Number of aux entries.
  -- Offset of associated file name.
  -- Offset of vernaux array.
  -- Offset of next verneed entry.
   type Elf64_Verneed is record
      vn_version : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2679
      vn_cnt : aliased Elf64_Half;  -- /raven/include/elfdefinitions.h:2680
      vn_file : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2681
      vn_aux : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2682
      vn_next : aliased Elf64_Word;  -- /raven/include/elfdefinitions.h:2683
   end record;
   pragma Convention (C_Pass_By_Copy, Elf64_Verneed);  -- /raven/include/elfdefinitions.h:2684

   subtype Elf64_Versym is Elf64_Half;  -- /raven/include/elfdefinitions.h:2686

  -- * The header for GNU-style hash sections.
  --

  -- Number of hash buckets.
  -- First visible symbol in .dynsym.
  -- #maskwords used in bloom filter.
  -- Bloom filter shift count.
   type Elf_GNU_Hash_Header is record
      gh_nbuckets : aliased unsigned;  -- /raven/include/elfdefinitions.h:2694
      gh_symndx : aliased unsigned;  -- /raven/include/elfdefinitions.h:2695
      gh_maskwords : aliased unsigned;  -- /raven/include/elfdefinitions.h:2696
      gh_shift2 : aliased unsigned;  -- /raven/include/elfdefinitions.h:2697
   end record;
   pragma Convention (C_Pass_By_Copy, Elf_GNU_Hash_Header);  -- /raven/include/elfdefinitions.h:2698

end elfdefinitions_h;
