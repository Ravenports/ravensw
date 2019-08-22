pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with elfdefinitions_h;
with libelf_h;
with System;

package gelf_h is

   --  unsupported macro: GELF_M_INFO ELF64_M_INFO
   --  unsupported macro: GELF_M_SIZE ELF64_M_SIZE
   --  unsupported macro: GELF_M_SYM ELF64_M_SYM
   --  unsupported macro: GELF_R_INFO ELF64_R_INFO
   --  unsupported macro: GELF_R_SYM ELF64_R_SYM
   --  unsupported macro: GELF_R_TYPE ELF64_R_TYPE
   --  unsupported macro: GELF_R_TYPE_DATA ELF64_R_TYPE_DATA
   --  unsupported macro: GELF_R_TYPE_ID ELF64_R_TYPE_ID
   --  unsupported macro: GELF_R_TYPE_INFO ELF64_R_TYPE_INFO
   --  unsupported macro: GELF_ST_BIND ELF64_ST_BIND
   --  unsupported macro: GELF_ST_INFO ELF64_ST_INFO
   --  unsupported macro: GELF_ST_TYPE ELF64_ST_TYPE
   --  unsupported macro: GELF_ST_VISIBILITY ELF64_ST_VISIBILITY
  ---
  -- * Copyright (c) 2006,2008 Joseph Koshy
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
  -- * $Id$
  --

  -- Addresses
   subtype GElf_Addr is elfdefinitions_h.Elf64_Addr;  -- gelf.h:34

  -- Half words (16 bit)
   subtype GElf_Half is elfdefinitions_h.Elf64_Half;  -- gelf.h:35

  -- Offsets
   subtype GElf_Off is elfdefinitions_h.Elf64_Off;  -- gelf.h:36

  -- Signed words (32 bit)
   subtype GElf_Sword is elfdefinitions_h.Elf64_Sword;  -- gelf.h:37

  -- Signed long words (64 bit)
   subtype GElf_Sxword is elfdefinitions_h.Elf64_Sxword;  -- gelf.h:38

  -- Unsigned words (32 bit)
   subtype GElf_Word is elfdefinitions_h.Elf64_Word;  -- gelf.h:39

  -- Unsigned long words (64 bit)
   subtype GElf_Xword is elfdefinitions_h.Elf64_Xword;  -- gelf.h:40

  -- ".dynamic" section entries
   subtype GElf_Dyn is elfdefinitions_h.Elf64_Dyn;  -- gelf.h:42

  -- ELF header
   subtype GElf_Ehdr is elfdefinitions_h.Elf64_Ehdr;  -- gelf.h:43

  -- Program header
   subtype GElf_Phdr is elfdefinitions_h.Elf64_Phdr;  -- gelf.h:44

  -- Section header
   subtype GElf_Shdr is elfdefinitions_h.Elf64_Shdr;  -- gelf.h:45

  -- Symbol table entries
   subtype GElf_Sym is elfdefinitions_h.Elf64_Sym;  -- gelf.h:46

  -- Relocation entries
   subtype GElf_Rel is elfdefinitions_h.Elf64_Rel;  -- gelf.h:47

  -- Relocation entries with addend
   subtype GElf_Rela is elfdefinitions_h.Elf64_Rela;  -- gelf.h:48

  -- SW/HW capabilities
   subtype GElf_Cap is elfdefinitions_h.Elf64_Cap;  -- gelf.h:50

  -- Move entries
   subtype GElf_Move is elfdefinitions_h.Elf64_Move;  -- gelf.h:51

  -- Symbol information
   subtype GElf_Syminfo is elfdefinitions_h.Elf64_Syminfo;  -- gelf.h:52

   function gelf_checksum (u_elf : access libelf_h.Elf) return long;  -- gelf.h:73
   pragma Import (C, gelf_checksum, "gelf_checksum");

   function gelf_fsize
     (u_elf : access libelf_h.Elf;
      u_type : libelf_h.Elf_Type;
      u_count : unsigned_long;
      u_version : unsigned) return unsigned_long;  -- gelf.h:74
   pragma Import (C, gelf_fsize, "gelf_fsize");

   function gelf_getclass (u_elf : access libelf_h.Elf) return int;  -- gelf.h:76
   pragma Import (C, gelf_getclass, "gelf_getclass");

   function gelf_getdyn
     (arg1 : access libelf_h.Elf_Data;
      arg2 : int;
      arg3 : access GElf_Dyn) return access GElf_Dyn;  -- gelf.h:77
   pragma Import (C, gelf_getdyn, "gelf_getdyn");

   function gelf_getehdr (arg1 : access libelf_h.Elf; arg2 : access GElf_Ehdr) return access GElf_Ehdr;  -- gelf.h:78
   pragma Import (C, gelf_getehdr, "gelf_getehdr");

   function gelf_getphdr
     (arg1 : access libelf_h.Elf;
      arg2 : int;
      arg3 : access GElf_Phdr) return access GElf_Phdr;  -- gelf.h:79
   pragma Import (C, gelf_getphdr, "gelf_getphdr");

   function gelf_getrel
     (arg1 : access libelf_h.Elf_Data;
      arg2 : int;
      arg3 : access GElf_Rel) return access GElf_Rel;  -- gelf.h:80
   pragma Import (C, gelf_getrel, "gelf_getrel");

   function gelf_getrela
     (arg1 : access libelf_h.Elf_Data;
      arg2 : int;
      arg3 : access GElf_Rela) return access GElf_Rela;  -- gelf.h:81
   pragma Import (C, gelf_getrela, "gelf_getrela");

   function gelf_getshdr (arg1 : access libelf_h.Elf_Scn; arg2 : access GElf_Shdr) return access GElf_Shdr;  -- gelf.h:82
   pragma Import (C, gelf_getshdr, "gelf_getshdr");

   function gelf_getsym
     (arg1 : access libelf_h.Elf_Data;
      arg2 : int;
      arg3 : access GElf_Sym) return access GElf_Sym;  -- gelf.h:83
   pragma Import (C, gelf_getsym, "gelf_getsym");

   function gelf_getsymshndx
     (arg1 : access libelf_h.Elf_Data;
      arg2 : access libelf_h.Elf_Data;
      arg3 : int;
      arg4 : access GElf_Sym;
      arg5 : access elfdefinitions_h.Elf32_Word) return access GElf_Sym;  -- gelf.h:84
   pragma Import (C, gelf_getsymshndx, "gelf_getsymshndx");

   function gelf_newehdr (arg1 : access libelf_h.Elf; arg2 : int) return System.Address;  -- gelf.h:86
   pragma Import (C, gelf_newehdr, "gelf_newehdr");

   function gelf_newphdr (arg1 : access libelf_h.Elf; arg2 : unsigned_long) return System.Address;  -- gelf.h:87
   pragma Import (C, gelf_newphdr, "gelf_newphdr");

   function gelf_update_dyn
     (u_dst : access libelf_h.Elf_Data;
      u_index : int;
      u_src : access GElf_Dyn) return int;  -- gelf.h:88
   pragma Import (C, gelf_update_dyn, "gelf_update_dyn");

   function gelf_update_ehdr (u_elf : access libelf_h.Elf; u_src : access GElf_Ehdr) return int;  -- gelf.h:89
   pragma Import (C, gelf_update_ehdr, "gelf_update_ehdr");

   function gelf_update_phdr
     (u_elf : access libelf_h.Elf;
      u_index : int;
      u_src : access GElf_Phdr) return int;  -- gelf.h:90
   pragma Import (C, gelf_update_phdr, "gelf_update_phdr");

   function gelf_update_rel
     (u_dst : access libelf_h.Elf_Data;
      u_index : int;
      u_src : access GElf_Rel) return int;  -- gelf.h:91
   pragma Import (C, gelf_update_rel, "gelf_update_rel");

   function gelf_update_rela
     (u_dst : access libelf_h.Elf_Data;
      u_index : int;
      u_src : access GElf_Rela) return int;  -- gelf.h:92
   pragma Import (C, gelf_update_rela, "gelf_update_rela");

   function gelf_update_shdr (u_dst : access libelf_h.Elf_Scn; u_src : access GElf_Shdr) return int;  -- gelf.h:93
   pragma Import (C, gelf_update_shdr, "gelf_update_shdr");

   function gelf_update_sym
     (u_dst : access libelf_h.Elf_Data;
      u_index : int;
      u_src : access GElf_Sym) return int;  -- gelf.h:94
   pragma Import (C, gelf_update_sym, "gelf_update_sym");

   function gelf_update_symshndx
     (u_symdst : access libelf_h.Elf_Data;
      u_shindexdst : access libelf_h.Elf_Data;
      u_index : int;
      u_symsrc : access GElf_Sym;
      u_shindexsrc : elfdefinitions_h.Elf32_Word) return int;  -- gelf.h:95
   pragma Import (C, gelf_update_symshndx, "gelf_update_symshndx");

   function gelf_xlatetof
     (arg1 : access libelf_h.Elf;
      arg2 : access libelf_h.Elf_Data;
      arg3 : access constant libelf_h.Elf_Data;
      arg4 : unsigned) return access libelf_h.Elf_Data;  -- gelf.h:97
   pragma Import (C, gelf_xlatetof, "gelf_xlatetof");

   function gelf_xlatetom
     (arg1 : access libelf_h.Elf;
      arg2 : access libelf_h.Elf_Data;
      arg3 : access constant libelf_h.Elf_Data;
      arg4 : unsigned) return access libelf_h.Elf_Data;  -- gelf.h:98
   pragma Import (C, gelf_xlatetom, "gelf_xlatetom");

   function gelf_getcap
     (arg1 : access libelf_h.Elf_Data;
      arg2 : int;
      arg3 : access GElf_Cap) return access GElf_Cap;  -- gelf.h:100
   pragma Import (C, gelf_getcap, "gelf_getcap");

   function gelf_getmove
     (arg1 : access libelf_h.Elf_Data;
      arg2 : int;
      arg3 : access GElf_Move) return access GElf_Move;  -- gelf.h:101
   pragma Import (C, gelf_getmove, "gelf_getmove");

   function gelf_getsyminfo
     (arg1 : access libelf_h.Elf_Data;
      arg2 : int;
      arg3 : access GElf_Syminfo) return access GElf_Syminfo;  -- gelf.h:102
   pragma Import (C, gelf_getsyminfo, "gelf_getsyminfo");

   function gelf_update_cap
     (u_dst : access libelf_h.Elf_Data;
      u_index : int;
      u_src : access GElf_Cap) return int;  -- gelf.h:103
   pragma Import (C, gelf_update_cap, "gelf_update_cap");

   function gelf_update_move
     (u_dst : access libelf_h.Elf_Data;
      u_index : int;
      u_src : access GElf_Move) return int;  -- gelf.h:104
   pragma Import (C, gelf_update_move, "gelf_update_move");

   function gelf_update_syminfo
     (u_dst : access libelf_h.Elf_Data;
      u_index : int;
      u_src : access GElf_Syminfo) return int;  -- gelf.h:105
   pragma Import (C, gelf_update_syminfo, "gelf_update_syminfo");

end gelf_h;
