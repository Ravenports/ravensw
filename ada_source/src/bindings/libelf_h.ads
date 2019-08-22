pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;
limited with elfdefinitions_h;

package libelf_h is

   --  unsupported macro: ELF_K_FIRST ELF_K_NONE
   --  unsupported macro: ELF_K_LAST ELF_K_NUM
   --  unsupported macro: ELF_T_FIRST ELF_T_ADDR
   --  unsupported macro: ELF_T_LAST ELF_T_GNUHASH
   --  unsupported macro: ELF_C_FIRST ELF_C_NULL
   --  unsupported macro: ELF_C_LAST ELF_C_NUM
   ELF_F_LAYOUT : constant := 16#001#;  --  /raven/include/libelf.h:172
   ELF_F_DIRTY : constant := 16#002#;  --  /raven/include/libelf.h:173

   ELF_F_ARCHIVE : constant := 16#100#;  --  /raven/include/libelf.h:176
   ELF_F_ARCHIVE_SYSV : constant := 16#200#;  --  /raven/include/libelf.h:177

  ---
  -- * Copyright (c) 2006,2008-2010 Joseph Koshy
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
  -- * $Id: libelf.h 3174 2015-03-27 17:13:41Z emaste $
  --

  -- Library private data structures
   type u_Elf is null record;   -- incomplete struct
   subtype Elf is u_Elf;  -- /raven/include/libelf.h:37
   type u_Elf_Scn is null record;   -- incomplete struct
   subtype Elf_Scn is u_Elf_Scn;  -- /raven/include/libelf.h:38

  -- File types
  -- `ar' archives
  -- COFF files (unsupported)
  -- ELF files
   type Elf_Kind is
     (ELF_K_NONE,
      ELF_K_AR,
      ELF_K_COFF,
      ELF_K_ELF,
      ELF_K_NUM);
   pragma Convention (C, Elf_Kind);  -- /raven/include/libelf.h:47

  -- Data types
  -- GNU style hash tables.
   type Elf_Type is
     (ELF_T_ADDR,
      ELF_T_BYTE,
      ELF_T_CAP,
      ELF_T_DYN,
      ELF_T_EHDR,
      ELF_T_HALF,
      ELF_T_LWORD,
      ELF_T_MOVE,
      ELF_T_MOVEP,
      ELF_T_NOTE,
      ELF_T_OFF,
      ELF_T_PHDR,
      ELF_T_REL,
      ELF_T_RELA,
      ELF_T_SHDR,
      ELF_T_SWORD,
      ELF_T_SXWORD,
      ELF_T_SYMINFO,
      ELF_T_SYM,
      ELF_T_VDEF,
      ELF_T_VNEED,
      ELF_T_WORD,
      ELF_T_XWORD,
      ELF_T_GNUHASH,
      ELF_T_NUM);
   pragma Convention (C, Elf_Type);  -- /raven/include/libelf.h:79

  -- Commands
   type Elf_Cmd is
     (ELF_C_NULL,
      ELF_C_CLR,
      ELF_C_FDDONE,
      ELF_C_FDREAD,
      ELF_C_RDWR,
      ELF_C_READ,
      ELF_C_SET,
      ELF_C_WRITE,
      ELF_C_NUM);
   pragma Convention (C, Elf_Cmd);  -- /raven/include/libelf.h:95

  -- * An `Elf_Data' structure describes data in an ELF section.
  --	 * `Public' members that are part of the ELF(3) API.

   type u_Elf_Data is record
      d_align : aliased unsigned_long;  -- /raven/include/libelf.h:108
      d_buf : Interfaces.C.Strings.chars_ptr;  -- /raven/include/libelf.h:109
      d_off : aliased unsigned_long;  -- /raven/include/libelf.h:110
      d_size : aliased unsigned_long;  -- /raven/include/libelf.h:111
      d_type : aliased Elf_Type;  -- /raven/include/libelf.h:112
      d_version : aliased unsigned;  -- /raven/include/libelf.h:113
   end record;
   pragma Convention (C_Pass_By_Copy, u_Elf_Data);  -- /raven/include/libelf.h:104

   subtype Elf_Data is u_Elf_Data;  -- /raven/include/libelf.h:114

  -- * An `Elf_Arhdr' structure describes an archive header.
  -- archive member name
  -- 'raw' member name
  --	 * Members that are not part of the public API.

   type Elf_Arhdr is record
      ar_date : aliased long;  -- /raven/include/libelf.h:121
      ar_name : Interfaces.C.Strings.chars_ptr;  -- /raven/include/libelf.h:122
      ar_gid : aliased long;  -- /raven/include/libelf.h:123
      ar_mode : aliased unsigned_short;  -- /raven/include/libelf.h:124
      ar_rawname : Interfaces.C.Strings.chars_ptr;  -- /raven/include/libelf.h:125
      ar_size : aliased unsigned_long;  -- /raven/include/libelf.h:126
      ar_uid : aliased unsigned;  -- /raven/include/libelf.h:127
      ar_flags : aliased unsigned;  -- /raven/include/libelf.h:132
   end record;
   pragma Convention (C_Pass_By_Copy, Elf_Arhdr);  -- /raven/include/libelf.h:133

  -- * An `Elf_Arsym' describes an entry in the archive symbol table.

  -- byte offset to member's header
  -- elf_hash() value for name
  -- null terminated symbol name
   type Elf_Arsym is record
      as_off : aliased long;  -- /raven/include/libelf.h:140
      as_hash : aliased unsigned_long;  -- /raven/include/libelf.h:141
      as_name : Interfaces.C.Strings.chars_ptr;  -- /raven/include/libelf.h:142
   end record;
   pragma Convention (C_Pass_By_Copy, Elf_Arsym);  -- /raven/include/libelf.h:143

  -- * Error numbers.

   type Elf_Error is
     (ELF_E_NONE,
      ELF_E_ARCHIVE,
      ELF_E_ARGUMENT,
      ELF_E_CLASS,
      ELF_E_DATA,
      ELF_E_HEADER,
      ELF_E_IO,
      ELF_E_LAYOUT,
      ELF_E_MODE,
      ELF_E_RANGE,
      ELF_E_RESOURCE,
      ELF_E_SECTION,
      ELF_E_SEQUENCE,
      ELF_E_UNIMPL,
      ELF_E_VERSION,
      ELF_E_NUM);
   pragma Convention (C, Elf_Error);  -- /raven/include/libelf.h:149

  -- No error
  -- Malformed ar(1) archive
  -- Invalid argument
  -- Mismatched ELF class
  -- Invalid data descriptor
  -- Missing or malformed ELF header
  -- I/O error
  -- Layout constraint violation
  -- Wrong mode for ELF descriptor
  -- Value out of range
  -- Resource exhaustion
  -- Invalid section descriptor
  -- API calls out of sequence
  -- Feature is unimplemented
  -- Unknown API version
  -- Max error number
  -- * Flags defined by the API.
  --

  -- ELF(3) API extensions.
   function elf_begin
     (arg1 : int;
      arg2 : Elf_Cmd;
      arg3 : access Elf) return access Elf;  -- /raven/include/libelf.h:182
   pragma Import (C, elf_begin, "elf_begin");

   function elf_cntl (u_elf : access Elf; u_cmd : Elf_Cmd) return int;  -- /raven/include/libelf.h:183
   pragma Import (C, elf_cntl, "elf_cntl");

   function elf_end (u_elf : access Elf) return int;  -- /raven/include/libelf.h:184
   pragma Import (C, elf_end, "elf_end");

   function elf_errmsg (arg1 : int) return Interfaces.C.Strings.chars_ptr;  -- /raven/include/libelf.h:185
   pragma Import (C, elf_errmsg, "elf_errmsg");

   function elf_errno return int;  -- /raven/include/libelf.h:186
   pragma Import (C, elf_errno, "elf_errno");

   procedure elf_fill (u_fill : int);  -- /raven/include/libelf.h:187
   pragma Import (C, elf_fill, "elf_fill");

   function elf_flagarhdr
     (u_arh : access Elf_Arhdr;
      u_cmd : Elf_Cmd;
      u_flags : unsigned) return unsigned;  -- /raven/include/libelf.h:188
   pragma Import (C, elf_flagarhdr, "elf_flagarhdr");

   function elf_flagdata
     (u_data : access Elf_Data;
      u_cmd : Elf_Cmd;
      u_flags : unsigned) return unsigned;  -- /raven/include/libelf.h:190
   pragma Import (C, elf_flagdata, "elf_flagdata");

   function elf_flagehdr
     (u_elf : access Elf;
      u_cmd : Elf_Cmd;
      u_flags : unsigned) return unsigned;  -- /raven/include/libelf.h:192
   pragma Import (C, elf_flagehdr, "elf_flagehdr");

   function elf_flagelf
     (u_elf : access Elf;
      u_cmd : Elf_Cmd;
      u_flags : unsigned) return unsigned;  -- /raven/include/libelf.h:193
   pragma Import (C, elf_flagelf, "elf_flagelf");

   function elf_flagphdr
     (u_elf : access Elf;
      u_cmd : Elf_Cmd;
      u_flags : unsigned) return unsigned;  -- /raven/include/libelf.h:194
   pragma Import (C, elf_flagphdr, "elf_flagphdr");

   function elf_flagscn
     (u_scn : access Elf_Scn;
      u_cmd : Elf_Cmd;
      u_flags : unsigned) return unsigned;  -- /raven/include/libelf.h:195
   pragma Import (C, elf_flagscn, "elf_flagscn");

   function elf_flagshdr
     (u_scn : access Elf_Scn;
      u_cmd : Elf_Cmd;
      u_flags : unsigned) return unsigned;  -- /raven/include/libelf.h:196
   pragma Import (C, elf_flagshdr, "elf_flagshdr");

   function elf_getarhdr (arg1 : access Elf) return access Elf_Arhdr;  -- /raven/include/libelf.h:197
   pragma Import (C, elf_getarhdr, "elf_getarhdr");

   function elf_getarsym (arg1 : access Elf; arg2 : access unsigned_long) return access Elf_Arsym;  -- /raven/include/libelf.h:198
   pragma Import (C, elf_getarsym, "elf_getarsym");

   function elf_getbase (u_elf : access Elf) return long;  -- /raven/include/libelf.h:199
   pragma Import (C, elf_getbase, "elf_getbase");

   function elf_getdata (arg1 : access Elf_Scn; arg2 : access Elf_Data) return access Elf_Data;  -- /raven/include/libelf.h:200
   pragma Import (C, elf_getdata, "elf_getdata");

   function elf_getident (arg1 : access Elf; arg2 : access unsigned_long) return Interfaces.C.Strings.chars_ptr;  -- /raven/include/libelf.h:201
   pragma Import (C, elf_getident, "elf_getident");

   function elf_getphdrnum (u_elf : access Elf; u_dst : access unsigned_long) return int;  -- /raven/include/libelf.h:202
   pragma Import (C, elf_getphdrnum, "elf_getphdrnum");

  -- Deprecated
   function elf_getphnum (u_elf : access Elf; u_dst : access unsigned_long) return int;  -- /raven/include/libelf.h:203
   pragma Import (C, elf_getphnum, "elf_getphnum");

   function elf_getscn (arg1 : access Elf; arg2 : unsigned_long) return access Elf_Scn;  -- /raven/include/libelf.h:204
   pragma Import (C, elf_getscn, "elf_getscn");

   function elf_getshdrnum (u_elf : access Elf; u_dst : access unsigned_long) return int;  -- /raven/include/libelf.h:205
   pragma Import (C, elf_getshdrnum, "elf_getshdrnum");

  -- Deprecated
   function elf_getshnum (u_elf : access Elf; u_dst : access unsigned_long) return int;  -- /raven/include/libelf.h:206
   pragma Import (C, elf_getshnum, "elf_getshnum");

   function elf_getshdrstrndx (u_elf : access Elf; u_dst : access unsigned_long) return int;  -- /raven/include/libelf.h:207
   pragma Import (C, elf_getshdrstrndx, "elf_getshdrstrndx");

  -- Deprecated
   function elf_getshstrndx (u_elf : access Elf; u_dst : access unsigned_long) return int;  -- /raven/include/libelf.h:208
   pragma Import (C, elf_getshstrndx, "elf_getshstrndx");

   function elf_hash (u_name : Interfaces.C.Strings.chars_ptr) return unsigned_long;  -- /raven/include/libelf.h:209
   pragma Import (C, elf_hash, "elf_hash");

   function F_elf_kind (u_elf : access Elf) return Elf_Kind;  -- /raven/include/libelf.h:210
   pragma Import (C, F_elf_kind, "elf_kind");

   function elf_memory (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : unsigned_long) return access Elf;  -- /raven/include/libelf.h:211
   pragma Import (C, elf_memory, "elf_memory");

   function elf_ndxscn (u_scn : access Elf_Scn) return unsigned_long;  -- /raven/include/libelf.h:212
   pragma Import (C, elf_ndxscn, "elf_ndxscn");

   function elf_newdata (arg1 : access Elf_Scn) return access Elf_Data;  -- /raven/include/libelf.h:213
   pragma Import (C, elf_newdata, "elf_newdata");

   function elf_newscn (arg1 : access Elf) return access Elf_Scn;  -- /raven/include/libelf.h:214
   pragma Import (C, elf_newscn, "elf_newscn");

   function elf_nextscn (arg1 : access Elf; arg2 : access Elf_Scn) return access Elf_Scn;  -- /raven/include/libelf.h:215
   pragma Import (C, elf_nextscn, "elf_nextscn");

   function elf_next (u_elf : access Elf) return Elf_Cmd;  -- /raven/include/libelf.h:216
   pragma Import (C, elf_next, "elf_next");

   function elf_open (arg1 : int) return access Elf;  -- /raven/include/libelf.h:217
   pragma Import (C, elf_open, "elf_open");

   function elf_openmemory (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : unsigned_long) return access Elf;  -- /raven/include/libelf.h:218
   pragma Import (C, elf_openmemory, "elf_openmemory");

   function elf_rand (u_elf : access Elf; u_off : long) return long;  -- /raven/include/libelf.h:219
   pragma Import (C, elf_rand, "elf_rand");

   function elf_rawdata (arg1 : access Elf_Scn; arg2 : access Elf_Data) return access Elf_Data;  -- /raven/include/libelf.h:220
   pragma Import (C, elf_rawdata, "elf_rawdata");

   function elf_rawfile (arg1 : access Elf; arg2 : access unsigned_long) return Interfaces.C.Strings.chars_ptr;  -- /raven/include/libelf.h:221
   pragma Import (C, elf_rawfile, "elf_rawfile");

   function elf_setshstrndx (u_elf : access Elf; u_shnum : unsigned_long) return int;  -- /raven/include/libelf.h:222
   pragma Import (C, elf_setshstrndx, "elf_setshstrndx");

   function elf_strptr
     (arg1 : access Elf;
      arg2 : unsigned_long;
      arg3 : unsigned_long) return Interfaces.C.Strings.chars_ptr;  -- /raven/include/libelf.h:223
   pragma Import (C, elf_strptr, "elf_strptr");

   function elf_update (u_elf : access Elf; u_cmd : Elf_Cmd) return long;  -- /raven/include/libelf.h:224
   pragma Import (C, elf_update, "elf_update");

   function elf_version (u_version : unsigned) return unsigned;  -- /raven/include/libelf.h:225
   pragma Import (C, elf_version, "elf_version");

   function elf32_checksum (u_elf : access Elf) return long;  -- /raven/include/libelf.h:227
   pragma Import (C, elf32_checksum, "elf32_checksum");

   function elf32_fsize
     (u_type : Elf_Type;
      u_count : unsigned_long;
      u_version : unsigned) return unsigned_long;  -- /raven/include/libelf.h:228
   pragma Import (C, elf32_fsize, "elf32_fsize");

   function elf32_getehdr (arg1 : access Elf) return access elfdefinitions_h.Elf32_Ehdr;  -- /raven/include/libelf.h:230
   pragma Import (C, elf32_getehdr, "elf32_getehdr");

   function elf32_getphdr (arg1 : access Elf) return access elfdefinitions_h.Elf32_Phdr;  -- /raven/include/libelf.h:231
   pragma Import (C, elf32_getphdr, "elf32_getphdr");

   function elf32_getshdr (arg1 : access Elf_Scn) return access elfdefinitions_h.Elf32_Shdr;  -- /raven/include/libelf.h:232
   pragma Import (C, elf32_getshdr, "elf32_getshdr");

   function elf32_newehdr (arg1 : access Elf) return access elfdefinitions_h.Elf32_Ehdr;  -- /raven/include/libelf.h:233
   pragma Import (C, elf32_newehdr, "elf32_newehdr");

   function elf32_newphdr (arg1 : access Elf; arg2 : unsigned_long) return access elfdefinitions_h.Elf32_Phdr;  -- /raven/include/libelf.h:234
   pragma Import (C, elf32_newphdr, "elf32_newphdr");

   function elf32_xlatetof
     (arg1 : access Elf_Data;
      arg2 : access constant Elf_Data;
      arg3 : unsigned) return access Elf_Data;  -- /raven/include/libelf.h:235
   pragma Import (C, elf32_xlatetof, "elf32_xlatetof");

   function elf32_xlatetom
     (arg1 : access Elf_Data;
      arg2 : access constant Elf_Data;
      arg3 : unsigned) return access Elf_Data;  -- /raven/include/libelf.h:237
   pragma Import (C, elf32_xlatetom, "elf32_xlatetom");

   function elf64_checksum (u_elf : access Elf) return long;  -- /raven/include/libelf.h:240
   pragma Import (C, elf64_checksum, "elf64_checksum");

   function elf64_fsize
     (u_type : Elf_Type;
      u_count : unsigned_long;
      u_version : unsigned) return unsigned_long;  -- /raven/include/libelf.h:241
   pragma Import (C, elf64_fsize, "elf64_fsize");

   function elf64_getehdr (arg1 : access Elf) return access elfdefinitions_h.Elf64_Ehdr;  -- /raven/include/libelf.h:243
   pragma Import (C, elf64_getehdr, "elf64_getehdr");

   function elf64_getphdr (arg1 : access Elf) return access elfdefinitions_h.Elf64_Phdr;  -- /raven/include/libelf.h:244
   pragma Import (C, elf64_getphdr, "elf64_getphdr");

   function elf64_getshdr (arg1 : access Elf_Scn) return access elfdefinitions_h.Elf64_Shdr;  -- /raven/include/libelf.h:245
   pragma Import (C, elf64_getshdr, "elf64_getshdr");

   function elf64_newehdr (arg1 : access Elf) return access elfdefinitions_h.Elf64_Ehdr;  -- /raven/include/libelf.h:246
   pragma Import (C, elf64_newehdr, "elf64_newehdr");

   function elf64_newphdr (arg1 : access Elf; arg2 : unsigned_long) return access elfdefinitions_h.Elf64_Phdr;  -- /raven/include/libelf.h:247
   pragma Import (C, elf64_newphdr, "elf64_newphdr");

   function elf64_xlatetof
     (arg1 : access Elf_Data;
      arg2 : access constant Elf_Data;
      arg3 : unsigned) return access Elf_Data;  -- /raven/include/libelf.h:248
   pragma Import (C, elf64_xlatetof, "elf64_xlatetof");

   function elf64_xlatetom
     (arg1 : access Elf_Data;
      arg2 : access constant Elf_Data;
      arg3 : unsigned) return access Elf_Data;  -- /raven/include/libelf.h:250
   pragma Import (C, elf64_xlatetom, "elf64_xlatetom");

end libelf_h;
