--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Interfaces.C.Strings;
with Interfaces.C.Extensions;
with elfdefinitions_h;
--  with gelf_h;

package body Libelf is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;
   package ICX renames Interfaces.C.Extensions;
   package DEF renames elfdefinitions_h;

   --------------------------------------------------------------------
   --  initialize_libelf
   --------------------------------------------------------------------
   function initialize_libelf return Boolean
   is
      use type IC.unsigned;
   begin
      return (libelf_h.elf_version (DEF.EV_CURRENT) /= DEF.EV_NONE);
   end initialize_libelf;


   --------------------------------------------------------------------
   --  elf_errmsg
   --------------------------------------------------------------------
   function elf_errmsg return String
   is
      result : ICS.chars_ptr;
   begin
      result := libelf_h.elf_errmsg (IC.int (-1));
      --  Don't free result
      return ICS.Value (result);
   end elf_errmsg;


   --------------------------------------------------------------------
   --  elf_begin_read
   --------------------------------------------------------------------
   function elf_begin_read (fd : Core.Unix.File_Descriptor) return access libelf_h.Elf
   is
   begin
      return libelf_h.elf_begin (IC.int (fd), libelf_h.ELF_C_READ, null);
   end elf_begin_read;


   --------------------------------------------------------------------
   --  elf_object_is_null
   --------------------------------------------------------------------
   function elf_object_is_null (elf_obj : access libelf_h.Elf) return Boolean
   is
      use type libelf_h.Elf;
   begin
      return (elf_obj = null);
   end elf_object_is_null;


   --------------------------------------------------------------------
   --  elf_end
   --------------------------------------------------------------------
   procedure elf_end (elf_obj : access libelf_h.Elf)
   is
      result : IC.int;
   begin
      if elf_obj /= null then
         result := libelf_h.elf_end (elf_obj);
      end if;
   end elf_end;


   --------------------------------------------------------------------
   --  get_elf_header
   --------------------------------------------------------------------
   function get_elf_header (elf_obj : access libelf_h.Elf;
                            header  : access gelf_h.GElf_Ehdr) return Boolean
   is
      result : access gelf_h.GElf_Ehdr;
   begin
      result := gelf_h.gelf_getehdr (elf_obj, header);
      return (result /= null);
   end get_elf_header;


   --------------------------------------------------------------------
   --  elf_next_section
   --------------------------------------------------------------------
   function elf_next_section (elf_obj : access libelf_h.Elf;
                              section : access libelf_h.Elf_Scn) return Boolean
   is
      result : access libelf_h.Elf_Scn;
   begin
      result := libelf_h.elf_nextscn (elf_obj, section);
      return (result /= null);
   end elf_next_section;


   --------------------------------------------------------------------
   --  elf_get_section_header
   --------------------------------------------------------------------
   function elf_get_section_header (section : access libelf_h.Elf_Scn;
                                    sheader : access gelf_h.GElf_Shdr) return Boolean
   is
      result : access gelf_h.GElf_Shdr;
   begin
      result := gelf_h.gelf_getshdr (section, sheader);
      return (result /= sheader);
   end elf_get_section_header;


   --------------------------------------------------------------------
   --  elf_get_section_header
   --------------------------------------------------------------------
   function section_header_is_elf_note (section : gelf_h.GElf_Shdr) return Boolean
   is
      --  should have been in elfdefinitions.h
      SHT_NOTE : constant elfdefinitions_h.Elf64_Word := elfdefinitions_h.Elf64_Word (7);

      use type elfdefinitions_h.Elf64_Word;
   begin
      return section.sh_type = SHT_NOTE;
   end section_header_is_elf_note;


   --------------------------------------------------------------------
   --  elf_getdata
   --------------------------------------------------------------------
   function elf_getdata (section : access libelf_h.Elf_Scn) return access libelf_h.Elf_Data is
   begin
      return libelf_h.elf_getdata (section, null);
   end elf_getdata;


   --------------------------------------------------------------------
   --  get_ident_byte
   --------------------------------------------------------------------
   function get_ident_byte (header : access gelf_h.GElf_Ehdr; offset : EI_OFFSETS) return EI_Byte
   is
      index : constant Natural := Natural (EI_OFFSETS'Pos (offset));
   begin
      return EI_Byte (header.e_ident (header.e_ident'First + index));
   end get_ident_byte;


end Libelf;
