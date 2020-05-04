--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../../License.txt

with Core.Unix;
with libelf_h;
with gelf_h;

package Libelf is

   type EI_OFFSETS is (EI_MAG0, EI_MAG1, EI_MAG2, EI_MAG3, EI_CLASS, EI_DATA, EI_VERSION,
                       EI_OSABI, EI_ABIVERSION, EI_PAD);
   subtype EI_Byte is Natural range 0 .. 255;

   --  Return true if elf library successfully initialized
   function initialize_libelf return Boolean;

   --  Returns error message from libelf
   function elf_errmsg return String;

   --  Return Elf object for reading
   function elf_begin_read (fd : Core.Unix.File_Descriptor) return access libelf_h.Elf;

   --  If elf_object is not null, close it
   procedure elf_end (elf_obj : access libelf_h.Elf);

   --  Return True if elf object is empty
   function elf_object_is_null (elf_obj : access libelf_h.Elf) return Boolean;

   --  Populates header argument from elf_object.  Returns False if problem occurred
   function get_elf_header (elf_obj : access libelf_h.Elf;
                            header  : access gelf_h.GElf_Ehdr) return Boolean;

   --  Advances internal pointer to next section and populate scn variable.
   --  Returns False if at end of header
   function elf_next_section (elf_obj : access libelf_h.Elf;
                              section : access libelf_h.Elf_Scn) return access libelf_h.Elf_Scn;

   --  Advances to section's header.  Returns False if problem occurred
   function elf_get_section_header (section : access libelf_h.Elf_Scn;
                                    sheader : access gelf_h.GElf_Shdr) return Boolean;

   --  Return true if given section header defines an elf note.
   function section_header_is_elf_note (section : access gelf_h.GElf_Shdr) return Boolean;

   --  Return data from given section
   function elf_getdata (section : access libelf_h.Elf_Scn) return access libelf_h.Elf_Data;

   --  Return byte indicated at offset of the e_ident field
   function get_ident_byte (header : gelf_h.GElf_Ehdr; offset : EI_OFFSETS) return EI_Byte;

   --  Convert the elf data buffer into a full string (ignore null terminators)
   function convert_elf_data_buffer (data : access libelf_h.Elf_Data) return String;

   --  Return size of elf note to help unchecked conversion
   function elf_note_size return Natural;

end Libelf;
