--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Unchecked_Conversion;

with Core.Unix;
with Core.Event;
with Core.Strings; use Core.Strings;

with elfdefinitions_h;

package body Core.Elf_Operations is

   package EV  renames Core.Event;
   package DIR renames Ada.Directories;
   package ENV renames Ada.Environment_Variables;

   --------------------------------------------------------------------
   --  calculate_abi
   --------------------------------------------------------------------
   function calculate_abi return abi_result
   is
      result : abi_result;
   begin
      result.error := RESULT_OK;
      case platform is
         when solaris =>
            result.abi := SUS ("solaris:10:amd64");
            return result;
         when macos =>
            result.abi := SUS (triplet_1 & ":tbd:x86_64");
            return result;
         when omnios =>
            result.abi := SUS (triplet_1 & ":tbd:tbd");
            return result;
         when others =>
            declare
               elf_properties : T_parse_result := parse_elf_for_arch (triplet_1);
            begin
               result.error := elf_properties.error;
               if result.error = RESULT_OK then
                  result.abi := SUS (triplet_1 & ":" &
                                       USS (elf_properties.release) & ":" &
                                       triplet_3 (elf_properties));
               end if;
            end;
            return result;
      end case;
   end calculate_abi;


   --------------------------------------------------------------------
   --  parse_elf_for_arch
   --------------------------------------------------------------------
   function parse_elf_for_arch (expected_osname : String) return T_parse_result
   is
      result      : T_parse_result;
      fd          : Unix.File_Descriptor := Unix.not_connected;
      elf_obj     : access libelf_h.Elf;
      elf_header  : aliased gelf_h.GElf_Ehdr;
      elf_section : access libelf_h.Elf_Scn := null;
      info        : T_elf_info;
      clean_now   : Boolean := False;
      success     : Boolean;
   begin
      if not Libelf.initialize_libelf then
         EV.emit_error ("ELF library initialization failed: " & Libelf.elf_errmsg);
         result.error := RESULT_FATAL;
         return result;
      end if;

      declare
         readonly_flags : constant Unix.T_Open_Flags := (RDONLY => True, others => False);
      begin
         declare
            abi_file : String := ENV.Value ("ABI_FILE");
         begin
            --  If ABI_FILE set, then limit ABI checks to it.
            if DIR.Exists (abi_file) then
               fd := Unix.open_file (abi_file, readonly_flags);
            end if;
         end;
      exception
         when Constraint_Error => --  No env override, check standard flags
            if platform = solaris then
               fd := Unix.open_file ("/lib/64/libc.so.1", readonly_flags);
            else
               fd := Unix.open_file ("/usr/bin/uname", readonly_flags);
               if not Unix.file_connected (fd) then
                  fd := Unix.open_file ("/bin/sh", readonly_flags);
               end if;
            end if;
      end;

      if not Unix.file_connected (fd) then
         EV.emit_error ("Unable to determine the ABI due to missing reference file");
         result.error := RESULT_FATAL;
         return result;
      end if;

      elf_obj := Libelf.elf_begin_read (fd);
      if Libelf.elf_object_is_null (elf_obj) then
         EV.emit_error ("elfparse/elf_begin() failed: " & Libelf.elf_errmsg);
         result.error := RESULT_FATAL;
         clean_now := True;
      end if;

      if not clean_now then
         if not Libelf.get_elf_header (elf_obj, elf_header'Access) then
            EV.emit_error ("elfparse/getehdr() failed: " & Libelf.elf_errmsg);
            result.error := RESULT_FATAL;
            clean_now := True;
         end if;
      end if;

      declare
         section_header : aliased gelf_h.GElf_Shdr;
      begin
         if not clean_now then
            loop
               elf_section := Libelf.elf_next_section (elf_obj, elf_section);
               exit when elf_section = null;

               if not Libelf.elf_get_section_header (section => elf_section,
                                                     sheader => section_header'Access)
               then
                  EV.emit_error ("elfparse/getshdr() failed: " & Libelf.elf_errmsg);
                  result.error := RESULT_FATAL;
                  clean_now := True;
                  exit;
               end if;

               if Libelf.section_header_is_elf_note (section_header'Access) then
                  declare
                     data : access libelf_h.Elf_Data := Libelf.elf_getdata (elf_section);
                  begin
                     if elf_note_analyze (data, elf_header, info) then
                        --  OS note found
                        exit;
                     end if;
                  end;
               end if;
            end loop;

            if IsBlank (info.osname) then
               EV.emit_error ("failed to get the note section");
               result.error := RESULT_FATAL;
               clean_now := True;
            end if;
         end if;
      end;

      if not clean_now then
         result.error     := RESULT_OK;
         result.osname    := info.osname;
         result.wordsize  := determine_word_size (elf_header);
         result.endian    := determine_endian (elf_header);
         result.arch      := determine_architecture (elf_header);
         result.fpu       := determine_fpu (elf_header, result.arch);
         result.abi       := determine_abi (elf_header, result.arch, result.wordsize);

         if info.use_gnu_tag then
            result.release := SUS (create_strversion_type2 (info.tag));
         else
            result.release := SUS (create_strversion_type1 (info.osversion));
         end if;

         --  validity checks
         if result.abi = abi_unknown then
            --  check abi
            EV.emit_error ("unknown ABI for " & result.arch'Img & " architecture");
            result.error := RESULT_FATAL;
         elsif result.wordsize = size_unknown then
            --  check wordsize
            EV.emit_error ("unknown word size for " & result.arch'Img & " architecture");
            result.error := RESULT_FATAL;
         elsif result.endian = endian_unknown then
            --  check endianness
            EV.emit_error ("unknown endianness for " & result.arch'Img & " architecture");
            result.error := RESULT_FATAL;
         elsif contains (triplet_3 (result), "?") then
            --  check if arch component doesn't have "?" in it (indicating bad combo)
            EV.emit_error ("Bad attribute combination: " & triplet_3 (result));
            result.error := RESULT_FATAL;
         end if;

         if result.error = RESULT_OK then
            --  check that osnames match (exclude solaris, MacOS, and generic)
            case platform is
               when dragonfly | freebsd | netbsd | linux | openbsd =>
                  if not equivalent (result.osname, expected_osname) then
                     EV.emit_error ("OS name mismatch.  Found " & USS (result.osname) &
                                       ", but expected " & expected_osname);
                     result.error := RESULT_FATAL;
                  end if;
               when solaris | omnios | macos | generic_unix => null;
            end case;
         end if;
      end if;

      --  Cleanup

      Libelf.elf_end (elf_obj);
      success := Unix.close_file (fd);

      return result;
   end parse_elf_for_arch;


   --------------------------------------------------------------------
   --  create_strversion_type1
   --------------------------------------------------------------------
   function create_strversion_type1 (osversion : T_Word) return String
   is
      full_integer : Natural;
      fraction     : Natural;
   begin
      case platform is
         when solaris =>
            return "x";   --  Can't happen
         when netbsd =>
            full_integer := Natural ((osversion + 1_000_000) / 100_000_000);
            return int2str (full_integer);
         when dragonfly =>
            --  Return fraction as 0,2,4,6,8
            --  e.g. 5.1 | 5.2 => 5.2, 5.3 | 5.4 => 5.4, etc.
            full_integer := Natural (osversion / 100_000);
            fraction     := Natural (((((osversion / 100) mod 1000) + 1) / 2) * 2);
            return int2str (full_integer) & "." & int2str (fraction);
         when others =>
            full_integer := Natural (osversion / 100_000);
            return int2str (full_integer);
      end case;
   end create_strversion_type1;


   --------------------------------------------------------------------
   --  create_strversion_type2
   --------------------------------------------------------------------
      function create_strversion_type2 (tag : T_GNU_tag) return String is
      begin
         return
           int2str (Integer (tag.version_major)) & "." &
           int2str (Integer (tag.version_minor)) & "." &
           int2str (Integer (tag.version_point));
      end create_strversion_type2;


   --------------------------------------------------------------------
   --  triplet_1
   --------------------------------------------------------------------
   function triplet_1 return String is
   begin
      case platform is
         when freebsd      => return "FreeBSD";
         when dragonfly    => return "DragonFly";
         when netbsd       => return "NetBSD";
         when openbsd      => return "OpenBSD";
         when linux        => return "Linux";
         when solaris      => return "Solaris";
         when macos        => return "Darwin";
         when omnios       => return "Omnios";
         when generic_unix => return "Unix";
      end case;
   end triplet_1;


   --------------------------------------------------------------------
   --  triplet_3
   --------------------------------------------------------------------
   function triplet_3 (breakdown : T_parse_result) return String
   is
      bad_mips : constant String := "mips??";
      bad_arm  : constant String := "arm??";
   begin
      case breakdown.arch is
         when unknown => return "unknown";
         when x86     => return "i386";
         when aarch64 => return "aarch64";
         when ia64    => return "ia64";
         when sparc64 => return "sparc64";
         when x86_64 =>
            case platform is
               when dragonfly | linux | macos =>
                  return "x86_64";
               when others =>
                  return "amd64";
            end case;
         when powerpc =>
            case breakdown.wordsize is
               when BITS_32      => return "powerpc";
               when BITS_64      => return "powerpc64";
               when size_unknown => return "powerpc??";  -- can't get here.
            end case;
         when mips =>
            case breakdown.wordsize is
               when BITS_32 =>
                  case breakdown.endian is
                     when LITTLE =>
                        case breakdown.abi is
                           when abi_irrelevant => return "mips32el";
                           when o32            => return "mips32elo";
                           when n32            => return "mips32eln";
                           when others         => return bad_mips;
                        end case;
                     when BIG =>
                        case breakdown.abi is
                           when abi_irrelevant => return "mips32eb";
                           when o32            => return "mips32ebo";
                           when n32            => return "mips32ebn";
                           when others         => return bad_mips;
                        end case;
                     when endian_unknown => return bad_mips;  -- can't get here
                  end case;
               when BITS_64 =>
                  case breakdown.endian is
                     when LITTLE =>
                        case breakdown.abi is
                           when abi_irrelevant => return "mips64el";
                           when o64            => return "mips64elo";
                           when n64            => return "mips64eln";
                           when others         => return bad_mips;
                        end case;
                     when BIG =>
                        case breakdown.abi is
                           when abi_irrelevant => return "mips64eb";
                           when o64            => return "mips64ebo";
                           when n64            => return "mips64ebn";
                           when others         => return bad_mips;
                        end case;
                     when endian_unknown       => return bad_mips;  -- can't get here
                  end case;
               when size_unknown => return bad_mips;  -- can't get here
            end case;
         when arm =>
            case breakdown.wordsize is
               when BITS_64 | size_unknown => return bad_arm & "64!";
               when BITS_32 =>
                  case breakdown.subarch is
                     when not_applicable =>
                        case breakdown.endian is
                           when LITTLE =>
                              case breakdown.abi is
                                 when eabi | oabi =>
                                    case breakdown.fpu is
                                       when softfp => return "arm";
                                       when others => return bad_arm & "fpu";
                                    end case;
                                 when others => return bad_arm & breakdown.abi'Img;
                              end case;
                           when BIG =>
                              case breakdown.abi is
                                 when eabi | oabi =>
                                    case breakdown.fpu is
                                       when softfp => return "armeb";
                                       when others => return bad_arm & "fpu";
                                    end case;
                                 when others => return bad_arm & breakdown.abi'Img;
                              end case;
                           when endian_unknown => return bad_arm & "endian";
                        end case;
                     when armv6 =>
                        case breakdown.abi is
                           when eabi =>
                              case breakdown.fpu is
                                 when softfp | hardfp => return "armv6";
                                 when fpu_irrelevant  => return bad_arm & "fpu";
                              end case;
                           when others => return bad_arm & "abi";
                        end case;
                     when armv7 =>
                        case breakdown.abi is
                           when eabi =>
                              case breakdown.fpu is
                                 when softfp | hardfp => return "armv7";
                                 when fpu_irrelevant  => return bad_arm & "fpu";
                              end case;
                           when others => return bad_arm & "abi";
                        end case;
                  end case;
            end case;
      end case;
   end triplet_3;

   --------------------------------------------------------------------
   --  determine_word_size
   --------------------------------------------------------------------
   function determine_word_size (elfhdr : gelf_h.GElf_Ehdr) return T_wordsize
   is
      value : Libelf.EI_Byte := Libelf.get_ident_byte (elfhdr, Libelf.EI_CLASS);
   begin
      case value is
         when 1 => return BITS_32;
         when 2 => return BITS_64;
         when others => return size_unknown;
      end case;
   end determine_word_size;


   --------------------------------------------------------------------
   --  determine_endian
   --------------------------------------------------------------------
   function determine_endian (elfhdr : gelf_h.GElf_Ehdr) return T_endian
   is
      value : Libelf.EI_Byte := Libelf.get_ident_byte (elfhdr, Libelf.EI_DATA);
   begin
      case value is
         when 1 => return LITTLE;
         when 2 => return BIG;
         when others => return endian_unknown;
      end case;
   end determine_endian;


   --------------------------------------------------------------------
   --  determine_architecture
   --------------------------------------------------------------------
   function determine_architecture (elfhdr : gelf_h.GElf_Ehdr) return T_arch
   is
      machine : constant elfdefinitions_h.Elf32_Half := elfhdr.e_machine;
   begin
      case machine is
         when   3 => return x86;
         when   8 => return mips;
         when  20 => return powerpc;
         when  21 => return powerpc;
         when  40 => return arm;
         when  43 => return sparc64;
         when  50 => return ia64;
         when  62 => return x86_64;
         when 183 => return aarch64;
         when others => return unknown;
      end case;
   end determine_architecture;


   --------------------------------------------------------------------
   --  determine_fpu
   --------------------------------------------------------------------
   function determine_fpu (elfhdr : gelf_h.GElf_Ehdr; arch : T_arch) return T_fpu is
   begin
      case arch is
         when arm =>
            declare
               type modword is mod 2 ** 16;
               flags            : constant modword := modword (elfhdr.e_flags);
               EF_ARM_VFP_FLOAT : constant modword := 16#400#;
               float_setting    : constant modword := flags and EF_ARM_VFP_FLOAT;
            begin
               case float_setting is
                  when      0 => return softfp;
                  when others => return hardfp;
               end case;
            end;
         when others => return fpu_irrelevant;
      end case;
   end determine_fpu;


   --------------------------------------------------------------------
   --  determine_abi
   --------------------------------------------------------------------
   function determine_abi (elfhdr : gelf_h.GElf_Ehdr;
                           arch   : T_arch;
                           size   : T_wordsize) return T_abi
   is
      type modword is mod 2 ** 32;
      EF_ARM_EABIMASK : constant modword := 16#FF000000#;
      EF_MIPS_ABI     : constant modword := 16#0000F000#;
      EF_MIPS_ABI_O32 : constant modword := 16#00001000#;
      EF_MIPS_ABI_N32 : constant modword := 16#00000020#;
      EF_MIPS_ABI_O64 : constant modword := 16#00002000#;
      ELFOSABI_NONE   : constant Libelf.EI_Byte := 0;
      flags           : constant modword := modword (elfhdr.e_flags);
   begin
      case arch is
         when arm =>
            declare
               eabi_mask : constant modword := flags and EF_ARM_EABIMASK;
               osabi     : Libelf.EI_Byte := Libelf.get_ident_byte (elfhdr, Libelf.EI_OSABI);
            begin
               if eabi_mask /= 0 then
                  return eabi;
               elsif osabi /= ELFOSABI_NONE then
                  --  EABI executables all have this field set to ELFOSABI_NONE,
                  --  therefore it must be an oabi file.
                  return oabi;
               else
                  --  EABI executables all have this field set to ELFOSABI_NONE,
                  --  So if the eabi mask flag wasn't set, there's an identification problem
                  return abi_unknown;
               end if;
            end;
         when mips =>
            declare
               mips_abi : constant modword := flags and EF_MIPS_ABI;
               mips_n32 : constant modword := flags and EF_MIPS_ABI_N32;
            begin
               if mips_abi = 0 then
                  if mips_n32 = EF_MIPS_ABI_N32 then
                     return n32;
                  else
                     return abi_irrelevant;
                  end if;
               else
                  case mips_abi is
                     when EF_MIPS_ABI_O32 => return o32;
                     when EF_MIPS_ABI_O64 => return o64;
                     when others =>
                        case size is
                           when BITS_32      => return o32;
                           when BITS_64      => return n64;
                           when size_unknown => return abi_unknown;
                        end case;
                  end case;
               end if;
            end;
         when others => return abi_irrelevant;
      end case;
   end determine_abi;


   --------------------------------------------------------------------
   --  roundup2
   --------------------------------------------------------------------
   function roundup2 (x, y : Natural) return Natural
   is
      --  c code: (((x)+((y)-1))&(~((y)-1)))  /* if y is powers of two */
      type pos32 is mod 2 ** 32;
      xx   : pos32 := pos32 (x);
      yym1 : pos32 := pos32 (y - 1);
   begin
      return Natural ((xx + yym1) and (not yym1));
   end roundup2;


   --------------------------------------------------------------------
   --  le32dec
   --------------------------------------------------------------------
   function le32dec (wordstr : T_Wordstr) return T_Word
   is
      p0 : Integer := Character'Pos (wordstr (wordstr'First));
      p1 : Integer := Character'Pos (wordstr (wordstr'First + 1));
      p2 : Integer := Character'Pos (wordstr (wordstr'First + 2));
      p3 : Integer := Character'Pos (wordstr (wordstr'First + 3));
   begin
      return T_Word
        (p0 +
        (p1 * 2 ** 8) +
        (p2 * 2 ** 16) +
        (p3 * 2 ** 24));
   end le32dec;



   --------------------------------------------------------------------
   --  be32dec
   --------------------------------------------------------------------
   function be32dec (wordstr : T_Wordstr) return T_Word
   is
      p0 : Integer := Character'Pos (wordstr (wordstr'First));
      p1 : Integer := Character'Pos (wordstr (wordstr'First + 1));
      p2 : Integer := Character'Pos (wordstr (wordstr'First + 2));
      p3 : Integer := Character'Pos (wordstr (wordstr'First + 3));
   begin
      return T_Word
        (p3 +
        (p2 * 2 ** 8) +
        (p1 * 2 ** 16) +
        (p0 * 2 ** 24));
   end be32dec;


   --------------------------------------------------------------------
   --  elf_note_analyze
   --------------------------------------------------------------------
   function elf_note_analyze (data   : access libelf_h.u_Elf_Data;
                              elfhdr : gelf_h.GElf_Ehdr;
                              info   : out T_elf_info) return Boolean
   is
      subtype note_buffer is String (1 .. Libelf.elf_note_size);

      NT_VERSION     : constant Natural := 1;
      NT_GNU_ABI_TAG : constant Natural := 1;
      ELFDATA2MSB    : constant Natural := 2;

      function buffer_to_elfnote is
        new Ada.Unchecked_Conversion (Source => note_buffer,
                                      Target => elfdefinitions_h.Elf_Note);

      buffer : String := Libelf.convert_elf_data_buffer (data);
      index  : Natural := buffer'First;
      bnote  : note_buffer;
      note   : elfdefinitions_h.Elf_Note;
      found  : Boolean := False;

      eidata : Libelf.EI_Byte := Libelf.get_ident_byte (elfhdr, Libelf.EI_DATA);
      bigend : constant Boolean := (eidata = ELFDATA2MSB);

      invalid_osname : constant String := "unknown";
   begin

      loop
         exit when found;
         exit when index + note_buffer'Length - 1 > buffer'Last;
         bnote := buffer (index .. index + note_buffer'Length - 1);
         note := buffer_to_elfnote (bnote);
         index := index + note_buffer'Length;
         declare
            --  Subtract 1 from n_namesz, the strings are null-terminated
            name : String := buffer (index .. index + Natural (note.n_namesz) - 2);
         begin
            index := index + roundup2 (Natural (note.n_namesz), 4);
            if name = "DragonFly" or else
              name = "FreeBSD" or else
              name = "NetBSD"
            then
               if Natural (note.n_type) = NT_VERSION then
                  found := True;
                  info.use_gnu_tag := False;
                  info.osname := SUS (name);
                  if bigend then
                     info.osversion := be32dec (buffer (index .. index + 3));
                  else
                     info.osversion := le32dec (buffer (index .. index + 3));
                  end if;
               end if;
            elsif name = "GNU" then
               if Natural (note.n_type) = NT_GNU_ABI_TAG then
                  --  verify we have 16 more characters in the buffer.
                  --  If so, set tag and osname.
                  --  If not, output debug message
                  if index + 15 <= buffer'Last then
                     found := True;
                     info.use_gnu_tag := True;
                     declare
                        workword : T_Word;
                     begin
                        for x in 1 .. 4 loop
                           if bigend then
                              workword := be32dec (buffer (index .. index + 3));
                           else
                              workword := le32dec (buffer (index .. index + 3));
                           end if;
                           case x is
                              when 1 => info.tag.os_descriptor := workword;
                              when 2 => info.tag.version_major := workword;
                              when 3 => info.tag.version_minor := workword;
                              when 4 => info.tag.version_point := workword;
                           end case;
                           index := index + 4;
                        end loop;
                     end;
                     case info.tag.os_descriptor is
                        when 0 => info.osname := SUS ("Linux");
                        when 1 => info.osname := SUS ("GNU");
                        when 2 => info.osname := SUS ("Solaris");
                        when 3 => info.osname := SUS ("FreeBSD");
                        when 4 => info.osname := SUS ("NetBSD");
                        when 5 => info.osname := SUS ("Syllable");
                        when others => info.osname := SUS (invalid_osname);
                     end case;
                  else
                     EV.emit_debug (3, "elf-note: NT_GNU_ABI_TAG found, but tag < 16 chars");
                  end if;
               end if;
            elsif IsBlank (name) then
               if Natural (note.n_type) = NT_VERSION then
                  found := True;
                  info.use_gnu_tag := False;
                  info.osname := SUS (invalid_osname);
               end if;
            end if;
         end;
      end loop;

      return found;
   end elf_note_analyze;


end Core.Elf_Operations;
