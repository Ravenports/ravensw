--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Pkg; use Core.Pkg;

private with Libelf;
private with libelf_h;
private with gelf_h;

package Core.Elf_Operations is

   type abi_result is
      record
         abi : Text;
         error : Pkg_Error_Type;
      end record;

   function calculate_abi return abi_result;

private

   type T_wordsize is (size_unknown, BITS_32, BITS_64);
   type T_endian   is (endian_unknown, BIG, LITTLE);
   type T_arch     is (unknown, x86, x86_64, aarch64, arm, mips, powerpc, sparc64, ia64);
   type T_subarch  is (not_applicable, armv6, armv7);
   type T_fpu      is (fpu_irrelevant, hardfp, softfp);
   type T_abi      is (abi_irrelevant, eabi, oabi, o32, n32, o64, n64, abi_unknown);

   type T_parse_result is
      record
         osname    : Text;
         release   : Text;
         wordsize  : T_wordsize;
         endian    : T_endian;
         arch      : T_arch;
         subarch   : T_subarch;
         fpu       : T_fpu;
         abi       : T_abi;
         error     : Pkg_Error_Type;
      end record;

   type T_Word is range 0 .. 2 ** 32 - 1;
   subtype T_Wordstr is String (1 .. 4);

   type T_GNU_tag is
      record
         os_descriptor : T_Word;
         version_major : T_Word;
         version_minor : T_Word;
         version_point : T_Word;
      end record;

   --  Return <OS> of <OS>:<VERSION>:<architecture> triplet
   function triplet_1 return String;

   --  Return <architecture> of <OS>:<VERSION>:<architecture> triplet
   function triplet_3 (breakdown : T_parse_result) return String;

   --  Create version string from version natural
   function create_strversion_type1 (osversion : Natural) return String;

   --  Create version string from GNU tag
   function create_strversion_type2 (tag : T_GNU_tag) return String;

   type T_elf_info is
      record
         osname      : Text;
         use_gnu_tag : Boolean;
         osversion   : Natural;
         tag         : T_GNU_tag;
      end record;

   --  Read a file for ABI information and return it
   function pkg_get_myarch_elfparse (expected_osname : String) return T_parse_result;

   --  Given elf note section, derive note info.  Return True if OS version not found
   function elf_note_analyse (data   : access libelf_h.u_Elf_Data;
                              elfhdr : access gelf_h.GElf_Ehdr;
                              info   : out T_elf_info) return Boolean;

   --  Check value of e_ident[EI_CLASS] and derive word size
   function determine_word_size (elfhdr : access gelf_h.GElf_Ehdr) return T_wordsize;

   --  Check value of e_ident[EI_DATA] and derive endian orientation
   function determine_endian (elfhdr : access gelf_h.GElf_Ehdr) return T_endian;

   --  Check value of e_machine and derive machine
   function determine_architecture (elfhdr : access gelf_h.GElf_Ehdr) return T_arch;

   --  If necessary, specify floating point capability
   function determine_fpu (elfhdr : access gelf_h.GElf_Ehdr; arch : T_arch) return T_fpu;

   --  Return abi given archive and elfclass, if pertinent
   function determine_abi (elfhdr : access gelf_h.GElf_Ehdr;
                           arch   : T_arch;
                           size   : T_wordsize) return T_abi;

   --  Round up by y, but y has to be a power of 2
   function roundup2 (x, y : Natural) return Natural;

   --  Decode 4-character string into word (big endian)
   function be32dec (wordstr : T_Wordstr) return T_Word;

   --  Decode 4-character string into word (littler endian)
   function le32dec (wordstr : T_Wordstr) return T_Word;

end Core.Elf_Operations;
