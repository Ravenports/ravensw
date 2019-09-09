--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Core.Checksum;

package body Core.Atf is

   --------------------------------------------------------------------
   --  pkg_checksum_file
   --------------------------------------------------------------------
   function pkg_checksum_file (path : ICS.chars_ptr; checksum_type : T_checksum_type)
                               return ICS.chars_ptr
   is
      path2  : String := ICS.Value (path);
      result : String := Checksum.pkg_checksum_file (path2, checksum_type);
   begin
      return ICS.New_String (result);
   end pkg_checksum_file;


   --------------------------------------------------------------------
   --  pkg_checksum_symlink
   --------------------------------------------------------------------
   function pkg_checksum_symlink (path : ICS.chars_ptr; checksum_type : T_checksum_type)
                                  return ICS.chars_ptr
   is
      path2  : String := ICS.Value (path);
      result : String := Checksum.pkg_checksum_symlink (path2, checksum_type);
   begin
      return ICS.New_String (result);
   end pkg_checksum_symlink;


   --------------------------------------------------------------------
   --  pkg_checksum_symlink
   --------------------------------------------------------------------
   function pkg_checksum_validate_file (path : ICS.chars_ptr; sum : ICS.chars_ptr) return IC.int
   is
      path2  : String := ICS.Value (path);
      sum2   : String := ICS.Value (sum);
      result : Boolean := Checksum.pkg_checksum_validate_file (path2, sum2);
   begin
      if result then
         return IC.int (0);
      else
         return IC.int (-1);
      end if;
   end pkg_checksum_validate_file;


   --------------------------------------------------------------------
   --  pkg_checksum_generate_file
   --------------------------------------------------------------------
   function pkg_checksum_generate_file (path : ICS.chars_ptr; checksum_type : T_checksum_type)
                                        return ICS.chars_ptr
   is
      path2  : String := ICS.Value (path);
      result : String := Checksum.pkg_checksum_generate_file (path2, checksum_type);
   begin
      return ICS.New_String (result);
   end pkg_checksum_generate_file;

end Core.Atf;
