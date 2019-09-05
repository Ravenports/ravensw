--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Core.Iterators is

   --------------------------------------------------------------------
   --  invalid_iterator
   --------------------------------------------------------------------
   function invalid_iterator (this : Base_Iterators) return Boolean is
   begin
      return this.valid;
   end invalid_iterator;

end Core.Iterators;
