--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Core.Pkg is


   --------------------------------------------------------------------
   --  repo_priority_less_than
   --------------------------------------------------------------------
   function repo_priority_less_than (A, B : T_repo_priority) return Boolean is
   begin
      return A.priority < B.priority;
   end repo_priority_less_than;

end Core.Pkg;
