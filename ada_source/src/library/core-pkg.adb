--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Core.Pkg is


   --------------------------------------------------------------------
   --  repo_priority_less_than
   --------------------------------------------------------------------
   function repo_priority_less_than (A, B : T_repo_priority) return Boolean
   is
      --  Display 100 before 90, so it's reverse order (use greater than for "<")
   begin
      if A.priority = B.priority then
         return SU.">" (A.reponame, B.reponame);
      else
         return A.priority > B.priority;
      end if;
   end repo_priority_less_than;

end Core.Pkg;
