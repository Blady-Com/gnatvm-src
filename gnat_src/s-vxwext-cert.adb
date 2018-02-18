------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                     S Y S T E M . V X W O R K S . E X T                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--            Copyright (C) 2008-2011, Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides vxworks specific support functions needed
--  by System.OS_Interface.

--  This is the version for VxWorks CERT 6.6 and greater

package body System.VxWorks.Ext is

   ERROR : constant := -1;

   function sigwaitinfo
     (set      : access sigset_t;
      sigvalue : System.Address) return int;
   pragma Export (C, sigwaitinfo, "sigwaitinfo");
   --  Dummy version for VxWorks 6 Cert kernel

   --------------
   -- Int_Lock --
   --------------

   function intLock return int;
   pragma Import (C, intLock, "intLock");

   function Int_Lock return int renames intLock;

   ----------------
   -- Int_Unlock --
   ----------------

   function intUnlock return int;
   pragma Import (C, intUnlock, "intUnlock");

   function Int_Unlock return int renames intUnlock;

   ---------------
   -- semDelete --
   ---------------

   function semDelete (Sem : SEM_ID) return int is
   pragma Unreferenced (Sem);
   begin
      --  Not supported in VxWorks CERT

      return ERROR;
   end semDelete;

   -----------------
   -- sigwaitinfo --
   -----------------

   function sigwaitinfo
     (set      : access sigset_t;
      sigvalue : System.Address) return int
   is
      pragma Unreferenced (set, sigvalue);
   begin
      return ERROR;
   end sigwaitinfo;

   ------------------------
   -- taskCpuAffinitySet --
   ------------------------

   function taskCpuAffinitySet (tid : t_id; CPU : int) return int is
      pragma Unreferenced (tid, CPU);
   begin
      return ERROR;
   end taskCpuAffinitySet;

   -------------------------
   -- taskMaskAffinitySet --
   -------------------------

   function taskMaskAffinitySet (tid : t_id; CPU_Set : unsigned) return int is
      pragma Unreferenced (tid, CPU_Set);
   begin
      return ERROR;
   end taskMaskAffinitySet;

   --------------
   -- taskStop --
   --------------

   function Task_Stop (tid : t_id) return int is
      function taskSuspend (tid : t_id) return int;
      pragma Import (C, taskSuspend, "taskSuspend");
   begin
      return taskSuspend (tid);
   end Task_Stop;

end System.VxWorks.Ext;
