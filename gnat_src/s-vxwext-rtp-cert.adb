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

--  This package provides VxWorks specific support functions needed
--  by System.OS_Interface.

--  This is the VxWorks Cert 6 RTP / ravenscar-cert version of this package

package body System.VxWorks.Ext is

   ERROR : constant := -1;

   function sigwaitinfo
     (set      : access sigset_t;
      sigvalue : System.Address) return int;
   pragma Export (C, sigwaitinfo, "sigwaitinfo");
   --  Dummy version for VxWorks Cert 6 RTPs

   --------------
   -- Int_Lock --
   --------------

   function Int_Lock return int is
   begin
      return ERROR;
   end Int_Lock;

   ----------------
   -- Int_Unlock --
   ----------------

   function Int_Unlock return int is
   begin
      return ERROR;
   end Int_Unlock;

   -----------------------
   -- Interrupt_Connect --
   -----------------------

   function Interrupt_Connect
     (Vector    : Interrupt_Vector;
      Handler   : Interrupt_Handler;
      Parameter : System.Address := System.Null_Address) return int
   is
      pragma Unreferenced (Vector, Handler, Parameter);
   begin
      return ERROR;
   end Interrupt_Connect;

   -----------------------
   -- Interrupt_Context --
   -----------------------

   function Interrupt_Context return int is
   begin
      --  For RTPs, never in an interrupt context

      return 0;
   end Interrupt_Context;

   --------------------------------
   -- Interrupt_Number_To_Vector --
   --------------------------------

   function Interrupt_Number_To_Vector
     (intNum : int) return Interrupt_Vector
   is
      pragma Unreferenced (intNum);
   begin
      return 0;
   end Interrupt_Number_To_Vector;

   ---------------
   -- semDelete --
   ---------------

   function semDelete (Sem : SEM_ID) return int is
   pragma Unreferenced (Sem);
   begin
      --  Not supported in cert RTP API subset
      return ERROR;
   end semDelete;

   --------------------
   -- Set_Time_Slice --
   --------------------

   function Set_Time_Slice (ticks : int) return int is
      pragma Unreferenced (ticks);
   begin
      return ERROR;
   end Set_Time_Slice;

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

end System.VxWorks.Ext;