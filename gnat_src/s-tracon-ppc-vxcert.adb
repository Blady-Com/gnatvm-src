------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             S Y S T E M . T R A C E B A C K _ C O N T R O L              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--             Copyright (C) 2012, Free Software Foundation, Inc.           --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This is the System.Traceback_Control implementation for PPC/VxWorks-Cert.

package body System.Traceback_Control is

   ---------------------------
   --  Is_Topframe_Retaddr  --
   ---------------------------

   function Is_Topframe_Retaddr (Retaddr : System.Address) return Boolean is

      --  VxWorks-Cert marks call-chain entry points with a null return
      --  address.

   begin
      return Retaddr = System.Null_Address;
   end Is_Topframe_Retaddr;

   -----------------------------
   --  Return_Address_Offset  --
   -----------------------------

   function Return_Address_Offset return System.Address is
      --  VxWorks obeys the PowerPC EABI where the return address slot
      --  is located right past the 4bytes back-chain link in a frame:
      Return_Address_Slot_Offset : constant := 4;

   begin
      return Return_Address_Slot_Offset;
   end Return_Address_Offset;

end System.Traceback_Control;
