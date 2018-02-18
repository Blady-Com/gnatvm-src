------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 G N A T . S E C O N D A R Y _ S T A C K                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides facilities for modifying the secondary stack pointer.

with System; use System;
with System.Secondary_Stack;
with System.Tasking;

package GNAT.Secondary_Stack is
   function Get_Sec_Stack return Address
     renames System.Tasking.Get_Sec_Stack;
   --  Return the address of the task specific secondary stack, as expected by
   --  System.Secondary_Stack.

   procedure Set_Sec_Stack (Stk : Address)
     renames System.Tasking.Set_Sec_Stack;
   --  Set the secondary stack pointer for the current task

   procedure SS_Init (Stack : System.Address; Size : Natural)
     renames System.Secondary_Stack.SS_Init;
   --  Initialize secondary stack Stack. Must be called once before using it.
   --  The value Size is the size of the stack in bytes. The minimal size
   --  is 16 bytes, and the stack must be aligned at least on
   --  Standard'Maximum_Alignment.
end GNAT.Secondary_Stack;
