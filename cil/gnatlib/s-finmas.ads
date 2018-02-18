------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           S Y S T E M . F I N A L I Z A T I O N _ M A S T E R S          --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--             Copyright (C) 2011, Free Software Foundation, Inc.           --
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

--  This is a .NET/JVM-specific version

with Ada.Finalization;
with System.Finalization_Root;

package System.Finalization_Masters is

   type Finalization_Master is
     new Ada.Finalization.Limited_Controlled with private;

   type Finalization_Master_Ptr is access all Finalization_Master;
   for Finalization_Master_Ptr'Storage_Size use 0;

   procedure Attach
     (Master : in out Finalization_Master;
      Obj    : System.Finalization_Root.Root_Controlled_Ptr);
   --  Prepend an object to a specific master

   procedure Detach (Obj : System.Finalization_Root.Root_Controlled_Ptr);
   --  Remove an object from an arbitrary list

   overriding procedure Finalize (Master : in out Finalization_Master);
   --  Traverse the objects of Master, invoking Deep_Finalize on eanch of them.
   --  In the end, the routine destroys its dummy head.

   overriding procedure Initialize (Master : in out Finalization_Master);
   --  Create a new Master by allocating a dummy head

private
   type Finalization_Master is
     new Ada.Finalization.Limited_Controlled with
   record
      Objects : aliased System.Finalization_Root.Root_Controlled;
      --  The head of a doubly linked list

      Finalization_Started : Boolean := False;
      --  When the finalization of a master takes place, any allocations
      --  on the same master are prohibited and the action must raise
      --  Program_Error.
   end record;

end System.Finalization_Masters;
