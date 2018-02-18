------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           S Y S T E M . F I N A L I Z A T I O N _ M A S T E R S          --
--                                                                          --
--                                B o d y                                   --
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

with Ada.Exceptions;           use Ada.Exceptions;

with System;                   use System;
with System.Finalization_Root; use System.Finalization_Root;

package body System.Finalization_Masters is

   ------------
   -- Attach --
   ------------

   procedure Attach
     (Master : in out Finalization_Master;
      Obj    : System.Finalization_Root.Root_Controlled_Ptr)
   is
   begin
      --  Do not allow the allocation of controlled objects while the
      --  associated master is being finalized.

      if Master.Finalization_Started then
         raise Program_Error with "allocation after finalization started";
      end if;

      Master.Objects.Next.Prev := Obj;
      Obj.Next := Master.Objects.Next;
      Master.Objects.Next := Obj;
      Obj.Prev := Master.Objects'Unchecked_Access;
   end Attach;

   ------------
   -- Detach --
   ------------

   procedure Detach (Obj : System.Finalization_Root.Root_Controlled_Ptr) is
   begin
      if Obj.Next /= null and then Obj.Prev /= null then
         Obj.Prev.Next := Obj.Next;
         Obj.Next.Prev := Obj.Prev;
         Obj.Prev := null;
         Obj.Next := null;
      end if;
   end Detach;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Master : in out Finalization_Master) is
      Curr_Ptr : Root_Controlled_Ptr;
      Ex_Occur : Exception_Occurrence;
      Raised   : Boolean := False;

   begin
      --  Lock the master to prevent any allocations while the objects are
      --  being finalized. The master remains locked because the associated
      --  access type is about to go out of scope.

      Master.Finalization_Started := True;

      --  Skip the dummy head

      Curr_Ptr := Master.Objects.Next;
      while Curr_Ptr /= Master.Objects'Unchecked_Access loop
         begin
            --  This call is treated specially by the compiler (see Exp_Ch6.
            --  Expand_Call) and is converted into a call to Deep_Finalize.

            Finalize (Curr_Ptr.all);

         exception
            when Fin_Except : others =>
               if not Raised then
                  Raised := True;
                  Save_Occurrence (Ex_Occur, Fin_Except);
               end if;
         end;

         Curr_Ptr := Curr_Ptr.Next;
      end loop;

      --  If the finalization of a particular node raised an exception, reraise
      --  it after the remainder of the list has been finalized.

      if Raised then
         Reraise_Occurrence (Ex_Occur);
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Master : in out Finalization_Master) is
   begin
      --  The dummy head must point to itself in both directions

      Master.Objects.Next := Master.Objects'Unchecked_Access;
      Master.Objects.Prev := Master.Objects'Unchecked_Access;
   end Initialize;

end System.Finalization_Masters;
