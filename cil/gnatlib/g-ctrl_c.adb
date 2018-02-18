------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          G N A T . C T R L _ C                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 2002-2010, AdaCore                    --
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

with Interfaces.CIL.Object;

package body GNAT.Ctrl_C is

   --  Imports from System.Console

   type ConsoleCancelEventHandler is access procedure
     (sender : Interfaces.CIL.Object.Ref;
      arg    : Interfaces.CIL.Object.Ref);
   pragma Import
     (CIL, ConsoleCancelEventHandler,
      "[mscorlib]System.ConsoleCancelEventHandler");

   procedure set_TreatControlCAsInput (Val : Boolean);
   pragma Import
     (CIL, set_TreatControlCAsInput,
      "[mscorlib]System.Console.set_TreatControlCAsInput");

   procedure add_CancelKeyPress (Handler : ConsoleCancelEventHandler);
   pragma Import
     (CIL, add_CancelKeyPress,
      "[mscorlib]System.Console.add_CancelKeyPress");

   procedure remove_CancelKeyPress (Handler : ConsoleCancelEventHandler);
   pragma Import
     (CIL, remove_CancelKeyPress,
      "[mscorlib]System.Console.remove_CancelKeyPress");

   --  Callbacks

   Ada_Handler : Handler_Type;

   procedure CIL_Handler_Internal
     (sender : Interfaces.CIL.Object.Ref;
      arg    : Interfaces.CIL.Object.Ref);
   pragma Convention (CIL, CIL_Handler_Internal);

   --------------------------
   -- CIL_Handler_Internal --
   --------------------------

   procedure CIL_Handler_Internal
     (sender : Interfaces.CIL.Object.Ref;
      arg    : Interfaces.CIL.Object.Ref)
   is
      pragma Unreferenced (sender, arg);
   begin
      Ada_Handler.all;
   end CIL_Handler_Internal;

   --  Use a constant here so that we can use the same CIL delegate for both
   --  add and remove.

   CIL_Handler : constant ConsoleCancelEventHandler :=
                   CIL_Handler_Internal'Access;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler (Handler : Handler_Type) is
      Add_CIL_Handler : Boolean := False;

   begin
      if Ada_Handler = null then
         Add_CIL_Handler := True;
      end if;

      Ada_Handler := Handler;

      if Add_CIL_Handler then
         set_TreatControlCAsInput (False);
         add_CancelKeyPress (CIL_Handler);
      end if;
   end Install_Handler;

   -----------------------
   -- Uninstall_Handler --
   -----------------------

   procedure Uninstall_Handler is
   begin
      if Ada_Handler = null then
         return;
      end if;

      remove_CancelKeyPress (CIL_Handler);
      Ada_Handler := null;
   end Uninstall_Handler;

end GNAT.Ctrl_C;
