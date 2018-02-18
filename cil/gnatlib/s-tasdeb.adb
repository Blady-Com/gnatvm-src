------------------------------------------------------------------------------
--                                                                          --
--                    GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                  S Y S T E M . T A S K I N G . D E B U G                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 1998-2010, AdaCore                     --
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
-- The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by --
-- AdaCore - http://www.adacore.com                                         --
--                                                                          --
------------------------------------------------------------------------------

--  This is the .NET/JVM specific version of this package
--  It currently only provides null debug functions.

with System.Task_Primitives.Operations;

package body System.Tasking.Debug is

   package STPO renames System.Task_Primitives.Operations;

   ------------------------
   -- Task_Creation_Hook --
   ------------------------

   procedure Task_Creation_Hook (Thread : OS_Interface.Thread_Id) is
      pragma Inspection_Point (Thread);
   begin
      null;
   end Task_Creation_Hook;

   ---------------------------
   -- Task_Termination_Hook --
   ---------------------------

   procedure Task_Termination_Hook is
   begin
      null;
   end Task_Termination_Hook;

   --------------------
   -- Set_User_State --
   --------------------

   procedure Set_User_State (Value : Long_Integer) is
   begin
      STPO.Self.User_State := Value;
   end Set_User_State;

   --------------------
   -- Get_User_State --
   --------------------

   function Get_User_State return Long_Integer is
   begin
      return 0;
   end Get_User_State;

   ------------------------
   -- Signal_Debug_Event --
   ------------------------

   procedure Signal_Debug_Event
     (Event_Kind : Event_Kind_Type;
      Task_Value : Task_Id)
   is
      pragma Unreferenced (Event_Kind, Task_Value);
   begin
      null;
   end Signal_Debug_Event;

   ------------------------
   -- Print_Current_Task --
   ------------------------

   procedure Print_Current_Task is
   begin
      null;
   end Print_Current_Task;

   ---------------------
   -- Print_Task_Info --
   ---------------------

   procedure Print_Task_Info (T : Task_Id) is
      pragma Unreferenced (T);
   begin
      null;
   end Print_Task_Info;

   ----------------------
   -- Print_List_Tasks --
   ----------------------

   procedure List_Tasks is
   begin
      null;
   end List_Tasks;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Self_Id  : Task_Id;
      Msg      : String;
      Flag     : Character;
      Other_Id : Task_Id := null)
   is
      pragma Unreferenced (Self_Id, Msg, Other_Id, Flag);
   begin
      null;
   end Trace;

   ---------------
   -- Set_Trace --
   ---------------

   procedure Set_Trace
     (Flag  : Character;
      Value : Boolean := True)
   is
      pragma Unreferenced (Flag, Value);
   begin
      null;
   end Set_Trace;

   --------------------
   -- Stop_All_Tasks --
   --------------------

   procedure Stop_All_Tasks is
      C : Task_Id;

      Dummy : Boolean;
      pragma Unreferenced (Dummy);

   begin
      STPO.Lock_RTS;

      C := All_Tasks_List;
      while C /= null loop
         Dummy := STPO.Stop_Task (C);
         C := C.Common.All_Tasks_Link;
      end loop;

      STPO.Unlock_RTS;
   end Stop_All_Tasks;

   ----------------------------
   -- Stop_All_Tasks_Handler --
   ----------------------------

   procedure Stop_All_Tasks_Handler is
   begin
      STPO.Stop_All_Tasks;
   end Stop_All_Tasks_Handler;

   ------------------------
   -- Continue_All_Tasks --
   ------------------------

   procedure Continue_All_Tasks is
      C : Task_Id;
      R : Boolean;
      pragma Unreferenced (R);
   begin
      C := All_Tasks_List;
      while C /= null loop
         R := STPO.Continue_Task (C);
         C := C.Common.All_Tasks_Link;
      end loop;
   end Continue_All_Tasks;

   -----------------------
   -- Suspend_All_Tasks --
   -----------------------

   procedure Suspend_All_Tasks (Thread_Self : OS_Interface.Thread_Id) is
      C : Task_Id;
      R : Boolean;
      pragma Unreferenced (R);
   begin
      C := All_Tasks_List;
      while C /= null loop
         R := STPO.Suspend_Task (C, Thread_Self);
         C := C.Common.All_Tasks_Link;
      end loop;
   end Suspend_All_Tasks;

   ----------------------
   -- Resume_All_Tasks --
   ----------------------

   procedure Resume_All_Tasks (Thread_Self : OS_Interface.Thread_Id) is
      C : Task_Id;
      R : Boolean;
      pragma Unreferenced (R);
   begin
      C := All_Tasks_List;
      while C /= null loop
         R := STPO.Resume_Task (C, Thread_Self);
         C := C.Common.All_Tasks_Link;
      end loop;
   end Resume_All_Tasks;

end System.Tasking.Debug;
