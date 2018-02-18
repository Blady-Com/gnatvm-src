------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--                     Copyright (C) 2009-2010, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Ravenscar version of this package for APEX

--  This package encapsulates all direct interfaces to OS services that are
--  needed by the tasking run-time (libgnarl).

with Unchecked_Conversion;
with System.Storage_Elements;

package body System.OS_Interface is

   ----------------
   -- Local data --
   ----------------

   Max_Thread_Num : constant := 512;
   --  Maximum number of threads that can be created (where does this
   --  limit come from ???)

   type Thread_Id_Array is
     array (Integer range 1 .. Max_Thread_Num) of Thread_Id;
   All_Threads : Thread_Id_Array := (others => null);
   --  Array of tasks.  Used to implement Get_ATCB.
   --  ??? This duplicates system.tasking.debug.known_tasks.

   Max_Name_Length : constant := 30;
   type Name_Range is range 0 .. Max_Name_Length - 1;
   type Name_Type is array (Name_Range) of Character;

   type Process_Attribute_Type is record
      Name          : Name_Type;
      Entry_Point   : System.Address;
      Stack_Size    : Integer;
      Base_Priority : Integer;
      Period        : Time;
      Time_Capacity : Time;
      Deadline      : Integer;
   end record;
   pragma Convention (C, Process_Attribute_Type);

   type Process_Status_Type is record
      Attributes       : Process_Attribute_Type;
      Current_Priority : Integer;
      Deadline_Time    : Integer;
      Process_State    : Integer;
   end record;
   pragma Convention (C, Process_Status_Type);

   ----------------------
   -- Local procedures --
   ----------------------

   procedure Create_Process
     (Attributes  : access Process_Attribute_Type;
      Process_Id  : out Integer;
      Return_Code : out Integer);
   pragma Import (C, Create_Process, "CREATE_PROCESS");

   procedure Start
     (Process_Id  : Integer;
      Return_Code : out Integer);
   pragma Import (C, Start, "START");

   procedure Get_My_Id
     (Process_Id  : out Integer;
      Return_Code : out Integer);
   pragma Import (C, Get_My_Id, "GET_MY_ID");

   procedure Get_Process_Status
     (Process_Id     : Integer;
      Process_Status : out Process_Status_Type;
      Return_Code    : out Integer);
   pragma Import (C, Get_Process_Status, "GET_PROCESS_STATUS");

   procedure Set_Priority
     (Process_Id  : Integer;
      Priority    : Integer;
      Return_Code : out Integer);
   pragma Import (C, Set_Priority, "SET_PRIORITY");

   procedure Suspend_Self
     (Time_Out    : Time_Span;
      Return_Code : out Integer);
   pragma Import (C, Suspend_Self, "SUSPEND_SELF");

   procedure Resume
     (Process_Id  : Integer;
      Return_Code : out Integer);
   pragma Import (C, Resume, "RESUME");

   procedure Get_Time
      (System_Time : out Time;
       Return_Code : out Integer);
   pragma Import (C, Get_Time, "GET_TIME");

   procedure Timed_Wait
     (Delay_Time  : Time_Span;
      Return_Code : out Integer);
   pragma Import (C, Timed_Wait, "TIMED_WAIT");

   procedure APEX_Task_Wrapper;
   --  On typical GNAT targets, we can pass arguments to threads. In APEX, we
   --  cannot pass arguments to processes, so we need to add an extra level of
   --  indirection which in turn will call the Ada task with the appropriate
   --  parameter.

   -------------------------
   --  APEX_Task_Wrapper  --
   -------------------------

   procedure APEX_Task_Wrapper is
      Self : constant Thread_Id := Thread_Self;

      type Task_Procedure_Access is access procedure (Arg : System.Address);

      function To_Procedure is
        new Unchecked_Conversion (System.Address, Task_Procedure_Access);

   begin
      --  Call the task wrapper with the required argument (Task_Id)

      To_Procedure (Self.Code) (Self.Arg);
   end APEX_Task_Wrapper;

   ----------------------
   --  Attach_Handler  --
   ----------------------

   procedure Attach_Handler
     (Handler : Interrupt_Handler;
      Id      : Interrupt_ID)
   is
   begin
      --  ??? Not yet supported

      raise Program_Error;
   end Attach_Handler;

   ------------------
   --  Current_CPU --
   ------------------

   function Current_CPU return Multiprocessors.CPU is
   begin
      --  No multiprocessor support, always return the first CPU Id

      return Multiprocessors.CPU'First;
   end Current_CPU;

   -------------------------
   --  Current_Interrupt  --
   -------------------------

   function Current_Interrupt return Interrupt_ID is
   begin
      --  ??? Not yet supported

      return No_Interrupt;
   end Current_Interrupt;

   -------------
   --  Clock  --
   -------------

   function Clock return Time is
      Now : Time;
      Res : Integer;

   begin
      Get_Time (Now, Res);
      pragma Assert (Res = 0);

      return Now;
   end Clock;

   -------------------
   --  Delay_Until  --
   -------------------

   procedure Delay_Until (T : Time) is
      Res : Integer;

   begin
      --  Never use negative numbers for delays because they mean infinite time

      Timed_Wait (Time_Span'Max (Time_Span (T - Clock), 0), Res);

      --  ARM D.2.3 7/2 requires a yield, even if delay is in the past or 0.
      --  Timed_Wait with argument 0 adds a dispatching point.

      pragma Assert (Res = 0);
   end Delay_Until;

   -----------------
   --  Initialize --
   -----------------

   procedure Initialize
     (Environment_Thread : Thread_Id;
      Main_Priority      : System.Any_Priority)
   is
      Pid : Integer;
      Res : Integer;

   begin
      Get_My_Id (Pid, Res);
      pragma Assert (Res = 0);

      --  This procecure must be called by the environment task (Id = 1)

      pragma Assert (Pid = 1);

      Environment_Thread.Process_Id := Pid;
      Environment_Thread.Base_Priority := Main_Priority;
      All_Threads (Pid) := Environment_Thread;

      Set_Priority (Main_Priority);
   end Initialize;

   ----------------------------------
   -- Initialize_Slave_Environment --
   ----------------------------------

   procedure Initialize_Slave_Environment (Environment_Thread : Thread_Id) is
      pragma Unreferenced (Environment_Thread);

   begin
      --  Multiprocessors are not supported on this target

      null;
   end Initialize_Slave_Environment;

   ---------------------
   --  Thread_Create  --
   ---------------------

   procedure Thread_Create
     (Id            : Thread_Id;
      Code          : System.Address;
      Arg           : System.Address;
      Priority      : System.Any_Priority;
      Base_CPU      : System.Multiprocessors.CPU_Range;
      Stack_Address : System.Address;
      Stack_Size    : System.Parameters.Size_Type)
   is
      pragma Unreferenced (Stack_Address, Base_CPU);

      function Get_Name return Name_Type;
      --  Function to generate name from the argument (which is the Task_Id)

      function Get_Name return Name_Type is
         Name     : Name_Type := (others => ASCII.NUL);
         Position : Name_Range := Name_Range'First;

         procedure Set_Digit (Number : Natural);
         --  Procedure to translate the Task_Id into an array of characters

         ---------------
         -- Set_Digit --
         ---------------

         procedure Set_Digit (Number : Natural) is
         begin
            if Number >= 10 then
               Set_Digit (Number / 10);
            end if;

            Name (Position) := Character'Val (48 + (Number rem 10));
            Position := Position + 1;
         end Set_Digit;

      --  Start of processing for Get_Name

      begin
         Set_Digit (Natural (System.Storage_Elements.To_Integer (Arg)));
         return Name;
      end Get_Name;

      Name : constant Name_Type := Get_Name;

      Attr : aliased Process_Attribute_Type :=
        (Name          => Name,
         Entry_Point   => APEX_Task_Wrapper'Address,
         Stack_Size    => Integer (Stack_Size),
         Base_Priority => Priority,
         Period        => Infinite_Time,
         Time_Capacity => Infinite_Time,
         Deadline      => 0);

      Pid : Integer;
      --  APEX Id

      Res : Integer;

   begin
      --  Create the APEX process with the attributes previously defined

      Create_Process (Attr'Access, Pid, Res);
      pragma Assert (Res = 0);

      pragma Assert (Pid < Max_Thread_Num);

      All_Threads (Pid) := Id;
      Id.Base_Priority := Priority;
      Id.Process_Id := Pid;
      Id.Code := Code;
      Id.Arg := Arg;

      Start (Pid, Res);
      pragma Assert (Res = 0);
   end Thread_Create;

   -------------------
   --  Thread_Self  --
   -------------------

   function Thread_Self return Thread_Id is
      Pid : Integer;
      Res : Integer;
   begin
      Get_My_Id (Pid, Res);
      pragma Assert (Res = 0);
      return All_Threads (Pid);
   end Thread_Self;

   ----------------
   --  Lwp_Self  --
   ----------------

   function Lwp_Self return System.Address is
      Pid : Integer;
      Res : Integer;

   begin
      Get_My_Id (Pid, Res);
      pragma Assert (Res = 0);

      --  This magic value matches the tid returned by gdbstub

      return Address (Pid);
   end Lwp_Self;

   ----------------
   --  Set_ATCB  --
   ----------------

   procedure Set_ATCB (ATCB : System.Address) is
      Pid : Integer;
      Res : Integer;
   begin
      Get_My_Id (Pid, Res);
      pragma Assert (Res = 0);
      All_Threads (Pid).ATCB := ATCB;
   end Set_ATCB;

   ----------------
   --  Get_ATCB  --
   ----------------

   function Get_ATCB return System.Address is
      Pid : Integer;
      Res : Integer;
   begin
      Get_My_Id (Pid, Res);
      pragma Assert (Res = 0);
      return All_Threads (Pid).ATCB;
   end Get_ATCB;

   --------------------
   --  Set_Priority  --
   --------------------

   procedure Set_Priority (Priority : System.Any_Priority) is
      Pid : Integer;
      Res : Integer;
   begin
      Get_My_Id (Pid, Res);
      pragma Assert (Res = 0);
      Set_Priority (Pid, Priority, Res);
      pragma Assert (Res = 0);
   end Set_Priority;

   --------------------
   --  Get_Priority  --
   --------------------

   function Get_Priority (Id : Thread_Id) return System.Any_Priority is
      Res : Integer;
      Status : Process_Status_Type;
   begin
      Get_Process_Status (Id.Process_Id, Status, Res);
      pragma Assert (Res = 0);
      return Any_Priority (Status.Current_Priority);
   end Get_Priority;

   -------------
   --  Sleep  --
   -------------

   procedure Sleep is
      Res : Integer;
   begin
      Suspend_Self (-1, Res);
      pragma Assert (Res = 0);
   end Sleep;

   --------------
   --  Wakeup  --
   --------------

   procedure Wakeup (Id : Thread_Id) is
      Res : Integer;
   begin
      Resume (Id.Process_Id, Res);
      pragma Assert (Res = 0);
   end Wakeup;

   --------------------
   --  Get_Affinity  --
   --------------------

   function Get_Affinity (Id : Thread_Id) return Multiprocessors.CPU_Range is
      pragma Unreferenced (Id);

   begin
      --  No multiprocessor support, always return Not_A_Specific_CPU

      return Multiprocessors.Not_A_Specific_CPU;
   end Get_Affinity;

   ---------------
   --  Get_CPU  --
   ---------------

   function Get_CPU  (Id : Thread_Id) return Multiprocessors.CPU is
      pragma Unreferenced (Id);

   begin
      --  No multiprocessor support, always return the first CPU Id

      return Multiprocessors.CPU'First;
   end Get_CPU;

end System.OS_Interface;
