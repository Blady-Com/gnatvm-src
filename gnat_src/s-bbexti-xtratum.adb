------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--              S Y S T E M . B B . E X E C U T I O N _ T I M E             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2011-2013, AdaCore                     --
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
------------------------------------------------------------------------------

--  Ravenscar version of this package for XtratuM

with System.BB.Parameters;
with System.BB.Board_Support;
with System.BB.Threads.Queues;

with System.OS_Interface;
with Interfaces.C;

package body System.BB.Execution_Time is

   use type System.BB.Time.Time;
   use System.BB.Interrupts;
   use System.BB.Threads;

   -----------------------
   -- Local Definitions --
   -----------------------

   Interrupts_Execution_Time : array (Interrupt_ID) of System.BB.Time.Time :=
     (others => System.BB.Time.Time'First);
   --  Time counter for each interrupt

   CPU_Clock : System.BB.Time.Time := System.BB.Time.Time'First;
   --  Date of the last Interrupt

   Disabled : Boolean := False;
   --  If the CPU clock is disabled the next elapsed time will be discarded (to
   --  handle CPU idle time). At the start, the CPU clock is enabled.

   function Read_Execution_Clock return System.BB.Time.Time;
   --  Get the execution clock which counts only the time when the partition is
   --  active. This is different from Clock when we are on top of a partitioned
   --  system (like in this case).

   procedure Disable_Execution_Time;
   --  Disable the CPU clock of the current processor. The clock remains
   --  disabled until the next call to Scheduling_Event.

   procedure Scheduling_Event;
   --  Assign elapsed time to the executing Task/Interrupt and reset CPU clock.
   --  If the clock is disabled, the elapsed time is discarded and the clock
   --  re-enabled.
   --
   --  The Scheduling_Event procedure must be called at the end of an execution
   --  period:
   --    When the run-time switches from:
   --      a task to another task
   --      a task to an interrupt
   --      an interrupt to a task
   --      an interrupt to another interrupt
   --    and before idle loop.

   ----------------------------
   -- Disable_Execution_Time --
   ----------------------------

   procedure Disable_Execution_Time is
   begin
      Disabled := True;
   end Disable_Execution_Time;

   ----------------------------
   -- Global_Interrupt_Clock --
   ----------------------------

   function Global_Interrupt_Clock return System.BB.Time.Time is
      Sum : System.BB.Time.Time := System.BB.Time.Time'First;

   begin
      for Interrupt in Interrupt_ID loop
         Sum := Sum + Interrupts_Execution_Time (Interrupt);
      end loop;

      return Sum;
   end Global_Interrupt_Clock;

   ---------------------
   -- Interrupt_Clock --
   ---------------------

   function Interrupt_Clock
     (Interrupt : Interrupt_ID) return System.BB.Time.Time is
   begin
      return Interrupts_Execution_Time (Interrupt);
   end Interrupt_Clock;

   ----------------------
   -- Scheduling_Event --
   ----------------------

   procedure Scheduling_Event is
      Now            : constant System.BB.Time.Time := Read_Execution_Clock;
      Last_CPU_Clock : constant System.BB.Time.Time := CPU_Clock;
      Elapsed_Time   : System.BB.Time.Time;

   begin
      pragma Assert (Now >= Last_CPU_Clock);

      Elapsed_Time := Now - Last_CPU_Clock;

      --  Reset the clock

      CPU_Clock := Now;

      if Disabled then
         --  Discard the elapsed time and re-enable the clock

         Disabled := False;
         return;
      end if;

      --  Case where this CPU is currently executing an interrupt

      if Current_Interrupt /= No_Interrupt then
         Interrupts_Execution_Time (Current_Interrupt) :=
           Interrupts_Execution_Time (Current_Interrupt) + Elapsed_Time;

      --  Case where this CPU is currently executing a task

      else
         Thread_Self.Execution_Time :=
           Thread_Self.Execution_Time + Elapsed_Time;
      end if;
   end Scheduling_Event;

   ------------------
   -- Thread_Clock --
   ------------------

   function Thread_Clock
     (Th : System.BB.Threads.Thread_Id) return System.BB.Time.Time
   is
   begin
      pragma Assert (Th /= Null_Thread_Id);

      --  If the thread Th is running, we need to add the elapsed time between
      --  the last scheduling and now.

      --  The thread Th is running if it is the current one and:
      --    * no interrupt is executed
      --    * or the interrupt triggered this thread.

      if Th = Thread_Self and then Current_Interrupt = No_Interrupt then

         --  As the task is running, the execution time must not be disabled

         pragma Assert (not Disabled);

         declare
            Now          : constant BB.Time.Time := Read_Execution_Clock;
            Elapsed_Time : constant BB.Time.Time := Now - CPU_Clock;
         begin
            return Th.Execution_Time + Elapsed_Time;
         end;

      else
         return Th.Execution_Time;
      end if;
   end Thread_Clock;

   --------------------------
   -- Read_Execution_Clock --
   --------------------------

   function Read_Execution_Clock return System.BB.Time.Time is
      XM_EXEC_CLOCK : constant := 1;
      --  Execution-time clock

      type XM_Time_T is range -2 ** 63 .. 2 ** 63 - 1;
      for XM_Time_T'Size use 64;
      --  Time in XtratuM

      procedure Get_Time
        (Clock_Id : Interfaces.C.unsigned;
         Time     : access XM_Time_T);
      pragma Import (C, Get_Time, "XM_get_time");
      --  Read clock

      XtratuM_Time : aliased XM_Time_T;

   begin
      --  Get the execution time and not the wall clock time because we need to
      --  take into account only the time when the partition is active.

      Get_Time (XM_EXEC_CLOCK, XtratuM_Time'Access);

      return System.BB.Time.Time (XtratuM_Time);
   end Read_Execution_Clock;

--  Elaboration for package System.BB.Execution_Time

begin
   --  Set hooks to enable computation

   System.BB.Time.Scheduling_Event_Hook       := Scheduling_Event'Access;
   System.BB.Time.Disable_Execution_Time_Hook := Disable_Execution_Time'Access;

   --  Initialize CPU_Clock

   CPU_Clock := Read_Execution_Clock;
end System.BB.Execution_Time;
