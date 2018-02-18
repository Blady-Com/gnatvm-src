------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                    S Y S T E M . O S _ I N T E R F A C E                 --
--                                                                          --
--                                   S p e c                                --
--                                                                          --
--                      Copyright (C) 2009-2010, AdaCore                    --
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

pragma Restrictions (No_Elaboration_Code);

with System.Parameters;
with System.Multiprocessors;

package System.OS_Interface is
   pragma Preelaborate;

   ----------------
   -- Interrupts --
   ----------------

   Max_Interrupt : constant := 2;
   --  ??? Number of asynchronous interrupts (not implemented yet)

   subtype Interrupt_ID is Integer;
   --  Interrupt identifiers

   No_Interrupt : constant := 0;
   --  ??? Special value indicating no interrupt (not implemented yet)

   type Interrupt_Handler is access procedure (Id : Interrupt_ID);

   --------------------------
   -- Interrupt processing --
   --------------------------

   function Current_Interrupt return Interrupt_ID;
   --  Function that returns the hardware interrupt currently being handled (if
   --  any). In case no hardware interrupt is being handled the returned value
   --  is No_Interrupt.

   procedure Attach_Handler
     (Handler : Interrupt_Handler;
      Id      : Interrupt_ID);
   --  Attach a handler to a hardware interrupt

   ----------
   -- Time --
   ----------

   type Time is range -1 .. (2 ** 63) - 1;
   for Time'Size use 64;
   --  Representation of the time in ARINC 653. Any negative value represents
   --  infinite time.

   Infinite_Time : constant Time := -1;
   --  Standard representation of infinite time for APEX

   type Time_Span is range -2 ** 63 .. 2 ** 63 - 1;
   for Time_Span'Size use 64;
   --  Represents length of time intervals in ARINC 653

   Ticks_Per_Second : constant Natural := 1_000_000_000;
   --  Number of clock ticks per second (ARINC 653 resolution is 1ns)

   function Clock return Time;
   --  Get the number of ticks elapsed since startup

   procedure Delay_Until (T : Time);
   --  Suspend the calling task until the absolute time specified by T

   -------------
   -- Threads --
   -------------

   type Thread_Descriptor is private;
   --  Type that contains information about a thread (Id, code to execute, ...)

   type Thread_Id is access all Thread_Descriptor;
   --  Identifiers for the underlying threads

   Null_Thread_Id : constant Thread_Id := null;
   --  Identifier for a non valid thread

   procedure Initialize
     (Environment_Thread : Thread_Id;
      Main_Priority      : System.Any_Priority);
   --  Procedure for initializing the underlying tasking system

   procedure Initialize_Slave_Environment (Environment_Thread : Thread_Id);
   --  Procedure to initialize the fake environment thread on a slave CPU.
   --  This thread is used to handle interrupt if the CPU doesn't have any
   --  other task. Multiprocessors are not supported on this target, but
   --  this declaration is for uniformity with other variants of s-osinte.

   procedure Thread_Create
     (Id            : Thread_Id;
      Code          : System.Address;
      Arg           : System.Address;
      Priority      : System.Any_Priority;
      Base_CPU      : System.Multiprocessors.CPU_Range;
      Stack_Address : System.Address;
      Stack_Size    : System.Parameters.Size_Type);
   --  Create a new thread

   function Thread_Self return Thread_Id;
   --  Return the thread identifier for the calling task

   function Lwp_Self return System.Address;
   --  Return the LWP for the calling task

   ----------
   -- ATCB --
   ----------

   procedure Set_ATCB (ATCB : System.Address);
   --  Associate the specified ATCB to the currently running thread

   function Get_ATCB return System.Address;
   --  Get the ATCB associated to the currently running thread

   ----------------
   -- Scheduling --
   ----------------

   procedure Set_Priority (Priority  : System.Any_Priority);
   --  Set the active priority of the executing thread to the given value

   function Get_Priority  (Id : Thread_Id) return System.Any_Priority;
   --  Get the current base priority of a thread

   procedure Sleep;
   --  The calling thread is unconditionally suspended

   procedure Wakeup (Id : Thread_Id);
   --  The referred thread becomes ready (the thread must be suspended)

   ---------------------
   -- Multiprocessors --
   ---------------------

   function Current_CPU return Multiprocessors.CPU;
   --  Return the id of the current CPU

   function Get_Affinity (Id : Thread_Id) return Multiprocessors.CPU_Range;
   --  Return CPU affinity of the given thread (maybe Not_A_Specific_CPU)

   function Get_CPU (Id : Thread_Id) return Multiprocessors.CPU;
   --  Return the CPU in charge of the given thread (always a valid CPU)

private
   type Thread_Descriptor is record
      ATCB : System.Address;
      --  Address of the Ada Task Control Block corresponding to the Ada task
      --  that executes on this thread.

      Code : System.Address;
      --  Address of the code to execute by the thread

      Arg : System.Address;
      --  Argument for the task wrapper

      Process_Id : Integer;
      --  APEX process identifier

      Base_Priority : System.Any_Priority;
      --  Base priority of the thread
   end record;
end System.OS_Interface;
