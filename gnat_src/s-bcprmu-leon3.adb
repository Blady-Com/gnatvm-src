------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--              S Y S T E M . B B . C P U _ P R I M I T I V E S .           --
--                       M U L T I P R O C E S S O R S                      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 2010-2013, AdaCore                     --
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

pragma Restrictions (No_Elaboration_Code);

with System.Machine_Code;
with System.BB.Time;
with System.BB.Timing_Events;
with System.BB.Board_Support;
with System.BB.Board_Support.LEON_3;
with System.BB.Interrupts;
with System.BB.Threads;
with System.BB.Threads.Queues;
with System.BB.Protection;
with Interfaces;

package body System.BB.CPU_Primitives.Multiprocessors is

   use System.Machine_Code;
   use Board_Support;
   use Board_Support.LEON_3;
   use Interfaces;
   use System.Multiprocessors;

   procedure Poke_Handler (Interrupt : Interrupts.Interrupt_ID);
   --  Handler for the Poke interrupt

   function ASR_17 return Unsigned_32;
   --  Return current value of the ASR 17 register

   ------------
   -- ASR_17 --
   ------------

   function ASR_17 return Unsigned_32 is
      R : Unsigned_32;
   begin
      Asm ("mov %%asr17, %0" & ASCII.LF & ASCII.HT,
           Outputs => Unsigned_32'Asm_Output ("=r", R),
           Volatile => True);
      return R;
   end ASR_17;

   -----------------
   -- Current_CPU --
   -----------------

   function Current_CPU return CPU is

      --  Get CPU Id from bits 31-28 of the Asr17 register

     (if CPU'Last = 1 then 1 else CPU (Shift_Right (ASR_17, 28) + 1));

   --------------
   -- Poke_CPU --
   --------------

   procedure Poke_CPU (CPU_Id : CPU) is
   begin
      --  There is no need to protect access to the register since the only
      --  operation applied to it is this assignment and it's always with the
      --  same value (2**Poke_Interrupt_ID).

      --  No race condition possible here.

      Interrupt_Force (CPU_Id) :=
        Interrupt_Force (CPU_Id) or 2**Poke_Interrupt_ID;
   end Poke_CPU;

   ---------------
   -- Start_CPU --
   ---------------

   procedure Start_CPU (CPU_Id : CPU) is
   begin
      --  Set bit n in Status Register to start CPU n

      Multiprocessor_Status.Status (CPU'Pos (CPU_Id) - 1) := True;
   end Start_CPU;

   --------------------
   -- Start_All_CPUs --
   --------------------

   procedure Start_All_CPUs is
   begin
      Interrupts.Attach_Handler (Poke_Handler'Access, Poke_Interrupt_ID);

      for CPU_Id in CPU loop
         Start_CPU (CPU_Id);
      end loop;
   end Start_All_CPUs;

   ------------------
   -- Poke_Handler --
   ------------------

   procedure Poke_Handler (Interrupt : Interrupts.Interrupt_ID) is
      use type Threads.Thread_States;

      Now : Time.Time;

   begin
      --  Make sure we are handling the right interrupt

      pragma Assert (Interrupt = Board_Support.Poke_Interrupt_ID);

      Board_Support.Clear_Poke_Interrupt;

      --  The access to the queues must be protected

      Protection.Enter_Kernel;

      Now := Time.Clock;

      --  Execute expired events of the current CPU

      Timing_Events.Execute_Expired_Timing_Events (Now);

      --  Wake up alarms

      Threads.Queues.Wakeup_Expired_Alarms (Now);

      Protection.Leave_Kernel;
   end Poke_Handler;

end System.BB.CPU_Primitives.Multiprocessors;
