------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                S Y S T E M . B B . B O A R D _ S U P P O R T             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
--                     Copyright (C) 2003-2013, AdaCore                     --
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
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);

with System.BB.Board_Support.LEON; use System.BB.Board_Support.LEON;
with System.BB.Parameters;

package body System.BB.Board_Support is

   use CPU_Primitives;

   -----------------------
   -- Local Definitions --
   -----------------------

   Prescaler_Min : constant := 3;
   --  In order to obtain the highest granularity of the clock we set the
   --  minimum allowed prescaler division factor, which is 4, corresponding
   --  to a prescaler reload register value of 3.

   Periodic_Count : constant := 2**24 - 2;
   --  Value to be loaded in the clock counter to accomplish the
   --  Clock_Interrupt_Period.
   --
   --  One is subtracted from Timers_Counter'Last (2**24 -1) because the
   --  timeout period will count an extra cycle for reloading the counter.

   --  Constants defining the external interrupts

   General_Purpose_Timer : constant := 8;

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Initialize_Memory;
   pragma Inline (Initialize_Memory);
   --  Initialize the memory on the board

   procedure Initialize_Clock;
   --  Perform all the initialization related to the clock

   ------------------------
   -- Alarm_Interrupt_ID --
   ------------------------

   function Alarm_Interrupt_ID return Interrupts.Interrupt_ID is
   begin
      return General_Purpose_Timer;
   end Alarm_Interrupt_ID;

   ---------------------------
   -- Clear_Alarm_Interrupt --
   ---------------------------

   procedure Clear_Alarm_Interrupt is
   begin
      --  Interrupts are cleared automatically when they are acknowledged

      null;
   end Clear_Alarm_Interrupt;

   -----------------------------
   -- Clear_Interrupt_Request --
   -----------------------------

   procedure Clear_Interrupt_Request (Interrupt : Interrupts.Interrupt_ID) is
   begin
      null;
   end Clear_Interrupt_Request;

   --------------------------
   -- Clear_Poke_Interrupt --
   --------------------------

   procedure Clear_Poke_Interrupt is
   begin
      --  No Poke interrupt available for leon

      raise Program_Error;
   end Clear_Poke_Interrupt;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      --  The initialization of the LEON board consists on initializing the
      --  memory, and initializing the clock in order to have the desired
      --  granularity and range.

      Initialize_Memory;
      Initialize_Clock;
   end Initialize_Board;

   ----------------------
   -- Initialize_Clock --
   ----------------------

   procedure Initialize_Clock is
      Prescaler : constant Prescaler_Register :=
                    (Value => Prescaler_Min, Reserved => (others => False));
      --  Minimum prescaler to be used to achieve best granularity

      Real_Time_Clock_Reload : constant Timer_Register :=
                                 (Timer_Value => Periodic_Count,
                                  Reserved    => (others => False));
      --  Periodic count to be used for the clock

      Real_Time_Clock_Control : constant Timer_Control_Register :=
                                  (Enable         => True,
                                   Reload_Counter => True,
                                   Load_Counter   => True,
                                   Reserved       => (others => False));
      --  Program the timer in periodic mode to serve as a clock

   begin
      --  Set the prescaler value to achieve the required granularity

      Prescaler_Reload := Prescaler;

      --  Load the counter for the real-time clock

      Timer_2_Reload := Real_Time_Clock_Reload;

      --  Enable Timer 2

      Timer_2_Control := Real_Time_Clock_Control;
   end Initialize_Clock;

   -----------------------
   -- Initialize_Memory --
   -----------------------

   procedure Initialize_Memory is
   begin
      --  Nothing to be done for LEON

      null;
   end Initialize_Memory;

   ------------------------
   -- Max_Timer_Interval --
   ------------------------

   function Max_Timer_Interval return Timer_Interval is
   begin
      return Periodic_Count;
   end Max_Timer_Interval;

   -----------------------
   -- Poke_Interrupt_ID --
   -----------------------

   function Poke_Interrupt_ID return Interrupts.Interrupt_ID is
   begin
      --  No Poke interrupt available for leon

      raise Program_Error;

      --  Unreachable code

      return Interrupts.Interrupt_ID'First;
   end Poke_Interrupt_ID;

   ---------------------------
   -- Priority_Of_Interrupt --
   ---------------------------

   function Priority_Of_Interrupt
     (Interrupt : System.BB.Interrupts.Interrupt_ID) return System.Any_Priority
   is
   begin
      --  Assert that it is a real interrupt

      pragma Assert (Interrupt /= System.BB.Interrupts.No_Interrupt);

      return (Any_Priority (Interrupt) + Interrupt_Priority'First - 1);
   end Priority_Of_Interrupt;

   ----------------
   -- Read_Clock --
   ----------------

   function Read_Clock return Timer_Interval is
   begin
      return Timer_Interval (Periodic_Count - Timer_2_Counter.Timer_Value);
   end Read_Clock;

   ---------------
   -- Set_Alarm --
   ---------------

   procedure Set_Alarm (Ticks : Timer_Interval) is
      Timer_Reload_Aux : constant Timer_Register :=
                           (Timer_Value => Timers_Counter (Ticks),
                            Reserved    => (others => False));
      --  Load the required ticks

      Timer_Control_Aux : constant Timer_Control_Register :=
                            (Enable         => True,
                             Reload_Counter => False,
                             Load_Counter   => True,
                             Reserved       => (others => False));
      --  Program the timer in one-shot mode

      Interrupt_Mask_Aux : Interrupt_Mask_and_Priority_Register;

   begin
      --  Alarm Clock downcount will reach 0 in Ticks. The granularity of
      --  time intervals is equal to Clock Period.

      --  Set the prescaler: already done in Initialize_Clock

      --  Load the counter

      Timer_1_Reload := Timer_Reload_Aux;

      --  Write Timer Control Register

      Timer_1_Control := Timer_Control_Aux;

      --   Enable Timer 1 Interrupts

      Interrupt_Mask_Aux := Interrupt_Mask_and_Priority;
      Interrupt_Mask_Aux.Timer_1 := True;
      Interrupt_Mask_and_Priority := Interrupt_Mask_Aux;
   end Set_Alarm;

   --------------------------
   -- Set_Current_Priority --
   --------------------------

   procedure Set_Current_Priority (Priority : Any_Priority) is
   begin
      null; --  No board-specific actions necessary
   end Set_Current_Priority;

   ----------------------
   -- Ticks_Per_Second --
   ----------------------

   function Ticks_Per_Second return Natural is
   begin
      --  The prescaler is clocked by the system clock. When it underflows, it
      --  is reloaded from the prescaler reload register and a timer tick is
      --  generated. The effective division rate is therefore equal to the
      --  prescaler reload register value plus 1.

      return Parameters.Clock_Frequency / (Prescaler_Min + 1);
   end Ticks_Per_Second;

   ---------------------------
   -- Get_Interrupt_Request --
   ---------------------------

   function Get_Interrupt_Request
     (Vector : CPU_Primitives.Vector_Id)
      return System.BB.Interrupts.Interrupt_ID
   is
   begin
      --  The range corresponding to asynchronous traps is 16#11# .. 16#1F#

      pragma Assert (Vector in 16#11# .. 16#1F#);

      return System.BB.Interrupts.Interrupt_ID (Vector - 16#10#);
   end Get_Interrupt_Request;

   -------------------------------
   -- Install_Interrupt_Handler --
   -------------------------------

   procedure Install_Interrupt_Handler
     (Handler   : Address;
      Interrupt : Interrupts.Interrupt_ID)
   is
   begin
      CPU_Primitives.Install_Trap_Handler
        (Handler, CPU_Primitives.Vector_Id (Interrupt + 16#10#));
   end Install_Interrupt_Handler;

end System.BB.Board_Support;
