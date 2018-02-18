------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                  S Y S T E M . B B . P E R I P H E R A L S               --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2013, AdaCore                     --
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
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

with System.Machine_Code;

with Interfaces; use Interfaces;

package body System.BB.Board_Support is

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      --  Mask interrupts

      Set_Current_Priority (Interrupt_Priority'Last - 1);
   end Initialize_Board;

   ----------------------
   -- Ticks_Per_Second --
   ----------------------

   function Ticks_Per_Second return Natural is
   begin
      return 50; -- System.BB.Board_Parameters.Clock_Frequency;
   end Ticks_Per_Second;

   ---------------------------
   -- Clear_Alarm_Interrupt --
   ---------------------------

   procedure Clear_Alarm_Interrupt is
   begin
      null;
   end Clear_Alarm_Interrupt;

   ---------------------------
   -- Install_Alarm_Handler --
   ---------------------------

   procedure Install_Alarm_Handler
     (Handler : System.BB.Interrupts.Interrupt_Handler)
   is
      STRELOAD : Natural;
      for STRELOAD'Address use System'To_Address (16#e000_e014#);
      pragma Volatile (STRELOAD);
      pragma Import (Ada, STRELOAD);

      STCTRL : Unsigned_32;
      for STCTRL'Address use System'To_Address (16#e000_e010#);
      pragma Volatile (STCTRL);
      pragma Import (Ada, STCTRL);
   begin
      CPU_Specific.Install_Exception_Handler
        (Handler.all'Address, CPU_Specific.Systick_Interrupt);

      STRELOAD := 50_000_000 / Ticks_Per_Second;

      --  System clock, enable interrupts, enable counter

      STCTRL := STCTRL or 7;
   end Install_Alarm_Handler;

   ---------------------------
   -- Get_Interrupt_Request --
   ---------------------------

   function Get_Interrupt_Request
     (Vector : CPU_Specific.Vector_Id)
     return System.BB.Interrupts.Interrupt_ID is
      pragma Unreferenced (Vector);
      use System.Machine_Code;

      Res : Unsigned_32;
   begin
      Asm ("mrs %0, ipsr",
           Unsigned_32'Asm_Output ("=r", Res),
           Volatile => True);
      return System.BB.Interrupts.Interrupt_ID (Res and 16#3f#);
   end Get_Interrupt_Request;

   -------------------------------
   -- Install_Interrupt_Handler --
   -------------------------------

   procedure Install_Interrupt_Handler
     (Handler   : Address;
      Interrupt : Interrupts.Interrupt_ID)
   is
   begin
      CPU_Specific.Install_Exception_Handler
        (Handler, CPU_Specific.Vector_Id (Interrupt));
   end Install_Interrupt_Handler;

   ---------------------------
   -- Priority_Of_Interrupt --
   ---------------------------

   function Priority_Of_Interrupt
     (Interrupt : System.BB.Interrupts.Interrupt_ID)
      return System.Any_Priority
   is
   begin
      raise Program_Error;
      return Interrupt_Priority'First;
   end Priority_Of_Interrupt;

   -----------------------------
   -- Clear_Interrupt_Request --
   -----------------------------

   procedure Clear_Interrupt_Request
     (Interrupt : System.BB.Interrupts.Interrupt_ID)
   is
   begin
      null;
   end Clear_Interrupt_Request;

   --------------------------
   -- Set_Current_Priority --
   --------------------------

   procedure Set_Current_Priority (Priority : Any_Priority) is
      use System.Machine_Code;
      Basepri : Natural;
   begin
      --  Note that Priority cannot be the last one, as this procedure is
      --  unable to disable the decrementer interrupt.

      pragma Assert (Priority /= Interrupt_Priority'Last);

      if Priority in Interrupt_Priority then
         Basepri := Interrupt_Priority'Last - Priority;
      else
         Basepri := 0;
      end if;

      Asm ("msr BASEPRI, %0",
           Inputs => Natural'Asm_Input ("r", Basepri * 32),
           Volatile => True);
   end Set_Current_Priority;

end System.BB.Board_Support;
