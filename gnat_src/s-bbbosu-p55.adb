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

--  Board support for mpc5566

with System.BB.Board_Parameters;
with System.Machine_Code;

with Interfaces; use Interfaces;

package body System.BB.Board_Support is

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
      INTC_MCR : Unsigned_32;
      for INTC_MCR'Address use System'To_Address (16#fff4_8000#);
      pragma Volatile (INTC_MCR);
      pragma Import (Ada, INTC_MCR);
   begin
      --  Initialize the INTC

      INTC_MCR := 0; -- VTES=0, HVEN=0

      --  Mask interrupts

      Set_Current_Priority (Interrupt_Priority'Last - 1);
   end Initialize_Board;

   ----------------------
   -- Ticks_Per_Second --
   ----------------------

   function Ticks_Per_Second return Natural is
   begin
      return System.BB.Board_Parameters.Clock_Frequency;
   end Ticks_Per_Second;

   ---------------------------
   -- Clear_Alarm_Interrupt --
   ---------------------------

   procedure Clear_Alarm_Interrupt is
      use System.Machine_Code;
   begin
      --  Clear TSR[DIS]

      Asm ("mtspr 336,%0",
           Inputs => Unsigned_32'Asm_Input ("r", 2 ** (63 - 36)),
           Volatile => True);
   end Clear_Alarm_Interrupt;

   ---------------------------
   -- Install_Alarm_Handler --
   ---------------------------

   --  Not used on PowerPc

   procedure Install_Alarm_Handler
     (Handler : System.BB.Interrupts.Interrupt_Handler)
   is
   begin
      null;
   end Install_Alarm_Handler;

   ---------------------------
   -- Get_Interrupt_Request --
   ---------------------------

   function Get_Interrupt_Request
     (Vector : CPU_Specific.Vector_Id)
      return System.BB.Interrupts.Interrupt_ID is
      pragma Unreferenced (Vector);

      INTC_IACKR : Unsigned_32;
      for INTC_IACKR'Address use System'To_Address (16#fff4_8010#);
      pragma Volatile (INTC_IACKR);
      pragma Import (Ada, INTC_IACKR);
      --  Interrupt acknowledge register

   begin
      return System.BB.Interrupts.Interrupt_ID ((INTC_IACKR and 16#7fc#) / 4);
   end Get_Interrupt_Request;

   -------------------------------
   -- Install_Interrupt_Handler --
   -------------------------------

   procedure Install_Interrupt_Handler
     (Handler   : Address;
      Interrupt : Interrupts.Interrupt_ID)
   is
      pragma Unreferenced (Interrupt);
   begin
      CPU_Specific.Install_Exception_Handler
        (Handler, CPU_Specific.External_Interrupt_Excp);
   end Install_Interrupt_Handler;

   ---------------------------
   -- Priority_Of_Interrupt --
   ---------------------------

   function Priority_Of_Interrupt
     (Interrupt : System.BB.Interrupts.Interrupt_ID)
      return System.Any_Priority
   is
      type Intc_Psr_Type is
        array (System.BB.Interrupts.Interrupt_ID) of Unsigned_8;
      INTC_PSR : Intc_Psr_Type;
      for INTC_PSR'Address use System'To_Address (16#fff4_8040#);
      pragma Volatile (INTC_PSR);
      pragma Import (Ada, INTC_PSR);
   begin
      return Interrupt_Priority'First +
        Natural (INTC_PSR (Interrupt) and 16#0f#);
   end Priority_Of_Interrupt;

   -----------------------------
   -- Clear_Interrupt_Request --
   -----------------------------

   procedure Clear_Interrupt_Request
     (Interrupt : System.BB.Interrupts.Interrupt_ID)
   is
      pragma Unreferenced (Interrupt);

      INTC_EOIR : Unsigned_32;
      for INTC_EOIR'Address use System'To_Address (16#fff4_8018#);
      pragma Volatile (INTC_EOIR);
      pragma Import (Ada, INTC_EOIR);
      --  INTC end of interrupt register
   begin
      INTC_EOIR := 0;
   end Clear_Interrupt_Request;

   --------------------------
   -- Set_Current_Priority --
   --------------------------

   procedure Set_Current_Priority (Priority : Any_Priority) is
      INTC_CPR : Unsigned_32;
      for INTC_CPR'Address use System'To_Address (16#fff4_8008#);
      pragma Volatile (INTC_CPR);
      pragma Import (Ada, INTC_CPR);
      --  INTC current priority register

   begin
      --  Note that Priority cannot be the last one, as this procedure is
      --  unable to disable the decrementer interrupt.

      pragma Assert (Priority /= Interrupt_Priority'Last);

      if Priority in Interrupt_Priority then
         INTC_CPR := Unsigned_32 (Priority - Interrupt_Priority'First);
      else
         INTC_CPR := 0;
      end if;
   end Set_Current_Priority;

end System.BB.Board_Support;
