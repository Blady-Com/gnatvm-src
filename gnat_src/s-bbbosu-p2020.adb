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

with System.BB.Board_Parameters;
with System.Machine_Code;

pragma Warnings (off);
--  Vectors and priorities are defined in Ada.Interrupts.Names, which is not
--  preelaborated. Ignore this issue as we only reference static consants.
with Ada.Interrupts.Names;
pragma Warnings (on);

with Interfaces; use Interfaces;

package body System.BB.Board_Support is

   procedure Set_Vpr
     (Offset    : Address;
      Interrupt : Ada.Interrupts.Interrupt_ID);
   --  Set a VPR register of the OpenPIC at address Offset. Enable interrupt

   -------------
   -- Set_Vpr --
   -------------

   procedure Set_Vpr
     (Offset    : Address;
      Interrupt : Ada.Interrupts.Interrupt_ID)
   is
      Vpr : Unsigned_32;
      for Vpr'Address use
        System'To_Address (System.BB.Board_Parameters.CCSRBAR + Offset);
      pragma Volatile (Vpr);
      pragma Import (Ada, Vpr);

      Priority : constant Interrupt_Priority :=
                   Priority_Of_Interrupt (Interrupts.Interrupt_ID (Interrupt));

   begin
      Vpr := Unsigned_32 (Priority - Interrupt_Priority'First) * 2 ** 16 +
               Unsigned_32 (Interrupt);
   end Set_Vpr;

   ----------------------
   -- Initialize_Board --
   ----------------------

   procedure Initialize_Board is
   begin
      --  Initialize the OpenPIC

      declare
         Svr : Unsigned_32;
         for Svr'Address use
           System'To_Address (System.BB.Board_Parameters.CCSRBAR + 16#4_10e0#);
         pragma Volatile (Svr);
         pragma Import (Ada, Svr);

      begin
         --  Set the spurious vector register as No_Interrupt (0), so that
         --  spurious interrupts can be easily discarded.

         Svr := 16#0000#;
      end;

      --  Enable IPI

      Set_Vpr (16#4_10a0#, Ada.Interrupts.Names.Interprocessor_Interrupt_0);
      Set_Vpr (16#4_10b0#, Ada.Interrupts.Names.Interprocessor_Interrupt_1);
      Set_Vpr (16#4_10c0#, Ada.Interrupts.Names.Interprocessor_Interrupt_2);
      Set_Vpr (16#4_10d0#, Ada.Interrupts.Names.Interprocessor_Interrupt_3);

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
      return System.BB.Interrupts.Interrupt_ID
   is
      pragma Unreferenced (Vector);

      Iack : Unsigned_32;
      for Iack'Address use
        System'To_Address (System.BB.Board_Parameters.CCSRBAR + 16#6_00a0#);
      pragma Volatile (Iack);
      pragma Import (Ada, Iack);
      --  Interrupt acknowledge register

   begin
      return System.BB.Interrupts.Interrupt_ID (Iack);
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
      use Ada.Interrupts.Names;

   begin
      case Interrupt is

         --  Spurious interrupts

         when System.BB.Interrupts.No_Interrupt =>
            return Interrupt_Priority'First;

         --  Handled interrupts

         when Interrupts.Interrupt_ID (Interprocessor_Interrupt_0) =>
            return Interprocessor_Interrupt_0_Priority;
         when Interrupts.Interrupt_ID (Interprocessor_Interrupt_1) =>
            return Interprocessor_Interrupt_1_Priority;
         when Interrupts.Interrupt_ID (Interprocessor_Interrupt_2) =>
            return Interprocessor_Interrupt_2_Priority;
         when Interrupts.Interrupt_ID (Interprocessor_Interrupt_3) =>
            return Interprocessor_Interrupt_3_Priority;

         --  Unhandled interrupts

         when others =>
            raise Program_Error;
      end case;
   end Priority_Of_Interrupt;

   -----------------------------
   -- Clear_Interrupt_Request --
   -----------------------------

   procedure Clear_Interrupt_Request
     (Interrupt : System.BB.Interrupts.Interrupt_ID)
   is
      pragma Unreferenced (Interrupt);
      EOI : Unsigned_32;
      for EOI'Address use
        System'To_Address (System.BB.Board_Parameters.CCSRBAR + 16#6_00b0#);
      pragma Volatile (EOI);
      pragma Import (Ada, EOI);
   begin
      EOI := 0;
   end Clear_Interrupt_Request;

   --------------------------
   -- Set_Current_Priority --
   --------------------------

   procedure Set_Current_Priority (Priority : Any_Priority) is
      CTPR : Unsigned_32;
      for CTPR'Address use
        System'To_Address (System.BB.Board_Parameters.CCSRBAR + 16#6_0080#);
      pragma Volatile (CTPR);
      pragma Import (Ada, CTPR);

   begin
      --  Note that Priority cannot be the last one, as this procedure is
      --  unable to disable the decrementer interrupt.

      pragma Assert (Priority /= Interrupt_Priority'Last);

      if Priority in Interrupt_Priority then
         CTPR := Unsigned_32 (Priority - Interrupt_Priority'First);
      else
         CTPR := 0;
      end if;
   end Set_Current_Priority;

end System.BB.Board_Support;
