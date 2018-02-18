------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ S P E C I F I C                --
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
------------------------------------------------------------------------------

--  This package implements PowerPC architecture specific support for the GNAT
--  Ravenscar run time.

with System.BB.Interrupts;
with System.Machine_Code;
with System.Multiprocessors;

package body System.BB.CPU_Specific is

   type Exception_Handler_Array is array (Vector_Id) of Address;

   procedure Unhandled_Exception;

   Exception_Handlers : Exception_Handler_Array :=
                          (others => Unhandled_Exception'Address);
   pragma Export (C, Exception_Handlers, "__gnat_cortexm3_exception_handlers");

   Exception_Vectors : Address;
   pragma Import (Asm, Exception_Vectors, "__vectors");

   --------------------
   -- Initialize_CPU --
   --------------------

   procedure Initialize_CPU is
      use System.Machine_Code;
      use Interfaces;

      VTABLE : System.Address;
      for VTABLE'Address use System'To_Address (16#e000_ed08#);
      pragma Volatile (VTABLE);
      pragma Import (Ada, VTABLE);

      CFGCTRL : System.Address;
      for CFGCTRL'Address use System'To_Address (16#e000_ed14#);
      pragma Volatile (CFGCTRL);
      pragma Import (Ada, CFGCTRL);

      SYSHNDCTRL : System.Address;
      for SYSHNDCTRL'Address use System'To_Address (16#e000_ed24#);
      pragma Volatile (SYSHNDCTRL);
      pragma Import (Ada, SYSHNDCTRL);

      Interrupt_Stack_Table : array (System.Multiprocessors.CPU)
        of System.Address;
      pragma Import (Asm, Interrupt_Stack_Table, "interrupt_stack_table");
      --  Table that contains a pointer to the top of the stack for each
      --  processor

   begin
      --  Switch to PSP

      Asm ("mrs r0, MSP" & ASCII.LF & ASCII.HT &
           "msr PSP, r0" & ASCII.LF & ASCII.HT &
           "mrs r0, CONTROL" & ASCII.LF & ASCII.HT &
           "orr r0,r0,2" & ASCII.LF & ASCII.HT &
           "msr CONTROL,r0",
           Clobber => "r0",
           Volatile => True);

      --  Initialize MSP

      Asm ("msr MSP, %0",
           Inputs => Address'Asm_Input ("r", Interrupt_Stack_Table (1)),
           Volatile => True);

      --  Initiablize vector table

      VTABLE := Exception_Vectors'Address;

      --  Set configuration: stack is 8 byte aligned, trap on divide by 0,
      --  no trap on unaligned access, can enter thread mode from any level.

      CFGCTRL := CFGCTRL or 16#211#;

      --  Enable usage, bus and memory management fault

      SYSHNDCTRL := SYSHNDCTRL or 16#7_000#;

      --  Unmask Fault

      Asm ("cpsie f", Volatile => True);
   end Initialize_CPU;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Exception_Handler
     (Service_Routine : System.Address;
      Vector          : Vector_Id)
   is
   begin
      Exception_Handlers (Vector) := Service_Routine;
   end Install_Exception_Handler;

   -------------------------
   -- Unhandled_Exception --
   -------------------------

   procedure Unhandled_Exception is
   begin
      null;
   end Unhandled_Exception;

end System.BB.CPU_Specific;
