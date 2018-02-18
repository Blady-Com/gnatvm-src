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
--                     Copyright (C) 2003-2012, AdaCore                     --
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

package body System.BB.CPU_Specific is

   type Exception_Handler_Array is array (Vector_Id) of Address;

   procedure Unhandled_Exception;

   Exception_Handlers : Exception_Handler_Array :=
                          (others => Unhandled_Exception'Address);
   pragma Export (C, Exception_Handlers, "__gnat_powerpc_exception_handlers");

   --------------------
   -- Initialize_CPU --
   --------------------

   procedure Initialize_CPU is
      use System.Machine_Code;
      use Interfaces;

      Addr : Address;
      --  Interrupt vector table prefix

      DIE : constant Unsigned_32 := 16#0400_0000#;
      --  Decrementer interrupt enable

   begin
      --  Set TCR

      Asm ("mtspr 340, %0",
           Inputs => Unsigned_32'Asm_Input ("r", DIE),
           Volatile => True);

      --  Set IVPR

      Asm ("lis %0,handler_0@h",
           Outputs => Address'Asm_Output ("=r", Addr),
           Volatile => True);
      Asm ("mtspr 63, %0",
           Inputs => Address'Asm_Input ("r", Addr),
           Volatile => True);

      --  Set IVOR10 (decrementer)

      Asm ("li %0,handler_10@l",
           Outputs => Address'Asm_Output ("=r", Addr),
           Volatile => True);
      Asm ("mtspr 410, %0",
           Inputs => Address'Asm_Input ("r", Addr),
           Volatile => True);

      --  Set IVOR4 (External interrupt)

      Asm ("li %0,handler_4@l",
           Outputs => Address'Asm_Output ("=r", Addr),
           Volatile => True);
      Asm ("mtspr 404, %0",
           Inputs => Address'Asm_Input ("r", Addr),
           Volatile => True);

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
