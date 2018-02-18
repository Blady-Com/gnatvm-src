------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . C P U _ S P E C I F I C                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2004 The European Space Agency            --
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

--  This package contains the primitives which are dependent on the
--  underlying processor.

pragma Restrictions (No_Elaboration_Code);

with System;
with Interfaces;

package System.BB.CPU_Specific is
   pragma Preelaborate;

   ------------------------
   -- Context management --
   ------------------------

   --  The context buffer is a type that represents thread's state and is not
   --  otherwise stored in main memory. This typically includes all user-
   --  visible registers, and possibly some other status as well.

   --  In case different contexts have different amounts of state (for example,
   --  due to absence of a floating-point unit in a particular configuration,
   --  or just the FPU not being used), it is expected that these details
   --  are handled in the implementation.

   type Context_Buffer is record

      --  Only callee-saved (ie dedicated and nonvolatile) registers need to
      --  be saved.

      R4  : System.Address;
      R5  : System.Address;
      R6  : Interfaces.Unsigned_32;
      R7  : Interfaces.Unsigned_32;
      R8  : Interfaces.Unsigned_32;
      R9  : Interfaces.Unsigned_32;
      R10 : Interfaces.Unsigned_32;
      R11 : Interfaces.Unsigned_32;
      LR  : System.Address;
      SP  : System.Address;
      PSR : Interfaces.Unsigned_32;
   end record;

   Stack_Alignment : constant := 8;
   --  Stack alignment defined by the ABI

   --------------------
   -- Initialization --
   --------------------

   procedure Initialize_CPU;
   --  Initialize the CPU

   ---------------------------------
   -- Interrupt and Trap Handling --
   ---------------------------------

   type Vector_Id is range 0 .. 59;
   --  This corresponds to the interrupt number

   Systick_Interrupt : constant Vector_Id := 16#0f#;

   procedure Install_Exception_Handler
     (Service_Routine : System.Address;
      Vector          : Vector_Id);
   --  Install a new handler in the exception table

end System.BB.CPU_Specific;
