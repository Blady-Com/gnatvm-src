------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          G N A T . I O . P U T                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2012, AdaCore                        --
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

--  LEON3 version of text output routine

with System;

separate (GNAT.IO)
procedure Put (C : Character) is

   --  Minimal version of output routine to output to a APBUART device. A
   --  description of the UART is in the GRLIB IP Core User’s Manual. The
   --  address is hardcoded based on default template designs and the default
   --  configuration of various simulators.

   type Register is mod 2**32;

   UART_Address : constant := 16#8000_0100#;

   UART_Data    : Register; --  Write this register to transmit data
   UART_Status  : Register; --  Check this register for transmitter status

   for UART_Data'Address   use System'To_Address (UART_Address + 0);
   for UART_Status'Address use System'To_Address (UART_Address + 4);

   pragma Volatile (UART_Data);
   pragma Volatile (UART_Status);

   Transmitter_Empty_Bit : constant Register := 2**2;
   --  When this bit is set, another character can be queued for transmission

begin
   --  Busy-wait while the transmitter register is not empty

   while (UART_Status and Transmitter_Empty_Bit) = 0 loop
      null;
   end loop;

   UART_Data := Character'Pos (C);
end Put;
