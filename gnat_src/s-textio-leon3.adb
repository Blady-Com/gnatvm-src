------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . T E X T _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
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

--  Minimal version of Text_IO body for use on LEON3, writes to console

with System;

package body System.Text_IO is

   -----------
   -- Local --
   -----------

   type Reserved_24 is array (0 .. 23) of Boolean;
   for Reserved_24'Size use 24;
   pragma Pack (Reserved_24);

   UART_1_Data_Register_Address :
     constant System.Address := System'To_Address (16#8000_0100#);

   UART_1_Status_Register_Address :
     constant System.Address := System'To_Address (16#8000_0104#);

   type FIFO_Count is mod 64;
   for FIFO_Count'Size use 6;

   pragma Warnings (Off);
   type Parity_Kind is (Even, Odd);

   --  Mapping between bits in a 32-bit register as used in the hardware
   --  documentation and bit order as used by Ada. This makes it easier to
   --  verify correctness against the AUM. Ranges will need to be reversed,
   --  but the compiler will check this.

   Bit00 : constant := 31; Bit01 : constant := 30; Bit02 : constant := 29;
   Bit03 : constant := 28; Bit04 : constant := 27; Bit05 : constant := 26;
   Bit06 : constant := 25; Bit07 : constant := 24; Bit08 : constant := 23;
   Bit09 : constant := 22; Bit10 : constant := 21; Bit11 : constant := 20;
   Bit12 : constant := 19; Bit13 : constant := 18; Bit14 : constant := 17;
   Bit15 : constant := 16; Bit16 : constant := 15; Bit17 : constant := 14;
   Bit18 : constant := 13; Bit19 : constant := 12; Bit20 : constant := 11;
   Bit21 : constant := 10; Bit22 : constant := 09; Bit23 : constant := 08;
   Bit24 : constant :=  7; Bit25 : constant := 06; Bit26 : constant := 05;
   Bit27 : constant :=  4; Bit28 : constant := 03; Bit29 : constant := 02;
   Bit30 : constant :=  1; Bit31 : constant := 00;
   pragma Warnings (On);

   type UART_Data_Register is
      record
         FIFO  : Character;
         --  Reading and writing accesses receiver resp. transmitter FIFOs

         Reserved : Reserved_24;
         --  Not used
      end record;

   for UART_Data_Register use
      record
         Reserved at 0 range Bit31 .. Bit08;
         FIFO     at 0 range Bit07 .. Bit00;
      end record;

   for UART_Data_Register'Size use 32;

   pragma Suppress_Initialization (UART_Data_Register);

   type Reserved_9 is mod 2**9;
   for Reserved_9'Size use 9;

   type UART_Status_Register is
      record
         Data_Ready                       : Boolean;
         Transmitter_Shift_Register_Empty : Boolean;
         Transmitter_FIFO_Empty           : Boolean;
         Break_Received                   : Boolean;
         Overrun                          : Boolean;
         Parity_Error                     : Boolean;
         Framing_Error                    : Boolean;
         Transmitter_FIFO_Half_Full       : Boolean;
         Receiver_FIFO_Half_Full          : Boolean;
         Transmitter_FIFO_Full            : Boolean;
         Receiver_FIFO_Full               : Boolean;
         Reserved                         : Reserved_9;
         Transmitter_FIFO_Count           : FIFO_Count;
         Receiver_FIFO_Count              : FIFO_Count;
      end record;

   for UART_Status_Register use
      record
         Receiver_FIFO_Count              at 0 range Bit31 .. Bit26;
         Transmitter_FIFO_Count           at 0 range Bit25 .. Bit20;
         Reserved                         at 0 range Bit19 .. Bit11;
         Receiver_FIFO_Full               at 0 range Bit10 .. Bit10;
         Transmitter_FIFO_Full            at 0 range Bit09 .. Bit09;
         Receiver_FIFO_Half_Full          at 0 range Bit08 .. Bit08;
         Transmitter_FIFO_Half_Full       at 0 range Bit07 .. Bit07;
         Framing_Error                    at 0 range Bit06 .. Bit06;
         Parity_Error                     at 0 range Bit05 .. Bit05;
         Overrun                          at 0 range Bit04 .. Bit04;
         Break_Received                   at 0 range Bit03 .. Bit03;
         Transmitter_FIFO_Empty           at 0 range Bit02 .. Bit02;
         Transmitter_Shift_Register_Empty at 0 range Bit01 .. Bit01;
         Data_Ready                       at 0 range Bit00 .. Bit00;
      end record;

   for UART_Status_Register'Size use 32;
   pragma Suppress_Initialization (UART_Status_Register);

   UART_1_Data : UART_Data_Register;
   pragma Atomic (UART_1_Data);
   for UART_1_Data'Address use UART_1_Data_Register_Address;

   UART_1_Status : UART_Status_Register;
   pragma Atomic (UART_1_Status);
   for UART_1_Status'Address use UART_1_Status_Register_Address;

   ---------
   -- Get --
   ---------

   function Get return Character is
   begin
      --  Will never be called

      raise Program_Error;
      return ASCII.NUL;
   end Get;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Initialized := True;
   end Initialize;

   -----------------
   -- Is_Rx_Ready --
   -----------------

   function Is_Rx_Ready return Boolean is
   begin
      return False;
   end Is_Rx_Ready;

   -----------------
   -- Is_Tx_Ready --
   -----------------

   function Is_Tx_Ready return Boolean is
      UART_Status_Aux : constant UART_Status_Register := UART_1_Status;
   begin
      return not UART_Status_Aux.Transmitter_FIFO_Full;
   end Is_Tx_Ready;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
      UART_Tx : constant UART_Data_Register :=
                  (FIFO => C, Reserved => (others => False));
   begin
      UART_1_Data := UART_Tx;
   end Put;

   ----------------------------
   -- Use_Cr_Lf_For_New_Line --
   ----------------------------

   function Use_Cr_Lf_For_New_Line return Boolean is
   begin
      return True;
   end Use_Cr_Lf_For_New_Line;
end System.Text_IO;
