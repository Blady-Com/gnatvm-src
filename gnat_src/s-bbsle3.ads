------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--       S Y S T E M . B B . B O A R D _ S U P P O R T . L E O N _ 3        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2006 The European Space Agency            --
--                     Copyright (C) 2003-2011, AdaCore                     --
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

--  This package provides the appropriate mapping for the system registers.
--  This is a LEON3 specific  package, based on the UT699 LEON 3FT/SPARC V8
--  Micro-Processor Advanced Users Manual, dated March 2, 2009 from
--  www.aeroflex.com/LEON, referenced hereafter as AUM.

--  This package is not named Leon3, as that would cause a name krunching
--  collision with System.BB.Board_Support.Leon.

pragma Restrictions (No_Elaboration_Code);

with System.Multiprocessors;

package System.BB.Board_Support.LEON_3 is
   pragma Preelaborate;

   --  Pragma Suppress_Initialization (register_type) must be used in order
   --  to keep eficiency. Otherwise, initialization procedures are always
   --  generated for objects of packed boolean array types and of record types
   --  that have components of these types.

   ----------------------------
   -- Local type definitions --
   ----------------------------

   type Scaler_10 is mod 2 **  10;
   for Scaler_10'Size use  10;
   --  10-bit scaler

   type Scaler_12 is mod 2 **  12;
   for Scaler_12'Size use  12;
   --  12-bit scaler

   type Reserved_2 is array (0 .. 1) of Boolean;
   for Reserved_2'Size use 2;
   pragma Pack (Reserved_2);

   type Reserved_3 is array (0 .. 2) of Boolean;
   for Reserved_3'Size use 3;
   pragma Pack (Reserved_3);

   type Reserved_8 is array (0 .. 7) of Boolean;
   for Reserved_8'Size use 8;
   pragma Pack (Reserved_8);

   type Reserved_16 is array (0 .. 15) of Boolean;
   for Reserved_16'Size use 16;
   pragma Pack (Reserved_16);

   type Reserved_20 is array (0 .. 19) of Boolean;
   for Reserved_20'Size use 20;
   pragma Pack (Reserved_20);

   type Reserved_21 is array (0 .. 20) of Boolean;
   for Reserved_21'Size use 21;
   pragma Pack (Reserved_21);

   type Reserved_22 is array (0 .. 21) of Boolean;
   for Reserved_22'Size use 22;
   pragma Pack (Reserved_22);

   type Reserved_23 is array (0 .. 22) of Boolean;
   for Reserved_23'Size use 23;
   pragma Pack (Reserved_23);

   type Reserved_24 is array (0 .. 23) of Boolean;
   for Reserved_24'Size use 24;
   pragma Pack (Reserved_24);

   type Reserved_25 is array (0 .. 24) of Boolean;
   for Reserved_25'Size use 25;
   pragma Pack (Reserved_25);

   type Reserved_27 is array (0 .. 26) of Boolean;
   for Reserved_27'Size use 27;
   pragma Pack (Reserved_27);

   --  Mapping between bits in a 32-bit register as used in the hardware
   --  documentation and bit order as used by Ada.

   --  This makes it easier to verify correctness against the AUM. Ranges will
   --  need to be reversed, but the compiler will check this.

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

   ---------------------
   -- Timer Registers --
   ---------------------

   subtype Timer_Register is Timer_Interval;
   pragma Suppress_Initialization (Timer_Register);

   type Timer_Control_Register is
      record
         Enable            : Boolean;
         --  1  : enable counting
         --  0  : hold scaler (and counter) w

         Reload_Counter    : Boolean;
         --  1  : reload counter at zero and restart
         --  0  : stop counter at zero w

         Load_Counter      : Boolean;
         --  1  : load counter with preset value and start if enabled
         --  0  : no function w

         Interrupt_Enable  : Boolean;
         --  1  : timer underflow signals interrupt
         --  0  : interrupts disabled

         Interrupt_Pending : Boolean;
         --  0  : interrupt not pending
         --  1  : interrupt pending, remains 1 until writing 0 to this bit

         Chain             : Boolean;
         --  0  : timer functions independently
         --  1  : decrementing timer N begins when timer (N - 1) underflows

         Debug_Halt        : Boolean; -- State of timer when DF = 0, read only
         --  0  : active
         --  1  : frozen

         Reserved          : Reserved_25;
      end record;

   for Timer_Control_Register use
      record
         Reserved          at 0 range Bit31 .. Bit07;
         Debug_Halt        at 0 range Bit06 .. Bit06;
         Chain             at 0 range Bit05 .. Bit05;
         Interrupt_Pending at 0 range Bit04 .. Bit04;
         Interrupt_Enable  at 0 range Bit03 .. Bit03;
         Load_Counter      at 0 range Bit02 .. Bit02; -- Load_Timer (LD) in AUM
         Reload_Counter    at 0 range Bit01 .. Bit01; -- Restart (RS) in AUM
         Enable            at 0 range Bit00 .. Bit00;
      end record;

   for Timer_Control_Register'Size use 32;
   pragma Suppress_Initialization (Timer_Control_Register);

   -------------
   -- Timer 1 --
   -------------

   Timer_1_Counter : Timer_Register;
   Timer_1_Reload  : Timer_Register;
   Timer_1_Control : Timer_Control_Register;

   pragma Atomic (Timer_1_Counter);
   pragma Atomic (Timer_1_Reload);
   pragma Atomic (Timer_1_Control);

   for Timer_1_Counter'Address use System'To_Address (16#8000_0310#);
   for Timer_1_Reload'Address  use System'To_Address (16#8000_0314#);
   for Timer_1_Control'Address use System'To_Address (16#8000_0318#);

   -------------
   -- Timer 2 --
   -------------

   Timer_2_Counter : Timer_Register;
   Timer_2_Reload  : Timer_Register;
   Timer_2_Control : Timer_Control_Register;

   pragma Atomic (Timer_2_Counter);
   pragma Atomic (Timer_2_Reload);
   pragma Atomic (Timer_2_Control);

   for Timer_2_Counter'Address use System'To_Address (16#8000_0320#);
   for Timer_2_Reload'Address  use System'To_Address (16#8000_0324#);
   for Timer_2_Control'Address use System'To_Address (16#8000_0328#);

   -------------
   -- Timer 3 --
   -------------

   Timer_3_Counter : Timer_Register;
   Timer_3_Reload  : Timer_Register;
   Timer_3_Control : Timer_Control_Register;

   pragma Atomic (Timer_3_Counter);
   pragma Atomic (Timer_3_Reload);
   pragma Atomic (Timer_3_Control);

   for Timer_3_Counter'Address use System'To_Address (16#8000_0330#);
   for Timer_3_Reload'Address  use System'To_Address (16#8000_0334#);
   for Timer_3_Control'Address use System'To_Address (16#8000_0338#);

   -------------
   -- Timer 4 --
   -------------

   Timer_4_Counter : Timer_Register;
   Timer_4_Reload  : Timer_Register;
   Timer_4_Control : Timer_Control_Register;

   pragma Atomic (Timer_4_Counter);
   pragma Atomic (Timer_4_Reload);
   pragma Atomic (Timer_4_Control);

   for Timer_4_Counter'Address use System'To_Address (16#8000_0340#);
   for Timer_4_Reload'Address  use System'To_Address (16#8000_0344#);
   for Timer_4_Control'Address use System'To_Address (16#8000_0348#);

   --------------
   -- Watchdog --
   --------------

   --  Watchdog_Register_Address is not available.
   --  On LEON3, Timer_4 also drives the WDOGN watchdog signal

   Watchdog_Register : Timer_Register renames Timer_4_Counter;

   ---------------
   -- Prescaler --
   ---------------

   type Prescaler_Register is
      record
         Value    : Scaler_10;
         Reserved : Reserved_22;
      end record;

   for Prescaler_Register use
      record
         Reserved at 0 range Bit31 .. Bit10;
         Value    at 0 range Bit09 .. Bit00;
      end record;

   for Prescaler_Register'Size use 32;

   pragma Suppress_Initialization (Prescaler_Register);

   Prescaler_Counter : Prescaler_Register;
   pragma Atomic (Prescaler_Counter);
   for Prescaler_Counter'Address use System'To_Address (16#8000_0300#);

   Prescaler_Reload : Prescaler_Register;
   pragma Atomic (Prescaler_Reload);
   for Prescaler_Reload'Address use System'To_Address (16#8000_0304#);

   --------------------------
   --  Interrupt Registers --
   --------------------------

   type Interrupt_Register is mod 2**32;
   for Interrupt_Register'Size use 32;
   pragma Suppress_Initialization (Interrupt_Register);

   ------------------------------
   -- Interrupt Level Register --
   ------------------------------

   Interrupt_Level : Interrupt_Register;
   pragma Atomic (Interrupt_Level);
   for Interrupt_Level'Address use System'To_Address (16#8000_0200#);

   --------------------------------
   -- Interrupt Pending Register --
   --------------------------------

   Interrupt_Pending : Interrupt_Register;
   pragma Atomic (Interrupt_Pending);
   for Interrupt_Pending'Address use System'To_Address (16#8000_0204#);

   -------------------------------
   -- Interrupt Force Registers --
   -------------------------------

   type Interrupt_Force_Registers is
     array (System.Multiprocessors.CPU) of Interrupt_Register;
   for Interrupt_Force_Registers'Component_Size use 32;
   pragma Atomic_Components (Interrupt_Force_Registers);

   Interrupt_Force : Interrupt_Force_Registers;
   for Interrupt_Force'Address use System'To_Address (16#8000_0280#);

   ------------------------------
   -- Interrupt Clear Register --
   ------------------------------

   Interrupt_Clear : Interrupt_Register;
   pragma Atomic (Interrupt_Clear);
   for Interrupt_Clear'Address use System'To_Address (16#8000_020C#);

   ------------------------------
   -- Interrupt Mask Registers --
   ------------------------------

   type Interrupt_Mask_Registers is
     array (System.Multiprocessors.CPU) of Interrupt_Register;
   for Interrupt_Mask_Registers'Component_Size use 32;
   pragma Atomic_Components (Interrupt_Mask_Registers);

   Interrupt_Mask : Interrupt_Mask_Registers;
   for Interrupt_Mask'Address use System'To_Address (16#8000_0240#);

   ------------------------------------
   -- Multiprocessor status register --
   ------------------------------------

   type Scalar_4 is mod 2**4;
   for Scalar_4'Size use 4;

   type CPU_Status is array (0 .. 15) of Boolean;
   for CPU_Status'Size use 16;
   pragma Pack (CPU_Status);

   type Multiprocessor_Status_Register is
      record
         NCPUS : Scalar_4;
         --  Number of CPUs - 1

         Reserved : Reserved_8;

         EIRQ : Scalar_4;
         --  Interrupt number used for extended interrupts. 0 if extended
         --  interrupts are disabled.

         Status : CPU_Status;
         --  Power-down status of CPU [n]: 0 = power-down, 1 = running. Write
         --  with 1 to start processor n.
      end record;

   for Multiprocessor_Status_Register use
      record
         NCPUS    at 0 range Bit31 .. Bit28;
         Reserved at 0 range Bit27 .. Bit20;
         EIRQ     at 0 range Bit19 .. Bit16;
         Status   at 0 range Bit15 .. Bit00;
      end record;

   for Multiprocessor_Status_Register'Size use 32;
   pragma Suppress_Initialization (Multiprocessor_Status_Register);

   Multiprocessor_Status : Multiprocessor_Status_Register;
   for Multiprocessor_Status'Address use System'To_Address (16#8000_0210#);

   --------------------
   -- UART Registers --
   --------------------

   type FIFO_Count is mod 64;
   for FIFO_Count'Size use 6;

   type Parity_Kind is (Even, Odd);

   type UART_Data_Register is
      record
         FIFO : Character;
         --  Reading and writing accesses receiver resp. transmitter FIFOs

         Reserved : Reserved_24;
         --  Not used r
      end record;

   for UART_Data_Register use
      record
         Reserved at 0 range Bit31 .. Bit08;
         FIFO     at 0 range Bit07 .. Bit00;
      end record;

   type Reserved_9 is mod 2**9;
   for Reserved_9'Size use 9;

   for UART_Data_Register'Size use 32;
   pragma Suppress_Initialization (UART_Data_Register);

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

   type UART_Control_Register is
      record
         Receiver_Enable                   : Boolean;
         Transmitter_Enable                : Boolean; --  Transmitter enable
         Receiver_Interrupt_Enable         : Boolean;
         Transmitter_Interrupt_Enable      : Boolean;
         Parity_Select                     : Parity_Kind;
         Parity_Enable                     : Boolean;
         Reserved_1                        : Boolean;
         Loop_Back                         : Boolean;
         Reserved_2                        : Boolean;
         Receiver_FIFO_Interrupt_Enable    : Boolean;
         Transmitter_FIFO_Interrupt_Enable : Boolean;
         Reserved_3                        : Reserved_21;
      end record;

   for UART_Control_Register use
      record
         Reserved_3                        at 0 range Bit31 .. Bit11;
         Receiver_FIFO_Interrupt_Enable    at 0 range Bit10 .. Bit10;
         Transmitter_FIFO_Interrupt_Enable at 0 range Bit09 .. Bit09;
         Reserved_2                        at 0 range Bit08 .. Bit08;
         Loop_Back                         at 0 range Bit07 .. Bit07;
         Reserved_1                        at 0 range Bit06 .. Bit06;
         Parity_Enable                     at 0 range Bit05 .. Bit05;
         Parity_Select                     at 0 range Bit04 .. Bit04;
         Transmitter_Interrupt_Enable      at 0 range Bit03 .. Bit03;
         Receiver_Interrupt_Enable         at 0 range Bit02 .. Bit02;
         Transmitter_Enable                at 0 range Bit01 .. Bit01;
         Receiver_Enable                   at 0 range Bit00 .. Bit00;
      end record;

   for UART_Control_Register'Size use 32;

   pragma Suppress_Initialization (UART_Control_Register);

   type UART_Scaler_Register is
      record
         UART_Scaler : Scaler_12;
         --  1 - 4095  : Divide factor
         --  0  : stops the UART clock

         Reserved : Reserved_20;
      end record;

   for UART_Scaler_Register use
      record
         Reserved    at 0 range Bit31 .. Bit12;
         UART_Scaler at 0 range Bit11 .. Bit00;
      end record;

   for UART_Scaler_Register'Size use 32;
   pragma Suppress_Initialization (UART_Scaler_Register);

   ----------
   -- UART --
   ----------

   UART_Data : UART_Data_Register;
   pragma Atomic (UART_Data);
   for UART_Data'Address use System'To_Address (16#8000_0100#);

   UART_Status : UART_Status_Register;
   pragma Atomic (UART_Status);
   for UART_Status'Address use System'To_Address (16#8000_0104#);

   UART_Control : UART_Control_Register;
   pragma Atomic (UART_Control);
   for UART_Control'Address use System'To_Address (16#8000_0108#);

   UART_Scaler : UART_Scaler_Register;
   pragma Atomic (UART_Scaler);
   for UART_Scaler'Address use System'To_Address (16#8000_010C#);

   ----------------------------
   -- Cache Control Register --
   ----------------------------

   type Status_2 is mod 2 ** 2;
   for Status_2'Size use 2;

   type Test_Bit_Status is mod 2 ** 4;
   for Test_Bit_Status'Size use 4;

   type Cache_Control_Register is
      record
         Ics       : Status_2;
         Dcs       : Status_2;
         Icf       : Boolean;
         Dcf       : Boolean;
         Dde       : Status_2;
         Dte       : Status_2;
         Ide       : Status_2;
         Ite       : Status_2;
         Dp        : Boolean;
         Ip        : Boolean;
         Ib        : Boolean;
         Reserved1 : Reserved_2;
         Ft        : Status_2;
         Fi        : Boolean;
         Fd        : Boolean;
         Reserved2 : Boolean;
         Tb        : Test_Bit_Status;
         Ps        : Boolean;
         Reserved3 : Reserved_3;
      end record;

   for Cache_Control_Register use
     record
        Reserved3  at 0 range Bit31 .. Bit29;
        Ps         at 0 range Bit28 .. Bit28;
        Tb         at 0 range Bit27 .. Bit24;
        Reserved2  at 0 range Bit23 .. Bit23;
        Fd         at 0 range Bit22 .. Bit22;
        Fi         at 0 range Bit21 .. Bit21;
        Ft         at 0 range Bit20 .. Bit19;
        Reserved1  at 0 range Bit18 .. Bit17;
        Ib         at 0 range Bit16 .. Bit16;
        Ip         at 0 range Bit15 .. Bit15;
        Dp         at 0 range Bit14 .. Bit14;
        Ite        at 0 range Bit13 .. Bit12;
        Ide        at 0 range Bit11 .. Bit10;
        Dte        at 0 range Bit09 .. Bit08;
        Dde        at 0 range Bit07 .. Bit06;
        Dcf        at 0 range Bit05 .. Bit05;
        Icf        at 0 range Bit04 .. Bit04;
        Dcs        at 0 range Bit03 .. Bit02;
        Ics        at 0 range Bit01 .. Bit00;
     end record;

   for  Cache_Control_Register'Size use 32;

   pragma Suppress_Initialization (Cache_Control_Register);

end System.BB.Board_Support.LEON_3;
