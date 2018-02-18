------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M - S T A C K _ U S A G E                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2011, Free Software Foundation, Inc.          --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is a dummy version for CIL/JVM

with System.Storage_Elements;
with Interfaces;

package System.Stack_Usage is
   pragma Preelaborate;

   package SSE renames System.Storage_Elements;

   Byte_Size : constant := 8;
   Word_32_Size : constant := 4 * Byte_Size;

   type Word_32 is mod 2 ** Word_32_Size;
   for Word_32'Alignment use 4;

   subtype Stack_Address is SSE.Integer_Address;

   type Stack_Analyzer is private;

   procedure Fill_Stack (Analyzer : in out Stack_Analyzer);
   --  Fill an area of the stack with the pattern Analyzer.Pattern.

   procedure Initialize_Analyzer
     (Analyzer         : in out Stack_Analyzer;
      Task_Name        : String;
      Stack_Size       : Natural;
      Stack_Base       : Stack_Address;
      Pattern_Size     : Natural;
      Pattern          : Interfaces.Unsigned_32 := 16#DEAD_BEEF#);
   --  Should be called before any use of a Stack_Analyzer, to initialize it.
   --  Size is the size of the pattern zone. Bottom should be a close
   --  approximation of the caller base frame address.

   Is_Enabled : Boolean := False;
   --  When this flag is true, then stack analysis is enabled

   procedure Compute_Result (Analyzer : in out Stack_Analyzer);
   --  Read the patern zone and deduce the stack usage.

   procedure Report_Result (Analyzer : Stack_Analyzer);
   --  Store the results of the computation in memory,

private

   type Stack_Analyzer is null record;

end System.Stack_Usage;
