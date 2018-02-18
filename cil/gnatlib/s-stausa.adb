------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M - S T A C K _ U S A G E                    --
--                                                                          --
--                                 B o d y                                  --
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

package body System.Stack_Usage is

   procedure Fill_Stack (Analyzer : in out Stack_Analyzer) is
      pragma Unreferenced (Analyzer);
   begin
      null;
   end Fill_Stack;

   procedure Initialize_Analyzer
     (Analyzer         : in out Stack_Analyzer;
      Task_Name        : String;
      Stack_Size       : Natural;
      Stack_Base       : Stack_Address;
      Pattern_Size     : Natural;
      Pattern          : Interfaces.Unsigned_32 := 16#DEAD_BEEF#)
   is
      pragma Unreferenced
        (Analyzer, Task_Name, Stack_Size, Stack_Base, Pattern_Size, Pattern);
   begin
      null;
   end Initialize_Analyzer;

   procedure Compute_Result (Analyzer : in out Stack_Analyzer) is
      pragma Unreferenced (Analyzer);
   begin
      null;
   end Compute_Result;

   procedure Report_Result (Analyzer : Stack_Analyzer) is
      pragma Unreferenced (Analyzer);
   begin
      null;
   end Report_Result;

end System.Stack_Usage;
