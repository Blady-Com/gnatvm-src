------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . E X C E P T I O N S . T R A C E B A C K             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2012, Free Software Foundation, Inc.           --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  This package is part of the support for tracebacks on exceptions. This
--  version is for runtimes without exception propagation.

with System.Traceback_Entries;

package Ada.Exceptions.Traceback is

   package STBE renames System.Traceback_Entries;

   subtype Code_Loc is System.Address;
   --  Code location in executing program

   type Tracebacks_Array is array (Positive range <>) of STBE.Traceback_Entry;
   --  A traceback array is an array of traceback entries

   function Get_PC (TBE : STBE.Traceback_Entry) return Code_Loc
     renames STBE.PC_For;
   --  Returns the code address held by a given traceback entry,
   --  typically the address of a call instruction.

end Ada.Exceptions.Traceback;
