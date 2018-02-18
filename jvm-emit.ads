------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M . E M I T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2013, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by --
-- AdaCore - http://www.adacore.com                                         --
--                                                                          --
-- This work is partially  based on A#, an Ada  compiler for .NET by  Prof. --
-- Martin C. Carlisle of the United States Air Force Academy.               --
--                                                                          --
------------------------------------------------------------------------------

--  JVM.Emit encapsulates CIL/JVM capability for generating CIL/JVM class
--  files. The JVM package calls JVM.Emit, which in turn calls the
--  JVM_File interface.

package JVM.Emit is

   procedure Add_Assembly (Name : String);
   procedure Add_Assembly (Version : String; Name : String_Id);
   --  CLI_Target: Look at the external name from a pragma Import, and remember
   --  the assembly that needs to be imported so that it can be output
   --  when we get to Produce_Class_File
   --  JVM_Target: No-op.

   procedure Produce_Class_File (Class : Class_Id);
   --  Traverses Class's associated set of fields, methods, and constant pool
   --  information to generate its class file.

   function Type_String (Typ : Type_Id) return String;
   --  Gets the string associated with a type

end JVM.Emit;
