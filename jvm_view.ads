------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M _ V I E W                              --
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
------------------------------------------------------------------------------

--  Set of routines to print all JVM class file data structures defined in
--  package JVM_File.  These routines assume everything is consistent. If this
--  is not the case their behavior is undefined.

with J_Types;  use J_Types;
with JVM_File; use JVM_File;

package JVM_View is

   procedure Print_Class_File (CF : Class_File);
   --  Prints the contents of a class file without printing either the method's
   --  code or debug info and without printing the constant pool.

   procedure Print_Utf8 (T : Utf8.Table);
   --  Prints the Utf8 table T

   procedure Print_CP (T : CP.Table);
   --  Print the constant pool T

   procedure Print_CP_Entry (T : CP.Table; K : CP_Index);
   --  Print the contents of the K-th constant pool entry

   procedure Print_Access_Flags (A : Access_Mask);
   --  Prints the A's flags in a human readable format

   procedure Print_Member (M : Member_Info; T : CP.Table);
   --  Prints a field or method member M. T is the constant pool table for the
   --  correspondig class. For methods do not print the Code attribute. This is
   --  printed by using routine Print_Code below.

   procedure Print_Code
     (M           : Member_Info;
      T           : CP.Table;
      Source_Name : String;
      Source      : Stream_Of_U1;
      Do_Lines    : Boolean;
      Do_Vars     : Boolean);
   --  Prints the code attribute of method M. T is the constant pool table for
   --  the correspondig class. If not empty, Source is the stream of bytes of
   --  the source file containing the code for the original source and
   --  Source_Name the name of the source file. If Do_Lines is set then print
   --  the line number table. If Do_Vars is set then print the local variable
   --  table.

   procedure Print_Instruction
     (Bytecode  : Code_Array.Table;
      PC        : Instruction_Index;
      T         : CP.Table;
      Code_Attr : Code_Attribute.Table);
   --  Given a code array Bytecode, containing the bytecode of some method, the
   --  PC of some instruction in Bytecode, the constant pool table T and the
   --  code attribute table, Code_Attr, of the method, print the instruction
   --  located at PC in Bytecode.

   procedure Print_Handler_Table (H : Handler.Table; T : CP.Table);
   --  Prints exception handler table H. T is the constant pool of the
   --  corresponding class.

   procedure Print_Code_Attr (CA : Code_Attribute_Info; T : CP.Table);
   --  Prints a code attribute CA. T is the constant pool table for the
   --  correspondig class.

   procedure Print_Member_Attr (MA : Member_Attribute_Info; T : CP.Table);
   --  Prints a member attribute MA. T is the constant pool table for the
   --  correspondig class. Does not print the code attribute of a method, use
   --  Print_Code above for that purpose.

   procedure Print_Class_Attr (CA : Class_Attribute_Info; T : CP.Table);
   --  Prints a class attribute CA. T is the constant pool table for the
   --  correspondig class.

end JVM_View;
