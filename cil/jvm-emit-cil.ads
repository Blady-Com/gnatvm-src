------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          J V M . E M I T . C I L                         --
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

--  JVM.Emit.CIL encapsulates CIL's capability for generating CIL class
--  files. The JVM package calls JVM.Emit, which in turn calls the
--  JVM_File interface.

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

package JVM.Emit.CIL is
   Assembly_Names     : Unbounded_String := Null_Unbounded_String;
   First_Class_Opened : Boolean := True;
   Gnatlib_Text       : Types.Source_Buffer_Ptr;
   Gnatlib_Hi         : Types.Source_Ptr;
   Mscorlib_Text      : Types.Source_Buffer_Ptr;
   Mscorlib_Hi        : Types.Source_Ptr;
   --  So that we can put all of the classes in the same assembly
   --  file, which will make it easier to do the linking later

   procedure Add_Assembly (Name : String);
   procedure Add_Assembly (Version : String; Name : String_Id);
   --  Look at the external name from a pragma Import, and remember the
   --  assembly that needs to be imported so that it can be output when we get
   --  to Produce_Class_File

   function Output_File_Name return String;
   --  Returns the name of the output IL file

   procedure Produce_Empty_File;
   --  Generate an empty file is needed (i.e. no other class has been generated
   --  yet).

   function Translate_File_Name (Name : Name_Id) return String;
   --  For CIL, we need to use windows style paths: we need to add a drive
   --  letter if none is present, unless the path is an UNC path, and translate
   --  slashes to anti-slashes (doubled, if Handle_Line_ Directive is true
   --  because of interpretation by CIL as escape character).

   function Translate_File_Name
     (Input_Name            : String;
      Handle_Line_Directive : Boolean) return String;
   --  For CIL, we need to use windows style paths: we need to add a drive
   --  letter if none is present, unless the path is an UNC path, and translate
   --  slashes to anti-slashes (doubled, if Handle_Line_ Directive is true
   --  because of interpretation by CIL as escape character).

   procedure Update_Obj_File_Timestamp;
   --  Updates the timestamp of the generated IL file

end JVM.Emit.CIL;
