------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          J V M 2 A D A _ L I B                           --
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

with Hostparm;
with J_Types;  use J_Types;
with Ada.Characters.Handling;  use Ada.Characters.Handling;

package JVM_Ada is

   type String_Ptr is access String;

   type Supported_Output is (JNI, JGNAT);

   ---------------
   -- Constants --
   ---------------

   Type_String             : constant String := "Typ";
   --  Name used in each class package for the tagged type name

   Exception_String        : constant String := "Except";
   U_Exception_String      : constant String := To_Upper (Exception_String);
   --  Name used in each exception class package for the Ada exception

   Array_String            : constant String := "Arr";
   --  Variable name used for methods taking array parameters

   Array_Suffix            : constant String := '_' & Array_String;
   --  Suffix used for the arrays

   Root_Array              : constant String := "Root_Array";

   Env_Type_String         : constant String := "JNI_Env_Access";

   JNI_Prefix              : constant String := "JNI_";

   Constructor_String      : constant String := "JNI_Constructor";
   U_Constructor_String    : constant String := To_Upper (Constructor_String);

   Set_Class               : constant String := "Set_Class";
   Class_String            : constant String := JNI_Prefix & "Class";
   Value_String            : constant String := "Value";
   Default_Value_String    : constant String := "Default_Value";
   Length_String           : constant String := "Length";
   Class_Name_String       : constant String := "Class_Name";
   --  Variable names

   ----------------------
   -- Options Handling --
   ----------------------

   Number_Of_Array_Types : Positive := 2;
   --  This option decided the number of array types generated in each package.
   --  By default, the unidimensional array ([]) and the two dimensional
   --  array ([][]) types are generated

   Generation_Mode : Supported_Output := JGNAT;
   --  Select the output:
   --    -  only .ads files for JGNAT
   --    -  .ads and .adb files for JNI

   Keep_Original_Identifiers : Boolean := False;
   --  When False the identifiers encoutered in JVM .class file are mangled,
   --  whenever needed to turn them in Ada identifiers. When True,
   --  identifiers are left as is.

   Output_Dir : String_Ptr := new String'(Hostparm.Normalized_CWD);
   --  Output directory for Ada files

   Overwrite_Files : Boolean := False;
   --  When set, overwrite any Ada spec file if present

   Quiet_Mode : Boolean := False;
   --  When set, be quiet in the output

   Skip_Sun_Classes : Boolean := True;
   --  When set do not map Sun public classes into Ada Specs

   Verbose_Mode : Boolean := False;
   --  When set, be verbose in the output

   procedure Search_Classes_In (Zip : String);
   --  When looking for a class file search also in archive Zip

   procedure Search_Sources_In (Zip : String);
   --  When looking for a source file search also in archive Zip

   -----------------------
   -- Classes functions --
   -----------------------

   procedure Convert_To_Ada (Bytes : Stream_Of_U1);
   --  Convert the .class file corresponding to Bytes into a .ads file

   procedure Convert_Directory_To_Ada (Name : String);
   --  Creates a very simple .ads file corresponding to a directory in
   --  the Java class tree (like java/lang ...). 'Name' should have
   --  '/' separators between identifiers.

end JVM_Ada;
