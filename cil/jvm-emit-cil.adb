------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          J V M . E M I T . C I L                         --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Directories;
with Ada.Strings;               use Ada.Strings;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with J_String;                  use J_String;
with Lib;                       use Lib;
with Opt;                       use Opt;
with Osint;                     use Osint;
with Osint.C;                   use Osint.C;
with Output;                    use Output;
with Stringt;                   use Stringt;

package body JVM.Emit.CIL is

   ------------------
   -- Add_Assembly --
   ------------------

   procedure Add_Assembly (Name : String) is
      Lbracket : constant Natural := Index (Name, "[");
      Rbracket : constant Natural := Index (Name, "]");
      Pound    : constant Natural := Index (Name, "#");

   begin
      --  We use $ to mark the beginning so that it is easier to check if this
      --  already is present (we don't have to worry about prefixes or
      --  suffixes)

      if Name (Lbracket + 1 .. Rbracket - 1) /= "mscorlib"
        and then Name (Lbracket + 1 .. Rbracket - 1) /= "mgnat"
      then
         --  If there isn't version info

         if Pound < Name'First
           and then Index
             (Assembly_Names,
              "$" & Name (Lbracket + 1 .. Rbracket - 1) & ",") = 0
         then
            Assembly_Names := Assembly_Names
              & '$' & Name (Lbracket + 1 .. Rbracket - 1) & ",";

         elsif Pound > Name'First
           and then Index
             (Assembly_Names,
              "$" & Name (Lbracket + 1 .. Rbracket - 1) &
                Name (Pound .. Name'Last) & ",") = 0
         then
            Assembly_Names := Assembly_Names & '$' &
              Name (Lbracket + 1 .. Rbracket - 1) &
              Name (Pound .. Name'Last) & ",";
         end if;
      end if;
   end Add_Assembly;

   ------------------
   -- Add_Assembly --
   ------------------

   procedure Add_Assembly (Version : String; Name : String_Id) is
      Copy_Version : constant String := Version;

   begin
      --  Because Version is an in parameter, GNAT does pass by reference but
      --  the following call will overwrite it, so we make a copy of version

      String_To_Name_Buffer (Name);

      --  Here we piggy-back on the code above, and add a '#' as the separator
      --  between the version info and the assembly name

      Add_Assembly (Name_Buffer (1 .. Name_Len) & '#' & Copy_Version);
   end Add_Assembly;

   ----------------------
   -- Output_File_Name --
   ----------------------

   function Output_File_Name return String is
      Extension : constant String := ".il";

   begin
      if not Output_File_Name_Present then
         return
           Ada.Directories.Base_Name
             (Translate_File_Name (Name_Id (Unit_File_Name (Main_Unit))))
           & Extension;

      --  The Output file name was specified in the -o argument

      else
         --  Locate the last dot to remove the extension of native platforms
         --  (for example, file.o)

         declare
            S : constant String :=
                  CIL.Translate_File_Name (Get_Output_Object_File_Name, False);
         begin
            for J in reverse S'Range loop
               if S (J) = '.' then
                  return S (S'First .. J - 1) & Extension;
               end if;
            end loop;

            return S & Extension;
         end;
      end if;
   end Output_File_Name;

   ------------------------
   -- Produce_Empty_File --
   ------------------------

   procedure Produce_Empty_File is
      File : Ada.Text_IO.File_Type;

   begin
      if First_Class_Opened then
         Name_Buffer (1 .. 12) := "mscorlib.txt";
         Name_Len := 12;
         Read_Source_File
           (Name_Find, Lo => 0, Hi => Mscorlib_Hi, Src => Mscorlib_Text);

         Name_Buffer (1 .. 11) := "gnatlib.txt";
         Name_Len := 11;
         Read_Source_File
           (Name_Find, Lo => 0, Hi => Gnatlib_Hi, Src => Gnatlib_Text);

         Create
           (File => File,
            Mode => Out_File,
            Name => Output_File_Name);
         First_Class_Opened := False;

      else
         return;
      end if;

      if Mscorlib_Text = null then
         Write_Line
           ("fatal error, run-time library not installed correctly");
         Write_Line ("cannot locate file mscorlib.txt");
         raise Unrecoverable_Error;
      end if;

      Put (File, String (Mscorlib_Text (0 .. Mscorlib_Hi - 1)));
      Close (File);
   end Produce_Empty_File;

   -------------------------
   -- Translate_File_Name --
   -------------------------

   function Translate_File_Name (Name : Name_Id) return String is
   begin
      --  In order to keep unmodified the previous behavior of the compiler we
      --  inconditionally pass True in the argument Handle_Line_Directive. This
      --  should be reviewed because it does not seem correct???

      return
        Translate_File_Name
          (Input_Name            => Name_String (Name),
           Handle_Line_Directive => True);
   end Translate_File_Name;

   -------------------------
   -- Translate_File_Name --
   -------------------------

   function Translate_File_Name
     (Input_Name            : String;
      Handle_Line_Directive : Boolean) return String
   is
      Result : String (1 .. Input_Name'Length * 2);
      Count  : Natural := 0;

   begin
      --  For CIL, we need to use windows style paths: we need to add a drive
      --  letter if none is present, unless the path is an UNC path, and
      --  translate slashes to anti-slashes (doubled, because of interpretation
      --  by CIL as escape character)

      if Input_Name'Length > 2
        and then Input_Name (Input_Name'First) = '/'
        and then Input_Name (Input_Name'First + 1) /= ':'
        and then Input_Name (Input_Name'First .. Input_Name'First + 1) /= "\\"
      then
         declare
            Current_Dir : constant Dir_Name_Str := Get_Current_Dir;

         begin
            Count := Count + 1;
            Result (Count) := Current_Dir (Current_Dir'First);
            Count := Count + 1;
            Result (Count) := Current_Dir (Current_Dir'First + 1);
         end;
      end if;

      --  Also, for CIL, '\' needs to be replaced with "\\" in the .line
      --  directives

      for J in Input_Name'Range loop
         if Input_Name (J) = '\' or else Input_Name (J) = '/' then
            Count := Count + 1;
            Result (Count) := '\';

            if Handle_Line_Directive then
               Count := Count + 1;
               Result (Count) := '\';
            end if;

         else
            Count := Count + 1;
            Result (Count) := Input_Name (J);
         end if;
      end loop;

      return Result (1 .. Count);
   end Translate_File_Name;

   -------------------------------
   -- Update_Obj_File_Timestamp --
   -------------------------------

   procedure Update_Obj_File_Timestamp is
      Obj_File_Name : constant String := Output_File_Name;
      Success       : Boolean;
      pragma Unreferenced (Success);

   begin
      Osint.C.Set_Library_Info_Name;
      GNAT.OS_Lib.Copy_Time_Stamps
        (Name_Buffer (1 .. Name_Len), Obj_File_Name, Success);
   end Update_Obj_File_Timestamp;

end JVM.Emit.CIL;
