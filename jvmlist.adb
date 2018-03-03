------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J V M L I S T                               --
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
------------------------------------------------------------------------------

with Ada.Text_IO;        use Ada.Text_IO;
with GNAT.Command_Line;  use GNAT.Command_Line;
with J_Basics;           use J_Basics;
with J_Types;            use J_Types;
with J_Utils;            use J_Utils;
with JVM_File;           use JVM_File;
with JVM_View;
with Gnatvsn;

procedure Jvmlist is

   use Member;
   use Utf8;

   ----------------
   -- Local Data --
   ----------------

   --  Each of these correspond to a Jvmlist switch

   View_Methods_Code   : Boolean := False;       --  "-c"
   --  When set displays the bytecode of each method

   Embed_Source_Code   : Boolean := False;       --  "-g"
   --  When set embeds the source code in the methods bytecode

   View_Line_Info      : Boolean := False;       --  "-l"
   --  When set displays line number tables

   View_Var_Info       : Boolean := False;       --  "-t"
   --  When set displays local variable tables

   View_Constant_Pool  : Boolean := False;       --  "-p"
   --  When set displays the class file constant pool

   Verbose_Mode        : Boolean := False;       --  "-v"
   --  When set, puts jvmlist in verbose mode

   --------------------
   -- Local Routines --
   --------------------

   procedure Process_Class (Bytes : Stream_Of_U1);
   --  Lists the content of the class file contained in Bytes

   procedure Process_Directory (Name : String);
   --  Does nothing, needed by the generic instantiation of
   --  Jvmlist_Command_Line below.

   procedure Process_Switches;
   --  Parse the switches on the command line

   procedure Write_Usage;
   --  Print program options

   package Jvmlist_Command_Line is
     new J_Utils.Command_Line
       (Verbose           => Verbose_Mode,
        Write_Usage       => Write_Usage,
        Process_Switches  => Process_Switches,
        Process_Class     => Process_Class,
        Process_Directory => Process_Directory);
   --  Generic instantiation that does the actual job of processing the
   --  command line of a jvmlist invocation and calling the appropriate
   --  routines that list the contents of a JVM .class file.

   -------------------
   -- Process_Class --
   -------------------

   procedure Process_Class (Bytes : Stream_Of_U1) is
      Source_Name : Utf8.Table;
      --  Name of the source file used to produce the current class file

      Get_Source_File : Boolean := False;
      --  Set if we must read the above source file

      Src : Stream_Of_U1_Ptr := null;
      --  The source file

      CF : constant JVM_File.Class_File
             := JVM_File.Read (Bytes, Check => True);
      --  The parsed class file

   begin
      --  See if we have to read the source file

      if Embed_Source_Code then
         Source_Name := J_Utils.Source_File_Name (CF);

         if Length (Source_Name) = 0 then
            Print_Msg ("");
            Print_Msg ("WARNING - no source file name found in class file");
         elsif Has_Wide_Chars (Source_Name) then
            Print_Msg ("source file name has extended characters, " &
                       "can't handle it, source ignored");
         else
            Get_Source_File := True;
         end if;
      end if;

      if View_Methods_Code and Get_Source_File then
         Src := Get_Stream_Of_U1 (To_String (Source_Name), Dont_Fail => True);
      end if;

      --  Then print the .class file

      JVM_View.Print_Class_File (CF);

      if View_Constant_Pool then
         Put_Line ("Constant Pool");
         Put_Line ("-------------");
         JVM_View.Print_CP (CF.Constant_Pool);
      end if;

      if View_Methods_Code then
         New_Line;
         Put_Line ("Methods Code");
         Put_Line ("------------");

         if Src /= null then
            for K in 0 .. Last (CF.Methods) loop
               declare
                  Method : constant Member_Info := Get (CF.Methods, K);

               begin
                  if not    Is_Set (Method.Access_Flags, ACC_Abstract)
                    and not Is_Set (Method.Access_Flags, ACC_Native)
                  then
                     New_Line;
                     JVM_View.Print_Code
                       (Method,
                        CF.Constant_Pool,
                        Source_Name => To_String (Source_Name),
                        Source      => Src.all,
                        Do_Lines    => View_Line_Info,
                        Do_Vars     => View_Var_Info);
                  end if;
               end;
            end loop;

         else --  Do not print the source file or source file not available
            for K in 0 .. Last (CF.Methods) loop
               declare
                  Method : constant Member_Info := Get (CF.Methods, K);
               begin

                  if not    Is_Set (Method.Access_Flags, ACC_Abstract)

                    and not Is_Set (Method.Access_Flags, ACC_Native)
                  then

                     New_Line;
                     JVM_View.Print_Code
                       (Get (CF.Methods, K),
                        CF.Constant_Pool,
                        Source_Name => "",
                        Source      => Empty_Stream,
                        Do_Lines    => View_Line_Info,
                        Do_Vars     => View_Var_Info);
                  end if;
               end;
            end loop;
         end if;
      end if;
   end Process_Class;

   -----------------------
   -- Process_Directory --
   -----------------------

   procedure Process_Directory (Name : String) is
   begin
      if Verbose_Mode then
         Put_Line ("Processing directory: " & Name);
      end if;
   end Process_Directory;

   ----------------------
   -- Process_Switches --
   ----------------------

   procedure Process_Switches is
   begin
      loop
         case GNAT.Command_Line.Getopt ("c g l p t v") is
            when ASCII.NUL =>
               exit;

            when 'c' =>
               View_Methods_Code := True;

            when 'g' =>
               Embed_Source_Code := True;
               View_Methods_Code := True;

            when 'l' =>
               View_Line_Info := True;

            when 'p' =>
               View_Constant_Pool := True;

            when 't' =>
               View_Var_Info := True;

            when 'v' =>
               Verbose_Mode := True;

            when others =>
               Write_Usage;
               return;
         end case;
      end loop;
   end Process_Switches;

   -----------------
   -- Write_Usage --
   -----------------

   procedure Write_Usage is
   begin
      Put_Line ("JVMLIST " & Gnatvsn.Gnat_Version_String
                & " Copyright 1998-1999, Ada Core Technologies Inc.");
      Put_Line ("Usage: jvmlist [switches] file [file ...]");
      New_Line;
      Put_Line ("  file   Can be one of the following:");
      Put_Line ("         . A .class file (you can omit the .class suffix)");
      Put      ("         . A zip/jar file (jvmlist applies to all .class");
      Put_Line (" files in the archive)");
      Put      ("         . A .class file inside a zip/jar (e.g. ");
      Put_Line ("jre.zip/java/lang/Object.class)");
      New_Line;
      Put_Line ("Possible switches:");
      Put_Line ("  -c     Displays the bytecode of each method");
      Put_Line ("  -g     Implies -c and embeds the original source code");
      Put_Line ("  -l     Displays line number tables");
      Put_Line ("  -p     Displays the constant pool");
      Put_Line ("  -t     Displays local variable tables");
      Put_Line ("  -v     Verbose");
      New_Line;
   end Write_Usage;

--  Start of processing for Jvmlist
begin
   Jvmlist_Command_Line.Process;
end Jvmlist;
