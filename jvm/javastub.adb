------------------------------------------------------------------------------
--                                                                          --
--                                 J N I                                    --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.Command_Line;       use GNAT.Command_Line;
with J_Basics;                use J_Basics;
with J_Types;                 use J_Types;
with J_Utils;
with JVM_File;                use JVM_File;
with JVM_Utils;               use JVM_Utils;
with Utils;                   use Utils;

procedure Javastub is

   Verbose : Boolean := False;
   --  When true, display all the .class files and directories which are
   --  skipped while processing a jar/zip file.

   procedure Write_Usage;
   --  Print the program options

   procedure Process_Switches;
   --  Parse the command line

   procedure Process_Directory (Name : String);
   --  Process a directory encountered in an archive

   procedure Process_Class (Bytes : Stream_Of_U1);
   --  Process a single class

   function Signature_To_Parameters (Signature : String) return String;
   --  Given a method descriptor, return the mangled list of parameters

   package Javastub_Command_Line is
     new J_Utils.Command_Line
       (Verbose           => Verbose,
        Write_Usage       => Write_Usage,
        Process_Switches  => Process_Switches,
        Process_Class     => Process_Class,
        Process_Directory => Process_Directory);
   --  Generic instantiation that does the actual job of processing the
   --  command line of a jvm2ada invocation and calling the appropriate
   --  routines that transform JVM .class files into Ada specs.

   -----------------------------
   -- Signature_To_Parameters --
   -----------------------------

   function Signature_To_Parameters (Signature : String) return String is
      First, Last : Integer := Signature'First;
   begin
      Last := Signature'First;

      while Last < Signature'Last loop
         if Signature (Last) = '(' then
            First := Last;
         elsif Signature (Last) = ')' then
            exit;
         end if;

         Last := Last + 1;
      end loop;

      if Last - First <= 1 then
         return "";
      else
         return "__" & To_String
           (Mangle_ID (To_Wide_String (Signature (First + 1 .. Last - 1))));
      end if;
   end Signature_To_Parameters;

   -------------------
   -- Process_Class --
   -------------------

   procedure Process_Class (Bytes : Stream_Of_U1) is
      CF         : constant Class_File := Read (Bytes, Check => True);
      Class_Name : constant String := Get_String (CF, CF.This_Class);
      Spec_File  : Ada.Text_IO.File_Type;

      procedure NL;
      procedure P  (S : String);
      procedure PL (S : String);

      --------
      -- NL --
      --------

      procedure NL is
      begin
         New_Line (Spec_File);
      end NL;

      -------
      -- P --
      -------

      procedure P  (S : String) is
      begin
         Put (Spec_File, S);
      end P;

      --------
      -- PL --
      --------

      procedure PL (S : String) is
      begin
         Put_Line (Spec_File, S);
      end PL;

      T : constant CP.Table := CF.Constant_Pool;
      M : Member_Info;

   begin
      Create (Spec_File, Name => To_Lower (Class_Name) & "_jni.ads");

      PL ("--  Stub for class " & Class_Name);
      NL;
      PL ("with Interfaces.Java.JNI; use Interfaces.Java.JNI;");
      NL;

      PL ("package " & Class_Name & "_JNI is");

      for J in 0 .. Member.Last (CF.Methods) loop
         M := Member.Get (CF.Methods, J);

         declare
            Method_Name   : constant String :=
                              To_String (Get_Utf8 (T, M.Name_Index));
            Signature     : constant String :=
                              To_String (Get_Utf8 (T, M.Descriptor_Index));
            Nb_Parameters : constant Natural :=
                              Parameter_Count (Signature);
            Is_Procedure  : constant Boolean :=
                              Signature (Signature'Last) = 'V';

            Current_Pos      : Natural := Signature'First + 1;
            End_Pos          : Natural;
         begin
            if Is_Set (M.Access_Flags, ACC_Native) then
               NL;
               PL ("   --  Class:     " & Class_Name);
               PL ("   --  Method:    " & Method_Name);
               PL ("   --  Signature: " & Signature);

               if Is_Procedure then
                  P ("   procedure ");
               else
                  P ("   function ");
               end if;

               P (Method_Name &
                  " (Env : JNI_Env_Access; Obj : J_Object");

               for Param in 1 .. Nb_Parameters loop
                  End_Pos := Next_Declaration_Pos
                    (Signature (Current_Pos .. Signature'Last));
                  P ("; P" & Strip (Param'Img) & " : " &
                     Get_JNI_Type (Signature (Current_Pos)));
                  Current_Pos := End_Pos;
               end loop;

               if Is_Procedure then
                  PL (");");
               else
                  PL (")");
                  PL ("     return " &
                      Get_JNI_Type (Signature (Signature'Last)) & ";");
               end if;

               PL ("   pragma Export (C, "
                   & To_String (Get_Utf8 (T, M.Name_Index))
                   & ", ""Java_" & Class_Name & "_"
                   & To_String (Get_Utf8 (T, M.Name_Index))
                   & Signature_To_Parameters (Signature)
                   & """);");
            end if;
         end;
      end loop;
      NL;
      PL ("end " & Class_Name & "_JNI;");

   end Process_Class;

   -----------------------
   -- Process_Directory --
   -----------------------

   procedure Process_Directory (Name : String) is
   begin
      Put_Line ("Processing directory: " & Name);
   end Process_Directory;

   ----------------------
   -- Process_Switches --
   ----------------------

   procedure Process_Switches is
   begin
      loop
         case Getopt ("v") is
            when ASCII.NUL =>
               exit;

            when 'v' =>
               Verbose := True;

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
      procedure PL (S : String) renames Ada.Text_IO.Put_Line;
      procedure NL (Spacing : Positive_Count := 1) renames New_Line;
   begin
      PL ("Javasub Copyright 2007, AdaCore");
      PL ("Usage: javastub file [file ...]");
      NL;
      PL ("  -v     Verbose output");
   end Write_Usage;

begin
   Javastub_Command_Line.Process;
end Javastub;
