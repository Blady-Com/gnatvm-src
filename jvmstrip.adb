------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M S T R I P                              --
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

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.Command_Line;  use GNAT.Command_Line;
with J_Basics;
with J_Types;            use J_Types;
with J_Utils;
with J_Zip;
with JVM_File;
with Gnatvsn;

----------------------------------------------------
--  Process the files passed on the command line  --
----------------------------------------------------

procedure Jvmstrip is

   procedure Write_Usage;
   --  Print program options

   function Process_Single_Class_File
     (Bytes : J_Types.Stream_Of_U1) return Stream_Of_U1;
   --  Process a single class file

   Verbose_Mode : Boolean := False;
   --  Set if the verbose mode has been selected by the user

   -----------------
   -- Write Usage --
   -----------------

   procedure Write_Usage is
      package IO renames Ada.Text_IO;

   begin
      IO.Put_Line ("JVMSTRIP " & Gnatvsn.Gnat_Version_String
                & " Copyright 1998-1999, Ada Core Technologies Inc.");
      IO.Put_Line ("Strip debugging and line number information from a Java"
                   & " class file");
      IO.New_Line;
      IO.Put_Line ("Usage: jvmstrip [-v] files");
      IO.New_Line;
      IO.Put_Line (" -v     verbose, prints the name of the class file "
                   & "currently being checked");
      IO.Put_Line (" files  name of the files to check, for zip files "
                   & "containig an archive of class");
      IO.Put_Line ("        files each uncompressed class in the archive "
                   & "is processed in turn");
      IO.New_Line;
   end Write_Usage;

   --------------------------------
   --  Process_Single_Class_File --
   --------------------------------

   function Process_Single_Class_File
     (Bytes : J_Types.Stream_Of_U1) return Stream_Of_U1
   is
      use JVM_File;
      CF  : constant Class_File := Read (Bytes, Check => False);
      CF2 : Class_File;

   begin
      --  Copy strings from the original class file

      CF2.Magic         := CF.Magic;
      CF2.Minor_Version := CF.Minor_Version;
      CF2.Major_Version := CF.Major_Version;
      CF2.Access_Flags  := CF.Access_Flags;
      CF2.This_Class    := CF.This_Class;
      CF2.Super_Class   := CF.Super_Class;

      --  Copy the tables. Actually, we just copy a
      --  reference to the table, so that we shouldn't modify it!

      CF2.Interfaces    := CF.Interfaces;
      CF2.Fields        := CF.Fields;
      CF2.Attributes    := CF.Attributes;
      CF2.Constant_Pool := CF.Constant_Pool;

      --  Now process the table we have to strip

      Member.Allocate_Fixed_Table
        (CF2.Methods, Member.Last (CF.Methods) + 1);

      for K in 0 .. Member.Last (CF.Methods) loop
         declare
            Info : Member_Info;
            Old  : constant Member_Info
              := Member.Get (CF.Methods, K);
            Nb_Attr : constant Int_32
              := Member_Attribute.Last (Old.Attributes);
         begin
            Info.Access_Flags := Old.Access_Flags;
            Info.Name_Index   := Old.Name_Index;
            Info.Descriptor_Index := Old.Descriptor_Index;
            if Nb_Attr >= 0 then
               Member_Attribute.Allocate_Fixed_Table (Info.Attributes,
                                                      Nb_Attr + 1);
               for I in 0 .. Nb_Attr loop

                  declare
                     Old_Attr : constant Member_Attribute_Info
                       := Member_Attribute.Get (Old.Attributes, I);
                     Attr     : Member_Attribute_Info (Old_Attr.Kind);
                  begin
                     Attr.Attribute_Name_Index
                       := Old_Attr.Attribute_Name_Index;

                     case Old_Attr.Kind is
                        when Attr_Constant_Value =>
                           Attr.Constant_Value_Index
                             := Old_Attr.Constant_Value_Index;
                           Attr.Attribute_Length := 2;

                        when Attr_Exceptions =>
                           Attr.Exception_Index_Table
                             := Old_Attr.Exception_Index_Table;
                           Attr.Attribute_Length :=
                             Old_Attr.Attribute_Length;

                        when Attr_Code =>
                           Attr.Max_Stack        := Old_Attr.Max_Stack;
                           Attr.Max_Locals       := Old_Attr.Max_Locals;
                           Attr.Code             := Old_Attr.Code;
                           Attr.Exception_Table  := Old_Attr.Exception_Table;
                           --  Attributes not set (because it is stripped)
                           Attr.Attribute_Length := U4
                             (12 +
                              Code_Array.Length (Attr.Code) +
                              Handler.Length (Attr.Exception_Table) * 8);

                        when others =>
                           Attr.Attribute_Length := 0;
                           null;
                     end case;
                     Member_Attribute.Put (Info.Attributes, I,
                                           Attr);
                  end;
               end loop;  -- for I in 1 .. Nb_Attr
            end if; -- Nb_Attr /= 0

            Member.Put (CF2.Methods, K, Info);

         end;
      end loop;

      --  Finally, create a strip version of the class file
      declare
         Bytes_Out : J_Types.Stream_Of_U1 (1 .. JVM_File.Compute_Size (CF2));
      begin
         JVM_File.Write (CF => CF2, Stream => Bytes_Out);
         return Bytes_Out;
      end;

   end Process_Single_Class_File;

begin
   if Ada.Command_Line.Argument_Count = 0 then
      Write_Usage;
      return;
   end if;

   J_Utils.Set_Message_Prefix ("jvmstrip");

   --  Process the command line switches

   loop
      case GNAT.Command_Line.Getopt ("v") is
         when ASCII.NUL =>
            exit;

         when 'v' =>
            Verbose_Mode := True;

         when others =>
            Write_Usage;
      end case;
   end loop;

   loop
      declare
         File_Name : constant String
           := GNAT.Command_Line.Get_Argument (Do_Expansion => True);
         Bytes     : J_Types.Stream_Of_U1_Ptr;
      begin

         exit when File_Name = "";

         J_Utils.Set_Message_Prefix ("jvmstrip");

         JVM_File.Set_Water_Mark;

         if Verbose_Mode then
            Ada.Text_IO.Put_Line ("  Stripping " & File_Name);
         end if;

         --  Read the input file
         Bytes := J_Basics.Get_Stream_Of_U1 (File_Name);

         --  For a zip file, extract each of them
         if J_Zip.Has_Zip_Format (Bytes.all) then

            declare
               Dir : constant J_Zip.Archive_Directory
                 := J_Zip.Get_Archive_Dir (Bytes.all);
               Arc : J_Zip.Zip_Archive (Dir'Length);
            begin
               J_Zip.Create_New_Archive (File_Name, Arc);
               for K in Dir'Range loop
                  declare
                     Name : constant String := J_Basics.To_String
                       (Bytes (Dir (K).Name_First .. Dir (K).Name_Last));
                  begin

                     J_Utils.Set_Message_Prefix (Name);

                     --  Process only uncompressed .class files
                     if  Name (Name'Last) = '/' then
                        if Verbose_Mode then
                           Ada.Text_IO.Put_Line ("  Directory : " & Name);
                        end if;
                        J_Zip.Add_Stream_To_Archive
                          (Bytes (Dir (K).First .. Dir (K).Last),
                           Bytes (Dir (K).Name_First .. Dir (K).Name_Last),
                           Arc);

                     elsif Ada.Strings.Fixed.Tail (Name, 6) /= ".class"
                       or  Dir (K).Encrypted  then
                        if Verbose_Mode then
                           Ada.Text_IO.Put_Line ("  skipping : " & Name);
                        end if;
                        J_Zip.Add_Stream_To_Archive
                          (Bytes (Dir (K).First .. Dir (K).Last),
                           Bytes (Dir (K).Name_First .. Dir (K).Name_Last),
                           Arc);

                     elsif Dir (K).Last < Dir (K).First then
                        if Verbose_Mode then
                           Ada.Text_IO.Put_Line ("  skipping empty file "
                                                 & Name);
                        end if;
                        J_Zip.Add_Stream_To_Archive
                          (Bytes (Dir (K).First .. Dir (K).Last),
                           Bytes (Dir (K).Name_First .. Dir (K).Name_Last),
                           Arc);

                     else
                        if Verbose_Mode then
                           Ada.Text_IO.Put_Line ("  stripping " & Name);
                        end if;

                        J_Zip.Add_Stream_To_Archive
                          (Process_Single_Class_File
                           (Bytes (Dir (K).First .. Dir (K).Last)),
                           Bytes (Dir (K).Name_First .. Dir (K).Name_Last),
                           Arc);
                     end if;
                  end;
               end loop;

               J_Zip.Close_Archive (Arc);

            end;
         else  -- not Has_Zip_Format

            J_Utils.Set_Message_Prefix (File_Name);
            J_Basics.Put_Stream_Of_U1
              (Process_Single_Class_File (Bytes.all), File_Name);

         end if;

      end;

      JVM_File.Free_To_Next_Water_Mark;

   end loop;

exception
   when GNAT.Command_Line.Invalid_Switch =>
      Ada.Text_IO.Put_Line ("Invalid Switch "
                            & GNAT.Command_Line.Full_Switch);
      Write_Usage;

   when J_Utils.Fatal_Exception =>
      null;
end Jvmstrip;
