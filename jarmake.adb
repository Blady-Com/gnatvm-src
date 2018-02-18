------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J A R M A K E                               --
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

with Ada.Command_Line;            use Ada.Command_Line;
with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNAT.Command_Line;
with GNAT.Spitbol.Table_Boolean;
with Gnatvsn;
with J_Basics;                    use J_Basics;
with J_List;
with J_Utils;                     use J_Utils;
with J_Types;                     use J_Types;
with J_Zip;
with JVM_File;                    use JVM_File;
with Osint;
with Sdefault;

procedure Jarmake is

   ------------------------
   -- Local Types & Data --
   ------------------------

   type String_Ptr is access String;

   Lib       : constant String := Sdefault.Object_Dir_Default_Name.all;
   --  Something like "/usr/bin/jgnat-3.5/lib/jgnat/adalib/"

   JGNAT_JAR : constant String := Lib (Lib'First .. Lib'Last - 8) & ".jar";
   --  Something like "/usr/bin/jgnat-3.5/lib/jgnat.jar"

   File_Found : Boolean := False;
   --  Set if there is at least one file on the command line

   ---------------------
   -- Local Variables --
   ---------------------

   JAR_Archive : J_Zip.Zip_Archive (5000);
   --  The output archive. 5000 class file max should be plenty.

   ----------------------
   -- Options Handling --
   ----------------------

   Keep_Going : Boolean := False;
   --  When set keep going even if not all .class file can be located

   Look_For_Java_API_Classes : Boolean := False;
   --  When set look also for classes in the Java API

   Look_In_JGNAT_Lib : Boolean := True;
   --  When set do include the .class files in the JGNAT lib in the zip search
   --  path.

   Add_Main_Class_Attribute  : Boolean := False;
   --  When set add a manifest with a Main-Class attribute to the jar archive.
   --  The attribute names the first class processed which has a main method.
   --  Add_Main_Class_Attribute is reset when the first such class is processed
   --  thus disabling further discovery of main methods

   Output_File : String_Ptr := new String'("gnapplet.jar");
   --  Name of the output JAR file

   Quiet_Mode : Boolean := False;
   --  When set, do not output any warning messages

   Verbose_Mode : Boolean := False;
   --  When set, be verbose in the output

   --------------------------
   -- Search File Handling --
   --------------------------

   --  When an archive is specified via a -L switch the whole archive is kept
   --  in memory, uncompressed. When the archive is read, its content is parsed
   --  and the archive directory is kept so that looking for a file in the
   --  archive is fast.

   package Archive_Search is new J_List (J_Utils.Archive_Directory_Access);

   Zip_Search_List : Archive_Search.List := Archive_Search.Empty_List;
   --  List of archives we must look in to locate .class files

   procedure Search_Classes_In (Zip : String);
   --  When looking for a class file search also in archive Zip

   ----------------------------
   -- File Location Routines --
   ----------------------------

   No_Bytes : constant Stream_Of_U1 (1 .. 0) := (others => 0);

   function Find_And_Add_File (File_Name : String) return Stream_Of_U1;
   --  Locate the file whose name is File_Name and add its contents to the JAR
   --  file. The file is searched in the following order:
   --
   --    (1) Look in the current directory first. If the file is of the form
   --        "a/b/Bar.class" look into <current-directory>/a/b/Bar.class.
   --
   --    (2) Then look in each of the archives specified by the -Lzip switches
   --
   --    (3) Then look into jgnat.jar, the JGNAT library, unless -n was set
   --
   --  If no file is found and Keep_Going is True then a warning is emitted and
   --  No_Bytes is returned. If Keep_Going is False then halt execution and
   --  emit an error message.

   -------------------
   -- Class Marking --
   -------------------

   --  The following table is use to mark class files as they are processed to
   --  avoid that a class file gets added twice to the final JAR file.

   package Name_Table renames GNAT.Spitbol.Table_Boolean;
   Processed_Classes : Name_Table.Table (4096);

   procedure Mark (Class_Name : String);
   --  Mark the class whose full name is Class_Name as having been processed

   function Is_Marked (Class_Name : String) return Boolean;
   --  Returns True if Class_Name was already entered in table
   --  Processed_Classes, False otherwise.

   -------------------
   -- Main Routines --
   -------------------

   procedure Process_File (File_Name : String);
   --  Add File_Name to the JAR file if not there already and call
   --  Process_Class on it if File_Name denotes the name of a class file.

   procedure Process_Class (Class_Name : String);
   --  Add the contents of class file whose name is Class_Name to the JAR
   --  archive, if not already there, and calls Process_Class below on
   --  the class file.  If Class_Name is the name of a class file in the
   --  Java API then skip it unless Look_For_Java_API_Classes is set.

   procedure Process_Class (CF : Class_File);
   --  Traverses CF constant pool recursively calling Process_Class on
   --  each class is referenced there.
   --  Calls Add_Manifest for the 1st class with a main method that was
   --  added to the JAR

   procedure Add_Manifest (Main_Class_Name : String);
   --  Add a manifest to the JAR archive

   procedure Process_Switches;
   --  Parse the switches on the command line

   procedure Write_Usage;
   --  Print program options

   ---------------
   -- Utilities --
   ---------------

   procedure Add_To_JAR (File_Name : String; Stream : Stream_Of_U1);
   --  Adds Stream with namd File_Name to Jar_Archive

   function Is_Class_Name (File_Name : String) return Boolean;
   --  Retuns True if File_Name ends with a .class suffix

   function In_Java_API (Name : String) return Boolean;
   --  Returns True if Name is the name of a class file inside the Java API.
   --  Name is of the form "java/lang/Thread".

   procedure Msg (S : String);
   --  Prints S if not Quiet_Mode

   ----------------
   -- Add_To_JAR --
   ----------------

   procedure Add_To_JAR (File_Name : String; Stream : Stream_Of_U1) is
   begin
      if Verbose_Mode then
         Ada.Text_IO.Put_Line ("  Adding: " & File_Name);
      end if;

      J_Zip.Add_Stream_To_Archive
        (Stream    => Stream,
         File_Name => To_Stream_Of_U1 (File_Name),
         Archive   => JAR_Archive);
   end Add_To_JAR;

   -----------------------
   -- Find_And_Add_File --
   -----------------------

   function Find_And_Add_File (File_Name : String) return Stream_Of_U1 is
      use Archive_Search;

      procedure Free is
         new Ada.Unchecked_Deallocation (Stream_Of_U1, Stream_Of_U1_Ptr);

      --  Local Variables

      Iterator  : Archive_Search.List_Iterator;
      Bytes     : Stream_Of_U1_Ptr;

   --  Beginnging of Find_And_Add_File

   begin
      --  First see if the file can be located from the current directory

      Bytes := J_Basics.Get_Stream_Of_U1 (File_Name, Dont_Fail => True);

      if Bytes /= null then
         declare
            Bytes_All : constant Stream_Of_U1 := Bytes.all;
         begin
            Free (Bytes);
            Add_To_JAR (File_Name, Bytes_All);
            return Bytes_All;
         end;
      end if;

      --  Otherwise look in the archives specified

      Associate (Zip_Search_List, Iterator);
      while not Is_Last (Iterator) loop
         declare
            Zip : constant J_Utils.Archive_Directory_Access := Get (Iterator);
         begin
            for J in Zip.Archive'Range loop
               declare
                  F    : J_Zip.File_Info renames Zip.Archive (J);
                  Name : constant String :=
                    To_String (Zip.Stream (F.Name_First .. F.Name_Last));
               begin
                  if Name = File_Name then
                     Add_To_JAR (File_Name, Zip.Stream (F.First .. F.Last));
                     return Zip.Stream (F.First .. F.Last);
                  end if;
               end;
            end loop;
         end;
         Next (Iterator);
      end loop;

      --  If we get here we have not found the file

      if Keep_Going then
         Msg  ("Warning: Can't find: " & File_Name & ". File ignored.");
      else
         Osint.Fail ("Can't find: " & File_Name);
      end if;

      return No_Bytes;
   end Find_And_Add_File;

   -------------------
   -- Is_Class_Name --
   -------------------

   function Is_Class_Name (File_Name : String) return Boolean is
   begin
      for J in reverse File_Name'Range loop
         if File_Name (J) = '.'
           and then File_Name (J .. File_Name'Last) = ".class"
         then
            return True;
         end if;
      end loop;

      return False;
   end Is_Class_Name;

   -----------------
   -- In_Java_API --
   -----------------

   function In_Java_API (Name : String) return Boolean is
   begin
      if Look_For_Java_API_Classes then
         return False;
      end if;

      return
        (Name'Length > 5
         and then Name (Name'First .. Name'First + 4) = "java/")

        or else

        (Name'Length > 6
         and then Name (Name'First .. Name'First + 5) = "javax/")

        or else

        (Name'Length > 8
         and then Name (Name'First .. Name'First + 7) = "org/omg/");
   end In_Java_API;

   ---------------
   -- Is_Marked --
   ---------------

   function Is_Marked (Class_Name : String) return Boolean is
      use Name_Table;
   begin
      return Get (Processed_Classes, Class_Name);
   end Is_Marked;

   ----------
   -- Mark --
   ----------

   procedure Mark (Class_Name : String) is
      use Name_Table;
   begin
      pragma Assert (not Present (Processed_Classes, Class_Name));
      Set (Processed_Classes, Class_Name, True);
   end Mark;

   ---------
   -- Msg --
   ---------

   procedure Msg (S : String) is
   begin
      if not Quiet_Mode then
         Ada.Text_IO.Put_Line (S);
      end if;
   end Msg;

   -------------------
   -- Process_Class --
   -------------------

   procedure Process_Class (Class_Name : String) is
   begin
      if In_Java_API (Class_Name) then
         return;

      --  Check that the class name is not an array

      elsif Class_Name (Class_Name'First) = '[' then
         return;

      elsif Is_Marked (Class_Name) then
         return;

      else
         Mark (Class_Name);
      end if;

      declare
         Bytes : constant Stream_Of_U1 :=
                   Find_And_Add_File (Class_Name & ".class");
      begin
         if Bytes = No_Bytes then
            return;
         end if;

         JVM_File.Set_Water_Mark;
         Process_Class (JVM_File.Read (Bytes, Check => True));
         JVM_File.Free_To_Next_Water_Mark;

      exception
         when others =>
            Osint.Fail (Class_Name & ": Bad class file format");
      end;
   end Process_Class;

   procedure Process_Class (CF : Class_File) is
      use CP;

      Pool : constant CP.Table := CF.Constant_Pool;

      function Get_Name (K : CP_Index_Utf8) return String;
      --  Returns the string at entry K in the constant pool

      procedure Process_Descriptor (D : String);
      --  Scans descriptor D looking for class iles and processing each one it
      --  finds.

      ------------------------
      -- Process_Descriptor --
      ------------------------

      procedure Process_Descriptor (D : String) is
         Index : Natural := D'First;
         Start : Natural;

      begin
         while Index <= D'Last loop
            case D (Index) is
               when 'L' =>
                  Start := Index + 1;
                  while D (Index) /= ';' loop
                     Index := Index + 1;
                  end loop;
                  Index := Index + 1;
                  Process_Class (D (Start .. Index - 2));

               when others =>
                  Index := Index + 1;
            end case;
         end loop;
      end Process_Descriptor;

      --------------
      -- Get_Name --
      --------------

      function Get_Name (K : CP_Index_Utf8) return String is
      begin
         return To_String (Get_Utf8 (Pool, K));
      end Get_Name;

      F : Member_Info;
      M : Member_Info;

   --  Beginning of Process_Class

   begin
      --  First Loop through the class fields and methods to process the
      --  classes they reference.

      for K in 0 .. Member.Last (CF.Fields) loop
         F := Member.Get (CF.Fields, K);
         Process_Descriptor (Get_Name (F.Descriptor_Index));
      end loop;

      for K in 0 .. Member.Last (CF.Methods) loop
         M := Member.Get (CF.Methods, K);
         Process_Descriptor (Get_Name (M.Descriptor_Index));

         --  check if the class should be added to the manifest
         if Add_Main_Class_Attribute and then
           Get_Name (M.Name_Index) = "main" and then
           Get_Name (M.Descriptor_Index) = "([Ljava/lang/String;)V"
         then
            declare
               E : constant CP_Info := Get (Pool, CF.This_Class);
            begin
               --  disable further discovery of main methods
               Add_Main_Class_Attribute := False;

               --  add the manifest to the jar archive
               Add_Manifest (Get_Name (E.Class_Name_Index));
            end;
         end if;
      end loop;

      --  Finally loop through the symbol table entries and process all the
      --  referenced class files.

      for K in 1 .. Last (Pool) loop
         declare
            E : constant CP_Info := Get (Pool, K);
         begin
            case E.Tag is
               when CONSTANT_Class =>
                  Process_Class (Get_Name (E.Class_Name_Index));

               when CONSTANT_Name_And_Type =>
                  Process_Descriptor (Get_Name (E.Descriptor_Index));

               when others =>
                  null;
            end case;
         end;
      end loop;
   end Process_Class;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File (File_Name : String) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Stream_Of_U1, Stream_Of_U1_Ptr);

      Bytes : Stream_Of_U1_Ptr;

   begin
      --  Get the file from the current directory

      Bytes := J_Basics.Get_Stream_Of_U1 (File_Name);

      --  If we get here we have found a file

      pragma Assert (Bytes /= null);

      --  IS it a class file ?

      if Is_Class_Name (File_Name) then
         declare
         begin
            declare
               CF : constant Class_File := JVM_File.Read (Bytes.all, True);
               T  : constant CP.Table   := CF.Constant_Pool;
               CI : constant CP_Info    := CP.Get (T, CF.This_Class);
               CN : constant Utf8.Table := Get_Utf8 (T, CI.Class_Name_Index);

               Class_Name : constant String := To_String (CN);

            begin
               if In_Java_API (Class_Name) then
                  return;
               end if;

               if Is_Marked (Class_Name) then
                  return;
               else
                  Mark (Class_Name);
               end if;

               declare
                  Bytes_All : constant Stream_Of_U1 := Bytes.all;
               begin
                  Free (Bytes);
                  Add_To_JAR (Class_Name & ".class", Bytes_All);
               end;

               JVM_File.Set_Water_Mark;
               Process_Class (CF);
               JVM_File.Free_To_Next_Water_Mark;
            end;

         exception
            when others =>
               Osint.Fail (File_Name & ": Bad class file format");
         end;

      --  Otherwise it's a regular class

      else
         if Is_Marked (File_Name) then
            return;
         else
            Mark (File_Name);
         end if;

         declare
            Bytes_All : constant Stream_Of_U1 := Bytes.all;
         begin
            Free (Bytes);
            Add_To_JAR (File_Name, Bytes_All);
         end;
      end if;
   end Process_File;

   ----------------------
   -- Process_Switches --
   ----------------------

   procedure Process_Switches is
   begin
      loop
         case GNAT.Command_Line.Getopt ("L! j k m n o: q v") is
            when ASCII.NUL =>
               exit;

            when 'L' =>
               Search_Classes_In (GNAT.Command_Line.Parameter);

            when 'j' =>
               Look_For_Java_API_Classes := True;

            when 'k' =>
               Keep_Going := True;

            when 'm' =>
               Add_Main_Class_Attribute := True;

            when 'n' =>
               Look_In_JGNAT_Lib := False;

            when 'o' =>
               Output_File := new String'(GNAT.Command_Line.Parameter);

            when 'q' =>
               Quiet_Mode := True;

            when 'v' =>
               Verbose_Mode := True;

            when others =>
               Write_Usage;
               return;
         end case;
      end loop;
   end Process_Switches;

   -----------------------
   -- Search_Classes_In --
   -----------------------

   procedure Search_Classes_In (Zip : String) is
      Bytes : Stream_Of_U1_Ptr;
      --  Contains the bytes of the input archive

   begin
      Bytes := J_Basics.Get_Stream_Of_U1 (Zip);

      declare
         use J_Zip;
         Archive : constant J_Utils.Archive_Directory_Access :=
           (Stream  => Bytes,
            Archive => new Archive_Directory'(Get_Archive_Dir (Bytes.all)));
      begin
         Archive_Search.Append (Archive, Zip_Search_List);
      end;

   exception
      when J_Zip.Bad_Zip_Archive =>
         Osint.Fail (Zip & " is not a zip archive");

      when J_Zip.Compressed_Zip_Archive =>
         Osint.Fail
           ("Compressed archive: " &
            Zip & " jarmake can only handle uncompressed archives.");
   end Search_Classes_In;

   ------------------
   -- Add Manifest --
   ------------------

   procedure Add_Manifest (Main_Class_Name : String) is
      Bytes : constant Stream_Of_U1
       := To_Stream_Of_U1 ("Manifest-Version: 1.0"  & ASCII.LF &
                           "Main-Class: " & Main_Class_Name & ASCII.LF);
   begin
      Add_To_JAR ("META-INF/MANIFEST.MF", Bytes);
      if Verbose_Mode then
         Put_Line ("added manifest");
      end if;
   end Add_Manifest;

   -----------------
   -- Write_Usage --
   -----------------

   procedure Write_Usage is
   begin
      Put_Line ("JARMAKE " & Gnatvsn.Gnat_Version_String
                & " Copyright 1998-1999, Ada Core Technologies Inc.");
      Put_Line ("Usage: jarmake [switches] file [file .. file]");
      New_Line;
      Put_Line ("  file   A .class file or any other file");
      New_Line;
      Put_Line ("Possible switches:");
      Put_Line ("  -Lzip  Look for .class files in archive ""zip""");
      Put_Line ("  -j     Do not skip .class files in the Java API");
      Put_Line ("  -k     Keep going even if you don't find all .class files");
      Put_Line ("  -m     Add a manifest with a Main-Class attribute");
      Put_Line ("  -n     Do not include .class files in the JGNAT library");
      Put_Line ("  -o jar The output archive is called ""jar""");
      Put_Line ("  -q     Quiet");
      Put_Line ("  -v     Verbose");
      New_Line;
   end Write_Usage;

--  Start of processing of Jarmake

begin
   Set_Message_Prefix (Ada.Command_Line.Command_Name);

   Process_Switches;

   --  Add the JGNAT library to the list of ZIP archives searched in last
   --  position if switch -n was not set.

   if Look_In_JGNAT_Lib then
      Search_Classes_In (JGNAT_JAR);
   end if;

   --  Process the files on the command line

   loop
      declare
         use GNAT.Command_Line;
         S : constant String := Get_Argument (Do_Expansion => True);

      begin
         exit when S'Length = 0;

         --  If this is the first file we find on the command line, create the
         --  JAR file.

         if not File_Found then
            File_Found := True;
            J_Zip.Create_New_Archive (Output_File.all, JAR_Archive);
         end if;

         Process_File (S);
      end;
   end loop;

   if not File_Found then
      Write_Usage;
   else
      J_Zip.Close_Archive (JAR_Archive);
   end if;

exception
   when GNAT.Command_Line.Invalid_Switch =>
      Print_Msg ("Invalid Switch -" & GNAT.Command_Line.Full_Switch);
      Write_Usage;

   when GNAT.Command_Line.Invalid_Parameter =>
      Put_Line ("No parameter for -" & GNAT.Command_Line.Full_Switch);
      Write_Usage;

   when Fatal_Exception =>
      Set_Exit_Status (Failure);

   when others =>
      Print_Msg ("*Internal Error* Please report it to report@gnat.com");
      Set_Exit_Status (Failure);

end Jarmake;
