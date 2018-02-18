------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J _ U T I L S                               --
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
with Ada.Unchecked_Deallocation;
with GNAT.Command_Line;
with GNAT.Heap_Sort;
with J_Basics;    use J_Basics;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body J_Utils is

   use Class_Attribute;
   use Code_Attribute;
   use CP;
   use Line;
   use Member_Attribute;
   use Variable;

   ----------------
   -- Local Data --
   ----------------

   type String_Ptr is access String;
   Message_Prefix : String_Ptr;
   --  Prefix string that is displayed before any error message displayed by
   --  routines Fatal_Error and Print_Msg.

   Current_Archive : Archive_Directory_Access := (null, null);
   --  The current archive being processed by Process_Class, if any

   ----------------------------
   -- Is_Directory_Separator --
   ----------------------------

   function Is_Directory_Separator (C : Character) return Boolean;
   --  Returns True if C is a directory separator

   function Is_Directory_Separator (C : Character) return Boolean is
   begin
      return C = '/' or else C = Directory_Separator;
   end Is_Directory_Separator;

   ------------------
   -- Command_Line --
   ------------------

   package body Command_Line is

      --------------------
      -- Local Routines --
      --------------------

      function Equal (S1, S2 : String) return Boolean;
      --  Returns True if S1 and S2 are equal except for directory
      --  separator characters that may differ. For instance
      --  Equal ("java/lang/Object.class", "java\lang\Object.class")
      --  returns True.

      procedure Verbose_Msg (S : String);
      --  Prints S to stdout if Verbose is True

      -----------
      -- Equal --
      -----------

      function Equal (S1, S2 : String) return Boolean is
         K1 : Integer;
         K2 : Integer;

      begin
         if S1'Length /= S2'Length then
            return False;
         end if;

         K1 := S1'First;
         K2 := S2'First;

         for J in 1 .. S1'Length loop
            if S1 (K1) /= S2 (K2) then
               if not (Is_Directory_Separator (S1 (K1))
                       and then Is_Directory_Separator (S2 (K2)))
               then
                  return False;
               end if;
            end if;

            K1 := K1 + 1;
            K2 := K2 + 1;
         end loop;

         return True;
      end Equal;

      -------------
      -- Process --
      -------------

      procedure Process is
         use Ada.Text_IO;
         use Ada.Command_Line;

         Files_Found : Boolean := False;

      begin
         Set_Message_Prefix (Ada.Command_Line.Command_Name);

         Process_Switches;

         --  Process the files on the command line

         loop
            declare
               S : constant String
                 := GNAT.Command_Line.Get_Argument (Do_Expansion => True);
            begin
               exit when S'Length = 0;
               Files_Found := True;
               Process_File (S);
            end;
         end loop;

         if not Files_Found then
            Write_Usage;
            return;
         end if;

      exception
         when GNAT.Command_Line.Invalid_Switch =>
            Print_Msg ("Invalid Switch -" & GNAT.Command_Line.Full_Switch);
            Write_Usage;

         when GNAT.Command_Line.Invalid_Parameter =>
            Put_Line ("No parameter or invalid parameter for -" &
                      GNAT.Command_Line.Full_Switch);
            Write_Usage;

         when Fatal_Exception =>
            Set_Exit_Status (Failure);

         when others =>
            Print_Msg ("*Internal Error* Please report it to report@gnat.com");
            Set_Exit_Status (Failure);
      end Process;

      ------------------
      -- Process_File --
      ------------------

      procedure Process_File (File_Name : String) is
         use Ada.Command_Line;
         use Ada.Strings.Fixed;

         Bytes : Stream_Of_U1_Ptr;
         --  Contains the bytes of the input file or null if no file is found

         procedure Free is
           new Ada.Unchecked_Deallocation (Stream_Of_U1, Stream_Of_U1_Ptr);
         procedure Free is
           new Ada.Unchecked_Deallocation
           (J_Zip.Archive_Directory, Zip_Archive_Access);

      begin
         Set_Message_Prefix (Ada.Command_Line.Command_Name);
         JVM_File.Set_Water_Mark;

         declare
            File_Name_End : Integer := File_Name'Last;
            --  If File_Name is of the form "../zipfile/../xy.class" it points
            --  to the last character of the zip file in File_Name.

         begin
            Set_Message_Prefix (File_Name);

            --  First see if the file whose name is File_name exists

            Bytes := J_Basics.Get_Stream_Of_U1 (File_Name, Dont_Fail => True);

            --  If the file does not exist try to add a ".class" suffix

            if Bytes = null then
               Bytes := Get_Stream_Of_U1 (File_Name & ".class", True);
            end if;

            --  See if File_Name is of the form "../zipfile/../xy.class"

            if Bytes = null and then Tail (File_Name, 6) = ".class" then
               for E in File_Name'First .. File_Name'Last - 6 loop
                  if Is_Directory_Separator (File_Name (E + 1)) then
                     Bytes :=
                       Get_Stream_Of_U1
                         (File_Name (File_Name'First .. E), True);
                     File_Name_End := E;
                     exit when Bytes /= null;
                  end if;
               end loop;
            end if;

            --  If File_Name did not specify any existing file halt everything

            if Bytes = null then
               Fatal_Error (File_Name & " not found");
            end if;

            Verbose_Msg (File_Name (File_Name'First .. File_Name_End));

            if not J_Zip.Has_Zip_Format (Bytes.all) then
               Process_Class (Bytes.all);

            else  -- zip format
               Current_Archive := (Stream  => Bytes,
                                   Archive =>
                                     new J_Zip.Archive_Directory'
                                       (J_Zip.Get_Archive_Dir (Bytes.all)));

               --  Sort the archive to ensure that each nested package appears
               --  after its outer package
               Sort_Archive_Directory (Current_Archive);

               for K in Current_Archive.Archive'Range loop
                  declare
                     F : J_Zip.File_Info renames Current_Archive.Archive (K);
                     --  Current file in the zip archive being processed

                     Expected_Name : constant String :=
                       File_Name (File_Name_End + 2 .. File_Name'Last);
                     --  Name of the expected .class file in the zip file. This
                     --  name is empty if there is no expected .class file but
                     --  we have been requested to process all .class files.

                     Process_Only_1_File : constant Boolean
                       := Expected_Name'Length /= 0;
                     --  This boolean is True if we have been requested to
                     --  process a single .class file within the Zip file.

                     Name : constant String :=
                       To_String (Bytes (F.Name_First .. F.Name_Last));
                     --  Name of the current file or directory being looked at
                     --  in the Zip file.

                  begin
                     Set_Message_Prefix (Name);

                     if Is_Directory_Separator (Name (Name'Last)) then
                        if Process_Only_1_File then

                           --  Process only the directories corresponding to
                           --  the package hierarchy of the requested class.

                           if Name'Length < Expected_Name'Length
                             and then Name = Expected_Name
                             (Expected_Name'First ..
                              Expected_Name'First + Name'Length - 1)
                           then
                              Process_Directory
                                (Name (Name'First .. Name'Last - 1));
                              --  Send the directory name without the trailing
                              --  directory separator
                           else
                              Verbose_Msg ("  skipping directory " & Name);
                           end if;

                        else
                           Process_Directory
                             (Name (Name'First .. Name'Last - 1));
                           --  Send the directory name without the trailing
                           --  directory separator
                        end if;

                     elsif Tail (Name, 6) /= ".class" then
                        Verbose_Msg ("  skipping " & Name);

                     else  --  it's a .class file
                        if Process_Only_1_File
                          and then not Equal (Expected_Name, Name)
                        then
                           Verbose_Msg ("  skipping class file " & Name);

                        elsif F.First > F.Last then
                           Verbose_Msg ("  skipping empty file " & Name);

                        elsif F.Encrypted then
                           Verbose_Msg ("  skipping encrypted file " & Name);

                        elsif F.Compressed then
                           Verbose_Msg ("  skipping compressed file " & Name);

                        else
                           Verbose_Msg ("  -> " & Name);
                           JVM_File.Set_Water_Mark;
                           Process_Class (Bytes (F.First .. F.Last));
                           JVM_File.Free_To_Next_Water_Mark;
                        end if;
                     end if;
                  end;
               end loop;

               Free (Current_Archive.Archive);
               Current_Archive := (null, null);
            end if;

            Free (Bytes);

         exception
            when J_Zip.Bad_Zip_Archive =>
               Free (Bytes);
               Fatal_Error (File_Name & " is a bad zip archive");

            when J_Zip.Compressed_Zip_Archive =>
               Free (Bytes);
               Fatal_Error (File_Name & " is compressed. Uncompress it.");

            when others =>
               Free (Bytes);
               Fatal_Error ("Error while parsing class file in " & File_Name);
         end;

         JVM_File.Free_To_Next_Water_Mark;

      exception
         when Fatal_Exception =>
            Set_Exit_Status (Failure);

         when others =>
            Set_Message_Prefix (Ada.Command_Line.Command_Name);
            Print_Msg ("*Internal Error* Please report it to report@gnat.com");
            Set_Exit_Status (Failure);
      end Process_File;

      -----------------
      -- Verbose_Msg --
      -----------------

      procedure Verbose_Msg (S : String) is
      begin
         if Verbose then
            Ada.Text_IO.Put_Line (S);
         end if;
      end Verbose_Msg;

   end Command_Line;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error (S : String) is
      use Ada.Text_IO;
   begin
      if Message_Prefix /= null then
         Put (Message_Prefix.all & ": ");
      end if;
      Put_Line (S);
      raise Fatal_Exception;
   end Fatal_Error;

   ------------------------
   -- Get_Code_Attribute --
   ------------------------

   function Get_Code_Attribute
     (M    : Member_Info)
      return Member_Attribute_Info
   is
      MA : Member_Attribute_Info;

   begin
      for K in 0 .. Last (M.Attributes) loop
         MA := Get (M.Attributes, K);
         if MA.Kind = Attr_Code then
            return MA;
         end if;
      end loop;

      pragma Assert (False);
      return MA;
   end Get_Code_Attribute;

   -------------------------
   -- Get_Current_Archive --
   -------------------------

   function Get_Current_Archive return Archive_Directory_Access is
   begin
      return Current_Archive;
   end Get_Current_Archive;

   ----------------------------
   -- Sort_Archive_Directory --
   ----------------------------

   procedure Sort_Archive_Directory (A : Archive_Directory_Access)
   is
      procedure Xchg (Op1, Op2 : Natural);
      --  Exchange the File_Info at position Op1 and Op2

      function Lt (Op1, Op2 : Natural) return Boolean;
      --  Compares the name found in File_Info at position Op1 and Op2
      --  Put each nested class before its parent class otherwise use
      --  the lexicographic order

      Bytes   : constant Stream_Of_U1_Ptr := A.Stream;
      Archive : J_Zip.Archive_Directory renames A.Archive.all;

      ----------
      -- Xchg --
      ----------

      procedure Xchg (Op1, Op2 : Natural) is
         Tmp : J_Zip.File_Info;
      begin
         Tmp := Archive (Op1);
         Archive (Op1) := Archive (Op2);
         Archive (Op2) := Tmp;
      end Xchg;

      --------
      -- Lt --
      --------

      function Lt (Op1, Op2 : Natural) return Boolean
      is
         Name1 : String :=
           To_String (Bytes (Archive (Op1).Name_First ..
                             Archive (Op1).Name_Last));
         Name2 : String :=
           To_String (Bytes (Archive (Op2).Name_First ..
                             Archive (Op2).Name_Last));
      begin
         --  Names are using the following scheme:

         --   (1)   java/awt/RenderingHints.class
         --   (2)   java/awt/RenderingHints$Key.class

         --  We want (2) to be lower than (1) but
         --  Character'Pos ('$') < Character'Pos ('.')
         --  so we replace each '$' by a '/' since
         --  Character'Pos ('/') > Character'Pos ('.')

         for I in Name1'Range loop
            if Name1 (I) = '$' then
               Name1 (I) := '/';
            end if;
         end loop;

         for I in Name2'Range loop
            if Name2 (I) = '$' then
               Name2 (I) := '/';
            end if;
         end loop;

         --  Now just use the classical lexicographic order comparison
         return Name1 < Name2;
      end Lt;

   begin
      GNAT.Heap_Sort.Sort (Archive'Length, Xchg'Unrestricted_Access,
                             Lt'Unrestricted_Access);
   end Sort_Archive_Directory;

   -----------------------
   -- Get_PC_To_Src_Map --
   -----------------------

   function Get_PC_To_Src_Map (C : Code_Attribute.Table) return PC_Src_Map is
      Last_PC : Instruction_Index := 0;

      CA  : Code_Attribute_Info;
      LI  : Line_Info;

   begin
      --  First compute the size of the Src_Line_Map table

      for J in 0 .. Last (C) loop
         CA :=  Get (C, J);

         if CA.Kind = Attr_Line_Number_Table then
            for K in 0 .. Last (CA.Line_Number_Table) loop

               LI := Get (CA.Line_Number_Table, K);
               if LI.Start_PC > Last_PC then
                  Last_PC := LI.Start_PC;
               end if;
            end loop;
         end if;
      end loop;

      declare
         Map : PC_Src_Map (0 .. Last_PC) := (others => 0);

      begin
         for J in 0 .. Last (C) loop
            CA := Get (C, J);

            if CA.Kind = Attr_Line_Number_Table then
               for K in 0 .. Last (CA.Line_Number_Table) loop

                  LI := Get (CA.Line_Number_Table, K);
                  Map (LI.Start_PC) := LI.Line_Number;
               end loop;
            end if;
         end loop;

         return Map;
      end;
   end Get_PC_To_Src_Map;

   ------------------
   -- Get_Var_Info --
   ------------------

   function Get_Var_Info
     (C    : Code_Attribute.Table;
      Var  : Local_Variable_Index;
      PC   : Byte_Index)
      return Variable_Info
   is
      CA    : Code_Attribute_Info;
      VI    : Variable_Info;
      Empty : Variable_Info;

   begin
      for J in 0 .. Last (C) loop
         CA := Get (C, J);

         if CA.Kind = Attr_Local_Variable_Table then
            for K in 0 .. Last (CA.Local_Variable_Table) loop
               VI := Get (CA.Local_Variable_Table, K);

               if VI.Index = Var
                 and then
                   PC in VI.Start_PC .. VI.Start_PC + Byte_Index (VI.Length)
               then
                  return VI;
               end if;
            end loop;
         end if;
      end loop;

      return Empty;
   end Get_Var_Info;

   ------------------------
   -- Set_Message_Prefix --
   ------------------------

   procedure Set_Message_Prefix (S : String) is
      procedure Free is new Ada.Unchecked_Deallocation (String, String_Ptr);

   begin
      if Message_Prefix /= null then
         Free (Message_Prefix);
      end if;

      Message_Prefix := new String'(S);
   end Set_Message_Prefix;

   ---------------
   -- Print_Msg --
   ---------------

   procedure Print_Msg (S : String) is
      use Ada.Text_IO;
   begin
      if Message_Prefix /= null then
         Put (Message_Prefix.all & ": ");
      end if;
      Put_Line (S);
   end Print_Msg;

   -----------------------
   -- Supported_Version --
   -----------------------

   function Supported_Version (CF : Class_File) return Boolean is
   begin
      if CF.Major_Version > Min_SUN_Major_Version
        and CF.Major_Version < Max_SUN_Major_Version
      then
         return True;
      elsif CF.Major_Version = Max_SUN_Major_Version then
         return CF.Minor_Version = Max_SUN_Minor_Version;
      elsif CF.Major_Version = Min_SUN_Major_Version then
         return True;
      else
         return False;
      end if;
   end Supported_Version;

   -----------------------
   --  Source_File_Name --
   -----------------------

   function Source_File_Name (CF : Class_File) return Utf8.Table is
      T     : constant CP.Table := CF.Constant_Pool;
      CA    : Class_Attribute_Info;
      Empty : Utf8.Table;

   begin
      for K in 0 .. Last (CF.Attributes) loop
         CA := Get (CF.Attributes, K);
         if CA.Kind = Attr_Source_File then
            return J_Basics.Get_Utf8 (T, CA.Source_File_Index);
         end if;
      end loop;
      return Empty;
   end Source_File_Name;

   ----------------
   -- Safe_Image --
   ----------------

   function Safe_Image (U : U2) return String
   is
      S : constant String := U2'Image (U);
   begin
      if S'Length >= 1 and then S (S'First .. S'First) = " " then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Safe_Image;

end J_Utils;
