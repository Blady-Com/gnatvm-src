------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            G N A T . D I R E C T O R Y _ O P E R A T I O N S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1998-2010, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada; use Ada;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with System;        use System;
with System.OS_Lib; use System.OS_Lib;

package body GNAT.Directory_Operations is

   Filename_Max : constant Integer := 1024;
   --  1024 is the value of FILENAME_MAX in stdio.h

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name
     (Path   : Path_Name;
      Suffix : String := "")
      return String
   is
      function Get_File_Names_Case_Sensitive return Integer;
      pragma Import
        (C, Get_File_Names_Case_Sensitive,
         "__gnat_get_file_names_case_sensitive");

      Case_Sensitive_File_Name : constant Boolean :=
                                   Get_File_Names_Case_Sensitive = 1;

      function Basename
        (Path   : Path_Name;
         Suffix : String := "") return String;
      --  This function does the job. The only difference between Basename
      --  and Base_Name (the parent function) is that the former is case
      --  sensitive, while the latter is not. Path and Suffix are adjusted
      --  appropriately before calling Basename under platforms where the
      --  file system is not case sensitive.

      --------------
      -- Basename --
      --------------

      function Basename
        (Path   : Path_Name;
         Suffix : String    := "") return String
      is
         Cut_Start : Natural :=
                       Strings.Fixed.Index
                         (Path, Dir_Seps, Going => Strings.Backward);
         Cut_End : Natural;

      begin
         --  Cut_Start point to the first basename character

         if Cut_Start = 0 then
            Cut_Start := Path'First;

         else
            Cut_Start := Cut_Start + 1;
         end if;

         --  Cut_End point to the last basename character

         Cut_End := Path'Last;

         --  If basename ends with Suffix, adjust Cut_End

         if Suffix /= ""
           and then Path (Path'Last - Suffix'Length + 1 .. Cut_End) = Suffix
         then
            Cut_End := Path'Last - Suffix'Length;
         end if;

         Check_For_Standard_Dirs : declare
            Offset : constant Integer := Path'First - Base_Name.Path'First;
            BN     : constant String  :=
                       Base_Name.Path (Cut_Start - Offset .. Cut_End - Offset);
            --  Here we use Base_Name.Path to keep the original casing

            Has_Drive_Letter : constant Boolean :=
                                 System.OS_Lib.Path_Separator /= ':';
            --  If Path separator is not ':' then we are on a DOS based OS
            --  where this character is used as a drive letter separator.

         begin
            if BN = "." or else BN = ".." then
               return "";

            elsif Has_Drive_Letter
              and then BN'Length > 2
              and then Characters.Handling.Is_Letter (BN (BN'First))
              and then BN (BN'First + 1) = ':'
            then
               --  We have a DOS drive letter prefix, remove it

               return BN (BN'First + 2 .. BN'Last);

            else
               return BN;
            end if;
         end Check_For_Standard_Dirs;
      end Basename;

   --  Start processing for Base_Name

   begin
      if Path'Length <= Suffix'Length then
         return Path;
      end if;

      if Case_Sensitive_File_Name then
         return Basename (Path, Suffix);
      else
         return Basename
           (Characters.Handling.To_Lower (Path),
            Characters.Handling.To_Lower (Suffix));
      end if;
   end Base_Name;

   ----------------
   -- Change_Dir --
   ----------------

   procedure Change_Dir (Dir_Name : Dir_Name_Str) is
      function Internal (Dir_Name : Dir_Name_Str) return Integer;
      pragma Import (C, Internal, "chdir");
   begin
      if Internal (Dir_Name) /= 0 then
         raise Directory_Error;
      end if;
   end Change_Dir;

   -----------
   -- Close --
   -----------

   procedure Close (Dir : in out Dir_Type) is
      Discard : Integer;
      pragma Unreferenced (Discard);

      function Closedir (Dir : Dir_Type_Value) return Integer;
      pragma Import (C, Closedir, "closedir");
   begin
      if not Is_Open (Dir) then
         raise Directory_Error;
      end if;

      Discard := Closedir (Dir.all);
      Dir := Null_Dir;
   end Close;

   --------------
   -- Dir_Name --
   --------------

   function Dir_Name (Path : Path_Name) return Dir_Name_Str is
      Last_DS : constant Natural :=
                  Ada.Strings.Fixed.Index
                    (Path, Dir_Seps, Going => Strings.Backward);

   begin
      if Last_DS = 0 then

         --  There is no directory separator, returns current working directory

         return "." & Dir_Separator;

      else
         return Path (Path'First .. Last_DS);
      end if;
   end Dir_Name;

   -----------------
   -- Expand_Path --
   -----------------

   function Expand_Path
     (Path : Path_Name;
      Mode : Environment_Style := System_Default) return Path_Name
   is
      Environment_Variable_Char : Character;
      pragma Import (C, Environment_Variable_Char, "__gnat_environment_char");

      Result      : OS_Lib.String_Access := new String (1 .. 200);
      Result_Last : Natural := 0;

      procedure Append (C : Character);
      procedure Append (S : String);
      --  Append to Result

      procedure Double_Result_Size;
      --  Reallocate Result, doubling its size

      function Is_Var_Prefix (C : Character) return Boolean;
      pragma Inline (Is_Var_Prefix);

      procedure Read (K : in out Positive);
      --  Update Result while reading current Path starting at position K. If
      --  a variable is found, call Var below.

      procedure Var (K : in out Positive);
      --  Translate variable name starting at position K with the associated
      --  environment value.

      ------------
      -- Append --
      ------------

      procedure Append (C : Character) is
      begin
         if Result_Last = Result'Last then
            Double_Result_Size;
         end if;

         Result_Last := Result_Last + 1;
         Result (Result_Last) := C;
      end Append;

      procedure Append (S : String) is
      begin
         while Result_Last + S'Length - 1 > Result'Last loop
            Double_Result_Size;
         end loop;

         Result (Result_Last + 1 .. Result_Last + S'Length) := S;
         Result_Last := Result_Last + S'Length;
      end Append;

      ------------------------
      -- Double_Result_Size --
      ------------------------

      procedure Double_Result_Size is
         New_Result : constant OS_Lib.String_Access :=
                        new String (1 .. 2 * Result'Last);
      begin
         New_Result (1 .. Result_Last) := Result (1 .. Result_Last);
         OS_Lib.Free (Result);
         Result := New_Result;
      end Double_Result_Size;

      -------------------
      -- Is_Var_Prefix --
      -------------------

      function Is_Var_Prefix (C : Character) return Boolean is
      begin
         return (C = Environment_Variable_Char and then Mode = System_Default)
           or else
             (C = '$' and then (Mode = UNIX or else Mode = Both))
           or else
             (C = '%' and then (Mode = DOS or else Mode = Both));
      end Is_Var_Prefix;

      ----------
      -- Read --
      ----------

      procedure Read (K : in out Positive) is
         P : Character;

      begin
         For_All_Characters : loop
            if Is_Var_Prefix (Path (K)) then
               P := Path (K);

               --  Could be a variable

               if K < Path'Last then
                  if Path (K + 1) = P then

                     --  Not a variable after all, this is a double $ or %,
                     --  just insert one in the result string.

                     Append (P);
                     K := K + 1;

                  else
                     --  Let's parse the variable

                     Var (K);
                  end if;

               else
                  --  We have an ending $ or % sign

                  Append (P);
               end if;

            else
               --  This is a standard character, just add it to the result

               Append (Path (K));
            end if;

            --  Skip to next character

            K := K + 1;

            exit For_All_Characters when K > Path'Last;
         end loop For_All_Characters;
      end Read;

      ---------
      -- Var --
      ---------

      procedure Var (K : in out Positive) is
         P : constant Character := Path (K);
         T : Character;
         E : Positive;

      begin
         K := K + 1;

         if P = '%' or else Path (K) = '{' then

            --  Set terminator character

            if P = '%' then
               T := '%';
            else
               T := '}';
               K := K + 1;
            end if;

            --  Look for terminator character, k point to the first character
            --  for the variable name.

            E := K;

            loop
               E := E + 1;
               exit when Path (E) = T or else E = Path'Last;
            end loop;

            if Path (E) = T then

               --  OK found, translate with environment value

               declare
                  Env : OS_Lib.String_Access :=
                          OS_Lib.Getenv (Path (K .. E - 1));

               begin
                  Append (Env.all);
                  OS_Lib.Free (Env);
               end;

            else
               --  No terminator character, not a variable after all or a
               --  syntax error, ignore it, insert string as-is.

               Append (P);       --  Add prefix character

               if T = '}' then   --  If we were looking for curly bracket
                  Append ('{');  --  terminator, add the curly bracket
               end if;

               Append (Path (K .. E));
            end if;

         else
            --  The variable name is everything from current position to first
            --  non letter/digit character.

            E := K;

            --  Check that first chartacter is a letter

            if Characters.Handling.Is_Letter (Path (E)) then
               E := E + 1;

               Var_Name : loop
                  exit Var_Name when E > Path'Last;

                  if Characters.Handling.Is_Letter (Path (E))
                    or else Characters.Handling.Is_Digit (Path (E))
                  then
                     E := E + 1;
                  else
                     exit Var_Name;
                  end if;
               end loop Var_Name;

               E := E - 1;

               declare
                  Env : OS_Lib.String_Access := OS_Lib.Getenv (Path (K .. E));

               begin
                  Append (Env.all);
                  OS_Lib.Free (Env);
               end;

            else
               --  This is not a variable after all

               Append ('$');
               Append (Path (E));
            end if;

         end if;

         K := E;
      end Var;

   --  Start of processing for Expand_Path

   begin
      declare
         K : Positive := Path'First;

      begin
         Read (K);

         declare
            Returned_Value : constant String := Result (1 .. Result_Last);

         begin
            OS_Lib.Free (Result);
            return Returned_Value;
         end;
      end;
   end Expand_Path;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension (Path : Path_Name) return String is
      First : Natural :=
                Strings.Fixed.Index
                  (Path, Dir_Seps, Going => Strings.Backward);

      Dot : Natural;

   begin
      if First = 0 then
         First := Path'First;
      end if;

      Dot := Strings.Fixed.Index (Path (First .. Path'Last),
                                  ".",
                                  Going => Strings.Backward);

      if Dot = 0 or else Dot = Path'Last then
         return "";
      else
         return Path (Dot .. Path'Last);
      end if;
   end File_Extension;

   ---------------
   -- File_Name --
   ---------------

   function File_Name (Path : Path_Name) return String is
   begin
      return Base_Name (Path);
   end File_Name;

   ---------------------
   -- Format_Pathname --
   ---------------------

   function Format_Pathname
     (Path  : Path_Name;
      Style : Path_Style := System_Default)
      return Path_Name
   is
   begin
      --  ??? Generated stub: replace with real body!
      raise Program_Error;
      return Format_Pathname (Path, Style);
   end Format_Pathname;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   Max_Path : Integer;
   pragma Import (C, Max_Path, "__gnat_max_path_len");

   function Get_Current_Dir return Dir_Name_Str is
      Path : String (1 .. Max_Path);
      Last : Natural;
   begin
      Get_Current_Dir (Path, Last);
      return Path (1 .. Last);
   end Get_Current_Dir;

   procedure Get_Current_Dir (Dir : out Dir_Name_Str; Last : out Natural) is
      Buffer   : String (Dir'First .. Dir'First + Max_Path + 1);
      pragma Warnings (Off, Buffer);
      Len      : Natural;

      function Local_Get_Current_Dir
        (Dir : String) return Natural;
      pragma Import (C, Local_Get_Current_Dir, "__gnat_get_current_dir");

   begin
      Len := Local_Get_Current_Dir (Buffer);

      if Dir'Length > Len then
         Last := Dir'First + Len - 1;
      else
         Last := Dir'Last;
      end if;

      Dir (Buffer'First .. Last) := Buffer (Buffer'First .. Last);

      if Last < Dir'Last then
         Last := Last + 1;
         Dir (Last) := System.OS_Lib.Directory_Separator;
      end if;
   end Get_Current_Dir;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Dir : Dir_Type) return Boolean is
      function Isopendir (Dir : Dir_Type_Value) return Integer;
      pragma Import (C, Isopendir, "__gnat_dir_isopen");
   begin
      return Dir /= Null_Dir
        and then Isopendir (Dir.all) = 1;
   end Is_Open;

   --------------
   -- Make_Dir --
   --------------

   procedure Make_Dir (Dir_Name : Dir_Name_Str) is
      function Internal (Dir_Name : Dir_Name_Str) return Integer;
      pragma Import (C, Internal, "__gnat_mkdir");
   begin
      if Internal (Dir_Name) /= 0 then
         raise Directory_Error;
      end if;
   end Make_Dir;

   ----------
   -- Open --
   ----------

   procedure Open (Dir : out Dir_Type; Dir_Name : Dir_Name_Str) is
      function Opendir (Dir : Dir_Name_Str) return Dir_Type_Value;
      pragma Import (C, Opendir, "opendir");
   begin
      Dir := new Dir_Type_Value'(Opendir (Dir_Name));

      if not Is_Open (Dir) then
         Dir := Null_Dir;
         raise Directory_Error;
      end if;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Dir  : Dir_Type;
      Str  : out String;
      Last : out Natural)
   is
      function Readdir (Dir : Dir_Type_Value;
                        Str : String) return Natural;
      pragma Import (C, Readdir, "__gnat_readdir");

      Buffer : String (1 .. Filename_Max);
      pragma Warnings (Off, Buffer);
      Len    : Natural;
   begin
      Len := Readdir (Dir.all, Buffer);
      if Len > Str'Length then
         Len := Str'Length;
      end if;

      Str (Str'First .. Str'First + Len - 1) :=
        Buffer (Buffer'First .. Buffer'First + Len - 1);
      Last := Str'First + Len - 1;
   end Read;

   -------------------------
   -- Read_Is_Thread_Safe --
   -------------------------

   function Read_Is_Thread_Safe return Boolean is
      function Internal return Integer;
      pragma Import (C, Internal, "__gnat_readdir_is_thread_safe");
   begin
      return Internal = 1;
   end Read_Is_Thread_Safe;

   ----------------
   -- Remove_Dir --
   ----------------

   procedure Remove_Dir
     (Dir_Name  : Dir_Name_Str;
      Recursive : Boolean := False)
   is
      procedure Rmdir (Dir_Name : Dir_Name_Str);
      pragma Import (C, Rmdir, "rmdir");

   begin
      if not Recursive then
         begin
            Rmdir (Dir_Name);
         exception
            when others =>
               raise Directory_Error;
         end;

         if Is_Directory (Dir_Name) then
            raise Directory_Error;
         end if;

      else
         declare
            Current_Dir : constant Dir_Name_Str := Get_Current_Dir;
            Last        : Integer;
            Str         : String (1 .. Filename_Max);
            Success     : Boolean;
            Working_Dir : Dir_Type;
         begin
            Change_Dir (Dir_Name);
            Open (Working_Dir, ".");

            loop
               Read (Working_Dir, Str, Last);
               exit when Last = 0;

               if Is_Directory (Str (1 .. Last)) then
                  if Str (1 .. Last) /= "."
                    and then Str (1 .. Last) /= ".."
                  then
                     Remove_Dir (Str (1 .. Last), True);
                     Remove_Dir (Str (1 .. Last));
                  end if;

               else
                  Delete_File (Str (1 .. Last), Success);

                  if not Success then
                     Change_Dir (Current_Dir);
                     raise Directory_Error;
                  end if;
               end if;
            end loop;

            Change_Dir (Current_Dir);
            Close (Working_Dir);
            Remove_Dir (Dir_Name);

         exception
            when others =>

               --  An exception occurred. We must make sure the current working
               --  directory is unchanged.

               Change_Dir (Current_Dir);

               --  What if the Change_Dir raises an exception itself, shouldn't
               --  that be protected? ???

               raise;
         end;
      end if;
   end Remove_Dir;

end GNAT.Directory_Operations;
