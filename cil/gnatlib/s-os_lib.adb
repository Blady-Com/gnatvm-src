------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . O S _ L I B                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1995-2013, AdaCore                     --
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

with System.CRTL;
with System.Soft_Links;
with Ada.Unchecked_Conversion;
with System; use System;
with System.Case_Util;

package body System.OS_Lib is

   package SSL renames System.Soft_Links;

   --  The following are used by Create_Temp_File

   First_Temp_File_Name : constant String := "GNAT-TEMP-000000.TMP";
   --  Used to initialize Current_Temp_File_Name and Temp_File_Name_Last_Digit

   Current_Temp_File_Name : String := First_Temp_File_Name;
   --  Name of the temp file last created

   Temp_File_Name_Last_Digit : constant Positive :=
                                 First_Temp_File_Name'Last - 4;
   --  Position of the last digit in Current_Temp_File_Name

   Max_Attempts : constant := 100;
   --  The maximum number of attempts to create a new temp file

   -----------------------
   -- Local Subprograms --
   -----------------------

--     function Args_Length (Args : Argument_List) return Natural;
--     --  Returns total number of characters needed to create a string
--     --  of all Args terminated by ASCII.NUL characters

   function C_String_Length (S : Address) return Integer;
   --  Returns the length of a C string. Does check for null address
   --  (returns 0).

   procedure Spawn_Internal
     (Program_Name : String;
      Args         : Argument_List;
      Result       : out Integer;
      Pid          : out Process_Id;
      Blocking     : Boolean;
      Redirect_Fd  : File_Descriptor := Invalid_FD;
      Err_To_Out   : Boolean := True);
   --  Internal routine to implement the two Spawn (blocking/non blocking)
   --  routines. If Blocking is set to True then the spawn is blocking
   --  otherwise it is non blocking. In this latter case the Pid contains the
   --  process id number. The first three parameters are as in Spawn. Note that
   --  Spawn_Internal normalizes the argument list before calling the low level
   --  system spawn routines (see Normalize_Arguments).
   --  If Redirect_Fd is set, then output (and error if Err_To_Out is set) will
   --  be redirected to this file.
   --
   --  Note: Normalize_Arguments is designed to do nothing if it is called more
   --  than once, so calling Normalize_Arguments before calling one of the
   --  spawn routines is fine.

   function To_Path_String_Access
     (Path_Addr : Address;
      Path_Len  : Integer) return String_Access;
   --  Converts a C String to an Ada String. We could do this making use of
   --  Interfaces.C.Strings but we prefer not to import that entire package

   ---------
   -- "<" --
   ---------

   function "<"  (X, Y : OS_Time) return Boolean is
   begin
      return Long_Integer (X) < Long_Integer (Y);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<="  (X, Y : OS_Time) return Boolean is
   begin
      return Long_Integer (X) <= Long_Integer (Y);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">"  (X, Y : OS_Time) return Boolean is
   begin
      return Long_Integer (X) > Long_Integer (Y);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">="  (X, Y : OS_Time) return Boolean is
   begin
      return Long_Integer (X) >= Long_Integer (Y);
   end ">=";

--     -----------------
--     -- Args_Length --
--     -----------------
--
--     function Args_Length (Args : Argument_List) return Natural is
--        Len : Natural := 0;
--
--     begin
--        for J in Args'Range loop
--           Len := Len + Args (J)'Length + 1; --  One extra for ASCII.NUL
--        end loop;
--
--        return Len;
--     end Args_Length;

   -----------------------------
   -- Argument_String_To_List --
   -----------------------------

   function Argument_String_To_List
     (Arg_String : String) return Argument_List_Access
   is
      Max_Args : constant Integer := Arg_String'Length;
      New_Argv : Argument_List (1 .. Max_Args);
      New_Argc : Natural := 0;
      Idx      : Integer;

   begin
      Idx := Arg_String'First;

      loop
         exit when Idx > Arg_String'Last;

         declare
            Quoted  : Boolean := False;
            Backqd  : Boolean := False;
            Old_Idx : Integer;

         begin
            Old_Idx := Idx;

            loop
               --  An unquoted space is the end of an argument

               if not (Backqd or Quoted)
                 and then Arg_String (Idx) = ' '
               then
                  exit;

               --  Start of a quoted string

               elsif not (Backqd or Quoted)
                 and then Arg_String (Idx) = '"'
               then
                  Quoted := True;

               --  End of a quoted string and end of an argument

               elsif (Quoted and not Backqd)
                 and then Arg_String (Idx) = '"'
               then
                  Idx := Idx + 1;
                  exit;

               --  Following character is backquoted

               elsif Arg_String (Idx) = '\' then
                  Backqd := True;

               --  Turn off backquoting after advancing one character

               elsif Backqd then
                  Backqd := False;

               end if;

               Idx := Idx + 1;
               exit when Idx > Arg_String'Last;
            end loop;

            --  Found an argument

            New_Argc := New_Argc + 1;
            New_Argv (New_Argc) :=
              new String'(Arg_String (Old_Idx .. Idx - 1));

            --  Skip extraneous spaces

            while Idx <= Arg_String'Last and then Arg_String (Idx) = ' ' loop
               Idx := Idx + 1;
            end loop;
         end;
      end loop;

      return new Argument_List'(New_Argv (1 .. New_Argc));
   end Argument_String_To_List;

   ---------------------
   -- C_String_Length --
   ---------------------

   function C_String_Length (S : Address) return Integer is
      pragma Unreferenced (S);
   begin
      return 0;
   end C_String_Length;

   -----------
   -- Close --
   -----------

   procedure Close (FD : File_Descriptor) is
      Status : Boolean;
      pragma Unreferenced (Status);
   begin
      Close (FD, Status);
   end Close;

   procedure Close (FD : File_Descriptor; Status : out Boolean) is
      function C_Close (FD : File_Descriptor) return Integer;
      pragma Import (C, C_Close, "close");
   begin
      Status := (C_Close (FD) = 0);
   end Close;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File
     (Name     : String;
      Pathname : String;
      Success  : out Boolean;
      Mode     : Copy_Mode := Copy;
      Preserve : Attribute := Time_Stamps)
   is
      From : File_Descriptor := Invalid_FD;
      To   : File_Descriptor := Invalid_FD;

      Copy_Error : exception;
      --  Internal exception raised to signal error in copy

      function Build_Path (Dir : String; File : String) return String;
      --  Returns pathname Dir catenated with File adding the directory
      --  separator only if needed.

      procedure Copy (From, To : File_Descriptor);
      --  Read data from From and place them into To. In both cases the
      --  operations uses the current file position. Raises Constraint_Error
      --  if a problem occurs during the copy.

      procedure Copy_To (To_Name : String);
      --  Does a straight copy from source to designated destination file

      ----------------
      -- Build_Path --
      ----------------

      function Build_Path (Dir : String; File : String) return String is
         Res : String (1 .. Dir'Length + File'Length + 1);

         Base_File_Ptr : Integer;
         --  The base file name is File (Base_File_Ptr + 1 .. File'Last)

         function Is_Dirsep (C : Character) return Boolean;
         pragma Inline (Is_Dirsep);
         --  Returns True if C is a directory separator. On Windows we
         --  handle both styles of directory separator.

         ---------------
         -- Is_Dirsep --
         ---------------

         function Is_Dirsep (C : Character) return Boolean is
         begin
            return C = Directory_Separator or else C = '/';
         end Is_Dirsep;

      --  Start of processing for Build_Path

      begin
         --  Find base file name

         Base_File_Ptr := File'Last;
         while Base_File_Ptr >= File'First loop
            exit when Is_Dirsep (File (Base_File_Ptr));
            Base_File_Ptr := Base_File_Ptr - 1;
         end loop;

         declare
            Base_File : constant String :=
                          File (Base_File_Ptr + 1 .. File'Last);

         begin
            Res (1 .. Dir'Length) := Dir;

            if Is_Dirsep (Dir (Dir'Last)) then
               Res (Dir'Length + 1 .. Dir'Length + Base_File'Length) :=
                 Base_File;
               return Res (1 .. Dir'Length + Base_File'Length);

            else
               Res (Dir'Length + 1) := Directory_Separator;
               Res (Dir'Length + 2 .. Dir'Length + 1 + Base_File'Length) :=
                 Base_File;
               return Res (1 .. Dir'Length + 1 + Base_File'Length);
            end if;
         end;
      end Build_Path;

      ----------
      -- Copy --
      ----------

      procedure Copy (From, To : File_Descriptor) is
         function C_Copy (From, To : File_Descriptor) return Boolean;
         pragma Import (CIL, C_Copy,
                        "mgnat.adalib.GNAT_libc.__gnat_copy_file");
      begin
         if not C_Copy (From, To) then
            raise Copy_Error;
         end if;
      end Copy;

      -------------
      -- Copy_To --
      -------------

      procedure Copy_To (To_Name : String) is

         function Copy_Attributes
           (From, To : String;
            Mode     : Integer) return Integer;
         pragma Import (CIL, Copy_Attributes,
                        "mgnat.adalib.GNAT_libc.__gnat_copy_attribs");
         --  Mode = 0 - copy only time stamps.
         --  Mode = 1 - copy time stamps and read/write/execute attributes

      begin
         From := Open_Read (Name, Binary);

         --  Do not clobber destination file if source file could not be opened

         if From /= Invalid_FD then
            To := Create_File (To_Name, Binary);
         end if;

         Copy (From, To);

         --  Copy attributes

         case Preserve is

            when Time_Stamps =>
               if Copy_Attributes (Name, To_Name, 0) = -1 then
                  raise Copy_Error;
               end if;

            when Full =>
               if Copy_Attributes (Name, To_Name, 1) = -1 then
                  raise Copy_Error;
               end if;

            when None =>
               null;
         end case;

      end Copy_To;

   --  Start of processing for Copy_File

   begin
      Success := True;

      --  The source file must exist

      if not Is_Regular_File (Name) then
         raise Copy_Error;
      end if;

      --  The source file exists

      case Mode is

         --  Copy case, target file must not exist

         when Copy =>

            --  If the target file exists, we have an error

            if Is_Regular_File (Pathname) then
               raise Copy_Error;

            --  Case of target is a directory

            elsif Is_Directory (Pathname) then
               declare
                  Dest : constant String := Build_Path (Pathname, Name);

               begin
                  --  If target file exists, we have an error, else do copy

                  if Is_Regular_File (Dest) then
                     raise Copy_Error;
                  else
                     Copy_To (Dest);
                  end if;
               end;

            --  Case of normal copy to file (destination does not exist)

            else
               Copy_To (Pathname);
            end if;

         --  Overwrite case (destination file may or may not exist)

         when Overwrite =>
            if Is_Directory (Pathname) then
               Copy_To (Build_Path (Pathname, Name));
            else
               Copy_To (Pathname);
            end if;

         --  Append case (destination file may or may not exist)

         when Append =>

            --  Appending to existing file

            if Is_Regular_File (Pathname) then

               --  Append mode and destination file exists, append data at the
               --  end of Pathname. But if we fail to open source file, do not
               --  touch destination file at all.

               From := Open_Read (Name, Binary);

               if From /= Invalid_FD then
                  To := Open_Read_Write (Pathname, Binary);
               end if;

               Lseek (To, 0, Seek_End);

               Copy (From, To);

            --  Appending to directory, not allowed

            elsif Is_Directory (Pathname) then
               raise Copy_Error;

            --  Appending when target file does not exist

            else
               Copy_To (Pathname);
            end if;
      end case;

   --  All error cases are caught here

   exception
      when Copy_Error =>
         Success := False;
   end Copy_File;

   procedure Copy_File
     (Name     : C_File_Name;
      Pathname : C_File_Name;
      Success  : out Boolean;
      Mode     : Copy_Mode := Copy;
      Preserve : Attribute := Time_Stamps)
   is
      Ada_Name : String_Access :=
                   To_Path_String_Access
                     (Name, C_String_Length (Name));

      Ada_Pathname : String_Access :=
                       To_Path_String_Access
                         (Pathname, C_String_Length (Pathname));

   begin
      Copy_File (Ada_Name.all, Ada_Pathname.all, Success, Mode, Preserve);
      Free (Ada_Name);
      Free (Ada_Pathname);
   end Copy_File;

   ----------------------
   -- Copy_Time_Stamps --
   ----------------------

   procedure Copy_Time_Stamps (Source, Dest : String; Success : out Boolean) is

         function Copy_Attributes
           (From, To : String;
            Mode     : Integer) return Integer;
         pragma Import (CIL, Copy_Attributes,
                        "mgnat.adalib.GNAT_libc.__gnat_copy_attribs");
      --  Mode = 0 - copy only time stamps.
      --  Mode = 1 - copy time stamps and read/write/execute attributes

   begin
      if Is_Regular_File (Source) and then Is_Writable_File (Dest) then
         if Copy_Attributes (Source, Dest, 0) = -1 then
            Success := False;
         else
            Success := True;
         end if;

      else
         Success := False;
      end if;
   end Copy_Time_Stamps;

   procedure Copy_Time_Stamps
     (Source, Dest : C_File_Name;
      Success      : out Boolean)
   is
      Ada_Source : String_Access :=
                     To_Path_String_Access
                       (Source, C_String_Length (Source));

      Ada_Dest : String_Access :=
                   To_Path_String_Access
                     (Dest, C_String_Length (Dest));
   begin
      Copy_Time_Stamps (Ada_Source.all, Ada_Dest.all, Success);
      Free (Ada_Source);
      Free (Ada_Dest);
   end Copy_Time_Stamps;

   -----------------
   -- Create_File --
   -----------------

   function Create_File
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor
   is
      pragma Unreferenced (Name, Fmode);
   begin
      --  Not implemented
      return Invalid_FD;
   end Create_File;

   function Create_File
     (Name  : String;
      Fmode : Mode) return File_Descriptor
   is
      function C_Create_File
        (Name  : String;
         Fmode : Mode) return File_Descriptor;
      pragma Import (CIL, C_Create_File,
                     "mgnat.adalib.GNAT_libc.__gnat_open_create");

   begin
      return C_Create_File (Name, Fmode);
   end Create_File;

   ---------------------
   -- Create_New_File --
   ---------------------

   function Create_New_File
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor
   is
      pragma Unreferenced (Name, Fmode);
   begin
      --  Not implemented
      return Invalid_FD;
   end Create_New_File;

   function Create_New_File
     (Name  : String;
      Fmode : Mode) return File_Descriptor
   is
      function C_Create_New_File
        (Name  : String;
         Fmode : Mode) return File_Descriptor;
      pragma Import (CIL, C_Create_New_File,
                     "mgnat.adalib.GNAT_libc.__gnat_open_new");

   begin
      return C_Create_New_File (Name, Fmode);
   end Create_New_File;

   -----------------------------
   -- Create_Output_Text_File --
   -----------------------------

   function Create_Output_Text_File (Name : String) return File_Descriptor is
      function C_Create_File
        (Name : String) return File_Descriptor;
      pragma Import
        (CIL, C_Create_File,
         "mgnat.adalib.GNAT_libc.__gnat_create_output_file");

   begin
      return C_Create_File (Name);
   end Create_Output_Text_File;

   ----------------------
   -- Create_Temp_File --
   ----------------------

   procedure Create_Temp_File
     (FD   : out File_Descriptor;
      Name : out Temp_File_Name)
   is
      function Open_New_Temp
        (Name  : System.Address;
         Fmode : Mode) return File_Descriptor;
      pragma Import
        (CIL, Open_New_Temp,
         "mgnat.adalib.GNAT_libc.__gnat_open_new_temp");

   begin
      FD := Open_New_Temp (Name'Address, Binary);
   end Create_Temp_File;

   procedure Create_Temp_File
     (FD   : out File_Descriptor;
      Name : out String_Access)
   is
      Pos      : Positive;
      Attempts : Natural := 0;
      Current  : String (Current_Temp_File_Name'Range);

   begin
      --  Loop until a new temp file can be created

      File_Loop : loop
         Locked : begin
            --  We need to protect global variable Current_Temp_File_Name
            --  against concurrent access by different tasks.

            SSL.Lock_Task.all;

            --  Start at the last digit

            Pos := Temp_File_Name_Last_Digit;

            Digit_Loop :
            loop
               --  Increment the digit by one

               case Current_Temp_File_Name (Pos) is
                  when '0' .. '8' =>
                     Current_Temp_File_Name (Pos) :=
                       Character'Succ (Current_Temp_File_Name (Pos));
                     exit Digit_Loop;

                  when '9' =>

                     --  For 9, set the digit to 0 and go to the previous digit

                     Current_Temp_File_Name (Pos) := '0';
                     Pos := Pos - 1;

                  when others =>

                     --  If it is not a digit, then there are no available
                     --  temp file names. Return Invalid_FD. There is almost
                     --  no that this code will be ever be executed, since
                     --  it would mean that there are one million temp files
                     --  in the same directory!

                     SSL.Unlock_Task.all;
                     FD := Invalid_FD;
                     Name := null;
                     exit File_Loop;
               end case;
            end loop Digit_Loop;

            Current := Current_Temp_File_Name;

            --  We can now release the lock, because we are no longer
            --  accessing Current_Temp_File_Name.

            SSL.Unlock_Task.all;

         exception
            when others =>
               SSL.Unlock_Task.all;
               raise;
         end Locked;

         --  Attempt to create the file

         FD := Create_New_File (Current, Binary);

         if FD /= Invalid_FD then
            Name := new String'(Current);
            exit File_Loop;
         end if;

         if not Is_Regular_File (Current) then

            --  If the file does not already exist and we are unable to create
            --  it, we give up after Max_Attempts. Otherwise, we try again with
            --  the next available file name.

            Attempts := Attempts + 1;

            if Attempts >= Max_Attempts then
               FD := Invalid_FD;
               Name := null;
               exit File_Loop;
            end if;
         end if;
      end loop File_Loop;
   end Create_Temp_File;

   -----------------------------
   -- Create_Temp_Output_File --
   -----------------------------

   procedure Create_Temp_Output_File
     (FD   : out File_Descriptor;
      Name : out String_Access) is
   begin
      Create_Temp_File (FD, Name);
   end Create_Temp_Output_File;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File (Name : Address; Success : out Boolean) is
      pragma Unreferenced (Name);
   begin
      --  Not implemented
      Success := False;
   end Delete_File;

   procedure Delete_File (Name : String; Success : out Boolean) is
      function C_Delete_File (Name : String) return Boolean;
      pragma Import (CIL, C_Delete_File,
                     "mgnat.adalib.GNAT_libc.__gnat_delete_file");

   begin
      Success := C_Delete_File (Name);
   end Delete_File;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp (FD : File_Descriptor) return OS_Time is
      function File_Time (FD : File_Descriptor) return OS_Time;
      pragma Import (C, File_Time, "__gnat_file_time_fd");
   begin
      return File_Time (FD);
   end File_Time_Stamp;

   function File_Time_Stamp (Name : C_File_Name) return OS_Time is
      pragma Unreferenced (Name);
   begin
      return Invalid_Time;
   end File_Time_Stamp;

   function File_Time_Stamp (Name : String) return OS_Time is
      function File_Time (Name : String) return OS_Time;
      pragma Import (C, File_Time, "__gnat_file_time_name");
   begin
      return File_Time (Name);
   end File_Time_Stamp;

   ---------------------------
   -- Get_Debuggable_Suffix --
   ---------------------------

   function Get_Debuggable_Suffix return String_Access is
   begin
      return new String'(".exe");
   end Get_Debuggable_Suffix;

   ---------------------------
   -- Get_Executable_Suffix --
   ---------------------------

   function Get_Executable_Suffix return String_Access is
   begin
      return new String'(".exe");
   end Get_Executable_Suffix;

   -----------------------
   -- Get_Object_Suffix --
   -----------------------

   function Get_Object_Suffix return String_Access is
   begin
      return new String'(".il");
   end Get_Object_Suffix;

   ----------------------------------
   -- Get_Target_Debuggable_Suffix --
   ----------------------------------

   function Get_Target_Debuggable_Suffix return String_Access is
   begin
      return new String'(".exe");
   end Get_Target_Debuggable_Suffix;

   ----------------------------------
   -- Get_Target_Executable_Suffix --
   ----------------------------------

   function Get_Target_Executable_Suffix return String_Access is
   begin
      return new String'(".exe");
   end Get_Target_Executable_Suffix;

   ------------------------------
   -- Get_Target_Object_Suffix --
   ------------------------------

   function Get_Target_Object_Suffix return String_Access is
   begin
      return new String'(".il");
   end Get_Target_Object_Suffix;

   ------------
   -- Getenv --
   ------------

   function Getenv (Name : String) return String_Access is
      function Get_Env_Value_Ptr (Name : String) return String;
      pragma Import (CIL, Get_Env_Value_Ptr,
                     "mgnat.adalib.GNAT_libc.__gnat_getenv");
      pragma Warnings (Off, Get_Env_Value_Ptr);

      Result : constant String := Get_Env_Value_Ptr (Name);

   begin
      return new String'(Result);
   end Getenv;

   ------------
   -- GM_Day --
   ------------

   function GM_Day (Date : OS_Time) return Day_Type is
      D  : Day_Type;

      pragma Warnings (Off);
      Y  : Year_Type;
      Mo : Month_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;
      pragma Warnings (On);

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return D;
   end GM_Day;

   -------------
   -- GM_Hour --
   -------------

   function GM_Hour (Date : OS_Time) return Hour_Type is
      H  : Hour_Type;

      pragma Warnings (Off);
      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      Mn : Minute_Type;
      S  : Second_Type;
      pragma Warnings (On);

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return H;
   end GM_Hour;

   ---------------
   -- GM_Minute --
   ---------------

   function GM_Minute (Date : OS_Time) return Minute_Type is
      Mn : Minute_Type;

      pragma Warnings (Off);
      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      S  : Second_Type;
      pragma Warnings (On);

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return Mn;
   end GM_Minute;

   --------------
   -- GM_Month --
   --------------

   function GM_Month (Date : OS_Time) return Month_Type is
      Mo : Month_Type;

      pragma Warnings (Off);
      Y  : Year_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;
      pragma Warnings (On);

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return Mo;
   end GM_Month;

   ---------------
   -- GM_Second --
   ---------------

   function GM_Second (Date : OS_Time) return Second_Type is
      S  : Second_Type;

      pragma Warnings (Off);
      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      pragma Warnings (On);

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return S;
   end GM_Second;

   --------------
   -- GM_Split --
   --------------

   procedure GM_Split
     (Date   : OS_Time;
      Year   : out Year_Type;
      Month  : out Month_Type;
      Day    : out Day_Type;
      Hour   : out Hour_Type;
      Minute : out Minute_Type;
      Second : out Second_Type)
   is
      procedure To_GM_Time
        (P_Time_T, P_Year, P_Month, P_Day, P_Hours, P_Mins, P_Secs : Address);
      pragma Import (C, To_GM_Time, "__gnat_to_gm_time");

      T  : OS_Time := Date;
      Y  : Integer;
      Mo : Integer;
      D  : Integer;
      H  : Integer;
      Mn : Integer;
      S  : Integer;

   begin
      --  Use the global lock because To_GM_Time is not thread safe

      Locked_Processing : begin
         SSL.Lock_Task.all;
         To_GM_Time
           (T'Address, Y'Address, Mo'Address, D'Address,
            H'Address, Mn'Address, S'Address);
         SSL.Unlock_Task.all;

      exception
         when others =>
            SSL.Unlock_Task.all;
            raise;
      end Locked_Processing;

      Year   := Y;
      Month  := Mo;
      Day    := D;
      Hour   := H;
      Minute := Mn;
      Second := S;
   end GM_Split;

   -------------
   -- GM_Year --
   -------------

   function GM_Year (Date : OS_Time) return Year_Type is
      Y  : Year_Type;

      pragma Warnings (Off);
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;
      pragma Warnings (On);

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return Y;
   end GM_Year;

   ----------------------
   -- Is_Absolute_Path --
   ----------------------

   function Is_Absolute_Path (Name : String) return Boolean is
      function C_Is_Absolute_Path (Name : String) return Boolean;
      pragma Import (CIL, C_Is_Absolute_Path,
                     "mgnat.adalib.GNAT_libc.__gnat_is_absolute_path");
   begin
      return C_Is_Absolute_Path (Name);
   end Is_Absolute_Path;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (Name : C_File_Name) return Boolean is
      pragma Unreferenced (Name);
   begin
      return False;
   end Is_Directory;

   function Is_Directory (Name : String) return Boolean is
      function C_Is_Directory (Name : String) return Boolean;
      pragma Import (CIL, C_Is_Directory,
                     "mgnat.adalib.GNAT_libc.__gnat_is_directory");
   begin
      return C_Is_Directory (Name);
   end Is_Directory;

   ----------------------
   -- Is_Readable_File --
   ----------------------

   function Is_Readable_File (Name : C_File_Name) return Boolean is
      pragma Unreferenced (Name);
   begin
      --  Not implemented.
      return False;
   end Is_Readable_File;

   function Is_Readable_File (Name : String) return Boolean is
      function C_Is_Readable_File (Name : String) return Boolean;
      pragma Import (CIL, C_Is_Readable_File,
                     "mgnat.adalib.GNAT_libc.__gnat_is_readable_file");
   begin
      return C_Is_Readable_File (Name);
   end Is_Readable_File;

   ------------------------
   -- Is_Executable_File --
   ------------------------

   function Is_Executable_File (Name : C_File_Name) return Boolean is
      pragma Unreferenced (Name);
   begin
      return False;
   end Is_Executable_File;

   function Is_Executable_File (Name : String) return Boolean is
      function C_Is_Executable_File (Name : String) return Boolean;
      pragma Import
        (CIL, C_Is_Executable_File,
         "mgnat.adalib.GNAT_libc.__gnat_is_executable_file");
   begin
      return C_Is_Executable_File (Name);
   end Is_Executable_File;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Name : C_File_Name) return Boolean is
      pragma Unreferenced (Name);
   begin
      return False;
   end Is_Regular_File;

   function Is_Regular_File (Name : String) return Boolean is
      function C_Is_Regular_File (Name : String) return Boolean;
      pragma Import
        (CIL, C_Is_Regular_File,
         "mgnat.adalib.GNAT_libc.__gnat_is_regular_file");
   begin
      return C_Is_Regular_File (Name);
   end Is_Regular_File;

   ----------------------
   -- Is_Symbolic_Link --
   ----------------------

   function Is_Symbolic_Link (Name : C_File_Name) return Boolean is
      pragma Unreferenced (Name);
   begin
      --  Not implemented
      return False;
   end Is_Symbolic_Link;

   function Is_Symbolic_Link (Name : String) return Boolean is
      function C_Is_Symbolic_Link (Name : String) return Boolean;
      pragma Import (CIL, C_Is_Symbolic_Link,
                     "mgnat.adalib.GNAT_libc.__gnat_is_symbolic_link");
   begin
      return C_Is_Symbolic_Link (Name);
   end Is_Symbolic_Link;

   ----------------------
   -- Is_Writable_File --
   ----------------------

   function Is_Writable_File (Name : C_File_Name) return Boolean is
      pragma Unreferenced (Name);
   begin
      --  Not implemented
      return False;
   end Is_Writable_File;

   function Is_Writable_File (Name : String) return Boolean is
      function C_Is_Writable_File (Name : String) return Boolean;
      pragma Import (CIL, C_Is_Writable_File,
                     "mgnat.adalib.GNAT_libc.__gnat_is_writable_file");
   begin
      return C_Is_Writable_File (Name);
   end Is_Writable_File;

   -------------------------
   -- Locate_Exec_On_Path --
   -------------------------

   function Locate_Exec_On_Path
     (Exec_Name : String) return String_Access
   is
      pragma Warnings (Off);
      function C_Locate_Exec_On_Path (C_Exec_Name : String) return String;
      pragma Import
        (CIL, C_Locate_Exec_On_Path,
         "mgnat.adalib.GNAT_libc.__gnat_locate_exec_on_path");
      pragma Warnings (On);

      Path_Addr : constant String := C_Locate_Exec_On_Path (Exec_Name);

   begin
      if Path_Addr'Length = 0 then
         return null;

      else
         --  Always return an absolute path name

         if not Is_Absolute_Path (Path_Addr) then
            declare
               Absolute_Path : constant String :=
                                 Normalize_Pathname (Path_Addr);
            begin
               return new String'(Absolute_Path);
            end;
         end if;

         return new String'(Path_Addr);
      end if;
   end Locate_Exec_On_Path;

   -------------------------
   -- Locate_Regular_File --
   -------------------------

   function Locate_Regular_File
     (File_Name : C_File_Name;
      Path      : C_File_Name) return String_Access
   is
      pragma Unreferenced (File_Name, Path);
   begin
      --  Not implemented
      return null;
   end Locate_Regular_File;

   function Locate_Regular_File
     (File_Name : String;
      Path      : String) return String_Access
   is
      function C_Locate_Regular_File
        (C_File_Name, Path_Val : String) return String;
      pragma Import
        (CIL, C_Locate_Regular_File,
         "mgnat.adalib.GNAT_libc.__gnat_locate_regular_file");
      pragma Warnings (Off, C_Locate_Regular_File);

      Result : constant String :=
                 C_Locate_Regular_File (File_Name, Path);
   begin
      --  Always return an absolute path name

      if Result /= "" then
         if not Is_Absolute_Path (Result) then
            declare
               Absolute_Path : constant String := Normalize_Pathname (Result);
            begin
               return new String'(Absolute_Path);
            end;
         else
            return new String'(Result);
         end if;
      end if;

      return null;
   end Locate_Regular_File;

   ------------------------
   -- Non_Blocking_Spawn --
   ------------------------

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List) return Process_Id
   is
      Pid  : Process_Id;
      Junk : Integer;
      pragma Warnings (Off, Junk);
   begin
      Spawn_Internal (Program_Name, Args, Junk, Pid, Blocking => False);
      return Pid;
   end Non_Blocking_Spawn;

   function Non_Blocking_Spawn
     (Program_Name           : String;
      Args                   : Argument_List;
      Output_File_Descriptor : File_Descriptor;
      Err_To_Out             : Boolean := True) return Process_Id
   is
      Pid         : Process_Id;
      Return_Code : Integer;

   begin
      if Output_File_Descriptor = Invalid_FD then
         return Invalid_Pid;
      end if;

      --  Spawn the program
      Spawn_Internal (Program_Name, Args, Return_Code, Pid,
                      Blocking    => False,
                      Redirect_Fd => Output_File_Descriptor,
                      Err_To_Out  => Err_To_Out);
      return Pid;
   end Non_Blocking_Spawn;

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Output_File  : String;
      Err_To_Out   : Boolean := True) return Process_Id
   is
      Output_File_Descriptor : constant File_Descriptor :=
                                 Create_Output_Text_File (Output_File);
      Result : Process_Id;

   begin
      --  Do not attempt to spawn if the output file could not be created

      if Output_File_Descriptor = Invalid_FD then
         return Invalid_Pid;

      else
         Result := Non_Blocking_Spawn
                     (Program_Name, Args, Output_File_Descriptor, Err_To_Out);

         --  Close the file just created for the output, as the file descriptor
         --  cannot be used anywhere, being a local value. It is safe to do
         --  that, as the file descriptor has been duplicated to form
         --  standard output and error of the spawned process.

         Close (Output_File_Descriptor);

         return Result;
      end if;
   end Non_Blocking_Spawn;

   -------------------------
   -- Normalize_Arguments --
   -------------------------

   procedure Normalize_Arguments (Args : in out Argument_List) is

      procedure Quote_Argument (Arg : in out String_Access);
      --  Add quote around argument if it contains spaces

      C_Argument_Needs_Quote : Integer;
      pragma Import (C, C_Argument_Needs_Quote, "__gnat_argument_needs_quote");
      Argument_Needs_Quote : constant Boolean := C_Argument_Needs_Quote /= 0;

      --------------------
      -- Quote_Argument --
      --------------------

      procedure Quote_Argument (Arg : in out String_Access) is
         Res          : String (1 .. Arg'Length * 2);
         J            : Positive := 1;
         Quote_Needed : Boolean  := False;

      begin
         if Arg (Arg'First) /= '"' or else Arg (Arg'Last) /= '"' then

            --  Starting quote

            Res (J) := '"';

            for K in Arg'Range loop

               J := J + 1;

               if Arg (K) = '"' then
                  Res (J) := '\';
                  J := J + 1;
                  Res (J) := '"';
                  Quote_Needed := True;

               elsif Arg (K) = ' ' or else Arg (K) = ASCII.HT then
                  Res (J) := Arg (K);
                  Quote_Needed := True;

               else
                  Res (J) := Arg (K);
               end if;

            end loop;

            if Quote_Needed then

               --  If null terminated string, put the quote before

               if Res (J) = ASCII.NUL then
                  Res (J) := '"';
                  J := J + 1;
                  Res (J) := ASCII.NUL;

               --  If argument is terminated by '\', then double it. Otherwise
               --  the ending quote will be taken as-is. This is quite strange
               --  spawn behavior from Windows, but this is what we see!

               else
                  if Res (J) = '\' then
                     J := J + 1;
                     Res (J) := '\';
                  end if;

                  --  Ending quote

                  J := J + 1;
                  Res (J) := '"';
               end if;

               declare
                  Old : String_Access := Arg;

               begin
                  Arg := new String'(Res (1 .. J));
                  Free (Old);
               end;
            end if;

         end if;
      end Quote_Argument;

   --  Start of processing for Normalize_Arguments

   begin
      if Argument_Needs_Quote then
         for K in Args'Range loop
            if Args (K) /= null and then Args (K)'Length /= 0 then
               Quote_Argument (Args (K));
            end if;
         end loop;
      end if;
   end Normalize_Arguments;

   ------------------------
   -- Normalize_Pathname --
   ------------------------

   function Normalize_Pathname
     (Name           : String;
      Directory      : String  := "";
      Resolve_Links  : Boolean := True;
      Case_Sensitive : Boolean := True) return String
   is
      Max_Path : Integer;
      pragma Import (C, Max_Path, "__gnat_max_path_len");
      --  Maximum length of a path name

      On_Windows : constant Boolean := True;
      --  .NET is a windows equivalent platform

      function Get_Current_Dir
        (Dir : String) return Natural;
      pragma Import (C, Get_Current_Dir, "__gnat_get_current_dir");

      Path_Buffer : String (1 .. Max_Path + Max_Path + 2);
      End_Path    : Natural := 0;
      Link_Buffer : String (1 .. Max_Path + 2);
      Status      : Integer;
      Last        : Positive;
      Start       : Natural;
      Finish      : Positive;

      Max_Iterations : constant := 500;

      function Get_File_Names_Case_Sensitive return Integer;
      pragma Import
        (C, Get_File_Names_Case_Sensitive,
         "__gnat_get_file_names_case_sensitive");

      Fold_To_Lower_Case : constant Boolean :=
                             not Case_Sensitive
                               and then Get_File_Names_Case_Sensitive = 0;

      function Readlink
        (Path   : System.Address;
         Buf    : System.Address;
         Bufsiz : Integer) return Integer;
      pragma Import (C, Readlink, "__gnat_readlink");

      function Final_Value (S : String) return String;
      --  Make final adjustment to the returned string. This function strips
      --  trailing directory separators, and folds returned string to lower
      --  case if required.

      function Get_Directory  (Dir : String) return String;
      --  If Dir is not empty, return it, adding a directory separator
      --  if not already present, otherwise return current working directory
      --  with terminating directory separator.

      -----------------
      -- Final_Value --
      -----------------

      function Final_Value (S : String) return String is
         S1 : String := S;
         --  We may need to fold S to lower case, so we need a variable

         Last : Natural;

      begin
         if Fold_To_Lower_Case then
            System.Case_Util.To_Lower (S1);
         end if;

         --  Remove trailing directory separator, if any

         Last := S1'Last;

         if Last > 1
           and then (S1 (Last) = '/'
                       or else
                     S1 (Last) = Directory_Separator)
         then
            --  Special case for Windows: C:\

            if Last = 3
              and then S1 (1) /= Directory_Separator
              and then S1 (2) = ':'
            then
               null;

            else
               Last := Last - 1;
            end if;
         end if;

         return S1 (1 .. Last);
      end Final_Value;

      -------------------
      -- Get_Directory --
      -------------------

      function Get_Directory (Dir : String) return String is
      begin
         --  Directory given, add directory separator if needed

         if Dir'Length > 0 then
            if Dir (Dir'Last) = Directory_Separator then
               return Dir;
            else
               declare
                  Result : String (1 .. Dir'Length + 1);
               begin
                  Result (1 .. Dir'Length) := Dir;
                  Result (Result'Length) := Directory_Separator;
                  return Result;
               end;
            end if;

         --  Directory name not given, get current directory

         else
            declare
               Buffer   : String (1 .. Max_Path + 2) := (others => ASCII.NUL);
               Path_Len : Natural := Max_Path;

            begin
               Path_Len := Get_Current_Dir (Buffer);

               if Buffer (Path_Len) /= Directory_Separator then
                  Path_Len := Path_Len + 1;
                  Buffer (Path_Len) := Directory_Separator;
               end if;

               --  By default, the drive letter on Windows is in upper case

               if On_Windows and then Path_Len >= 2 and then
                 Buffer (2) = ':'
               then
                  System.Case_Util.To_Upper (Buffer (1 .. 1));
               end if;

               return Buffer (1 .. Path_Len);
            end;
         end if;
      end Get_Directory;

   --  Start of processing for Normalize_Pathname

   begin
      --  Special case, if name is null, then return null

      if Name'Length = 0 then
         return "";
      end if;

      --  First, convert VMS file spec to Unix file spec.
      --  If Name is not in VMS syntax, then this is equivalent
      --  to put Name at the begining of Path_Buffer.

      VMS_Conversion : begin
         Path_Buffer (1 .. Name'Length) := Name;
         End_Path := Name'Length;
         Last := 1;
      end VMS_Conversion;

      --  Replace all '/' by Directory Separators (this is for Windows)

      if Directory_Separator /= '/' then
         for Index in 1 .. End_Path loop
            if Path_Buffer (Index) = '/' then
               Path_Buffer (Index) := Directory_Separator;
            end if;
         end loop;
      end if;

      --  Resolve directory names for Windows (formerly also VMS)

      --  On VMS, if we have a Unix path such as /temp/..., and TEMP is a
      --  logical name, we must not try to resolve this logical name, because
      --  it may have multiple equivalences and if resolved we will only
      --  get the first one.

      --  On Windows, if we have an absolute path starting with a directory
      --  separator, we need to have the drive letter appended in front.

      --  On Windows, Get_Current_Dir will return a suitable directory
      --  name (path starting with a drive letter on Windows). So we take this
      --  drive letter and prepend it to the current path.

      if On_Windows
        and then Path_Buffer (1) = Directory_Separator
        and then Path_Buffer (2) /= Directory_Separator
      then
         declare
            Cur_Dir : constant String := Get_Directory ("");
            --  Get the current directory to get the drive letter

         begin
            if Cur_Dir'Length > 2
              and then Cur_Dir (Cur_Dir'First + 1) = ':'
            then
               Path_Buffer (3 .. End_Path + 2) := Path_Buffer (1 .. End_Path);
               Path_Buffer (1 .. 2) :=
                 Cur_Dir (Cur_Dir'First .. Cur_Dir'First + 1);
               End_Path := End_Path + 2;
            end if;
         end;
      end if;

      --  Start the conversions

      --  If this is not finished after Max_Iterations, give up and return an
      --  empty string.

      for J in 1 .. Max_Iterations loop

         --  If we don't have an absolute pathname, prepend the directory
         --  Reference_Dir.

         if Last = 1
           and then not Is_Absolute_Path (Path_Buffer (1 .. End_Path))
         then
            declare
               Reference_Dir : constant String  := Get_Directory (Directory);
               Ref_Dir_Len   : constant Natural := Reference_Dir'Length;
               --  Current directory name specified and its length

            begin
               Path_Buffer (Ref_Dir_Len + 1 .. Ref_Dir_Len + End_Path) :=
                 Path_Buffer (1 .. End_Path);
               End_Path := Ref_Dir_Len + End_Path;
               Path_Buffer (1 .. Ref_Dir_Len) := Reference_Dir;
               Last := Ref_Dir_Len;
            end;
         end if;

         Start  := Last + 1;
         Finish := Last;

         --  Ensure that Windows network drives are kept, e.g: \\server\drive-c

         if Start = 2
           and then Directory_Separator = '\'
           and then Path_Buffer (1 .. 2) = "\\"
         then
            Start := 3;
         end if;

         --  If we have traversed the full pathname, return it

         if Start > End_Path then
            return Final_Value (Path_Buffer (1 .. End_Path));
         end if;

         --  Remove duplicate directory separators

         while Path_Buffer (Start) = Directory_Separator loop
            if Start = End_Path then
               return Final_Value (Path_Buffer (1 .. End_Path - 1));

            else
               Path_Buffer (Start .. End_Path - 1) :=
                 Path_Buffer (Start + 1 .. End_Path);
               End_Path := End_Path - 1;
            end if;
         end loop;

         --  Find the end of the current field: last character or the one
         --  preceding the next directory separator.

         while Finish < End_Path
           and then Path_Buffer (Finish + 1) /= Directory_Separator
         loop
            Finish := Finish + 1;
         end loop;

         --  Remove "." field

         if Start = Finish and then Path_Buffer (Start) = '.' then
            if Start = End_Path then
               if Last = 1 then
                  return (1 => Directory_Separator);
               else

                  if Fold_To_Lower_Case then
                     System.Case_Util.To_Lower (Path_Buffer (1 .. Last - 1));
                  end if;

                  return Path_Buffer (1 .. Last - 1);

               end if;

            else
               Path_Buffer (Last + 1 .. End_Path - 2) :=
                 Path_Buffer (Last + 3 .. End_Path);
               End_Path := End_Path - 2;
            end if;

         --  Remove ".." fields

         elsif Finish = Start + 1
           and then Path_Buffer (Start .. Finish) = ".."
         then
            Start := Last;
            loop
               Start := Start - 1;
               exit when Start < 1 or else
                 Path_Buffer (Start) = Directory_Separator;
            end loop;

            if Start <= 1 then
               if Finish = End_Path then
                  return (1 => Directory_Separator);

               else
                  Path_Buffer (1 .. End_Path - Finish) :=
                    Path_Buffer (Finish + 1 .. End_Path);
                  End_Path := End_Path - Finish;
                  Last := 1;
               end if;

            else
               if Finish = End_Path then
                  return Final_Value (Path_Buffer (1 .. Start - 1));

               else
                  Path_Buffer (Start + 1 .. Start + End_Path - Finish - 1) :=
                    Path_Buffer (Finish + 2 .. End_Path);
                  End_Path := Start + End_Path - Finish - 1;
                  Last := Start;
               end if;
            end if;

         --  Check if current field is a symbolic link

         elsif Resolve_Links then
            declare
               Saved : constant Character := Path_Buffer (Finish + 1);

            begin
               Path_Buffer (Finish + 1) := ASCII.NUL;
               Status := Readlink (Path_Buffer'Address,
                                   Link_Buffer'Address,
                                   Link_Buffer'Length);
               Path_Buffer (Finish + 1) := Saved;
            end;

            --  Not a symbolic link, move to the next field, if any

            if Status <= 0 then
               Last := Finish + 1;

            --  Replace symbolic link with its value

            else
               if Is_Absolute_Path (Link_Buffer (1 .. Status)) then
                  Path_Buffer (Status + 1 .. End_Path - (Finish - Status)) :=
                  Path_Buffer (Finish + 1 .. End_Path);
                  End_Path := End_Path - (Finish - Status);
                  Path_Buffer (1 .. Status) := Link_Buffer (1 .. Status);
                  Last := 1;

               else
                  Path_Buffer
                    (Last + Status + 1 .. End_Path - Finish + Last + Status) :=
                    Path_Buffer (Finish + 1 .. End_Path);
                  End_Path := End_Path - Finish + Last + Status;
                  Path_Buffer (Last + 1 .. Last + Status) :=
                    Link_Buffer (1 .. Status);
               end if;
            end if;

         else
            Last := Finish + 1;
         end if;
      end loop;

      --  Too many iterations: give up

      --  This can happen when there is a circularity in the symbolic links: A
      --  is a symbolic link for B, which itself is a symbolic link, and the
      --  target of B or of another symbolic link target of B is A. In this
      --  case, we return an empty string to indicate failure to resolve.

      return "";
   end Normalize_Pathname;

   ---------------
   -- Open_Read --
   ---------------

   function Open_Read
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor
   is
      pragma Unreferenced (Name, Fmode);
   begin
      --  Not implemented
      return Invalid_FD;
   end Open_Read;

   function Open_Read
     (Name  : String;
      Fmode : Mode) return File_Descriptor
   is
      function C_Open_Read
        (Name  : String;
         Fmode : Mode) return File_Descriptor;
      pragma Import (CIL, C_Open_Read,
                     "mgnat.adalib.GNAT_libc.__gnat_open_read");
   begin
      return C_Open_Read (Name, Fmode);
   end Open_Read;

   ---------------------
   -- Open_Read_Write --
   ---------------------

   function Open_Read_Write
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor
   is
      pragma Unreferenced (Name, Fmode);
   begin
      --  Not implemented
      return Invalid_FD;
   end Open_Read_Write;

   function Open_Read_Write
     (Name  : String;
      Fmode : Mode) return File_Descriptor
   is
      function C_Open_Read_Write
        (Name  : String;
         Fmode : Mode) return File_Descriptor;
      pragma Import (CIL, C_Open_Read_Write,
                     "mgnat.adalib.GNAT_libc.__gnat_open_rw");
   begin
      return C_Open_Read_Write (Name, Fmode);
   end Open_Read_Write;

   -------------
   -- OS_Exit --
   -------------

   procedure OS_Exit (Status : Integer) is
   begin
      OS_Exit_Ptr (Status);
      raise Program_Error;
   end OS_Exit;

   ---------------------
   -- OS_Exit_Default --
   ---------------------

   procedure OS_Exit_Default (Status : Integer) is
      procedure GNAT_OS_Exit (Status : Integer);
      pragma Import (C, GNAT_OS_Exit, "__gnat_os_exit");
      pragma No_Return (GNAT_OS_Exit);
   begin
      GNAT_OS_Exit (Status);
   end OS_Exit_Default;

   --------------------
   -- Pid_To_Integer --
   --------------------

   function Pid_To_Integer (Pid : Process_Id) return Integer is
   begin
      return Integer (Pid);
   end Pid_To_Integer;

   ----------
   -- Read --
   ----------

   function Read
     (FD : File_Descriptor;
      A  : System.Address;
      N  : Integer) return Integer
   is
   begin
      return Integer (System.CRTL.read
        (System.CRTL.int (FD), System.CRTL.chars (A), System.CRTL.size_t (N)));
   end Read;

   -----------------
   -- Rename_File --
   -----------------

   procedure Rename_File
     (Old_Name : C_File_Name;
      New_Name : C_File_Name;
      Success  : out Boolean)
   is
      pragma Unreferenced (Old_Name, New_Name);
   begin
      --  Not implemented
      Success := False;
   end Rename_File;

   procedure Rename_File
     (Old_Name : String;
      New_Name : String;
      Success  : out Boolean)
   is
      function Rename (From, To : String) return Boolean;
      pragma Import (CIL, Rename,
                     "mgnat.adalib.GNAT_libc.__gnat_rename_file");
   begin
      Success := Rename (Old_Name, New_Name);
   end Rename_File;

   -----------------------
   -- Set_Close_On_Exec --
   -----------------------

   procedure Set_Close_On_Exec
     (FD            : File_Descriptor;
      Close_On_Exec : Boolean;
      Status        : out Boolean)
   is
      function C_Set_Close_On_Exec
        (FD : File_Descriptor; Close_On_Exec : System.CRTL.int)
         return System.CRTL.int;
      pragma Import (C, C_Set_Close_On_Exec, "__gnat_set_close_on_exec");
   begin
      Status := C_Set_Close_On_Exec (FD, Boolean'Pos (Close_On_Exec)) = 0;
   end Set_Close_On_Exec;

   --------------------
   -- Set_Executable --
   --------------------

   procedure Set_Executable (Name : String) is
      procedure C_Set_Executable (Name : String);
      pragma Import (C, C_Set_Executable, "__gnat_set_executable");
   begin
      C_Set_Executable (Name);
   end Set_Executable;

   ----------------------
   -- Set_Non_Readable --
   ----------------------

   procedure Set_Non_Readable (Name : String) is
      procedure C_Set_Non_Readable (Name : String);
      pragma Import (C, C_Set_Non_Readable, "__gnat_set_non_readable");
   begin
      C_Set_Non_Readable (Name);
   end Set_Non_Readable;

   ----------------------
   -- Set_Non_Writable --
   ----------------------

   procedure Set_Non_Writable (Name : String) is
      procedure C_Set_Read_Only (Name : String);
      pragma Import (C, C_Set_Read_Only, "__gnat_set_readonly");
   begin
      C_Set_Read_Only (Name);
   end Set_Non_Writable;

   ------------------
   -- Set_Readable --
   ------------------

   procedure Set_Readable (Name : String) is
      procedure C_Set_Readable (Name : String);
      pragma Import (C, C_Set_Readable, "__gnat_set_readable");
   begin
      C_Set_Readable (Name);
   end Set_Readable;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable (Name : String) is
      procedure C_Set_Writable (Name : String);
      pragma Import (C, C_Set_Writable, "__gnat_set_writable");
   begin
      C_Set_Writable (Name);
   end Set_Writable;

   ------------
   -- Setenv --
   ------------

   procedure Setenv (Name : String; Value : String) is
      procedure Set_Env_Value (Name, Value : String);
      pragma Import (CIL, Set_Env_Value,
                     "mgnat.adalib.GNAT_libc.__gnat_setenv");

   begin
      Set_Env_Value (Name, Value);
   end Setenv;

   -----------
   -- Spawn --
   -----------

   function Spawn
     (Program_Name : String;
      Args         : Argument_List) return Integer
   is
      Result : Integer;
      Junk   : Process_Id;
      pragma Warnings (Off, Junk);
   begin
      Spawn_Internal (Program_Name, Args, Result, Junk, Blocking => True);
      return Result;
   end Spawn;

   procedure Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Success      : out Boolean)
   is
   begin
      Success := (Spawn (Program_Name, Args) = 0);
   end Spawn;

   procedure Spawn
     (Program_Name           : String;
      Args                   : Argument_List;
      Output_File_Descriptor : File_Descriptor;
      Return_Code            : out Integer;
      Err_To_Out             : Boolean := True)
   is
      Pid : Process_Id;
      pragma Unreferenced (Pid);
   begin
      --  Spawn the program
      Spawn_Internal (Program_Name, Args, Return_Code, Pid,
                      Blocking    => True,
                      Redirect_Fd => Output_File_Descriptor,
                      Err_To_Out  => Err_To_Out);
   end Spawn;

   procedure Spawn
     (Program_Name  : String;
      Args          : Argument_List;
      Output_File   : String;
      Success       : out Boolean;
      Return_Code   : out Integer;
      Err_To_Out    : Boolean := True)
   is
      FD : File_Descriptor;

   begin
      Success := True;
      Return_Code := 0;

      FD := Create_Output_Text_File (Output_File);

      if FD = Invalid_FD then
         Success := False;
         return;
      end if;

      Spawn (Program_Name, Args, FD, Return_Code, Err_To_Out);

      Close (FD, Success);
   end Spawn;

   --------------------
   -- Spawn_Internal --
   --------------------

   procedure Spawn_Internal
     (Program_Name : String;
      Args         : Argument_List;
      Result       : out Integer;
      Pid          : out Process_Id;
      Blocking     : Boolean;
      Redirect_Fd  : File_Descriptor := Invalid_FD;
      Err_To_Out   : Boolean := True)
   is

      N_Args : Argument_List (Args'Range);
      --  Normalized arguments

      function Portable_Spawn
        (Cmd        : String;
         Args       : Argument_List;
         FD         : File_Descriptor;
         Err_To_Out : Boolean) return Integer;
      pragma Import
        (CIL, Portable_Spawn,
         "mgnat.adalib.GNAT_libc.__gnat_portable_spawn");

      function Portable_No_Block_Spawn
        (Cmd        : String;
         Args       : Argument_List;
         FD         : File_Descriptor;
         Err_To_Out : Boolean) return Process_Id;
      pragma Import
        (CIL, Portable_No_Block_Spawn,
         "mgnat.adalib.GNAT_libc.__gnat_portable_no_block_spawn");

   --  Start of processing for Spawn_Internal

   begin
      --  Copy arguments into a local structure

      for K in N_Args'Range loop
         N_Args (K) := new String'(Args (K).all);
      end loop;

      --  Normalize those arguments

      Normalize_Arguments (N_Args);

      --  Call spawn using the normalized arguments

      if Blocking then
         Pid     := Invalid_Pid;
         Result  := Portable_Spawn
           (Program_Name, N_Args, Redirect_Fd, Err_To_Out);
      else
         Pid     := Portable_No_Block_Spawn
           (Program_Name, N_Args, Redirect_Fd, Err_To_Out);
         Result  := Boolean'Pos (Pid /= Invalid_Pid);
      end if;

      --  Free arguments list

      for K in N_Args'Range loop
         Free (N_Args (K));
      end loop;
   end Spawn_Internal;

   ---------------------------
   -- To_Path_String_Access --
   ---------------------------

   function To_Path_String_Access
     (Path_Addr : Address;
      Path_Len  : Integer) return String_Access
   is
      subtype Path_String is String (1 .. Path_Len);
      type    Path_String_Access is access Path_String;

      function Address_To_Access is new
        Ada.Unchecked_Conversion (Source => Address,
                              Target => Path_String_Access);

      Path_Access : constant Path_String_Access :=
                      Address_To_Access (Path_Addr);

      Return_Val  : String_Access;

   begin
      Return_Val := new String (1 .. Path_Len);

      for J in 1 .. Path_Len loop
         Return_Val (J) := Path_Access (J);
      end loop;

      return Return_Val;
   end To_Path_String_Access;

   ------------------
   -- Wait_Process --
   ------------------

   procedure Wait_Process (Pid : out Process_Id; Success : out Boolean) is
      Status : Integer;

      function Portable_Wait (S : Address) return Process_Id;
      pragma Import (C, Portable_Wait, "__gnat_portable_wait");

   begin
      Pid := Portable_Wait (Status'Address);
      Success := (Status = 0);
   end Wait_Process;

   -----------
   -- Write --
   -----------

   function Write
     (FD : File_Descriptor;
      A  : System.Address;
      N  : Integer) return Integer
   is
   begin
      return Integer (System.CRTL.write
        (System.CRTL.int (FD), System.CRTL.chars (A), System.CRTL.size_t (N)));
   end Write;

end System.OS_Lib;
