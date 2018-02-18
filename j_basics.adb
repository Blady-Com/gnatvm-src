------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J _ B A S I C S                             --
--                                                                          --
--                                  B o d y                                 --
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

with Ada.Characters.Latin_1;
with GNAT.OS_Lib;
with GNAT.IO; use GNAT.IO;

package body J_Basics is

   ----------
   -- Fail --
   ----------

   E_Fatal : constant := 4;

   procedure Fail (S : String) is
   begin
      Put_Line (S);
      GNAT.OS_Lib.OS_Exit (E_Fatal);
   end Fail;

   ---------------
   -- Debug_Msg --
   ---------------

   procedure Debug_Msg (S : String) is
   begin
      if Debug_ON then
         Put_Line (S);
      end if;
   end Debug_Msg;

   --------------------
   -- Get_Line_Table --
   --------------------

   function Get_Line_Table (Source_File : Stream_Of_U1) return Line_Table is
      LF : constant := Character'Pos (Ada.Characters.Latin_1.LF);
      CR : constant := Character'Pos (Ada.Characters.Latin_1.CR);

      Total_Lines : Nat_32 := 0;

   begin
      for K in Source_File'Range loop
         if Source_File (K) = LF or else K = Source_File'Last then
            Total_Lines := Total_Lines + 1;
         end if;
      end loop;

      declare
         Line : Line_Table (1 .. Total_Lines);
         N    : Nat_32 := 0;
         --  Current line number

         New_Line_Starts : Boolean := True;

      begin
         for K in Source_File'Range loop
            if New_Line_Starts then
               N := N + 1;
               Line (N).First := K;
               New_Line_Starts := False;
            end if;

            if Source_File (K) = LF then
               New_Line_Starts := True;

               if K > Source_File'First and then Source_File (K - 1) = CR then
                  Line (N).Last := K - 2;
               else
                  Line (N).Last := K - 1;
               end if;

            elsif K = Source_File'Last then
               Line (N).Last := K;
            end if;
         end loop;

         return Line;
      end;
   end Get_Line_Table;

   ----------------------
   -- Get_Stream_Of_U1 --
   ----------------------

   function Get_Stream_Of_U1
     (File_Name : String;
      Dont_Fail : Boolean := False)
      return      Stream_Of_U1_Ptr
   is
      use GNAT.OS_Lib;

      File_Name_0 : constant String := File_Name & ASCII.NUL;
      File_Len    : Integer;
      Input_FD    : File_Descriptor;

   begin
      Input_FD := Open_Read (File_Name_0'Address, Binary);

      if Input_FD = Invalid_FD then
         if Dont_Fail then
            return null;
         else
            Fail ("Cannot find: " & File_Name);
         end if;
      end if;

      File_Len := Integer (File_Length (Input_FD));

      Read_The_Stream : declare
         Lo     : constant Nat_32  := 1;
         Hi     : Nat_32           := Nat_32 (File_Len);
         Stream : Stream_Of_U1_Ptr := new Stream_Of_U1 (Lo .. Hi);

         Actual_Len : Integer;

      begin
         --  Some systems (e.g. VMS) have file types that require one
         --  read per line, so read until we get the File_Len bytes or until
         --  there are no more characters.

         Hi := Lo;
         loop
            Actual_Len := Read (Input_FD, Stream (Hi)'Address, File_Len);

            if Actual_Len > 0 then
               Hi := Hi + Int_32 (Actual_Len);
            end if;

            exit when Actual_Len = File_Len or Actual_Len <= 0;
         end loop;

         Close (Input_FD);
         return Stream;
      end Read_The_Stream;
   end Get_Stream_Of_U1;

   --------------
   -- Get_Utf8 --
   --------------

   function Get_Utf8 (T : CP.Table; K : CP_Index) return Utf8.Table is
      R : constant CP_Info := CP.Get (T, K);
   begin
      pragma Assert (R.Tag = CONSTANT_Utf8);
      return R.Str_Bytes;
   end Get_Utf8;

   -------------------------
   -- Is_Java_Lang_Object --
   -------------------------

   function Is_Java_Lang_Object (T : CP.Table; K : CP_Index) return Boolean is
      R  : constant CP_Info := CP.Get (T, K);
      UT : Utf8.Table;
   begin
      pragma Assert (R.Tag = CONSTANT_Class);
      UT := Get_Utf8 (T, R.Class_Name_Index);

      if Has_Wide_Chars (UT) then
         return False;
      else
         return To_String (UT) = "java/lang/Object";
      end if;
   end Is_Java_Lang_Object;

   ------------------
   -- Left_Justify --
   ------------------

   function Left_Justify (S : String; Pos : Positive := 30) return String is
      New_S : constant String := Strip (S);
   begin
      return New_S & String'(1 .. Pos - New_S'Length => ' ');
   end Left_Justify;

   ----------------------
   -- Put_Stream_Of_U1 --
   ----------------------

   procedure Put_Stream_Of_U1 (Stream : Stream_Of_U1; File_Name : String) is
      use GNAT.OS_Lib;

      File_Name_0 : constant String := File_Name & ASCII.NUL;
      Output_FD   : File_Descriptor;

   begin
      Output_FD := Create_File (File_Name_0'Address, Binary);

      if Output_FD = Invalid_FD then
         Fail ("Cannot create: " & File_Name);
      end if;

      if Integer (Stream'Length) /=
        Write (Output_FD, Stream'Address, Integer (Stream'Length))
      then
         Fail ("error: disk full writing " & File_Name);
      end if;

      Close (Output_FD);
   end Put_Stream_Of_U1;

   -------------------
   -- Right_Justify --
   -------------------

   function Right_Justify (S : String; Pos : Positive := 5) return String is
      New_S : constant String := Strip (S);
   begin
      return String'(1 .. Pos - New_S'Length => ' ') & New_S;
   end Right_Justify;

   -----------
   -- Strip --
   -----------

   function Strip (S : String) return String is
      F : Integer := S'First;
      L : Integer := S'Last;

   begin
      while F <= L and then S (F) = ' ' loop
         F := F + 1;
      end loop;

      while F <= L and then S (L) = ' ' loop
         L := L - 1;
      end loop;

      return S (F .. L);
   end Strip;

   ---------------------
   -- To_Stream_Of_U1 --
   ---------------------

   function To_Stream_Of_U1 (S : String) return Stream_Of_U1 is
      Stream : Stream_Of_U1 (1 .. Nat_32 (S'Length));
      P      : Nat_32 := 1;

   begin
      for K in S'Range loop
         Stream (P) := U1 (Character'Pos (S (K)));
         P := P + 1;
      end loop;
      return Stream;
   end To_Stream_Of_U1;

   ---------------
   -- To_String --
   ---------------

   function To_String (Stream : Stream_Of_U1) return String is
      S : String (1 .. Natural (Stream'Length));
      P : Positive := 1;

   begin
      for K in Stream'Range loop
         S (P) := Character'Val (Stream (K));
         P := P + 1;
      end loop;
      return S;
   end To_String;

end J_Basics;
