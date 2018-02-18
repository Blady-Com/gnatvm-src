------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J _ B A S I C S                              --
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

--  This package contains basic utilities used by the JGNAT back-end

with J_Types;  use J_Types;
with JVM_File; use JVM_File;

package J_Basics is

   Debug_ON : constant Boolean := True;

   procedure Debug_Msg (S : String);
   --  Outputs S to standard out when Debug_ON is True

   function Strip (S : String) return String;
   --  Strips all leading and trailing spaces from S and returns the stripped
   --  string.

   function Right_Justify (S : String; Pos : Positive := 5) return String;
   --  Strips all leading and trailing blanks from S and right justifies it in
   --  a string of Pos blanks.

   function Left_Justify (S : String; Pos : Positive := 30) return String;
   --  Strips all leading and trailing blanks from S and left justifies it in
   --  a string of Pos blanks.

   function To_String (Stream : Stream_Of_U1) return String;
   --  Converts Stream into a stream, by converting each byte into the
   --  corresponding character.
   --  The returned string has always its first component at position 1.

   function To_Stream_Of_U1 (S : String) return Stream_Of_U1;
   --  Converts S into a Stream_Of_U1

   function Get_Stream_Of_U1
     (File_Name : String;
      Dont_Fail : Boolean := False)
      return      Stream_Of_U1_Ptr;
   --  Give the name of a file, this routine reads the file and returns the
   --  stream of bytes contained in the file. If the file cannot be opened
   --  and Dont_Fail is False an error message is emitted using Osint.Fail
   --  and execution is halted. Otherwise the null pointer is returned in
   --  the case of a failed file read operation.

   procedure Put_Stream_Of_U1 (Stream : Stream_Of_U1; File_Name : String);
   --  Give the name of a file, this routine writes the stream to the file
   --  If the file cannot be opened or written, an error message is emitted
   --  using Osint.Fail and execution is halted.

   type Line_Position is record
      First : Nat_32;
      Last  : Nat_32;
   end record;
   type Line_Table is array (Pos_32 range <>) of Line_Position;
   function Get_Line_Table (Source_File : Stream_Of_U1) return Line_Table;
   --  Given a source file as a stream of U1, this routine returns a table L,
   --  starting at index 1 and such that L (K) gives the position where the
   --  K-th line of Source_File begins and ends. Specifically, L (K).First
   --  gives the index in Source_File of the first character of the K-th line,
   --  whereas L (K).Last gives the index of the last printable character in
   --  the line. Using a First and Last makes things more portable because the
   --  end of a line is marked by a LF (line feed) on UNIX machines, whereas it
   --  is marked by a CR (carriage return) followed by a LF on Windows
   --  platforms.  If Source_File is empty, return an empty Line_Table.

   function Get_Utf8 (T : CP.Table; K : CP_Index) return Utf8.Table;
   --  Returns the Utf8 which is at the K-th entry of constant pool T. If there
   --  is no Utf8 at such entry an exception is raised.

   function Is_Java_Lang_Object (T : CP.Table; K : CP_Index) return Boolean;
   --  The K-the entry in table T must be a CONSTANT_Class entry, otherwise an
   --  exception is raised. The function returns True if this class is
   --  java.lang.Object, False otherwise.

   procedure Fail (S : String);
   pragma No_Return (Fail);
   --  Outputs error message S and exits with error code 4.

private
   pragma Inline (Debug_Msg);

end J_Basics;
