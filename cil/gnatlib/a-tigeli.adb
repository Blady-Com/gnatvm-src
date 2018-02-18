------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          A D A . T E X T _ I O                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
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

--  This version is for VM targets

separate (Ada.Text_IO)
procedure Get_Line
  (File : File_Type;
   Item : out String;
   Last : out Natural)
is
   ch : int;

begin
   FIO.Check_Read_Status (AP (File));
   Last := Item'First - 1;

   --  Immediate exit for null string, this is a case in which we do not
   --  need to test for end of file and we do not skip a line mark under
   --  any circumstances.

   if Last >= Item'Last then
      return;
   end if;

   --  Here we have at least one character, if we are immediately before
   --  a line mark, then we will just skip past it storing no characters.

   if File.Before_LM then
      File.Before_LM := False;
      File.Before_LM_PM := False;

   --  Otherwise we need to read some characters

   else
      ch := Getc (File);

      --  If we are at the end of file now, it means we are trying to
      --  skip a file terminator and we raise End_Error (RM A.10.7(20))

      if ch = EOF then
         raise End_Error;
      end if;

      --  Loop through characters. Don't bother if we hit a page mark,
      --  since in normal files, page marks can only follow line marks
      --  in any case and we only promise to treat the page nonsense
      --  correctly in the absense of such rogue page marks.

      loop
         --  Exit the loop if read is terminated by encountering line mark

         exit when ch = LM;

         --  Otherwise store the character, note that we know that ch is
         --  something other than LM or EOF. It could possibly be a page
         --  mark if there is a stray page mark in the middle of a line,
         --  but this is not an official page mark in any case, since
         --  official page marks can only follow a line mark. The whole
         --  page business is pretty much nonsense anyway, so we do not
         --  want to waste time trying to make sense out of non-standard
         --  page marks in the file! This means that the behavior of
         --  Get_Line is different from repeated Get of a character, but
         --  that's too bad. We only promise that page numbers etc make
         --  sense if the file is formatted in a standard manner.

         --  Note: we do not adjust the column number because it is quicker
         --  to adjust it once at the end of the operation than incrementing
         --  it each time around the loop.

         Last := Last + 1;
         Item (Last) := Character'Val (ch);

         --  All done if the string is full, this is the case in which
         --  we do not skip the following line mark. We need to adjust
         --  the column number in this case.

         if Last = Item'Last then
            File.Col := File.Col + Count (Item'Length);
            return;
         end if;

         --  Otherwise read next character. We also exit from the loop if
         --  we read an end of file. This is the case where the last line
         --  is not terminated with a line mark, and we consider that there
         --  is an implied line mark in this case (this is a non-standard
         --  file, but it is nice to treat it reasonably).

         ch := Getc (File);
         exit when ch = EOF;
      end loop;
   end if;

   --  We have skipped past, but not stored, a line mark. Skip following
   --  page mark if one follows, but do not do this for a non-regular
   --  file (since otherwise we get annoying wait for an extra character)

   File.Line := File.Line + 1;
   File.Col := 1;

   if File.Before_LM_PM then
      File.Line := 1;
      File.Before_LM_PM := False;
      File.Page := File.Page + 1;

   elsif File.Is_Regular_File then
      ch := Getc (File);

      if ch = PM and then File.Is_Regular_File then
         File.Line := 1;
         File.Page := File.Page + 1;
      else
         Ungetc (ch, File);
      end if;
   end if;
end Get_Line;
