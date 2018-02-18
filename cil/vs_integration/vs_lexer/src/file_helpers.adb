---------------------------------------------------------------
--                                                           
--  FILE_HELPERS.ADB 
--  Description : IO helpers
--                       
--  By: Martin Carlisle
--      United States Air Force Academy
--                                         
-- FILE_HELPERS is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- FILE_HELPERS is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------

with Ada.Characters.Handling;

package body File_Helpers is
   ----------------------------------------------------
   -- PROCEDURE Get_String
   -- 
   -- reads from file the next string.  If first non-blank
   -- is ", then reads until ending ", o/w reads sequence
   -- of non-blank characters.  Stops at end of line.
   ----------------------------------------------------

   procedure Get_String(File : in Ada.Text_IO.File_Type;
         Item : out String; Last : out Natural) is
      Next_Char   : Character;
      Is_Quoted   : Boolean;
   begin -- Get_String
      Last := Item'First-1;

      -- skip any preceding blank space
      loop
         if Ada.Text_IO.End_Of_Line(File) then
            return;
         else
            Ada.Text_IO.Get(File,Next_Char);
         end if;

         exit when Next_Char/=' ' and then Next_Char/=ASCII.HT;
      end loop;

      if Next_Char='"' then
         Is_Quoted := True;
         if Ada.Text_IO.End_Of_Line(File) then
            return;
         else
            Ada.Text_IO.Get(File,Next_Char);
         end if;
      else
         Is_Quoted := False;
      end if;

      Last := Last + 1;
      Item(Last) := Next_Char;

      -- get the rest of the string
      loop
         if Is_Quoted then
            if Ada.Text_IO.End_Of_Line(File) then
               return;
            end if;

            Ada.Text_Io.Get(File,Next_Char);
            if Next_Char = '"' then
               return;
            end if;
         else
            if Ada.Text_IO.End_Of_Line(File) then
               return;
            end if;

            Ada.Text_Io.Get(File,Next_Char);
            if Next_Char = ' ' or else Next_Char = ASCII.HT then
               return;
            end if;
         end if;

         Last := Last + 1;
         Item(Last) := Next_Char;
      end loop;

   end Get_String;

   ----------------------------------------------------
   -- PROCEDURE Get_Number
   -- 
   -- given the first character, reads from file the 
   -- next number.  First_Char ends with character following
   -- the number
   ----------------------------------------------------
   procedure Get_Number(File : in Ada.Text_IO.File_Type;
         Item : out Integer; First_Char : in out Character) is
   begin
      -- loop until start of number
      while not Ada.Characters.Handling.Is_Digit(First_Char) loop
         Ada.Text_IO.Get(File => File, Item => First_Char);
      end loop;
      
      Item := Character'Pos(First_Char) - Character'Pos('0');
      while not Ada.Text_IO.End_Of_Line(File) loop
         Ada.Text_IO.Get(File => File, Item => First_Char);
         exit when not Ada.Characters.Handling.Is_Digit(First_Char);
         Item := Item * 10;
         Item := Item + Character'Pos(First_Char) - Character'Pos('0');
      end loop;
   end Get_Number;
end File_Helpers;
