-----------------------------------------------------------------------
-- writer.adb
--
-- Author: Robert A. French and Martin C. Carlisle
-- E-mail: carlislem@acm.org
--
-- Description:
-- This package provides various operations that write to a string that
-- is pointed to by a string pointer. 
-----------------------------------------------------------------------
---------------------------------------------------------------
-- Change log:
-- Jun 20, 2003 (alf) -- added checks for lineend to CHOMP_SPACES
--  & Feb 25, 2004       and CHOMP_LINE to support rich edit 2.0
--                       which uses CR to show end of line (and 
--                       does not have LF). 
--                       Changed Write_Line to add either a CR or 
--                       an LF, for adaption to the changes in 
--                       Lexer and Reformat_RTF
---------------------------------------------------------------
with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Lexer;
use type Lexer.StringPointer;
use type Lexer.TokenClass;
with Settings;

-----------------------------------------------------------------------
-- WRITER
-----------------------------------------------------------------------
package body Writer is
   Last_Line_Feed : Natural := 0;
   --------------------------------------------------------------------
   -- FREE
   --------------------------------------------------------------------
   procedure Free is
      new Ada.Unchecked_Deallocation(Object => String,
                                     Name   => Lexer.StringPointer);
   --------------------------------------------------------------------
   -- CHECK_INDEX
   -- Implementation Notes:
   --   - This procedure is used for checking both to see whether a 
   --     string is null or not, and whether the index is within the
   --     bounds of the string. If null, or if the index is out of 
   --     bounds, it raises an exception.
   --------------------------------------------------------------------
   procedure Check_Index (
      Item  : in     Lexer.Stringpointer;
      Index : in     Positive
     ) is
   begin
      if Item = null then
         raise Invalid_String;
      elsif Index not in Item.all'range then
         raise Invalid_Index;
      end if;
   end Check_Index;

   --------------------------------------------------------------------
   -- CHOMP_SPACES
   -- Implementation Notes:
   --   - This procedure starts at index (presumably the end of the 
   --     string), and looks backwards, "eating" spaces until it gets
   --     back to a non-space character.  If it would eat back to a new
   --     line, then do nothing
   --------------------------------------------------------------------
   procedure Chomp_Spaces (
      Into  : in     Lexer.StringPointer;
      Index : in out Positive;
      Do_Nothing_On_New_Line : in Boolean := True
     ) is
     Old_Index : Positive := Index;
   begin
      Check_Index(Into, Index);
      while Index > Into.all'First and then
            Into.all(Index - 1) = ' ' loop
         Index := Index - 1;
      end loop;
      if Do_Nothing_On_New_Line and then
            (Into.All(Index)=Settings.New_Line_Char or
             Into.All(Index)=Settings.New_Line_Char2) then
        Index := Old_Index;
      end if;
   end Chomp_Spaces;
   --------------------------------------------------------------------
   -- CHOMP_LINE
   -- Implementation Notes:
   --   - This procedure starts at index (presumably the end of the 
   --     string), and looks backwards, "eating" spaces and a single
   --     new_line.  Stops if it gets back to a non-space character.
   --------------------------------------------------------------------
   procedure Chomp_Line (
      Into     : in     Lexer.StringPointer;
      Index    : in out Positive
     ) is
   begin
      Check_Index(Into, Index);
      while Index > Into.all'First and then
            (Into.All(Index - 1) = ' ' or
            Into.All(Index - 1) = Settings.New_Line_Char2) loop
         Index := Index - 1;
      end loop;
      if Into.all(Index - 1) = Settings.New_Line_Char then
         Index := Index - 1;
      end if;
   end Chomp_Line;
   --------------------------------------------------------------------
   -- BASIC_WRITE
   -- Implementation Notes:
   --   - This is the "raw" output procedures; given a string, it
   --     writes it to the output string. It also handles any memory
   --     issues; if the string is full, it essentially doubles the
   --     size of the allocated memory.
   --------------------------------------------------------------------
   procedure Basic_Write (
      Item  : in     String;
      Into  : in out Lexer.Stringpointer;
      Index : in out Positive
     ) is
      Temp     : Lexer.Stringpointer;
      Overflow : Integer;
   begin
      Check_Index(Into, Index);
      Overflow := (Index + Item'Length) - Into.all'Last;

      if Overflow > 0 then
         Temp := new String'(Into.all & (Overflow + Into'Length) * ' ');
         Free(Into);
         Into := Temp;
      end if;

      Into.all(Index .. Index + Item'Length - 1) := Item;
      Index := Index + Item'Length;
   end Basic_Write;
   --------------------------------------------------------------------
   -- WRITE
   --------------------------------------------------------------------
   procedure Write (
      Item  : in     String;
      Into  : in out Lexer.Stringpointer;
      Index : in out Positive
     ) is
   begin
      Basic_Write (Item, Into, Index);
   end Write;
   --------------------------------------------------------------------
   -- Indent
   --------------------------------------------------------------------
   procedure Indent (
      Count : in     Natural;
      Into  : in out Lexer.Stringpointer;
      Index : in out Positive
     ) is
   begin  -- Indent
      Basic_Write(Count * ' ', Into, Index);
   end Indent;
   --------------------------------------------------------------------
   -- New_Line
   --------------------------------------------------------------------
   procedure New_Line (
      Into  : in out Lexer.Stringpointer;
      Index : in out Positive
     ) is
   begin
--      Basic_Write(ASCII.LF & "", Into, Index);
      Chomp_Spaces (Into, Index, False);
      Basic_Write(Settings.New_Line_Str & "", Into, Index);
      Last_Line_Feed := Index;
   end New_Line;
   
   function Line_Too_Long(Index : in Natural) return Boolean is
   begin
      return Index-Last_Line_Feed > Settings.Break_Lines;
   end Line_Too_Long;
   
   --------------------------------------------------------------------
   -- FINALIZE
   --------------------------------------------------------------------
   procedure Finalize (
      Item  : in out Lexer.Stringpointer;
      Index : in     Positive
     ) is
      Temp : Lexer.Stringpointer;
   begin
      Check_Index(Item, Index);
      Temp := new String'(Item.all(Item.all'First .. Index - 1));
      Free(Item);
      Item := Temp;
   end Finalize;
   --------------------------------------------------------------------
end Writer;
-----------------------------------------------------------------------      
