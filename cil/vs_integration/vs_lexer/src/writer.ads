-----------------------------------------------------------------------
-- writer.ads
--
-- Author: Robert A. French and Martin C. Carlisle
-- E-mail: carlislem@acm.org
--
-- Description:
-- This package provides various operations that write to a string that
-- is pointed to by a string pointer. 
-----------------------------------------------------------------------
with Lexer;
-----------------------------------------------------------------------
-- WRITER
-----------------------------------------------------------------------
package Writer is

   --------------------------------------------------------------------
   -- EXCEPTIONS
   --------------------------------------------------------------------

   Invalid_String,
   Invalid_Index : exception;
   --| Raised if a StringPointer argument is null, or if the index 
   --| argument is outside the range of the string.

   --------------------------------------------------------------------
   -- METHODS
   --------------------------------------------------------------------

   -- GENERAL PRECONDITIONS
   -- - Into points to an existing string
   -- - Index is within the string's range

   -- GENERAL POSTCONDITIONS
   -- - Item is written to Into at Index
   -- - The string grows if necessary
   -- - Index is after the last item written

   -- GENERAL EXCEPTIONS
   -- - Invalid_String, Invalid_Index

   procedure Write (
         Item  : in     String;
         Into  : in out Lexer.Stringpointer;
         Index : in out Positive
       );

   --| Preconditions  : - From points to an existing string
   --|                  - Item is a token within the string
   --| Postconditions : - The value of Item is written to Into
   --|                  - Pad # of spaces are written after

   procedure Indent (
         Count : in     Natural;
         Into  : in out Lexer.Stringpointer;
         Index : in out Positive
       );
   --| Postconditions : - Writes Count number of spaces

   procedure New_Line (
         Into  : in out Lexer.Stringpointer;
         Index : in out Positive
       );
   --| Postconditions : - Trailing spaces are removed
   --|                  - A newline character (LF) is written

   procedure Chomp_Line (
         Into     : in     Lexer.Stringpointer;
         Index    : in out Positive
       );
   --| Postconditions : - Trailing spaces are removed
   --|                    along with a single newline character (LF)
   --|                    if present

   procedure Chomp_Spaces (
      Into  : in     Lexer.StringPointer;
      Index : in out Positive;
      Do_Nothing_On_New_Line : in Boolean := True
     );
   --| Postconditions : - Trailing spaces are removed
   --|                    similar to Chomp_Line, but doesn't consume
   --|                    LF

   procedure Check_Index (
      Item  : in     Lexer.Stringpointer;
      Index : in     Positive
     );
   --| Postconditions : - either Index is a legal position in Item
   --|                    or an exception is raised

   procedure Finalize (
         Item  : in out Lexer.Stringpointer;
         Index : in     Positive
       );
   --| Postconditions : - Item is saved up to (not including) Index,
   --|                    everything else is deallocated

   function Line_Too_Long(Index : in Natural) return Boolean;
   --| Postconditions : returns true if line length exceeds
   --|                  Settings.Break_Lines
end Writer;
----------------------------------------------------------------------- 
