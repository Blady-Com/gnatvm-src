-----------------------------------------------------------------------
-- token_writer.ads
--
-- Author: Robert A. French and Martin C. Carlisle
-- E-mail: carlislem@acm.org
--
-- Description:
-- This package provides various operations that write to a string that
-- is pointed to by a string pointer. Everything is accessed through a
-- Write procedure of some sort. These procedures also will 
-- ensure proper spacing around Ada95 tokens in >almost< all cases.
--
-- This package also handles Colorization/Bolding as per settings
-----------------------------------------------------------------------
with Lexer;
-----------------------------------------------------------------------
-- WRITER
-----------------------------------------------------------------------
package Token_Writer is

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
         Item  : in     Lexer.Tokens;
         Into  : in out Lexer.StringPointer;
         Index : in out Positive
       );

   procedure Write (
         Item  : in     Lexer.Token;
         From  : in     Lexer.StringPointer;
         Into  : in out Lexer.StringPointer;
         Index : in out Positive;
         Pad   : in     Natural := 1;
         Chomp : in     Boolean := True
       );
   --| Preconditions  : - From points to an existing string
   --|                  - Item is a token within the string
   --| Postconditions : - The value of Item is written to Into
   --|                  - Pad # of spaces are written after
   --|                    Chomp spaces before for certain tokens
   --|                    only if Chomp is true

end Token_Writer;
-----------------------------------------------------------------------
