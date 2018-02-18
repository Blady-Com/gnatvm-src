-----------------------------------------------------------------------
-- declarations-types.ads
--
-- Author: Robert A. French
-- E-mail: rfrench99@hotmail.com
--
-- Description:
-- This is used to format type and subtype declarations.
-----------------------------------------------------------------------
with Lexer;
-----------------------------------------------------------------------
-- DECLARATIONS.TYPES
-- This package handles the reading and formatting of type/subtype 
-- definitions.
-----------------------------------------------------------------------
package Declarations.Types is

   procedure Reformat (
         From   : in     Lexer.StringPointer;
         First  : in out Positive;
         Into   : in out Lexer.StringPointer;
         Index  : in out Positive;
         Indent : in     Natural := 0
       );
   --| This procedure is used to reformat a type or subtype declaration
   --| of whatever type. It begins with the TYPE or SUBTYPE reserved
   --| word.

end Declarations.Types;
-----------------------------------------------------------------------
