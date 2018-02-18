-----------------------------------------------------------------------
-- declarations-subprograms.ads
--
-- Author: Robert A. French
-- E-mail: rfrench99@hotmail.com
--
-- Description:
-- This is used to format any type of subprogram, ie, packages,
-- procedures, functions, tasks.
-----------------------------------------------------------------------
with Lexer;
-----------------------------------------------------------------------
-- DECLARATIONS.SUBPROGRAMS
-- This package handles the reading and formatting of functions, 
-- packages, procedures, etc. It is a child of Declarations for 
-- organizational purposes... subprogram declarations fall under
-- Declarations at least in name.
-----------------------------------------------------------------------
package Declarations.Subprograms is

   procedure Reformat (
         From   : in     Lexer.StringPointer;
         First  : in out Positive;
         Into   : in out Lexer.StringPointer;
         Index  : in out Positive;
         Indent : in out Natural;
         Withed : in     Boolean
       );
   --| This procedure is used to reformat a subprogram declaration. It
   --| will modify indent if the subprogram declaration is not just a
   --| forward declaration, but also defines the body of the given
   --| subprogram. Note: this actually handles functions, packages,
   --| procedures, and tasks, not all of which are necessarily
   --| subprograms, but nonetheless can be handled similarly.

end Declarations.Subprograms;
-----------------------------------------------------------------------
