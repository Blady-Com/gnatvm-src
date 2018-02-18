-----------------------------------------------------------------------
-- declarations.ads
--
-- Author: Robert A. French
-- E-mail: rfrench99@hotmail.com
--
-- Description:
-- Defines the root of the "Declarations" hierarchy for the Ada95 code
-- reformatter. Declarations are basically any Ada construct that is
-- not a statement per se. For example, procedures, functions, types,
-- packages, variables, etc.
-----------------------------------------------------------------------
with Ada.Finalization;
with Ada.Unchecked_Deallocation;

with Lexer;
-----------------------------------------------------------------------
-- DECLARATIONS
-- This package constructs the abstract Declaration class that is the
-- root of the Declaration class hierarchy. It provides memory-use
-- facilities as well as a standard interface for operating on a basic
-- Declaration entry.
-----------------------------------------------------------------------
package Declarations is

   type Declaration is 
         abstract new Ada.Finalization.Controlled with private;
   --| This is the basic Declaration class, containing information that
   --| will probably be present in some way in any plausible idea of a 
   --| Declaration.

   procedure Initialize (Object : in out Declaration);
   procedure Adjust (Object : in out Declaration);
   procedure Finalize (Object : in out Declaration);
   --| These are overriden from the Controlled class, and are used to
   --| implement deep copies of the strings in a Declaration as well as
   --| ensure proper allocation/deallocation of memory used by string
   --| pointers.   

   procedure Read (
         Item  :    out Declaration;
         From  : in     Lexer.StringPointer;
         First : in out Positive
       ) is abstract;
   --| This method reads the input string for a Declaration and stores
   --| it in the given Declaration object. On return, the input string
   --| will be ready to read the next available token.

   procedure Write (
         Item   : in out Declaration;
         Into   : in out Lexer.StringPointer;
         Index  : in out Positive;
         Indent : in     Natural := 0
       );
   --| This method writes the Declaration to the given output string.

   procedure Reset (Item : in out Declaration'Class);
   --| This method resets the fields of a Declaration (ie, free up the
   --| string memory, set booleans back to false, etc).

   procedure Free is new Ada.Unchecked_Deallocation (
         Object => String,
         Name   => Lexer.StringPointer
       );
   --| This is used throughout the Declaration classes to deallocate
   --| strings pointed to by StringPointers.

   procedure Get_Comments (
         Line  : in     Integer;
         From  : in     Lexer.StringPointer;
         First : in out Positive;
         Into  : in out Lexer.StringPointer;
         Index : in out Positive
       );
   --| This procedure is here because it will be useful to various
   --| Declaration classes. It appends all subsequent comment tokens
   --| from the input to the output ONLY IF they're on the same line
   --| that is specified. This is useful for eating up a possible 
   --| comment at the end of a declaration, but not those comments on
   --| subsequent lines. This may aid maintaining some of the author's
   --| original format for comments.

private

   type Declaration is abstract new Ada.Finalization.Controlled with
      record
         Name_Str : Lexer.StringPointer;
         Name_End : Positive;
         --| Every Declaration, of whatever derivation, should have
         --| some sort of name field (or could have, at least).
         Has_Comment : Boolean;
         Comment_Str : Lexer.StringPointer;
         Comment_End : Positive;
         --| There is always the posibility of a comment, on any line,
         --| anytime, anyplace, anyway...
         Output_Str : Lexer.StringPointer;
         Output_End : Positive;
         --| All output from Declaration classes should be through this
         --| mechanism, ie, the formatting method should place the 
         --| output line in here.
      end record;

end Declarations;
-----------------------------------------------------------------------
