-----------------------------------------------------------------------
-- declarations-variables.ads
--
-- Author: Robert A. French
-- E-mail: rfrench99@hotmail.com
--
-- Description:
-- Defines the "Variables" subclass. This is basically used for places
-- that variables/constants could be declared, including functions,
-- procedures, or even record components.
-----------------------------------------------------------------------
with Lexer;
-----------------------------------------------------------------------
-- DECLARATIONS.VARIABLES
-- This package defines the Variable subclass of Declaration. This 
-- class is used anywhere there are non-parameter variable declarations
-- such as in a procedure/function's declaration area, inside a record,
-- or in a declare-begin-end block. It is a child of Declarations in
-- order to have visibility into the private part of Declarations.
-----------------------------------------------------------------------
package Declarations.Variables is

   type Variable is new Declaration with private;
   --| This is the basic Variable class, containing slightly more data
   --| than a simple abstract Declaration.

   procedure Initialize (Object : in out Variable);
   procedure Adjust (Object : in out Variable);
   procedure Finalize (Object : in out Variable);
   --| These handle the newly added fields.

   procedure Read (
         Item  :    out Variable;
         From  : in     Lexer.StringPointer;
         First : in out Positive
       );
   --| This method reads the input string for a Variable and stores it
   --| in the given Variable object.

   procedure Write (
         Item   : in out Variable;
         Into   : in out Lexer.StringPointer;
         Index  : in out Positive;
         Indent : in     Natural := 0
       );
   --| This method writes the Variable to the given output string.

   procedure Reformat (
         From   : in     Lexer.StringPointer;
         First  : in out Positive;
         Into   : in out Lexer.StringPointer;
         Index  : in out Positive;
         Indent : in     Natural := 0
       );
   --| This is the interface for reformatting lists of Variable
   --| Declarations. It will format a list of Variables, including
   --| possible leading comments, and write them (formatted) into the
   --| output string.

private

   type Variable is new Declaration with
      record
         Has_Comma,
         Has_Semicolon : Boolean;
         --| Either of these signifies the termination of the current
         --| Variable Declaration.
         Has_Type : Boolean;
         Type_Str : Lexer.StringPointer;
         Type_End : Positive;
         --| The presence of a type name indicates as well that it was
         --| preceeded by a colon.
         Has_Value : Boolean;
         Value_Str : Lexer.StringPointer;
         Value_End : Positive;
         --| The presence of a default value specification means that
         --| it was preceeded by an assignment operator.
         Is_Constant : Boolean;
         --| This can only be true if the Variable also has a colon and
         --| a type name.
         Colon_Location      : Natural := 0;
         Assignment_Location : Natural := 0;
         Comment_Location    : Natural := 0;
      end record;

end Declarations.Variables;
-----------------------------------------------------------------------
