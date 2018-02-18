-----------------------------------------------------------------------
-- declarations-parameters.ads
--
-- Author: Robert A. French
-- E-mail: rfrench99@hotmail.com
--
-- Description:
-- Defines the "Parameters" subclass. This is basically used for
-- anything that could possibly approximate parameters, whether for 
-- functions, procedures, packages, whatever.
-----------------------------------------------------------------------
with Lexer;

with Parm_Modes;
-----------------------------------------------------------------------
-- DECLARATIONS.PARAMETERS
-- This package defines the Parameter subclass of Declaration. This
-- class is used anywhere there are parameter declarations, eg, for 
-- function/procedure definitions. It is a child of Declarations in
-- order to have visibility into the private part of Declarations.
-----------------------------------------------------------------------
package Declarations.Parameters is

   type Parameter is new Declaration with private;
   --| This is the basic Parameter class, containing slightly more data
   --| than a simple abstract Declaration.

   procedure Initialize (Object : in out Parameter);
   procedure Adjust (Object : in out Parameter);
   procedure Finalize (Object : in out Parameter);
   --| These handle the newly added fields.

   procedure Read (
         Item  :    out Parameter;
         From  : in     Lexer.StringPointer;
         First : in out Positive
       );
   --| This method reads the input string for a Parameter and stores it
   --| in the given Parameter object.

   procedure Write (
         Item   : in out Parameter;
         Into   : in out Lexer.StringPointer;
         Index  : in out Positive;
         Indent : in     Natural := 0
       );
   --| This method writes the Parameter to the given output string.

   procedure Suppress_Indent;
   --| This procedure suppresses indentation of the first line on the
   --| next call to Reformat; it is only effective on the next call.
   --| This is used to start the Parameter list immediately following a
   --| left paren, rather than on its own line.

   procedure Reformat (
         From   : in     Lexer.StringPointer;
         First  : in out Positive;
         Into   : in out Lexer.StringPointer;
         Index  : in out Positive;
         Indent : in     Natural := 0
       );
   --| This is the interface for reformatting lists of Parameter
   --| Declarations. It will format a list of Parameters, including
   --| possible embedded comments, and write them (formatted) into the
   --| output string.

private

   type Parameter is new Declaration with
      record
         Has_Comma,
         Has_Semicolon : Boolean;
         --| Either of these signifies the termination of the current
         --| Parameter Declaration.
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
         Mode : Parm_Modes.Mode;
         --| The parameter can have no mode unless there is a colon and
         --| type name.
      end record;

end Declarations.Parameters;
-----------------------------------------------------------------------
