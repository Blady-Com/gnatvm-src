-----------------------------------------------------------------------
-- decls.ads
--
-- Author: Robert A. French
-- E-mail: rfrench99@hotmail.com
--
-- Description:
-- This is primarily used within type definitions to provide formatting
-- of type components possibly interspersed with comments. The same
-- applies for parameter and argument lists.
-----------------------------------------------------------------------
with Lexer;
-----------------------------------------------------------------------
-- DECLS
-- This package basically allows for lists of declarations that are
-- possibly also separated by comments.
-----------------------------------------------------------------------
package Decls is

   type Decl_Mode is (General, Parameters, Arguments);

   procedure Setup (Mode : in     Decl_Mode);
   --| This procedure determines whether to look for declarations,
   --| parameters, arguments, etc.

   procedure Suppress_Indent;
   --| This procedure suppresses indentatation of the first line on the
   --| next call to Reformat; it is only effective on the next call.

   procedure Reformat (
         From   : in     Lexer.StringPointer;
         First  : in out Positive;
         Into   : in out Lexer.StringPointer;
         Index  : in out Positive;
         Indent : in     Natural := 0
       );
   --| This procedure performs the actual reformatting.

end Decls;
-----------------------------------------------------------------------
