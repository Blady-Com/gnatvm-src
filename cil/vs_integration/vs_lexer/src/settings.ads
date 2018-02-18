-----------------------------------------------------------------------
-- settings.ads
--
-- Author: Robert A. French and Martin C. Carlisle
-- E-mail: carlislem@acm.org
--
-- Description:
-- This package allows retrieval of formatter settings, such as various
-- indent levels. Not all are currently used, nor is there currently
-- any way to change the settings (simple to fix, though).
-----------------------------------------------------------------------
-- Change log:
-- July 14, 2003 (alf) -- Addes "New_Line_Char" and "Set_New_Line_Char"
-----------------------------------------------------------------------
-- SETTINGS
-- This package contains functions and procedures to manipulate various
-- reformatter settings.
-----------------------------------------------------------------------
package Settings is

   function Normal_Indent return Positive;
   --| This function returns the current indentation size, in spaces.
   procedure Set_Normal_Indent(To : Positive);

   function Break_Indent return Positive;
   --| This is the number of spaces to indent on (most) line breaks.
   procedure Set_Break_Indent(To : Positive);

   function Break_Lines return Positive;
   --| where do we start breaking lines
   procedure Set_Break_Lines(To : Positive);

   function Do_Vertical_Spacing return Boolean;
   procedure Set_Do_Vertical_Spacing(To : Boolean);
   --| Do we try to do one statement per line (change where LFs
   --| occur)?  Also, do we reformat procedure calls and decls?

   function Reformat_Types return Boolean;
   --| Since variant types aren't handled, provide separate mechanism
   --| to disable this.
   procedure Set_Reformat_Types(To : Boolean);
   
   function Enum_Indent return Positive;
   --| This function returns the number of spaces to indent values of
   --| enumeration types in the type definition.
   procedure Set_Enum_Indent(To : Positive);

   function Record_Indent return Positive;
   --| This function returns the number of spaces to indent "begin" and
   --| "end" in record declarations, if applicable.
   procedure Set_Record_Indent(To : Positive);

   function Field_Indent return Positive;
   --| This returns the indentation for the fields of a record, 
   --| relative to the "type" word, not "begin".
   procedure Set_Field_Indent(To : Positive);

   function Parm_Indent return Positive;
   --| This function returns the number of spaces to indent parameters.
   procedure Set_Parm_Indent(To : Positive);

   function Paren_Indent return Positive;
   --| This returns the indentation for the paren after parameters.
   procedure Set_Paren_Indent(To : Positive);

   type Case_Mode is (Ada_RM95, Save_Space, Short_Lists);
   function Case_Style return Case_Mode;
   --| This returns the type of formatting that should be applied to
   --| case statements.
   procedure Set_Case_Style(To : Case_Mode);

   type Text_Case is (Upper_Case, Lower_Case, Mixed_No_Force_Lower,
      Mixed_Force_Lower, Dont_Change);
   -- Case options for outputting letters.

   function Identifier_Case return Text_Case;
   procedure Set_Identifier_Case (To : in Text_Case);
   function Keyword_Case return Text_Case;
   procedure Set_Keyword_Case (To : in Text_Case);
   -- Return the case settings for identifiers or reserved words.
   -- Set the types of capitalization to use.

   -- type to specify colorization option
   TYPE Colorize_Option IS ( Colorize_Only, Colorize, Bold_Only,
                             Bold, Font_No_Color, No_Colorize );
   function Colorization return Colorize_Option;
   procedure Set_Colorization(To : in Colorize_Option);
   
   function Indent_Nested_Paren return Boolean;
   procedure Set_Indent_Nested_Paren(To : Boolean);
   -- Do we indent a continuation line based on paren level
   
   function New_Line_Str return String;
   function New_Line_Char return Character;
   function New_Line_Char2 return Character;
  procedure Set_New_Line_Char (nlc : string);
end Settings;
-----------------------------------------------------------------------   
