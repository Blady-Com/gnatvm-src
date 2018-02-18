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
with Ada.Strings.Unbounded;
use ada.Strings.Unbounded;
package body Settings is
   --------------------------------------------------------------------
   -- CONSTANTS
   --------------------------------------------------------------------
   Default_Normal_Indent : constant := 3;
   Default_Break_Indent  : constant := 2;
   Default_Break_Lines   : constant := 72;
   Default_Enum_Indent   : constant := 6;
   Default_Record_Indent : constant := 3;
   Default_Field_Indent  : constant := 6;
   Default_Parm_Indent   : constant := 6;
   Default_Paren_Indent  : constant := 4;

   Default_Case_Style : constant Case_Mode := Ada_RM95;

   Default_Identifier_Case : constant Text_Case := Mixed_No_Force_Lower;
   Default_Keyword_Case    : constant Text_Case := Lower_Case;
   
   Default_Do_Vertical_Spacing : constant Boolean := True;
   Default_Colorization : constant Colorize_Option := No_Colorize;
   Default_Indent_Nested_Paren : constant Boolean := True;
   Default_Reformat_Types      : constant Boolean := True;
   
   --------------------------------------------------------------------
   -- VARIABLES
   --------------------------------------------------------------------
   Current_Normal_Indent : Positive := Default_Normal_Indent;
   Current_Break_Indent  : Positive := Default_Break_Indent;
   Current_Break_Lines   : Positive := Default_Break_Lines;
   Current_Enum_Indent   : Positive := Default_Enum_Indent;
   Current_Record_Indent : Positive := Default_Record_Indent;
   Current_Field_Indent  : Positive := Default_Field_Indent;
   Current_Parm_Indent   : Positive := Default_Parm_Indent;
   Current_Paren_Indent  : Positive := Default_Paren_Indent;

   Current_Case_Style : Case_Mode := Default_Case_Style;

   Current_Identifier_Case : Text_Case := Default_Identifier_Case;
   Current_Keyword_Case    : Text_Case := Default_Keyword_Case;

   Current_Do_Vertical_Spacing : Boolean := Default_Do_Vertical_Spacing;
   Current_Colorization : Colorize_Option := Default_Colorization;
   Current_Indent_Nested_Paren : Boolean := Default_Indent_Nested_Paren;
   Current_Reformat_Types      : Boolean := Default_Reformat_Types;

   function Reformat_Types return Boolean is
   begin
      return Current_Reformat_Types;
   end Reformat_Types;

   procedure Set_Reformat_Types(To : Boolean) is
   begin
      Current_Reformat_Types := To;
   end Set_Reformat_Types;

   function Colorization return Colorize_Option is
   begin
      return Current_Colorization;
   end Colorization;
   
   procedure Set_Colorization(To : Colorize_Option) is
   begin
      Current_Colorization := To;
   end Set_Colorization;
   
   function Do_Vertical_Spacing return Boolean is
   begin
      return Current_Do_Vertical_Spacing;
   end Do_Vertical_Spacing;
   
   procedure Set_Do_Vertical_Spacing(To : Boolean) is
   begin
      Current_Do_Vertical_Spacing := To;
   end Set_Do_Vertical_Spacing;
   
   function Indent_Nested_Paren return Boolean is
   begin
      return Current_Indent_Nested_Paren;
   end Indent_Nested_Paren;
   
   procedure Set_Indent_Nested_Paren(To : Boolean) is
   begin
      Current_Indent_Nested_Paren := To;
   end Set_Indent_Nested_Paren;
   ------------------
   -- Break_Indent --
   ------------------

   function Break_Indent return Positive is
   begin
      return Current_Break_Indent;
   end Break_Indent;

   -----------------
   -- Break_Lines --
   -----------------

   function Break_Lines return Positive is
   begin
      return Current_Break_Lines;
   end Break_Lines;

   ----------------
   -- Case_Style --
   ----------------

   function Case_Style return Case_Mode is
   begin
      return Current_Case_Style;
   end Case_Style;

   -----------------
   -- Enum_Indent --
   -----------------

   function Enum_Indent return Positive is
   begin
      return Current_Enum_Indent;
   end Enum_Indent;

   ------------------
   -- Field_Indent --
   ------------------

   function Field_Indent return Positive is
   begin
      return Current_Field_Indent;
   end Field_Indent;

   ---------------------
   -- Identifier_Case --
   ---------------------

   function Identifier_Case return Text_Case is
   begin
      return Current_Identifier_Case;
   end Identifier_Case;

   ------------------
   -- Keyword_Case --
   ------------------

   function Keyword_Case return Text_Case is
   begin
      return Current_Keyword_Case;
   end Keyword_Case;

   -------------------
   -- Normal_Indent --
   -------------------

   function Normal_Indent return Positive is
   begin
      return Current_Normal_Indent;
   end Normal_Indent;

   ------------------
   -- Paren_Indent --
   ------------------

   function Paren_Indent return Positive is
   begin
      return Current_Paren_Indent;
   end Paren_Indent;

   -----------------
   -- Parm_Indent --
   -----------------

   function Parm_Indent return Positive is
   begin
      return Current_Parm_Indent;
   end Parm_Indent;

   -------------------
   -- Record_Indent --
   -------------------

   function Record_Indent return Positive is
   begin
      return Current_Record_Indent;
   end Record_Indent;

   ----------------------
   -- Set_Break_Indent --
   ----------------------

   procedure Set_Break_Indent (To : Positive) is
   begin
      Current_Break_Indent := To;
   end Set_Break_Indent;

   ---------------------
   -- Set_Break_Lines --
   ---------------------

   procedure Set_Break_Lines (To : Positive) is
   begin
      Current_Break_Lines := To;
   end Set_Break_Lines;

   --------------------
   -- Set_Case_Style --
   --------------------

   procedure Set_Case_Style (To : Case_Mode) is
   begin
      Current_Case_Style := To;
   end Set_Case_Style;

   ---------------------
   -- Set_Enum_Indent --
   ---------------------

   procedure Set_Enum_Indent (To : Positive) is
   begin
      Current_Enum_Indent := To;
   end Set_Enum_Indent;

   ----------------------
   -- Set_Field_Indent --
   ----------------------

   procedure Set_Field_Indent (To : Positive) is
   begin
      Current_Field_Indent := To;
   end Set_Field_Indent;

   -------------------------
   -- Set_Identifier_Case --
   -------------------------

   procedure Set_Identifier_Case (To : in Text_Case) is
   begin
      Current_Identifier_Case := To;
   end Set_Identifier_Case;

   ----------------------
   -- Set_Keyword_Case --
   ----------------------

   procedure Set_Keyword_Case (To : in Text_Case) is
   begin
      Current_Keyword_Case := To;
   end Set_Keyword_Case;

   -----------------------
   -- Set_Normal_Indent --
   -----------------------

   procedure Set_Normal_Indent (To : Positive) is
   begin
      Current_Normal_Indent:= To;
   end Set_Normal_Indent;

   ----------------------
   -- Set_Paren_Indent --
   ----------------------

   procedure Set_Paren_Indent (To : Positive) is
   begin
      Current_Paren_Indent := To;
   end Set_Paren_Indent;

   ---------------------
   -- Set_Parm_Indent --
   ---------------------

   procedure Set_Parm_Indent (To : Positive) is
   begin
      Current_Parm_Indent := To;
   end Set_Parm_Indent;

   -----------------------
   -- Set_Record_Indent --
   -----------------------

   procedure Set_Record_Indent (To : Positive) is
   begin
      Current_Record_Indent := To;
   end Set_Record_Indent;
   

   -----------------------
   -- Set_New_Line_Char --
   -----------------------

   New_Line_Character : Unbounded_String := To_Unbounded_String(
      Ascii.CR & Ascii.LF);
  
  function New_Line_Str return String is
  begin
    return To_String(New_Line_Character);
  end New_Line_Str;

  function New_Line_Char return Character is
     X : String := To_String(New_Line_Character);
  begin
    return X(X'First);
  end New_Line_Char;

  function New_Line_Char2 return Character is
     X : String := To_String(New_Line_Character);
  begin
     if X'First+1<=X'Last then
        return X(X'First+1);
     else
        return ASCII.NUL;
     end if;
  end New_Line_Char2;

  procedure Set_New_Line_Char (nlc : String) is
  begin
    New_Line_Character := To_Unbounded_String(nlc);
  end Set_New_Line_Char;

end Settings;
