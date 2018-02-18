-----------------------------------------------------------------------
-- token_writer.adb
--
-- Author: Robert A. French and Martin C. Carlisle
-- E-mail: carlislem@acm.org
--
-- Description:
-- This package provides various operations that write to a string that
-- is pointed to by a string pointer. Everything is accessed through a
-- Write procedure of some sort. These procedures also will 
-- ensure proper spacing around Ada95 tokens in >almost< all cases.
-----------------------------------------------------------------------
with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with ada.text_io;
with Lexer;
use type Lexer.StringPointer;
use type Lexer.TokenClass;
with Writer;
with Settings;
--with Windows;
-----------------------------------------------------------------------
-- WRITER
-----------------------------------------------------------------------
package body Token_Writer is

   --------------------------------------------------------------------
   -- TOKEN_VALUE
   -- Implementation Notes:
   --   - This function returns the string value of a given token.
   --------------------------------------------------------------------
   function Token_Value (
      Item : Lexer.Token;
      From : Lexer.StringPointer
     ) return String is
   begin
      Writer.Check_Index(From, Item.Startloc);
      Writer.Check_Index(From, Item.Endloc);
      return From.all(Item.Startloc .. Item.Endloc);
   end Token_Value;
   --------------------------------------------------------------------
   -- CHANGE_CASE
   -- Implementation Notes:
   --   - This function changes the case of the given string based on
   --     whether it is a reserved word or identifier, and what the
   --     current capitalization/case options are.
   --------------------------------------------------------------------
   function Change_Case (
      Item : Lexer.Token;
      From : Lexer.StringPointer
     ) return String is
     use type Settings.Text_Case;

      -----------------------------------------------------------------
      -- CAP_LETTER
      -- Implementation Notes:
      --   - This is an iterative function called to build a mixed case
      --     string to avoid a bunch of string pointer stuff.
      -----------------------------------------------------------------
      function Cap_Letter (
          Item      : String;
          Case_Type : Settings.Text_Case
         ) return String is
         Result  : String(Item'first..Item'last);
         First   : Boolean := True;
      begin
         for Index in Item'range loop
            if Item(Index) = '_' or Item(Index) = '.' then
               Result(Index) := Item(Index);
               First := True;
            elsif First then
               First := False;
               Result(Index) := To_Upper(Item(Index));
            elsif Case_Type = Settings.Mixed_Force_Lower then
               Result(Index) := To_Lower(Item(Index));
            else
               Result(Index) := Item(Index);
            end if;
         end loop;
         return Result;
      end Cap_Letter;
      -----------------------------------------------------------------

      Case_Type : Settings.Text_Case;

   begin  -- Change_Case
      -- First set the case type based on the token's class.
      if Lexer.GetTokenClass(Item) = Lexer.ReservedClass then
         Case_Type := Settings.Keyword_Case;
      elsif Lexer.GetTokenClass(Item) = Lexer.IdentifierClass then
         Case_Type := Settings.Identifier_Case;
      else
         Case_Type := Settings.Dont_Change;
      end if;

      -- Now format based on the option that was set.
      case Case_Type is
      when Settings.Upper_Case =>
         return To_Upper(Token_Value(Item, From));

      when Settings.Lower_Case =>
         return To_Lower(Token_Value(Item, From));

      when Settings.Mixed_Force_Lower |
           Settings.Mixed_No_Force_Lower =>
         return Cap_Letter(Token_Value(Item, From),
            Case_Type);

      when Settings.Dont_Change =>
         return Token_Value(Item, From);
      end case;
   end Change_Case;
   --------------------------------------------------------------------
   -- WRITE
   --------------------------------------------------------------------
   procedure Write (
         Item  : in     Lexer.Tokens;
         Into  : in out Lexer.StringPointer;
         Index : in out Positive
       ) is

      The_Colon    : constant Lexer.Token := (Lexer.Colon_T, 1, 1, 1, 1);
      The_Assign   : constant Lexer.Token := (Lexer.Assignment_T, 1, 2, 2, 3);
      The_Semi     : constant Lexer.Token := (Lexer.Semicolon_T, 1, 4, 4, 4);
      The_Comma    : constant Lexer.Token := (Lexer.Comma_T, 1, 5, 5, 5);
      The_Constant : constant Lexer.Token := (Lexer.Constant_T, 1, 6, 6, 13);
      The_In       : constant Lexer.Token := (Lexer.In_T, 1, 14, 14, 15);
      The_Out      : constant Lexer.Token := (Lexer.Out_T, 1, 16, 16, 18);
      The_Arrow    : constant Lexer.Token := (Lexer.Arrow_T, 1, 19, 19, 20);
      The_Access   : constant Lexer.Token := (Lexer.Access_T, 1, 21, 21, 26);
      The_String   : Lexer.StringPointer  := new String'("::=;,constantinout=>access");

   begin  -- Write
      case Item is
         when Lexer.Colon_T =>
            Write(The_Colon, The_String, Into, Index);
         when Lexer.Assignment_T =>
            Write(The_Assign, The_String, Into, Index);
         when Lexer.Semicolon_T =>
            Write(The_Semi, The_String, Into, Index);
         when Lexer.Comma_T =>
            Write(The_Comma, The_String, Into, Index);
         when Lexer.Constant_T =>
            Write(The_Constant, The_String, Into, Index);
         when Lexer.In_T =>
            Write(The_In, The_String, Into, Index);
         when Lexer.Out_T =>
            Write(The_Out, The_String, Into, Index);
         when Lexer.Arrow_T =>
            Write(The_Arrow, The_String, Into, Index);
         when Lexer.Access_T =>
            Write(The_Access, The_String, Into, Index);
         when others =>
            null;
      end case;
   end Write;
   --------------------------------------------------------------------
   -- WRITE
   --------------------------------------------------------------------
   procedure Write (
      Item  : in     Lexer.Token;
      From  : in     Lexer.StringPointer;
      Into  : in out Lexer.StringPointer;
      Index : in out Positive;
      Pad   : in     Natural := 1;
      Chomp : in     Boolean := True
     ) is
      use Lexer;
   begin
      case Lexer.GetTokenType(Item) is
         -- These delimiters should have spaces before and after. This
         -- relies on the previous spaces being provided during writing
         -- of the previous token (this is common to all tokens).
         when Plus_T | Minus_T | Times_T | Divide_T | Ampersand_T |
              Lt_T | Eq_T | Gt_T | NotEq_T | LEq_T | GEq_T | Assignment_T |
              Arrow_T | Pipe_T | Double_Dot_T | Colon_T | Box_T =>
            Writer.Write(Token_Value(Item, From) & Pad * ' ', Into, Index);
         when Mod_T | Rem_T =>
            -- it is essential that mod and rem be preceded by a space
            -- even if the padding says no.
            if Index-1>= Into.all'First and then Into.all(Index-1) /= ' ' then
               Writer.Write(" " & Token_Value(Item, From) & Pad * ' ', Into, Index);
            else
               Writer.Write(Token_Value(Item, From) & Pad * ' ', Into, Index);
            end if;
         -- Character and string literals have spaces before and after.
         when String_T | Char_Literal_T =>
            Writer.Write(Token_Value(Item, From) & Pad * ' ', Into, Index);

         -- Parentheses have spaces outside, but not inside. For left
         -- parentheses, write no spaces afterward.
         when LParen_T =>
            Writer.Write(Token_Value(Item, From), Into, Index);

         -- For right parentheses, "chomp" any previous spaces, then
         -- write the parenthesis with trailing spaces.
         when RParen_T =>
            if Chomp then
               Writer.Chomp_Spaces(Into, Index);
            end if;
            Writer.Write(Token_Value(Item, From) & Pad * ' ', Into, Index);

         -- Semicolons and commas have spaces after, but not before.
         when Semicolon_T | Comma_T =>
            if Chomp then
               Writer.Chomp_Spaces(Into, Index);
            end if;
            Writer.Write(Token_Value(Item, From) & Pad * ' ', Into, Index);

         -- Dots, exponent ops, and "tick" have no surrounding spaces.
         when Double_Star_T | Tick_T | Dot_T =>
            if Chomp then
               Writer.Chomp_Spaces(Into, Index);
            end if;
            Writer.Write(Token_Value(Item, From), Into, Index);

         -- Comments don't get any special spaces, get rid of extraneous at end
         when Comment_T =>
            Writer.Write(Token_Value(Item, From), Into, Index);
            Writer.Chomp_Spaces(Into, Index);
            
         -- Everything else, just put spaces after.
         -- (Identifiers, reserved words, constants)
         when others =>
            Writer.Write(Change_Case(Item, From) & Pad * ' ', Into, Index);
      end case;
   end Write;
end Token_Writer;
-----------------------------------------------------------------------       
