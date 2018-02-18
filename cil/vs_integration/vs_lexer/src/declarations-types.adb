-----------------------------------------------------------------------
-- declarations-types.adb
--
-- Author: Robert A. French
-- E-mail: rfrench99@hotmail.com
--
-- Description:
-- This is used to format type and subtype declarations.
-----------------------------------------------------------------------
-- Change log:
-- 02/29/04 (alf) : added linebreak after comments
-- 07/21/99 (mcc) : worked on null records
-----------------------------------------------------------------------
with Lexer;
use type Lexer.Tokens;

with Decls;
with Settings;
with Writer;
with Token_Writer;
-- with ada.text_io;
-----------------------------------------------------------------------
-- DECLARATIONS.TYPES
-----------------------------------------------------------------------
package body Declarations.Types is
   --------------------------------------------------------------------
   -- REFORMAT
   --------------------------------------------------------------------
   procedure Reformat (
         From   : in     Lexer.StringPointer;
         First  : in out Positive;
         Into   : in out Lexer.StringPointer;
         Index  : in out Positive;
         Indent : in     Natural := 0
       ) is

      The_Token : Lexer.Token;
      Last      : Positive;
      Have      : Boolean := True;
      Done      : Boolean := False;
      --| These variables are all used while retrieving a token. Most
      --| aren't really important to this package.

      Had_Array : Boolean := False;
      Had_Is    : Boolean := False;
      Had_Null  : Boolean := False;
      --| This is used to suppress a newline before a left paren if it
      --| is part of an array definition.
      Remember_Line : Natural;
   begin  -- Reformat
      -- Read/write the starting reserved word first (ie, "type").
      Remember_Line := Lexer.Getcurrentline;
      Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
      if Have then
         if Lexer.GetTokenType(The_Token) = Lexer.Type_T or
            Lexer.GetTokenType(The_Token) = Lexer.Subtype_T then
            Writer.Indent(Indent, Into, Index);
            Token_Writer.Write(The_Token, From, Into, Index);
            First := Last + 1;
         else
            Lexer.Initialize(Remember_Line);
            return;
         end if;
      else
         return;
      end if;

      -- This loop writes everything it receives until it gets to one
      -- of the following: left paren, "record", "array", semicolon.
      -- Each of these will result it some form of special processing.
      while Have loop
         Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
         if Have then
            case Lexer.GetTokenType(The_Token) is
               when Lexer.LParen_T =>
                  Had_Null := False;
                  -- Go to the new line, indent, then write left paren.
                  First := Last + 1;
                  if not Had_Array then
                     Writer.New_Line(Into, Index);
                     Writer.Indent(Indent + Settings.Enum_Indent, Into, Index);
                     Token_Writer.Write(The_Token, From, Into, Index);

                     if not Had_is then
                        Decls.Setup(Decls.Parameters);
                     else
                        -- Probably enumeration, so try formatting as
                        -- arguments, they're similar.
                        Decls.Setup(Decls.Arguments);
                     end if;
                     Decls.Suppress_Indent;
                     Decls.Reformat(From, First, Into, Index, Indent + Settings.Enum_Indent + 1);
                  else
                     Token_Writer.Write(The_Token, From, Into, Index);
                  end if;

               when Lexer.Record_T =>
                  if Had_Null then
                     Token_Writer.Write(The_Token, From, Into, Index);
                     First := Last + 1;
                  else
                     Had_Null := False;
                     -- Goes to the new line, indents and writes, then proceeds
                     -- to process record component declarations until it reaches
                     -- the end.
                     Writer.New_Line(Into, Index);
                     Writer.Indent(Indent + Settings.Record_Indent, 
                        Into, Index);
                     Token_Writer.Write(The_Token, From, Into, Index);
                     Writer.New_Line(Into, Index);
                     First := Last + 1;

                     -- Now process possible record components.
                     Decls.Setup(Decls.General);
                     Decls.Reformat(From, First, Into, Index, 
                        Indent + Settings.Field_Indent);

                     -- Now check for a possible "end".
                     Remember_Line := Lexer.Getcurrentline;
                     Lexer.GetNextToken(From, First, Last, The_Token, 
                        Have, Done);
                     if Have then
                        if Lexer.GetTokenType(The_Token) = Lexer.End_T then
                           Writer.Indent(Indent + Settings.Record_Indent,
                              Into, Index);
                           Token_Writer.Write(The_Token, From, Into, Index);
                           First := Last + 1;
                        else
                           Lexer.Initialize(Remember_Line);
                        end if;
                     end if;
   
                     -- Now check for a possible "record".
                     Remember_Line := Lexer.Getcurrentline;
                     Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
                     if Have then
                        if Lexer.GetTokenType(The_Token) = Lexer.Record_T then
                           Token_Writer.Write(The_Token, From, Into, Index);
                           First := Last + 1;
                        else
                           Lexer.Initialize(Remember_Line);
                        end if;
                     end if;
                  end if;
               when Lexer.Array_T =>
                  Had_Null := False;
                  First := Last + 1;
                  -- Writes it, and suppress newline for following paren.
                  Token_Writer.Write(The_Token, From, Into, Index);
                  Had_Array := True;

               when 
                  Lexer.Procedure_T |
                  Lexer.Function_T |
                  Lexer.Begin_T |
                  Lexer.Type_T |
                  -- before this is a list of unexpected things
                  -- that should make us just stop
                  Lexer.Semicolon_T =>
                  -- Write it, go to the next line, and exit the 
                  -- procedure.
             
                  Had_Null := False;
                  Token_Writer.Write(The_Token, From, Into, Index);
                  Writer.New_Line(Into, Index);
                  First := Last + 1;
                  return;
               when Lexer.Is_T =>
                  Had_Null := False;
                  Had_Is := True;
                  Token_Writer.Write(The_Token, From, Into, Index);
                  First := Last + 1;
               when Lexer.Null_T =>
                  Had_Null := True;
                  Token_Writer.Write(The_Token, From, Into, Index);
                  First := Last + 1;
               when Lexer.Comment_T =>
                  -- Write it, go to the next line, and exit the 
                  -- procedure if in the middle of an array
                  if Had_Array then
                     Had_Null := False;
                     Token_Writer.Write(The_Token, From, Into, Index);
                     Writer.New_Line(Into, Index);
                     First := Last + 1;
                     return;
                  -- do others case
                  else
                     Had_Null := False;
                     Token_Writer.Write(The_Token, From, Into, Index);
                     Writer.New_Line(Into, Index);
                     Writer.Indent(Indent, Into, Index);
                     First := Last + 1;
                  end if;
               when others =>
                  Had_Null := False;
                  Token_Writer.Write(The_Token, From, Into, Index);
                  First := Last + 1;
            end case;
         end if;
      end loop;
   end Reformat;
   --------------------------------------------------------------------
end Declarations.Types;
-----------------------------------------------------------------------         
