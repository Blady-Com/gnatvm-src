-----------------------------------------------------------------------
-- declarations-subprograms.adb
--
-- Author: Robert A. French
-- E-mail: rfrench99@hotmail.com
--
-- Description:
-- This is used to format any type of subprogram, ie, packages,
-- procedures, functions, tasks.
-----------------------------------------------------------------------
with Lexer;
use type Lexer.Tokens;

with ada.text_io; -- for debugging

with Decls;
with Settings;
with Writer;
with Token_Writer;
-----------------------------------------------------------------------
-- DECLARATIONS.SUBPROGRAMS
-----------------------------------------------------------------------
package body Declarations.Subprograms is
   --------------------------------------------------------------------
   -- REFORMAT
   --------------------------------------------------------------------
   procedure Reformat (
         From   : in     Lexer.StringPointer;
         First  : in out Positive;
         Into   : in out Lexer.StringPointer;
         Index  : in out Positive;
         Indent : in out Natural;
         Withed : in     Boolean
       ) is

      The_Token : Lexer.Token;
      Last      : Positive;
      Have      : Boolean := True;
      Done      : Boolean := False;
      --| These variables are all used while retrieving a token. Most
      --| aren't really important to this package.

      Had_Paren : Boolean := False;
      --| This is used to suppress a newline before "return" if it
      --| follows a right parenthesis.

      Comment_Str : Lexer.StringPointer := new String'(" ");
      Comment_End : Positive            := 1;
      --| There are used to handle the possibility of embedded
      --| comments. As comments are encountered while searching for
      --| other things, they will be placed in this string, each
      --| followed by a newline.

      Top_Token : Lexer.Tokens;
      --| This is used to remember the type of the original construct 
      --| in order to push it onto the stack.
      Remember_Line : Natural;
   begin  -- Reformat
      -- Read/write the starting reserved word first (eg, "function").
      Remember_Line := Lexer.Getcurrentline;
      Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
      if Have then
         if Lexer.GetTokenType(The_Token) = Lexer.Function_T  or
            Lexer.GetTokenType(The_Token) = Lexer.Entry_T   or
            Lexer.GetTokenType(The_Token) = Lexer.Accept_T or
            Lexer.GetTokenType(The_Token) = Lexer.Procedure_T      then
            First := Last + 1;
            Writer.Chomp_Spaces(Into,Index);
            if not Withed then
               Writer.Indent(Indent, Into, Index);
            else
               Writer.Indent(1, Into, Index);
            end if;
            Token_Writer.Write(The_Token, From, Into, Index);
            Top_Token := Lexer.GetTokenType(The_Token);
         else
            Lexer.Initialize(Remember_Line);
            return;
         end if;
      else
         return;
      end if;

      -- This loop writes everything it receives until it gets to one
      -- of the following: left paren, "return", "is", semicolon.
      -- Each of these will result in some form of special processing.
      loop
         Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
         if not Have or Done then
            Writer.New_Line(Into, Index);
            if Comment_End > 1 then
               -- Write stored comments.
               Writer.Finalize(Comment_Str, Comment_End);
               Writer.Write(Comment_Str.all, Into, Index);
            end if;
            return;
         end if;

         case Lexer.GetTokenType(The_Token) is

            when Lexer.LParen_T =>
               -- Write the left paren, then go to the next line.
               Token_Writer.Write(The_Token, From, Into, Index);
               Writer.New_Line(Into, Index);
               First := Last + 1;

               -- Now format possible parameter declarations.
               Decls.Setup(Decls.Parameters);
               Decls.Reformat(From, First, Into, Index, Indent + Settings.Parm_Indent);

               -- Next look for a right parenthesis. If it's there, go
               -- to the next line, indent, and write. Then go ahead
               -- with the loop.
               -- *** NOTE: Had to kludge in the right paren here, ie,
               -- use a literal, to avoid conflict with my Writer
               -- package (the space chomper...).
               Remember_Line := Lexer.Getcurrentline;
               Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
               if Have then
                  if Lexer.GetTokenType(The_Token) = Lexer.RParen_T then
--                     Writer.New_Line(Into, Index);
--                     Writer.Indent(Indent + Settings.Paren_Indent, Into, Index);
                     --Writer.Write(The_Token, From, Into, Index);
                     Writer.Chomp_Spaces (Into, Index);
                     Writer.Write(") ", Into, Index);
                     First := Last + 1;
                     Had_Paren := True;
                  else  -- not RParen_T
                     Lexer.Initialize(Remember_Line);
                  end if;
               end if;

            when Lexer.Return_T =>
               First := Last + 1;
               -- Go to newline and indent if this did not follow a
               -- right parenthesis immediately.
               if Had_Paren then
                  Writer.New_Line(Into, Index);
                  Writer.Indent(Indent + Settings.Break_Indent, Into, Index);
                  Had_Paren := False;
               end if;
               -- Write it out.
               Token_Writer.Write(The_Token, From, Into, Index);

            when Lexer.Is_T|Lexer.Do_T =>
               First := Last + 1;
               Remember_Line := Lexer.Getcurrentline;
               declare
                  Next_Token : Lexer.Token;
               begin
                  Lexer.GetNextToken(From, First, Last, Next_Token,
                     Have, Done);
                  if (Have and not Done) and then
                     Lexer.Gettokentype(Next_Token) /= Lexer.box_t then
                     -- Write it and go to the next line.
                     Token_Writer.Write(The_Token, From, Into, Index);
                     Writer.New_Line(Into, Index);
                     -- Write out the comments.
                     if Comment_End > 1 then
                       Writer.Finalize(Comment_Str, Comment_End);
                       Writer.Write(Comment_Str.all, Into, Index);
                     end if;
                     Lexer.Initialize(Remember_Line);
                     return;
                  else
                     Token_Writer.Write(The_Token, From, Into, Index);
                     Token_Writer.Write(Next_Token, From, Into, Index);
                     First := Last + 1;
                  end if;
               end;


            when Lexer.Semicolon_T =>
               First := Last + 1;
               -- Write it and go to the next line.
               Token_Writer.Write(The_Token, From, Into, Index);
               Writer.New_Line(Into, Index);
               -- Write out the comments.
               if Comment_End > 1 then
                  Writer.Finalize(Comment_Str, Comment_End);
                  Writer.Write(Comment_Str.all, Into, Index);
               end if;
               return;

            when Lexer.Comment_T =>
               First := Last + 1;
               -- Indent next line of the string.
               Writer.Indent(Indent + Settings.Normal_Indent,
                            Comment_Str, Comment_End);
               -- Write the comment out followed by newline.
               Token_Writer.Write(The_Token, From, Comment_Str, Comment_End);
               Writer.New_Line(Comment_Str, Comment_End);

            when others =>
               -- Just write them using their default style.
               Token_Writer.Write(The_Token, From, Into, Index);
               First := Last + 1;
         end case;
      end loop;
   end Reformat;
   --------------------------------------------------------------------
end Declarations.Subprograms;
----------------------------------------------------------------------- 
