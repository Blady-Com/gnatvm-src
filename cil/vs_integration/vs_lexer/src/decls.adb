-----------------------------------------------------------------------
-- decls.adb
--
-- Author: Robert A. French
-- E-mail: rfrench99@hotmail.com
--
-- Description:
-- This is primarily used within type definitions to provide formatting
-- of type components possibly interspersed with comments. The same
-- applies for parameter and argument lists.
-----------------------------------------------------------------------
with Declarations.Arguments;
with Declarations.Parameters;
with Declarations.Variables;

with Writer;
with Token_Writer;
with Lexer;
use type Lexer.Tokens;

-----------------------------------------------------------------------
-- DECLS
-----------------------------------------------------------------------
package body Decls is
   --------------------------------------------------------------------
   -- VARIABLES
   --------------------------------------------------------------------
   Current_Mode : Decl_Mode := General;
   Suppressing  : Boolean   := False;
   --------------------------------------------------------------------
   -- SETUP
   --------------------------------------------------------------------
   procedure Setup (Mode : in     Decl_Mode) is
   begin
      Current_Mode := Mode;
   end Setup;
   --------------------------------------------------------------------
   -- SUPPRESS_INDENT
   --------------------------------------------------------------------
   procedure Suppress_Indent is
   begin
      Suppressing := True;
   end Suppress_Indent;
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
      Have      : Boolean  := True;
      Done      : Boolean  := False;
      Kind      : Lexer.Tokens;
      --| These variables are all used while retrieving a token. Most
      --| aren't really important to this method.

      Mode     : Decl_Mode := Current_Mode;
      --| These ensure that the values persist for the procedure call.

      Remember_Line : Natural;
      Last_Was_Comment : Boolean := False;

   begin  -- Reformat
      -- Attempt to format based on the first token reached each time
      -- through the loop.
      loop
         Remember_Line := Lexer.Getcurrentline;
         Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
         Kind := Lexer.GetTokenType(The_Token);
         if not Have or Done then
            Suppressing := False;
            return;
         end if;

         case Kind is

            when Lexer.Name_T =>
               -- These are declarations, so it must be a declaration
               -- list of some sort.
               Lexer.Initialize(Remember_Line);
               if Mode = General then
                  Declarations.Variables.Reformat(From, First, Into, Index, Indent);
                  Remember_Line := Lexer.Getcurrentline;
                  Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
                  Kind := Lexer.GetTokenType(The_Token);
                  -- we quit b/c of blank line
                  if Kind = Lexer.Name_T then
                     Writer.New_Line(Into,Index);
                  end if;
                  Lexer.Initialize(Remember_Line);
               elsif Mode = Parameters then
                  if Suppressing then
                     Declarations.Parameters.Suppress_Indent;
                     Suppressing := False;
                  end if;
                  Declarations.Parameters.Reformat(From, First, Into, Index, Indent);
               elsif Mode = Arguments then
                  if Last_Was_Comment then
                     Declarations.Arguments.Continuation_List;
                     Writer.Chomp_Line(Into,Index);
                  end if;
                  if Suppressing then
                     Declarations.Arguments.Suppress_Indent;
                     Suppressing := False;
                  end if;
                  Declarations.Arguments.Reformat(From, First, Into, Index, Indent);
               end if;
               Last_Was_Comment := False;

            when Lexer.Comment_T =>
               -- Comments will always have a newline afterwards.
               -- not sure why 3 works here, but I seem to have an
               -- off by 2 error
               for i in Remember_Line+3..Lexer.GetCurrentLine loop
                  Writer.New_Line(Into,Index);
               end loop;
               if not Suppressing then
                  Writer.Indent(Indent, Into, Index);
               end if;
               Token_Writer.Write(The_Token, From, Into, Index);
               Writer.New_Line(Into, Index);
               First := Last + 1;
               Last_Was_Comment := True;
            when others =>
               Last_Was_Comment := False;
               -- Let someone else handle it :-)
               Lexer.Initialize(Remember_Line);
               exit;
         end case;

         -- Reset first-line newline suppression.
         Suppressing := False;

      end loop;
      -- Reset first-line newline suppression.
      Suppressing := False;
   end Reformat;
   --------------------------------------------------------------------
end Decls;
-----------------------------------------------------------------------
