-----------------------------------------------------------------------
-- declarations.adb
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
with Lexer;
use type Lexer.StringPointer;
use type Lexer.Tokens;

with Writer;
with Token_Writer;
with Ada.Characters.Handling;
use Ada.Characters.Handling;
-----------------------------------------------------------------------
-- DECLARATIONS
-----------------------------------------------------------------------
package body Declarations is
   --------------------------------------------------------------------
   -- INITIALIZE
   --------------------------------------------------------------------
   procedure Initialize (Object : in out Declaration) is
   begin
      Ada.Finalization.Initialize(Ada.Finalization.Controlled(Object));
      Object.Name_End    := 1;
      Object.Comment_End := 1;
      Object.Output_End  := 1;
      Object.Has_Comment := False;
      Object.Name_Str    := new String'(" ");
      Object.Comment_Str := new String'(" ");
      Object.Output_Str  := new String'(" ");
   end Initialize;
   --------------------------------------------------------------------
   -- ADJUST
   --------------------------------------------------------------------
   procedure Adjust (Object : in out Declaration) is
      Temp : Lexer.StringPointer;
   begin
      Ada.Finalization.Adjust(Ada.Finalization.Controlled(Object));
      if Object.Name_Str /= null then
         Temp            := Object.Name_Str;
         Object.Name_Str := new String'(Temp.all);
      end if;
      if Object.Comment_Str /= null then
         Temp               := Object.Comment_Str;
         Object.Comment_Str := new String'(Temp.all);
      end if;
      if Object.Output_Str /= null then
         Temp              := Object.Output_Str;
         Object.Output_Str := new String'(Temp.all);
      end if;
   end Adjust;
   --------------------------------------------------------------------
   -- FINALIZE
   --------------------------------------------------------------------
   procedure Finalize (Object : in out Declaration) is
   begin
      if Object.Name_Str /= null then
         Free(Object.Name_Str);
      end if;
      if Object.Comment_Str /= null then
         Free(Object.Comment_Str);
      end if;
      if Object.Output_Str /= null then
         Free(Object.Output_Str);
      end if;
      Ada.Finalization.Finalize(Ada.Finalization.Controlled(Object));
   end Finalize;
   --------------------------------------------------------------------
   -- WRITE
   -- The output string is finalized before writing, meaning that the
   -- only memory allocated to it is the memory actually being used, so
   -- it's a little simpler on the output routine (no slicing needed).
   --------------------------------------------------------------------   
   procedure Write (
         Item   : in out Declaration;
         Into   : in out Lexer.StringPointer;
         Index  : in out Positive;
         Indent : in     Natural := 0
       ) is
   begin
      Writer.Indent(Indent, Into, Index);
      Writer.Finalize(Item.Output_Str, Item.Output_End);
      Writer.Write(Item.Output_Str.all, Into, Index);
   end Write;
   --------------------------------------------------------------------
   -- RESET
   -- This was simplified by making all initialization occur in the 
   -- Initialize method versus in the type declaration; it's easier to
   -- reset by simply finalizing and then re-initializing.
   --------------------------------------------------------------------
   procedure Reset (Item : in out Declaration'Class) is
   begin
      Finalize(Item);
      Initialize(Item);
   end Reset;
   --------------------------------------------------------------------
   -- GET_COMMENTS
   --------------------------------------------------------------------
   procedure Get_Comments (
         Line  : in     Integer;
         From  : in     Lexer.StringPointer;
         First : in out Positive;
         Into  : in out Lexer.StringPointer;
         Index : in out Positive
       ) is

      The_Token : Lexer.Token;
      Last      : Positive;
      Have      : Boolean  := True;
      Done      : Boolean  := False;
      --| These variables are all used while retrieving a token. Most
      --| aren't really important to this method.
      Remember_Line : Natural;
   begin  -- Get_Comments
      -- Only get comments, and only if they're on the same line as
      -- specified.
      Remember_Line := Lexer.GetCurrentLine;
      loop
         Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
         if not Have or Done then
            return;
         elsif Lexer.GetTokenType(The_Token) = Lexer.Comment_T and
               Lexer.GetLine(The_Token)      = Line then
            Token_Writer.Write(The_Token, From, Into, Index);
            First := Last;
            while Is_Control(From(First)) or From(First)=' ' loop
               First := First - 1;
            end loop;
            First := First + 1;
            return;
         else
            Lexer.Initialize(Remember_Line);
            return;
         end if;
      end loop;
   end Get_Comments;
   --------------------------------------------------------------------
end Declarations;
-----------------------------------------------------------------------         
