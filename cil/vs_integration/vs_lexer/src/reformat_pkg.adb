------------------------------------------------------------------------------
--                                                                          --
--                      GNAT VISUAL STUDIO INTEGRATION                      --
--                                                                          --
--                          DECLARATIONS.ARGUMENTS                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2007, AdaCore                        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by --
-- AdaCore - http://www.adacore.com                                         --
--                                                                          --
-- This work is based on A# Visual Studio integration by  Prof. Martin C.   --
-- Carlisle of the United States Air Force Academy.                         --
--                                                                          --
------------------------------------------------------------------------------
---------------------------------------------------------------
--  REFORMAT_PKG.ADB
--
--  By: Tim Chamillard and Martin Carlisle
--
--  Description : This package provides a number of
--                functions to reformat Ada code to
--                use the proper capitalization and
--                indentation.  The Ada code is provided
--                as a pointer to a string, which is
--                reformatted and returned as the same
--                type (terminated by Character'First if RTF)
--
---------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;

with Lexer;
with Writer;
with Token_Writer;
with Settings;
use type Settings.Colorize_Option;
use type Settings.Text_Case;

--  with and use of child package
with Reformat_Pkg.Scope_Changes;
use  Reformat_Pkg.Scope_Changes;

--  uses to get "=" on TokenClass, Tokens
use type Lexer.Tokenclass;
use type Lexer.Tokens;

with Token_Writer;
with Token_Stack;

with Decls;
with Reformat_Rtf;
with Declarations.Arguments;
with Declarations.Subprograms;
with Declarations.Types;

with Ada.Characters.Latin_1;

package body Reformat_Pkg is

   --  constants for starting size of result
   Initial_Size       : constant Positive := 50000;

   ---------------------------------------------------------------------
   --
   --  Procedure: Free
   --  Description: This procedure deallocates the space for a
   --     Reformat_String
   --  Parameters:
   --     The_String : the string to deallocate
   ---------------------------------------------------------------------

   procedure Free (The_String : in out Reformat_String) is

      --  instantiation to free string memory space
      procedure Free_String is new Ada.Unchecked_Deallocation
        (String, Reformat_String);

   begin   --  Free

      Free_String (The_String);

   end Free;

   ---------------------------------------------------------------------
   --  function : Is_Record_With_Case
   --  determines if this is a record with a case inside of it
   --  If so, we won't be able to reformat the type with the
   --  advanced reformatting.
   --  Parameters:
   --    The_String           : what's being reformatted
   --    Current_Location     : where we are
   ---------------------------------------------------------------------
   function Is_Record_With_Case
     (The_String       : in  Reformat_String;
      Current_Location : in  Integer)
      return Boolean
   is
      Lookahead_Token  : Lexer.Token;
      Startlocation    : Integer       := Current_Location;
      Endlocation      : Integer;
      Current_Line     : Natural;
      Got_One          : Boolean;
      Next_Done        : Boolean;
      Token_Type       : Lexer.Tokens;
   begin

      Current_Line := Lexer.Getcurrentline;
      --  first find either "record" or semicolon
      loop
         Lexer.Getnexttoken
           (Str     => The_String,
            First   => Startlocation,
            Last    => Endlocation,
            Result  => Lookahead_Token,
            Haveone => Got_One,
            Done    => Next_Done);

         if not Got_One then
            Lexer.Initialize (Line => Current_Line);
            return False;
         end if;

         Token_Type := Lexer.Gettokentype (Item => Lookahead_Token);

         exit when Token_Type = Lexer.Record_T or
           Token_Type = Lexer.Semicolon_T;

         Startlocation := Endlocation + 1;
      end loop;

      if Token_Type = Lexer.Semicolon_T then
         Lexer.Initialize (Line => Current_Line);
         return False;
      end if;

      --  found "record", now find either "end" or "case"
      loop
         Lexer.Getnexttoken
           (Str     => The_String,
            First   => Startlocation,
            Last    => Endlocation,
            Result  => Lookahead_Token,
            Haveone => Got_One,
            Done    => Next_Done);

         if not Got_One then
            Lexer.Initialize (Line => Current_Line);

            return False;
         end if;

         Token_Type := Lexer.Gettokentype (Item => Lookahead_Token);

         exit when Token_Type = Lexer.End_T or
           Token_Type = Lexer.Case_T;

         Startlocation := Endlocation + 1;
      end loop;

      --  set current line in lexer to the line it was
      --  before the lookahead
      Lexer.Initialize (Line => Current_Line);

      if Token_Type = Lexer.Case_T then
         return True;
      else
         return False;
      end if;
   end Is_Record_With_Case;

   ---------------------------------------------------------------------
   --
   --  Procedure : Lookahead_Past_Comments
   --  Description : Get the next non-commented token
   --               Resets current line after looking ahead
   --  Parameters:
   --    Lookahead_Token      : next non-commented token
   --    Got_One              : True on success
   --    The_String           : what's being reformatted
   --    Current_Location     : where we are
   --
   ---------------------------------------------------------------------

   procedure Lookahead_Past_Comments
     (Lookahead_Token  :    out Lexer.Token;
      Got_One          :    out Boolean;
      The_String       : in     Reformat_String;
      Current_Location : in     Integer)
   is
      Startlocation : Integer := Current_Location;
      Endlocation   : Integer;
      Current_Line  : Natural;
      Next_Done     : Boolean;
   begin

      Current_Line := Lexer.Getcurrentline;
      loop
         Lexer.Getnexttoken
           (Str     => The_String,
            First   => Startlocation,
            Last    => Endlocation,
            Result  => Lookahead_Token,
            Haveone => Got_One,
            Done    => Next_Done);

         exit when not Got_One or else
         Lexer.Gettokentype (Item => Lookahead_Token) /= Lexer.Comment_T;

         Startlocation := Endlocation + 1;
      end loop;

      --  set current line in lexer to the line it was
      --  before the lookahead
      Lexer.Initialize (Line => Current_Line);
   end Lookahead_Past_Comments;

   --  check to see if there is an arrow inside parens before
   --  closing paren
   function No_Nested_Call
     (The_String       : in Reformat_String;
      Current_Location : in Integer) return Boolean
   is
      Lookahead_Token : Lexer.Token;
      Got_One         : Boolean;
      Startlocation   : Integer := Current_Location;
      Endlocation     : Integer;
      Current_Line    : Natural;
      Next_Done       : Boolean;
      Paren_Count     : Integer := 1;
      --  we have already seen an rparen and arrow to get here
      Result          : Boolean := True;
   begin

      Current_Line := Lexer.Getcurrentline;

      loop
         Lexer.Getnexttoken
           (Str     => The_String,
            First   => Startlocation,
            Last    => Endlocation,
            Result  => Lookahead_Token,
            Haveone => Got_One,
            Done    => Next_Done);

         if Lexer.GetTokenType (Item => Lookahead_Token) = Lexer.Lparen_T then
            Paren_Count := Paren_Count + 1;

         elsif Lexer.GetTokenType (Item => Lookahead_Token) =
           Lexer.Comma_T
         then
            if Paren_Count > 1 then
               Result := false;
               exit;
            end if;

         elsif Lexer.GetTokenType (Item => Lookahead_Token) =
           Lexer.Arrow_T
         then
            if Paren_Count > 1 then
               Result := false;
               exit;
            end if;

         elsif Lexer.GetTokenType (Item => Lookahead_Token) =
           Lexer.Semicolon_T then
            Result := false;
            exit;

         elsif Lexer.GetTokenType (Item => Lookahead_Token) = Lexer.End_T then
            Result := false;
            exit;
         elsif Lexer.GetTokenType (Item => Lookahead_Token) =
           Lexer.Rparen_T
         then
            Paren_Count := Paren_Count - 1;
         end if;

         exit when not Got_One or else
           (Lexer.Gettokentype (Item => Lookahead_Token) = Lexer.Rparen_T
            and Paren_Count <= 0);

         Startlocation := Endlocation + 1;
      end loop;

      --  set current line in lexer to the line it was
      --  before the lookahead
      Lexer.Initialize (Line => Current_Line);
      return Result;
   end No_Nested_Call;

   ------------------------------------------------------------------
   --
   --  Procedure: Initialize
   --  Description: Provides initial values for flags
   --  Parameters:
   --    Current_Line        : line in input string
   --    Last_Char           : last character location in new string
   --    Start_For_Next_Token         : start location of token
   --    Last_Token_End      : end location of previous token
   --    Previous_Token_Type : type of the previous token
   ------------------------------------------------------------------

   procedure Initialize
     (Current_Line          : out Natural;
      Last_Char             : out Natural;
      Start_For_Next_Token  : out Natural;
      Last_Token_End        : out Natural;
      Parenthesis_Depth     : out Natural;
      Stack_Token_Type      : out Lexer.Tokens;
      Need_To_Check_Is      : out Boolean;
      Previous_Token_Type   : out Lexer.Tokens;
      Continuation_Line     : out Boolean;
      Bad_Token_Starts_Line : out Boolean) is

   begin   --  Initialize

      --  Set current line to 0
      Current_Line := 0;

      --  Set last character to 1
      Last_Char := 1;

      --  Set Start_For_Next_Token to 1
      Start_For_Next_Token := 1;

      --  Set Last_Token_End to 0
      Last_Token_End := 0;

      --  Set parenthesis depth to 0
      Parenthesis_Depth := 0;

      --  Set stack token to null_t
      Stack_Token_Type := Lexer.Null_T;

      --  Set previous token to null
      Previous_Token_Type := Lexer.Null_T;

      --  Set need to check is to false
      Need_To_Check_Is := False;

      --  Set need semicolon to false
      Continuation_Line := False;

      --  Set bad token at start of line to false
      Bad_Token_Starts_Line := False;

      --  empty the stack
      Token_Stack.Empty_Stack;
   end Initialize;

   ---------------------------------------------------------------------
   --
   --  Function: Check_Continuation_Line
   --  Description: This function checks to see is the Continuation_Line
   --    flag should be set, cleared, or left alone
   --  CLEAR for begin, declare, do, or generic
   --  CLEAR for semicolon if paren depth = 0
   --  CLEAR for arrow if paren depth = 0
   --  LEAVE ALONE for comment
   --  FOR abort, CLEAR if previous was then, else SET
   --  FOR or, CLEAR if next token is when/accept/terminate/delay, o/w SET
   --  FOR else, SET if previous was or, o/w CLEAR
   --  FOR then, SET if previous was and, o/w CLEAR
   --  FOR private, SET if next is ; o/w CLEAR
   --  FOR exception, CLEAR if next is when, o/w SET
   --  FOR is, set if (NOT Need_To_Check_Is and next is not record) or
   --    next is new, abstract or separate or <>, o/w CLEAR
   --  FOR loop, record, select, SET if previous is end, o/w CLEAR
   --
   --  SET FOR ALL OTHERS
   --  Parameters:
   --    The_Token           : the current token
   --    Continuation_Line      : the current value of the flag
   --    Previous_Token_Type : type of the previous token
   --    The_String          : the string, so we can do lookahead
   --    Current_Location    : location in The_String, so we can do
   --                          lookahead
   --    RETURN              : the new value of the flag
   ---------------------------------------------------------------------

   function Check_Continuation_Line
     (The_Token           : in Lexer.Token;
      Next_Token_Type     : in Lexer.Tokens;
      Continuation_Line   : in Boolean;
      Previous_Token_Type : in Lexer.Tokens;
      Need_To_Check_Is    : in Boolean;
      The_String          : in Reformat_String;
      Parenthesis_Depth   : in Integer;
      Current_Location    : in Natural)
      return Boolean
   is
      Result          : Boolean;
      Token_Type      : Lexer.Tokens;

   begin   --  Check_Continuation_Line

      --  get current token type
      Token_Type := Lexer.Gettokentype (Item => The_Token);

      case Token_Type is
            --  CLEAR for begin, declare, do, abort,
            --  or generic
         when Lexer.Begin_T | Lexer.Declare_T | Lexer.Do_T | Lexer.Generic_T =>
            return False;

            --  FOR abort, CLEAR if previous was then, o/w SET
         when Lexer.Abort_T =>
            return Previous_Token_Type /= Lexer.Then_T;

            --  FOR or, CLEAR if next is when/accept/terminate or delay
            --  o/w SET
         when Lexer.Or_T =>
            return not
              (Next_Token_Type = Lexer.When_T or else
               Next_Token_Type = Lexer.Accept_T or else
               Next_Token_Type = Lexer.Terminate_T or else
               Next_Token_Type = Lexer.Delay_T);

            --  FOR ;, =>, SET if parenthesis_depth > 0, o/w CLEAR
         when Lexer.Semicolon_T | Lexer.Arrow_T =>
            return Parenthesis_Depth > 0;

            --  FOR private, SET if next is ; o/w CLEAR
         when Lexer.Private_T =>
            return Next_Token_Type = Lexer.Semicolon_T;

            --  FOR loop, record, select, SET if previous is end, o/w CLEAR
         when Lexer.Record_T | Lexer.Select_T | Lexer.Loop_T =>
            return Previous_Token_Type = Lexer.End_T;

            --  LEAVE ALONE for comment
         when Lexer.Comment_T =>
            return Continuation_Line;

            --  FOR else, SET if previous was or, o/w CLEAR
         when Lexer.Else_T =>
            return Previous_Token_Type = Lexer.Or_T;

            --  FOR then, SET if previous was and, o/w CLEAR
         when Lexer.Then_T =>
            return Previous_Token_Type = Lexer.And_T;

            --  FOR exception, CLEAR if next is when, o/w SET
         when Lexer.Exception_T =>
            return Previous_Token_Type /= Lexer.Semicolon_T;

            --  FOR is, set if (NOT Need_To_Check_Is and next is not record
            --  or when) or next is new, abstract or separate, o/w CLEAR
         when Lexer.Is_T =>
            Result := (not Need_To_Check_Is
              and then (Next_Token_Type /= Lexer.Record_T
                and then Next_Token_Type /= Lexer.When_T))
              or else Next_Token_Type = Lexer.New_T
              or else Next_Token_Type = Lexer.Abstract_T
              or else Next_Token_Type = Lexer.Box_T
              or else Next_Token_Type = Lexer.Separate_T;
            return Result;

            --  SET FOR ALL OTHERS
         when others =>
            return True;
      end case;
   end Check_Continuation_Line;

   ---------------------------------------------------------------------
   --
   --  Procedure: Add_Indentation
   --  Description: This procedure adds the indentation at the start of a
   --    new line, including indentation for statements that are inside
   --    parentheses
   --       We have a special case when we encounter a BEGIN or
   --    EXCEPTION, because we want these indented at the SAME
   --    indentation as the procedure header (or declare block) rather
   --    than indented Indent_Spaces.  Similarly, ELSIF and ELSE should
   --    be at the same indentation as the IF.
   --  Parameters:
   --    The_Token           : the current token
   --    Current_Location    : Current location in string
   --    Current_Indentation : the current number of spaces to indent
   --    Indent_Spaces       : the number of spaces to indent each
   --                          lexical scope
   --    Parenthesis_Depth   : the current depth of parentheses
   --    Indent_Parenthesis  : whether to indent parenthesis
   --    Continuation_Line      : tells whether we're in the middle of
   --                          a statement
   --    Stack_Token_Type    : the type of token on the stack
   --    Last_Char           : the index of the last character in the
   --                          new string
   --    The_String          : string being reformatted
   --    New_String          : the string to add indentation to
   ---------------------------------------------------------------------

   procedure Add_Indentation
     (The_Token           : in     Lexer.Token;
      Current_Location    : in     Natural;
      Current_Indentation : in     Natural;
      Indent_Spaces       : in     Natural;
      Parenthesis_Depth   : in     Natural;
      Indent_Parenthesis  : in     Boolean;
      Continuation_Line   : in     Boolean;
      Stack_Token_Type    : in     Lexer.Tokens;
      Previous_Token_Type : in     Lexer.Tokens;
      Next_Token_Type     : in     Lexer.Tokens;
      Last_Char           : in out Natural;
      The_String          : in     Reformat_String;
      New_String          : in out Reformat_String)
   is
      Indent_By  : Integer := Current_Indentation;
   begin   --  Add_Indentation

      case Lexer.Gettokentype (The_Token) is
         when Lexer.Begin_T =>
            --  16-Feb-2002 (mcc) : added Error_T for
            --  reformat on enter.  Moves begin backward
            --  if it the stack is empty
            if (Stack_Token_Type = Lexer.Procedure_T) or else
              (Stack_Token_Type = Lexer.Function_T) or else
              (Stack_Token_Type = Lexer.Package_T) or else
              (Stack_Token_Type = Lexer.Task_T) or else
              (Stack_Token_Type = Lexer.Declare_T) or else
              (Stack_Token_Type = Lexer.Error_T) or else
              (Stack_Token_Type = Lexer.Entry_T) then
               Indent_By := Indent_By - Indent_Spaces;
            end if;

         when Lexer.Procedure_T | Lexer.Function_T | Lexer.Package_T =>
            if (Stack_Token_Type = Lexer.Generic_T) and then
              (Previous_Token_Type /= Lexer.With_T) then
               Indent_By := Indent_By - Indent_Spaces;
            end if;

         when Lexer.Exception_T =>
            if Previous_Token_Type = Lexer.Semicolon_T
              or else Previous_Token_Type = Lexer.Begin_T
            then
               Indent_By := Indent_By - Indent_Spaces;
            end if;

            --  previous token is not WITH, IS, ABSTRACT, LIMITED or
            --  TAGGED.  Inside Package, task or protected scope.
         when Lexer.Private_T =>
            if Previous_Token_Type /= Lexer.With_T
              and then Previous_Token_Type /= Lexer.Is_T
              and then Previous_Token_Type /= Lexer.Abstract_T
              and then Previous_Token_Type /= Lexer.Limited_T
              and then Previous_Token_Type /= Lexer.Tagged_T
              and then
                (Stack_Token_Type = Lexer.Package_T
                 or else Stack_Token_Type = Lexer.Task_T
                 or else Stack_Token_Type = Lexer.Protected_T)
            then
               Indent_By := Indent_By - Indent_Spaces;
            end if;

         when Lexer.Elsif_T | Lexer.Else_T =>
            if Stack_Token_Type = Lexer.If_T
              or else Stack_Token_Type = Lexer.Select_T
            then
               Indent_By := Indent_By - Indent_Spaces;
            end if;

         when Lexer.When_T =>
            if Stack_Token_Type = Lexer.Case_T
              or else Stack_Token_Type = Lexer.Exception_T
            then
               Indent_By := Indent_By - Indent_Spaces;
            end if;

         when Lexer.Comment_T =>
            if (Stack_Token_Type = Lexer.Case_T
                or else Stack_Token_Type = Lexer.Exception_T)
              and then Next_Token_Type = Lexer.When_T
            then
               Indent_By := Indent_By - Indent_Spaces;
            end if;

         when Lexer.Or_T =>
            if Next_Token_Type = Lexer.When_T
              or else Next_Token_Type = Lexer.Accept_T
              or else Next_Token_Type = Lexer.Delay_T
              or else Next_Token_Type = Lexer.Terminate_T
            then
               Indent_By := Indent_By - Indent_Spaces;
            end if;

         when Lexer.Then_T =>
            if Next_Token_Type = Lexer.Abort_T then
               Indent_By := Indent_By - Indent_Spaces;
            end if;

         when others =>
            null;
      end case;

      if Continuation_Line then
         Indent_By := Indent_By + Indent_Spaces;

         if Indent_Parenthesis and then Parenthesis_Depth > 0 then
            Indent_By := Indent_By + (Parenthesis_Depth - 1) * Indent_Spaces;
         end if;

      else
         if Indent_Parenthesis and then Parenthesis_Depth > 0 then
            Indent_By := Indent_By + Parenthesis_Depth * Indent_Spaces;
         end if;
      end if;

      --  add space for indentation
      if Indent_By > 0 then
         Writer.Indent
           (Count => Indent_By,
            Into  => New_String,
            Index => Last_Char);
      end if;
   end Add_Indentation;

   ----------------------------------------------------------------------------
   --
   --  Function: Reformat
   --  Description: This function performs both capitalization and
   --    indentation and returns a null-terminated string
   --    For capitalization, reserved words and Identifiers are
   --       reformatted in several ways:
   --       a.  changed to all upper case
   --       b.  changed to all lower case
   --       c.  changed to mixed case, characters at beginning of words
   --              capitalized, characters in the middle of words unchanged
   --       d.  changed to mixed case, characters at beginning of words
   --              capitalized, characters in the middle of words forced
   --              to lower case
   --    For indentation, we indent each lexical scope the number of
   --       spaces specified as a parameter
   --    The Success parameter tells whether:
   --       a.  no reformatting was performed
   --       b.  capitalization only was performed
   --       c.  both capitalization and indentation were performed
   --
   --  Algorithm
   --    Allocate space for new string
   --    Initialize the lexer
   --    Initialize flags
   --    If Color_Option is not No_Color
   --       Add RTF.Header to new string
   --       Add other_color string to new string
   --    Get the first token
   --    Set last token end to token_end
   --    While not at end of input string
   --       If token line is > current line
   --          Copy remainder of previous line(s) as follows:
   --          Set copy index to last token end + 1
   --          While token line is > current line
   --             While not at <eol> of the string
   --                Copy character into new string
   --                Increment copy index
   --             Increment copy index (past LF)
   --             Add <eol> and RTF line feed to new string
   --             Increment current line
   --          Add beginning of line indentation
   --       Otherwise,
   --          Add characters between last token and this one
   --          Set last token end to end column of token
   --       Colorize based on color option
   --       If token is a reserved word
   --          check for a change in scope
   --          If color option is colorize only
   --             Copy into new string
   --          Otherwise
   --             Capitalize reserved word based on RW_Option
   --       Otherwise, if token is an identifier
   --          If color option is colorize only
   --             Copy into new string
   --          Otherwise
   --             Capitalize identifier word based on ID_Option
   --       Otherwise
   --          Copy into new string
   --       Set Start_For_Next_Token to Token_End + 1
   --       Get the next token in the string
   --    Add any remaining characters to new string
   --    If Color_Option is not No_Color
   --       Add RTF.Footer to new string
   --    Copy into a string that's exactly the right size
   --    Free memory for new string
   --
   --  Parameters:
   --    The_String    : the string to be reformatted
   --    RW_Option     : the capitalization option selected for reserved
   --                    words
   --    ID_Option     : the capitalization option selected for
   --                    identifiers
   --    Indent_Spaces : the number of spaces to indent each lexical
   --                    scope
   --    Color_Option  : what level of colorization to do
   --    Success       : at what level the reformat was completed
   --    Result        : the reformatted string
   ----------------------------------------------------------------------------

   procedure Reformat
     (The_String      : in     Reformat_String;
      Success         :    out Boolean;
      Result          :    out Reformat_String;
      Starting_Indent : in     Natural         := 0)
   is

      New_String            : Reformat_String;

      --  variables to keep track of line/indentation
      Current_Line          : Natural;
      Current_Indentation   : Natural := Starting_Indent;

      --  token variables
      Current_Token         : Lexer.Token;
      Next_Token            : Lexer.Token;
      Start_For_Next_Token  : Natural;
      Token_Start           : Natural;
      Token_End             : Natural;
      Got_Token             : Boolean;
      Got_Next_Token        : Boolean;
      End_Of_Input          : Boolean;
      Current_Token_Last    : Natural;
      Token_Line            : Natural;
      Token_Class           : Lexer.Tokenclass;

      --  variable to keep track of where tokens are located
      Last_Token_End        : Natural;

      --  variable to keep track of current character in new string
      Last_Char             : Natural;

      --  variables to keep track of parenthesis depth and whether
      --  or not we're processing a declaration
      Parenthesis_Depth     : Natural;
      Stack_Token_Type      : Lexer.Tokens;

      --  variables to keep track of processing status
      Token_Type, Previous_Token_Type, Next_Token_Type : Lexer.Tokens;
      Need_To_Check_Is      : Boolean;
      Continuation_Line     : Boolean;
      Bad_Token_Starts_Line : Boolean;

      ----------------------------------------
      --  PROCEDURE Process_Line_Start
      --
      --  copy up to beginning of line
      --  and indent this line
      ----------------------------------------

      procedure Process_Line_Start is
         Copy_Index : Integer;
         End_Copy   : Integer;
      begin
         --  If current line is 0, we just found the
         --  first token in the file.  We want to increase
         --  current line to 1 and set the copy index to
         --  1 (to start copying from the beginning of
         --  the file)
         if Current_Line = 0 then
            Current_Line := 1;
            Copy_Index := 1;
            --  otherwise, copy remainder of previous line(s)
         else
            if  The_String (Last_Token_End) = Settings.New_Line_Char or
              The_String (Last_Token_End) = Settings.New_Line_Char2
            then
               Copy_Index := Last_Token_End;
            else
               Copy_Index := Last_Token_End + 1;
            end if;
         end if;

         End_Copy := Copy_Index;

         --  While token line is > current line
         while Token_Line > Current_Line loop

            --  find next line start
            while The_String (End_Copy) /= Settings.New_Line_Char loop
               End_Copy := End_Copy + 1;
            end loop;

            --  copy, deleting whitespace at end of line
            for I in Copy_Index .. End_Copy - 1 loop
               if The_String (I) /= ' '
                 and then The_String (I) /= Ascii.Ht
               then
                  Writer.Write
                    (Item   => The_String (I .. I),
                     Into   => New_String,
                     Index  => Last_Char);
               end if;
            end loop;
            --  End_Copy is a LF
            Writer.New_Line (New_String, Last_Char);

            --  Increment copy index (past LF)
            Copy_Index := End_Copy + 1;
            End_Copy   := Copy_Index;

            --  Increment current line
            Current_Line := Current_Line + 1;

         end loop;

         --  Add beginning of line indentation
         --  NOTE: if color option is colorize_only, simply
         --  copy spaces in line up to token.  Otherwise, if
         --  we didn't get a token at the beginning of the
         --  line we need to copy up to token_end.  Otherwise,
         --  need to use Add_Indentation to add the
         --  appropriate indentation

         --  check for "bad" token at start of line
         if not Got_Token then
            Writer.Write
              (Item   => The_String (Copy_Index .. Token_End),
               Into   => New_String,
               Index  => Last_Char);
            --  set bad token at start of line flag
            Bad_Token_Starts_Line := True;
         else

            if Settings.Colorization = Settings.Colorize_Only
              or else Settings.Colorization = Settings.Bold_Only
            then

               Writer.Write
                 (Item   => The_String (Copy_Index .. Token_Start - 1),
                  Into   => New_String,
                  Index  => Last_Char);
            else
               --  add indentation
               Add_Indentation
                 (The_Token           => Current_Token,
                  Current_Location    => Start_For_Next_Token,
                  Current_Indentation => Current_Indentation,
                  Indent_Spaces       => Settings.Normal_Indent,
                  Parenthesis_Depth   => Parenthesis_Depth,
                  Indent_Parenthesis  => Settings.Indent_Nested_Paren,
                  Stack_Token_Type    => Stack_Token_Type,
                  Previous_Token_Type => Previous_Token_Type,
                  Next_Token_Type     => Next_Token_Type,
                  Continuation_Line   => Continuation_Line,
                  Last_Char           => Last_Char,
                  The_String          => The_String,
                  New_String          => New_String);
            end if;   --  check for colorize only
         end if;   --  check for got token
      end Process_Line_Start;

      Old_Indentation : Natural;
      Last_Was_Comment : Boolean := False;

   begin   --  Reformat
      --  Allocate space for new string
      New_String := new String (1 .. Initial_Size);

      --  Initialize the lexer
      Lexer.Initialize;

      if Settings.Colorization /= Settings.Colorize_Only
        and then Settings.Colorization /= Settings.Bold_Only
      then
         --  Initialize flags
         Initialize
           (Current_Line          => Current_Line,
            Last_Char             => Last_Char,
            Start_For_Next_Token  => Start_For_Next_Token,
            Last_Token_End        => Last_Token_End,
            Parenthesis_Depth     => Parenthesis_Depth,
            Stack_Token_Type      => Stack_Token_Type,
            Previous_Token_Type   => Previous_Token_Type,
            Need_To_Check_Is      => Need_To_Check_Is,
            Continuation_Line     => Continuation_Line,
            Bad_Token_Starts_Line => Bad_Token_Starts_Line);

         loop
            --  Get the next token in the string
            Lexer.Getnexttoken
              (Str     => The_String,
               First   => Start_For_Next_Token,
               Last    => Current_Token_Last,
               Result  => Current_Token,
               Haveone => Got_Token,
               Done    => End_Of_Input);

            if Got_Token then
               Token_Start := Lexer.Getstart (Item => Current_Token);
               Token_End := Lexer.Getend (Item => Current_Token);
               Token_Line := Lexer.Getline (Item => Current_Token);
               Token_Type := Lexer.Gettokentype (Item => Current_Token);
            else
               Token_Start := Start_For_Next_Token;
               Token_End := Current_Token_Last;
               Token_Line := Lexer.Getcurrentline;
               Token_Type := Lexer.Comment_T;
            end if;

            --  stop when at end of input string
            exit when End_Of_Input;

            --  Set Start_For_Next_Token to Current_Token_Last
            Start_For_Next_Token := Current_Token_Last + 1;

            --  get next token type
            if not Got_Token
              or else Lexer.Gettokenclass (Current_Token) /= Lexer.Commentclass
            then
               Lookahead_Past_Comments
                 (Lookahead_Token  => Next_Token,
                  Got_One          => Got_Next_Token,
                  The_String       => The_String,
                  Current_Location => Start_For_Next_Token);

               if Got_Next_Token then
                  Next_Token_Type := Lexer.Gettokentype (Item => Next_Token);
               else
                  Next_Token_Type := Lexer.Null_T;
               end if;
            end if;

            --  If got a good token
            if Got_Token then

               --  check for a scope DECREASE
               Check_Scope_Decrease
                 (The_Token           => Current_Token,
                  Current_Indentation => Current_Indentation,
                  Indent_Spaces       => Settings.Normal_Indent,
                  Parenthesis_Depth   => Parenthesis_Depth,
                  Stack_Token_Type    => Stack_Token_Type,
                  Previous_Token_Type => Previous_Token_Type,
                  Need_To_Check_Is    => Need_To_Check_Is);

            end if;

            --  If token line is > current line
            --  Note that we don't do this on the first line
            if Token_Line > Current_Line then
               Process_Line_Start; --  call beginning of line

               --  Otherwise,
            elsif (Settings.Do_Vertical_Spacing
                   and then not Continuation_Line
                   and then Token_Type /= Lexer.Comment_T)
              or else
            --  add a line break if appropriate
              (Writer.Line_Too_Long
                 (Lexer.GetEnd (Current_Token) -
                    Lexer.GetStart (Current_Token) + Last_Char)
               and then
                 (Token_Type = Lexer.Integer_t
                  or else Token_Type = Lexer.Float_t
                  or else Token_Type = Lexer.Based_t
                  or else Token_Type = Lexer.Comment_t
                  or else Token_Type = Lexer.Name_t
                  or else Token_Type = Lexer.String_t))
            then
               Writer.New_Line (New_String, Last_Char);
               --  add indentation
               Add_Indentation
                 (The_Token           => Current_Token,
                  Current_Location    => Start_For_Next_Token,
                  Current_Indentation => Current_Indentation,
                  Indent_Spaces       => Settings.Normal_Indent,
                  Parenthesis_Depth   => Parenthesis_Depth,
                  Indent_Parenthesis  => Settings.Indent_Nested_Paren,
                  Stack_Token_Type    => Stack_Token_Type,
                  Previous_Token_Type => Previous_Token_Type,
                  Next_Token_Type     => Next_Token_Type,
                  Continuation_Line   => Continuation_Line,
                  Last_Char           => Last_Char,
                  The_String          => The_String,
                  New_String          => New_String);
            else
               --  Add characters between last token and this one
               Writer.Write
                 (Item   => The_String (Last_Token_End + 1 .. Token_Start - 1),
                  Into   => New_String,
                  Index  => Last_Char);

            end if;
            -------------------------------------

            --  Set last token end to end of current token
            if Got_Token then
               Last_Token_End := Lexer.Getend (Item => Current_Token);
            else
               Last_Token_End := Token_End;
            end if;

            if Got_Token then
               Old_Indentation := Current_Indentation;
               --  check for a scope INCREASE
               Check_Scope_Increase
                 (The_Token           => Current_Token,
                  The_String          => The_String,
                  Current_Location    => Start_For_Next_Token,
                  Current_Indentation => Current_Indentation,
                  Parenthesis_Depth   => Parenthesis_Depth,
                  Indent_Spaces       => Settings.Normal_Indent,
                  Stack_Token_Type    => Stack_Token_Type,
                  Next_Token_Type     => Next_Token_Type,
                  Need_To_Check_Is    => Need_To_Check_Is,
                  Previous_Token_Type => Previous_Token_Type);

               --  check for semicolon separately from other flags since it
               --  requires that we pass in the string and the current
               --  location so we can do lookahead
               Continuation_Line := Check_Continuation_Line
                 (The_Token           => Current_Token,
                  Next_Token_Type     => Next_Token_Type,
                  Continuation_Line   => Continuation_Line,
                  Previous_Token_Type => Previous_Token_Type,
                  Need_To_Check_Is    => Need_To_Check_Is,
                  The_String          => The_String,
                  Parenthesis_Depth   => Parenthesis_Depth,
                  Current_Location    => Start_For_Next_Token);

               if Token_Type = Lexer.Is_T then
                  Need_To_Check_Is := False;
               end if;

            end if;
            -------------------------------------

            -------------------------------------
            --  Output token
            if Got_Token then
               Token_Class := Lexer.Gettokenclass (Item => Current_Token);

               if Token_Class = Lexer.Reservedclass
                 or else Token_Class = Lexer.Identifierclass
               then

                  if Lexer.Gettokentype (Current_Token) = Lexer.Name_T
                    and then Settings.Do_Vertical_Spacing
                    and then
                      (Next_Token_Type = Lexer.Colon_T
                       or else Next_Token_Type = Lexer.Comma_T)
                    and then not Need_To_Check_Is
                    and then not Token_Stack.Is_Empty
                    and then Parenthesis_Depth = 0
                  then

                     case Token_Stack.Top_Of_Stack is
                        when Lexer.Generic_T   |
                             Lexer.Declare_T   |
                             Lexer.Task_T      |
                             Lexer.Procedure_T |
                             Lexer.Function_T  |
                             Lexer.Entry_T     |
                             Lexer.Package_T =>
                           Decls.Setup (Decls.General);
                           Writer.Chomp_Spaces (New_String, Last_Char);
                           Start_For_Next_Token :=
                             Lexer.Getstart (Current_Token);
                           Decls.Reformat
                             (From   => The_String,
                              First  => Start_For_Next_Token,
                              Into   => New_String,
                              Index  => Last_Char,
                              Indent => Current_Indentation);
                           Token_End := Start_For_Next_Token;
                           Last_Token_End := Start_For_Next_Token;
                           --  Catch_Up := True;
                           Writer.Chomp_Line (New_String, Last_Char);
                           Current_Line := Lexer.Getcurrentline;
                           Continuation_Line := False;

                        when others =>
                           Token_Writer.Write
                             (Item  => Current_Token,
                              From  => The_String,
                              Into  => New_String,
                              Index => Last_Char,
                              Pad   => 0);
                     end case;

                  --  check if we can format a
                  --  parameter list (don't do if nested)
                  --  (don't start if not the first parameter)

                  --  really only do this if
                  --  we just saw the first parenthesis
                  --  and there isn't a nested call

                  elsif Lexer.Gettokentype (Current_Token) = Lexer.Name_T
                    and then Settings.Do_Vertical_Spacing
                    and then Next_Token_Type = Lexer.Arrow_t
                    and then Parenthesis_Depth = 1
                    and then Previous_Token_Type = Lexer.LParen_t
                    and then No_Nested_Call (The_String, Start_For_Next_Token)
                    and then Previous_Token_Type /= Lexer.when_t
                    and then Previous_Token_Type /= Lexer.pipe_t
                  then
                     Decls.Setup (Decls.Arguments);
                     Writer.Chomp_Line (New_String, Last_Char);
                     Start_For_Next_Token :=
                       Lexer.Getstart (Current_Token);

                     --  ada.text_io.put_line("arrow");
                     if Last_Was_Comment then
                        Declarations.Arguments.Continuation_List;
                     end if;

                     declare
                        Indent : Natural;
                     begin
                        Indent := Current_Indentation + Settings.Normal_Indent;
                        Decls.Reformat
                          (From   => The_String,
                           First  => Start_For_Next_Token,
                           Into   => New_String,
                           Index  => Last_Char,
                           Indent => Indent);
                     end;

                     Token_End := Start_For_Next_Token;
                     Last_Token_End := Start_For_Next_Token;
                     --  Catch_Up := True;
                     Writer.Chomp_Line (New_String, Last_Char);
                     Current_Line := Lexer.Getcurrentline;
                     Continuation_Line :=
                       not Declarations.Arguments.Ending_Comment;

                  --  check if we need to reformat
                  --  a subprogram decl
                  elsif Settings.Do_Vertical_Spacing
                    and then
                      (Lexer.Gettokentype (Current_Token) = Lexer.Function_T
                       or else Lexer.Gettokentype (Current_Token) =
                                  Lexer.Procedure_T
                       or else Lexer.Gettokentype (Current_Token) =
                                  Lexer.Accept_T
                       or else Lexer.Gettokentype (Current_Token) =
                                  Lexer.Entry_T)
                  then
                     Start_For_Next_Token := Lexer.Getstart (Current_Token);
                     Declarations.Subprograms.Reformat
                       (From   => The_String,
                        First  => Start_For_Next_Token,
                        Into   => New_String,
                        Index  => Last_Char,
                        Indent => Old_Indentation,
                        Withed => Previous_Token_Type = Lexer.With_T);
                     Token_End := Start_For_Next_Token;
                     Last_Token_End := Start_For_Next_Token;
                     Writer.Chomp_Line (New_String, Last_Char);
                     Current_Line := Lexer.Getcurrentline;
                     Continuation_Line := False;
                     Need_To_Check_Is  := False;

                  --  check to see if we are formatting a
                  --  type definition (not for protected, task)
                  elsif Settings.Reformat_Types
                    and then Lexer.Gettokentype (Current_Token) = Lexer.Type_T
                    and then Previous_Token_type /= Lexer.Use_T
                    and then Previous_Token_Type /= Lexer.Protected_T
                    and then Previous_Token_type /= Lexer.Task_T
                    and then not Is_Record_With_Case
                                   (The_String, Start_For_Next_Token)
                  then
                     Start_For_Next_Token := Lexer.Getstart (Current_Token);
                     Writer.Chomp_Spaces (New_String, Last_Char);
                     Declarations.Types.Reformat
                       (From   => The_String,
                        First  => Start_For_Next_Token,
                        Into   => New_String,
                        Index  => Last_Char,
                        Indent => Old_Indentation);
                     Token_End := Start_For_Next_Token;
                     Last_Token_End := Start_For_Next_Token;
                     Writer.Chomp_Line (New_String, Last_Char);
                     Current_Line := Lexer.Getcurrentline;
                     Continuation_Line := False;
                     Need_To_Check_Is  := False;
                  else
                     --  Capitalize based on Settings
                     Token_Writer.Write
                       (Item  => Current_Token,
                        From  => The_String,
                        Into  => New_String,
                        Index => Last_Char,
                        Pad   => 0);
                  end if;
                  --  as long as the current token type isn't a comment,
               else
                  --  Add any remaining characters to new string
                  Writer.Write
                    (Item   =>
                       The_String (Lexer.Getstart (Item => Current_Token) ..
                           Lexer.Getend (Item => Current_Token)),
                     Into   => New_String,
                     Index  => Last_Char);

               end if;
               --  update previous token type
               if Token_Type /= Lexer.Comment_T then
                  Previous_Token_Type := Token_Type;
                  Last_Was_Comment := False;
               else
                  Last_Was_Comment := True;
               end if;

            else
               --  bad token, so set to other
               Token_Class := Lexer.Otherclass;
               --  there is a special case for a "bad" token at the start
               --  of a line - in this case, we've already copied the string
               --  representing the bad token, so we don't want to copy it
               --  again here
               if Bad_Token_Starts_Line then

                  --  simply reset the flag
                  Bad_Token_Starts_Line := False;

               else
                  Writer.Write
                    (Item   => The_String (Token_Start .. Token_End),
                     Into   => New_String,
                     Index  => Last_Char);
               end if;   --  check of bad token at start of line flag
            end if;

         end loop;

         --  windows.ok_box(Integer'Image (token_end), "reformat_pkg");
         if Got_Token and then The_String (Token_End) = Ascii.NUL then
            --  In rich edit the end of the text is marked with Ascii.Nul
            --  (C "\0"), but behind this there is some waste in the array
            Writer.Write
              (Item   => The_String (Last_Token_End + 1 .. Token_End),
               Into   => New_String,
               Index  => Last_Char);
         elsif Token_End + 1 <= The_String'Length then
            --  Add any remaining characters to new string
            Writer.Write
              (Item   => The_String (Token_End + 1 .. The_String'Length),
               Into   => New_String,
               Index  => Last_Char);
         end if;

         --  copy new string into exact size string
         Writer.Finalize (New_String, Last_Char);

         if Settings.Colorization /= Settings.No_Colorize then
            Reformat_Rtf.Reformat (New_String, Result);
         else
            Result := New_String;
         end if;

      else
         Reformat_Rtf.Reformat (The_String, Result);
      end if; --  colorize

      --  set success to true
      Success := True;

      --  given a malformed Ada program, or a program containing constructs not
      --  yet handled by this procedure, the procedure raises an exception.
      --  Check for that here
   exception

      when others =>
         --  free memory from new string
         Free (New_String);

         --  reformat was not successful, so set success to false
         Success := False;

         --  set result to an empty, null-terminated string
         Result := new String (1 .. 1);
         Result (1) := Character'First;

   end Reformat;

   --------------
   -- Reformat --
   --------------

   function Reformat
     (File      : in MSSyst.String.Ref;
      StartLine : in Natural;
      EndLine   : in Natural) return MSSyst.String.Ref
   is
      Buffer         : Reformat_String := new String'(+File);
      Success        : Boolean;
      Result         : Reformat_String;
      Starting_Index : Natural := 0;
      Where          : Natural := 1;
   begin
      if Startline > 0 then
         while Where < Buffer.all'Last loop
            case Buffer.all (Where) is
               when Ada.Characters.Latin_1.CR | Ada.Characters.Latin_1.LF =>
                  Starting_Index := 0;

               when Ada.Characters.Latin_1.HT =>
                  Starting_Index := Starting_Index + 4;

               when ' ' =>
                  Starting_Index := Starting_Index + 1;

               when others =>
                  exit;

            end case;

            Where := Where + 1;
         end loop;
      end if;

      Reformat
        (The_String       => Buffer,
         Success          => Success,
         Result           => Result,
         Starting_Indent  => Starting_Index);

      if Success then
         declare
            Res_Str : String := Result.all;
         begin
            return +Res_Str;
         end;
      else
         return File;
      end if;
   end Reformat;

end Reformat_Pkg;
