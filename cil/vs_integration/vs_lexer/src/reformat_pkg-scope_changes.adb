---------------------------------------------------------------
--                                                           
--  ADA GNAT INTEGRATED DEVELOPMENT ENVIRONMENT (AdaGIDE)    
--                                                           
--  REFORMAT_PKG.SCOPE_CHANGES.ADB 
--  Description : This is a private child package of Reformat_Pkg.
--                It provides a number of functions to check for
--                scope changes (increase or decrease) in the
--                program being reformatted
--                       
--  By: Tim Chamillard
--                                         
-- REFORMAT_PKG.SCOPE_CHANGES is free software; you can redistribute
-- it and/or modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly
-- indicate if it has been modified.
--
-- REFORMAT_PKG.SCOPE_CHANGES is distributed in the hope that it will 
-- be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--                                         
---------------------------------------------------------------

with Lexer;
with Token_Stack;


-- uses to get "=" on TokenClass, Tokens
use type Lexer.Tokenclass;
use type Lexer.Tokens;

package body Reformat_Pkg.Scope_Changes is

   ---------------------------------------------------------------------
   --
   -- Function: Decrease_Indentation
   -- Description: This function decreases the indentation by
   --    indent spaces
   -- Parameters:
   --    Current_Indentation : current level of indentation
   --    Indent_Spaces       : indent spaces for each level
   --    RETURN              : new level of indentation
   ---------------------------------------------------------------------

   function Decrease_Indentation ( Current_Indentation : in Natural;
         Indent_Spaces : in Natural ) return Natural is

   begin   -- Decrease_Indentation

      if Current_Indentation > Indent_Spaces then
         return Current_Indentation - Indent_Spaces;
      else
         return 0;
      end if;

   end Decrease_Indentation;
   ---------------------------------------------------------------------
   --
   -- Procedure : Lookahead_For_Use_Or_Loop
   -- Description : Seeks through tokens until loop or use
   --               found, returning that token.
   -- Parameters:
   --    Lookahead_Token      : next non-commented token
   --    Got_One              : True on success
   --    The_String           : what's being reformatted
   --    Current_Location     : where we are
   --
   ---------------------------------------------------------------------
   procedure Lookahead_For_Use_Or_Loop(
         Lookahead_Token  : out Lexer.Token;
         Got_One          : out Boolean;
         The_String       : in  Reformat_String;
         Current_Location : in  Integer) is
      Startlocation : Integer := Current_Location;
      Endlocation   : Integer;
      Current_Line  : Natural;
      Next_Done     : Boolean;
   begin

      Current_Line := Lexer.Getcurrentline;
      loop
         Lexer.Getnexttoken (Str => The_String,
            First   => Startlocation,
            Last    => Endlocation,
            Result  => Lookahead_Token,
            Haveone => Got_One,
            Done    => Next_Done );
         exit when Next_Done or else
            Lexer.Gettokentype (Item => Lookahead_Token) = Lexer.Use_T or else
            Lexer.Gettokentype (Item => Lookahead_Token) = Lexer.Loop_T;
         Startlocation := Endlocation + 1;
      end loop;

      -- set current line in lexer to the line it was
      -- before the lookahead
      Lexer.Initialize (Line => Current_Line);
   end Lookahead_For_Use_Or_Loop;

   ---------------------------------------------------------------------
   --
   -- Function : Check_if_spec
   -- Description : Seeks through tokens until we
   --               determine if spec or not
   --               i.e. found IS/DO before ; outside () or RENAMES and also
   --               next after is is not NEW, SEPARATE or ABSTRACT
   -- Parameters:
   --    Lookahead_Token      : next non-commented token
   --    Got_One              : True on success
   --    The_String           : what's being reformatted
   --    Current_Location     : where we are
   --
   ---------------------------------------------------------------------
   function Check_If_Spec(
         The_String       : in  Reformat_String;
         Current_Location : in  Integer) return Boolean is
      Startlocation     : Integer := Current_Location;
      Endlocation       : Integer;
      Current_Line      : Natural;
      Next_Done         : Boolean;
      Got_One           : Boolean;
      Lookahead_Token   : Lexer.Token;
      Token_Type        : Lexer.Tokens;
      Next_Token_Type   : Lexer.Tokens;
      Parenthesis_Depth : Integer := 0;
   begin

      Current_Line := Lexer.Getcurrentline;
      loop
         Lexer.Getnexttoken (Str => The_String,
            First   => Startlocation,
            Last    => Endlocation,
            Result  => Lookahead_Token,
            Haveone => Got_One,
            Done    => Next_Done );

         if Got_One then
            Token_Type := Lexer.Gettokentype(Item => Lookahead_Token);
         else
            Token_Type := Lexer.Null_T;
         end if;

         if Token_Type = Lexer.Lparen_T then
            Parenthesis_Depth := Parenthesis_Depth + 1;
         elsif Token_Type = Lexer.Rparen_T then
            Parenthesis_Depth := Parenthesis_Depth - 1;
         end if;

         Startlocation := Endlocation + 1;

         exit when Next_Done or else
            Token_Type=Lexer.Is_T or else
            Token_Type=Lexer.Renames_T or else
            Token_Type=Lexer.Do_T or else
            (Token_Type=Lexer.Semicolon_T
            and then Parenthesis_Depth = 0);

      end loop;


      Lookahead_Past_Comments(Lookahead_Token => Lookahead_Token,
         Got_One          => Got_One,
         The_String       => The_String,
         Current_Location => Startlocation);

      if Got_One then
         Next_Token_Type := Lexer.Gettokentype(Item => Lookahead_Token);
      else
         Next_Token_Type := Lexer.Null_T;
      end if;

      -- set current line in lexer to the line it was
      -- before the lookahead
      Lexer.Initialize (Line => Current_Line);


      if Token_Type=Lexer.Is_T then
         return Next_Token_Type = Lexer.New_T or else
            Next_Token_Type = Lexer.Separate_T or else
            Next_Token_Type = Lexer.Abstract_T;
      elsif Token_Type=Lexer.Do_T then
         return False;
      else
         return True;
      end if;

   end Check_If_Spec;

   ---------------------------------------------------------------------
   --
   -- Procedure: Check_Scope_Increase
   -- Description: This procedure checks for an increaes in scope depth
   --
   -- Parameters:
   --    The_Token           : the token to check
   --    The_String          : String being reformatted
   --    Current_Location    : current location in string
   --    Current_Indentation : the current number of spaces to indent
   --    Indent_Spaces       : the number of spaces to indent each 
   --                          lexical scope
   --    Previous_Token_Type : type of previous token
   --    Next_Token_Type     : type of next token
   --    Need_To_Check_Is    : set on non-specs, must clear elsewhere
   --    Stack_Token_Type    : Token type on top of stack
   ---------------------------------------------------------------------

   procedure Check_Scope_Increase ( The_Token : in Lexer.Token;
         The_String          : in     Reformat_String;
         Current_Location    : in     Natural;
         Current_Indentation : in out Natural;
         Parenthesis_Depth   : in out Natural;
         Indent_Spaces       : in     Natural;
         Previous_Token_Type : in     Lexer.Tokens;
         Next_Token_Type     : in     Lexer.Tokens;
         Need_To_Check_Is    : in out Boolean;
         Stack_Token_Type    : in out Lexer.Tokens ) is

      Token_Type : Lexer.Tokens;
      Next_Token : Lexer.Token;
      Got_Token  : Boolean; -- used for exception when

      procedure Increase_Scope is
      begin
         -- found depth increase, so "increment" indentation
         Current_Indentation := Current_Indentation + Indent_Spaces;

         -- push onto the stack and set stack token
         Token_Stack.Push (The_Token => Token_Type);
         Stack_Token_Type := Token_Type;
      end Increase_Scope;

   begin   -- Check_Scope_Increase

      -- get current token type and class
      Token_Type := Lexer.Gettokentype (Item => The_Token);

      case Token_Type is
         when Lexer.If_T|Lexer.Select_T =>
            if Previous_Token_Type /= Lexer.End_T then
               Increase_Scope;
            end if;
         -- double indentation for case  
         when Lexer.Case_T =>
            if Previous_Token_Type /= Lexer.End_T then
               Increase_Scope;
               Current_Indentation := Current_Indentation + Indent_Spaces;
            end if;
         when Lexer.Record_T =>
            if Previous_Token_Type /= Lexer.End_T and then
                  Previous_Token_Type /= Lexer.Null_T then
               Increase_Scope;
            end if;
         when Lexer.Generic_T =>
            Increase_Scope;
         when Lexer.Protected_T =>
            if ( Next_Token_Type /= Lexer.Procedure_T ) and then
                  ( Next_Token_Type /= Lexer.Function_T ) then
               Need_To_Check_Is := True;
               Increase_Scope;
            end if;

         -- note by rules, task/entry are different, but can get away with
         -- it.
         when Lexer.Package_T|Lexer.Procedure_T|Lexer.Function_T|
               Lexer.Entry_T|Lexer.Task_T =>
            if Previous_Token_Type /= Lexer.With_T then
               if not Check_If_Spec(The_String,Current_Location) then
                  Need_To_Check_Is := True;
                  if Stack_Token_Type = Lexer.Generic_T then
                     -- swap with top token
                     Stack_Token_Type := Token_Stack.Pop;
                     Token_Stack.Push(The_Token => Lexer.Begin_T);
                     Stack_Token_Type := Token_Type;
                  else
                     Increase_Scope;
                  end if;
               else
                  if Stack_Token_Type = Lexer.Generic_T then
                     begin
                        -- generic is done, decrease scope
                        -- note we might we the outermost scope
                        -- so catch empty exception
                        Stack_Token_Type := Token_Stack.Pop;
                        Stack_Token_Type := Token_Stack.Top_of_Stack;
                     exception
                        when Token_Stack.Empty =>
                           Stack_Token_Type := Lexer.Null_T;
                     end;
                     Current_Indentation := Decrease_Indentation (
                        Current_Indentation => Current_Indentation,
                        Indent_Spaces       => Indent_Spaces );
                  end if;
               end if;
            end if;
         when Lexer.Accept_T =>
            if not Check_If_Spec(The_String,Current_Location) then
               Increase_Scope;
            end if;
         when Lexer.Declare_T =>
            Increase_Scope;
         when Lexer.Begin_T =>
            if (( Stack_Token_Type /= Lexer.Task_T )       and then
                  ( Stack_Token_Type /= Lexer.Procedure_T )  and then
                  ( Stack_Token_Type /= Lexer.Function_T )   and then
                  ( Stack_Token_Type /= Lexer.Declare_T )    and then
                  ( Stack_Token_Type /= Lexer.Entry_T )      and then
                  ( Stack_Token_Type /= Lexer.Package_T ) )  then
               Increase_Scope;
            else -- swap with top token
               Stack_Token_Type := Token_Stack.Pop;
               Token_Stack.Push(The_Token => Token_Type);
               Stack_Token_Type := Token_Type;
            end if;
         when Lexer.While_T =>
            Increase_Scope;
         when Lexer.For_T =>
            Lookahead_For_Use_Or_Loop(Next_Token,Got_Token,The_String,
               Current_Location);
            if Got_Token and then
                  ( Lexer.Gettokentype (Item => Next_Token) =
                  Lexer.Loop_T ) then
               Increase_Scope;
            end if;
         when Lexer.Loop_T =>
            if Previous_Token_Type /= Lexer.End_T then
               -- check if while/for on top of stack
               -- if so, swap, o/w increase depth
               if Stack_Token_Type = Lexer.While_T or else
                     Stack_Token_Type = Lexer.For_T then
                  Stack_Token_Type := Token_Stack.Pop;
                  Token_Stack.Push (The_Token => Token_Type);
                  Stack_Token_Type := Token_Type;
               else
                  Increase_Scope;
               end if;
            end if;
         when Lexer.Exception_T =>
            if Next_Token_Type = Lexer.When_T then
               Increase_Scope;
            end if;
         when Lexer.Lparen_T =>
            Parenthesis_Depth := Parenthesis_Depth + 1;
         when Lexer.Rparen_T =>
            if Parenthesis_Depth > 0 then
               Parenthesis_Depth := Parenthesis_Depth - 1;
            end if;
         when others =>
            null;
      end case;

      -- add an exception handler to catch exception if we try to
      -- pop an empty stack
   exception

      -- don't do anything, just catch exception
      when Token_Stack.Empty => null;

   end Check_Scope_Increase;




   ---------------------------------------------------------------------
   --
   -- Procedure: Check_Scope_Decrease
   -- Description: This procedure checks for a decrease in scope depth
   --    based on the following:
   --       Lexical scope depth DECREASES whenever we encounter an END,
   --    a ';' (we found a spec), a NEW, SEPARATE, or ABSTRACT when we're
   --    checking for an IS (found a rename or instantiation), or 
   --    RENAMES
   -- Parameters:
   --    The_Token           : the token to check
   --    The_String          : the string we're using as input
   --    Current_Location    : the current location in The_String
   --    Current_Indentation : the current number of spaces to indent
   --    Indent_Spaces       : the number of spaces to indent each 
   --                          lexical scope
   --    Parenthesis_Depth   : the current depth of parentheses
   --    Previous_Token_Type : type of previous token
   --    Need_To_Check_Is    : tells whether we're checking for a
   --                          spec vs body
   --    Stack_Token_Type    : tells the current stack token type for
   --                          determining scope
   ---------------------------------------------------------------------

   procedure Check_Scope_Decrease ( The_Token : in Lexer.Token;
         Current_Indentation : in out Natural;
         Indent_Spaces       : in     Natural;
         Parenthesis_Depth   : in     Natural;
         Previous_Token_Type : in     Lexer.Tokens;
         Need_To_Check_Is    : in out Boolean;
         Stack_Token_Type    : in out Lexer.Tokens ) is

      Token_Type : Lexer.Tokens;
      Token_Class : Lexer.Tokenclass;

   begin   -- Check_Scope_Decrease

      -- get current token type and class
      Token_Type := Lexer.Gettokentype (Item => The_Token);
      Token_Class := Lexer.Gettokenclass (Item => The_Token);

      -- check for reserved word
      if ( Token_Class = Lexer.Reservedclass ) then

         -- check for an end
         if ( Token_Type = Lexer.End_T ) then

            -- need to check for an exception_t on the top of the
            -- token stack
            if ( Stack_Token_Type = Lexer.Exception_T ) then

               -- in this case we need to decrease indentation TWICE
               -- and pop the stack TWICE
               Current_Indentation := Decrease_Indentation (
                  Current_Indentation => Current_Indentation,
                  Indent_Spaces       => Indent_Spaces );
               Current_Indentation := Decrease_Indentation (
                  Current_Indentation => Current_Indentation,
                  Indent_Spaces       => Indent_Spaces );
               Stack_Token_Type := Token_Stack.Pop;
               Stack_Token_Type := Token_Stack.Pop;
               Stack_Token_Type := Token_Stack.Top_Of_Stack;

               -- otherwise
            else

               -- adjust indentation
               Current_Indentation := Decrease_Indentation (
                  Current_Indentation => Current_Indentation,
                  Indent_Spaces       => Indent_Spaces );

               -- check for a case following an end.  When we start a
               -- case statement, we increase indentation TWICE, when
               -- we see END we decrease indentation ONCE, so if we
               -- see the CASE in END CASE here, we need to decrease
               -- the indentation the second time
               if ( Stack_Token_Type = Lexer.Case_T ) then

                  -- found our special case; decrease depth again
                  Current_Indentation := Decrease_Indentation (
                     Current_Indentation => Current_Indentation,
                     Indent_Spaces       => Indent_Spaces );

               end if;

               -- pop the token stack and set stack token type
               -- to the current top of the stack
               Stack_Token_Type := Token_Stack.Pop;
               Stack_Token_Type := Token_Stack.Top_Of_Stack;

            end if;   -- check for exception

         end if;   -- check for renames, end

      end if;   -- check for reserved word


      -- add an exception handler to catch exception if we try to
      -- pop an empty stack
   exception

      -- don't do anything, just catch exception
      when Token_Stack.Empty => Stack_Token_Type := Lexer.error_t;

   end Check_Scope_Decrease;

end Reformat_Pkg.Scope_Changes;
