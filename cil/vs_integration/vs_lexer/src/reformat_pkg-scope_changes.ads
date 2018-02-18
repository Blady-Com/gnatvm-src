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

private package Reformat_Pkg.Scope_Changes is



   ---------------------------------------------------------------------
   --
   -- Procedure: Check_Scope_Increase
   -- Description: This procedure checks for an increaes in scope depth
   --    based on the following:
   --       Lexical scope depth INCREASES whenever we encounter one of 
   --    the following reserved words: SELECT, TASK, PROCEDURE
   --    (not a generic), FUNCTION (not a generic),
   --    DECLARE, BEGIN (not in declaration scope), 
   --    RECORD, CASE, IF, LOOP, PROTECTED, ENTRY, ACCEPT, or
   --    PACKAGE (not a generic)
   -- Parameters:
   --    The_Token           : the token to check
   --    The_String          : String being reformatted
   --    Current_Location    : current location in string
   --    Current_Indentation : the current number of spaces to indent
   --    Parenthesis_Depth   : # of nested ()s
   --    Indent_Spaces       : the number of spaces to indent each 
   --                          lexical scope
   --    Previous_Token_Type : type of previous token
   --    Next_Token_Type     : type of next token
   --    Need_To_Check_Is    : see if we're waiting for is
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
         Stack_Token_Type    : in out Lexer.Tokens );

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
         Stack_Token_Type    : in out Lexer.Tokens );
         
end Reformat_Pkg.Scope_Changes;
