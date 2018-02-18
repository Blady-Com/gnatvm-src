---------------------------------------------------------------
--                                                           
--  ADA GNAT INTEGRATED DEVELOPMENT ENVIRONMENT (AdaGIDE)    
--                                                           
--  TOKEN_STACK.ADS 
--  Description : provides operations on a stack of tokens 
--                       
--  By: Tim Chamillard
--                                         
-- TOKEN_STACK is free software; you can redistribute it and/or
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly
-- indicate if it has been modified.
--
-- TOKEN_STACK is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--
---------------------------------------------------------------
-- Change log:
-- 11/23/00 (mcc): added empty to fix with bug at beginning
--                 of file after reformatting something else
--                 that didn't empty stack.
---------------------------------------------------------------

WITH Lexer;

PACKAGE Token_Stack IS

   -- exceptions to indicate trying to push onto a full stack or
   -- trying to pop from an empty stack
   Overflow : EXCEPTION;
   Empty	: EXCEPTION;

   ------------------------------------------------------------------------
   --
   -- function : Is_Empty
   -- Description
   --    ask if the stack is empty
   --
   ------------------------------------------------------------------------
   function Is_Empty return Boolean;
   
   ------------------------------------------------------------------------
   --
   -- Procedure : Push
   -- Description
   --    Pushes The_Token onto the stack
   -- Parameters
   --    The_Token : the token to push onto the stack
   --
   ------------------------------------------------------------------------

   PROCEDURE Push( The_Token : Lexer.Tokens );

   ------------------------------------------------------------------------
   --
   -- Function : Top_Of_Stack
   -- Description
   --    Returns a token from the stack WITHOUT POPPING THE STACK
   -- Parameters
   --    RETURN : the token popped from the stack
   --
   ------------------------------------------------------------------------

   FUNCTION Top_Of_Stack RETURN Lexer.Tokens;

   ------------------------------------------------------------------------
   --
   -- Function : Pop
   -- Description
   --    Pops and returns a token from the stack
   -- Parameters
   --    RETURN : the token popped from the stack
   --
   ------------------------------------------------------------------------

   FUNCTION Pop RETURN Lexer.Tokens;
   
   ------------------------------------------------------------------------
   -- procedure Empty_Stack
   --
   -- empty the stack.  Should be called at the beginning of each
   -- reformat
   ------------------------------------------------------------------------
   procedure Empty_Stack;

END Token_Stack;
