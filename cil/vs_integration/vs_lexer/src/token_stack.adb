---------------------------------------------------------------
--                                                           
--  ADA GNAT INTEGRATED DEVELOPMENT ENVIRONMENT (AdaGIDE)    
--                                                           
--  TOKEN_STACK.ADB 
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
WITH Lexer;
WITH Ada.Unchecked_Deallocation;

PACKAGE BODY Token_Stack IS

   -- type definitions for the stack
   TYPE Stack_Element;
   TYPE Stack_Element_Access IS ACCESS Stack_Element;

   TYPE Stack_Element IS
   RECORD
      	Next	: Stack_Element_Access:= NULL;
      	Data	: Lexer.Tokens;
   END RECORD;

   -- package global to hold stack
   Stack_Head  : Stack_Element_Access:= NULL;

   ------------------------------------------------------------------------
   --
   -- Procedure : Push
   -- Description
   --    Pushes The_Token onto the stack
   -- Parameters
   --    The_Token : the token to push onto the stack
   --
   ------------------------------------------------------------------------

   PROCEDURE Push (The_Token : Lexer.Tokens) IS

      New_Element : Stack_Element_Access;

   BEGIN

      -- allocate space for new stack element and add token
      New_Element := NEW Stack_Element;
      New_Element.Data:= The_Token;

      -- move new element to top of stack
      New_Element.Next:= Stack_Head;
      Stack_Head:= New_Element;

   -- check for overflow        
   EXCEPTION
      WHEN OTHERS => RAISE Overflow;

   END Push;

   function Is_Empty return Boolean is
   begin
      return Stack_Head = null;
   end Is_Empty;
   
   ------------------------------------------------------------------------
   --
   -- Function : Top_Of_Stack
   -- Description
   --    Returns a token from the stack WITHOUT POPPING THE STACK
   -- Parameters
   --    RETURN : the token popped from the stack
   --
   ------------------------------------------------------------------------

   FUNCTION Top_Of_Stack RETURN Lexer.Tokens IS

   BEGIN

      -- make sure stack has at least one element
      IF Stack_Head /= NULL THEN

         -- return top of stack
         RETURN Stack_Head.Data;

      -- stack is empty
      ELSE
         RAISE Empty;

      END IF;   -- check for empty stack

   END Top_Of_Stack;

   ------------------------------------------------------------------------
   --
   -- Procedure : Pop
   -- Description
   --    Pops and returns a token from the stack
   -- Parameters
   --    RETURN : the token popped from the stack
   --
   ------------------------------------------------------------------------

   FUNCTION Pop RETURN Lexer.Tokens IS
      Data        : Lexer.Tokens;
      Top_Element : Stack_Element_Access;
      PROCEDURE Free IS NEW Ada.Unchecked_Deallocation (
         Object => Stack_Element,
         Name   => Stack_Element_Access);

   BEGIN

      -- make sure stack has at least one element
      IF Stack_Head /= NULL THEN

         -- move top of stack down and return old top
         Top_Element := Stack_Head;
         Stack_Head := Top_Element.Next;
         Data := Top_Element.Data;
         Free (Top_Element);
         return Data;

      -- stack is empty
      ELSE
         RAISE Empty;

      END IF;   -- check for empty stack

   END Pop;
   
   procedure Empty_Stack is
      X : Lexer.Tokens;
   begin
      while not Is_Empty loop
         x := Pop;
      end loop;
   end Empty_Stack;

END Token_Stack;
