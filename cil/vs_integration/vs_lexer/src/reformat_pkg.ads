---------------------------------------------------------------
--                                                           
--  ADA GNAT INTEGRATED DEVELOPMENT ENVIRONMENT (AdaGIDE)    
--                                                           
--  REFORMAT_PKG.ADS 
--  Description : This package provides a number of 
--                functions to reformat Ada code to 
--                use the proper capitalization and 
--                indentation.  The Ada code is provided 
--                as a pointer to a string, which is 
--                reformatted and returned as the same 
--                type (terminated by Character'First)
--                       
--  By: Tim Chamillard
--                                         
-- REFORMAT_PKG is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- REFORMAT_PKG is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------

with Lexer;
with Mssyst.String;
use Mssyst.String;


PACKAGE Reformat_Pkg IS

   SUBTYPE Reformat_String IS Lexer.StringPointer;

   -------------------------------------------------------------------------------
   --
   -- Procedure: Free
   -- Description: This procedure deallocates the space for a 
   --    Reformat_String
   -- Parameters:
   --    The_String : the string to deallocate
   ----------------------------------------------------------------------

   PROCEDURE Free ( The_String : IN OUT Reformat_String);

   ----------------------------------------------------------------------
   --
   -- Function: Reformat
   -- Description: This function performs both capitalization and 
   --    indentation and returns a null-terminated string
   --    For capitalization, reserved words and Identifiers are 
   --       reformatted in several ways:
   --       a.  changed to all upper case
   --       b.  changed to all lower case
   --       c.  changed to mixed case, characters at beginning of words
   --              capitalized, characters in the middle of words 
   --              unchanged
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
   --       See settings for more information
   -- Parameters:
   --    The_String    : the string to be reformatted
   --    Success       : at what level the reformat was completed
   --    Result        : the reformatted string
   -------------------------------------------------------------------------------

   PROCEDURE Reformat (The_String         : IN  Reformat_String;
                       Success            : OUT Boolean;
                       Result             : OUT Reformat_String;
                       Starting_Indent    : in  Natural := 0);
   function Reformat (
      File      : in MSSyst.String.Ref;
      StartLine : in Natural;
      EndLine   : in Natural) return MSSyst.String.Ref;
      
PRIVATE
   ---------------------------------------------------------------------
   --
   -- Procedure : Lookahead_Past_Comments
   -- Description : Get the next non-commented token
   --               Resets current line after looking ahead
   -- Parameters:
   --    Lookahead_Token      : next non-commented token
   --    Got_One              : True on success
   --    The_String           : what's being reformatted
   --    Current_Location     : where we are
   --
   ---------------------------------------------------------------------
   procedure Lookahead_Past_Comments(Lookahead_Token  : out Lexer.Token;
         Got_One          : out Boolean;
         The_String       : in  Reformat_String;
         Current_Location : in  Integer);
END Reformat_Pkg;
