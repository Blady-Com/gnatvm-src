---------------------------------------------------------------
--                                                           
--  ADA GNAT INTEGRATED DEVELOPMENT ENVIRONMENT (AdaGIDE)    
--                                                           
--  REFORMAT_RTF.ADB 
--  Description : This package provides a number of 
--                functions to reformat Ada code to 
--                use the proper capitalization and 
--                indentation.  The Ada code is provided 
--                as a pointer to a string, which is 
--                reformatted and returned as the same 
--                type (terminated by Character'First if RTF)
--                       
--  By: Tim Chamillard and Martin Carlisle
--                                         
-- REFORMAT_RTF is free software; you can redistribute it and/or
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly
-- indicate if it has been modified.
--
-- REFORMAT_RTF is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--                                         
---------------------------------------------------------------
with Lexer;
package Reformat_Rtf is
   -------------------------------------------------------------------------------
   --
   -- Function: Reformat
   -- Description: This function performs RTF
   -------------------------------------------------------------------------------

   procedure Reformat (The_String    : in  Lexer.Stringpointer;
         Result             : out Lexer.Stringpointer);

end Reformat_Rtf;
