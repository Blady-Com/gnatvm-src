---------------------------------------------------------------
--                                                           
--  ADA GNAT INTEGRATED DEVELOPMENT ENVIRONMENT (AdaGIDE)    
--                                                           
--  RTF.ADS 
--  Description : provides constants describing the
--                Rich Text Format (RTF)
--
--  By: Dr. Martin C. Carlisle
--                                         
-- RTF is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- RTF is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------
--  3-Feb-2002 (GdM): Header as function, to match Options.Colors

PACKAGE RTF IS
    -- removed \deflang1033 to fix foreign keyboard problem
    
    function Header return String;

    reserved_color : CONSTANT String := "\cf4 ";
    other_color : CONSTANT String := "\cf0 ";
    constant_color : CONSTANT String := "\cf2 ";
    comment_color : CONSTANT String := "\cf1 ";
    string_color : CONSTANT String := "\cf3 ";
    bold_color : CONSTANT String := "\b ";
    unbold_color : CONSTANT String := "\plain";
    LF : CONSTANT String := "\par "; -- put AFTER Line Feed
    Footer : constant String := "\par }" & Character'First;
    PROCEDURE Set_Font_Size(X : String);
    function Font_Size return String;
    -- in 1/2 points, put after
    -- both unbold_color and Header
END RTF;
