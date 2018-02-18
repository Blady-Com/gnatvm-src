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
--
-- Modification log:
-- May    30, 2003 (alf) : modification to support rich edit 2.0
-- June   29, 1999 (mcc) : Fixed deletion of LF at end of file
-- May    24, 1999 (mcc) : Changed for version 2.0
-- August 25, 1997 (mcc) : Added Bold, Bold_Only
---------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with Lexer;
with Rtf;
with Writer;
--with Windows; -- for debugging
with Settings;
use type Settings.Colorize_Option;
use type Settings.Text_Case;

-- with and use of child package
-- uses to get "=" on TokenClass, Tokens
use type Lexer.Tokenclass;
use type Lexer.Tokens;

-- with Ada.Text_IO;

package body Reformat_Rtf is
   -- type to keep track of current color 
   type Colors is ( Reserved, Other, Comment, Const, Strg );

   -- constants for starting size of result
   Initial_Size       : constant Positive := 50000;

   ---------------------------------------------------------------------
   --
   -- Procedure: Free
   -- Description: This procedure deallocates the space for a
   --    Reformat_String
   -- Parameters:
   --    The_String : the string to deallocate
   ---------------------------------------------------------------------

   procedure Free ( The_String : in out Lexer.stringpointer ) is

      -- instantiation to free string memory space
      procedure Free_String is new Ada.Unchecked_Deallocation ( String,
         Lexer.stringpointer );

   begin   -- Free

      Free_String (The_String);

   end Free;


   ------------------------------------------------------------------
   --
   -- Procedure: Initialize
   -- Description: Provides initial values for flags
   -- Parameters:
   --    Current_Color       : color of characters
   --    Current_Line        : line in input string
   --    Current_Indentation : number of spaces to indent
   --    Last_Char           : last character location in new string
   --    Start_For_Next_Token         : start location of token
   --    Last_Token_End      : end location of previous token
   --    Previous_Token_Type : type of the previous token
   ------------------------------------------------------------------

   procedure Initialize ( Current_Color : out Colors;
         Last_Char             : out Natural;
         Start_For_Next_Token  : out Natural;
         Last_Token_End        : out Natural;
         Bad_Token_Starts_Line : out Boolean ) is

   begin   -- Initialize

      --    Set current color to other
      Current_Color := Other;

      --    Set last character to 1
      Last_Char := 1;

      --    Set Start_For_Next_Token to 1
      Start_For_Next_Token := 1;

      --    Set Last_Token_End to 0
      Last_Token_End := 0;

      --    Set bad token at start of line to false
      Bad_Token_Starts_Line := False;

   end Initialize;

   ---------------------------------------------------------------------
   --
   -- Procedure: Color
   -- Description: Sets color for following token if token color
   --    should be different from current color.  If color is
   --    changed, Current_Color is set to the new color.
   -- Algorithm:
   --    If Current_Token is a reserved word and Current_Color is not
   --       Reserved
   --       Add RTF.reserved_color to new string
   --       Change Current_Color to Reserved
   --    Elsif Current_Token is a constant and Current_Color is not
   --       Const
   --       Add RTF.constant_color to new string
   --       Change Current_Color to Const
   --    Elsif Current_Token is a comment and Current_Color is not
   --       Comment
   --       Add RTF.comment_color to new string
   --       Change Current_Color to Comment
   --    Elsif Current_Token is a string constant and Current_Color
   --       is not Strg
   --       Add RTF.string_color to new string
   --       Change Current_Color to Strg
   --    Elsif Current_Token is an other class or an identifier
   --       class and Current_Color is not Other
   --       Add RTF.other_color to new string
   --       Change Current_Color to Other
   -- Parameters:
   --    New_String    : the new string to which to add colors
   --    Last_Char     : location of the last character in New_String
   --    Current_Color : the current color of characters
   --    Current_Token : the current token
   ---------------------------------------------------------------------

   procedure Color ( New_String : in out Lexer.stringpointer;
         Last_Char     : in out Natural;
         Current_Color : in out Colors;
         Current_Token : in     Lexer.Token ) is

      Token_Class : Lexer.Tokenclass;

   begin   -- Color

      Token_Class := Lexer.Gettokenclass ( Item => Current_Token );

      --    If Current_Token is a reserved word and Current_Color is not
      --       Reserved
      if ( Token_Class = Lexer.Reservedclass ) and then
            ( Current_Color /= Reserved ) then

         --       Add RTF.reserved_color to new string
         Writer.Write(
            Item  => Rtf.Reserved_Color,
            Into  => New_String,
            Index => Last_Char);

         --       Change Current_Color to Reserved
         Current_Color := Reserved;

         --    Elsif Current_Token is a constant and Current_Color is not
         --       Const
      elsif ( Token_Class = Lexer.Constantclass ) and then
            ( Current_Color /= Const ) then

         --       Add RTF.constant_color to new string
         Writer.Write(
            Item  => Rtf.Constant_Color,
            Into  => New_String,
            Index => Last_Char);

         --       Change Current_Color to Const
         Current_Color := Const;

         --    Elsif Current_Token is a comment and Current_Color is not
         --       Comment
      elsif ( Token_Class = Lexer.Commentclass ) and then
            ( Current_Color /= Comment ) then

         --       Add RTF.comment_color to new string
         Writer.Write(
            Item  => Rtf.Comment_Color,
            Into  => New_String,
            Index => Last_Char);

         --       Change Current_Color to Comment
         Current_Color := Comment;

         --    Elsif Current_Token is a string constant and Current_Color
         --       is not Strg
      elsif ( Token_Class = Lexer.Stringclass ) and then
            ( Current_Color /= Strg ) then

         --       Add RTF.string_color to new string
         Writer.Write(
            Item  => Rtf.String_Color,
            Into  => New_String,
            Index => Last_Char);

         --       Change Current_Color to Strg
         Current_Color := Strg;

         --    Elsif Current_Token is an other class or an identifier
         --       class and Current_Color is not Other
      elsif ( ( Token_Class = Lexer.Otherclass ) or
            ( Token_Class = Lexer.Identifierclass ) ) and then
            ( Current_Color /= Other ) then

         --       Add RTF.other_color to new string
         Writer.Write(
            Item  => Rtf.Other_Color,
            Into  => New_String,
            Index => Last_Char);

         --       Change Current_Color to Other
         Current_Color := Other;

      end if;

   end Color;

   procedure Bold ( New_String : in out Lexer.stringpointer;
         Last_Char     : in out Natural;
         Current_Color : in out Colors;
         Current_Token : in     Lexer.Token ) is

      Token_Class : Lexer.Tokenclass;

   begin   -- Bold

      Token_Class := Lexer.Gettokenclass ( Item => Current_Token );

      --    If Current_Token is a reserved word and Current_Color is not
      --       Reserved
      if ( Token_Class = Lexer.Reservedclass ) and then
            ( Current_Color /= Reserved ) then

         --       Add RTF.reserved_color to new string
         Writer.Write(
            Item  => Rtf.Bold_Color,
            Into  => New_String,
            Index => Last_Char);

         --       Change Current_Color to Reserved
         Current_Color := Reserved;

      elsif ( Token_Class /= Lexer.Reservedclass ) and then
            ( Current_Color /= Other ) then

         --       Add RTF.Unbold_color to new string
         Writer.Write(
            Item  => Rtf.Unbold_Color,
            Into  => New_String,
            Index => Last_Char);

         --       Change Current_Color to Other
         Current_Color := Other;

      end if;

   end Bold;

   -------------------------------------------------------------------------------
   --
   -- Procedure: Add_Slash
   -- Description: This procedure adds an additional '/' character
   --    preceding '/', '{', and '}' in the input string
   -- Parameters: 
   --    Current_Character : character to check for /, {, }
   --    New_String        : the new string
   --    Last_Char         : the last character in the new string
   --    Color_Option      : whether or not to colorize
   ---------------------------------------------------------------------

   procedure Add_Slash ( Current_Character : in Character;
         New_String   : in out Lexer.stringpointer;
         Last_Char    : in out Natural;
         Color_Option : in     Settings.Colorize_Option ) is

   begin   -- Add_Slash

      --                IF character is a '\', '{', or '}',
      --                precede with a '\'
      if ( Color_Option /= Settings.No_Colorize ) and then
            ( ( Current_Character = '\' ) or
            ( Current_Character = '{' ) or
            ( Current_Character = '}' ) ) then
         Writer.Write(
            Item => "\",
            Into => New_String,
            Index => Last_Char);
      end if;

   end Add_Slash;

   ------------------------
   -- PROCEDURE Add_Footer 
   --
   -- Check and add footer if necessary
   ------------------------
   procedure Add_Footer(Color_Option : in Settings.Colorize_Option;
         Last_Char  : in out Integer;
         New_String : in out Lexer.stringpointer) is
   begin
      --    If Color_Option is not No_Color
      if ( Color_Option /= Settings.No_Colorize ) then
         -- Add RTF.Footer to new string
         Writer.Write(
            Item => RTF.Footer,
            Into => New_String,
            Index => Last_Char);
      end if;
   end Add_Footer;

   ------------------------
   -- PROCEDURE Add_Header 
   --
   -- Check and add header if necessary
   ------------------------
   procedure Add_Header(Color_Option : in Settings.Colorize_Option;
         Last_Char  : in out Integer;
         New_String : in out Lexer.stringpointer) is
   begin
      --    If Color_Option is not No_Color
      if ( Color_Option /= Settings.No_Colorize ) then

         --       Add RTF.Header to new string
         Writer.Write(
            Item => RTF.Header,
            Into => New_String,
            Index => Last_Char);

         --       Add other_color string to new string
         Writer.Write(
            Item => RTF.Other_Color,
            Into => New_String,
            Index => Last_Char);

         --       Change font size
         Writer.Write(
            Item => RTF.Font_Size,
            Into => New_String,
            Index => Last_Char);
      end if;
   end Add_Header;

   ----------------------------------------
   -- PROCEDURE Copy_To_New_String
   --
   -- copy from Starting to Ending into
   -- new string, adding slash as needed
   ----------------------------------------
   procedure Copy_To_New_String(Starting : in Integer;
         Ending       : in Integer;
         The_String   : in Lexer.stringpointer;
         New_String   : in out lexer.stringpointer;
         Last_Char    : in out Integer;
         Color_Option : in Settings.Colorize_Option) is
   begin
      --          Copy into new string
      for Next_Char in Starting..Ending loop
         --                Check for adding a slash
         Add_Slash ( Current_Character => The_String(Next_Char),
            New_String   => New_String,
            Last_Char    => Last_Char,
            Color_Option => Color_Option );

         Writer.Write(
            Item => The_String(Next_Char) & "",
            Into => New_String,
            Index => Last_Char);

         -- if character is a LF add RTF LF if not no_colorize  
         if (The_String(Next_Char) = Settings.New_Line_Char ) and then
               ( Color_Option /= Settings.No_Colorize ) then
            Writer.Write(
               Item  => RTF.LF,
               Into  => New_String,
               Index => Last_Char);
         end if;
--         if (The_String(Next_Char) = Ascii.cr ) and then
--               ( Color_Option /= Settings.No_Colorize ) then
--            Writer.Write(
--               Item  => Ascii.CR & RTF.LF,
--               Into  => New_String,
--               Index => Last_Char);
--         elsif (The_String(Next_Char) = Ascii.LF ) and then
--               ( Color_Option /= Settings.No_Colorize ) then
--            null;
--         end if;


      end loop;
   end Copy_To_New_String;

   -------------------------------------------------------------------------------
   --
   -- Function: Reformat
   -- Description: This function performs RTF
   -------------------------------------------------------------------------------

   procedure Reformat (The_String    : in  Lexer.stringpointer;
         Result             : out Lexer.stringpointer) is

      -- variable to keep track of current color
      Current_Color         : Colors;

      New_String            : Lexer.stringpointer;

      -- token variables
      Current_Token         : Lexer.Token;
      Start_For_Next_Token  : Natural;
      Token_Start           : Natural;
      Token_End             : Natural;
      Got_Token             : Boolean;
      End_Of_Input          : Boolean;
      Current_Token_Last    : Natural;
      Token_Line            : Natural;
      Token_Class           : Lexer.Tokenclass;

      -- variable to keep track of where tokens are located     
      Last_Token_End        : Natural;

      -- variable to keep track of current character in new string
      Last_Char             : Natural;


      -- variables to keep track of parenthesis depth and whether
      -- or not we're processing a declaration
      
      -- variables to keep track of processing status
      Token_Type : Lexer.Tokens;
      Bad_Token_Starts_Line : Boolean;

   begin   -- Reformat

      --    Allocate space for new string
      New_String := new String (1 .. Initial_Size);

      --    Initialize the lexer
      Lexer.Initialize;

      --    Initialize flags
      Initialize ( Current_Color => Current_Color,
         Last_Char             => Last_Char,
         Start_For_Next_Token  => Start_For_Next_Token,
         Last_Token_End        => Last_Token_End,
         Bad_Token_Starts_Line => Bad_Token_Starts_Line );

      Add_Header(Color_Option => Settings.Colorization,
         Last_Char  => Last_Char,
         New_String => New_String);


      loop
         --       Get the next token in the string
         Lexer.Getnexttoken ( Str     => The_String,
            First   => Start_For_Next_Token,
            Last    => Current_Token_Last,
            Result  => Current_Token,
            Haveone => Got_Token,
            Done    => End_Of_Input );

         if Got_Token then
            Token_Start := Lexer.Getstart (Item => Current_Token);
            Token_End := Lexer.Getend (Item => Current_Token);
            Token_Line := Lexer.Getline (Item => Current_Token );
            Token_Type := Lexer.Gettokentype (Item => Current_Token);
         else
            Token_Start := Start_For_Next_Token;
            Token_End := Current_Token_Last;
            Token_Line := Lexer.Getcurrentline;
            Token_Type := Lexer.Comment_T;
         end if;

         --    stop when at end of input string
         exit when End_Of_Input;

         --       Set Start_For_Next_Token to Current_Token_Last
         Start_For_Next_Token := Current_Token_Last+1;


--    Windows.Ok_Box("Copy1: " & 
--       integer'image(last_token_end+1) &
--       integer'image(token_start-1) & '"' &
--       the_string(last_token_end+1..token_start-1) & '"');
       
            --          Add characters between last token and this one
            Copy_To_New_String(Starting => Last_Token_End + 1,
               Ending       => Token_Start - 1,
               The_String   => The_String,
               New_String   => New_String,
               Last_Char    => Last_Char,
               Color_Option => Settings.Colorization);
         -------------------------------------        

         --          Set last token end to end of current token
         if Got_Token then
            Last_Token_End := Lexer.Getend (Item => Current_Token);
         else
            Last_Token_End := Token_End;
         end if;

         -------------------------------------        

         --       Colorize based on color option
         if Got_Token then
            if ( Settings.Colorization = Settings.Colorize or else
                  Settings.Colorization = Settings.Colorize_Only) then
               Color ( New_String    => New_String,
                  Last_Char     => Last_Char,
                  Current_Color => Current_Color,
                  Current_Token => Current_Token );
            elsif (Settings.Colorization = Settings.Bold or else
                  Settings.Colorization = Settings.Bold_Only) then
               Bold ( New_String    => New_String,
                  Last_Char     => Last_Char,
                  Current_Color => Current_Color,
                  Current_Token => Current_Token );
            end if;
         end if;

         -------------------------------------        
         --       Output token
         if Got_Token then
            Token_Class := Lexer.Gettokenclass ( Item => Current_Token );
               Copy_To_New_String(Starting => Lexer.Getstart (Item => Current_Token),
                  Ending       => Lexer.Getend (Item => Current_Token),
                  The_String   => The_String,
                  New_String   => New_String,
                  Last_Char    => Last_Char,
                  Color_Option => Settings.Colorization);
--    Windows.Ok_Box("Copy2: " & 
--       integer'image(lexer.getstart(current_token)) &
--       integer'image(lexer.getend(current_token)) & '"' &
--       the_string(lexer.getstart(current_token)..lexer.getend(current_token)) & '"');
        else
            -- bad token, so set to other
            Token_Class := Lexer.Otherclass;
            -- there is a special case for a "bad" token at the start
            -- of a line - in this case, we've already copied the string
            -- representing the bad token, so we don't want to copy it
            -- again here
            if Bad_Token_Starts_Line then
               -- simply reset the flag
               Bad_Token_Starts_Line := False;

            else
               Copy_To_New_String(Starting => Token_Start,
                  Ending       => Token_End,
                  The_String   => The_String,
                  New_String   => New_String,
                  Last_Char    => Last_Char,
                  Color_Option => Settings.Colorization);
            end if;   -- check of bad token at start of line flag
         end if;
      end loop;

-- windows.ok_box(Integer'Image (token_end), "reformat_rtf");
      if Got_Token and then The_String (Token_End) = Ascii.Nul
      then -- In rich edit the end of the text is marked with Ascii.Nul (C "\0"), but behind this there is some waste in the array
        Copy_To_New_String(Starting => Last_Token_End + 1,
                          Ending       => Token_End - 1,
                          The_String   => The_String,
                          New_String   => New_String,
                          Last_Char    => Last_Char,
                          Color_Option => Settings.Colorization);
      else   
           --    Add any remaining characters to new string
        Copy_To_New_String(Starting => Token_End,
           -- this is kind of confusing, but on EOF,
           -- Token_End points to first character that was
           -- searched where no token was found
           Ending       => The_String'Length-1, -- ends in Character'first
           The_String   => The_String,
           New_String   => New_String,
           Last_Char    => Last_Char,
           Color_Option => Settings.Colorization);
      end if;
      
      Add_Footer(Color_Option => Settings.Colorization,
         Last_Char  => Last_Char,
         New_String => New_String);

      -- copy new string into exact size string
      Result := new String (1..Last_Char-1);
      for Index in 1 .. Last_Char-1 loop
         Result(Index) := New_String(Index);
      end loop;
      Free (New_String);

   end Reformat;

end Reformat_Rtf;
