-----------------------------------------------------------------------
-- parm_modes.adb
--
-- Author: Robert A. French
-- E-mail: rfrench99@hotmail.com
--
-- Description:
-- This provides a type and procedure for reading possible parameter
-- modes from a parameter declaration and returning the given modes.
-- This aids in vertical alignment of parameter modes.
-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- PARM_MODES
-----------------------------------------------------------------------
package body Parm_Modes is
   --------------------------------------------------------------------
   -- GET
   --------------------------------------------------------------------
   procedure Get (
      Value :    out Mode;
      From  : in     Lexer.StringPointer;
      First : in out Positive
     ) is

      The_Token : Lexer.Token;
      Last      : Positive;
      Have      : Boolean  := True;
      Done      : Boolean  := False;
      Kind      : Lexer.Tokens;
      --| These variables are all used while retrieving a token. Most
      --| aren't really important to this package.

      use type Lexer.Tokens;
      Remember_Line : Natural;
   begin  -- Get
      -- Get the first token.
      Remember_Line := Lexer.Getcurrentline;
      Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
      if Have then
         Kind := Lexer.GetTokenType(The_Token);

         -- It can be IN, OUT, or neither.
         if Kind = Lexer.In_T then
            First := Last + 1;
            Remember_Line := Lexer.Getcurrentline;
            -- Get the second token.
            Lexer.GetNextToken(From, First, Last, The_Token, Have,
                               Done);
            if Have then
               Kind := Lexer.GetTokenType(The_Token);

               -- It can be OUT or something else.
               if Kind = Lexer.Out_T then
                  First := Last + 1;
                  Value := In_Out_Mode;
               else
                  Lexer.Initialize(Remember_Line);
                  Value := In_Mode;
               end if;

            else
               Value := In_Mode;
            end if;

         elsif Kind = Lexer.Out_T then
            First := Last + 1;
            Value := Out_Mode;
         elsif Kind = Lexer.Access_T then
            First := Last + 1;
            Value := Access_Mode;
         else
            Lexer.Initialize(Remember_Line);
            Value := No_Mode;
         end if;

      else
         Value := No_Mode;
      end if;
   end Get;
   --------------------------------------------------------------------
end Parm_Modes;
-----------------------------------------------------------------------
