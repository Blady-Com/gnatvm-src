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
--
--  LEXER.ADB
--  Description : parses a string into Ada tokens
--
--  By: Dr. Martin C. Carlisle
--
---------------------------------------------------------------

with Ada.Long_Long_Integer_Text_IO;
with Ada.Long_Long_Float_Text_IO;
with Ada.Characters.Handling;
with Ada.Exceptions;

package body Lexer is

   NameLength : constant Integer := 200; --  see LRM 2.2 (15)
   subtype NameType is String (1 .. NameLength);

   --  Following is an array of the string corresponding to each reserved word
   type ImageArray is array (ReservedWords) of String (1 .. 12);
   Images : ImageArray :=
              (abort_t        => "ABORT       ",
               abs_t          => "ABS         ",
               abstract_t     => "ABSTRACT    ",
               accept_t       => "ACCEPT      ",
               access_t       => "ACCESS      ",
               aliased_t      => "ALIASED     ",
               all_t          => "ALL         ",
               and_t          => "AND         ",
               array_t        => "ARRAY       ",
               at_t           => "AT          ",
               begin_t        => "BEGIN       ",
               body_t         => "BODY        ",
               case_t         => "CASE        ",
               constant_t     => "CONSTANT    ",
               declare_t      => "DECLARE     ",
               delay_t        => "DELAY       ",
               delta_t        => "DELTA       ",
               digits_t       => "DIGITS      ",
               do_t           => "DO          ",
               else_t         => "ELSE        ",
               elsif_t        => "ELSIF       ",
               end_t          => "END         ",
               entry_t        => "ENTRY       ",
               exception_t    => "EXCEPTION   ",
               exit_t         => "EXIT        ",
               for_t          => "FOR         ",
               function_t     => "FUNCTION    ",
               generic_t      => "GENERIC     ",
               goto_t         => "GOTO        ",
               if_t           => "IF          ",
               in_t           => "IN          ",
               is_t           => "IS          ",
               limited_t      => "LIMITED     ",
               loop_t         => "LOOP        ",
               mod_t          => "MOD         ",
               new_t          => "NEW         ",
               not_t          => "NOT         ",
               null_t         => "NULL        ",
               of_t           => "OF          ",
               or_t           => "OR          ",
               others_t       => "OTHERS      ",
               out_t          => "OUT         ",
               package_t      => "PACKAGE     ",
               pragma_t       => "PRAGMA      ",
               private_t      => "PRIVATE     ",
               procedure_t    => "PROCEDURE   ",
               protected_t    => "PROTECTED   ",
               raise_t        => "RAISE       ",
               range_t        => "RANGE       ",
               record_t       => "RECORD      ",
               rem_t          => "REM         ",
               renames_t      => "RENAMES     ",
               requeue_t      => "REQUEUE     ",
               return_t       => "RETURN      ",
               reverse_t      => "REVERSE     ",
               select_t       => "SELECT      ",
               separate_t     => "SEPARATE    ",
               subtype_t      => "SUBTYPE     ",
               tagged_t       => "TAGGED      ",
               task_t         => "TASK        ",
               terminate_t    => "TERMINATE   ",
               then_t         => "THEN        ",
               type_t         => "TYPE        ",
               until_t        => "UNTIL       ",
               use_t          => "USE         ",
               when_t         => "WHEN        ",
               While_T        => "WHILE       ",
               With_T         => "WITH        ",
               Xor_T          => "XOR         ",
               Interface_T    => "INTERFACE   ",
               Overriding_T   => "OVERRIDING  ",
               Synchronized_T => "SYNCHRONIZED");

   --  This is an array of the lengths of each reserved word
   ImageLengthArray : array (ReservedWords) of Natural :=
                        (abort_t     => 5,
                         abs_t       => 3,
                         abstract_t  => 8,
                         accept_t    => 6,
                         access_t    => 6,
                         aliased_t   => 7,
                         all_t       => 3,
                         and_t       => 3,
                         array_t     => 5,
                         at_t        => 2,
                         begin_t     => 5,
                         body_t      => 4,
                         case_t      => 4,
                         constant_t  => 8,
                         declare_t   => 7,
                         delay_t     => 5,
                         delta_t     => 5,
                         digits_t    => 6,
                         do_t        => 2,
                         else_t      => 4,
                         elsif_t     => 5,
                         end_t       => 3,
                         entry_t     => 5,
                         exception_t => 9,
                         exit_t      => 4,
                         for_t       => 3,
                         function_t  => 8,
                         generic_t   => 7,
                         goto_t      => 4,
                         if_t        => 2,
                         in_t        => 2,
                         is_t        => 2,
                         limited_t   => 7,
                         loop_t      => 4,
                         mod_t       => 3,
                         new_t       => 3,
                         not_t       => 3,
                         null_t      => 4,
                         of_t        => 2,
                         or_t        => 2,
                         others_t    => 6,
                         out_t       => 3,
                         package_t   => 7,
                         pragma_t    => 6,
                         private_t   => 7,
                         procedure_t => 9,
                         protected_t => 9,
                         raise_t     => 5,
                         range_t     => 5,
                         record_t    => 6,
                         rem_t       => 3,
                         renames_t   => 7,
                         requeue_t   => 7,
                         return_t    => 6,
                         reverse_t   => 7,
                         select_t    => 6,
                         separate_t  => 8,
                         subtype_t   => 7,
                         tagged_t    => 6,
                         task_t      => 4,
                         terminate_t => 9,
                         then_t      => 4,
                         type_t      => 4,
                         until_t     => 5,
                         use_t       => 3,
                         when_t      => 4,
                         While_T     => 5,
                         With_T      => 4,
                         Xor_T       => 3,
                         interface_t    => 9,
                         overriding_t   => 10,
                         synchronized_t => 12);

   --  This is an array of strings corresponding to each compound delimiter
   ImageCompoundDelimiter : array (CompoundDelimiters) of String (1 .. 2) :=
                              (arrow_t               => "=>",
                               double_dot_t          => "..",
                               double_star_t         => "**",
                               assignment_t          => ":=",
                               noteq_t               => "/=",
                               geq_t                 => ">=",
                               leq_t                 => "<=",
                               left_label_bracket_t  => "<<",
                               right_label_bracket_t => ">>",
                               box_t                 => "<>");

   --  This is an array of characters corresponding to each delimiter
   ImageDelimiter : array (Delimiters) of String (1 .. 1) :=
                      (ampersand_t => "&",
                       tick_t      => "'",
                       lparen_t    => "(",
                       rparen_t    => ")",
                       times_t     => "*",
                       plus_t      => "+",
                       comma_t     => ",",
                       minus_t     => "-",
                       dot_t       => ".",
                       divide_t    => "/",
                       colon_t     => ":",
                       semicolon_t => ";",
                       lt_t        => "<",
                       eq_t        => "=",
                       gt_t        => ">",
                       pipe_t      => "|");

   type LookaheadArray is array (1 .. 3) of Character;
   LookAhead : LookaheadArray; --  lookahead for GetNextChar
   HaveLookAhead : Integer := 0;

   InputStringLocation : Integer := 0; --  where are we in string?
   InputString : StringPointer;

   EndOfString : exception; --  attempted to get token past end of string
   SecondUnget : exception; --  can't do two ungets in a row

   --  used in case we need to unget a CR.
   EndOfPreviousLine : Natural;

   type Position_Flashback is record
      Column : Natural := 1;
      Line   : Natural := 1;
   end record;

   --  One token of lookahead
   Number_Token_Lookaheads : constant := 2;
   Old_Positions           : array (1 .. Number_Token_Lookaheads + 1) of
                               Position_Flashback;
   TokenAhead              : array (1 .. Number_Token_Lookaheads) of Token;
   HaveTokenAhead          : Natural := 0;

   --  keep track of where we are in the file
   CurrentCol  : Natural := 1;
   CurrentLine : Natural := 1;

   --------------------
   -- GetCurrentLine --
   --------------------

   function GetCurrentLine return Natural is
   begin
      if HaveTokenAhead = 0 then
         return CurrentLine;
      else
         return Old_Positions
           (Number_Token_Lookaheads + 1 - HaveTokenAhead).Line;
      end if;
   end GetCurrentLine;

   ------------------------------------------
   --  PROCEDURE Initialize
   --
   --  Sets Line/Col to values or 1,1 (default)
   ------------------------------------------
   procedure Initialize (Line : in Integer := 1;
                         Col  : in Integer := 1)
   is
   begin
      CurrentCol := Col;
      CurrentLine := Line;
   end Initialize;

   ---------------------------
   --  FUNCTION StringCompare
   --
   --  Inputs: two strings
   --  Outputs: TRUE if two strings are equal (ignore capitalization)
   --
   ---------------------------
   function StringCompare (Left, Right : String) return Boolean is
   begin
      if Left'Length /= Right'Length then
         return False;
      end if;

      for Index in Left'Range loop
         if Ada.Characters.Handling.To_Upper (Left (Index)) /=
           Ada.Characters.Handling.To_Upper
             (Right (Index - Left'First + Right'First))
         then
            return False;
         end if;
      end loop;

      return True;
   end StringCompare;

   --------------------------
   --  FUNCTION GetNextChar
   --
   --  returns next character from input stream or
   --  Standard.ASCII.LF on new line
   --
   --  updates position in file
   --------------------------
   function GetNextChar return Character is
   begin
      --  update string location
      InputStringLocation := InputStringLocation + 1;

      if HaveLookAhead > 0 then
         if LookAhead (HaveLookAhead) = Standard.ASCII.LF then
            CurrentLine := CurrentLine + 1;
            CurrentCol := 1;
         else
            CurrentCol := CurrentCol + 1;
         end if;

         HaveLookAhead := HaveLookAhead - 1;

         return LookAhead (HaveLookAhead + 1);
      else

         CurrentCol := CurrentCol + 1;

         if InputStringLocation - 1 > InputString'Last + 1 then
            raise EndOfString;
         elsif InputStringLocation - 1 = InputString'Last + 1
           or else
             ((InputString (InputStringLocation - 1) = Standard.ASCII.CR)
              and then
                (InputString (InputStringLocation) /= Standard.ASCII.LF))
           or else InputString (InputStringLocation - 1) = Standard.ASCII.LF
         then

            EndOfPreviousLine := CurrentCol - 1;
            CurrentLine := CurrentLine + 1;
            CurrentCol := 1;
            return Standard.ASCII.LF;

         else
            --  already incremented
            return InputString (InputStringLocation - 1);
         end if;
      end if;
   end GetNextChar;

   -------------------------
   --  PROCEDURE UngetChar
   --
   --  undoes last GetNextChar
   --  may not be called twice in succession or
   --  raises SecondUngetChar exception
   -------------------------
   procedure UngetChar (Item : Character) is
   begin
      --  update string location
      InputStringLocation := InputStringLocation - 1;

      if HaveLookAhead = LookAhead'Last then
         raise SecondUnget;
      else
         if Item = Standard.ASCII.LF then
            CurrentLine := CurrentLine - 1;
            CurrentCol := EndOfPreviousLine;
         else
            CurrentCol := CurrentCol - 1;
         end if;

         HaveLookAhead := HaveLookAhead + 1;
         LookAhead (HaveLookAhead) := Item;
      end if;
   end UngetChar;

   -------------------------
   --  PROCEDURE Get_Line
   --
   --  does same thing as Ada.Text_IO.Get_Line, but from correct stream
   -------------------------
   procedure Get_Line (Item : out String; Last : out Natural) is
      Start, J  : Integer;
      NextChar  : Character;
   begin
      Start := InputStringLocation;

      loop
         NextChar  := GetNextChar;
         if NextChar = Standard.ASCII.NUL then
            UngetChar (NextChar);
            exit;
         elsif NextChar = Standard.ASCII.LF then
            exit;
         end if;
      end loop;

      J := Item'First;

      if NextChar = ASCII.LF then
         for I in Start .. InputStringLocation - 2 loop
            Item (J) := InputString (I);
            J := J + 1;
         end loop;

         Last := Item'First + InputStringLocation - Start - 1;

      elsif NextChar = ASCII.CR then
         for I in Start .. InputStringLocation - 1 loop
            Item (J) := InputString (I);
            J := J + 1;
         end loop;

         Last := Item'First + InputStringLocation - Start;

      elsif NextChar = ASCII.NUL then
         for I in Start .. InputStringLocation loop
            Item (J) := InputString (I);
            J := J + 1;
         end loop;

         Last := Item'First + InputStringLocation - Start + 1;
      end if;
   end Get_Line;

   TwoUnderscores : exception;

   --------------------------
   --  FUNCTION GetComment
   --
   --  gets rest of line and returns comment token
   --  updates Current line and column
   --
   --  Inputs: none
   --  Output: token pointer to comment token
   --
   --  algorithm:
   --  1) read rest of line
   --  2) create CommentToken
   --  3) return
   --------------------------
   function GetComment return Token is
      comment         : NameType;
      length          : Integer;
      startcol, start : Integer;
   begin
      start := InputStringLocation - 2;
      startcol := CurrentCol - 2;
      Get_Line (Item => comment, Last => length);
      CurrentCol := 1;
      return Token'(token_type => comment_t,
                    line       => CurrentLine - 1,
                    startcol   => startcol,
                    startloc   => start,
                    endloc     => start + length);
   end GetComment;

   --------------------------
   --  PROCEDURE GetStringConstant
   --
   --  Inputs:  first character of string constant '"'
   --  Outputs: string literal and its length
   --
   --  According to LRM, a string literal can consist of characters and ""
   --  until a single "
   --
   --  Algorithm:
   --------------------------
   procedure GetStringConstant (FirstChar : in     Character;
                                Name      :    out String;
                                Length    :    out Positive) is

      NextChar : Character;
      IllegalString : exception;
   begin
      Name (Name'First) := FirstChar;
      NextChar := GetNextChar;
      Length := 2;

      loop
         while NextChar /= '"' loop
            if NextChar = Standard.ASCII.LF
              or else NextChar = Standard.ASCII.NUL
            then
               UngetChar (NextChar);

               raise IllegalString;
            end if;

            Name (Length) := NextChar;
            Length := Length + 1;
            NextChar := GetNextChar;
         end loop;

         Name (Length) := '"';
         Length := Length + 1;
         NextChar := GetNextChar;
         exit when NextChar /= '"';
         NextChar := GetNextChar;
         Length := Length + 1;
      end loop;

      --  we got one more character than we need
      UngetChar (NextChar);
      Length := Length - 1;

   end GetStringConstant;

   --------------------------
   --  PROCEDURE GetIdentifier
   --
   --  Inputs:  first character of identifier
   --  Outputs: identifier and its length
   --
   --  According to LRM, an identifier may consist of a letter followed by
   --  a sequence of letters, digits and underscores.  (However, there may not
   --  be two adjacent underscores).  For AdaGIDE, we overlook this exception.
   --
   --  Algorithm:  this procedure loops until it finds the end of the
   --              identifier, and returns the identifier and its length
   --  exceptions: TwoUnderscores
   --
   --  For AdaGIDE only, in an exception to the LRM, we count Ada.Text_IO
   --  as a single identifier.
   --------------------------
   procedure GetIdentifier (FirstChar : in     Character;
                            Name      :    out String;
                            Length    :    out Positive) is

      NextChar : Character;
      WasUnderscore : Boolean := False;

   begin
      NextChar := FirstChar;
      Length := 1;

      while Ada.Characters.Handling.Is_Letter (NextChar)
        or else NextChar = '_'
        or else NextChar = '.'
        or else Ada.Characters.Handling.Is_Digit (NextChar)
      loop
         WasUnderscore := (NextChar = '_');
         Name (Length) := NextChar;
         Length := Length + 1;
         NextChar := GetNextChar;
      end loop;

      --  we got one more character than we need
      UngetChar (NextChar);
      Length := Length - 1;
   end GetIdentifier;

   ErrorInNumber : exception;
   ------------------------------------------
   --  GetNumber
   --
   --  reads either an integer constant or floating point
   --  constant
   --
   --  inputs: first digit
   --  output: either integer or floating result and
   --         boolean saying whether a decimal point was found
   --
   --  algorithm:
   --  1) get integer part
   --  2) check for decimal point
   --   a) if not, check for #
   --      if #, then read through to first blank, checking for .
   --        if . found, use float_text_io.get o/w integer_text_io.get
   --        trap exception and report error
   --      else
   --        compute IntValue
   --   b) else, ensure one digit
   --     2) get decimal part
   --     3) check for exponent
   --       a) if present
   --         1) get sign if present and ensure one digit
   --         2) get rest of exponent
   --     4) compute FloatResult
   --
   --  exceptions: ErrorInNumber
   ------------------------------------------
   procedure GetNumber
     (FirstDigit  : in     Character;
      IntResult   :    out Long_Long_Integer;
      FloatResult :    out Long_Long_Float;
      HasDecimal  :    out Boolean;
      endloc      :    out Positive)
   is
      NumString : NameType;
      NextChar  : Character;
      Counter   : Integer;
      Last      : Integer;

      procedure GetExponent is
      begin
         if NextChar = 'E' or else NextChar = 'e' then
            NumString (Counter) := 'E';
            Counter := Counter + 1;
            NextChar := GetNextChar;

            if NextChar = '+' or else NextChar = '-' then
               NumString (Counter) := NextChar;
               Counter := Counter + 1;
               NextChar := GetNextChar;
            end if;

            if not Ada.Characters.Handling.Is_Digit (NextChar) then
               UngetChar (NextChar);
               Ada.Exceptions.Raise_Exception
                 (ErrorInNumber'Identity,
                  "Must have at least one in exponent");
            end if;

            while Ada.Characters.Handling.Is_Digit (NextChar)
              or else NextChar = '_'
            loop
               NumString (Counter) := NextChar;
               Counter := Counter + 1;
               NextChar := GetNextChar;
            end loop;
         end if;
      end GetExponent;

   begin
      HasDecimal := False;
      NextChar := FirstDigit;
      Counter := 1;
      while Ada.Characters.Handling.Is_Digit (NextChar) or NextChar = '_' loop
         NumString (Counter) := NextChar;
         Counter := Counter + 1;
         NextChar := GetNextChar;
      end loop;

      if NextChar /= '.' then
         if NextChar = '#' then
            while Ada.Characters.Handling.Is_Digit (NextChar)
              or else Ada.Characters.Handling.Is_Letter (NextChar)
              or else NextChar = '_'
              or else NextChar = '.'
              or else NextChar = '#'
            loop
               if NextChar = '.' then
                  HasDecimal := True;
               end if;

               NumString (Counter) := NextChar;
               Counter := Counter + 1;
               NextChar := GetNextChar;
            end loop;

            GetExponent;
            UngetChar (NextChar);

            if HasDecimal then
               Ada.Long_Long_Float_Text_IO.Get
                 (From => NumString (1 .. Counter - 1),
                  Item => FloatResult,
                  Last => Last);
            else
               Ada.Long_Long_Integer_Text_IO.Get
                 (From => NumString (1 .. Counter - 1),
                  Item => IntResult,
                  Last => Last);
            end if;
         else
            GetExponent;
            UngetChar (NextChar);
            Ada.Long_Long_Integer_Text_IO.Get
              (From => NumString (1 .. Counter - 1),
               Item => IntResult,
               Last => Last);
            HasDecimal := False;
         end if;
      else
         NextChar := GetNextChar;

         if NextChar = '.' then --  .. token should be added for next time
            HasDecimal := False;
            Ada.Long_Long_Integer_Text_IO.Get
              (From => NumString (1 .. Counter - 1),
               Item => IntResult,
               Last => Last);

            endloc := InputStringLocation - 3;
            UngetChar ('.');
            UngetChar ('.');

            return;
         end if;

         HasDecimal := True;
         NumString (Counter) := '.';
         Counter := Counter + 1;

         if not Ada.Characters.Handling.Is_Digit (NextChar) then
            HasDecimal := FALSE;
            UngetChar (NextChar);
            UngetChar ('.');
            Counter := Counter - 1;
            Ada.Long_Long_Integer_Text_IO.Get
              (From => NumString (1 .. Counter - 1),
               Item => IntResult,
               Last => Last);
            endloc := InputStringLocation - 1;

            return;
            --  11/20/01 (mcc) fixed bug of -3 to -1 above
            --  7/2/99 (mcc)
            --  modified for colorization so we can type 10..5 without having
            --  an error after just 10.
            --  similar to .. above
            --
            --  old error is below
            --  Ada.Exceptions.Raise_Exception(ErrorInNumber'Identity,
            --  "Must have at least one digit after decimal point");
         end if;

         while Ada.Characters.Handling.Is_Digit (NextChar)
           or else NextChar = '_'
         loop
            NumString (Counter) := NextChar;
            Counter := Counter + 1;
            NextChar := GetNextChar;
         end loop;

         GetExponent;
         UngetChar (NextChar);
         Ada.Long_Long_Float_Text_IO.Get
           (From => NumString (1 .. Counter - 1),
            Item => FloatResult,
            Last => Last);

      end if;

      endloc := InputStringLocation - 1;

   exception
         --  this should happen if a IO.Get call fails
      when others =>
         Ada.Exceptions.Raise_Exception
           (ErrorInNumber'Identity,
            NumString (1 .. Counter - 1) & " is not a valid number");
         endloc := InputStringLocation - 1;
   end GetNumber;

   --------------------------------
   --  PROCEDURE UngetToken
   --
   --  undoes last GetToken
   --  cannot be called twice in succession, or
   --  exception CannotUnget is raised
   --------------------------------
   procedure UngetToken (tok : Token) is
   begin
      if HaveTokenAhead = Number_Token_Lookaheads then
         raise CannotUnget;
      else
         HaveTokenAhead := HaveTokenAhead + 1;
         TokenAhead (HaveTokenAhead) := tok;
      end if;
   end UngetToken;

   ------------------------------
   --  FUNCTION CheckReservedWords
   --
   --  Inputs : a string, its length and first character
   --  Output : a token pointer of the appropriate reserved word or
   --          identifier
   ------------------------------
   function CheckReservedWords (Name      : in String;
                                Length    : in Natural;
                                FirstChar : in Character) return Token is
      First, Last : ReservedWords;
   begin
      case FirstChar is
         when 'A' | 'a' =>
            first := abort_t;
            last := at_t;
         when 'B' | 'b' =>
            first := begin_t;
            last := body_t;
         when 'C' | 'c' =>
            first := case_t;
            last := constant_t;
         when 'D' | 'd' =>
            first := declare_t;
            last := do_t;
         when 'E' | 'e' =>
            first := else_t;
            last := exit_t;
         when 'F' | 'f' =>
            first := for_t;
            last := function_t;
         when 'G' | 'g' =>
            first := generic_t;
            last := goto_t;
         when 'I' | 'i' =>
            first := if_t;
            last := is_t;
         when 'L' | 'l' =>
            first := limited_t;
            last := loop_t;
         when 'M' | 'm' =>
            first := mod_t;
            last := mod_t;
         when 'N' | 'n' =>
            first := new_t;
            last := null_t;
         when 'O' | 'o' =>
            first := of_t;
            last := overriding_t;
         when 'P' | 'p' =>
            first := package_t;
            last := protected_t;
         when 'R' | 'r' =>
            first := raise_t;
            last := reverse_t;
         when 'S' | 's' =>
            first := select_t;
            last := synchronized_t;
         when 'T' | 't' =>
            first := tagged_t;
            last := type_t;
         when 'U' | 'u' =>
            first := until_t;
            last := use_t;
         when 'W' | 'w' =>
            first := when_t;
            last := with_t;
         when 'X' | 'x' =>
            first := xor_t;
            last := xor_t;
         when others =>
            first := xor_t;
            last := abort_t; -- null range
      end case;

      --  check against all reserved words beginning with same letter as Name
      for Index in first .. last loop
         if Length = ImageLengthArray (Index)
           and then StringCompare
             (Images (Index) (1 .. Length),
              Name (Name'First .. Name'First + Length - 1))
         then
            return Token'(token_type => Index,
                          startcol   => CurrentCol - Length,
                          startloc   => InputStringLocation - Length,
                          endloc     => InputStringLocation - 1,
                          line       => CurrentLine);
         end if;
      end loop;

      --  not a reserved word
      return Token'
        (token_type => name_t,
         startcol   => CurrentCol - Length,
         startloc   => InputStringLocation - Length,
         endloc     => InputStringLocation - 1,
         line       => CurrentLine);

   end CheckReservedWords;

   --------------------------
   -- Get_First_Token_Type --
   --------------------------

   function Get_First_Token_Type (Str : in String) return Tokens is
      First   : Integer := Str'First;
      Last    : Integer;
      Result  : Token;
      HaveOne : Boolean;
      Done    : Boolean;
   begin
      GetNextToken
        (Str'Unrestricted_Access,
         First,
         Last,
         Result,
         HaveOne,
         Done);

      if HaveOne then
         return GetTokenType (Result);
      else
         return error_t;
      end if;
   end Get_First_Token_Type;

   --------------------------------------------
   --  PROCEDURE GetNextToken
   --
   --  the heart of the lexer, returns a pointer
   --  to the next token object in input stream
   --
   --  Algorithm:
   --    This is a simple LL(1) descent lexer, which
   --    checks the first character, and then
   --    branches to an appropriate handler
   --    If the first character is not sufficient to
   --    identify the token type, a nested IF is used
   --  exceptions:
   --   BadCharacter
   ---------------------------------------------
   BadCharacter : exception;

   procedure GetNextToken
     (Str     : in StringPointer;
      First   : in Integer;
      Last    : out Integer;
      Result  : out Token;
      HaveOne : out Boolean;
      Done    : out Boolean)
   is
      NextChar : Character;
      IntValue : Long_Long_Integer;
      FloatValue : Long_Long_Float;
      IsFloat : Boolean;
      Name : NameType;
      Length : Integer;
      OldCol, OldLoc, endloc : Positive;
   begin
      Done := False;
      HaveOne := True;
      HaveLookAhead := 0; -- only since reading from a string
      InputString := Str;

      if HaveTokenAhead > 0 then
         Result :=  TokenAhead (HaveTokenAhead);
         Last := TokenAhead (HaveTokenAhead).endloc;
         --    CurrentLine := Old_Positions(
         --       Number_Token_Lookaheads+1-HaveTokenAhead).Line;
         --    CurrentCol  := Old_Positions(
         --       Number_Token_Lookaheads+1-HaveTokenAhead).Column;
         HaveTokenAhead := HaveTokenAhead - 1;

         return;
      end if;

      InputStringLocation := First;
      Last := InputStringLocation;   -- important in case of exception
      NextChar := GetNextChar;

      OldLoc := InputStringLocation - 1;

      --  skip white space
      while Ada.Characters.Handling.Is_Control (NextChar)
        or else NextChar = ' '
      loop

         --  rich edit 2.0 ends with "\0"
         if NextChar = ASCII.NUL then
            HaveOne := true;
            Done := True;
            --  return error-token, because of reading end of input
            Result := Token'(token_type => error_t,
                             line       => CurrentLine,
                             startloc   => OldLoc,
                             startcol   => CurrentCol - 1,
                             endloc     => InputStringLocation - 1);
            return;
         end if;

         NextChar := GetNextChar;
      end loop;

      --  used in case of error
      OldCol := CurrentCol - 1;
      OldLoc := InputStringLocation - 1;

      case NextChar is
         when '&' =>
            Result := Token'
              (token_type => ampersand_t,
               line       => CurrentLine,
               startloc   => InputStringLocation - 1,
               startcol   => CurrentCol - 1,
               endloc     => InputStringLocation - 1);

         when ''' =>
            declare
               AfterTick, TwoAfterTick : Character;
            begin
               AfterTick := GetNextChar;
               TwoAfterTick := GetNextChar;

               if TwoAfterTick /= ''' then
                  UngetChar (TwoAfterTick);
                  UngetChar (AfterTick);
                  Result := Token'
                    (token_type => tick_t,
                     line       => CurrentLine,
                     startcol   => CurrentCol - 1,
                     startloc   => InputStringLocation - 1,
                     endloc     => InputStringLocation - 1);
               else
                  Result := Token'
                    (token_type => char_literal_t,
                     line       => CurrentLine,
                     startcol   => CurrentCol - 3,
                     startloc   => InputStringLocation - 3,
                     endloc     => InputStringLocation - 1);
               end if;
            end;

         when '(' =>
            Result := Token'
              (token_type => lparen_t,
               line       => CurrentLine,
               startcol   => CurrentCol - 1,
               startloc   => InputStringLocation - 1,
               endloc     => InputStringLocation - 1);

         when ')' =>
            Result := Token'
              (token_type => rparen_t,
               line       => CurrentLine,
               startcol   => CurrentCol - 1,
               startloc   => InputStringLocation - 1,
               endloc     => InputStringLocation - 1);
         when '*' =>
            NextChar := GetNextChar;

            if NextChar = '*' then
               Result :=  Token'
                 (token_type => double_star_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 2,
                  startloc   => InputStringLocation - 2,
                  endloc     => InputStringLocation - 1);
            else
               UnGetChar (Item => NextChar);
               Result := Token'
                 (token_type => times_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 1,
                  startloc   => InputStringLocation - 1,
                  endloc     => InputStringLocation - 1);
            end if;

         when '+' =>
            Result :=  Token'
              (token_type => plus_t,
               line       => CurrentLine,
               startcol   => CurrentCol - 1,
               startloc   => InputStringLocation - 1,
               endloc     => InputStringLocation - 1);

         when ',' =>
            Result :=  Token'
              (token_type => comma_t,
               line       => CurrentLine,
               startcol   => CurrentCol - 1,
               startloc   => InputStringLocation - 1,
               endloc     => InputStringLocation - 1);

         when '-' =>
            NextChar := GetNextChar;

            if NextChar = '-' then
               Result :=  GetComment;
            else
               UnGetChar (Item => NextChar);
               Result := Token'
                 (token_type => minus_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 1,
                  startloc   => InputStringLocation - 1,
                  endloc     => InputStringLocation - 1);
            end if;

         when '.' =>
            NextChar := GetNextChar;

            if NextChar = '.' then
               Result :=  Token'
                 (token_type => double_dot_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 2,
                  startloc   => InputStringLocation - 2,
                  endloc     => InputStringLocation - 1);
            else
               UnGetChar (Item => NextChar);
               Result := Token'
                 (token_type => dot_t,
                  startcol   => CurrentCol - 1,
                  line       => CurrentLine,
                  startloc   => InputStringLocation - 1,
                  endloc     => InputStringLocation - 1);
            end if;

         when '/' =>
            NextChar := GetNextChar;

            if NextChar = '=' then
               Result :=  Token'
                 (token_type => noteq_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 2,
                  startloc   => InputStringLocation - 2,
                  endloc     => InputStringLocation - 1);
            else
               UnGetChar (Item => NextChar);
               Result := Token'
                 (token_type => divide_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 1,
                  startloc   => InputStringLocation - 1,
                  endloc     => InputStringLocation - 1);
            end if;

         when ':' =>
            NextChar := GetNextChar;

            if NextChar = '=' then
               Result := Token'
                 (token_type => assignment_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 2,
                  startloc   => InputStringLocation - 2,
                  endloc     => InputStringLocation - 1);
            else
               UnGetChar (Item => NextChar);
               Result := Token'
                 (token_type => colon_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 1,
                  startloc   => InputStringLocation - 1,
                  endloc     => InputStringLocation - 1);
            end if;

         when ';' =>
            Result := Token'
              (token_type => semicolon_t,
               line       => CurrentLine,
               startcol   => CurrentCol - 1,
               startloc   => InputStringLocation - 1,
               endloc     => InputStringLocation - 1);

         when '<' =>
            NextChar := GetNextChar;

            if NextChar = '=' then
               Result := Token'
                 (token_type => leq_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 2,
                  startloc   => InputStringLocation - 2,
                  endloc     => InputStringLocation - 1);

            elsif NextChar = '<' then
               Result := Token'
                 (token_type => left_label_bracket_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 2,
                  startloc   => InputStringLocation - 2,
                  endloc     => InputStringLocation - 1);
            elsif NextChar = '>' then
               Result := Token'
                 (token_type => box_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 2,
                  startloc   => InputStringLocation - 2,
                  endloc     => InputStringLocation - 1);
            else
               UnGetChar (Item => NextChar);
               Result :=  Token'
                 (token_type => lt_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 2,
                  startloc   => InputStringLocation - 1,
                  endloc     => InputStringLocation - 1);
            end if;

         when '=' =>
            NextChar := GetNextChar;

            if NextChar = '>' then
               Result := Token'
                 (token_type => arrow_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 2,
                  startloc   => InputStringLocation - 2,
                  endloc     => InputStringLocation - 1);
            else
               UnGetChar (Item => NextChar);
               Result :=  Token'
                 (token_type => eq_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 1,
                  startloc   => InputStringLocation - 1,
                  endloc     => InputStringLocation - 1);
            end if;

         when '>' =>
            NextChar := GetNextChar;

            if NextChar = '=' then
               Result :=  Token'
                 (token_type => geq_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 2,
                  startloc   => InputStringLocation - 2,
                  endloc     => InputStringLocation - 1);
            elsif NextChar = '>' then
               Result :=  Token'
                 (token_type => right_label_bracket_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 2,
                  startloc   => InputStringLocation - 2,
                  endloc     => InputStringLocation - 1);
            else
               UnGetChar (Item => NextChar);
               Result := Token'
                 (token_type => gt_t,
                  line       => CurrentLine,
                  startcol   => CurrentCol - 1,
                  startloc   => InputStringLocation - 1,
                  endloc     => InputStringLocation - 1);
            end if;

         when '|' =>
            Result :=  Token'
              (token_type => pipe_t,
               line       => CurrentLine,
               startcol   => CurrentCol - 1,
               startloc   => InputStringLocation - 1,
               endloc     => InputStringLocation - 1);

         when '0' .. '9' =>
            GetNumber
              (FirstDigit  => NextChar,
               IntResult   => IntValue,
               FloatResult => FloatValue,
               HasDecimal  => IsFloat,
               endloc      => endloc);

            if IsFloat then
               Result :=  Token'
                 (token_type => float_t,
                  endloc     => endloc,
                  startcol   => OldCol,
                  startloc   => OldLoc,
                  line       => CurrentLine);
            else
               Result := Token'
                 (token_type => integer_t,
                  endloc     => endloc,
                  startcol   => OldCol,
                  startloc   => OldLoc,
                  line       => CurrentLine);
            end if;

         when '"' =>
            GetStringConstant
              (FirstChar => NextChar,
               Name      => Name,
               Length    => Length);
            Result := Token'
              (token_type => string_t,
               line       => CurrentLine,
               startcol   => CurrentCol - Length,
               startloc   => InputStringLocation - Length,
               endloc     => InputStringLocation - 1);

         when 'A' .. 'Z' | 'a' .. 'z' =>
            GetIdentifier
              (FirstChar => NextChar,
               Name      => Name,
               Length    => Length);
            Result :=  CheckReservedWords
              (Name      => Name (1 .. Length),
               Length    => Length,
               FirstChar => NextChar);

         when others =>
            if Ada.Characters.Handling.Is_Letter (NextChar) then
               GetIdentifier
                 (FirstChar => NextChar,
                  Name      => Name,
                  Length    => Length);
               Result :=  CheckReservedWords
                 (Name      => Name (1 .. Length),
                  Length    => Length,
                  FirstChar => NextChar);
            else
               Ada.Exceptions.Raise_Exception
                 (BadCharacter'Identity,
                  "Character is not valid in Ada");
            end if;
      end case;

      Last := InputStringLocation - 1;

      for i in reverse 2 .. Number_Token_Lookaheads + 1 loop
         Old_Positions (i) := Old_Positions (i - 1);
      end loop;

      Old_Positions (1).Column := CurrentCol;
      Old_Positions (1).Line   := CurrentLine;

   exception
      when EndOfString =>
         HaveOne := False;
         Done := True;
      when E : others =>
         Result := Token'
           (token_type => error_t,
            line       => CurrentLine,
            startcol   => OldCol,
            startloc   => Oldloc,
            endloc     => InputStringLocation - 1);
         HaveOne := True;
         Last := InputStringLocation - 1;
   end GetNextToken;

   -------------------
   -- GetTokenClass --
   -------------------

   function GetTokenClass (Item : Token) return TokenClass is
   begin
      if Item.token_type in ReservedWords then
         return ReservedClass;
      elsif Item.token_type in Constants then
         return ConstantClass;
      elsif Item.token_type = string_t then
         return StringClass;
      elsif Item.token_type = comment_t then
         return CommentClass;
      elsif Item.token_type = name_t then
         return IdentifierClass;
      else
         return OtherClass;
      end if;
   exception
      when others =>
         return OtherClass;
   end GetTokenClass;

   ------------------
   -- GetTokenType --
   ------------------

   function GetTokenType (Item : Token) return Tokens is
   begin
      return Item.token_type;
   end GetTokenType;

   --------------
   -- GetStart --
   --------------

   function GetStart (Item : Token) return Integer is
   begin
      return Item.startloc;
   end GetStart;

   ------------
   -- GetEnd --
   ------------

   function GetEnd (Item : Token) return Integer is
   begin
      return Item.endloc;
   end GetEnd;

   ------------
   -- GetCol --
   ------------

   function GetCol (Item : Token) return Integer is
   begin
      return Item.startcol;
   end GetCol;

   -------------
   -- GetLine --
   -------------

   function GetLine (Item : Token) return Integer is
   begin
      return Item.line;
   end GetLine;

end lexer;
