------------------------------------------------------------------------------
--                                                                          --
--                      GNAT VISUAL STUDIO INTEGRATION                      --
--                                                                          --
--                                 VS_LEXER                                 --
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

with Ada.Wide_Characters.Unicode;
with MSSyst.String;
use MSSyst.String;
with Ada.Characters.Wide_Latin_1;

--  The following withs are here for inclusion in the library
with Krunch;
with MCC.Gnat_Tools.Xref;
with Settings;
with Reformat_Pkg;

package body Vs_Lexer is

   Number_Of_Reserved_Words : constant := 72;

   subtype Index_Range is Natural range 0 .. Number_Of_Reserved_Words - 1;

   subtype Uppercase is Wide_Character range 'A' .. 'Z';

   type Table_Array is array (Uppercase) of Natural;

   Table_1 : constant Table_Array :=
               (41, 33, 35, 28, 42, 9, 14, 1, 21, 0, 22, 32, 65, 14,
                11, 14, 2, 12, 56, 0, 0, 71, 30, 0, 0, 0);

   Table_2 : constant Table_Array :=
               (3, 4, 8, 17, 68, 8, 34, 0, 32, 0, 26, 15, 35, 42, 1,
                60, 0, 51, 20, 18, 5, 42, 11, 7, 0, 0);

   function Hash (Name : Wide_String) return Index_Range;
   --  Preconditions : Name contains only uppercase characters
   --  Name contains at least one character

   function Check_Reserved (Y : Wide_String) return Tokens;

   procedure Get_Identifier
     (X      :        Wide_String;
      Result : in out Token_Record);
   --  loop through and get a complete identifier
   --  such as Ada.Text_IO

   procedure Get_Number
     (X      :        Wide_String;
      Result : in out Token_Record);
   --  loop through and get a complete identifier
   --  such as 16#AF3.45#

   procedure Get_String
     (X      :        Wide_String;
      Result : in out Token_Record);
   --  loop through and get a complete string
   --  such as "asdfasdf""asdfasdf"

   ----------
   -- Hash --
   ----------

   function Hash (Name : Wide_String) return Index_Range is
      Result : Index_Range;

   begin
      Result := (Table_1 (Name (107 rem Name'Length + Name'First)) +
                   Table_2 (Name (108 rem Name'Length + Name'First)) +
                   Name'Length)
                rem Number_Of_Reserved_Words;

      return Result;
   end Hash;

   --------------------
   -- Check_Reserved --
   --------------------

   function Check_Reserved (Y : Wide_String) return Tokens is
   begin
      case Hash (Y) is
         when 0 =>
            if Y = "EXIT" then
               return Exit_T;
            end if;
         when 1 =>
            if Y = "OVERRIDING" then
               return Overriding_T;
            end if;
         when 2 =>
            if Y = "REVERSE" then
               return Reverse_T;
            end if;
         when 3 =>
            if Y = "NEW" then
               return New_T;
            end if;
         when 4 =>
            if Y = "OUT" then
               return Out_T;
            end if;
         when 5 =>
            if Y = "AT" then
               return At_T;
            end if;
         when 6 =>
            if Y = "NULL" then
               return Null_T;
            end if;
         when 7 =>
            if Y = "DIGITS" then
               return Digits_T;
            end if;
         when 8 =>
            if Y = "BODY" then
               return Body_T;
            end if;
         when 9 =>
            if Y = "ACCEPT" then
               return Accept_T;
            end if;
         when 10 =>
            if Y = "CONSTANT" then
               return Constant_T;
            end if;
         when 11 =>
            if Y = "INTERFACE" then
               return Interface_T;
            end if;
         when 12 =>
            if Y = "OR" then
               return Or_T;
            end if;
         when 13 =>
            if Y = "RECORD" then
               return Record_T;
            end if;
         when 14 =>
            if Y = "REQUEUE" then
               return Requeue_T;
            end if;
         when 15 =>
            if Y = "OF" then
               return Of_T;
            end if;
         when 16 =>
            if Y = "WITH" then
               return With_T;
            end if;
         when 17 =>
            if Y = "GENERIC" then
               return Generic_T;
            end if;
         when 18 =>
            if Y = "IS" then
               return Is_T;
            end if;
         when 19 =>
            if Y = "EXCEPTION" then
               return Exception_T;
            end if;
         when 20 =>
            if Y = "ARRAY" then
               return Array_T;
            end if;
         when 21 =>
            if Y = "ELSIF" then
               return Elsif_T;
            end if;
         when 22 =>
            if Y = "XOR" then
               return Xor_T;
            end if;
         when 23 =>
            if Y = "FOR" then
               return For_T;
            end if;
         when 24 =>
            if Y = "RENAMES" then
               return Renames_T;
            end if;
         when 25 =>
            if Y = "PROTECTED" then
               return Protected_T;
            end if;
         when 26 =>
            if Y = "SELECT" then
               return Select_T;
            end if;
         when 27 =>
            if Y = "END" then
               return End_T;
            end if;
         when 28 =>
            if Y = "SEPARATE" then
               return Separate_T;
            end if;
         when 29 =>
            if Y = "WHEN" then
               return When_T;
            end if;
         when 30 =>
            if Y = "DO" then
               return Do_T;
            end if;
         when 31 =>
            if Y = "ALIASED" then
               return Aliased_T;
            end if;
         when 32 =>
            if Y = "LIMITED" then
               return Limited_T;
            end if;
         when 33 =>
            if Y = "LOOP" then
               return Loop_T;
            end if;
         when 34 =>
            if Y = "AND" then
               return And_T;
            end if;
         when 35 =>
            if Y = "PRAGMA" then
               return Pragma_T;
            end if;
         when 36 =>
            if Y = "THEN" then
               return Then_T;
            end if;
         when 37 =>
            if Y = "UNTIL" then
               return Until_T;
            end if;
         when 38 =>
            if Y = "ALL" then
               return All_T;
            end if;
         when 39 =>
            if Y = "PROCEDURE" then
               return Procedure_T;
            end if;
         when 40 =>
            if Y = "DELAY" then
               return Delay_T;
            end if;
         when 41 =>
            if Y = "WHILE" then
               return While_T;
            end if;
         when 42 =>
            if Y = "ELSE" then
               return Else_T;
            end if;
         when 43 =>
            if Y = "IF" then
               return If_T;
            end if;
         when 44 =>
            if Y = "TASK" then
               return Task_T;
            end if;
         when 45 =>
            if Y = "NOT" then
               return Not_T;
            end if;
         when 46 =>
            if Y = "RAISE" then
               return Raise_T;
            end if;
         when 47 =>
            if Y = "REM" then
               return Rem_T;
            end if;
         when 48 =>
            if Y = "IN" then
               return In_T;
            end if;
         when 49 =>
            if Y = "GOTO" then
               return Goto_T;
            end if;
         when 50 =>
            if Y = "USE" then
               return Use_T;
            end if;
         when 51 =>
            if Y = "BEGIN" then
               return Begin_T;
            end if;
         when 52 =>
            if Y = "TAGGED" then
               return Tagged_T;
            end if;
         when 53 =>
            if Y = "RANGE" then
               return Range_T;
            end if;
         when 54 =>
            if Y = "CASE" then
               return Case_T;
            end if;
         when 55 =>
            if Y = "DELTA" then
               return Delta_T;
            end if;
         when 56 =>
            if Y = "ENTRY" then
               return Entry_T;
            end if;
         when 57 =>
            if Y = "DECLARE" then
               return Declare_T;
            end if;
         when 58 =>
            if Y = "SUBTYPE" then
               return Subtype_T;
            end if;
         when 59 =>
            if Y = "ABSTRACT" then
               return Abstract_T;
            end if;
         when 60 =>
            if Y = "SYNCHRONIZED" then
               return Synchronized_T;
            end if;
         when 61 =>
            if Y = "FUNCTION" then
               return Function_T;
            end if;
         when 62 =>
            if Y = "ABS" then
               return Abs_T;
            end if;
         when 63 =>
            if Y = "OTHERS" then
               return Others_T;
            end if;
         when 64 =>
            if Y = "TYPE" then
               return Type_T;
            end if;
         when 65 =>
            if Y = "ACCESS" then
               return Access_T;
            end if;
         when 66 =>
            if Y = "MOD" then
               return Mod_T;
            end if;
         when 67 =>
            if Y = "ABORT" then
               return Abort_T;
            end if;
         when 68 =>
            if Y = "PACKAGE" then
               return Package_T;
            end if;
         when 69 =>
            if Y = "TERMINATE" then
               return Terminate_T;
            end if;
         when 70 =>
            if Y = "PRIVATE" then
               return Private_T;
            end if;
         when 71 =>
            if Y = "RETURN" then
               return Return_T;
            end if;
      end case;
      return Name_T;
   end Check_Reserved;

   --------------------
   -- Get_Identifier --
   --------------------

   procedure Get_Identifier
     (X      :        Wide_String;
      Result : in out Token_Record)
   is
      Maybe_Reserved : Boolean := True;
      Letter         : Boolean;
      Y              : Wide_String
        (Result.End_Index + 1 .. Result.End_Index + 255);

   begin
      loop
         exit when Result.End_Index > X'Last;
         Letter := Ada.Wide_Characters.Unicode.Is_Letter
           (X (Result.End_Index));

         exit when not Letter
           and then X (Result.End_Index) /= '_'
           and then not Ada.Wide_Characters.Unicode.Is_Digit
             (X (Result.End_Index));

         if not Letter then
            Maybe_Reserved := False;
         else
            Y (Result.End_Index + 1) :=
              Ada.Wide_Characters.Unicode.To_Upper_Case (X (Result.End_Index));
         end if;

         Result.End_Index := Result.End_Index + 1;
      end loop;

      --  we got one more character than we need
      Result.End_Index := Result.End_Index - 1;

      if Maybe_Reserved and Result.End_Index + 1 > Y'First then
         Result.Kind := Check_Reserved (Y (Y'First .. Result.End_Index + 1));

         if Result.Kind /= Name_T then
            Result.Class_Of := Keyword;
         else
            Result.Class_Of := Identifier;
         end if;
      else
         Result.Class_Of := Identifier;
         Result.Kind := Name_T;
      end if;
   end Get_Identifier;

   ----------------
   -- Get_Number --
   ----------------

   procedure Get_Number
     (X      :        Wide_String;
      Result : in out Token_Record)
   is
      Digit : Boolean;
   begin
      loop
         exit when Result.End_Index > X'Last;

         Digit := Ada.Wide_Characters.Unicode.Is_Digit (X (Result.End_Index))
           or else (X (Result.End_Index) >= 'A'
                    and then X (Result.End_Index) <= 'F');

         exit when not Digit
           and then X (Result.End_Index) /= '_'
           and then X (Result.End_Index) /= '.'
           and then X (Result.End_Index) /= '#';

         Result.End_Index := Result.End_Index + 1;
      end loop;

      --  we got one more character than we need
      Result.End_Index := Result.End_Index - 1;
      Result.Class_Of := Literal;
      Result.Kind := Float_T;
   end Get_Number;

   ----------------
   -- Get_String --
   ----------------

   procedure Get_String
     (X      :        Wide_String;
      Result : in out Token_Record)
   is
      Correct : Boolean := False;
   begin
      loop
         while Result.End_Index <= X'Last
           and then X (Result.End_Index) /= '"'
           and then X (Result.End_Index) /= Ada.Characters.Wide_Latin_1.CR
           and then X (Result.End_Index) /= Ada.Characters.Wide_Latin_1.LF
         loop
            Result.End_Index := Result.End_Index + 1;
         end loop;

         if Result.End_Index > X'Last then
            Result.Kind := Error_T;
            Result.Class_Of := Unknown;
            exit;
         end if;

         Result.End_Index := Result.End_Index + 1;

         exit when Result.End_Index > X'Last
           or else X (Result.End_Index) /= '"';

         Result.End_Index := Result.End_Index + 1;
      end loop;

      Result.Kind := String_T;
      Result.Class_Of := String;
      Result.End_Index := Result.End_Index - 1;
   end Get_String;

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token
     (X              : Wide_String;
      Current_Offset : Natural)
      return Token_Record
   is
      Result  : Token_Record;
   begin
      Result.Class_Of := Operator;
      --  get all of the white space
      Result.Start_Index := Current_Offset;
      Result.End_Index := Current_Offset;
      Result.Triggers := None;

      while Result.End_Index <= X'Last
        and then
          (Ada.Wide_Characters.Unicode.Is_Space (X (Result.End_Index))
           or else X (Result.End_Index) = Ada.Characters.Wide_Latin_1.CR
           or else X (Result.End_Index) = Ada.Characters.Wide_Latin_1.LF)
      loop
         Result.End_Index := Result.End_Index + 1;
      end loop;

      --  stop if we got some white space
      if Result.End_Index > Result.Start_Index then
         Result.Kind := White_Space_T;
         Result.End_Index := Result.End_Index - 1;
         Result.Class_Of := Whitespace;

         return Result;
      end if;

      if Ada.Wide_Characters.Unicode.Is_Letter (X (Result.End_Index)) then
         Get_Identifier (X, Result);

         if Result.Start_Index = Result.End_Index then
            Result.Triggers := MemberSelect;
         end if;

         return Result;
      end if;

      --  consume numbers
      if Ada.Wide_Characters.Unicode.Is_Digit (X (Result.End_Index)) then
         Get_Number (X, Result);

         return Result;
      end if;

      --  consume string
      if X (Result.End_Index) = '"' then
         Result.End_Index := Result.End_Index + 1;
         Get_String (X, Result);

         return Result;
      end if;

      case X (Result.End_Index) is
         when '&' =>
            Result.Kind := Ampersand_T;

         when ''' =>
            if Result.End_Index + 2 <= X'Last
              and then X (Result.End_Index + 2) = '''
            then
               Result.End_Index := Result.End_Index + 2;
               Result.Kind := Char_Literal_T;
               Result.Class_Of := Literal;
            else
               Result.Kind := Tick_T;
            end if;

         when '(' =>
            Result.Kind := Lparen_T;
            Result.Triggers := MatchBraces + ParameterStart;

         when ')' =>
            Result.Kind := Rparen_T;
            Result.Triggers := MatchBraces;

         when '*' =>
            if Result.End_Index < X'Last
              and then X (Result.End_Index + 1) = '*'
            then
               Result.End_Index := Result.End_Index + 1;
               Result.Kind := Double_Star_T;
            else
               Result.Kind := Times_T;
            end if;

         when '+' =>
            Result.Kind := Plus_T;

         when ',' =>
            Result.Kind := Comma_T;
            Result.Triggers := ParameterNext;

         when '-' =>
            if Result.End_Index < X'Last
              and then X (Result.End_Index + 1) = '-'
            then
               loop
                  Result.End_Index := Result.End_Index + 1;
                  exit when Result.End_Index = X'Last
                    or else
                      X (Result.End_Index) = Ada.Characters.Wide_Latin_1.CR
                    or else
                      X (Result.End_Index) = Ada.Characters.Wide_Latin_1.LF;
               end loop;

               Result.Kind :=  Comment_T;
               Result.Class_Of := Linecomment;
            else
               Result.Kind := Minus_T;
            end if;

         when '.' =>
            if Result.End_Index < X'Last
              and then X (Result.End_Index + 1) = '.'
            then
               Result.End_Index := Result.End_Index + 1;
               Result.Kind := Double_Dot_T;
            else
               Result.Kind := Dot_T;
               Result.Triggers := MemberSelect;
            end if;

         when '/' =>
            if Result.End_Index < X'Last
              and then X (Result.End_Index + 1) = '='
            then
               Result.End_Index := Result.End_Index + 1;
               Result.Kind := Noteq_T;
            else
               Result.Kind := Divide_T;
            end if;

         when ':' =>
            if Result.End_Index < X'Last
              and then X (Result.End_Index + 1) = '='
            then
               Result.End_Index := Result.End_Index + 1;
               Result.Kind := Assignment_T;
            else
               Result.Kind := Colon_T;
            end if;

         when ';' =>
            Result.Kind := Semicolon_T;
            Result.Triggers := ParameterEnd;

         when '<' =>
            if Result.End_Index < X'Last
              and then X (Result.End_Index + 1) = '='
            then
               Result.End_Index := Result.End_Index + 1;
               Result.Kind := Leq_T;

            elsif Result.End_Index < X'Last
              and then X (Result.End_Index + 1) = '<'
            then
               Result.End_Index := Result.End_Index + 1;
               Result.Kind := Left_Label_Bracket_T;

            elsif Result.End_Index < X'Last
              and then X (Result.End_Index + 1) = '>'
            then
               Result.End_Index := Result.End_Index + 1;
               Result.Kind := Box_T;

            else
               Result.Kind := Lt_T;
            end if;

         when '=' =>
            if Result.End_Index < X'Last
              and then X (Result.End_Index + 1) = '>'
            then
               Result.End_Index := Result.End_Index + 1;
               Result.Kind := Arrow_T;

            else
               Result.Kind := Eq_T;
            end if;

         when '>' =>
            if Result.End_Index < X'Last
              and then X (Result.End_Index + 1) = '>'
            then
               Result.End_Index := Result.End_Index + 1;
               Result.Kind := Right_Label_Bracket_T;

            elsif Result.End_Index < X'Last
              and then X (Result.End_Index + 1) = '='
            then
               Result.End_Index := Result.End_Index + 1;
               Result.Kind := Geq_T;

            else
               Result.Kind := Gt_T;
            end if;

         when '|' =>
            Result.Kind := Pipe_T;

         when others =>
            Result.Kind := Error_T;
            Result.Class_Of := Unknown;
      end case;

      return Result;
   end Get_Token;

   ---------------------
   -- Match_Direction --
   ---------------------

   function Match_Direction (Kind : Tokens) return Integer is
   begin
      case Kind is
         when Lparen_T => return 1;
         when Rparen_T => return -1;
         when others => return 0;
      end case;
   end Match_Direction;

end Vs_Lexer;
