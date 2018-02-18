------------------------------------------------------------------------------
--                                                                          --
--                      GNAT VISUAL STUDIO INTEGRATION                      --
--                                                                          --
--                                 VS_LEXER                                 --
--                                                                          --
--                                 S p e c                                  --
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

with MSSyst.String;

package Vs_Lexer is

   type Tokens is
     ( --  reserved words from LRM 2.9 (2)
      Abort_T,
      Abs_T,
      Abstract_T,
      Accept_T,
      Access_T,
      Aliased_T,
      All_T,
      And_T,
      Array_T,
      At_T,
      Begin_T,
      Body_T,
      Case_T,
      Constant_T,
      Declare_T,
      Delay_T,
      Delta_T,
      Digits_T,
      Do_T,
      Else_T,
      Elsif_T,
      End_T,
      Entry_T,
      Exception_T,
      Exit_T,
      For_T,
      Function_T,
      Generic_T,
      Goto_T,
      If_T,
      In_T,
      Interface_T,
      Is_T,
      Limited_T,
      Loop_T,
      Mod_T,
      New_T,
      Not_T,
      Null_T,
      Of_T,
      Or_T,
      Others_T,
      Out_T,
      Overriding_T,
      Package_T,
      Pragma_T,
      Private_T,
      Procedure_T,
      Protected_T,
      Raise_T,
      Range_T,
      Record_T,
      Rem_T,
      Renames_T,
      Requeue_T,
      Return_T,
      Reverse_T,
      Select_T,
      Separate_T,
      Subtype_T,
      Synchronized_T,
      Tagged_T,
      Task_T,
      Terminate_T,
      Then_T,
      Type_T,
      Until_T,
      Use_T,
      When_T,
      While_T,
      With_T,
      Xor_T,
      --  compound delimiters from LRM 2.2 (14)
      --  => .. ** := /= >= <= << >> <>
      Arrow_T,
      Double_Dot_T,
      Double_Star_T,
      Assignment_T,
      Noteq_T,
      Geq_T,
      Leq_T,
      Left_Label_Bracket_T,
      Right_Label_Bracket_T,
      Box_T,
      --  delimiters from LRM 2.2 (9)
      --  & ' ( ) * + , - . / : ; < = > |
      Ampersand_T,
      Tick_T,
      Lparen_T,
      Rparen_T,
      Times_T,
      Plus_T,
      Comma_T,
      Minus_T,
      Dot_T,
      Divide_T,
      Colon_T,
      Semicolon_T,
      Lt_T,
      Eq_T,
      Gt_T,
      Pipe_T,
      --  constants from LRM 2.4
      Integer_T,
      Float_T,
      Based_T,
      Char_Literal_T,
      --  other tokens
      Name_T,
      String_T,
      Comment_T,
      White_Space_T,
      Error_T);

   subtype Reservedwords is Tokens range Abort_T .. Xor_T;

   subtype Compounddelimiters is Tokens range Arrow_T .. Box_T;

   subtype Delimiters is Tokens range Ampersand_T .. Pipe_T;

   subtype Constants is Tokens range Integer_T .. Char_Literal_T;

   type Token_Class is
     (Unknown,
      Text,
      Keyword,
      Identifier,
      String,
      Literal,
      Operator,
      Delimeter,   --  asdf
      Whitespace,
      Linecomment,
      Comment);

   type TokenTriggers is new Integer;

   None           : constant TokenTriggers := 0;
   MemberSelect   : constant TokenTriggers := 1;
   MatchBraces    : constant TokenTriggers := 2;
   ParameterStart : constant TokenTriggers := 16;
   ParameterNext  : constant TokenTriggers := 32;
   ParameterEnd   : constant TokenTriggers := 64;
   Parameter      : constant TokenTriggers := 128;
   MethodTip      : constant TokenTriggers := 240;

   type Token_Record is record
      Start_Index : Natural;
      End_Index   : Natural;
      Kind        : Tokens;
      Triggers    : TokenTriggers;
      Class_Of    : Token_Class;
   end record;

   function Get_Token (X : Wide_String; Current_Offset : Natural)
                       return Token_Record;

   function Match_Direction (Kind : Tokens) return Integer;
   --  returns +1 for (, -1 for ), 0 otherwise

end Vs_Lexer;
