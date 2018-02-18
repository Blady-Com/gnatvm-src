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
-----------------------------------------------------------------------
--  declarations-arguments.adb
--
--  Author: Robert A. French
--  E-mail: rfrench99@hotmail.com
--
--  Description:
--  Defines the "Arguments" subclass. This is basically used for
--  anything that could possibly approximate arguments, whether they be
--  named with formal parameters, simply separated by commas, whatever.
-----------------------------------------------------------------------

with Ada.Strings.Fixed;
use  Ada.Strings.Fixed;
with Lists_Generic;

with Lexer;
use type Lexer.StringPointer;
use type Lexer.Tokens;

with Writer;
with Token_Writer;
with Reformat_Pkg;
with Settings;

package body Declarations.Arguments is

   Continuation : Boolean := False;
   --  is this a argument list after a comment

   Suppressing : Boolean := False;
   --  This indicates whether or not indentation will be suppressed
   --  when outputting the first line of an Argument list.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Argument) is
   begin
      Initialize (Declaration (Object));
      Object.Value_End := 1;
      Object.Has_Comma := False;
      Object.Has_Value := False;
      Object.Value_Str := new String'(" ");
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Argument) is
      Temp : Lexer.StringPointer;
   begin
      Adjust (Declaration (Object));
      if Object.Value_Str /= null then
         Temp             := Object.Value_Str;
         Object.Value_Str := new String'(Temp.all);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Argument) is
   begin
      if Object.Value_Str /= null then
         Free (Object.Value_Str);
      end if;
      Finalize (Declaration (Object));
   end Finalize;

   ----------
   -- Read --
   ----------

   procedure Read
     (Item  :    out Argument;
      From  : in     Lexer.StringPointer;
      First : in out Positive)
   is

      The_Token : Lexer.Token;
      Last      : Positive;
      Have      : Boolean  := True;
      Done      : Boolean  := False;
      Kind      : Lexer.Tokens;
      --  These variables are all used while retrieving a token. Most
      --  aren't really important to this method.

      LParens : Natural := 0;
      --  This tracks the number of left parentheses that are still
      --  unmatched. If it tries to go negative, then there is an
      --  unmatched right parenthesis.
      Remember_Line : Natural;

   begin
      --  The rules for reading an Argument declaration are basically:
      --   - Anything up to the arrow (except a comment) is part of the
      --     name.
      --   - In addition, a comma can appear where the arrow is. Any comma
      --     will end the declaration.
      --   - Comments will be accumulated and concatenated in the comment
      --     string. This will be output after the current declaration.
      --   - A reserved word will also end the declaration. Unlike commas,
      --     the reserved word will still be the next available read from
      --     the input.
      --   - Embedded comments will all be concatenated to the comment
      --     string. After a comma , however, only comments on the same
      --     line as the last token will be added. Otherwise, they will be
      --     available for the next read.
      --   - An unmatched right parenthesis will terminate the Argument.
      --   - NULL can appear anywhere.

      --  First start looking for an arrow or comma (or a possible
      --  comment).
      loop
         Remember_Line := Lexer.GetCurrentLine;
         Lexer.GetNextToken (From, First, Last, The_Token, Have, Done);
         Kind := Lexer.GetTokenType (The_Token);

         if not Have or else Done then
            return;

         elsif Kind in Lexer.ReservedWords
           and then Kind /= Lexer.Null_T
           and then Kind /= Lexer.Access_T
         then
            Lexer.Initialize (Remember_Line);
            return;
         end if;

         case Kind is
            when Lexer.Arrow_T =>
               if Lparens = 0 then
                  --  Have an arrow, now go look for a value (eg, an actual
                  --  parameter in an argument list).
                  Item.Has_Value := True;
                  First := Last + 1;
                  exit;
               else
                  Item.Nested_Name := True;
                  First := Last + 1;
                  Token_Writer.Write (The_Token, From, Item.Name_Str,
                            Item.Name_End);
               end if;

            when Lexer.Comma_T =>
               if Lparens = 0 then
                  --  That ends this Argument.
                  Item.Has_Comma := True;
                  First := Last + 1;
                  Get_Comments (Lexer.GetLine (The_Token), From, First,
                            Item.Comment_Str, Item.Comment_End);
                  if Item.Comment_End > 1 then
                     Item.Has_Comment := True;
                  end if;
                  return;
               else
                  Item.Nested_Name := True;
                  First := Last + 1;
                  Token_Writer.Write (The_Token, From, Item.Name_Str,
                            Item.Name_End);
               end if;
            when Lexer.Comment_T =>
               Item.Has_Comment := True;
               Token_Writer.Write (The_Token, From, Item.Comment_Str,
                            Item.Comment_End);
               First := Last + 1;

            when Lexer.LParen_T =>
               LParens := LParens + 1;
               Token_Writer.Write (The_Token, From, Item.Name_Str,
                            Item.Name_End);
               First := Last + 1;

            when Lexer.RParen_T =>
               --  If LParens is already zero, this new right paren is
               --  unmatched. "Unread" it and complete this list.
               if LParens = 0 then
                  Lexer.Initialize (Remember_Line);
                  return;
               else
                  LParens := LParens - 1;
                  Token_Writer.Write (The_Token, From, Item.Name_Str,
                               Item.Name_End);
                  First := Last + 1;
               end if;

            when others =>
               First := Last + 1;
               Token_Writer.Write (The_Token, From, Item.Name_Str,
                            Item.Name_End);
         end case;
      end loop;

      --  From here, we revert to normal rules and start getting most
      --  tokens up to the comma as part of the value (actual argument).
      loop
         Remember_Line := Lexer.Getcurrentline;
         Lexer.GetNextToken (From, First, Last, The_Token, Have, Done);
         Kind := Lexer.GetTokenType (The_Token);
         if not Have or Done then
            return;
         elsif Kind in Lexer.ReservedWords and Kind /= Lexer.Null_T then
            Lexer.Initialize (Remember_Line);
            return;
         end if;

         case Kind is
            when Lexer.Comma_T =>
               if Lparens = 0 then
                  Item.Has_Comma := True;
                  Get_Comments (Lexer.GetLine (The_Token), From, First,
                            Item.Comment_Str, Item.Comment_End);
                  if Item.Comment_End > 1 then
                     Item.Has_Comment := True;
                  end if;
                  First := Last + 1;
                  return;
               else
                  Item.Nested_Value := True;
                  Token_Writer.Write (The_Token, From, Item.Value_Str,
                            Item.Value_End);
                  First := Last + 1;
               end if;

            when Lexer.Comment_T =>
               Item.Has_Comment := True;
               Token_Writer.Write (The_Token, From, Item.Comment_Str,
                            Item.Comment_End);
               First := Last + 1;
            when Lexer.LParen_T =>
               LParens := LParens + 1;
               Token_Writer.Write (The_Token, From, Item.Value_Str,
                            Item.Value_End);
               First := Last + 1;

            when Lexer.RParen_T =>
               if LParens = 0 then
                  Lexer.Initialize (Remember_Line);
                  return;
               else
                  LParens := LParens - 1;
                  Token_Writer.Write (The_Token, From, Item.Value_Str,
                               Item.Value_End);
                  First := Last + 1;
               end if;

            when others =>
               Token_Writer.Write (The_Token, From, Item.Value_Str,
                            Item.Value_End);
               First := Last + 1;
         end case;
      end loop;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Item   : in out Argument;
      Into   : in out Lexer.StringPointer;
      Index  : in out Positive;
      Indent : in     Natural := 0) is
   begin
      Write (Declaration (Item), Into, Index, Indent);
      if Item.Has_Comma or Item.Has_Comment then
         Writer.New_Line (Into, Index);
      end if;
   end Write;

   --------------------------------------------------------------------
   --  LISTS
   --  (internal package)
   --  This is used to store a list of Argument Declarations. They will
   --  each be formatted with respect to the others in the same list.
   --------------------------------------------------------------------
   package Lists is new Lists_Generic (Argument);

   procedure Read
     (Item  :    out Lists.List;
      From  : in     Lexer.StringPointer;
      First : in out Positive);
   --  This procedure reads a list of Declarations. Unfortunately, this
   --  sort of procedure cannot be inherited because it uses the Lists
   --  package above, which can't use classwide types.

   procedure Format (Item : in out Lists.List);
   --  This procedure formats the given list of declarations based on
   --  their relationships to each other, and creates formatted output
   --  strings in each line's data structure. For Arguments, the arrows
   --  will all be vertically aligned, as will trailing comments.

   procedure Write
     (Item   : in     Lists.List;
      Into   : in out Lexer.StringPointer;
      Index  : in out Positive;
      Indent : in     Natural);
   --  This procedure writes out the given list of declarations to the
   --  output string.

   ----------
   -- Read --
   ----------

   procedure Read
     (Item  :    out Lists.List;
      From  : in     Lexer.StringPointer;
      First : in out Positive) is

      The_Token : Lexer.Token;
      Last      : Positive;
      Have      : Boolean  := True;
      Done      : Boolean  := False;
      Kind      : Lexer.Tokens;
      --  These variables are all used while retrieving a token. Most
      --  aren't really important to this method.

      Data : Argument;
      --  This is used to store up Argument info.
      Remember_Line : Natural;
   begin  --  Read
      Lists.Initialize (Item);
      --  Assuming we get an identifier, read it as a new Argument and
      --  add it to the list.
      loop
         Remember_Line := Lexer.getcurrentline;
         Lexer.GetNextToken (From, First, Last, The_Token, Have, Done);
         Kind := Lexer.GetTokenType (The_Token);
         if not Have or Done then
            return;
         elsif Kind /= Lexer.Name_T then
            Lexer.Initialize (Remember_Line);
            return;
         else
            --  Use the Argument Read method to read each line.
            Lexer.Initialize (Remember_Line);
            Read (Data, From, First);
            Lists.AddToRear (Item, Data);
            Reset (Data);
         end if;
      end loop;
   end Read;

   ------------
   -- Format --
   ------------

   procedure Format (Item : in out Lists.List) is

      Current : Lists.Position := Lists.First (Item);
      --  This is used to step through the declaration list.

      Current_Data : Argument;
      --  This is used for dereferencing pointers.

      Longest_Name,
      Longest_Value : Natural := 0;
      --  These track which entries have the longest fields.

      Longest_Line : Natural := 0;
      --  This is used to help line up comment entries.

      Pad : Natural := 0;
      --  This determines the amount of extra spaces to add to a line.

   begin  --  Format
      --  Skip an empty list.
      if Lists.IsPastEnd (Item, Current) then
         return;
      end if;

      --  Determine which entries have longest fields.
      loop
         Current_Data := Lists.Retrieve (Item, Current);

         --  Perform simple comparisons against current data, beginning
         --  with the name.
         --  Only move arrow if this actually has one!
         if Current_Data.Has_Value and
            Current_Data.Name_End > Longest_Name then
            Longest_Name := Current_Data.Name_End;
         end if;

         --  Must have an arrow, or else there's no value.
         if Current_Data.Has_Value and
            Current_Data.Value_End > Longest_Value then
            Longest_Value := Current_Data.Value_End;
         end if;

         exit when Lists.IsLast (Item, Current);
         Lists.GoAhead (Item, Current);
      end loop;
      Current := Lists.First (Item);

      --  Loop through and process all entries (exclude comments).
      --  Write names and necessary pads to line's output string.
      loop
         Current_Data := Lists.Retrieve (Item, Current);

         --  Finalize the name, value, and comment strings, so that
         --  they're easier to work with.
         Writer.Finalize (Current_Data.Name_Str,
                         Current_Data.Name_End);
         Writer.Finalize (Current_Data.Value_Str,
                         Current_Data.Value_End);
         Writer.Finalize (Current_Data.Comment_Str,
                         Current_Data.Comment_End);

         --  Append spaces to shorter entries, but only if a value
         if Current_Data.Has_Value then
            Pad := Longest_Name - Current_Data.Name_End;
            Writer.Write (Current_Data.Name_Str.all & Pad * " ",
                      Current_Data.Output_Str,
                      Current_Data.Output_End);
         else
            Writer.Write (Current_Data.Name_Str.all,
                      Current_Data.Output_Str,
                      Current_Data.Output_End);
         end if;

         --  Only add spaces to value if there's a value.
         if Current_Data.Has_Value then
            Pad := Longest_Value - Current_Data.Value_End;
            Token_Writer.Write (Lexer.Arrow_T, Current_Data.Output_Str,
                         Current_Data.Output_End);
            Writer.Write (Current_Data.Value_Str.all & Pad * " ",
                         Current_Data.Output_Str,
                         Current_Data.Output_End);
         end if;

         --  Now write the possible comma.
         if Current_Data.Has_Comma then
            Token_Writer.Write (Lexer.Comma_T, Current_Data.Output_Str,
                         Current_Data.Output_End);
         end if;

         --  Update the list with the modified entry.
         Lists.Replace (Item, Current_Data, Current);
         exit when Lists.IsLast (Item, Current);
         Lists.GoAhead (Item, Current);
      end loop;
      Current := Lists.First (Item);

      --  Now determine which line is longest (excluding comments). This
      --  is in order to figure out where to line up the comments.
      loop
         Current_Data := Lists.Retrieve (Item, Current);

         --  Compare against the "remembered" value again.
         if Current_Data.Output_End > Longest_Line then
            Longest_Line := Current_Data.Output_End;
         end if;

         exit when Lists.IsLast (Item, Current);
         Lists.GoAhead (Item, Current);
      end loop;
      Current := Lists.First (Item);

      --  Write out the comments to the correct spacing.
      loop
         Current_Data := Lists.Retrieve (Item, Current);

         --  Determine how much space to add.
         Pad := Longest_Line - Current_Data.Output_End;
         Writer.Indent (Pad, Current_Data.Output_Str,
                      Current_Data.Output_End);
         Writer.Write (Current_Data.Comment_Str.all,
                      Current_Data.Output_Str,
                      Current_Data.Output_End);

         --  Update the list with the modified entry.
         Lists.Replace (Item, Current_Data, Current);
         exit when Lists.IsLast (Item, Current);
         Lists.GoAhead (Item, Current);
      end loop;
   end Format;

   --------------------
   -- Ending_Comment --
   --------------------

   Ended_Comment : Boolean;

   function Ending_Comment return Boolean is
   begin
      return Ended_Comment;
   end Ending_Comment;

   -----------
   -- Write --
   -----------

   procedure Write
     (Item   : in     Lists.List;
      Into   : in out Lexer.StringPointer;
      Index  : in out Positive;
      Indent : in     Natural)
   is

      Current : Lists.Position := Lists.First (Item);
      --  This is used to step through the parameter list.

      Current_Data : Argument;
      --  This is used for dereferencing pointers.

   begin  --  Write
      --  Skip an empty list.
      if Lists.IsPastEnd (Item, Current) then
         return;
      end if;

      --  don't add new line for single item list
      if Lists.IsLast (Item, Current) then
         Current_Data := Lists.Retrieve (Item, Current);
         if not Current_Data.Has_Comma and not Continuation then
            Suppressing := True;
         end if;
      end if;
      Continuation := False;
      if not Suppressing and not Continuation then
         Writer.New_Line (Into, Index);
      end if;

      --  Loop through and write each entry.
      loop
         Current_Data := Lists.Retrieve (Item, Current);

         if Suppressing then
            --  Don't indent the first line.
            Write (Current_Data, Into, Index, 0);
            Suppressing := False;
         else
            Write (Current_Data, Into, Index, Indent);
         end if;
         if Lists.IsLast (Item, Current) then
            if Current_Data.Has_Comment then
               Ended_Comment := True;
            else
               Ended_Comment := False;
            end if;
            exit;
         end if;
         Lists.GoAhead (Item, Current);
      end loop;
   end Write;

   ---------------------
   -- Suppress_Indent --
   ---------------------

   procedure Suppress_Indent is
   begin
      Suppressing := True;
   end Suppress_Indent;

   -----------------------
   -- Continuation_List --
   -----------------------

   procedure Continuation_List is
   begin
      Continuation := True;
   end Continuation_List;

   --------------
   -- Reformat --
   --------------

   procedure Reformat
     (From   : in     Lexer.StringPointer;
      First  : in out Positive;
      Into   : in out Lexer.StringPointer;
      Index  : in out Positive;
      Indent : in     Natural := 0)
   is

      The_List : Lists.List;
      --  This stores all Parameter declaration lines that are
      --  formatted.

   begin  --  Reformat
      Read (The_List, From, First);
      Format (The_List);
      Write (The_List, Into, Index, Indent);

      --  Reset suppression of first-line indents.
      Suppressing := False;
   end Reformat;

end Declarations.Arguments;
