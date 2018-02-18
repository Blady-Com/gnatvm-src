-----------------------------------------------------------------------
-- declarations-parameters.adb
--
-- Author: Robert A. French
-- E-mail: rfrench99@hotmail.com
--
-- Description:
-- Defines the "Parameters" subclass. This is basically used for
-- anything that could possibly approximate parameters, whether for 
-- functions, procedures, packages, whatever.
-----------------------------------------------------------------------
with Ada.Strings.Fixed;
use  Ada.Strings.Fixed;

with Lists_Generic;
--with ada.text_io; -- for debugging

with Lexer;
use type Lexer.StringPointer;
use type Lexer.Tokens;
with Parm_Modes;
use type Parm_Modes.Mode;

with Writer;
with Token_Writer;
-----------------------------------------------------------------------
-- DECLARATIONS.PARAMETERS
-----------------------------------------------------------------------
package body Declarations.Parameters is
   --------------------------------------------------------------------
   -- VARIABLES
   --------------------------------------------------------------------
   Suppressing : Boolean := False;
   --| This indicates whether or not indentation will be suppressed
   --| when outputting the first line of a Parameter list.
   --------------------------------------------------------------------
   -- INITIALIZE
   --------------------------------------------------------------------
   procedure Initialize (Object : in out Parameter) is
   begin
      Initialize(Declaration(Object));
      Object.Type_End      := 1;
      Object.Value_End     := 1;
      Object.Has_Comma     := False;
      Object.Has_Semicolon := False;
      Object.Has_Type      := False;
      Object.Has_Value     := False;
      Object.Mode          := Parm_Modes.No_Mode;
      Object.Type_Str      := new String'(" ");
      Object.Value_Str     := new String'(" ");
   end Initialize;
   --------------------------------------------------------------------
   -- ADJUST
   --------------------------------------------------------------------
   procedure Adjust (Object : in out Parameter) is
      Temp : Lexer.StringPointer;
   begin
      Adjust(Declaration(Object));
      if Object.Type_Str /= null then
         Temp            := Object.Type_Str;
         Object.Type_Str := new String'(Temp.all);
      end if;
      if Object.Value_Str /= null then
         Temp             := Object.Value_Str;
         Object.Value_Str := new String'(Temp.all);
      end if;
   end Adjust;
   --------------------------------------------------------------------
   -- FINALIZE
   --------------------------------------------------------------------
   procedure Finalize (Object : in out Parameter) is
   begin
      if Object.Type_Str /= null then
         Free(Object.Type_Str);
      end if;
      if Object.Value_Str /= null then
         Free(Object.Value_Str);
      end if;
      Finalize(Declaration(Object));
   end Finalize;
   --------------------------------------------------------------------
   -- READ
   -- The rules for reading a Parameter declaration are basically:
   --   - Anything up to the colon (except a comment) is part of the
   --     name.
   --   - After the colon is the one place a reserved word (IN/OUT)
   --     or ACCESS
   --     can appear without terminating the Declaration. 
   --   - Anything up to the assignment is part of the type name.
   --   - Anything up to the semicolon is part of the default value.
   --   - In addition, a comma can appear where the colon is, but it
   --     will terminate the declaration like a semicolon.
   --   - Comments will be accumulated and concatenated in the comment
   --     string. 
   --   - A reserved word (except as specified above) will also end the
   --     declaration. Unlike commas or semicolons, the reserved word
   --     will still be the next available read from the input.
   --   - Embedded comments will all be concatenated to the comment
   --     string. After a comma or semicolon, however, only comments
   --     on the same line as the last token will be added. Otherwise,
   --     they will be available for the next read.
   --   - An unmatched right parenthesis will terminate the Parameter.
   --   - NULL can appear in the default value.
   --------------------------------------------------------------------
   procedure Read (
         Item  :    out Parameter;
         From  : in     Lexer.StringPointer;
         First : in out Positive
       ) is

      The_Token : Lexer.Token;
      Last      : Positive;
      Have      : Boolean  := True;
      Done      : Boolean  := False;
      Kind      : Lexer.Tokens;
      --| These variables are all used while retrieving a token. Most
      --| aren't really important to this method.

      LParens : Natural := 0;
      --| This tracks the number of left parentheses that are still
      --| unmatched. If it tries to go negative, then there is an
      --| unmatched right parenthesis.

      The_Mode : Parm_Modes.Mode;
      --| This stores the parameter mode of the retrieved Parameter.
      Remember_Line : Natural;
   begin  -- Read
      -- First start looking for a colon, comma, or semicolon (or a
      -- possible comment).
      loop
         Remember_Line := Lexer.Getcurrentline;
         Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
         Kind := Lexer.GetTokenType(The_Token);
         if not Have or Done then
            return;
         elsif Kind in Lexer.ReservedWords then
            Lexer.Initialize(Remember_Line);
            return;
         end if;

         case Kind is
            when Lexer.Colon_T =>
               -- Now start reading in the type name.
               Item.Has_Type := True;
               First         := Last + 1;
               exit;

            when Lexer.Comma_T =>
               -- Multiple declarations; this one's over.
               Item.Has_Comma := True;
               Get_Comments(Lexer.GetLine(The_Token), From, First,
                            Item.Comment_Str, Item.Comment_End);
               First         := Last + 1;
               return;

            when Lexer.Semicolon_T =>
               -- End of this declaration.
               Item.Has_Semicolon := True;
               Get_Comments(Lexer.GetLine(The_Token), From, First,
                            Item.Comment_Str, Item.Comment_End);
               First         := Last + 1;
               return;

            when Lexer.Comment_T =>
               Item.Has_Comment := True;
               Token_Writer.Write(The_Token, From, Item.Comment_Str,
                            Item.Comment_End);
               First         := Last + 1;

            when Lexer.LParen_T =>
               LParens := LParens + 1;
               Token_Writer.Write(The_Token, From, Item.Name_Str,
                            Item.Name_End);
               First := Last + 1;

            when Lexer.RParen_T =>
               -- If LParens is zero but we find a new right paren,
               -- unmatched, so put it back and end.
               if LParens = 0 then
                  Lexer.Initialize(Remember_Line);
                  return;
               else
                  LParens := LParens - 1;
                  Token_Writer.Write(The_Token, From, Item.Name_Str,
                               Item.Name_End);
                  First := Last + 1;
               end if;

            when others =>
               Token_Writer.Write(The_Token, From, Item.Name_Str,
                            Item.Name_End);
               First         := Last + 1;
         end case;
      end loop;

      -- Found a colon, so now we try to get parameter modes.
      Parm_Modes.Get(The_Mode, From, First);
      Item.Mode := The_Mode;

      -- From here, we revert to normal rules and start getting most
      -- tokens up to the assignment operator as part of the type
      -- name.
      loop
         Remember_Line := Lexer.getcurrentline;
         Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
         Kind := Lexer.GetTokenType(The_Token);
         if not Have or Done then
            return;
         elsif Kind in Lexer.ReservedWords then
            Lexer.Initialize(Remember_Line);
            return;
         end if;

         case Kind is
            when Lexer.Assignment_T =>
               -- Everything after this is part of the default value.
               Item.Has_Value := True;
               First         := Last + 1;
               exit;

            when Lexer.Semicolon_T =>
               Item.Has_Semicolon := True;
               Get_Comments(Lexer.GetLine(The_Token), From, First,
                            Item.Comment_Str, Item.Comment_End);
               First         := Last + 1;
               return;

            when Lexer.Comment_T =>
               Item.Has_Comment := True;
               Token_Writer.Write(The_Token, From, Item.Comment_Str,
                            Item.Comment_End);
               First         := Last + 1;

            when Lexer.LParen_T =>
               LParens := LParens + 1;
               Token_Writer.Write(The_Token, From, Item.Type_Str,
                            Item.Type_End);
               First := Last + 1;

            when Lexer.RParen_T =>
               if LParens = 0 then
                  Lexer.Initialize(Remember_Line);
                  return;
               else
                  LParens := LParens - 1;
                  Token_Writer.Write(The_Token, From, Item.Type_Str,
                               Item.Type_End);
                  First := Last + 1;
               end if;

            when others =>
               Token_Writer.Write(The_Token, From, Item.Type_Str,
                            Item.Type_End);
               First         := Last + 1;
         end case;
      end loop;

      -- Everything now up until the semicolon (or possible reserved
      -- word) is part of the default value.
      loop
         Remember_Line := Lexer.Getcurrentline;
         Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
         Kind := Lexer.GetTokenType(The_Token);
         if not Have or Done then
            return;
         elsif Kind in Lexer.ReservedWords and Kind /= Lexer.Null_T then
            Lexer.Initialize(Remember_Line);
            return;
         end if;

         case Kind is
            when Lexer.Semicolon_T =>
               Item.Has_Semicolon := True;
               First := Last + 1;
               Get_Comments(Lexer.GetLine(The_Token), From, First,
                            Item.Comment_Str, Item.Comment_End);
               return;

            when Lexer.Comment_T =>
               Item.Has_Comment := True;
               Token_Writer.Write(The_Token, From, Item.Comment_Str,
                            Item.Comment_End);
               First := Last + 1;

            when Lexer.LParen_T =>
               LParens := LParens + 1;
               Token_Writer.Write(The_Token, From, Item.Value_Str,
                            Item.Value_End);
               First := Last + 1;

            when Lexer.RParen_T =>
               if LParens = 0 then
                  Lexer.Initialize(Remember_Line);
                  return;
               else
                  LParens := LParens - 1;
                  Token_Writer.Write(The_Token, From, Item.Value_Str,
                               Item.Value_End);
                  First := Last + 1;
               end if;

            when others =>
               First := Last + 1;
               Token_Writer.Write(The_Token, From, Item.Value_Str,
                            Item.Value_End);
         end case;
      end loop;
   end Read;
   --------------------------------------------------------------------
   -- WRITE
   -- This writes out a newline after the declaration if it has either
   -- a comma or a semicolon.
   --------------------------------------------------------------------
   procedure Write (
         Item   : in out Parameter;
         Into   : in out Lexer.StringPointer;
         Index  : in out Positive;
         Indent : in     Natural := 0
       ) is
   begin
      Write(Declaration(Item), Into, Index, Indent);
      if Item.Has_Semicolon or Item.Has_Comma then
         Writer.New_Line(Into, Index);
      end if;
   end Write;
   --------------------------------------------------------------------
   -- LISTS
   -- (internal package)
   -- This is used to store a list of Parameter Declarations. They will
   -- each be formatted with respect to the others in the same list.
   --------------------------------------------------------------------
   package Lists is new Lists_Generic (Parameter);
   --------------------------------------------------------------------
   -- READ
   -- (internal procedure)
   -- This procedure reads a list of Declarations. Unfortunately, this
   -- sort of procedure cannot be inherited because it uses the Lists
   -- package above, which can't use classwide types.
   --------------------------------------------------------------------
   procedure Read (
         Item  :    out Lists.List;
         From  : in     Lexer.StringPointer;
         First : in out Positive
       ) is

      The_Token : Lexer.Token;
      Last      : Positive;
      Have      : Boolean  := True;
      Done      : Boolean  := False;
      Kind      : Lexer.Tokens;
      --| These variables are all used while retrieving a token. Most
      --| aren't really important to this method.

      Data : Parameter;
      --| This is used to store up Parameter info.
      Remember_Line : Natural;
   begin  -- Read
      -- Assuming we get an identifier, read it as a new Parameter and
      -- add it to the list.
      loop
         Remember_Line := Lexer.Getcurrentline;
         Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
         Kind := Lexer.GetTokenType(The_Token);
         if not Have or Done then
            return;
         elsif Kind /= Lexer.Name_T then
            Lexer.Initialize(Remember_Line);
            return;
         else
            -- Read each line in and add it to the list.
            Lexer.Initialize(Remember_Line);
            Read(Data, From, First);
            Lists.AddToRear(Item, Data);
            Reset(Data);
         end if;
      end loop;
   end Read;
   --------------------------------------------------------------------
   -- FORMAT
   -- (internal procedure)
   -- This procedure formats the given list of declarations based on
   -- their relationships to each other, and creates formatted output
   -- strings in each line's data structure.
   --------------------------------------------------------------------
   procedure Format (Item : in out Lists.List) is

      Current : Lists.Position := Lists.First(Item);
      --| This is used to step through the declaration list.

      Current_Data : Parameter;
      --| This is used for dereferencing pointers.

      Longest_Name,
      Longest_Type,
      Longest_Value : Natural := 0;
      --| These track which entries have the longest fields.

      Longest_Line : Natural := 0;
      --| This is used to help line up comment entries.

      Pad : Natural := 0;
      --| This determines the amount of extra spaces to add to a line.

      Have_Modes : Boolean := False;
      --| This determines whether any entries have specified parameter
      --| modes.

      Temp_Str : Lexer.StringPointer;
      --| This stores temporary strings while doing work.

      Const_Len  : constant := 9;
      In_Len     : constant := 3;
      Out_Len    : constant := 4;
      In_Out_Len : constant := 7;
      Access_Len : constant := 7;
      --| The length of the strings "constant ", "in ", "out ", and 
      --| "in out ".

   begin  -- Format
      -- Skip an empty list.
      if Lists.IsPastEnd(Item, Current) then
         return;
      end if;

      -- First loop through all of the entries and see if there are any
      -- that have specified modes.
      loop
         Current_Data := Lists.Retrieve(Item, Current);
         if Current_Data.Mode /= Parm_Modes.No_Mode then
            Have_Modes := True;
         end if;
         exit when Lists.IsLast(Item, Current);
         Lists.GoAhead(Item, Current);
      end loop;
      Current := Lists.First(Item);

      -- Now, if there was a parameter mode, loop through and insert 
      -- appropriate spaces and/or reserved words (IN, OUT).
      if Have_Modes then
         loop
            Current_Data := Lists.Retrieve(Item, Current);

            -- We're temporarily storing away the type name, and 
            -- resetting it with any needed parameter modes. Then we'll
            -- append the original type names again.
            Temp_Str              := new String'
               (Current_Data.Type_Str(1 .. Current_Data.Type_End - 1));
            Current_Data.Type_End := 1;

            -- Prepend appropriate spaces if necessary.
            if Current_Data.Mode = Parm_Modes.No_Mode then
               Writer.Indent(In_Out_Len, Current_Data.Type_Str,
                            Current_Data.Type_End);
            elsif Current_Data.Mode = Parm_Modes.Out_Mode then
               Writer.Indent(In_Len, Current_Data.Type_Str,
                            Current_Data.Type_End);
            end if;

            -- Put in reserved words (IN, IN OUT).
            if Current_Data.Mode = Parm_Modes.In_Mode or
               Current_Data.Mode = Parm_Modes.In_Out_Mode then
               Token_Writer.Write(Lexer.In_T, Current_Data.Type_Str,
                            Current_Data.Type_End);
            end if;

            -- Put OUT if necessary.
            if Current_Data.Mode = Parm_Modes.Out_Mode or
               Current_Data.Mode = Parm_Modes.In_Out_Mode then
               Token_Writer.Write(Lexer.Out_T, Current_Data.Type_Str,
                            Current_Data.Type_End);
            end if;

            -- Put ACCESS if necessary
            if Current_Data.Mode = Parm_Modes.Access_Mode then
               Token_Writer.Write(Lexer.Access_T, Current_Data.Type_Str,
                            Current_Data.Type_End);
            end if;
            
            -- Trailing spaces if needed.
            if Current_Data.Mode = Parm_Modes.In_Mode then
               Writer.Indent(Out_Len, Current_Data.Type_Str,
                            Current_Data.Type_End);
            end if;

            Writer.Write(Temp_Str.all, Current_Data.Type_Str,
                         Current_Data.Type_End);

            -- Update the list with the modified entry.
            Lists.Replace(Item, Current_Data, Current);
            exit when Lists.IsLast(Item, Current);
            Lists.GoAhead(Item, Current);
         end loop;
         Current := Lists.First(Item);
      end if;

      -- Determine which entries have longest fields.
      loop
         Current_Data := Lists.Retrieve(Item, Current);

         -- Perform simple comparisons against current data, beginning
         -- with the name.
         if Current_Data.Name_End > Longest_Name then
            Longest_Name := Current_Data.Name_End;
         end if;

         -- Must have a colon, or else there's no type.
         if Current_Data.Has_Type and
            Current_Data.Type_End > Longest_Type then
            Longest_Type := Current_Data.Type_End;
         end if;

         -- Must have an assignment, or there's no default value.
         if Current_Data.Has_Value and
            Current_Data.Value_End > Longest_Value then
            Longest_Value := Current_Data.Value_End;
         end if;

         exit when Lists.IsLast(Item, Current);
         Lists.GoAhead(Item, Current);
      end loop;
      Current := Lists.First(Item);

      -- Loop through and process all entries (exclude comments).
      -- Write names and necessary pads to line's output string.
      loop
         Current_Data := Lists.Retrieve(Item, Current);

         -- Finalize the name, type, value, and comment strings, so 
         -- that they're easier to work with.
         Writer.Finalize(Current_Data.Name_Str,
                         Current_Data.Name_End);
         Writer.Finalize(Current_Data.Type_Str,
                         Current_Data.Type_End);
         Writer.Finalize(Current_Data.Value_Str,
                         Current_Data.Value_End);
         Writer.Finalize(Current_Data.Comment_Str,
                         Current_Data.Comment_End);

         -- Append spaces to shorter entries.
         Pad := Longest_Name - Current_Data.Name_End;
         Writer.Write(Current_Data.Name_Str.all & Pad * " ",
                      Current_Data.Output_Str,
                      Current_Data.Output_End);

         -- If there's a comma, write it. Everything else should be
         -- false, except maybe a comment.
         if Current_Data.Has_Comma then
            Token_Writer.Write(Lexer.Comma_T, Current_Data.Output_Str,
                         Current_Data.Output_End);
         end if;

         -- Only add spaces to type if there's a type.
         if Current_Data.Has_Type then
            Pad := Longest_Type - Current_Data.Type_End;
            Token_Writer.Write(Lexer.Colon_T, Current_Data.Output_Str,
                         Current_Data.Output_End);
            Writer.Write(Current_Data.Type_Str.all & Pad * " ",
                         Current_Data.Output_Str,
                         Current_Data.Output_End);
         end if;

         -- Only add spaces to value if there's a value.
         if Current_Data.Has_Value then
            Pad := Longest_Value - Current_Data.Value_End;
            Token_Writer.Write(Lexer.Assignment_T, Current_Data.Output_Str,
                         Current_Data.Output_End);
            Writer.Write(Current_Data.Value_Str.all & Pad * " ",
                         Current_Data.Output_Str,
                         Current_Data.Output_End);
         end if;

         -- Now write the possible semicolon.
         if Current_Data.Has_Semicolon then
            Token_Writer.Write(Lexer.Semicolon_T, Current_Data.Output_Str,
                         Current_Data.Output_End);
         end if;

         -- Update the list with the modified entry.
         Lists.Replace(Item, Current_Data, Current);
         exit when Lists.IsLast(Item, Current);
         Lists.GoAhead(Item, Current);
      end loop;
      Current := Lists.First(Item);

      -- Now determine which line is longest (excluding comments). This
      -- is in order to figure out where to line up the comments.
      loop
         Current_Data := Lists.Retrieve(Item, Current);

         -- Compare against the "remembered" value again.
         if Current_Data.Output_End > Longest_Line then
            Longest_Line := Current_Data.Output_End;
         end if;

         exit when Lists.IsLast(Item, Current);
         Lists.GoAhead(Item, Current);
      end loop;
      Current := Lists.First(Item);

      -- Write out the comments to the correct spacing.
      loop
         Current_Data := Lists.Retrieve(Item, Current);

         -- Determine how much space to add.
         Pad := Longest_Line - Current_Data.Output_End;
         Writer.Indent(Pad, Current_Data.Output_Str,
                      Current_Data.Output_End);
         Writer.Write(Current_Data.Comment_Str.all,
                      Current_Data.Output_Str,
                      Current_Data.Output_End);

         -- Update the list with the modified entry.
         Lists.Replace(Item, Current_Data, Current);
         exit when Lists.IsLast(Item, Current);
         Lists.GoAhead(Item, Current);
      end loop;
   end Format;
   --------------------------------------------------------------------
   -- WRITE
   -- (internal procedure)
   -- This procedure writes out the given list of declarations to the
   -- output string.
   --------------------------------------------------------------------
   procedure Write (
         Item   : in     Lists.List;
         Into   : in out Lexer.StringPointer;
         Index  : in out Positive;
         Indent : in     Natural
       ) is

      Current : Lists.Position := Lists.First(Item);
      --| This is used to step through the parameter list.

      Current_Data : Parameter;
      --| This is used for dereferencing pointers.

   begin  -- Write
      -- Skip an empty list.
      if Lists.IsPastEnd(Item, Current) then
         return;
      end if;

         -- Loop through and write each entry.
         loop
            Current_Data := Lists.Retrieve(Item, Current);

            if Suppressing then
               -- Don't indent the first line.
               Write(Current_Data, Into, Index, 0);
               Suppressing := False;
            else
               Write(Current_Data, Into, Index, Indent);
            end if;
            exit when Lists.IsLast(Item, Current);
            Lists.GoAhead(Item, Current);
         end loop;
         if Current_Data.Has_Comment then
            Writer.New_Line(Into, Index);
            Writer.Indent(Indent, Into, Index);
         end if;
   end Write;
   --------------------------------------------------------------------
   -- SUPPRESS_INDENT
   --------------------------------------------------------------------
   procedure Suppress_Indent is
   begin
      Suppressing := True;
   end Suppress_Indent;
   --------------------------------------------------------------------
   -- REFORMAT
   --------------------------------------------------------------------
   procedure Reformat (
      From   : in     Lexer.StringPointer;
      First  : in out Positive;
      Into   : in out Lexer.StringPointer;
      Index  : in out Positive;
      Indent : in     Natural := 0
     ) is

      The_List : Lists.List;
      --| This stores all Parameter declaration lines that are 
      --| formatted.

   begin  -- Reformat
      Read(The_List, From, First);
      Format(The_List);
      Write(The_List, Into, Index, Indent);

      -- Reset suppression of first-line indents.
      Suppressing := False;
   end Reformat;
   --------------------------------------------------------------------
end Declarations.Parameters;
-----------------------------------------------------------------------     
