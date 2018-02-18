-----------------------------------------------------------------------
-- declarations-variables.adb
--
-- Author: Robert A. French
-- E-mail: rfrench99@hotmail.com
--
-- Description:
-- Defines the "Variables" subclass. This is basically used for places
-- that variables/constants could be declared, including functions,
-- procedures, or even record components.
--
-- Change log
-- 05/02/01 (mcc) : added ACCESS as a reserved word in the value section
-- 11/20/00 (mcc) : added RANGE and ALIASED as reserved word in type 
--                  section
-----------------------------------------------------------------------
with Ada.Strings.Fixed;
use  Ada.Strings.Fixed;
--with Windows;

with Lists_Generic;

with Lexer;
use type Lexer.StringPointer;
use type Lexer.Tokens;
with ada.text_io;
with Writer;
with Token_Writer;
with Settings;

-----------------------------------------------------------------------
-- DECLARATIONS.VARIABLES
-----------------------------------------------------------------------
package body Declarations.Variables is
   --------------------------------------------------------------------
   -- INITIALIZE
   --------------------------------------------------------------------
   procedure Initialize (Object : in out Variable) is
   begin
      Initialize(Declaration(Object));
      Object.Type_End      := 1;
      Object.Value_End     := 1;
      Object.Has_Comma     := False;
      Object.Has_Semicolon := False;
      Object.Has_Type      := False;
      Object.Has_Value     := False;
      Object.Is_Constant   := False;
      Object.Type_Str      := new String'(" ");
      Object.Value_Str     := new String'(" ");
   end Initialize;
   --------------------------------------------------------------------
   -- ADJUST
   --------------------------------------------------------------------
   procedure Adjust (Object : in out Variable) is
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
   procedure Finalize (Object : in out Variable) is
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
   -- The rules for reading a Variable declaration are basically:
   --   - Anything up to the colon (except a comment) is part of the
   --     name.
   --   - After the colon is the one place a reserved word (CONSTANT)
   --     can appear without terminating the Declaration. Actually,
   --     EXCEPTION can also appear here, but is treated as any other
   --     type name rather than by using a boolean to track it.
   --     ARRAY is handled similarly. (mcc)
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
   --   - NULL can be part of the default value.
   --------------------------------------------------------------------
   procedure Read (
         Item  :    out Variable;
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
         elsif Kind in Lexer.ReservedWords or else 
               Kind = Lexer.Comment_t then
            Lexer.Initialize(Remember_Line);
            return;
         end if;

         case Kind is
            when Lexer.Colon_T =>
               -- Now go on to read the type name.
               Item.Has_Type := True;
               First := Last + 1;
               exit;

            when Lexer.Comma_T =>
               -- Comma ends the declaration (assume multiline).
               Item.Has_Comma := True;
               First := Last + 1;
               Get_Comments(Lexer.GetLine(The_Token), From, First,
                            Item.Comment_Str, Item.Comment_End);
               return;

            when Lexer.Semicolon_T =>
               -- This ends the declaration too, goes to the next one.
               Item.Has_Semicolon := True;
               First := Last + 1;
               Get_Comments(Lexer.GetLine(The_Token), From, First,
                            Item.Comment_Str, Item.Comment_End);
               return;

            when Lexer.Comment_T =>
               Item.Has_Comment := True;
               First := Last + 1;
               Token_Writer.Write(The_Token, From, Item.Comment_Str,
                            Item.Comment_End);

            when others =>
               First := Last + 1;
               Token_Writer.Write(The_Token, From, Item.Name_Str,
                            Item.Name_End);
         end case;
      end loop;

      -- Found a colon, so now we first handle the two possible special
      -- cases, CONSTANT or EXCEPTION (and of course handle possible
      -- comments).
      loop
         Remember_Line := Lexer.Getcurrentline;
         Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
         Kind := Lexer.GetTokenType(The_Token);
         if not Have or Done then
            return;
         end if;

         case Kind is
            when Lexer.Comment_T =>
               Item.Has_Comment := True;
               Token_Writer.Write(The_Token, From, Item.Comment_Str,
                            Item.Comment_End);
               First := Last + 1;

            when Lexer.Constant_T =>
               -- Store this as a boolean.
               Item.Is_Constant := True;
               First := Last + 1;
               exit;

            when Lexer.Exception_T|Lexer.Array_T =>
               -- Use this as the actual type name.
               Token_Writer.Write(The_Token, From, Item.Type_Str,
                            Item.Type_End);
               First := Last + 1;
               exit;

            when others =>
               Lexer.Initialize(Remember_Line);
               exit;
         end case;
      end loop;

      -- From here, we revert to normal rules and start getting most
      -- tokens up to the assignment operator as part of the type
      -- name.
      loop
         Remember_Line := Lexer.Getcurrentline;
         Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
         Kind := Lexer.GetTokenType(The_Token);
         if not Have or Done then
            return;
         elsif Kind in Lexer.ReservedWords and then
            Kind /= Lexer.Range_T and then
            Kind /= Lexer.Aliased_T and then
            Kind /= Lexer.Renames_T and then
            Kind /= Lexer.Of_T then
            Lexer.Initialize(Remember_Line);
            return;
         end if;

         case Kind is
            when Lexer.Assignment_T =>
               -- Go get the default value after the assignment.
               Item.Has_Value := True;
               First := Last + 1;
               exit;

            when Lexer.Semicolon_T =>
               Item.Has_Semicolon := True;
               First := Last + 1;
               Get_Comments(Lexer.GetLine(The_Token), From, First,
                            Item.Comment_Str, Item.Comment_End);
               return;

            when Lexer.Comment_T =>
               Item.Has_Comment := True;
               First := Last + 1;
               Token_Writer.Write(The_Token, From, Item.Comment_Str,
                            Item.Comment_End);

            when others =>
               First := Last + 1;
               Token_Writer.Write(The_Token, From, Item.Type_Str,
                            Item.Type_End);
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
         elsif Kind in Lexer.ReservedWords and Kind /= Lexer.Access_T and
            Kind /= Lexer.Null_T and
            Kind /= Lexer.Others_T then
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
               First := Last + 1;
               Item.Has_Comment := True;
               Token_Writer.Write(The_Token, From, Item.Comment_Str,
                            Item.Comment_End);

            when others =>
               First := Last + 1;
               Token_Writer.Write(The_Token, From, Item.Value_Str,
                            Item.Value_End);
         end case;
      end loop;
   end Read;
   --------------------------------------------------------------------
   -- WRITE
   --------------------------------------------------------------------
   procedure Write (
         Item   : in out Variable;
         Into   : in out Lexer.StringPointer;
         Index  : in out Positive;
         Indent : in     Natural := 0
       ) is
      First               : Natural := 1;
      Current_Indentation : Natural := Indent;
      Comment_Location    : Natural;
      Double_Quotation_Count     : Natural := 0;
      Single_Quotation_Count     : Natural := 0;
   begin
      --Write(Declaration(Item), Into, Index, Indent);
      Writer.Indent(Indent, Into, Index);
      if Item.Colon_Location + Current_Indentation >
         Settings.Break_Lines then
         Writer.Write(Item.Output_Str.all(1..Item.Colon_Location-1),
            Into,Index);
         Writer.New_Line(Into, Index);
         Writer.Indent(Indent+Settings.Break_Indent, Into, Index);
         First := Item.Colon_Location;
         Current_Indentation := Indent+Settings.Break_Indent;
      end if;

      if Item.Assignment_Location - First + 1 + Current_Indentation >
         Settings.Break_Lines then
         Writer.Write(Item.Output_Str.all(First..Item.Assignment_Location-1),
            Into,Index);
         Writer.New_Line(Into, Index);
         Writer.Indent(Indent+Settings.Break_Indent, Into, Index);
         First := Item.Assignment_Location;
         Current_Indentation := Indent+Settings.Break_Indent;
      end if;

      if Item.Comment_Location = 0 then
         Comment_Location := Item.Output_End;
      else
         Comment_Location := integer'min(Item.Comment_Location-1,
            Item.Output_End);
      end if;
      
      for i in First..Comment_Location loop
         if Item.Output_Str.all(i) = '"' and 
            Single_Quotation_Count = 0 then
            Double_Quotation_Count := Double_Quotation_Count + 1;
         end if;
         
         if (Item.Output_Str.all(i) = ''' and 
            Double_Quotation_Count mod 2 = 0 and 
            Single_Quotation_Count = 0) then 
            if (i+2 <= Item.Output_Str.all'Last and then
                Item.Output_Str.all(i+2) = ''') then
               Single_Quotation_Count := 1;
            end if;
         elsif Item.Output_Str.all(i) = ''' and
            Single_Quotation_Count = 1 then
            Single_Quotation_Count := 0;
         end if;

         if i-First+1+Current_Indentation > Settings.Break_Lines and then
            (Double_Quotation_Count mod 2 = 0) and then
            (Single_Quotation_Count mod 2 = 0) and then
            Item.Output_Str.all(i) = ' ' and then
            index_non_blank(item.output_str.all(i..item.output_end)) > 0 then
            Writer.New_Line(Into, Index);
            Writer.Indent(Indent+Settings.Break_Indent, Into, Index);
            Current_Indentation := Indent+Settings.Break_Indent;
            First := i;
         else
            Writer.Write(Item.Output_Str.all(i) & "",Into,Index);
         end if;
      end loop;

      if Item.Output_End - First + 1 + Current_Indentation >
         Settings.Break_Lines and 
         Comment_Location+1<Item.Output_End then
         Writer.Indent(Indent, Into, Index);
      end if;

      if Comment_Location+1 <= Item.Output_End then
        Writer.Write(Item.Output_Str.all(Comment_Location+1..Item.Output_End),
            Into,Index);
        Writer.Chomp_Spaces(Into, Index);
      end if;
      
      if Item.Has_Semicolon or Item.Has_Comma then
         Writer.New_Line(Into, Index);
      end if;
   end Write;
   --------------------------------------------------------------------
   -- LISTS
   -- (internal package)
   -- This is used to store a list of Variable Declarations. They will
   -- each be formatted with respect to the others in the same list.
   --------------------------------------------------------------------
   package Lists is new Lists_Generic (Variable);
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

      Data : Variable;
      --| This is used to store up Variable info.
      Remember_Line : Natural;
      First_Decl : Boolean := True;
   begin  -- Read
      -- Assuming we get an identifier, read it as a new Variable and
      -- add it to the list.
      loop
         Remember_Line := Lexer.Getcurrentline;
         Lexer.GetNextToken(From, First, Last, The_Token, Have, Done);
         Kind := Lexer.GetTokenType(The_Token);
         if not Have or Done then
            return;
         elsif Kind /= Lexer.Name_T or else
            (First_Decl = false and then 
             Lexer.GetCurrentLine > Remember_Line + 1) then
            Lexer.Initialize(Remember_Line);
            return;
         else
            -- Read in each successive line.
            Lexer.Initialize(Remember_Line);
            Read(Data, From, First);
            Lists.AddToRear(Item, Data);
            Reset(Data);
         end if;
         First_Decl := False;
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

      Current_Data : Variable;
      --| This is used for dereferencing pointers.

      Longest_Name,
      Longest_Type,
      Longest_Value : Natural := 0;
      --| These track which entries have the longest fields.

      Longest_Line : Natural := 0;
      --| This is used to help line up comment entries.

      Pad : Natural := 0;
      --| This determines the amount of extra spaces to add to a line.

      Have_Constant : Boolean := False;
      --| This determines whether any entries are constants.

      Temp_Str : Lexer.StringPointer;
      --| This stores temporary strings while doing work.

      Const_Len : constant := 9;
      --| The length of the string "constant ".

   begin  -- Format
      -- Skip an empty list.
      if Lists.IsPastEnd(Item, Current) then
         return;
      end if;

      -- First loop through all of the entries and see if there are any
      -- constants.
      loop
         Current_Data := Lists.Retrieve(Item, Current);
         if Current_Data.Is_Constant then
            Have_Constant := True;
         end if;
         exit when Lists.IsLast(Item, Current);
         Lists.GoAhead(Item, Current);
      end loop;
      Current := Lists.First(Item);

      -- Now, if there was a constant, loop through and prepend spaces
      -- to type names of all entries without constant.
      if Have_Constant then
         loop
            Current_Data := Lists.Retrieve(Item, Current);

            -- Need to store away the type string, because we'll 
            -- temporarily need to replace it with "CONSTANT " or
            -- equivalent spaces.
            Temp_Str              := new String'
               (Current_Data.Type_Str(1 .. Current_Data.Type_End - 1));
            Current_Data.Type_End := 1;

            -- Prepend "CONSTANT " to types for constant definitions.
            if Current_Data.Is_Constant then
               Token_Writer.Write(Lexer.Constant_T, Current_Data.Type_Str,
                            Current_Data.Type_End);
            else
               Writer.Indent(Const_Len, Current_Data.Type_Str,
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

         -- Finalize the name, type, value, and comment strings, so that
         -- they're easier to work with.
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
            Current_Data.Colon_Location := Current_Data.Output_End;
            Writer.Write(Current_Data.Type_Str.all & Pad * " ",
                         Current_Data.Output_Str,
                         Current_Data.Output_End);
         end if;

         -- Only add spaces to value if there's a value.
         if Current_Data.Has_Value then
            Pad := Longest_Value - Current_Data.Value_End;
            Token_Writer.Write(Lexer.Assignment_T,
                         Current_Data.Output_Str,
                         Current_Data.Output_End);
            Current_Data.Assignment_Location := Current_Data.Output_End;
            Writer.Write(Current_Data.Value_Str.all & Pad * " ",
                         Current_Data.Output_Str,
                         Current_Data.Output_End);
         end if;

         -- Now write the possible semicolon.
         if Current_Data.Has_Semicolon then
            Token_Writer.Write(Lexer.Semicolon_T,
                         Current_Data.Output_Str,
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
         if Current_Data.Comment_Str.all /= "" then
            Writer.Indent(Pad, Current_Data.Output_Str,
               Current_Data.Output_End);

            Current_Data.Comment_Location := Current_Data.Output_End;
            Writer.Write(Current_Data.Comment_Str.all,
               Current_Data.Output_Str,
               Current_Data.Output_End);

            -- Update the list with the modified entry.
            Lists.Replace(Item, Current_Data, Current);
         end if;

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

      Current_Data : Variable;
      --| This is used for dereferencing pointers.

   begin  -- Write
      -- Skip an empty list.
      if Lists.IsPastEnd(Item, Current) then
         return;
      end if;

      -- Loop through and write each entry.
      loop
         Current_Data := Lists.Retrieve(Item, Current);
         Write(Current_Data, Into, Index, Indent);
         exit when Lists.IsLast(Item, Current);
         Lists.GoAhead(Item, Current);
      end loop;
   end Write;
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
      --| This stores all variable declaration lines that are 
      --| formatted.

   begin  -- Reformat
      Read(The_List, From, First);
      Format(The_List);
      Write(The_List, Into, Index, Indent);
   end Reformat;
   --------------------------------------------------------------------
end Declarations.Variables;
-----------------------------------------------------------------------           
