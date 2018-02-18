------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M . C O D E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1998-2013, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by --
-- AdaCore - http://www.adacore.com                                         --
--                                                                          --
-- This work is partially  based on A#, an Ada  compiler for .NET by  Prof. --
-- Martin C. Carlisle of the United States Air Force Academy.               --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Table;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Maps;        use Ada.Strings.Maps;
with Ada.Text_IO;             use Ada.Text_IO; --  debugging
with JVM.Emit.CIL;            use JVM.Emit.CIL;
with JVM_File;                use JVM_File;
with JVM.Info;                use JVM.Info;
with JVM.Pool;                use JVM.Pool;
with Opt;                     use Opt;
with Output;                  use Output;
with Sinput;                  use Sinput;
with Stringt;                 use Stringt;

package body JVM.Code is

   package Code_Table is new GNAT.Table (
     Table_Component_Type => Instruction,
     Table_Index_Type     => Instr_Id,
     Table_Low_Bound      => Instr_Id'First,
     Table_Initial        => 10_000,
     Table_Increment      => 100);

   Next_Code_Index : Instr_Id := Null_Instr + 1;
   --  The table index of the next available instruction slot

   Free_List : Instr_Id := Null_Instr;
   --  A list of freed Instruction records that can be reused

   Empty_Instr : constant Instruction :=
     (Op => UNUSED24, Next => Null_Instr);
   --  The default value of an Instruction record allocated by New_Instr

   package Handler_Table is new GNAT.Table (
     Table_Component_Type => Handler_Entry,
     Table_Index_Type     => Handler_Id,
     Table_Low_Bound      => Handler_Id'First,
     Table_Initial        => 10_000,
     Table_Increment      => 100);

   Next_Handler_Index : Handler_Id := Null_Handler + 1;
   --  The table index of the next available handler entry slot

   Free_Handler_List : Handler_Id := Null_Handler;
   --  A list of freed Handler_Entry records that can be reused

   -------------------------------------
   -- Instruction Sequence Operations --
   -------------------------------------

   function Count_Sequence (Seq : Code_Sequence) return Natural is
   begin
      return Seq.Count;
   end Count_Sequence;

   --------------------
   -- Start_Sequence --
   --------------------

   procedure Start_Sequence (Seq : in out Code_Sequence) is
   begin
      pragma Assert (Seq.First = Null_Instr);

      Seq := Empty_Sequence;
   end Start_Sequence;

   -------------------
   -- Free_Sequence --
   -------------------

   procedure Free_Sequence (Seq : in out Code_Sequence) is
   begin
      if Seq.Last /= Null_Instr then
         Code_Table.Table (Seq.Last).Next := Free_List;
         Free_List := Seq.First;
         Seq := Empty_Sequence;
      end if;
   end Free_Sequence;

   -------------------------
   -- Print_Code_Sequence --
   -------------------------

   procedure Print_Code_Sequence (Seq : Code_Sequence) is
      Code : Instr_Id := Seq.First;
   begin
      while Code /= Null_Instr loop
         Put_Line (CIL_Operation'Image (Code_Table.Table (Code).Op));
         Code := Code_Table.Table (Code).Next;
      end loop;
   end Print_Code_Sequence;

   -----------
   -- First --
   -----------

   function First (Seq : Code_Sequence) return Instr_Id is
   begin
      return Seq.First;
   end First;

   ----------
   -- Last --
   ----------

   function Last (Seq : Code_Sequence) return Instr_Id is
   begin
      return Seq.Last;
   end Last;

   ------------
   -- Append --
   ------------

   procedure Append (Seq : in out Code_Sequence; Instr : Instruction) is
   begin
      Append (Seq, New_Instr);
      Code_Table.Table (Seq.Last) := Instr;
   end Append;

   ------------
   -- Insert --
   ------------

   procedure Insert (New_Instr : Instr_Id; After : Instr_Id) is
   begin
      pragma Assert (Code_Table.Table (New_Instr).Next = Null_Instr);

      Code_Table.Table (After).Next := New_Instr;
   end Insert;

   ------------
   -- Append --
   ------------

   procedure Append (Seq : in out Code_Sequence; Id : Instr_Id) is
   begin
      pragma Assert (Code_Table.Table (Id).Next = Null_Instr);

      if Seq.First = Null_Instr then
         Seq.First := Id;
      else
         Code_Table.Table (Seq.Last).Next := Id;
      end if;

      Seq.Last  := Id;
      Seq.Count := Seq.Count + 1;
   end Append;

   ------------
   -- Attach --
   ------------

   procedure Attach (First_Seq, Second_Seq : in out Code_Sequence) is
   begin
      pragma Assert (First_Seq.First /= Second_Seq.First);

      if First_Seq.First = Null_Instr then
         First_Seq := Second_Seq;
      else
         Code_Table.Table (First_Seq.Last).Next := Second_Seq.First;
         First_Seq.Last := Second_Seq.Last;
      end if;
      Second_Seq := Empty_Sequence;
   end Attach;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (First_Seq, Second_Seq : in out Code_Sequence) is
   begin
      pragma Assert (First_Seq.First /= Second_Seq.First);

      if Second_Seq.First = Null_Instr then
         Second_Seq := First_Seq;

      elsif First_Seq.First = Null_Instr then
         return;

      else
         Code_Table.Table (First_Seq.Last).Next := Second_Seq.First;
         Second_Seq.First := First_Seq.First;
      end if;
      First_Seq := Empty_Sequence;
   end Prepend;

   ---------------
   -- New_Instr --
   ---------------

   function New_Instr return Instr_Id is
      Freed_Id : Instr_Id := Free_List;

   begin
      if Freed_Id /= Null_Instr then
         Free_List := Code_Table.Table (Freed_Id).Next;
      else
         Freed_Id := Next_Code_Index;
         Next_Code_Index := Next_Code_Index + 1;
         Code_Table.Set_Last (Next_Code_Index);
      end if;

      Code_Table.Table (Freed_Id) := Empty_Instr;
      return Freed_Id;
   end New_Instr;

   ---------------
   -- New_Instr --
   ----------------

   function New_Instr (Instr : Instruction) return Instr_Id is
      New_Instr_Id : constant Instr_Id := New_Instr;
   begin
      Code_Table.Table (New_Instr_Id) := Instr;
      return New_Instr_Id;
   end New_Instr;

   ---------
   -- Get --
   ---------

   function Get (Id : Instr_Id) return Instruction is
   begin
      return Code_Table.Table (Id);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get (Id : Instr_Id; Instr : out Instruction) is
   begin
      Instr := Code_Table.Table (Id);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (Id : Instr_Id; Instr : Instruction) is
      Save_Next : constant Instr_Id := Code_Table.Table (Id).Next;

   begin
      Code_Table.Table (Id) := Instr;
      Code_Table.Table (Id).Next := Save_Next;
   end Put;

   -----------------------------------
   -- Switch Instruction Operations --
   -----------------------------------

   -----------------------
   -- Start_Switch_List --
   -----------------------

   procedure Start_Switch_List (List : in out Switch_List) is
   begin
      pragma Assert (List = null);

      List := new Switch_List_Record;
   end Start_Switch_List;

   ----------------------
   -- Free_Switch_List --
   ----------------------

   procedure Free_Switch_List (List : in out Switch_List) is
      pragma Unreferenced (List);

   begin
      null;  -- TBD ???
   end Free_Switch_List;

   ---------------------
   -- Add_Switch_Pair --
   ---------------------

   procedure Add_Switch_Pair
     (List         : in out Switch_List;
      Match_Value  : Int_32;
      Switch_Label : Label_Id)
   is
      New_Pair  : constant Switch_Pair_Id := new Switch_Pair;
      Curr_Pair : Switch_Pair_Id;
      Prev_Pair : Switch_Pair_Id;

   begin
      pragma Assert (List /= null);

      New_Pair.Match_Value := Match_Value;
      New_Pair.Switch_Lbl  := Switch_Label;

      --  This is the first list element

      if List.First_Pair = Null_Switch_Pair then
         List.First_Pair    := New_Pair;
         List.Last_Pair     := New_Pair;
         New_Pair.Next_Pair := Null_Switch_Pair;

      --  Add the new pair to the end of the list

      elsif Match_Value > List.Last_Pair.Match_Value then
         List.Last_Pair.Next_Pair := New_Pair;
         New_Pair.Next_Pair       := Null_Switch_Pair;
         List.Last_Pair           := New_Pair;

      --  Add the new pair to the beginning of the list

      elsif Match_Value < List.First_Pair.Match_Value then
         New_Pair.Next_Pair := List.First_Pair;
         List.First_Pair    := New_Pair;

      --  Insert the new pair in front of the first element with
      --  a greater match value.

      else
         Curr_Pair := List.First_Pair;
         while Match_Value > Curr_Pair.Match_Value loop
            Prev_Pair := Curr_Pair;
            Curr_Pair := Curr_Pair.Next_Pair;
         end loop;

         pragma Assert (Match_Value /= Curr_Pair.Match_Value);

         Prev_Pair.Next_Pair := New_Pair;
         New_Pair.Next_Pair  := Curr_Pair;
      end if;

      List.Pair_Count := List.Pair_Count + 1;
   end Add_Switch_Pair;

   -----------------------
   -- Switch_Pair_Count --
   -----------------------

   function Switch_Pair_Count (List : Switch_List) return U4 is
   begin
      return List.Pair_Count;
   end Switch_Pair_Count;

   ----------------
   -- First_Pair --
   ----------------

   function First_Pair (List : Switch_List) return Switch_Pair_Id is
   begin
      return List.First_Pair;
   end First_Pair;

   ---------------
   -- Last_Pair --
   ---------------

   function Last_Pair (List : Switch_List) return Switch_Pair_Id is
   begin
      return List.Last_Pair;
   end Last_Pair;

   ---------------
   -- Next_Pair --
   ---------------

   function Next_Pair (Pair : Switch_Pair_Id) return Switch_Pair_Id is
   begin
      pragma Assert (Pair /= Null_Switch_Pair);

      return Pair.Next_Pair;
   end Next_Pair;

   -----------------
   -- Match_Value --
   -----------------

   function Match_Value (Pair : Switch_Pair_Id) return Int_32 is
   begin
      pragma Assert (Pair /= Null_Switch_Pair);

      return Pair.Match_Value;
   end Match_Value;

   ------------------
   -- Switch_Label --
   ------------------

   function Match_Label (Pair : Switch_Pair_Id) return Label_Id is
   begin
      pragma Assert (Pair /= Null_Switch_Pair);

      return Pair.Switch_Lbl;
   end Match_Label;

   ----------------------------------
   -- Exception Handler Operations --
   ----------------------------------

   --------------------
   -- Start_Sequence --
   --------------------

   procedure Start_Sequence (Seq : in out Handler_Sequence) is
   begin
      pragma Assert (Seq.First = Null_Handler);

      Seq := (First => Null_Handler, Last => Null_Handler);
   end Start_Sequence;

   -------------------
   -- Free_Sequence --
   -------------------

   procedure Free_Sequence (Seq : in out Handler_Sequence) is
   begin
      if Seq.Last /= Null_Handler then
         Handler_Table.Table (Seq.Last).Next := Free_Handler_List;
         Free_Handler_List := Seq.First;
         Seq := (First => Null_Handler, Last => Null_Handler);
      end if;
   end Free_Sequence;

   -----------------------
   -- New_Handler_Entry --
   -----------------------

   function New_Handler_Entry
     (Exc_Class       : Pool_Id;
      Start_Lbl       : Label_Id;
      End_Lbl         : Label_Id;
      Handler_Lbl     : Label_Id;
      End_Handler_Lbl : Label_Id;
      Kind            : Handler_Kind;
      Filter_Lbl      : Label_Id) return Handler_Id
   is
      Freed_Handler : Handler_Id := Free_Handler_List;
   begin
      if Freed_Handler /= Null_Handler then
         Free_Handler_List := Handler_Table.Table (Freed_Handler).Next;
      else
         Freed_Handler := Next_Handler_Index;
         Next_Handler_Index := Next_Handler_Index + 1;
         Handler_Table.Set_Last (Next_Handler_Index);
      end if;

      Handler_Table.Table (Freed_Handler)
        := (Exc_Class, Start_Lbl, End_Lbl, Handler_Lbl,
            End_Handler_Lbl, Kind, Filter_Lbl, Null_Handler);

      return Freed_Handler;
   end New_Handler_Entry;

   ------------
   -- Append --
   ------------

   procedure Append (Seq : in out Handler_Sequence; Handler : Handler_Id) is
   begin
      pragma Assert (Handler_Table.Table (Handler).Next = Null_Handler);

      if Seq.First = Null_Handler then
         Seq.First := Handler;
      else
         Handler_Table.Table (Seq.Last).Next := Handler;
      end if;
      Seq.Last := Handler;
   end Append;

   -----------
   -- First --
   -----------

   function First (Seq : Handler_Sequence) return Handler_Id is
   begin
      return Seq.First;
   end First;

   ----------
   -- Next --
   ----------

   function Next (Id : Handler_Id) return Handler_Id is
   begin
      return Handler_Table.Table (Id).Next;
   end Next;

   ----------
   -- Last --
   ----------

   function Last (Seq : Handler_Sequence) return Handler_Id is
   begin
      return Seq.Last;
   end Last;

   ---------
   -- Get --
   ---------

   function Get (Id : Handler_Id) return Handler_Entry is
   begin
      return Handler_Table.Table (Id);
   end Get;

   -----------
   -- Image --
   -----------

   function Image (Op : CIL_Operation) return String is
      function Keyword_Translate (S : String) return String;

      function Keyword_Translate (S : String) return String is
      begin
         if S (S'Last - 1 .. S'Last) = ".k" then
            return S (S'First .. S'Last - 2);
         else
            return S;
         end if;
      end Keyword_Translate;
   begin
      return Keyword_Translate
        (Translate
          (Source  => To_Lower (CIL_Operation'Image (Op)),
           Mapping => To_Mapping ("_", ".")));
   end Image;

   -----------------------
   -- Print_Instruction --
   -----------------------

   procedure Print_Instruction (Instr : Instruction) is

      procedure Print_Name (N : Name_Id);
      --  Print string of name

      function To_String (L : Label_Id) return String;
      --  Convert a label number to string

      ---------------
      -- To_String --
      ---------------

      function To_String (L : Label_Id) return String is
         Str : constant String := Label_Number (L)'Img;

      begin
         return "L" & Str (Str'First + 1 .. Str'Last);
      end To_String;

      ----------------
      -- Print_Name --
      ----------------

      procedure Print_Name (N : Name_Id) is
      begin
         if N = No_Name then
            Write_Str ("<no name>");
         else
            Write_Name (N);
         end if;
      end Print_Name;

   --  Start of processing for Print_Instruction

   begin
      --  Generate .line directives

      if Instr.Op = NOP
        and then Instr.Line_Number /= No_Location
        and then Opt.Debugger_Level > 0
      then
         if Instr.Label_Def /= Null_Label then
            Write_Str (To_String (Instr.Label_Def) & ":");
         end if;

         Write_Str (" .line");
         Write_Str
           (Integer'Image
             (Integer
               (Sinput.Get_Physical_Line_Number (Instr.Line_Number))));
         Write_Str
           (" '" &
            Translate_File_Name
              (Name_Id
                (Full_Debug_Name
                  (Get_Source_File_Index (Instr.Line_Number)))) &
            "'");
         Write_Eol;

      elsif Instr.Op = NOP
        and then Instr.Label_Def /= Null_Label
      then
         Write_Str (To_String (Instr.Label_Def) & ":");

      elsif Instr.Op = NOP
        and then Instr.Annotation /= No_Annotation
      then
         Write_Str (".annotation " & Instr.Annotation'Img);

      else
         Write_Str ("     " & Instr.Op'Img);

         case Instr.Op is

            when LDSFLD | LDFLD | LDFLDA | STFLD | STSFLD =>
               Write_Str  (" ");

               if Instr.Class /= Null_Class then
                  Print_Name (Name (Instr.Class));
                  Write_Str  ("::");
               end if;

               if Instr.Field /= Null_Field then
                  Print_Name (Name (Instr.Field));
               else
                  Write_Str ("<<null-field>> ???");
               end if;

            --  Conditional jumps

            when BRFALSE_S | BRTRUE_S | BEQ_S    | BGE_S    | BGT_S    |
                 BLE_S     | BLT_S    | BNE_UN_S | BGE_UN_S | BGT_UN_S |
                 BLE_UN_S  | BLT_UN_S |

                 BRFALSE   | BRTRUE   | BEQ      | BGE      | BGT      |
                 BLE       | BLT      | BNE_UN   | BGE_UN   | BGT_UN   |
                 BLE_UN    | BLT_UN   =>
               Write_Str (" " & To_String (Instr.Target));

            --  Known target unconditional jumps

            when BR_S | BR  | JMP =>
               Write_Str (To_String (Instr.Target));

            when LEAVE =>
               if Instr.Target /= Null_Label then
                  Write_Str (" " & To_String (Instr.Target));
               end if;

            when NEWARR =>
               if Instr.Dimensions > 1 then
                  Write_Str (Instr.Dimensions'Img);
                  Write_Str (" dimensions");
               end if;

            when SWITCH =>
               declare
                  Switch_Pair : Switch_Pair_Id;

               begin
                  Switch_Pair := First_Pair (Instr.Switch_Pairs);
                  Write_Str (" ");
                  Write_Str (To_String (Match_Label (Switch_Pair)));

                  loop
                     Switch_Pair := Next_Pair (Switch_Pair);
                     exit when Switch_Pair = Null_Switch_Pair;

                     Write_Str (", ");
                     Write_Str (To_String (Match_Label (Switch_Pair)));
                  end loop;

                  Write_Str (", ");
                  Write_Str (To_String (Instr.Default_Label));
               end;

            when others =>
               if Instr.Local /= Null_Local_Var then
                  declare
                     Local_Name : constant Name_Id := Name (Instr.Local);

                  begin
                     Write_Str  (" ");
                     Print_Name (Local_Name);
                  end;
               end if;

               if Instr.Pool_Item /= Null_Pool_Item then
                  Write_Str (" [Pool] ");

                  case Pool_Item_Tag (Instr.Pool_Item) is
                     when CONSTANT_Utf8 =>
                        Write_Str ("Utf8 ");

                     when CONSTANT_Integer =>
                        Write_Str ("Integer ");
                        Write_Int
                          (UI_To_Int (Pool_Integer (Instr.Pool_Item)));

                     when CONSTANT_Float =>
                        Write_Str ("Float ");
                        UR_Write (Pool_Float (Instr.Pool_Item));

                     when CONSTANT_Long =>
                        Write_Str ("Long ");
                        UI_Image (Pool_Integer (Instr.Pool_Item));
                        Write_Str
                          (Uintp.UI_Image_Buffer (1 .. UI_Image_Length));

                     when CONSTANT_Double =>
                        Write_Str ("Double ");
                        UR_Write (Pool_Double (Instr.Pool_Item));

                     when CONSTANT_Class =>
                        if Is_Array_Type (Instr.Pool_Item) then
                           Write_Str ("Array ");
                           Print_Name
                             (Name (Ref_Class_Type (Instr.Pool_Item)));
                        else
                           Write_Str ("Class ");
                           Print_Name (Name (Ref_Class (Instr.Pool_Item)));
                        end if;

                     when CONSTANT_String =>
                        Write_Str ("String ");
                        String_To_Name_Buffer
                          (Pool_String (Instr.Pool_Item));
                        Write_Str (Name_Buffer (1 .. Name_Len));

                     when CONSTANT_Fieldref =>
                        Write_Str ("Fieldref ");
                        Print_Name (Name (Ref_Field (Instr.Pool_Item)));

                     when CONSTANT_Methodref |
                          CONSTANT_Interface_Methodref =>
                        declare
                           Method : constant Method_Id :=
                                      Ref_Method (Instr.Pool_Item);
                        begin
                           if Pool_Item_Tag (Instr.Pool_Item)
                             = CONSTANT_Methodref
                           then
                              Write_Str ("Methodref ");
                           else
                              Write_Str ("IfaceMethodref ");
                           end if;

                           if Result_Type (Method) /= Void_Type then
                              Print_Name (Name (Result_Type (Method)));
                              Write_Str (" ");
                           end if;

                           Print_Name (Name (Class_Of (Method)));
                           Write_Str ("::");
                           Print_Name (Name (Method));
                           Write_Str (" (");

                           declare
                              LV : Local_Var_Id;
                           begin
                              LV := First_Local_Var (Method);
                              while LV /= Null_Local_Var
                                 and then Is_Param (LV)
                              loop
                                 Print_Name (Name (Type_Of (LV)));
                                 LV := Next_Local_Var (LV);

                                 if LV /= Null_Local_Var
                                   and then Is_Param (LV)
                                 then
                                    Write_Str (", ");
                                 end if;
                              end loop;
                           end;

                           Write_Str (")");
                        end;

                     when CONSTANT_Name_And_Type =>
                        Write_Str ("Name_And_Type ");
                        Print_Name (Name (Ref_Type (Instr.Pool_Item)));

                     when CONSTANT_Empty =>
                        Write_Str ("Empty ");
                  end case;
               end if;
         end case;
      end if;

      Write_Eol;
   end Print_Instruction;

   --------
   -- PI --
   --------

   procedure PI (I : Instruction) is
   begin
      Print_Instruction (I);
   end PI;

begin
   Code_Table.Set_Last (Next_Code_Index);
end JVM.Code;
