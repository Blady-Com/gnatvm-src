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
------------------------------------------------------------------------------

with GNAT.Table;
with JVM.Dbg;    use JVM.Dbg;
with JVM.Info;   use JVM.Info;
with JVM.Pool;   use JVM.Pool;
with J_String;   use J_String;

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
     (Op => Xxxunusedxxx, Next => Null_Instr);
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

   --------------------
   -- Count_Sequence --
   --------------------

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

      Seq.Last := Id;
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
     (Exc_Class   : Pool_Id;
      Start_Lbl   : Label_Id;
      End_Lbl     : Label_Id;
      Handler_Lbl : Label_Id)
      return        Handler_Id
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
        := (Exc_Class, Start_Lbl, End_Lbl, Handler_Lbl, Null_Handler);

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

   -----------------------
   -- Print_Instruction --
   -----------------------

   procedure Print_Instruction (Instr : Instruction) is
   begin
      if Instr.Op /= Nop or else Instr.Label_Def = Null_Label then
         Print ("         ");

         if Instr.Op = Jump then
            Print ("GOTO");

         elsif Instr.Op = Newobject then
            Print ("NEW");

         elsif Instr.Op = Vreturn then
            Print ("RETURN");

         else
            Print (Operation'Image (Instr.Op));
         end if;
      end if;

      --  Print any operands of the instruction

      case Instr.Op is
         when Nop =>
            if Instr.Label_Def /= Null_Label then
               Print (Label_Number (Instr.Label_Def)'Img & "$:");
            end if;

            if Instr.Line_Number /= No_Location then
               Print ("  " & Source_Ptr'Image (Instr.Line_Number));
            end if;

         when Bipush | Sipush =>
            Print (Instr.Sint'Img);

         when Newarray =>
            Print (" ");
            Print (Instr.Element_Type'Img);

         when Iload  | Lload  | Fload  | Dload  | Aload  |
              Istore | Lstore | Fstore | Dstore | Astore |
              Ret =>
            Print (" ");
            Print (Name (Instr.Local));

         when Iinc =>
            Print (" ");
            Print (Name (Instr.Inc_Local));
            Print (Instr.Increment'Img);

         when Ifeq      | Ifne      | Iflt      | Ifge      | Ifgt | Ifle |
              If_Icmpeq | If_Icmpne | If_Icmplt | If_Icmpge |
              If_Icmpgt | If_Icmple | If_Acmpeq | If_Acmpne |
              Ifnull    | Ifnonnull | Jump      | Goto_W    |
              Jsr       | Jsr_W     =>
            Print (Label_Number (Instr.Target)'Img & "$");

         when Ldc | Ldc_W | Ldc2_W =>
            if Pool_Item_Tag (Instr.Pool_Item) = CONSTANT_String then
               Print (" """);
               Print (Str (Pool_String (Instr.Pool_Item)));
               Print ("""");
            else
               Print (" <some value>");
               --  ??? Print (Value (Instr.Pool_Item));
            end if;

         when Getstatic | Putstatic =>
            Print (" ");
            Print (Name (Class (Ref_Field (Instr.Pool_Item))));
            Print (".");
            Print (Name (Ref_Field (Instr.Pool_Item)));

         when Getfield | Putfield =>
            --  Print (" ");
            --  Print (Name (Type_Of (Ref_Field (Instr.Pool_Item))));
            Print (" ");
            Print (Name (Ref_Field (Instr.Pool_Item)));

         when Invokevirtual | Invokespecial |
              Invokestatic  | Invokeinterface =>
            declare
               Method : constant Method_Id := Ref_Method (Instr.Pool_Item);
               Arg_N  : Natural := 0;
               Arg_C  : Natural := 1;
               LV     : Local_Var_Id;

            begin
               Print (" ");

               if Result_Type (Method) /= Void_Type then
                  Print (Name (Result_Type (Method)));
                  Print (" ");
               else
                  Print ("void ");
               end if;

               Print (Name (Class (Ref_Method (Instr.Pool_Item))));
               Print (".");
               Print (Name (Ref_Method (Instr.Pool_Item)));

               LV := First_Local_Var (Method);
               while LV /= Null_Local_Var and then Is_Param (LV) loop
                  Arg_N := Arg_N + 1;
                  LV    := Next_Local_Var (LV);
               end loop;

               if Arg_N > 0 then
                  Print (" (");

                  LV := First_Local_Var (Method);
                  while LV /= Null_Local_Var and then Arg_C < Arg_N loop
                     Arg_C := Arg_C + 1;
                     Print (Name (Type_Of (LV))); Print (", ");
                     LV := Next_Local_Var (LV);
                  end loop;

                  Print (Name (Type_Of (LV))); Print (") ");
               end if;
            end;

         when Newobject =>
            Print (" ");
            Print (Name (Ref_Class (Instr.Pool_Item)));

         when Anewarray =>
            Print (" ");
            Print (Name (Ref_Class_Type (Instr.Pool_Item)));
            Print ("[]");

         when Checkcast | Instanceof =>
            Print (" ");
            Print (Name (Ref_Class_Type (Instr.Pool_Item)));

         when Multianewarray =>
            Print (" ");
            Print
              (Name (Element_Type (Ref_Class_Type (Instr.Array_Class))));
            Print ("[]");
            Print (", Dimensions => ");
            Print (Instr.Dimensions'Img);

         when Tableswitch | Lookupswitch =>
            Print (" ");
            Print (Label_Number (Instr.Default_Label)'Img & "$");
            --  Print list of switch pairs... ???

         when others =>
            null;
      end case;

      Print_Line;
   end Print_Instruction;

begin
   Code_Table.Set_Last (Next_Code_Index);
end JVM.Code;
