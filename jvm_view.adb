------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M _ V I E W                              --
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

with Ada.Text_IO;
with Ada.Wide_Text_IO;
with J_Basics;         use J_Basics;
with J_Utils;          use J_Utils;

package body JVM_View is

   use Ada;
   use Class_Attribute;
   use Class_Index;
   use Code_Array;
   use Code_Attribute;
   use CP;
   use Handler;
   use Line;
   use Inner_Class;
   use Member;
   use Member_Attribute;
   use Utf8;
   use Variable;

   --------------------
   -- Local Routines --
   --------------------

   procedure Print (S : String);
   procedure Print (V : U1);
   procedure Print (V : U2);
   procedure Print (V : U4);
   procedure Print (V : Instruction_Index);
   procedure Print (V : Local_Variable_Index);
   procedure Print (V : Jump_Offset_U4);
   procedure Print (V : Int_8);
   procedure Print (V : Int_16);
   procedure Print (V : Int_32);
   procedure Print (V : Int_64);
   procedure Print (V : IEEE_Float_32);
   procedure Print (V : IEEE_Float_64);
   --  Respectively output S and V to standard out

   procedure Print_Wide (W : Wide_String);
   --  Prints W to standard out

   procedure Print_EOL (Nb : Positive := 1);
   --  Outputs a Nb new lines to standard out

   procedure Print_EOL (S : String);
   --  Outputs S followed by a new line to standard out

   procedure Print_CP_Index (K : CP_Index);
   --  Outputs CP index K to standard out

   procedure Print_CP_Info (T : CP.Table; K : CP_Index);
   --  Prints in a human readable format the CP_Info record inside constant
   --  pool table T at entry K.

   procedure Print_Code_Attribute
     (CA          : Code_Attribute_Info;
      T           : CP.Table;
      Print_Lines : Boolean;
      Print_Vars  : Boolean);
   --  Like Print_Code_Attr (see spec) except that no attribute is printed if
   --  Print_Lines is False and the attribute is an Attr_Line_Number_Table.
   --  Likewise for Print_Vars and attribute Attr_Local_Variable_Table.

   procedure Print_Simple_Name (T : Utf8.Table);
   --  Prints out T without surrounding ""

   procedure Print_Qualified_Name (T : Utf8.Table);
   --  Prints out T replacing every "/" with a"."

   procedure Print_Name_And_Type (T : CP.Table; K_Class, K_Name : CP_Index);
   --  The K_Class-th entry and the K_Name-th entries in constant pool table T
   --  must be respectively a CONSTANT_Class and a CONSTANT_Name_And_Type
   --  entry. If this is not the case the outcome of this procedure is
   --  undefined, otherwise this routine prints out in a human readable format,
   --  the field or method name and its type described by the K_Name entry.

   procedure Print_Member (M : Member_Info; T : CP.Table; Indent : Natural);
   --  Same thing as the Print_Member routine specified in the spec except for
   --  the fact that one can specify amount of indentation to be used when
   --  printing out the member.

   procedure Print_Member_Info (T : CP.Table;
                                K_Simple, K_Type : CP_Index;
                                K_Package : CP_Index := 0);
   --  The K_Simple-th entry and the K_Type-th entries in constant pool table T
   --  must be two CONSTANT_Utf8 entries respectively giving the simple name
   --  and the type descriptor of a field of method. If this is not the case
   --  the outcome of this procedure is undefined, otherwise this routine
   --  prints out in a human readable format, the field or method information.
   --  If K_Package is not 0, then the resulting name is added to the field or
   --  method name before printint.

   procedure Parse (Typ : Utf8.Table; Prefix, Suffix : out Utf8.Table);
   --  Parses the type signature Typ into a prefix type and a suffix type. For
   --  instance if Typ="[[I" then Prefix="int [][]" and Suffix="". If
   --  Typ="(BD)V", then Prefix="void" and Suffix="(byte, double)". If Typ is
   --  not parsable Constraint_Error is raised. Note that this procedure does
   --  allocate a new Utf8 for Prefic and Suffix. As a result to avoid memory
   --  leaks the callers of this routine must free Prefix and Suffix when they
   --  do not need it.

   -----------
   -- Parse --
   -----------

   procedure Parse (Typ : Utf8.Table; Prefix, Suffix : out Utf8.Table) is
      B : constant U1 := Character'Pos ('B');
      C : constant U1 := Character'Pos ('C');
      D : constant U1 := Character'Pos ('D');
      F : constant U1 := Character'Pos ('F');
      I : constant U1 := Character'Pos ('I');
      J : constant U1 := Character'Pos ('J');
      S : constant U1 := Character'Pos ('S');
      Z : constant U1 := Character'Pos ('Z');
      V : constant U1 := Character'Pos ('V');
      L : constant U1 := Character'Pos ('L');

      Open_Bracket : constant U1 := Character'Pos ('[');
      Open_Paren   : constant U1 := Character'Pos ('(');
      Close_Paren  : constant U1 := Character'Pos (')');
      Semicolon    : constant U1 := Character'Pos (';');

      Current_Pos : U2 := 0;
      --  Index in Typ where the Prefix starts

      procedure Parse_Field_Or_Return_Type (Out_Typ : in out Utf8.Table);
      --  Parses the signature of a field or return type starting at index
      --  Current_Pos in Typ and appends the result to Out_Typ.

      procedure Parse_Param_Types (Out_Params : in out Utf8.Table);
      --  Parses the signature of a paramter list starting at index 0 in Typ
      --  and appends the result to Out_Params.

      --------------------------------
      -- Parse_Field_Or_Return_Type --
      --------------------------------

      procedure Parse_Field_Or_Return_Type (Out_Typ : in out Utf8.Table) is
         Bracket_Count : Natural := 0;

      begin
         while Get (Typ, Current_Pos) = Open_Bracket loop
            Current_Pos := Current_Pos + 1;
            Bracket_Count := Bracket_Count + 1;
         end loop;

         if Get (Typ, Current_Pos) = L then
            loop
               Current_Pos := Current_Pos + 1;
               exit when Get (Typ, Current_Pos) = Semicolon;
               Add (Out_Typ, Get (Typ, Current_Pos));
            end loop;

         else
            case Get (Typ, Current_Pos) is
               when B => Append_String (Out_Typ, "byte");
               when C => Append_String (Out_Typ, "char");
               when D => Append_String (Out_Typ, "double");
               when F => Append_String (Out_Typ, "float");
               when I => Append_String (Out_Typ, "int");
               when J => Append_String (Out_Typ, "long");
               when S => Append_String (Out_Typ, "short");
               when Z => Append_String (Out_Typ, "boolean");
               when V => Append_String (Out_Typ, "void");

               when others =>
                  raise Constraint_Error;
            end case;
         end if;

         Current_Pos := Current_Pos + 1;

         if Bracket_Count > 0 then
            Append_String (Out_Typ, " ");
            for J in 1 .. Bracket_Count loop
               Append_String (Out_Typ, "[]");
            end loop;
         end if;
      end Parse_Field_Or_Return_Type;

      -----------------------
      -- Parse_Param_Types --
      -----------------------

      procedure Parse_Param_Types (Out_Params : in out Utf8.Table) is
      begin
         Current_Pos := 1;
         Append_String (Out_Params, "(");
         while Get (Typ, Current_Pos) /= Close_Paren loop
            if Current_Pos /= 1 then
               Append_String (Out_Params, ", ");
            end if;
            Parse_Field_Or_Return_Type (Out_Params);
         end loop;

         Append_String (Out_Params, ")");
      end Parse_Param_Types;

   --  Beginning of Parse

   begin
      --  If first character is a "(" then we are dealing with a method
      --  signature, so advance Current_Pos after you have found the matching
      --  ")".

      if Get (Typ, U2'(0)) = Open_Paren then
         loop
            Current_Pos := Current_Pos + 1;
            exit when Get (Typ, Current_Pos) = Close_Paren;
         end loop;
         Current_Pos := Current_Pos + 1;
      end if;

      --  Parse the field type or the method return type. After parsing this
      --  there must be no more charcters left in Typ.

      declare
      begin
         Allocate_Expandable_Table (Prefix);
         Parse_Field_Or_Return_Type (Prefix);
         Freeze_Expandable_Table (Prefix);
      exception
         when others =>
            Freeze_Expandable_Table (Prefix);
            raise;
      end;

      --  If at this stage we have not consumed all characters in Typ then
      --  there is something wrong.

      if Nat_32 (Current_Pos) < Length (Typ) then
         raise Constraint_Error;
      end if;

      --  If we are dealing with a method then parse the parameters

      if Get (Typ, U2'(0)) = Open_Paren then
         declare
         begin
            Allocate_Expandable_Table (Suffix);
            Parse_Param_Types (Suffix);
            Freeze_Expandable_Table (Suffix);
         exception
            when others =>
               Freeze_Expandable_Table (Suffix);
               raise;
         end;

      else
         Utf8.Allocate_Fixed_Table (Suffix, 0);
      end if;

   end Parse;

   -----------
   -- Print --
   -----------

   procedure Print (S : String) is
   begin
      Text_IO.Put (S);
   end Print;

   procedure Print (V : U1) is
   begin
      Text_IO.Put (Image (V));
   end Print;

   procedure Print (V : U2) is
   begin
      Text_IO.Put (Image (V));
   end Print;

   procedure Print (V : U4) is
   begin
      Text_IO.Put (Image (V));
   end Print;

   procedure Print (V : Instruction_Index) is
   begin
      Text_IO.Put (Image (U2 (V)));
   end Print;

   procedure Print (V : Local_Variable_Index) is
   begin
      Text_IO.Put (Image (U2 (V)));
   end Print;

   procedure Print (V : Jump_Offset_U4) is
   begin
      Text_IO.Put (Image (Int_32 (V)));
   end Print;

   procedure Print (V : Int_8) is
   begin
      Text_IO.Put (Image (V));
   end Print;

   procedure Print (V : Int_16) is
   begin
      Text_IO.Put (Image (V));
   end Print;

   procedure Print (V : Int_32) is
   begin
      Text_IO.Put (Image (V));
   end Print;

   procedure Print (V : Int_64) is
   begin
      Text_IO.Put (Image (V));
   end Print;

   procedure Print (V : IEEE_Float_32) is
   begin
      Text_IO.Put (Strip (IEEE_Float_32'Image (V)));
   end Print;

   procedure Print (V : IEEE_Float_64) is
   begin
      Text_IO.Put (Strip (IEEE_Float_64'Image (V)));
   end Print;

   ------------------------
   -- Print_Access_Flags --
   ------------------------

   procedure Print_Access_Flags (A : Access_Mask) is
   begin
      if Is_Set (A, ACC_Public) then
         Print ("public ");
      end if;

      if Is_Set (A, ACC_Private) then
         Print ("private ");
      end if;

      if Is_Set (A, ACC_Protected) then
         Print ("protected ");
      end if;

      if Is_Set (A, ACC_Static) then
         Print ("static ");
      end if;

      if Is_Set (A, ACC_Final) then
         Print ("final ");
      end if;

      if Is_Set (A, ACC_Synchronized) then
         Print ("synchronized ");
      end if;

      if Is_Set (A, ACC_Volatile) then
         Print ("volatile ");
      end if;

      if Is_Set (A, ACC_Transient) then
         Print ("transient ");
      end if;

      if Is_Set (A, ACC_Native) then
         Print ("native ");
      end if;

      if Is_Set (A, ACC_Interface) then
         Print ("interface ");
      end if;

      if Is_Set (A, ACC_Abstract) then
         Print ("abstract ");
      end if;
   end Print_Access_Flags;

   ----------------------
   -- Print_Class_Attr --
   ----------------------

   procedure Print_Class_Attr (CA : Class_Attribute_Info; T : CP.Table) is
   begin
      case CA.Kind is

         when Attr_Source_File =>
            Print ("  Source File  : ");
            Print_Utf8 (Get_Utf8 (T, CA.Source_File_Index));

         when Attr_Inner_Classes =>
            Print ("  Nesting info : ");

            for K in 0 .. Last (CA.Classes) loop
               declare
                  ICI : constant Inner_Class_Info := Get (CA.Classes, K);
               begin
                  Print_EOL;
                  Print ("    Inner=");
                  Print_CP_Entry (T, ICI.Inner_Class_Info_Index);
                  Print ("  Outer=");
                  Print_CP_Entry (T, ICI.Outer_Class_Info_Index);
                  Print ("  Name=");
                  if ICI.Inner_Name_Index /= 0 then
                     Print_Utf8 (Get_Utf8 (T, ICI.Inner_Name_Index));
                  else
                     Print ("<unknown>");
                  end if;
                  Print ("  Flags=");
                  Print_Access_Flags (ICI.Inner_Class_Access_Flags);
               end;
            end loop;

         when Attr_Deprecated =>
            Print ("  Deprecated");

         when Attr_Synthetic =>
            Print ("  Synthetic");

         when others =>
            Print ("  ");
            Print_Utf8 (Get_Utf8 (T, CA.Attribute_Name_Index));
            Print (" attribute skipped (");
            Print (CA.Attribute_Length);
            Print (" bytes)");
      end case;

      Print_EOL;
   end Print_Class_Attr;

   ----------------------
   -- Print_Class_File --
   ----------------------

   procedure Print_Class_File (CF : Class_File) is
      Nb_Underlines : Natural := 0;
   begin
      Print_EOL;

      if Is_Set (CF.Access_Flags, ACC_Interface) then
         Print ("INTERFACE : ");
         Nb_Underlines := 9;
      else
         Print ("CLASS : ");
         Nb_Underlines := 5;
      end if;

      Print_CP_Entry (CF.Constant_Pool, CF.This_Class);
      Print_EOL;
      Print (String'(1 .. Nb_Underlines => '*'));
      Print_EOL (2);

      Print ("  Access flags : ");
      Print_Access_Flags (CF.Access_Flags);
      Print_EOL;

      Print ("  Extends      : ");
      Print_CP_Entry (CF.Constant_Pool, CF.Super_Class);
      Print_EOL;

      Print ("  Implements   : ");
      for K in 0 .. Last (CF.Interfaces) loop
         Print_CP_Entry (CF.Constant_Pool, Get (CF.Interfaces, K));
         Print (" ");
      end loop;
      Print_EOL (2);

      Print_EOL ("Class Attributes");
      Print_EOL ("----------------");
      for K in 0 .. Last (CF.Attributes) loop
         Print_Class_Attr (Get (CF.Attributes, K), CF.Constant_Pool);
      end loop;
      Print_EOL;

      Print_EOL ("Fields");
      Print_EOL ("------");
      for K in 0 .. Last (CF.Fields) loop
         Print_Member (Get (CF.Fields, K), CF.Constant_Pool);
      end loop;
      Print_EOL;

      Print_EOL ("Methods");
      Print_EOL ("-------");
      for K in 0 .. Last (CF.Methods) loop
         Print_Member (Get (CF.Methods, K), CF.Constant_Pool);
      end loop;
      Print_EOL;
   end Print_Class_File;

   ----------------
   -- Print_Code --
   ----------------

   procedure Print_Code
     (M           : Member_Info;
      T           : CP.Table;
      Source_Name : String;
      Source      : Stream_Of_U1;
      Do_Lines    : Boolean;
      Do_Vars     : Boolean)
   is
      Do_Source : constant Boolean    := Source'Length /= 0;
      Line      : constant Line_Table := Get_Line_Table (Source);
      Code      : constant Member_Attribute_Info := Get_Code_Attribute (M);
      Bytecode  : constant Code_Array.Table      := Code.Code;

      Current_PC : Instruction_Index;
      --  Keeps track of the current PC when printing bytecode

      PC_To_Src_Line : constant PC_Src_Map
                         := Get_PC_To_Src_Map (Code.Attributes);
      --  Maps the method's bytecode offsets (also known as PC) to source line
      --  numbers.

      Line_Num : Nat_32;

   begin
      Print_Member (M, T, Indent => 0);
      Print_EOL;

      Print ("Max_Stack=");
      Print (Code.Max_Stack);
      Print ("   Max_Locals=");
      Print (Code.Max_Locals);
      Print_EOL (2);

      Print_Handler_Table (Code.Exception_Table, T);

      for K in 0 .. Last (Code.Attributes) loop
         Print_Code_Attribute (Get (Code.Attributes, K), T, Do_Lines, Do_Vars);
      end loop;

      Print_EOL ("Bytecode :");
      Current_PC := 0;
      while Nat_32 (Current_PC) <= Last (Bytecode) loop

         --  Check if the current PC corresponds to a new source line

         if Do_Source
           and then Current_PC <= PC_To_Src_Line'Last
         then
            Line_Num := Nat_32 (PC_To_Src_Line (Current_PC));

            if Line_Num /= 0 then
               Print_EOL;
               Print (Source_Name);
               Print (":");
               Print (Line_Num);
               Print (": ");

               if Line_Num > Line'Last then
                  Print_EOL ("***********  Cannot find line in source file");
               else
                  Print_EOL
                    (To_String
                     (Source (Line (Line_Num).First .. Line (Line_Num).Last)));
               end if;
            end if;
         end if;

         Print_Instruction (Bytecode, Current_PC, T, Code.Attributes);
         Current_PC := Next_Instruction (Bytecode, Current_PC);
      end loop;

      Print_EOL (String'(1 .. 78 => '_'));
   end Print_Code;

   ---------------------
   -- Print_Code_Attr --
   ---------------------

   procedure Print_Code_Attr (CA : Code_Attribute_Info; T : CP.Table) is
   begin
      Print_Code_Attribute (CA, T, True, True);
   end Print_Code_Attr;

   --------------------------
   -- Print_Code_Attribute --
   --------------------------

   procedure Print_Code_Attribute
     (CA          : Code_Attribute_Info;
      T           : CP.Table;
      Print_Lines : Boolean;
      Print_Vars  : Boolean)
   is
      LI : Line_Info;
      VI : Variable_Info;

   begin
      case CA.Kind is
         when Attr_Line_Number_Table =>
            if not Print_Lines then
               return;
            end if;

            Print_EOL ("Line_Number_Table :");
            for K in 0 .. Last (CA.Line_Number_Table) loop
               LI := Get (CA.Line_Number_Table, K);
               Print ("   pc=");
               Print (Left_Justify (Instruction_Index'Image (LI.Start_PC), 5));
               Print ("  line=");
               Print (LI.Line_Number);
               Print_EOL;
            end loop;
            Print_EOL;

         when Attr_Local_Variable_Table =>
            if not Print_Vars then
               return;
            end if;

            Print_EOL ("Local_Variable_Table :");
            for K in 0 .. Last (CA.Local_Variable_Table) loop
               VI := Get (CA.Local_Variable_Table, K);
               Print ("   <");
               Print_Member_Info (T, VI.Name_Index, VI.Descriptor_Index);
               Print (">=");
               Print (VI.Index);
               Print ("  pc=[");
               Print (VI.Start_PC);
               Print (", ");
               Print (U2 (VI.Start_PC) + VI.Length);
               Print ("]");
               Print_EOL;
            end loop;
            Print_EOL;

         when others =>
            Print ("  ");
            Print_Utf8 (Get_Utf8 (T, CA.Attribute_Name_Index));
            Print (" attribute skipped (");
            Print (CA.Attribute_Length);
            Print (" bytes)");
            Print_EOL (2);
      end case;
   end Print_Code_Attribute;

   --------------
   -- Print_CP --
   --------------

   procedure Print_CP (T : CP.Table) is
   begin
      for K in 0 .. Last (T) loop
         Print (Right_Justify (Image (K)));
         Print (": ");
         Print_CP_Entry (T, CP_Index (K));
         Print_EOL;
      end loop;
   end Print_CP;

   --------------------
   -- Print_CP_Index --
   --------------------

   procedure Print_CP_Index (K : CP_Index) is

   begin
      Print ("#");
      Print (Strip (CP_Index'Image (K)));
      Print (" ");
   end Print_CP_Index;

   --------------------
   -- Print_CP_Entry --
   --------------------

   procedure Print_CP_Entry (T : CP.Table; K : CP_Index) is
      R : constant CP_Info := Get (T, K);

   begin
      case R.Tag is
         when CONSTANT_Empty =>
            null;

         when CONSTANT_Class =>
            Print_CP_Index (R.Class_Name_Index);

         when CONSTANT_Fieldref
           |  CONSTANT_Methodref
           |  CONSTANT_Interface_Methodref =>
            Print_CP_Index (R.Class_Index);
            Print_CP_Index (R.Name_And_Type_Index);

         when CONSTANT_String =>
            Print_CP_Index (R.String_Index);

         when CONSTANT_Integer =>
            Print (R.Bytes);

         when CONSTANT_Float   =>
            Print (R.Bytes);

         when CONSTANT_Long =>
            Print (R.High_Bytes);
            Print (R.Low_Bytes);
            null;

         when  CONSTANT_Double  =>
            Print (R.High_Bytes);
            Print (R.Low_Bytes);

         when CONSTANT_Name_And_Type =>
            Print_CP_Index (R.Name_Index);
            Print_CP_Index (R.Descriptor_Index);

         when CONSTANT_Utf8 =>
            Print_Utf8 (R.Str_Bytes);
      end case;

      Print_CP_Info (T, K);

   end Print_CP_Entry;

   -------------------
   -- Print_CP_Info --
   -------------------

   procedure Print_CP_Info (T : CP.Table; K : CP_Index) is
      R : constant CP_Info := Get (T, K);
      Int_Val : Int_32;

   begin
      if R.Tag = CONSTANT_Utf8 then
         return;
      end if;

      Print ("<");

      case R.Tag is
         when CONSTANT_Empty =>
            Print ("EMPTY");

         when CONSTANT_Class =>
            Print ("class ");
            Print_Qualified_Name (Get_Utf8 (T, R.Class_Name_Index));

         when CONSTANT_Fieldref
           |  CONSTANT_Methodref
           |  CONSTANT_Interface_Methodref =>
            if R.Tag = CONSTANT_Interface_Methodref then
               Print ("InterfaceMethod  ");
            end if;
            Print_Name_And_Type (T, R.Class_Index, R.Name_And_Type_Index);

         when CONSTANT_String =>
            Print ("String ");
            Print_Utf8 (Get_Utf8 (T, R.String_Index));

         when CONSTANT_Integer =>
            Print ("integer ");
            Int_Val := To_Int_32 (R.Bytes);
            Print (Int_Val);

         when CONSTANT_Float  =>
            Print ("float ");
            Print (To_IEEE_Float_32 (R.Bytes));

         when CONSTANT_Long =>
            declare
               U_64 : constant U8 := To_U8 (R.High_Bytes, R.Low_Bytes);
            begin
               Print ("long ");
               Print (To_Int_64 (U_64));
            end;

         when CONSTANT_Double =>
            declare
               U_64 : constant U8 := To_U8 (R.High_Bytes, R.Low_Bytes);
            begin
               Print ("double ");
               Print (To_IEEE_Float_64 (U_64));
            end;

         when CONSTANT_Name_And_Type =>
            Print ("Name=");
            Print_Utf8 (Get_Utf8 (T, R.Name_Index));
            Print (" Type=");
            Print_Utf8 (Get_Utf8 (T, R.Descriptor_Index));

         when CONSTANT_Utf8 =>
            null;

      end case;

      Print (">");
   end Print_CP_Info;

   ---------------
   -- Print_EOL --
   ---------------

   procedure Print_EOL (Nb : Positive := 1) is
   begin
      for K in 1 .. Nb loop
         Text_IO.New_Line;
      end loop;
   end Print_EOL;

   procedure Print_EOL (S : String) is
   begin
      Print (S);
      Print_EOL;
   end Print_EOL;

   -------------------------
   -- Print_Handler_Table --
   -------------------------

   procedure Print_Handler_Table (H : Handler.Table; T : CP.Table) is
      HI : Handler_Info;

   begin
      if Length (H) = 0 then
         return;
      end if;

      Print_EOL ("Exception_Table :");
      for K in 0 .. Last (H) loop
         HI := Get (H, K);
         Print ("   ");
         Print_CP_Info (T, HI.Catch_Type);
         Print ("  pc=[");
         Print (HI.Start_PC);
         Print (", ");
         Print (HI.End_PC);
         Print (")  handler_pc=");
         Print (HI.Handler_PC);
         Print_EOL;
      end loop;
      Print_EOL;
   end Print_Handler_Table;

   -----------------------
   -- Print_Instruction --
   -----------------------

   procedure Print_Instruction
     (Bytecode  : Code_Array.Table;
      PC        : Instruction_Index;
      T         : CP.Table;
      Code_Attr : Code_Attribute.Table)
   is
      I     : constant Instruction := Get (Bytecode, PC);
      Match : Int_32;

      procedure Print_Local_Var (Var : Local_Variable_Index);
      --  Prints a local variable whose index is Var

      procedure Print_Match_And_Offset (M : Int_32; Off : Jump_Offset_U4);
      --  Prints the match and offset pairs of a Lookupswitcth or Tableswitch
      --  operation.

      procedure Print_Offset (Off : Jump_Offset_U2);
      procedure Print_Offset (Off : Jump_Offset_U4);
      --  Prints out an offset and the corresponding PC value

      ---------------------
      -- Print_Local_Var --
      ---------------------

      procedure Print_Local_Var (Var : Local_Variable_Index) is
         VI : Variable_Info := Get_Var_Info (Code_Attr, Var, PC);
         PC_Index : Instruction_Index := 0;
      begin
         --  Keep looking for a valid local variable, as the start_PC in the
         --  local variables table can be set after the first store instruction
         --  in the code
         --  An example is :  Local_Variable_Table :
         --                     <Test this>=0  pc=[0, 303]
         --                     <int counter>=1  pc=[2, 303]
         --                     <double tmp>=2  pc=[7, 303]
         --                     <int j>=4  pc=[10, 303]
         --                   Bytecode :
         --                     0: iconst_0
         --                     1: istore_1  <int counter>
         --                     2: iconst_2
         --  where counter is used before the pc_start.
         --  To prevent an infinite loop when the java program was not compiled
         --  with -g, we only look a few steps further (from pc to pc+5, as no
         --  java instruction is longer than 5 bytes, except lookupswitch and
         --  tableswitch, which do not concern us here).
         while VI.Name_Index = CP_Empty loop
            if PC_Index > 5 then
               return;
            end if;
            PC_Index := PC_Index + 1;
            VI := Get_Var_Info (Code_Attr, Var, PC + PC_Index);
         end loop;

         Print (" <");
         Print_Member_Info (T, VI.Name_Index, VI.Descriptor_Index);
         Print (">");
      end Print_Local_Var;

      ----------------------------
      -- Print_Match_And_Offset --
      ----------------------------

      procedure Print_Match_And_Offset (M : Int_32; Off : Jump_Offset_U4) is
      begin
         Print ("           Match=");
         Print (M);
         Print ("  Offset=");
         Print_Offset (Off);
      end Print_Match_And_Offset;

      ------------------
      -- Print_Offset --
      ------------------

      procedure Print_Offset (Off : Jump_Offset_U2) is
      begin
         Print_Offset (Jump_Offset_U4 (Off));
      end Print_Offset;

      procedure Print_Offset (Off : Jump_Offset_U4) is
      begin
         Print (Off);
         Print ("  <pc=");
         Print (Jump_Offset_U4 (PC) + Off);
         Print (">");
      end Print_Offset;

   --  Beginning of Print_Instruction

   begin
      Print (Right_Justify (Image (PC), 5));
      Print (": ");

      if (I.Op = Iinc
          and then I.Wide_Iinc)
        or else
        ((I.Op = Ret
          or else I.Op in Iload  .. Aload
          or else I.Op in Istore .. Astore)
         and then I.Wide)
      then
         Print_EOL ("wide");
         Print (Right_Justify (Image (PC + 1), 5));
         Print (": ");
      end if;

      Print (Operation_Mnemonic (I.Op));
      Print (" ");

      case I.Op is
         when Bipush =>
            Print (I.Byte);

         when Sipush =>
            Print (I.Short);

         when Iinc =>
            Print (I.Var_Iinc);
            Print_Local_Var (I.Var_Iinc);
            Print (" ");
            Print (I.Const_Val);

         when Ret
           |  Iload  | Lload  | Fload  | Dload  | Aload
           |  Istore | Lstore | Fstore | Dstore | Astore =>
            Print (I.Local_Var);
            Print_Local_Var (I.Local_Var);

         when Istore_0 | Lstore_0 | Fstore_0 | Dstore_0 | Astore_0
           |  Iload_0  | Lload_0  | Fload_0  | Dload_0  | Aload_0 =>
            Print_Local_Var (0);

         when Istore_1 | Lstore_1 | Fstore_1 | Dstore_1 | Astore_1
           |  Iload_1  | Lload_1  | Fload_1  | Dload_1  | Aload_1 =>
            Print_Local_Var (1);

         when Istore_2 | Lstore_2 | Fstore_2 | Dstore_2 | Astore_2
           |  Iload_2  | Lload_2  | Fload_2  | Dload_2  | Aload_2 =>
            Print_Local_Var (2);

         when Istore_3 | Lstore_3 | Fstore_3 | Dstore_3 | Astore_3
           |  Iload_3  | Lload_3  | Fload_3  | Dload_3  | Aload_3 =>
            Print_Local_Var (3);

         when Invokeinterface =>
            Print_CP_Entry (T, I.I_Method);
            Print (" ");
            Print (I.Nargs);

         when Newarray =>
            Print (Array_Type'Image (I.Atype));

         when Multianewarray =>
            Print_CP_Entry (T, I.Array_Class);
            Print (" ");
            Print (I.Dimensions);

         when Lookupswitch =>
            Print_EOL;
            Print ("           default_offset=");
            Print_Offset (I.Lookup_Default);
            for K in 1 .. I.Table_Length loop
               Print_EOL;
               Print_Match_And_Offset
                 (I.Lookup_Table (K).Match, I.Lookup_Table (K).Offset);
            end loop;

         when Tableswitch =>
            Print_EOL;
            Print ("           Low=");
            Print (I.Low);
            Print ("  High=");
            Print (I.High);
            Print ("    default_offset=");
            Print_Offset (I.Table_Default);

            Match := I.Low;
            for K in 1 .. I.Table_Length loop
               Print_EOL;
               Print_Match_And_Offset (Match, I.Jump_Table (K));
               Match := Match + 1;
            end loop;

         when Ldc =>
            Print_CP_Entry (T, I.CP_Const_U1);
            null;

         when Ldc_W =>
            Print_CP_Entry (T, I.CP_Const_U2);

         when Ldc2_W =>
            Print_CP_Entry (T, I.CP_Const);

         when Jump
           |  Jsr
           |  Ifnull    | Ifnonnull
           |  If_Acmpeq | If_Acmpne
           |  Ifeq      | Ifne
           |  Ifle      | Iflt      | Ifge      | Ifgt
           |  If_Icmpeq | If_Icmpne
           |  If_Icmple | If_Icmplt | If_Icmpge | If_Icmpgt =>
            Print_Offset (I.Offset);

         when Jsr_W
           |  Goto_W =>
            Print_Offset (I.Offset_U4);

         when Getstatic | Putstatic
           |  Getfield  | Putfield =>
            Print_CP_Entry (T, I.Field);

         when Invokestatic
           |  Invokespecial
           |  Invokevirtual =>
            Print_CP_Entry (T, I.Method);

         when Newobject
           |  Anewarray
           |  Checkcast
           |  Instanceof =>
            Print_CP_Entry (T, I.Class);

         when others =>
            null;
      end case;

      Print_EOL;
   end Print_Instruction;

   ------------------
   -- Print_Member --
   ------------------

   procedure Print_Member (M : Member_Info; T : CP.Table) is
   begin
      Print_Member (M, T, 2);
   end Print_Member;

   procedure Print_Member (M : Member_Info; T : CP.Table; Indent : Natural) is
   begin
      Print (String'(1 .. Indent => ' '));
      Print_Access_Flags (M.Access_Flags);
      Print_Member_Info (T, M.Name_Index, M.Descriptor_Index);
      Print_EOL;
      for K in 0 .. Last (M.Attributes) loop
         if Get (M.Attributes, K).Kind /= Attr_Code then
            Print (String'(1 .. Indent + 2 => ' '));
            Print_Member_Attr (Get (M.Attributes, K), T);
         end if;
      end loop;
   end Print_Member;

   -----------------------
   -- Print_Member_Attr --
   -----------------------

   procedure Print_Member_Attr (MA : Member_Attribute_Info; T : CP.Table) is
   begin
      if MA.Kind /= Attr_Code and then MA.Kind /= Attr_Unknown then
         Print_Simple_Name (Get_Utf8 (T, MA.Attribute_Name_Index));
      end if;

      case MA.Kind is
         when Attr_Constant_Value =>
            Print (" = ");
            Print_CP_Entry (T, MA.Constant_Value_Index);

         when Attr_Deprecated
           |  Attr_Synthetic  =>
            null;

         when Attr_Code =>
            null;

         when Attr_Exceptions =>
            Print (" : ");
            for K in 0 .. Last (MA.Exception_Index_Table) loop
               Print_CP_Entry (T, Get (MA.Exception_Index_Table, K));
               Print (" ");
            end loop;

         when others =>
            Print ("  ");
            Print_Utf8 (Get_Utf8 (T, MA.Attribute_Name_Index));
            Print (" attribute skipped (");
            Print (MA.Attribute_Length);
            Print (" bytes)");
      end case;

      if MA.Kind /= Attr_Code then
         Print_EOL;
      end if;
   end Print_Member_Attr;

   -----------------------
   -- Print_Member_Info --
   -----------------------

   procedure Print_Member_Info (T : CP.Table;
                                K_Simple, K_Type : CP_Index;
                                K_Package : CP_Index := 0) is
      P     : Utf8.Table;
      S     : Utf8.Table;

      Name  : constant Utf8.Table := Get_Utf8 (T, K_Simple);
      Typ   : constant Utf8.Table := Get_Utf8 (T, K_Type);

   begin
      Utf8.Set_Water_Mark;

      Parse (Typ, Prefix => P, Suffix => S);

      Print_Qualified_Name (P);
      Print (" ");

      if K_Package /= 0 then
         Print_Qualified_Name (Get_Utf8 (T, K_Package));
         Print (".");
      end if;

      Print_Simple_Name (Name);

      if Length (S) > 0 then
         Print (" ");
         Print_Qualified_Name (S);
      end if;

      Utf8.Free_To_Next_Water_Mark;

   exception
      when others =>
         Utf8.Free_To_Next_Water_Mark;
         raise;
   end Print_Member_Info;

   -------------------------
   -- Print_Name_And_Type --
   -------------------------

   procedure Print_Name_And_Type (T : CP.Table; K_Class, K_Name : CP_Index) is
      R : constant CP_Info := Get (T, K_Name);
   begin
      Print_Member_Info (T, R.Name_Index, R.Descriptor_Index,
                         Get (T, K_Class).Class_Name_Index);
   end Print_Name_And_Type;

   --------------------------
   -- Print_Qualified_Name --
   --------------------------

   procedure Print_Qualified_Name (T : Utf8.Table) is
   begin
      if not Has_Wide_Chars (T) then
         declare
            S : String := To_String (T);
         begin
            for J in S'Range loop
               if S (J) = '/' then
                  S (J) := '.';
               end if;
            end loop;
            Print (S);
         end;

      else
         declare
            W : Wide_String := To_Wide_String (T);
         begin
            for J in W'Range loop
               if W (J) = '/' then
                  W (J) := '.';
               end if;
            end loop;
            Print_Wide (W);
         end;
      end if;
   end Print_Qualified_Name;

   -----------------------
   -- Print_Simple_Name --
   -----------------------

   procedure Print_Simple_Name (T : Utf8.Table) is
   begin
      if not Has_Wide_Chars (T) then
         Print (To_String (T));
      else
         Print_Wide (To_Wide_String (T));
      end if;
   end Print_Simple_Name;

   ----------------
   -- Print_Utf8 --
   ----------------

   procedure Print_Utf8 (T : Utf8.Table) is
   begin
      Print ("""");
      if not Has_Wide_Chars (T) then
         Print (To_String (T));
      else
         Print_Wide (To_Wide_String (T));
      end if;
      Print ("""");
   end Print_Utf8;

   ----------------
   -- Print_Wide --
   ----------------

   procedure Print_Wide (W : Wide_String) is
   begin
      Wide_Text_IO.Put (W);
   end Print_Wide;

end JVM_View;
