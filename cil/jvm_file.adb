------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M _ F I L E                              --
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

with GNAT.Case_Util;
with Ada.Unchecked_Conversion; use Ada;

package body JVM_File is

   use Utf8;
   use Class_Index;
   use CP;
   use Code_Array;
   use Handler;
   use Line;
   use Variable;
   use Inner_Class;
   use Code_Attribute;
   use Member_Attribute;
   use Class_Attribute;
   use Member;

   ----------------
   -- Local Data --
   ----------------

   IO_Mask   : constant := 2#10_000000#;
   IIO_Mask  : constant := 2#110_00000#;
   IIIO_Mask : constant := 2#1110_0000#;
   --  Utf8 masks needed for the conversions between Utf8s and character or
   --  wide characters.

   Encoding_Of_Char_255 : constant := 2#1100_0011#;
   --  Gives the biggest of the 2 Utf8 byte encodings of the biggest Character
   --  (ir the Character with position 255). Every time a byte with a value
   --  greater than this is found in a Utf8, we must be dealing with a
   --  Wide_String (in Ada terms).

   ---------------
   -- Size Data --
   ---------------

   --  The following give the size (in bytes) of the data structures defined in
   --  the spec of this package when stored as bytecode in a ".class" file.

   CP_Info_Size : constant array (CP_Tag) of Nat_32 := (
   --  Size for each CP_Info entry. For CONSTANT_Utf8 entries the size of the
   --  Str_Bytes table needs to be added to the value given here.

      CONSTANT_Empty               => 0,
      CONSTANT_Utf8                => 3,
      CONSTANT_Integer             => 5,
      CONSTANT_Float               => 5,
      CONSTANT_Long                => 9,
      CONSTANT_Double              => 9,
      CONSTANT_Class               => 3,
      CONSTANT_String              => 3,
      CONSTANT_Fieldref            => 5,
      CONSTANT_Methodref           => 5,
      CONSTANT_Interface_Methodref => 5,
      CONSTANT_Name_And_Type       => 5
   );

   Attribute_Base_Size   : constant :=  6;
   Member_Info_Base_Size : constant :=  8;
   Class_File_Base_Size  : constant := 24;
   --  Base size of the corresponding data structure when all the tables it
   --  contains are empty.

   Instruction_Size : constant array (Operation) of Int_32 := (
   --  Size for each CIL operation. Because the instruction size of the
   --  Lookupswitch and Tableswitch operations is variable their entry in this
   --  table is equal to zero. The exact size needs to be computed from the
   --  actual instruction. The instruction size of [ILFDA]load/store, Ret as
   --  well as Iinc depends on whether the "Wide" operation modifier preeceeds
   --  them. If X is their size in the absence of the Wide modifier, then in
   --  the presence of the Wide modifier their size becomes 2 * X. To quickly
   --  spot out all such operations the instruction size for these operations
   --  will be set to -X.

      Aaload          =>  1,
      Aastore         =>  1,
      Aconst_Null     =>  1,
      Aload           => -2,
      Aload_0         =>  1,
      Aload_1         =>  1,
      Aload_2         =>  1,
      Aload_3         =>  1,
      Anewarray       =>  3,
      Areturn         =>  1,
      Arraylength     =>  1,
      Astore          => -2,
      Astore_0        =>  1,
      Astore_1        =>  1,
      Astore_2        =>  1,
      Astore_3        =>  1,
      Athrow          =>  1,
      Baload          =>  1,
      Bastore         =>  1,
      Bipush          =>  2,
      Caload          =>  1,
      Castore         =>  1,
      Checkcast       =>  3,
      D2f             =>  1,
      D2i             =>  1,
      D2l             =>  1,
      Dadd            =>  1,
      Daload          =>  1,
      Dastore         =>  1,
      Dcmpg           =>  1,
      Dcmpl           =>  1,
      Dconst_0        =>  1,
      Dconst_1        =>  1,
      Ddiv            =>  1,
      Dload           => -2,
      Dload_0         =>  1,
      Dload_1         =>  1,
      Dload_2         =>  1,
      Dload_3         =>  1,
      Dmul            =>  1,
      Dneg            =>  1,
      Drem            =>  1,
      Dreturn         =>  1,
      Dstore          => -2,
      Dstore_0        =>  1,
      Dstore_1        =>  1,
      Dstore_2        =>  1,
      Dstore_3        =>  1,
      Dsub            =>  1,
      Dup             =>  1,
      Dup_X1          =>  1,
      Dup_X2          =>  1,
      Dup2            =>  1,
      Dup2_X1         =>  1,
      Dup2_X2         =>  1,
      F2d             =>  1,
      F2i             =>  1,
      F2l             =>  1,
      Fadd            =>  1,
      Faload          =>  1,
      Fastore         =>  1,
      Fcmpg           =>  1,
      Fcmpl           =>  1,
      Fconst_0        =>  1,
      Fconst_1        =>  1,
      Fconst_2        =>  1,
      Fdiv            =>  1,
      Fload           => -2,
      Fload_0         =>  1,
      Fload_1         =>  1,
      Fload_2         =>  1,
      Fload_3         =>  1,
      Fmul            =>  1,
      Fneg            =>  1,
      Frem            =>  1,
      Freturn         =>  1,
      Fstore          => -2,
      Fstore_0        =>  1,
      Fstore_1        =>  1,
      Fstore_2        =>  1,
      Fstore_3        =>  1,
      Fsub            =>  1,
      Getfield        =>  3,
      Getstatic       =>  3,
      Goto_W          =>  5,
      I2b             =>  1,
      I2c             =>  1,
      I2d             =>  1,
      I2f             =>  1,
      I2l             =>  1,
      I2s             =>  1,
      Iadd            =>  1,
      Iaload          =>  1,
      Iand            =>  1,
      Iastore         =>  1,
      Iconst_0        =>  1,
      Iconst_1        =>  1,
      Iconst_2        =>  1,
      Iconst_3        =>  1,
      Iconst_4        =>  1,
      Iconst_5        =>  1,
      Iconst_M1       =>  1,
      Idiv            =>  1,
      If_Acmpeq       =>  3,
      If_Acmpne       =>  3,
      If_Icmpeq       =>  3,
      If_Icmpge       =>  3,
      If_Icmpgt       =>  3,
      If_Icmple       =>  3,
      If_Icmplt       =>  3,
      If_Icmpne       =>  3,
      Ifeq            =>  3,
      Ifge            =>  3,
      Ifgt            =>  3,
      Ifle            =>  3,
      Iflt            =>  3,
      Ifne            =>  3,
      Ifnonnull       =>  3,
      Ifnull          =>  3,
      Iinc            => -3,
      Iload           => -2,
      Iload_0         =>  1,
      Iload_1         =>  1,
      Iload_2         =>  1,
      Iload_3         =>  1,
      Imul            =>  1,
      Ineg            =>  1,
      Instanceof      =>  3,
      Invokeinterface =>  5,
      Invokespecial   =>  3,
      Invokestatic    =>  3,
      Invokevirtual   =>  3,
      Ior             =>  1,
      Irem            =>  1,
      Ireturn         =>  1,
      Ishl            =>  1,
      Ishr            =>  1,
      Istore          => -2,
      Istore_0        =>  1,
      Istore_1        =>  1,
      Istore_2        =>  1,
      Istore_3        =>  1,
      Isub            =>  1,
      Iushr           =>  1,
      Ixor            =>  1,
      Jsr             =>  3,
      Jsr_W           =>  5,
      Jump            =>  3,
      L2d             =>  1,
      L2f             =>  1,
      L2i             =>  1,
      Ladd            =>  1,
      Laload          =>  1,
      Land            =>  1,
      Lastore         =>  1,
      Lcmp            =>  1,
      Lconst_0        =>  1,
      Lconst_1        =>  1,
      Ldc             =>  2,
      Ldc_W           =>  3,
      Ldc2_W          =>  3,
      Ldiv            =>  1,
      Lload           => -2,
      Lload_0         =>  1,
      Lload_1         =>  1,
      Lload_2         =>  1,
      Lload_3         =>  1,
      Lmul            =>  1,
      Lneg            =>  1,
      Lookupswitch    =>  0,
      Lor             =>  1,
      Lrem            =>  1,
      Lreturn         =>  1,
      Lshl            =>  1,
      Lshr            =>  1,
      Lstore          => -2,
      Lstore_0        =>  1,
      Lstore_1        =>  1,
      Lstore_2        =>  1,
      Lstore_3        =>  1,
      Lsub            =>  1,
      Lushr           =>  1,
      Lxor            =>  1,
      Monitorenter    =>  1,
      Monitorexit     =>  1,
      Wide            =>  1,
      Multianewarray  =>  4,
      Newarray        =>  2,
      Newobject       =>  3,
      Nop             =>  1,
      Pop             =>  1,
      Pop2            =>  1,
      Putfield        =>  3,
      Putstatic       =>  3,
      Ret             => -2,
      Saload          =>  1,
      Sastore         =>  1,
      Sipush          =>  3,
      Swap            =>  1,
      Tableswitch     =>  0,
      Vreturn         =>  1,
      Xxxunusedxxx    =>  1
   );

   Tableswitch_Base_Size  : constant := 13;
   Lookupswitch_Base_Size : constant :=  9;
   --  Base size of a Tableswitch and Lookupswitch instruction

   Lookup_Info_Size    : constant := 8;
   Jump_Offset_U4_Size : constant := 4;
   --  Sizes of a Lookup_Info & Jump_Offset_U4 object

   --------------------
   -- Local Routines --
   --------------------

   function To_Access_Mask is
     new Unchecked_Conversion (Access_Flag, Access_Mask);
   --  Converts an Access_Flag into an Access_Mask

   function Number_Of_Jumps (T : Code_Array.Table; K : I_Index) return U4;
   --  The operation stored in T at index K must be either a Tableswitch or a
   --  Lookupswitch. If this is not the case an exception is raised. Otherwise,
   --  this function returns the number of jumps contained in the jump or
   --  lookup table for the corresponding operation.

   function Get (T : Code_Array.Table; P : Byte_Index) return U4;
   --  Routine to convert a stream of 4 bytes stored in table T, starting
   --  at position P, into a U4.

   ---------
   -- Add --
   ---------

   procedure Add (T : in out CA_Table; I : in out Instruction) is
      pragma Unreferenced (T);

   --  Beginning of Add

   begin
      Set_Wide_Format_If_Needed (I);
      --  code removed ???
      --  Add_Instruction (I);
   end Add;

   ------------
   -- Append --
   ------------

   procedure Append (T : in out Utf8.Table; C : Character) is
      U : constant U1 := Character'Pos (C);

   begin
      if U in 16#01# .. 16#7F# then
         Add (T, U);
      else
         Append (T, Wide_Character'Val (U));
      end if;
   end Append;

   procedure Append (T : in out Utf8.Table; X : Wide_Character) is
      V : constant U2 := Wide_Character'Pos (X);

      function Bits (V : U2; X, Y : Natural) return U1;
      pragma Inline (Bits);
      --  Extracts bits X to Y of V and returns them as a U1. X and Y must be
      --  in the range 0 to 15. If Y - X >= 8 the result is undefined.

      function Bits (V : U2; X, Y : Natural) return U1 is
      begin
         return U1 (Shift_Right (Shift_Left (V, 15 - Y), X + 15 - Y));
      end Bits;

   begin
      if V in 16#00_01# .. 16#00_7F# then
         Add (T, U1 (V));

      elsif V <= 16#07_FF# then
         Add (T, IIO_Mask or Bits (V,  6, 10));
         Add (T,  IO_Mask or Bits (V,  0, 5));

      else
         Add (T, IIIO_Mask or Bits (V, 12, 15));
         Add (T,   IO_Mask or Bits (V,  6, 11));
         Add (T,   IO_Mask or Bits (V,  0, 5));
      end if;
   end Append;

   procedure Append (T : in out Utf8.Table; S : String) is
   begin
      for J in S'Range loop
         Append (T, S (J));
      end loop;
   end Append;

   procedure Append (T : in out Utf8.Table; W : Wide_String) is
   begin
      for J in W'Range loop
         Append (T, W (J));
      end loop;
   end Append;

   -------------------
   -- Append_String --
   -------------------

   procedure Append_String (T : in out Utf8.Table; S : String) is
   begin
      for J in S'Range loop
         Add (T, Character'Pos (S (J)));
      end loop;
   end Append_String;

   --------------------
   -- Attribute_Name --
   --------------------

   function Attribute_Name (A : Attribute_Kind) return String is
   begin
      case A is
         when Attr_Unknown              => return "";
         when Attr_Source_File          => return "SourceFile";
         when Attr_Constant_Value       => return "ConstantValue";
         when Attr_Code                 => return "Code";
         when Attr_Exceptions           => return "Exceptions";
         when Attr_Line_Number_Table    => return "LineNumberTable";
         when Attr_Local_Variable_Table => return "LocalVariableTable";
         when Attr_Inner_Classes        => return "InnerClasses";
         when Attr_Deprecated           => return "Deprecated";
         when Attr_Synthetic            => return "Synthetic";
      end case;
   end Attribute_Name;

   --------------
   -- Bytecode --
   --------------

   function Bytecode (Op : Operation) return U1 is
   begin
      return Operation'Pos (Op);
   end Bytecode;

   -------------------------------
   -- Compute_Attribute_Lengths --
   -------------------------------

   procedure Compute_Attribute_Lengths (CF : Class_File) is
      Line_Info_Size        : constant :=  4;
      Variable_Info_Size    : constant := 10;
      Handler_Info_Size     : constant :=  8;
      Inner_Class_Info_Size : constant :=  8;
      --  Size of the corresponding record types

      procedure Visit (A : in out Code_Attribute.Data_Array);
      --  Compute the Attribute_Length of each Code attribute in table A

      procedure Visit (A : in out Member_Attribute.Data_Array);
      --  Same as above but for member attributes

      procedure Visit (A : in out Class_Attribute.Data_Array);
      --  Same as above but for class attributes

      -----------
      -- Visit --
      -----------

      procedure Visit (A : in out Code_Attribute.Data_Array) is
         S : Nat_32;

      begin
         for K in A'Range loop
            case A (K).Kind is
               when Attr_Line_Number_Table =>
                  S := 2 + Line_Info_Size * Length (A (K).Line_Number_Table);

               when Attr_Local_Variable_Table =>
                  S := 2 +
                    Variable_Info_Size * Length (A (K).Local_Variable_Table);

               when others =>
                  pragma Assert (False);
                  null;
            end case;

            A (K).Attribute_Length := U4 (S);
         end loop;
      end Visit;

      procedure Visit (A : in out Member_Attribute.Data_Array) is
         S : Nat_32;

         procedure Process_Code_Attributes is new Code_Attribute.Process;
         --  Process all the Code attributes of a method

      begin
         for K in A'Range loop
            case A (K).Kind is
               when Attr_Constant_Value =>
                  S := 2;

               when Attr_Deprecated
                 |  Attr_Synthetic  =>
                  S := 0;

               when Attr_Code =>
                  --  Process code atributes first to compute their lengths

                  Process_Code_Attributes (A (K).Attributes);

                  S := 12 + Length (A (K).Code)
                    + Handler_Info_Size * Length (A (K).Exception_Table);

                  for J in 0 .. Last (A (K).Attributes) loop
                     S := S + Attribute_Base_Size
                       + Nat_32 (Get (A (K).Attributes, J).Attribute_Length);
                  end loop;

               when Attr_Exceptions =>
                  S := 2 + 2 * Length (A (K).Exception_Index_Table);

               when others =>
                  pragma Assert (False);
                  null;
            end case;

            A (K).Attribute_Length := U4 (S);
         end loop;
      end Visit;

      procedure Visit (A : in out Class_Attribute.Data_Array) is
         S : Nat_32;

      begin
         for K in A'Range loop
            case A (K).Kind is
               when Attr_Source_File =>
                  S := 2;

               when Attr_Inner_Classes =>
                  S := 2 + Inner_Class_Info_Size * Length (A (K).Classes);

               when Attr_Deprecated =>
                  S := 0;

               when others =>
                  pragma Assert (False);
                  null;
            end case;

            A (K).Attribute_Length := U4 (S);
         end loop;
      end Visit;

      procedure Process_Member_Attributes is new Member_Attribute.Process;
      --  Process the member attributes of a member of a class file

      procedure Process_Class_Attributes  is new Class_Attribute.Process;
      --  Process the class attributes of a class file

   --  Beginning of Compute_Attribute_Lengths

   begin
      --  Process the field attributes

      for M in 0 .. Last (CF.Fields) loop
         Process_Member_Attributes (Get (CF.Fields, M).Attributes);
      end loop;

      --  Process the method attributes including code attributes

      for M in 0 .. Last (CF.Methods) loop
         Process_Member_Attributes (Get (CF.Methods, M).Attributes);
      end loop;

      --  Process the class attributes

      Process_Class_Attributes (CF.Attributes);
   end Compute_Attribute_Lengths;

   ------------------
   -- Compute_Size --
   ------------------

   function Compute_Size (CF : Class_File) return Nat_32 is
      S : Nat_32 := Class_File_Base_Size;

      CP_Entry : CP_Info;

      MAT : Member_Attribute.Table;
      CAT : Class_Attribute.Table;

      procedure Compute_Member_Size (MT : Member.Table);
      --  Add up the size of a member table

      procedure Compute_Member_Size (MT : Member.Table) is
      begin
         for J in 0 .. Last (MT) loop
            S  := S + Member_Info_Base_Size;
            MAT := Get (MT, J).Attributes;
            for K in 0 .. Last (MAT) loop
               S := S + Attribute_Base_Size
                    + Nat_32 (Get (MAT, K).Attribute_Length);
            end loop;
         end loop;
      end Compute_Member_Size;

   --  Start of processing for Compute_Size

   begin
      --  First compute the size of the constant pool entries and make sure the
      --  0-th entry has a CONSTANT_Empty tag.

      pragma Assert
        (Get (CF.Constant_Pool, CP_Index'(0)).Tag = CONSTANT_Empty);

      for J in 1 .. Last (CF.Constant_Pool) loop
         CP_Entry := Get (CF.Constant_Pool, J);
         S        := S + CP_Info_Size (CP_Entry.Tag);

         if CP_Entry.Tag = CONSTANT_Utf8 then
            S := S + Length (CP_Entry.Str_Bytes);
         end if;
      end loop;

      --  Add the number of interfaces this class implements

      S := S + 2 * Length (CF.Interfaces);

      --  Add the size of the member fields included in CF

      Compute_Member_Size (CF.Fields);
      Compute_Member_Size (CF.Methods);

      --  Add the size of CF's attributes

      CAT := CF.Attributes;
      for K in 0 .. Last (CAT) loop
         S := S + Attribute_Base_Size + Nat_32 (Get (CAT, K).Attribute_Length);
      end loop;

      return S;
   end Compute_Size;

   -----------------------------
   -- Free_To_Next_Water_Mark --
   -----------------------------

   procedure Free_To_Next_Water_Mark is
   begin
      Utf8             . Free_To_Next_Water_Mark;
      Class_Index      . Free_To_Next_Water_Mark;
      CP               . Free_To_Next_Water_Mark;
      Code_Array       . Free_To_Next_Water_Mark;
      Handler          . Free_To_Next_Water_Mark;
      Line             . Free_To_Next_Water_Mark;
      Variable         . Free_To_Next_Water_Mark;
      Inner_Class      . Free_To_Next_Water_Mark;
      Code_Attribute   . Free_To_Next_Water_Mark;
      Member_Attribute . Free_To_Next_Water_Mark;
      Class_Attribute  . Free_To_Next_Water_Mark;
      Member           . Free_To_Next_Water_Mark;
   end Free_To_Next_Water_Mark;

   ---------
   -- Get --
   ---------

   function Get (T : Code_Array.Table; P : Byte_Index) return U4 is
   begin
      return
        To_U4 (Get (T, P), Get (T, P + 1), Get (T, P + 2), Get (T, P + 3));
   end Get;

   function Get (T : CA_Table; K : Instruction_Index) return Instruction is
      Current_Pos : Byte_Index;
      --  Current byte index position in table T

      --  Variables local to Get

      Nb_Of_Jumps : U4          := 0;
      Opcode      : constant U1 := Get (T, K);
      Op          : Operation;

   --  Beginning of Get

   begin
      if Opcode = Wide_Modifier then
         Current_Pos := K + 2;
         Op          := To_Operation (Get (T, K + 1));
      else
         Current_Pos := K + 1;
         Op          := To_Operation (Opcode);

         if Op in Var_Size_Operation then

            --  Skip the padding

            Current_Pos := Current_Pos + 3 - (K mod 4);
            Nb_Of_Jumps := Number_Of_Jumps (T, K);
         end if;
      end if;

      declare
         Ins : Instruction (Op, Nb_Of_Jumps);
      begin
         if Opcode = Wide_Modifier then
            Set_Wide_Format (Ins);
         end if;

         return Ins;
      end;
   end Get;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (T : CP.Table; K : CP_Index) return Attribute_Kind is
      UT : constant Utf8.Table := Get (T, K).Str_Bytes;

   begin
      if Has_Wide_Chars (UT) then
         return Attr_Unknown;
      end if;

      declare
         S : constant String := To_String (UT);
      begin
         for A in Attribute_Kind loop
            if Attribute_Name (A) = S then
               return A;
            end if;
         end loop;

         return Attr_Unknown;
      end;
   end Get_Attribute;

   --------------------
   -- Has_Wide_Chars --
   --------------------

   function Has_Wide_Chars (T : Utf8.Table) return Boolean is
   begin
      for J in 0 .. Last (T) loop
         if Get (T, J) > Encoding_Of_Char_255 then
            return True;
         end if;
      end loop;

      return False;
   end Has_Wide_Chars;

   ---------------------
   -- Has_Wide_Format --
   ---------------------

   function Has_Wide_Format (Op : Operation) return Boolean is
   begin
      return Instruction_Size (Op) < 0;
   end Has_Wide_Format;

   -----------------
   -- Is_Constant --
   -----------------

   function Is_Constant (T : CP.Table; K : CP_Index) return Boolean is
   begin
      return Is_Constant_U4 (T, K) or else Is_Constant_U8 (T, K);
   end Is_Constant;

   --------------------
   -- Is_Constant_U4 --
   --------------------

   function Is_Constant_U4 (T : CP.Table; K : CP_Index) return Boolean is
   begin
      return    Is_Integer (T, K)
        or else Is_Float   (T, K)
        or else Is_String  (T, K);
   end Is_Constant_U4;

   --------------------
   -- Is_Constant_U8 --
   --------------------

   function Is_Constant_U8 (T : CP.Table; K : CP_Index) return Boolean is
   begin
      return Is_Long (T, K) or else Is_Double (T, K);
   end Is_Constant_U8;

   --------------
   -- Is_Class --
   --------------

   function Is_Class (T : CP.Table; K : CP_Index) return Boolean is
   begin
      return Get (T, K).Tag = CONSTANT_Class;
   end Is_Class;

   ---------------
   -- Is_Double --
   ---------------

   function Is_Double (T : CP.Table; K : CP_Index) return Boolean is
   begin
      return Get (T, K).Tag = CONSTANT_Double;
   end Is_Double;

   -----------------
   -- Is_Fieldref --
   -----------------

   function Is_Fieldref (T : CP.Table; K : CP_Index) return Boolean is
   begin
      return Get (T, K).Tag = CONSTANT_Fieldref;
   end Is_Fieldref;

   --------------
   -- Is_Float --
   --------------

   function Is_Float (T : CP.Table; K : CP_Index) return Boolean is
   begin
      return Get (T, K).Tag = CONSTANT_Float;
   end Is_Float;

   ----------------
   -- Is_Integer --
   ----------------

   function Is_Integer (T : CP.Table; K : CP_Index) return Boolean is
   begin
      return Get (T, K).Tag = CONSTANT_Integer;
   end Is_Integer;

   ----------------------------
   -- Is_Interface_Methodref --
   ----------------------------

   subtype Bool is Boolean;

   function Is_Interface_Methodref (T : CP.Table; K : CP_Index) return Bool is
   begin
      return Get (T, K).Tag = CONSTANT_Interface_Methodref;
   end Is_Interface_Methodref;

   -------------
   -- Is_Long --
   -------------

   function Is_Long (T : CP.Table; K : CP_Index) return Boolean is
   begin
      return Get (T, K).Tag = CONSTANT_Long;
   end Is_Long;

   ------------------
   -- Is_Methodref --
   ------------------

   function Is_Methodref (T : CP.Table; K : CP_Index) return Boolean is
   begin
      return Get (T, K).Tag = CONSTANT_Methodref;
   end Is_Methodref;

   ----------------------
   -- Is_Name_And_Type --
   ----------------------

   function Is_Name_And_Type (T : CP.Table; K : CP_Index) return Boolean is
   begin
      return Get (T, K).Tag = CONSTANT_Name_And_Type;
   end Is_Name_And_Type;

   ---------------
   -- Is_String --
   ---------------

   function Is_String (T : CP.Table; K : CP_Index) return Boolean is
   begin
      return Get (T, K).Tag = CONSTANT_String;
   end Is_String;

   -------------
   -- Is_Utf8 --
   -------------

   function Is_Utf8 (T : CP.Table; K : CP_Index) return Boolean is
   begin
      return Get (T, K).Tag = CONSTANT_Utf8;
   end Is_Utf8;

   ------------
   -- Is_Set --
   ------------

   function Is_Set (M : Access_Mask; F : Access_Flag) return Boolean is
   begin
      return (M and To_Access_Mask (F)) > 0;
   end Is_Set;

   -----------------------
   -- Needs_Wide_Format --
   -----------------------

   function Needs_Wide_Format (I : Instruction) return Boolean is
   begin
      if not Has_Wide_Format (I.Op) then
         return False;
      end if;

      if I.Op = Iinc then
         return    I.Wide_Iinc
           or else (I.Var_Iinc > 255)
           or else (I.Const_Val not in -128 .. 127);
      else
         return I.Wide or else (I.Local_Var > 255);
      end if;
   end Needs_Wide_Format;

   ----------------------
   -- Next_Instruction --
   ----------------------

   function Next_Instruction (T : CA_Table; K : I_Index) return I_Index is
   begin
      return K + Instruction_Index (Size (T, K));
   end Next_Instruction;

   ---------------------
   -- Number_Of_Jumps --
   ---------------------

   function Number_Of_Jumps (T : Code_Array.Table; K : I_Index) return U4 is
      Op : constant Operation := To_Operation (Get (T, K));

      Padding : constant Byte_Index := 3 - (K mod 4);
      Offset  : constant Byte_Index := K + 5 + Padding;
      --  Offset of Npairs for Lookupswitch or Low for Tableswitch

   begin
      if Op = Lookupswitch then
         return Get (T, Offset);

      elsif Op = Tableswitch then
         return Get (T, Offset + 4) - Get (T, Offset) + 1;

      else
         pragma Assert (False);
         return 0;
      end if;
   end Number_Of_Jumps;

   ------------------------
   -- Operation_Mnemonic --
   ------------------------

   function Operation_Mnemonic (Op : Operation) return String is
   begin
      if    Op = Newobject then
         return "new";
      elsif Op = Jump      then
         return "goto";
      elsif Op = Vreturn   then
         return "return";
      else
         declare
            Op_Image : String := Operation'Image (Op);
         begin
            GNAT.Case_Util.To_Lower (Op_Image);
            return Op_Image;
         end;
      end if;
   end Operation_Mnemonic;

   ----------
   -- Read --
   ----------

   function Read (Stream : Stream_Of_U1; Check : Boolean) return Class_File is
      pragma Unreferenced (Stream, Check);

      CF : Class_File;
      --  The Class_File object returned

   --  Beginning of Read

   begin
      --  ??? code removed
      --  Read_Class (CF, Check);
      return CF;
   end Read;

   ---------
   -- Set --
   ---------

   procedure Set (M : in out Access_Mask; F : Access_Flag) is
   begin
      M := M or To_Access_Mask (F);
   end Set;

   --------------------
   -- Set_Water_Mark --
   --------------------

   procedure Set_Water_Mark is
   begin
      Utf8             . Set_Water_Mark;
      Class_Index      . Set_Water_Mark;
      CP               . Set_Water_Mark;
      Code_Array       . Set_Water_Mark;
      Handler          . Set_Water_Mark;
      Line             . Set_Water_Mark;
      Variable         . Set_Water_Mark;
      Inner_Class      . Set_Water_Mark;
      Code_Attribute   . Set_Water_Mark;
      Member_Attribute . Set_Water_Mark;
      Class_Attribute  . Set_Water_Mark;
      Member           . Set_Water_Mark;
   end Set_Water_Mark;

   ---------------------
   -- Set_Wide_Format --
   ---------------------

   procedure Set_Wide_Format (I : in out Instruction) is
   begin
      pragma Assert (Has_Wide_Format (I.Op));

      if I.Op = Iinc then
         I.Wide_Iinc := True;
      else
         I.Wide := True;
      end if;
   end Set_Wide_Format;

   -------------------------------
   -- Set_Wide_Format_If_Needed --
   -------------------------------

   procedure Set_Wide_Format_If_Needed (I : in out Instruction) is
   begin
      if Needs_Wide_Format (I) then
         Set_Wide_Format (I);
      end if;
   end Set_Wide_Format_If_Needed;

   ----------
   -- Size --
   ----------

   function Size (Op : Operation; Wide : Boolean := False) return U4 is
      S : constant Int_32 := Instruction_Size (Op);

   begin
      if S >= 0 then
         pragma Assert (not Wide);
         return U4 (S);
      elsif not Wide then
         return U4 (-S);
      else
         return U4 ((-2) * S);
      end if;
   end Size;

   function Size (Op : Var_Size_Operation; Off : I_Index; N : U4) return U4 is
      Pad : constant U4 := 3 - U4 (Off mod 4);

   begin
      if Op = Lookupswitch then
         return Lookupswitch_Base_Size + Pad + Lookup_Info_Size    * N;
      elsif Op = Tableswitch then
         return Tableswitch_Base_Size  + Pad + Jump_Offset_U4_Size * N;
      else
         pragma Assert (False);
         return 0;
      end if;
   end Size;

   function Size (T : CA_Table; K : I_Index) return U4 is
      Opcode : constant U1 := Get (T, K);
      Op     : Operation;
      S      : U4;

   begin
      if Opcode /= Wide_Modifier then
         Op := To_Operation (Opcode);
      else
         Op := To_Operation (Get (T, K + 1));
      end if;

      S := Size (Op, Opcode = Wide_Modifier);

      if S > 0 then
         return S;
      else
         return Size (Op, K, Number_Of_Jumps (T, K));
      end if;
   end Size;

   ------------------
   -- To_Operation --
   ------------------

   function To_Operation (U : U1) return Operation is
   begin
      return Operation'Val (U);
   end To_Operation;

   ---------------
   -- To_String --
   ---------------

   function To_String (T : Utf8.Table) return String is
      S : String (1 .. Natural (Length (T)));
      K : Natural := S'First - 1;
      --  Points to the last valid wide character in S

      U                : U1;
      Only_ASCII_Chars : Boolean := True;

   begin
      --  First see if we have a string of ASCII characters since we can deal
      --  with this easily.

      for J in 0 .. Last (T) loop
         K     := K + 1;
         U     := Get (T, J);
         S (K) := Character'Val (U);

         if U > 16#7F# then
            Only_ASCII_Chars := False;
            exit;
         end if;
      end loop;

      if Only_ASCII_Chars then
         return S;
      end if;

      --  Otherwise we must convert Utf8 characters into regular characters

      pragma Assert (not Has_Wide_Chars (T));

      Convert_Wide_String_To_String : declare
         W : constant Wide_String := To_Wide_String (T);

      begin
         K := S'First - 1;
         for J in W'Range loop
            K     := K + 1;
            S (K) := Character'Val (Wide_Character'Pos (W (J)));
         end loop;

         return S (S'First .. K);
      end Convert_Wide_String_To_String;
   end To_String;

   --------------------
   -- To_Wide_String --
   --------------------

   function To_Wide_String (T : Utf8.Table) return Wide_String is
      W : Wide_String (1 .. Integer (Length (T)));
      --  Will contain the wide string equivalent for T

      K : Natural := W'First - 1;
      --  Points to the last valid wide character in W

      J : Nat_32 := 0;
      U : U2;

   begin
      while J <= Last (T) loop
         U := U2 (Get (T, J));

         if U < 16#80# then
            J := J + 1;

         elsif U < IIIO_Mask then
            U := Shift_Left (U                  and 16#1F#, 6) +
                             U2 (Get (T, J + 1) and 16#3F#);
            J := J + 2;

         else
            U := Shift_Left (U                  and 16#0F#, 12) +
                 Shift_Left (U2 (Get (T, J + 1) and 16#3F#), 6) +
                             U2 (Get (T, J + 2) and 16#3F#);
            J := J + 3;
         end if;

         K     := K + 1;
         W (K) := Wide_Character'Val (U);
      end loop;

      return W (1 .. K);
   end To_Wide_String;

   -----------
   -- Write --
   -----------

   procedure Write (CF : in out Class_File; Stream : out Stream_Of_U1) is
      pragma Unreferenced (CF);
   begin
      --  ???
      Stream := Empty_Stream;
   end Write;

begin

   pragma Assert (Operation'Pos (Nop)             = 16#00#);
   pragma Assert (Operation'Pos (Aconst_Null)     = 16#01#);
   pragma Assert (Operation'Pos (Iconst_M1)       = 16#02#);
   pragma Assert (Operation'Pos (Iconst_0)        = 16#03#);
   pragma Assert (Operation'Pos (Iconst_1)        = 16#04#);
   pragma Assert (Operation'Pos (Iconst_2)        = 16#05#);
   pragma Assert (Operation'Pos (Iconst_3)        = 16#06#);
   pragma Assert (Operation'Pos (Iconst_4)        = 16#07#);
   pragma Assert (Operation'Pos (Iconst_5)        = 16#08#);
   pragma Assert (Operation'Pos (Lconst_0)        = 16#09#);
   pragma Assert (Operation'Pos (Lconst_1)        = 16#0A#);
   pragma Assert (Operation'Pos (Fconst_0)        = 16#0B#);
   pragma Assert (Operation'Pos (Fconst_1)        = 16#0C#);
   pragma Assert (Operation'Pos (Fconst_2)        = 16#0D#);
   pragma Assert (Operation'Pos (Dconst_0)        = 16#0E#);
   pragma Assert (Operation'Pos (Dconst_1)        = 16#0F#);
   pragma Assert (Operation'Pos (Bipush)          = 16#10#);
   pragma Assert (Operation'Pos (Sipush)          = 16#11#);
   pragma Assert (Operation'Pos (Ldc)             = 16#12#);
   pragma Assert (Operation'Pos (Ldc_W)           = 16#13#);
   pragma Assert (Operation'Pos (Ldc2_W)          = 16#14#);
   pragma Assert (Operation'Pos (Iload)           = 16#15#);
   pragma Assert (Operation'Pos (Lload)           = 16#16#);
   pragma Assert (Operation'Pos (Fload)           = 16#17#);
   pragma Assert (Operation'Pos (Dload)           = 16#18#);
   pragma Assert (Operation'Pos (Aload)           = 16#19#);
   pragma Assert (Operation'Pos (Iload_0)         = 16#1A#);
   pragma Assert (Operation'Pos (Iload_1)         = 16#1B#);
   pragma Assert (Operation'Pos (Iload_2)         = 16#1C#);
   pragma Assert (Operation'Pos (Iload_3)         = 16#1D#);
   pragma Assert (Operation'Pos (Lload_0)         = 16#1E#);
   pragma Assert (Operation'Pos (Lload_1)         = 16#1F#);
   pragma Assert (Operation'Pos (Lload_2)         = 16#20#);
   pragma Assert (Operation'Pos (Lload_3)         = 16#21#);
   pragma Assert (Operation'Pos (Fload_0)         = 16#22#);
   pragma Assert (Operation'Pos (Fload_1)         = 16#23#);
   pragma Assert (Operation'Pos (Fload_2)         = 16#24#);
   pragma Assert (Operation'Pos (Fload_3)         = 16#25#);
   pragma Assert (Operation'Pos (Dload_0)         = 16#26#);
   pragma Assert (Operation'Pos (Dload_1)         = 16#27#);
   pragma Assert (Operation'Pos (Dload_2)         = 16#28#);
   pragma Assert (Operation'Pos (Dload_3)         = 16#29#);
   pragma Assert (Operation'Pos (Aload_0)         = 16#2A#);
   pragma Assert (Operation'Pos (Aload_1)         = 16#2B#);
   pragma Assert (Operation'Pos (Aload_2)         = 16#2C#);
   pragma Assert (Operation'Pos (Aload_3)         = 16#2D#);
   pragma Assert (Operation'Pos (Iaload)          = 16#2E#);
   pragma Assert (Operation'Pos (Laload)          = 16#2F#);
   pragma Assert (Operation'Pos (Faload)          = 16#30#);
   pragma Assert (Operation'Pos (Daload)          = 16#31#);
   pragma Assert (Operation'Pos (Aaload)          = 16#32#);
   pragma Assert (Operation'Pos (Baload)          = 16#33#);
   pragma Assert (Operation'Pos (Caload)          = 16#34#);
   pragma Assert (Operation'Pos (Saload)          = 16#35#);
   pragma Assert (Operation'Pos (Istore)          = 16#36#);
   pragma Assert (Operation'Pos (Lstore)          = 16#37#);
   pragma Assert (Operation'Pos (Fstore)          = 16#38#);
   pragma Assert (Operation'Pos (Dstore)          = 16#39#);
   pragma Assert (Operation'Pos (Astore)          = 16#3A#);
   pragma Assert (Operation'Pos (Istore_0)        = 16#3B#);
   pragma Assert (Operation'Pos (Istore_1)        = 16#3C#);
   pragma Assert (Operation'Pos (Istore_2)        = 16#3D#);
   pragma Assert (Operation'Pos (Istore_3)        = 16#3E#);
   pragma Assert (Operation'Pos (Lstore_0)        = 16#3F#);
   pragma Assert (Operation'Pos (Lstore_1)        = 16#40#);
   pragma Assert (Operation'Pos (Lstore_2)        = 16#41#);
   pragma Assert (Operation'Pos (Lstore_3)        = 16#42#);
   pragma Assert (Operation'Pos (Fstore_0)        = 16#43#);
   pragma Assert (Operation'Pos (Fstore_1)        = 16#44#);
   pragma Assert (Operation'Pos (Fstore_2)        = 16#45#);
   pragma Assert (Operation'Pos (Fstore_3)        = 16#46#);
   pragma Assert (Operation'Pos (Dstore_0)        = 16#47#);
   pragma Assert (Operation'Pos (Dstore_1)        = 16#48#);
   pragma Assert (Operation'Pos (Dstore_2)        = 16#49#);
   pragma Assert (Operation'Pos (Dstore_3)        = 16#4A#);
   pragma Assert (Operation'Pos (Astore_0)        = 16#4B#);
   pragma Assert (Operation'Pos (Astore_1)        = 16#4C#);
   pragma Assert (Operation'Pos (Astore_2)        = 16#4D#);
   pragma Assert (Operation'Pos (Astore_3)        = 16#4E#);
   pragma Assert (Operation'Pos (Iastore)         = 16#4F#);
   pragma Assert (Operation'Pos (Lastore)         = 16#50#);
   pragma Assert (Operation'Pos (Fastore)         = 16#51#);
   pragma Assert (Operation'Pos (Dastore)         = 16#52#);
   pragma Assert (Operation'Pos (Aastore)         = 16#53#);
   pragma Assert (Operation'Pos (Bastore)         = 16#54#);
   pragma Assert (Operation'Pos (Castore)         = 16#55#);
   pragma Assert (Operation'Pos (Sastore)         = 16#56#);
   pragma Assert (Operation'Pos (Pop)             = 16#57#);
   pragma Assert (Operation'Pos (Pop2)            = 16#58#);
   pragma Assert (Operation'Pos (Dup)             = 16#59#);
   pragma Assert (Operation'Pos (Dup_X1)          = 16#5A#);
   pragma Assert (Operation'Pos (Dup_X2)          = 16#5B#);
   pragma Assert (Operation'Pos (Dup2)            = 16#5C#);
   pragma Assert (Operation'Pos (Dup2_X1)         = 16#5D#);
   pragma Assert (Operation'Pos (Dup2_X2)         = 16#5E#);
   pragma Assert (Operation'Pos (Swap)            = 16#5F#);
   pragma Assert (Operation'Pos (Iadd)            = 16#60#);
   pragma Assert (Operation'Pos (Ladd)            = 16#61#);
   pragma Assert (Operation'Pos (Fadd)            = 16#62#);
   pragma Assert (Operation'Pos (Dadd)            = 16#63#);
   pragma Assert (Operation'Pos (Isub)            = 16#64#);
   pragma Assert (Operation'Pos (Lsub)            = 16#65#);
   pragma Assert (Operation'Pos (Fsub)            = 16#66#);
   pragma Assert (Operation'Pos (Dsub)            = 16#67#);
   pragma Assert (Operation'Pos (Imul)            = 16#68#);
   pragma Assert (Operation'Pos (Lmul)            = 16#69#);
   pragma Assert (Operation'Pos (Fmul)            = 16#6A#);
   pragma Assert (Operation'Pos (Dmul)            = 16#6B#);
   pragma Assert (Operation'Pos (Idiv)            = 16#6C#);
   pragma Assert (Operation'Pos (Ldiv)            = 16#6D#);
   pragma Assert (Operation'Pos (Fdiv)            = 16#6E#);
   pragma Assert (Operation'Pos (Ddiv)            = 16#6F#);
   pragma Assert (Operation'Pos (Irem)            = 16#70#);
   pragma Assert (Operation'Pos (Lrem)            = 16#71#);
   pragma Assert (Operation'Pos (Frem)            = 16#72#);
   pragma Assert (Operation'Pos (Drem)            = 16#73#);
   pragma Assert (Operation'Pos (Ineg)            = 16#74#);
   pragma Assert (Operation'Pos (Lneg)            = 16#75#);
   pragma Assert (Operation'Pos (Fneg)            = 16#76#);
   pragma Assert (Operation'Pos (Dneg)            = 16#77#);
   pragma Assert (Operation'Pos (Ishl)            = 16#78#);
   pragma Assert (Operation'Pos (Lshl)            = 16#79#);
   pragma Assert (Operation'Pos (Ishr)            = 16#7A#);
   pragma Assert (Operation'Pos (Lshr)            = 16#7B#);
   pragma Assert (Operation'Pos (Iushr)           = 16#7C#);
   pragma Assert (Operation'Pos (Lushr)           = 16#7D#);
   pragma Assert (Operation'Pos (Iand)            = 16#7E#);
   pragma Assert (Operation'Pos (Land)            = 16#7F#);
   pragma Assert (Operation'Pos (Ior)             = 16#80#);
   pragma Assert (Operation'Pos (Lor)             = 16#81#);
   pragma Assert (Operation'Pos (Ixor)            = 16#82#);
   pragma Assert (Operation'Pos (Lxor)            = 16#83#);
   pragma Assert (Operation'Pos (Iinc)            = 16#84#);
   pragma Assert (Operation'Pos (I2l)             = 16#85#);
   pragma Assert (Operation'Pos (I2f)             = 16#86#);
   pragma Assert (Operation'Pos (I2d)             = 16#87#);
   pragma Assert (Operation'Pos (L2i)             = 16#88#);
   pragma Assert (Operation'Pos (L2f)             = 16#89#);
   pragma Assert (Operation'Pos (L2d)             = 16#8A#);
   pragma Assert (Operation'Pos (F2i)             = 16#8B#);
   pragma Assert (Operation'Pos (F2l)             = 16#8C#);
   pragma Assert (Operation'Pos (F2d)             = 16#8D#);
   pragma Assert (Operation'Pos (D2i)             = 16#8E#);
   pragma Assert (Operation'Pos (D2l)             = 16#8F#);
   pragma Assert (Operation'Pos (D2f)             = 16#90#);
   pragma Assert (Operation'Pos (I2b)             = 16#91#);
   pragma Assert (Operation'Pos (I2c)             = 16#92#);
   pragma Assert (Operation'Pos (I2s)             = 16#93#);
   pragma Assert (Operation'Pos (Lcmp)            = 16#94#);
   pragma Assert (Operation'Pos (Fcmpl)           = 16#95#);
   pragma Assert (Operation'Pos (Fcmpg)           = 16#96#);
   pragma Assert (Operation'Pos (Dcmpl)           = 16#97#);
   pragma Assert (Operation'Pos (Dcmpg)           = 16#98#);
   pragma Assert (Operation'Pos (Ifeq)            = 16#99#);
   pragma Assert (Operation'Pos (Ifne)            = 16#9A#);
   pragma Assert (Operation'Pos (Iflt)            = 16#9B#);
   pragma Assert (Operation'Pos (Ifge)            = 16#9C#);
   pragma Assert (Operation'Pos (Ifgt)            = 16#9D#);
   pragma Assert (Operation'Pos (Ifle)            = 16#9E#);
   pragma Assert (Operation'Pos (If_Icmpeq)       = 16#9F#);
   pragma Assert (Operation'Pos (If_Icmpne)       = 16#A0#);
   pragma Assert (Operation'Pos (If_Icmplt)       = 16#A1#);
   pragma Assert (Operation'Pos (If_Icmpge)       = 16#A2#);
   pragma Assert (Operation'Pos (If_Icmpgt)       = 16#A3#);
   pragma Assert (Operation'Pos (If_Icmple)       = 16#A4#);
   pragma Assert (Operation'Pos (If_Acmpeq)       = 16#A5#);
   pragma Assert (Operation'Pos (If_Acmpne)       = 16#A6#);
   pragma Assert (Operation'Pos (Jump)            = 16#A7#);
   pragma Assert (Operation'Pos (Jsr)             = 16#A8#);
   pragma Assert (Operation'Pos (Ret)             = 16#A9#);
   pragma Assert (Operation'Pos (Tableswitch)     = 16#AA#);
   pragma Assert (Operation'Pos (Lookupswitch)    = 16#AB#);
   pragma Assert (Operation'Pos (Ireturn)         = 16#AC#);
   pragma Assert (Operation'Pos (Lreturn)         = 16#AD#);
   pragma Assert (Operation'Pos (Freturn)         = 16#AE#);
   pragma Assert (Operation'Pos (Dreturn)         = 16#AF#);
   pragma Assert (Operation'Pos (Areturn)         = 16#B0#);
   pragma Assert (Operation'Pos (Vreturn)         = 16#B1#);
   pragma Assert (Operation'Pos (Getstatic)       = 16#B2#);
   pragma Assert (Operation'Pos (Putstatic)       = 16#B3#);
   pragma Assert (Operation'Pos (Getfield)        = 16#B4#);
   pragma Assert (Operation'Pos (Putfield)        = 16#B5#);
   pragma Assert (Operation'Pos (Invokevirtual)   = 16#B6#);
   pragma Assert (Operation'Pos (Invokespecial)   = 16#B7#);
   pragma Assert (Operation'Pos (Invokestatic)    = 16#B8#);
   pragma Assert (Operation'Pos (Invokeinterface) = 16#B9#);
   pragma Assert (Operation'Pos (Xxxunusedxxx)    = 16#BA#);
   pragma Assert (Operation'Pos (Newobject)       = 16#BB#);
   pragma Assert (Operation'Pos (Newarray)        = 16#BC#);
   pragma Assert (Operation'Pos (Anewarray)       = 16#BD#);
   pragma Assert (Operation'Pos (Arraylength)     = 16#BE#);
   pragma Assert (Operation'Pos (Athrow)          = 16#BF#);
   pragma Assert (Operation'Pos (Checkcast)       = 16#C0#);
   pragma Assert (Operation'Pos (Instanceof)      = 16#C1#);
   pragma Assert (Operation'Pos (Monitorenter)    = 16#C2#);
   pragma Assert (Operation'Pos (Monitorexit)     = 16#C3#);
   pragma Assert (Operation'Pos (Wide)            = 16#C4#);
   pragma Assert (Operation'Pos (Multianewarray)  = 16#C5#);
   pragma Assert (Operation'Pos (Ifnull)          = 16#C6#);
   pragma Assert (Operation'Pos (Ifnonnull)       = 16#C7#);
   pragma Assert (Operation'Pos (Goto_W)          = 16#C8#);
   pragma Assert (Operation'Pos (Jsr_W)           = 16#C9#);

   null;
end JVM_File;
