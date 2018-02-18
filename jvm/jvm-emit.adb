------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M . E M I T                              --
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

with J_Basics;    use J_Basics;
with J_String;    use J_String;
with JVM_File;    use JVM_File;
with JVM.Code;    use JVM.Code;
with JVM.Info;    use JVM.Info;
with JVM.Pool;    use JVM.Pool;
with Sinput;

package body JVM.Emit is

   Gen_Class : Class_Id := Null_Class;
   --  Designates the class whose class file is actively being generated

   Gen_Pool : CP.Table;
   --  The constant pool table associated with Gen_Class's class file

   Next_CP_Index : CP_Index := CP_Empty;
   --  Keeps track of Gen_Pool's current constant pool index

   --  Type_Char gives the JVM-specified type character for each basic
   --  form of JVM type (used in Utf8 field and method descriptors).

   Type_Char : constant array (JVM_Type_Kind) of Character :=
     (Void_Kind        => 'V',
      Boolean_Kind     => 'Z',
      Byte_Kind        => 'B',
      Char_Kind        => 'C',
      Short_Kind       => 'S',
      Int_Kind         => 'I',
      Long_Kind        => 'J',
      Float_Kind       => 'F',
      Double_Kind      => 'D',
      Array_Kind       => '[',
      Class_Kind       => 'L',
      Return_Addr_Kind => '?');

   Code_Attr_Utf8 : CP_Index_Utf8 := CP_Empty;
   Excs_Attr_Utf8 : CP_Index_Utf8 := CP_Empty;
   Line_Attr_Utf8 : CP_Index_Utf8 := CP_Empty;
   Local_Variable_Attr_Utf8 : CP_Index_Utf8 := CP_Empty;
   --  Above variables will hold a constant pool index to
   --  the names of the Code and Exceptions attributes for
   --  for methods of the class being generated. Used to
   --  avoid redundant emission of CP entries.

   subtype Short_Local_Index is Local_Variable_Index range 0 .. 3;
   --  Range of local indexes that allow use of short load/store instructions

   subtype Load_Ops is Operation range Iload .. Aload;

   subtype Store_Ops is Operation range Istore .. Astore;

   Short_Load_Ops : constant array (Load_Ops, Short_Local_Index) of Operation
     := (Iload => (Iload_0, Iload_1, Iload_2, Iload_3),
         Lload => (Lload_0, Lload_1, Lload_2, Lload_3),
         Fload => (Fload_0, Fload_1, Fload_2, Fload_3),
         Dload => (Dload_0, Dload_1, Dload_2, Dload_3),
         Aload => (Aload_0, Aload_1, Aload_2, Aload_3));

   Short_Store_Ops : constant array (Store_Ops, Short_Local_Index) of Operation
     := (Istore => (Istore_0, Istore_1, Istore_2, Istore_3),
         Lstore => (Lstore_0, Lstore_1, Lstore_2, Lstore_3),
         Fstore => (Fstore_0, Fstore_1, Fstore_2, Fstore_3),
         Dstore => (Dstore_0, Dstore_1, Dstore_2, Dstore_3),
         Astore => (Astore_0, Astore_1, Astore_2, Astore_3));

   ----------------------------------------------
   -- Name and String-related Utility Routines --
   ----------------------------------------------

   function Params_String (Params : Local_Var_Id) return String;
   --  Returns a string representing the concatenation of
   --  the type descriptor strings for a sequence of method
   --  parameters.

   function Full_Class_Name (Class : Class_Id; Filename : Boolean := False)
                             return String;
   --  Returns the internal form of the fully qualified class name
   --  associated with Class (e.g., for java.lang.String the internal
   --  form would be "java/lang/String").

   ---------------------------------------------
   -- Constant Pool Entry Generation Routines --
   ---------------------------------------------

   procedure Emit_CP_Class (Classref : Pool_Id);
   --  Emits a CONSTANT_Class entry in Gen_Pool for Classref

   procedure Emit_CP_Fieldref (Fieldref : Pool_Id);
   --  Emits a CONSTANT_Fieldref entry in Gen_Pool for Fieldref

   procedure Emit_CP_Methodref (Methodref : Pool_Id);
   --  Emits a CONSTANT_Methodref entry in Gen_Pool for Methodref

   procedure Emit_CP_Interface_Methodref (Methodref : Pool_Id);
   --  Emits a CONSTANT_Interface_Methodref entry in Gen_Pool for Methodref

   procedure Emit_CP_String (Str_Item : Pool_Id);
   --  Emits a CONSTANT_String entry in Gen_Pool for Str_Item

   procedure Emit_CP_Integer (Int_Item : Pool_Id);
   --  Emits a CONSTANT_Integer entry in Gen_Pool for Int_Item

   procedure Emit_CP_Long (Long_Item : Pool_Id);
   --  Emits a CONSTANT_Long entry in Gen_Pool for Long_Item

   procedure Emit_CP_Float (Float_Item : Pool_Id);
   --  Emits a CONSTANT_Float entry in Gen_Pool for Float_Item

   procedure Emit_CP_Double (Double_Item : Pool_Id);
   --  Emits a CONSTANT_Double entry in Gen_Pool for Double_Item

   procedure Emit_CP_Typeref (Typeref : Pool_Id);
   --  Emits a CONSTANT_Utf8 entry in Gen_Pool for Typeref

   procedure Emit_CP_Item (CP_Item : Pool_Id);
   --  Establishes an index in the constant pool Gen_Pool for the
   --  Pool_Item denoted by CP_Item and emits an entry in Gen_Class's
   --  constant pool based on CP_Item (unless the item already has
   --  an associated index and entry).

   function CP_Class_Index (Class : Class_Id) return CP_Index_Class;
   --  Returns the constant pool index of a CP_Info record with
   --  tag CONSTANT_Class corresponding to Class, creating the
   --  record in the constant pool if it doesn't already exist.

   --  function CP_Fieldref_Index (Field : Field_Id) return CP_Index_Field;
   --  --  Returns the constant pool index of a CP_Info record with
   --  --  tag CONSTANT_Fieldref corresponding to a reference to Field,
   --  --  creating the record in the constant pool if it doesn't already
   --  --  exist.

   --  function CP_Methodref_Index (Method : Method_Id) return CP_Index_Method;
   --  --  Returns the constant pool index of a CP_Info record with
   --  --  tag CONSTANT_Methodref corresponding to a reference to Method,
   --  --  creating the record in the constant pool if it doesn't already
   --  --  exist.

   --  function CP_Interface_Methodref_Index
   --    (Method : Method_Id)
   --       return CP_Index_Interface_Method;

   --  function CP_String_Index (Str : String_Id) return CP_Index_Constant;
   --  --  Returns the constant pool index of a CP_Info record with tag
   --  --  CONSTANT_String corresponding to the string value of Str,
   --  --  after creating the record in the constant pool.

   --  function CP_Integer_Index (Value : Uint) return CP_Index_Constant;
   --  --  Returns the constant pool index of a CP_Info record with tag
   --  --  CONSTANT_Integer corresponding to the integer value of Value,
   --  --  after creating the record in the constant pool.

   --  function CP_Long_Index (Value : Uint) return CP_Index_Constant;
   --  --  Returns the constant pool index of a CP_Info record with tag
   --  --  CONSTANT_Long corresponding to the integer value of Value,
   --  --  after creating the record in the constant pool.

   --  function CP_Float_Index (Value : Ureal) return CP_Index_Constant;
   --  --  Returns the constant pool index of a CP_Info record with tag
   --  --  CONSTANT_Float corresponding to the floating-point value of
   --  --  Value, after creating the record in the constant pool.

   --  function CP_Double_Index (Value : Ureal) return CP_Index_Constant;
   --  --  Returns the constant pool index of a CP_Info record with tag
   --  --  CONSTANT_Double corresponding to the floating-point value of
   --  --  Value, after creating the record in the constant pool.

   function CP_Name_And_Type_Index
     (Field : Field_Id)
       return CP_Index_Name_And_Type;
   --  Returns the constant pool index of a CP_Info record with tag
   --  CONSTANT_Name_And_Type corresponding to the name and type of
   --  Field, after creating the record in the constant pool.

   function CP_Name_And_Type_Index
     (Method : Method_Id)
        return CP_Index_Name_And_Type;
   --  Returns the constant pool index of a CP_Info record with tag
   --  CONSTANT_Name_And_Type corresponding to the name and method
   --  descriptor of Method, after creating the record in the constant
   --  pool.

   function CP_Utf8_Index (Str : String) return CP_Index_Utf8;
   --  Returns the constant pool index of a CP_Info record with tag
   --  CONSTANT_Utf8 containing the Utf8 value of Str, after creating
   --  the record in the constant pool.

   function CP_Utf8_Index (Name : Name_Id) return CP_Index_Utf8;
   --  Returns the constant pool index of a CP_Info record with tag
   --  CONSTANT_Utf8 containing the Utf8 value of the string name
   --  associated with Name, after creating the record in the
   --  constant pool.

   function CP_Utf8_Index (Typ : Type_Id) return CP_Index_Utf8;
   --  Returns the constant pool index of a CP_Info record with tag
   --  CONSTANT_Utf8 containing the Utf8 value of the type descriptor
   --  string associated with Typ, after creating the record in the
   --  constant pool.

   function CP_Utf8_Index (Method : Method_Id) return CP_Index_Utf8;
   --  Returns the constant pool index of a CP_Info record with tag
   --  CONSTANT_Utf8 containing the Utf8 value of the method descriptor
   --  string associated with Method, after creating the record in the
   --  constant pool.

   ----------------------------------------------------
   -- Primary Class File Generation Support Routines --
   ----------------------------------------------------

   procedure Emit_Field (Table : in out Member.Table; Field : Field_Id);
   --  Emits the constant pool description of Field in the constant
   --  pool associated with the class file of Gen_Class.

   procedure Emit_Method (Table : in out Member.Table; Method : Method_Id);
   --  Emits the constant pool description of Method in the constant
   --  pool associated with the class file of Gen_Class.

   procedure Emit_Code
     (Table  : in out Member_Attribute.Table;
      Method : Method_Id);
   --  Emits the code attribute for Method in the class file of Gen_Class.

   procedure Emit_Local_Variables
     (Code   : in out Member_Attribute_Info;
      Method : Method_Id);
   --  Emits the local variable table for the method

   procedure Emit_Line_Numbers
     (Lines  : in out Code_Attribute.Table;
      Method : Method_Id);
   --  Emits the line number attribute for the method.

   procedure Generate_Code
     (Method : Method_Id;
      Code   : in out Code_Array.Table);
   --  Generates all instructions for Method into the table Code.
   --  Resolves the branch offsets of any branch instructions.

   procedure Generate_Handler_Entries
     (Method   : Method_Id;
      Handlers : in out Handler.Table);
   --  Generates all exception handler entries for Method into the table
   --  Handler.

   function Instr_Size
     (Instr : JVM.Code.Instruction;
      Index : Instruction_Index)
     return   U4;
   --  Returns the size of the eventual JVM instruction in bytes, taking
   --  account of instructions that will be wide or variable-sized.

   procedure Compute_Instruction_Offset
     (Instr      : Instr_Id;
      Code_Index : in out Instruction_Index);
   --  Determines the byte offset of the J-code instruction corresponding
   --  to Instr, recording the offset in Instr if Instr is a branch or
   --  label, and increments Code_Index by the size of the instruction
   --  that will be generated for Instr.

   procedure Check_Branch_Size
     (Instr          : Instr_Id;
      Code_Increment : in out Instruction_Index);
   --  If Instr is a branch instruction then checks to see if it needs
   --  to be converted to use a 32-bit offset (based on its target label
   --  offset, which must be known at this point), and converts Instr to
   --  the long form when needed. Also increments Code_Increment to reflect
   --  the net increase in size, if any, due to converting a branch to its
   --  long form.

   procedure Generate_Instruction
     (Code  : in out Code_Array.Table;
      Instr : Instr_Id);
   --  Generates the J-code instruction corresponding to Instr
   --  in the table Code.

   ------------------
   -- Add_Assembly --
   ------------------

   procedure Add_Assembly (Name : String) is
      pragma Unreferenced (Name);
   begin
      null;
   end Add_Assembly;

   procedure Add_Assembly (Version : String; Name : String_Id) is
      pragma Unreferenced (Version, Name);
   begin
      null;
   end Add_Assembly;

   --------------------------------------------------------
   -- Bodies of Name and String-related Utility Routines --
   --------------------------------------------------------

   ---------------------
   -- Full_Class_Name --
   ---------------------

   function Full_Class_Name (Class : Class_Id; Filename : Boolean := False)
                             return String
   is
      Pkg   : String := Str (Pkg_Name (Class));

   begin
      if Class = Null_Class then
         return "";
      end if;

      --  Convert the package name to the special JVM internal
      --  version (e.g., "java.lang" => "java/lang").

      for Index in Pkg'Range loop
         if Pkg (Index) = '.' then
            Pkg (Index) := '/';
         end if;
      end loop;

      declare
         Outer : constant Class_Id := Outer_Class (Class);

      begin
         if Pkg'Length /= 0 and then not Filename then
            return Pkg & '/' & Name_String (Name (Class));
         elsif Outer /= Null_Class then
            return Full_Class_Name (Outer, Filename) &
              '$' & Name_String (Name (Class));
         else
            return Name_String (Name (Class));
         end if;
      end;
   end Full_Class_Name;

   -----------------
   -- Type_String --
   -----------------

   function Type_String (Typ : Type_Id) return String is
   begin
      case Type_Kind (Typ) is
         when Boolean_Kind | Byte_Kind  | Char_Kind | Short_Kind =>
            pragma Assert (False);
            return Type_Char (Type_Kind (Typ)) & "";

         when Int_Kind =>
            if Typ = Int_Type then
               return Type_Char (Type_Kind (Typ)) & "";
            elsif Typ = Boolean_Type then
               return Type_Char (Boolean_Kind) & "";
            elsif Typ = Byte_Type then
               return Type_Char (Byte_Kind) & "";
            elsif Typ = Char_Type then
               return Type_Char (Char_Kind) & "";
            elsif Typ = Short_Type then
               return Type_Char (Short_Kind) & "";
            else
               pragma Assert (False);
               return Type_Char (Type_Kind (Typ)) & "";
            end if;

         when Void_Kind | Long_Kind | Float_Kind | Double_Kind =>
            return Type_Char (Type_Kind (Typ)) & "";

         --  An array type descriptor has the form of a sequence of
         --  '[' characters (one for each dimension) followed by the
         --  type descriptor string for the element type.

         when Array_Kind =>
            return (1 .. Positive (Dimensions (Typ)) => Type_Char (Array_Kind))
                     & Type_String (Element_Type (Typ));

         --  A class's type descriptor has the form "Lclass_name;"

         when Class_Kind =>
            return
              Type_Char (Class_Kind) & Full_Class_Name (Class (Typ)) & ";";

         when Return_Addr_Kind =>
            pragma Assert (False);
            return Type_Char (Return_Addr_Kind) & "";
      end case;
   end Type_String;

   -------------------
   -- Params_String --
   -------------------

   function Params_String (Params : Local_Var_Id) return String is
   begin
      if Params = Null_Local_Var or else not Is_Param (Params) then
         return "";
      else
         --  Recursively apply the function to the remainder of the
         --  parameters in the list, concatenating the type descriptors
         --  of the parameters.

         return Type_String (Type_Of (Params))
           & Params_String (Next_Local_Var (Params));
      end if;
   end Params_String;

   -------------------------------------------------------
   -- Bodies of Constant Pool Entry Generation Routines --
   -------------------------------------------------------

   -------------------
   -- Emit_CP_Class --
   -------------------

   procedure Emit_CP_Class (Classref : Pool_Id) is
      C_Type   : constant Type_Id := Ref_Class_Type (Classref);
      CP_Class : CP_Info (CONSTANT_Class);

   begin
      pragma Assert (Pool_Index (Classref) = CP_Empty);

      pragma Assert (Is_Reference_Type (C_Type));

      --  For a CONSTANT_Class entry denoting a class, use the fully
      --  qualified name of the class for its Utf8 name.

      if Type_Kind (C_Type) = Class_Kind then
         CP_Class.Class_Name_Index
           := CP_Utf8_Index (Full_Class_Name (Class (C_Type)));

      --  Otherwise the type must be an array type, in which case we create
      --  a Uft8 entry that specifies the type descriptor for the array.

      else
         CP_Class.Class_Name_Index := CP_Utf8_Index (Type_String (C_Type));
      end if;

      CP.Add (Gen_Pool, CP_Class);
      Set_Pool_Index (Classref, Next_CP_Index);
      Next_CP_Index := CP_Index (CP.Length (Gen_Pool));
   end Emit_CP_Class;

   ----------------------
   -- Emit_CP_Fieldref --
   ----------------------

   procedure Emit_CP_Fieldref (Fieldref : Pool_Id) is
      Field       : constant Field_Id := Ref_Field (Fieldref);
      CP_Fieldref : CP_Info (CONSTANT_Fieldref);

   begin
      pragma Assert (Pool_Index (Fieldref) = CP_Empty);

      CP_Fieldref.Class_Index := CP_Class_Index (Class (Field));
      CP_Fieldref.Name_And_Type_Index := CP_Name_And_Type_Index (Field);

      CP.Add (Gen_Pool, CP_Fieldref);
      Set_Pool_Index (Fieldref, Next_CP_Index);
      Next_CP_Index := CP_Index (CP.Length (Gen_Pool));
   end Emit_CP_Fieldref;

   -----------------------
   -- Emit_CP_Methodref --
   -----------------------

   procedure Emit_CP_Methodref (Methodref : Pool_Id) is
      Method      : constant Method_Id := Ref_Method (Methodref);
      CP_Method : CP_Info (CONSTANT_Methodref);

   begin
      pragma Assert (Pool_Index (Methodref) = CP_Empty);

      CP_Method.Class_Index := CP_Class_Index (Class (Method));
      CP_Method.Name_And_Type_Index := CP_Name_And_Type_Index (Method);

      CP.Add (Gen_Pool, CP_Method);
      Set_Pool_Index (Methodref, Next_CP_Index);
      Next_CP_Index := CP_Index (CP.Length (Gen_Pool));
   end Emit_CP_Methodref;

   ---------------------------------
   -- Emit_CP_Interface_Methodref --
   ---------------------------------

   procedure Emit_CP_Interface_Methodref (Methodref : Pool_Id) is
      Method      : constant Method_Id := Ref_Method (Methodref);
      CP_Method : CP_Info (CONSTANT_Interface_Methodref);

   begin
      pragma Assert (Pool_Index (Methodref) = CP_Empty);

      CP_Method.Class_Index := CP_Class_Index (Class (Method));
      CP_Method.Name_And_Type_Index := CP_Name_And_Type_Index (Method);

      CP.Add (Gen_Pool, CP_Method);
      Set_Pool_Index (Methodref, Next_CP_Index);
      Next_CP_Index := CP_Index (CP.Length (Gen_Pool));
   end Emit_CP_Interface_Methodref;

   --------------------
   -- Emit_CP_String --
   --------------------

   procedure Emit_CP_String (Str_Item : Pool_Id) is
      CP_String : CP_Info (CONSTANT_String);

   begin
      CP_String.String_Index := CP_Utf8_Index (Str (Pool_String (Str_Item)));

      CP.Add (Gen_Pool, CP_String);
      Set_Pool_Index (Str_Item, Next_CP_Index);
      Next_CP_Index := CP_Index (CP.Length (Gen_Pool));
   end Emit_CP_String;

   ---------------------
   -- Emit_CP_Integer --
   ---------------------

   procedure Emit_CP_Integer (Int_Item : Pool_Id) is
      CP_Integer : CP_Info (CONSTANT_Integer);
      Int_Value  : constant Uint := Pool_Integer (Int_Item);
      Value32    : Int_32;

   begin
      Value32 := Int_32 (UI_To_Int (Int_Value));
      CP_Integer.Bytes := To_U4 (Value32);

      CP.Add (Gen_Pool, CP_Integer);
      Set_Pool_Index (Int_Item, Next_CP_Index);
      Next_CP_Index := CP_Index (CP.Length (Gen_Pool));
   end Emit_CP_Integer;

   ------------------
   -- Emit_CP_Long --
   ------------------

   procedure Emit_CP_Long (Long_Item : Pool_Id) is
      CP_Long    : CP_Info (CONSTANT_Long);
      Long_Value : Uint := Pool_Long (Long_Item);
      Negative   : constant Boolean := (Long_Value < 0);

   begin

      --  Special case for Long_Long_Integer'First
      if Long_Value = -(Uint_2 ** 63) then
         CP_Long.Low_Bytes := 0;
         CP_Long.High_Bytes := 2 ** 31;

      else

         pragma Assert (Long_Value < Uint_2 ** 63);
         pragma Assert (Long_Value > -(Uint_2 ** 63));

         Long_Value := abs (Long_Value);

         CP_Long.Low_Bytes
           := To_U4 (Int_32 (UI_To_Int (Long_Value mod (Uint_2 ** 16))));
         Long_Value := Long_Value / (Uint_2 ** 16);
         CP_Long.Low_Bytes
           := CP_Long.Low_Bytes
           + To_U4 (Int_32 (UI_To_Int (Long_Value mod (Uint_2 ** 16))))
           * (2 ** 16);

         Long_Value := Long_Value / (Uint_2 ** 16);
         CP_Long.High_Bytes
           := To_U4 (Int_32 (UI_To_Int (Long_Value mod (Uint_2 ** 16))));
         Long_Value := Long_Value / (Uint_2 ** 16);
         CP_Long.High_Bytes
           := CP_Long.High_Bytes
           + To_U4 (Int_32 (UI_To_Int (Long_Value mod (Uint_2 ** 16))))
           * (2 ** 16);

         if Negative then
            CP_Long.High_Bytes
              := To_U4 (Int_32 (-1) - Int_32 (CP_Long.High_Bytes));
            CP_Long.Low_Bytes
              := To_U4 (-Int_32 (CP_Long.Low_Bytes));
         end if;

      end if;

      CP.Add (Gen_Pool, CP_Long);
      Set_Pool_Index (Long_Item, Next_CP_Index);
      Next_CP_Index := CP_Index (CP.Length (Gen_Pool));

      --  Note: For historical reasons the JVM constant pool
      --  definition requires that an extra pool entry be present
      --  following a Long pool entry, so we emit an Empty entry.

      CP.Add (Gen_Pool, CP_Info'(Tag => CONSTANT_Empty));
      Next_CP_Index := CP_Index (CP.Length (Gen_Pool));
   end Emit_CP_Long;

   -------------------
   -- Emit_CP_Float --
   -------------------

   procedure Emit_CP_Float (Float_Item : Pool_Id) is
      Value    : constant Ureal := Pool_Float (Float_Item);
      CP_Float : CP_Info (CONSTANT_Float);
      Num      : Long_Long_Float := 0.0;
      Den      : Long_Long_Float := 0.0;
      Sign     : Long_Long_Float := 1.0;
      Tmp      : Uint;
      Index    : Integer;
   begin
      if UR_Is_Negative (Value) then
         Sign := -1.0;
      end if;

      --  In the following calculus, we consider numbers modulo 2 ** 31,
      --  so that we don't have problems with signed Int...

      Tmp := abs (Numerator (Value));
      Index := 0;
      while Tmp > 0 loop
         Num := Num
           + Long_Long_Float (UI_To_Int (Tmp mod (Uint_2 ** 31)))
           * (2.0 ** Index);
         Tmp := Tmp / Uint_2 ** 31;
         Index := Index + 31;
      end loop;

      Tmp := abs (Denominator (Value));
      if Rbase (Value) /= 0 then
         Tmp := Rbase (Value) ** Tmp;
      end if;

      Index := 0;
      while Tmp > 0 loop
         Den := Den
           + Long_Long_Float (UI_To_Int (Tmp mod (Uint_2 ** 31)))
           * (2.0 ** Index);
         Tmp := Tmp / Uint_2 ** 31;
         Index := Index + 31;
      end loop;

      --  If the denominator denotes a negative power of Rbase,
      --  then multiply by the denominator.

      if Rbase (Value) /= 0 and then Denominator (Value) < 0 then
         CP_Float.Bytes := To_U4 (IEEE_Float_32 (Sign * Num * Den));

      --  Otherwise compute the quotient

      else
         CP_Float.Bytes := To_U4 (IEEE_Float_32 (Sign * Num / Den));
      end if;

      CP.Add (Gen_Pool, CP_Float);
      Set_Pool_Index (Float_Item, Next_CP_Index);
      Next_CP_Index := CP_Index (CP.Length (Gen_Pool));
   end Emit_CP_Float;

   --------------------
   -- Emit_CP_Double --
   --------------------

   procedure Emit_CP_Double (Double_Item : Pool_Id) is
      Value     : constant Ureal := Pool_Double (Double_Item);
      CP_Double : CP_Info (CONSTANT_Double);
      Num       : Long_Long_Float := 0.0;
      Den       : Long_Long_Float := 0.0;
      Sign      : Long_Long_Float := 1.0;
      Tmp       : Uint;
      Index     : Integer;

   begin

      if UR_Is_Negative (Value) then
         Sign := -1.0;
      end if;

      Tmp := abs (Numerator (Value));
      Index := 0;
      while Tmp > 0 loop
         Num := Num
           + Long_Long_Float (UI_To_Int (Tmp mod (Uint_2 ** 31)))
           * (2.0 ** Index);
         Tmp := Tmp / Uint_2 ** 31;
         Index := Index + 31;
      end loop;

      Tmp := abs (Denominator (Value));
      if Rbase (Value) /= 0 then
         Tmp := Rbase (Value) ** Tmp;
      end if;

      Index := 0;
      while Tmp > 0 loop
         Den := Den
           + Long_Long_Float (UI_To_Int (Tmp mod (Uint_2 ** 31)))
           * (2.0 ** Index);
         Tmp := Tmp / Uint_2 ** 31;
         Index := Index + 31;
      end loop;

      --  If the denominator denotes a negative power of Rbase,
      --  then multiply by the denominator.

      if Rbase (Value) /= 0 and then Denominator (Value) < 0 then
         To_U4 (To_U8 (IEEE_Float_64 (Sign * Num * Den)),
                CP_Double.High_Bytes, CP_Double.Low_Bytes);

      --  Otherwise compute the quotient

      else
         To_U4 (To_U8 (IEEE_Float_64 (Sign * Num / Den)),
                CP_Double.High_Bytes, CP_Double.Low_Bytes);
      end if;

      CP.Add (Gen_Pool, CP_Double);
      Set_Pool_Index (Double_Item, Next_CP_Index);
      Next_CP_Index := CP_Index (CP.Length (Gen_Pool));

      --  Note: For historical reasons the JVM constant pool
      --  definition requires that an extra pool entry be present
      --  following a Double pool entry, so we emit an Empty entry.

      CP.Add (Gen_Pool, CP_Info'(Tag => CONSTANT_Empty));
      Next_CP_Index := CP_Index (CP.Length (Gen_Pool));
   end Emit_CP_Double;

   ---------------------
   -- Emit_CP_Typeref --
   ---------------------

   procedure Emit_CP_Typeref (Typeref : Pool_Id) is
   begin
      pragma Assert (Pool_Index (Typeref) = CP_Empty);

      Set_Pool_Index
        (Typeref, CP_Utf8_Index (Type_String (Ref_Type (Typeref))));
   end Emit_CP_Typeref;

   ------------------
   -- Emit_CP_Item --
   ------------------

   procedure Emit_CP_Item (CP_Item : Pool_Id) is
   begin
      if Pool_Index (CP_Item) = CP_Empty then
         case Pool_Item_Tag (CP_Item) is
            when CONSTANT_Empty =>
               pragma Assert (False);
               null;
            when CONSTANT_Utf8 =>
               pragma Assert (False);
               null;
            when CONSTANT_Integer =>
               Emit_CP_Integer (CP_Item);
            when CONSTANT_Float =>
               Emit_CP_Float (CP_Item);
            when CONSTANT_Long =>
               Emit_CP_Long (CP_Item);
            when CONSTANT_Double =>
               Emit_CP_Double (CP_Item);
            when CONSTANT_Class =>
               Emit_CP_Class (CP_Item);
            when CONSTANT_String =>
               Emit_CP_String (CP_Item);
            when CONSTANT_Fieldref =>
               Emit_CP_Fieldref (CP_Item);
            when CONSTANT_Methodref =>
               Emit_CP_Methodref (CP_Item);
            when CONSTANT_Interface_Methodref =>
               Emit_CP_Interface_Methodref (CP_Item);
            when CONSTANT_Name_And_Type =>
               Emit_CP_Typeref (CP_Item);
         end case;
      end if;
   end Emit_CP_Item;

   --------------------
   -- CP_Class_Index --
   --------------------

   function CP_Class_Index (Class : Class_Id) return CP_Index_Class is
      CP_Id : constant Pool_Id := Class_Item (Gen_Class, Class);

   begin
      if Pool_Index (CP_Id) = CP_Empty then
         Emit_CP_Class (CP_Id);
      end if;

      return Pool_Index (CP_Id);
   end CP_Class_Index;

   ----------------------------
   -- CP_Name_And_Type_Index --
   ----------------------------

   function CP_Name_And_Type_Index
     (Field : Field_Id)
       return CP_Index_Name_And_Type
   is
      CP_Name_Type : CP_Info (CONSTANT_Name_And_Type);

   begin
      CP_Name_Type.Name_Index := CP_Utf8_Index (Name (Field));
      CP_Name_Type.Descriptor_Index := CP_Utf8_Index (Type_Of (Field));

      CP.Add (Gen_Pool, CP_Name_Type);
      Next_CP_Index := CP_Index (CP.Length (Gen_Pool));

      return Next_CP_Index - 1;
   end CP_Name_And_Type_Index;

   ----------------------------
   -- CP_Name_And_Type_Index --
   ----------------------------

   function CP_Name_And_Type_Index
     (Method : Method_Id)
        return CP_Index_Name_And_Type
   is
      CP_Name_Type : CP_Info (CONSTANT_Name_And_Type);

   begin
      CP_Name_Type.Name_Index := CP_Utf8_Index (Name (Method));
      CP_Name_Type.Descriptor_Index := CP_Utf8_Index (Method);

      CP.Add (Gen_Pool, CP_Name_Type);
      Next_CP_Index := CP_Index (CP.Length (Gen_Pool));

      return Next_CP_Index - 1;
   end CP_Name_And_Type_Index;

   -------------------
   -- CP_Utf8_Index --
   -------------------

   function CP_Utf8_Index (Str : String) return CP_Index_Utf8 is
      CP_Utf8 : CP_Info (CONSTANT_Utf8);

   begin
      Utf8.Allocate_Expandable_Table (CP_Utf8.Str_Bytes);
      Append (CP_Utf8.Str_Bytes, Str);
      --  Append_String (CP_Utf8.Str_Bytes, Str);
      --  for Index in 0 .. Str'Length - 1 loop
      --     Put (CP_Utf8.Str_Bytes, U2 (Index),
      --          U1'Val (Character'Pos (Str (Index))));
      --  end loop;
      Utf8.Freeze_Expandable_Table (CP_Utf8.Str_Bytes);

      CP.Add (Gen_Pool, CP_Utf8);
      Next_CP_Index := CP_Index (CP.Length (Gen_Pool));

      return Next_CP_Index - 1;
   end CP_Utf8_Index;

   -------------------
   -- CP_Utf8_Index --
   -------------------

   function CP_Utf8_Index (Name : Name_Id) return CP_Index_Utf8 is
   begin
      pragma Assert (Name /= No_Name);

      return CP_Utf8_Index (Name_String (Name));
   end CP_Utf8_Index;

   -------------------
   -- CP_Utf8_Index --
   -------------------

   function CP_Utf8_Index (Typ : Type_Id) return CP_Index_Utf8 is
      CP_Type : constant Pool_Id := Type_Item (Gen_Class, Typ);

   begin
      if Pool_Index (CP_Type) = CP_Empty then
         Emit_CP_Typeref (CP_Type);
      end if;

      return Pool_Index (CP_Type);
   end CP_Utf8_Index;

   -------------------
   -- CP_Utf8_Index --
   -------------------

   function CP_Utf8_Index (Method : Method_Id) return CP_Index_Utf8 is
      First_Param : Local_Var_Id := First_Local_Var (Method);

   begin
      if not Is_Static (Method) then
         First_Param := Next_Local_Var (First_Param);
      end if;

      return CP_Utf8_Index
               ("(" & Params_String (First_Param) & ")"
                    & Type_String (Result_Type (Method)));
   end CP_Utf8_Index;

   --------------------------------------------------------------
   -- Bodies of Primary Class File Generation Support Routines --
   --------------------------------------------------------------

   ----------------
   -- Emit_Field --
   ----------------

   procedure Emit_Field (Table : in out Member.Table; Field : Field_Id) is
      Field_Info : Member_Info;

   begin
      case Access_Mode (Field) is
         when Public_Access =>
            Set (Field_Info.Access_Flags, ACC_Public);
         when Package_Access =>
            null;
         when Protected_Access =>
            Set (Field_Info.Access_Flags, ACC_Protected);
         when Private_Access =>
            Set (Field_Info.Access_Flags, ACC_Private);
      end case;

      if Is_Static (Field) then
         Set (Field_Info.Access_Flags, ACC_Static);
      end if;

      if Is_Final (Field) then
         Set (Field_Info.Access_Flags, ACC_Final);
      end if;

      if Is_Volatile (Field) then
         Set (Field_Info.Access_Flags, ACC_Volatile);
      end if;

      Field_Info.Name_Index := CP_Utf8_Index (Name (Field));
      Field_Info.Descriptor_Index := CP_Utf8_Index (Type_Of (Field));

      --  For now we assume no ConstantValue attribute

      Member_Attribute.Allocate_Fixed_Table (Field_Info.Attributes, 0);

      Member.Add (Table, Field_Info);
   end Emit_Field;

   -----------------
   -- Emit_Method --
   -----------------

   procedure Emit_Method (Table : in out Member.Table; Method : Method_Id) is
      Method_Info : Member_Info;

   begin
      case Access_Mode (Method) is
         when Public_Access =>
            Set (Method_Info.Access_Flags, ACC_Public);
         when Package_Access =>
            null;
         when Protected_Access =>
            Set (Method_Info.Access_Flags, ACC_Protected);
         when Private_Access =>
            Set (Method_Info.Access_Flags, ACC_Private);
      end case;

      if Is_Static (Method) then
         Set (Method_Info.Access_Flags, ACC_Static);
      end if;

      if Is_Final (Method) then
         Set (Method_Info.Access_Flags, ACC_Final);
      end if;

      if Is_Synchronized (Method) then
         Set (Method_Info.Access_Flags, ACC_Synchronized);
      end if;

      if Is_Abstract (Method) then
         Set (Method_Info.Access_Flags, ACC_Abstract);
      end if;

      Method_Info.Name_Index := CP_Utf8_Index (Name (Method));
      Method_Info.Descriptor_Index := CP_Utf8_Index (Method);

      if Is_Abstract (Method) then
         Member_Attribute.Allocate_Fixed_Table (Method_Info.Attributes, 0);

      --  Nonabstract methods have two attributes: one for their associated
      --  bytecode and the other to indicate the set of exceptions that are
      --  thrown by the method (which we currently treat as empty and which
      --  is not enforced by the JVM in any case).

      else
         Member_Attribute.Allocate_Fixed_Table (Method_Info.Attributes, 2);
         Emit_Code (Method_Info.Attributes, Method);

         --  For now we use an empty Exceptions attribute

         declare
            Exc_Info : Member_Attribute_Info (Attr_Exceptions);

         begin
            Exc_Info.Attribute_Name_Index := Excs_Attr_Utf8;
            Class_Index.Allocate_Fixed_Table
              (Exc_Info.Exception_Index_Table, 0);
            Member_Attribute.Put (Method_Info.Attributes, U2 (1), Exc_Info);
         end;
      end if;

      Member.Add (Table, Method_Info);
   end Emit_Method;

   ---------------
   -- Emit_Code --
   ---------------

   procedure Emit_Code
     (Table  : in out Member_Attribute.Table;
      Method : Method_Id)
   is
      Code_Info : Member_Attribute_Info (Attr_Code);
      Method_Seq : Code_Sequence := Method_Code (Method);

   begin
      Code_Info.Attribute_Name_Index := Code_Attr_Utf8;

      Code_Info.Max_Stack  := U2 (Max_Stack_Depth (Method));
      Code_Info.Max_Locals := U2 (Next_Local_Index (Method));

      Code_Array.Allocate_Expandable_Table (Code_Info.Code);
      Generate_Code (Method, Code_Info.Code);
      Code_Array.Freeze_Expandable_Table (Code_Info.Code);

      Handler.Allocate_Expandable_Table (Code_Info.Exception_Table);
      Generate_Handler_Entries (Method, Code_Info.Exception_Table);
      Handler.Freeze_Expandable_Table (Code_Info.Exception_Table);

      --  Set the Line number attribute table and the Local_Variable
      --  table. This has to be done after the Generate_Code function

      Code_Attribute.Allocate_Fixed_Table (Code_Info.Attributes, 2);
      Emit_Line_Numbers (Code_Info.Attributes, Method);
      Emit_Local_Variables (Code_Info, Method);

      Member_Attribute.Put (Table, U2 (0), Code_Info);

      Free_Sequence (Method_Seq);
   end Emit_Code;

   -----------------------
   -- Emit_Line_Numbers --
   -----------------------

   procedure Emit_Line_Numbers
     (Lines  : in out Code_Attribute.Table;
      Method : Method_Id)
   is
      Code         : Code_Attribute_Info (Attr_Line_Number_Table);
      Method_Seq   : constant Code_Sequence := Method_Code (Method);
      Instr        : Instr_Id               := First (Method_Seq);
      PC           : Instruction_Index      := 0;
      JVM_Instr    : JVM.Code.Instruction;
   begin
      Code.Attribute_Name_Index := Line_Attr_Utf8;
      Line.Allocate_Expandable_Table (Code.Line_Number_Table);

      --  Search for every label associated with a line number

      while Instr /= Null_Instr loop

         JVM_Instr := Get (Instr);
         if JVM_Instr.Op = Nop
           and then JVM_Instr.Line_Number /= No_Location
         then
            Line.Add
              (Code.Line_Number_Table,
               Line_Info'(Start_PC => PC,
                          Line_Number =>
                            U2 (Sinput.Get_Logical_Line_Number
                                         (JVM_Instr.Line_Number))));
         end if;

         if JVM_Instr.Op /= Nop or else JVM_Instr.Label_Def = Null_Label then
            PC := PC + Instruction_Index (Instr_Size (Get (Instr), PC));
         end if;

         Instr := JVM_Instr.Next;
      end loop;

      Line.Freeze_Expandable_Table (Code.Line_Number_Table);

      Code_Attribute.Put (Lines, U2 (0), Code);
   end Emit_Line_Numbers;

   --------------------------
   -- Emit_Local_Variables --
   --------------------------

   procedure Emit_Local_Variables
     (Code   : in out Member_Attribute_Info;
      Method : Method_Id)
   is
      Attr      : Code_Attribute_Info (Attr_Local_Variable_Table);
      Local     : Local_Var_Id := First_Local_Var (Method);
      Var       : Variable_Info;

   begin
      Attr.Attribute_Name_Index := Local_Variable_Attr_Utf8;
      Variable.Allocate_Expandable_Table (Attr.Local_Variable_Table);

      --  First, Create the variable with dummy Start_PC and Length

      while Local /= Null_Local_Var loop
         Var.Start_PC         := 0;
         Var.Length           := U2 (Code_Array.Length (Code.Code));
         Var.Name_Index       := CP_Utf8_Index (Name (Local));
         Var.Descriptor_Index := CP_Utf8_Index (Type_Of (Local));
         Var.Index            := Local_Index (Local);

         Variable.Add (Attr.Local_Variable_Table, Var);

         Local := Next_Local_Var (Local);
      end loop;

      Variable.Freeze_Expandable_Table (Attr.Local_Variable_Table);
      Code_Attribute.Put (Code.Attributes, U2 (1), Attr);
   end Emit_Local_Variables;

   -------------------
   -- Generate_Code --
   -------------------

   procedure Generate_Code
     (Method : Method_Id;
      Code   : in out Code_Array.Table)
   is
      Method_Seq   : Code_Sequence     := Method_Code (Method);
      Instr        : Instr_Id          := First (Method_Seq);
      J_Code_Index : Instruction_Index := 0;
      Subroutine   : Subroutine_Id     := First_Subroutine (Method);
      Subr_Seq     : Code_Sequence;
      Delta_Size   : Instruction_Index := 0;

   begin
      --  First determine the instruction offsets for branches and labels
      --  in the method proper.

      while Instr /= Null_Instr loop
         Compute_Instruction_Offset (Instr, Code_Index => J_Code_Index);
         Instr := Get (Instr).Next;
      end loop;

      --  Now determine the instruction offsets for branches and labels
      --  in any subroutines following the main method.

      while Subroutine /= Null_Subroutine loop
         pragma Assert (not Is_Open (Subroutine));

         Subr_Seq := Subroutine_Code (Subroutine);

         Instr := First (Subr_Seq);
         while Instr /= Null_Instr loop
            Compute_Instruction_Offset (Instr, Code_Index => J_Code_Index);
            Instr := Get (Instr).Next;
         end loop;

         --  Concatenate the subroutine's instructions with the method's
         --  code sequence so we don't have to separately traverse the
         --  subroutine's code sequences in later loops in this procedure.

         Attach (Method_Seq, Subr_Seq);

         Subroutine := Next_Subroutine (Subroutine);
      end loop;

      --  If the size of the code for the method is large enough that
      --  some branches may need greater than 16-bit offsets, then we
      --  make a second pass through the instructions to locate branches
      --  that need to converted to their long form (32-bit offsets).
      --  (NOTE: The current version of the JVM restricts code size
      --  to 32K per method, but it's still possible to have branches
      --  that require offsets in the range 16K-32K.)

      if J_Code_Index > 2**15 - 1 then

         --  Convert branches to use 32-bit offsets when a short branch
         --  is insufficient. Adjust label and branch instruction offsets
         --  as needed. At the end of the loop, Delta_Size will reflect the
         --  aggregate increase due to converting branches to long form.

         Delta_Size := 0;

         Instr := First (Method_Seq);
         while Instr /= Null_Instr loop
            Check_Branch_Size (Instr, Code_Increment => Delta_Size);
            Instr := Get (Instr).Next;
         end loop;

         J_Code_Index := J_Code_Index + Delta_Size;
      end if;

      --  Finally, generate the JVM byte code for all instructions and
      --  resolve the target offsets of branches.

      Instr := First (Method_Seq);
      while Instr /= Null_Instr loop
         Generate_Instruction (Code, Instr);
         Instr := Get (Instr).Next;
      end loop;

      --  The sequence may have been modified, so we have to put
      --  it back in the method

      Set_Code (Method, Method_Seq);

   end Generate_Code;

   ------------------------------
   -- Generate_Handler_Entries --
   ------------------------------

   procedure Generate_Handler_Entries
     (Method   : Method_Id;
      Handlers : in out Handler.Table)
   is
      Handler_Seq : Handler_Sequence := Method_Handlers (Method);
      Hdlr_Id     : Handler_Id       := First (Handler_Seq);
      Hdlr_Entry  : Handler_Entry;
      Hdlr_Info   : Handler_Info;
      Exc_Index   : CP_Index;

   begin
      while Hdlr_Id /= Null_Handler loop
         Hdlr_Entry := Get (Hdlr_Id);

         if Hdlr_Entry.Exc_Class = Null_Pool_Item then
            Exc_Index := 0;
         else
            Exc_Index := Pool_Index (Hdlr_Entry.Exc_Class);
         end if;

         Hdlr_Info := (Start_PC   => Code_Index (Hdlr_Entry.Start_Lbl),
                       End_PC     => Code_Index (Hdlr_Entry.End_Lbl),
                       Handler_PC => Code_Index (Hdlr_Entry.Handler_Lbl),
                       Catch_Type => Exc_Index);

         Handler.Add (Handlers, Hdlr_Info);

         Hdlr_Id := Hdlr_Entry.Next;
      end loop;

      Free_Sequence (Handler_Seq);
   end Generate_Handler_Entries;

   ----------------
   -- Instr_Size --
   ----------------

   function Instr_Size
     (Instr : JVM.Code.Instruction;
      Index : Instruction_Index)
     return   U4
   is
      I_Size : U4 := Size (Instr.Op, Wide => False);

   begin
      case Instr.Op is
         when Nop =>

            --  If this is a special "label" Nop, then the size is zero

            if Instr.Label_Def /= Null_Label then
               I_Size := 0;
            end if;

         when Iload  | Lload  | Fload  | Dload  | Aload  |
              Istore | Lstore | Fstore | Dstore | Astore =>

            if Local_Index (Instr.Local) in Short_Local_Index then
               I_Size := 1;  --  One-byte short form of instruction
            elsif Local_Index (Instr.Local) > 255 then
               I_Size := Size (Instr.Op, Wide => True);
            end if;

         when Ret =>

            if Local_Index (Instr.Local) > 255 then
               I_Size := Size (Instr.Op, Wide => True);
            end if;

         when Iinc =>

            if Local_Index (Instr.Inc_Local) > 255
              or else Instr.Increment not in -128 .. 127
            then
               I_Size := Size (Instr.Op, Wide => True);
            end if;

         when Tableswitch =>

            pragma Assert (I_Size = 0);

            if Switch_Pair_Count (Instr.Switch_Pairs) = 0 then
               I_Size
                 := Size (Tableswitch, Index, 0);

            else
               I_Size
                 := Size (Tableswitch, Index,
                      U4 (Match_Value (Last_Pair  (Instr.Switch_Pairs))
                        - Match_Value (First_Pair (Instr.Switch_Pairs)) + 1));
            end if;

         when Lookupswitch =>

            --  (TBD)
            pragma Assert (False);
            null;

         when others =>

            null;

      end case;

      return I_Size;
   end Instr_Size;

   --------------------------------
   -- Compute_Instruction_Offset --
   --------------------------------

   procedure Compute_Instruction_Offset
     (Instr      : Instr_Id;
      Code_Index : in out Instruction_Index)
   is
      JVM_Instr : JVM.Code.Instruction := Get (Instr);
      I_Size    : constant U4          := Instr_Size (JVM_Instr, Code_Index);

   begin
      case JVM_Instr.Op is
         when Nop =>

            --  If this is a special "label" Nop, then set
            --  the associated label's code index.

            if JVM_Instr.Label_Def /= Null_Label then
               Set_Code_Index (JVM_Instr.Label_Def, Code_Index);
            end if;

         when Ifeq      | Ifne      | Iflt      | Ifge      | Ifgt | Ifle |
              If_Icmpeq | If_Icmpne | If_Icmplt | If_Icmpge |
              If_Icmpgt | If_Icmple | If_Acmpeq | If_Acmpne |
              Ifnull    | Ifnonnull | Jump      | Goto_W    |
              Jsr       | Jsr_W     =>

            --  Branch resolution must be deferred; record the current
            --  instruction offset in the branch instruction itself.

            JVM_Instr.Offset := Code_Index;
            Put (Instr, JVM_Instr);

         when Tableswitch =>

            JVM_Instr.Switch_Offset := Code_Index;
            Put (Instr, JVM_Instr);

         when Lookupswitch =>

            --  (TBD)
            pragma Assert (False);
            null;

         when others =>

            null;

      end case;

      --  If this is a non-label instruction, then increment Code_Index
      --  by the size of the instruction.

      if JVM_Instr.Op /= Nop or else JVM_Instr.Label_Def = Null_Label then
         Code_Index := Code_Index + Instruction_Index (I_Size);
      end if;
   end Compute_Instruction_Offset;

   -----------------------
   -- Check_Branch_Size --
   -----------------------

   procedure Check_Branch_Size
     (Instr          : Instr_Id;
      Code_Increment : in out Instruction_Index)
   is
      JVM_Instr  : JVM.Code.Instruction := Get (Instr);

      procedure Make_Branch_Long
        (Branch_Instr   : in out JVM.Code.Instruction;
         Code_Increment : in out Instruction_Index);
      --  Converts a branch instruction with a 16-bit range to an
      --  equivalent long instruction (or instruction sequence)
      --  with a 32-bit range.

      procedure Make_Branch_Long
        (Branch_Instr   : in out JVM.Code.Instruction;
         Code_Increment : in out Instruction_Index)
      is

      begin
         case Branch_Instr.Op is
            when Jump =>

               Code_Increment
                 := Code_Increment
                      + Instruction_Index (Size (Goto_W) - Size (Jump));
               Branch_Instr := (Goto_W,
                                Branch_Instr.Next,
                                Branch_Instr.Target,
                                Branch_Instr.Offset);

            when Jsr =>

               Code_Increment
                 := Code_Increment
                      + Instruction_Index (Size (Jsr_W) - Size (Jsr));
               Branch_Instr := (Jsr_W,
                                Branch_Instr.Next,
                                Branch_Instr.Target,
                                Branch_Instr.Offset);

            when Ifeq      | Ifne      | Iflt      | Ifge      | Ifgt | Ifle |
                 If_Icmpeq | If_Icmpne | If_Icmplt | If_Icmpge |
                 If_Icmpgt | If_Icmple | If_Acmpeq | If_Acmpne |
                 Ifnull    | Ifnonnull =>

               declare
                  --  Table to map conditional branches into their inverses

                  Invert : constant array (Ifeq .. Ifnonnull) of Operation
                    := (Ifeq      => Ifne,      Ifne      => Ifeq,
                        Iflt      => Ifge,      Ifge      => Iflt,
                        Ifgt      => Ifle,      Ifle      => Ifgt,
                        If_Icmpeq => If_Icmpne, If_Icmpne => If_Icmpeq,
                        If_Icmplt => If_Icmpge, If_Icmpge => If_Icmplt,
                        If_Icmpgt => If_Icmple, If_Icmple => If_Icmpgt,
                        If_Acmpeq => If_Acmpne, If_Acmpne => If_Acmpeq,
                        Ifnull    => Ifnonnull, Ifnonnull => Ifnull,
                        others    => Xxxunusedxxx);

                  Inverse_Op : JVM.Code.Instruction (Invert (Branch_Instr.Op));
                  Long_Jump  : JVM.Code.Instruction (Goto_W);
                  Inv_Label  : JVM.Code.Instruction (Nop);
                  Jump_Instr : constant Instr_Id := New_Instr;
                  Lbl_Instr  : constant Instr_Id := New_Instr;
                  Lbl_Id     : constant Label_Id := JVM.Info.New_Label;

               begin
                  --  Create a conditional branch with opposite sense that
                  --  targets the new label that will follow the long jump.

                  Inverse_Op.Next   := Branch_Instr.Next;
                  Inverse_Op.Target := Lbl_Id;
                  Inverse_Op.Offset := Branch_Instr.Offset;
                  JVM_Instr         := Inverse_Op;

                  --  Target the new long jump instruction at the label
                  --  of the old conditional branch instruction.

                  Long_Jump.Target := Branch_Instr.Target;
                  Long_Jump.Offset
                    := Branch_Instr.Offset
                         + Instruction_Index (Size (Inverse_Op.Op));
                  Put (Jump_Instr, Long_Jump);
                  Insert (Jump_Instr, After => Instr);

                  --  Initialize the label instruction targeted by the
                  --  inverted branch.

                  Inv_Label.Label_Def := Lbl_Id;
                  Put (Lbl_Instr, Inv_Label);
                  Insert (Lbl_Instr, After => Jump_Instr);

                  --  Set the appropriate fields of the new label

                  Set_Location (Lbl_Id, Lbl_Instr);
                  Set_Is_Targeted (Lbl_Id);
                  Set_Code_Index (Lbl_Id, Long_Jump.Offset
                                    + Instruction_Index (Size (Long_Jump.Op)));

                  --  Increment by the length of the inserted jump instruction

                  Code_Increment := Code_Increment
                    + Instruction_Index (Size (Long_Jump.Op));
               end;

            when others =>
               pragma Assert (False);
               null;
         end case;
      end Make_Branch_Long;

   --  Start of processing for Check_Branch_Offset

   begin
      case JVM_Instr.Op is
         when Nop =>

            --  If this is a "label" Nop, then increment the associated
            --  label's code index by the current net size increase.

            if JVM_Instr.Label_Def /= Null_Label then
               Set_Code_Index
                 (JVM_Instr.Label_Def,
                  Code_Index (JVM_Instr.Label_Def) + Code_Increment);
            end if;

         when Ifeq      | Ifne      | Iflt      | Ifge      | Ifgt | Ifle |
              If_Icmpeq | If_Icmpne | If_Icmplt | If_Icmpge |
              If_Icmpgt | If_Icmple | If_Acmpeq | If_Acmpne |
              Ifnull    | Ifnonnull | Jump      | Jsr =>

            --  Increment the branch instruction's offset field by
            --  the current code increment to reflect the effect of
            --  earlier branch instructions that have been made long.

            JVM_Instr.Offset := JVM_Instr.Offset + Code_Increment;

            declare
               Label_Loc : constant Instruction_Index
                 := Code_Index (JVM_Instr.Target);
               Distance  : Jump_Offset_U4;

            begin
               Distance := Jump_Offset_U4 (Label_Loc - JVM_Instr.Offset);

               --  If this is a backward branch that exceeds the lower
               --  bound of the 16-bit jump range or a forward branch
               --  that may possibly exceed the upper bound (heuristically
               --  adjusted by a safety factor since we don't know the exact
               --  distance at this point due to the possibility of later
               --  branch increases), then convert the instruction to a
               --  long branch or the appropriate multiple instruction
               --  sequence.

               if Distance not in -(2**15) .. (2**15 - 5000) then
                  Make_Branch_Long (JVM_Instr, Code_Increment);
               end if;
            end;

            Put (Instr, JVM_Instr);

         when Goto_W | Jsr_W =>

            --  If the branch is already wide then leave it as is,
            --  but increment its instruction offset.

            JVM_Instr.Offset := JVM_Instr.Offset + Code_Increment;

         when Tableswitch | Lookupswitch =>

            --  Nothing to do, since branch offsets are 32 bits

            null;

         when others =>

            null;

      end case;
   end Check_Branch_Size;

   --------------------------
   -- Generate_Instruction --
   --------------------------

   procedure Generate_Instruction
     (Code  : in out Code_Array.Table;
      Instr : Instr_Id)
   is
      JVM_Instr  : constant JVM.Code.Instruction := Get (Instr);
      File_Instr : JVM_File.Instruction (JVM_Instr.Op, 0);
      Short_Op   : Operation;

   begin
      case JVM_Instr.Op is
         when Nop =>

            null;

         when Bipush =>

            File_Instr.Byte := Int_8 (JVM_Instr.Sint);

         when Sipush =>

            File_Instr.Short := JVM_Instr.Sint;

         when Newarray =>

            File_Instr.Atype := JVM_Instr.Element_Type;

         when Iload  | Lload  | Fload  | Dload  | Aload  |
              Istore | Lstore | Fstore | Dstore | Astore =>

            File_Instr.Local_Var := Local_Index (JVM_Instr.Local);
            Set_Wide_Format_If_Needed (File_Instr);

            --  Use short form of load/store for variables at small indexes

            if File_Instr.Local_Var in Short_Local_Index then
               if JVM_Instr.Op in Load_Ops then
                  Short_Op
                    := Short_Load_Ops (JVM_Instr.Op, File_Instr.Local_Var);

               elsif JVM_Instr.Op in Store_Ops then
                  Short_Op
                    := Short_Store_Ops (JVM_Instr.Op, File_Instr.Local_Var);

               else
                  pragma Assert (False);
                  raise Program_Error;
               end if;

               declare
                  Short_Instr : JVM_File.Instruction (Short_Op, 0);
               begin
                  Add (Code, Short_Instr);
               end;

               return;
            end if;

         when Ret =>

            File_Instr.Local_Var := Local_Index (JVM_Instr.Local);
            Set_Wide_Format_If_Needed (File_Instr);

         when Iinc =>

            File_Instr.Var_Iinc := Local_Index (JVM_Instr.Inc_Local);
            File_Instr.Const_Val := JVM_Instr.Increment;
            Set_Wide_Format_If_Needed (File_Instr);

         when Ifeq      | Ifne      | Iflt      | Ifge      | Ifgt | Ifle |
              If_Icmpeq | If_Icmpne | If_Icmplt | If_Icmpge |
              If_Icmpgt | If_Icmple | If_Acmpeq | If_Acmpne |
              Ifnull    | Ifnonnull | Jump      | Goto_W    |
              Jsr       | Jsr_W     =>

            --  The byte offset of the target label for a branch
            --  instruction is now known, so compute the relative
            --  offset from the instruction to the label.

            declare
               Label_Loc : constant Instruction_Index
                 := Code_Index (JVM_Instr.Target);

            begin
               if JVM_Instr.Op = Goto_W or else JVM_Instr.Op = Jsr_W then
                  File_Instr.Offset_U4
                    := Jump_Offset_U4 (Label_Loc - JVM_Instr.Offset);
               else
                  File_Instr.Offset
                    := Jump_Offset_U2 (Label_Loc - JVM_Instr.Offset);
               end if;
            end;

         when Ldc =>

            File_Instr.CP_Const_U1 := Pool_Index (JVM_Instr.Pool_Item);

            pragma Assert (File_Instr.CP_Const_U1 in 1 .. 255);

         when Ldc_W =>

            File_Instr.CP_Const_U2 := Pool_Index (JVM_Instr.Pool_Item);

         when Ldc2_W =>

            File_Instr.CP_Const := Pool_Index (JVM_Instr.Pool_Item);

         when Getstatic | Putstatic | Getfield  | Putfield =>

            File_Instr.Field := Pool_Index (JVM_Instr.Pool_Item);

         when Invokestatic | Invokespecial | Invokevirtual =>

            File_Instr.Method := Pool_Index (JVM_Instr.Pool_Item);

         when Invokeinterface =>

            File_Instr.I_Method := Pool_Index (JVM_Instr.Pool_Item);

            --  Set the Nargs attribute to the number of words required
            --  by the method's parameters.

            declare
               Method : constant Method_Id := Ref_Method (JVM_Instr.Pool_Item);
               LV     : Local_Var_Id       := First_Local_Var (Method);
               Words  : U1 := 0;

            begin
               while LV /= Null_Local_Var and then Is_Param (LV) loop
                  Words := Words + U1 (Word_Size (Variable_Type (LV)));
                  LV := Next_Local_Var (LV);
               end loop;

               File_Instr.Nargs := Words;
            end;

            File_Instr.Unused := 0;

         when Newobject | Anewarray | Checkcast | Instanceof =>

            File_Instr.Class := Pool_Index (JVM_Instr.Pool_Item);

         when Tableswitch =>

            declare
               Low_Match  : Int_32 := 0;
               High_Match : Int_32 := -1;

            begin
               if Switch_Pair_Count (JVM_Instr.Switch_Pairs) > 0 then
                  Low_Match
                    := Match_Value (First_Pair (JVM_Instr.Switch_Pairs));
                  High_Match
                    := Match_Value (Last_Pair (JVM_Instr.Switch_Pairs));
               end if;

               declare
                  T_Instr : JVM_File.Instruction
                              (Tableswitch, U4 (High_Match - Low_Match + 1));
                  T_Pair  : Switch_Pair_Id
                    := First_Pair (JVM_Instr.Switch_Pairs);
                  T_Delta : Int_32;

               begin
                  T_Instr.Table_Default
                    := Jump_Offset_U4 (Code_Index (JVM_Instr.Default_Label)
                                              - JVM_Instr.Switch_Offset);
                  T_Instr.Low  := Low_Match;
                  T_Instr.High := High_Match;
                  T_Delta := T_Instr.Low - Int_32 (T_Instr.Jump_Table'First);

                  T_Instr.Jump_Table := (others => T_Instr.Table_Default);

                  while T_Pair /= Null_Switch_Pair loop
                     T_Instr.Jump_Table (U4 (Match_Value (T_Pair) - T_Delta))
                       := Jump_Offset_U4 (Code_Index (Match_Label (T_Pair))
                                           - JVM_Instr.Switch_Offset);

                     T_Pair := Next_Pair (T_Pair);
                  end loop;

                  Add (Code, T_Instr);
               end;
            end;

            return;

         when Lookupswitch =>

            --  (TBD)
            pragma Assert (False);
            null;

         when Multianewarray =>

            File_Instr.Class := Pool_Index (JVM_Instr.Array_Class);
            File_Instr.Dimensions := U1 (JVM_Instr.Dimensions);

         when others =>

            null;

      end case;

      --  Add the JVM instruction to the code array, unless this is
      --  a Nop that represents a label.

      if JVM_Instr.Op /= Nop or else JVM_Instr.Label_Def = Null_Label then
         Add (Code, File_Instr);
      end if;

   end Generate_Instruction;

   ------------------------
   -- Produce_Class_File --
   ------------------------

   procedure Produce_Class_File (Class : Class_Id) is
      C_File : Class_File;

   begin
      JVM_File.Set_Water_Mark;

      Gen_Class := Class;

      --  Step 1: Generate the class file's constant pool

      CP.Allocate_Expandable_Table (Gen_Pool);

      --  The first element of the pool is required to have tag CONSTANT_Empty

      CP.Add (Gen_Pool, CP_Info'(Tag => CONSTANT_Empty));
      Next_CP_Index := CP_Index (CP.Length (Gen_Pool));

      --  Traverse the class's list of pool items, establishing
      --  constant pool indexes for all items and emitting their
      --  associated CP_Info records in Gen_Pool. Note that further
      --  constant pool entries for the class are emitted later
      --  below (e.g., for the class's own fields and methods).

      declare
         CP_Item : Pool_Id := First_Pool_Item (Class);

      begin
         --  For now we just assign CP indexes and emit pool entries
         --  sequentially, without regard for putting frequently
         --  referenced items at lower offsets.

         while CP_Item /= Null_Pool_Item loop
            --  If the item already has an associated index then
            --  its constant pool entry has already been emitted.
            --  This can occur due to references from one constant
            --  pool item to another and internal calls to emit
            --  CP_Info when such referenced items are encountered.

            if Pool_Index (CP_Item) = CP_Empty then
               Emit_CP_Item (CP_Item);
            end if;

            CP_Item := Next_Pool_Item (CP_Item);
         end loop;
      end;

      --  Step 2: Set the class's access flags

      if Is_Public (Class) then
         Set (C_File.Access_Flags, ACC_Public);
      end if;

      if Is_Final (Class) then
         Set (C_File.Access_Flags, ACC_Final);
      end if;

      if Is_Interface (Class) then
         Set (C_File.Access_Flags, ACC_Interface);
      end if;

      if Is_Abstract (Class) then
         Set (C_File.Access_Flags, ACC_Abstract);
      end if;

      --  Step 3: Set This_Class

      C_File.This_Class := CP_Class_Index (Class);

      --  Step 4: Set Super_Class

      C_File.Super_Class := CP_Class_Index (Superclass (Class));

      --  Step 5: Establish references to all implemented interfaces

      declare
         Interface_Ref : JVM_Entity_Ref := First_Interface_Ref (Class);

      begin
         Class_Index.Allocate_Expandable_Table (C_File.Interfaces);
         while Interface_Ref /= Null_Entity_Ref loop
            --  Add a reference to an interface implemented by this class
            Class_Index.Add
              (C_File.Interfaces,
               CP_Class_Index (Get_Interface (Interface_Ref)));

            Interface_Ref := Next_Interface_Ref (Interface_Ref);
         end loop;
         Class_Index.Freeze_Expandable_Table (C_File.Interfaces);
      end;

      --  Step 6: Emit field info

      declare
         Field : Field_Id := First_Field (Class);

      begin
         Member.Allocate_Expandable_Table (C_File.Fields);

         while Field /= Null_Field loop
            Emit_Field (C_File.Fields, Field);
            Field := Next_Field (Field);
         end loop;

         Member.Freeze_Expandable_Table (C_File.Fields);
      end;

      --  Step 7: Emit method info

      declare
         Method : Method_Id := First_Method (Class);

      begin
         Code_Attr_Utf8 := CP_Utf8_Index (Attribute_Name (Attr_Code));
         Excs_Attr_Utf8 := CP_Utf8_Index (Attribute_Name (Attr_Exceptions));
         Line_Attr_Utf8 := CP_Utf8_Index (Attribute_Name
                                          (Attr_Line_Number_Table));
         Local_Variable_Attr_Utf8 := CP_Utf8_Index
           (Attribute_Name (Attr_Local_Variable_Table));
         Member.Allocate_Expandable_Table (C_File.Methods);

         while Method /= Null_Method loop
            --  Takes care of emitting code and exception tables
            --  for each method.

            Emit_Method (C_File.Methods, Method);
            Method := Next_Method (Method);
         end loop;

         Member.Freeze_Expandable_Table (C_File.Methods);
      end;

      --  Step 8: Emit attributes (for now just the SourceFile attribute,
      --          but eventually we need to emit inner class info... ???)

      declare
         Source_Attr : Class_Attribute_Info (Attr_Source_File);

      begin
         if Src_Name (Class) = No_Name then
            Class_Attribute.Allocate_Fixed_Table (C_File.Attributes, 0);

         else
            Class_Attribute.Allocate_Fixed_Table (C_File.Attributes, 1);

            Source_Attr.Attribute_Name_Index
              := CP_Utf8_Index (Attribute_Name (Attr_Source_File));
            Source_Attr.Attribute_Length  := 2;
            Source_Attr.Source_File_Index := CP_Utf8_Index (Src_Name (Class));

            Class_Attribute.Put (C_File.Attributes, U2 (0), Source_Attr);
         end if;
      end;

      --  Everything has now been generated, so we can safely freeze the
      --  the pool's table and assign the constant pool and its size.

      CP.Freeze_Expandable_Table (Gen_Pool);
      C_File.Constant_Pool := Gen_Pool;

      --  Let JVM_File compute all class file table and attribute lengths and
      --  produce the byte stream representation for the class file. Then write
      --  out the actual ".class" file.

      Compute_Attribute_Lengths (C_File);

      declare
         CF_Name   : constant String :=
                       Full_Class_Name (Class, True) & ".class";
         CF_Stream : Stream_Of_U1 (1 .. Compute_Size (C_File));

      begin
         JVM_File.Write (C_File, CF_Stream);
         J_Basics.Put_Stream_Of_U1 (CF_Stream, CF_Name);
      end;

      --  Free any J-tables associated with the class file and
      --  enable generation of the next class.

      JVM_File.Free_To_Next_Water_Mark;

      --  Now generate all nested classes.
      declare
         Nested : Class_Id := First_Nested_Class (Class);
      begin
         while Nested /= Null_Class loop
            Produce_Class_File (Nested);
            Nested := Next_Nested_Class (Nested);
         end loop;
      end;
   end Produce_Class_File;

end JVM.Emit;
