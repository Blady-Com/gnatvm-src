------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  J V M                                   --
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

with Atree;     use Atree;
with J_String;  use J_String;
with JVM.API;   use JVM.API;
with JVM.Code;  use JVM.Code;
with JVM.Dbg;   use JVM.Dbg;
with JVM.Emit;
with JVM.Emit.CIL;
with JVM.Info;  use JVM.Info;
with JVM.Map;   use JVM.Map;
with JVM.Pool;  use JVM.Pool;
with JVM.Stack; use JVM.Stack;
with Jx_Decl;   use Jx_Decl;
with Einfo;     use Einfo;
with Errout;    use Errout;
with Sinfo;
with Snames;
with Stand;     use Stand;
with Stringt;   use Stringt;

package body JVM is

   Serial_Number : Positive := 1;
   --  Used for temp variables that are named with strings to make
   --  them distinct (needed for Linux Mono)

   Active_Method : Method_Id := Null_Method;
   --  Denotes the method for which code is currently being generated
   --  (also referred to as the 'current method').

   Active_Class  : Class_Id := Null_Class;
   --  Denotes the class associated with Active_Method (also referred to
   --  as the 'current method'). Any constant pool reference items generated
   --  for code in Active_Method will be associated with this class.

   Active_Subr   : Subroutine_Id := Null_Subroutine;
   --  Denotes a subroutine associated with the Active_Method which is
   --  currently having its code generated. There can be at most one
   --  active subroutine.

   Active_Seq    : Code_Sequence;
   --  The code sequence associated with the currently active method
   --  (or active subroutine when Active_Subr /= Null_Subroutine).

   Saved_Seq     : Code_Sequence;
   --  Temporary variable to hold on to the current method's main
   --  code sequence while an entry sequence for the method is
   --  being generated.

   Active_Handlers : Handler_Sequence;
   --  The exception handler entry sequence associated with the currently
   --  active method (or active subroutine if Active_Subr /= Null_Subroutine).

   Active_Stack  : Op_Stack_Id := Null_Op_Stack;
   --  The operand type stack associated with the currently active method
   --  (or active subroutine when Active_Subr /= Null_Subroutine).

   Active_Switch_Stmt : JVM.Code.Instruction;
   --  Represents a switch statement actively being generated for the
   --  current method, if any. If not active, then the Op discriminant
   --  must have the value Xxxunusedxxx.

   Aux_Stack : Op_Stack_Id := Null_Op_Stack;
   --  Auxiliary stack used to check the type of invocation arguments

   Entry_Seq_Active : Boolean := False;
   --  State variable indicating whether an entry code sequence is
   --  currently being generated for the active method.

   Stack_Checking : Boolean := True;
   --  State variable indicating whether checks should be performed
   --  on branch and label generation to ensure that the current method's
   --  evaluation stack is empty. Stack checking can be turned off by
   --  calls to Set_Stack_Checking (Enable => False).

   Stack_Marked : Boolean := False;
   --  State variable indicating whether the current method's operand
   --  stack has been marked via a call to Mark_Stack.

   Debug : Boolean := False;
   --  When true, enables debugging output from routines in this package.

   Valuetype_Address_Only : Boolean := False;
   --  Should we generate address of valuetype instead of value?

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Convert_To_Integer (Rounded : Boolean);
   --  Generates a conversion of the top (integer) stack item to Int

   procedure Convert_To_UInteger (Rounded : Boolean);
   --  Generates a conversion of the top (integer) stack item to UInt

   procedure Convert_To_Long (Rounded : Boolean);
   --  Generates a conversion of the top (integer) stack item to Long

   procedure Convert_To_ULong (Rounded : Boolean);
   --  Generates a conversion of the top (integer) stack item to ULong

   procedure Convert_To_Float;
   --  Generates a conversion of the top (floating-pt) stack item to Flt

   procedure Convert_To_Double;
   --  Generates a conversion of the top (floating-pt) stack item to Double

   procedure Gen_Zero (Jtype : JVM_Type_Kind);
   --  Generate comparison vs zero
   --  Used by Gen_Branch*

   function Generate_Overflow_Check
     (Modular        : Boolean;
      Integer_Type   : Boolean;
      Overflow_Check : Boolean) return Boolean;
   --  Returns True if we must generate an arithmetic instruction with overflow
   --  checks enabled (Add_Ovf, Sub_Ovf or Mul_Ovf)

   function Generate_String (Number : Type_Id) return String;
   --  Convert a Type_Id to a number and return string with no leading space

   function Generate_Serial_String return String;
   --  Comment needed???

   procedure Check_Arguments_Type (Method : Method_Id);
   --  Check that types of arguments on the stack match the type of the
   --  corresponding formal.

   function Is_Ancestor (Class : Class_Id; Target : Class_Id) return Boolean;
   --  Return True if Class is an ancestor of Target

   procedure Round_Float;
   --  Rounds the top (float) stack item for integer conversion

   -----------------------------
   -- Generate_Overflow_Check --
   -----------------------------

   function Generate_Overflow_Check
     (Modular        : Boolean;
      Integer_Type   : Boolean;
      Overflow_Check : Boolean) return Boolean is
   begin
      return not Modular
        and then Integer_Type
        and then Overflow_Check;
   end Generate_Overflow_Check;

   ----------------------------
   -- Generate_Serial_String --
   ----------------------------

   function Generate_Serial_String return String is
      Result : String := Integer'Image (Serial_Number);
   begin
      Serial_Number := Serial_Number + 1;

      --  Remove leading space
      Result (Result'First) := '_';

      return Result;
   end Generate_Serial_String;

   ---------------------
   -- Generate_String --
   ---------------------

   function Generate_String (Number : Type_Id) return String is
      Result : String := Integer'Image (Integer (Number));
   begin
      --  Remove leading space
      Result (Result'First) := '_';

      return Result;
   end Generate_String;

   --------------------------------
   -- For_Valuetypes_Use_Address --
   --------------------------------

   procedure For_Valuetypes_Use_Address is
   begin
      Valuetype_Address_Only := True;
   end For_Valuetypes_Use_Address;

   ------------------------------
   -- For_Valuetypes_Use_Value --
   ------------------------------

   procedure For_Valuetypes_Use_Value is
   begin
      Valuetype_Address_Only := False;
   end For_Valuetypes_Use_Value;

   ------------------------------
   -- Inside_Try_Catch_Finally --
   ------------------------------

   function Inside_Try_Catch_Finally return Boolean is
   begin
      return JVM.Info.Is_Exception_Block (Active_Method);
   end Inside_Try_Catch_Finally;

   procedure Inside_Try_Catch_Finally is
   begin
      JVM.Info.Set_Exception_Block (Active_Method, True);
   end Inside_Try_Catch_Finally;

   -------------------------------
   -- Outside_Try_Catch_Finally --
   -------------------------------

   procedure Outside_Try_Catch_Finally is
   begin
      JVM.Info.Set_Exception_Block (Active_Method, False);
   end Outside_Try_Catch_Finally;

   ---------------
   -- Set_Class --
   ---------------

   procedure Set_Class (Method : Method_Id; Class : Class_Id) is
   begin
      JVM.Info.Set_Class (Method, Class);
   end Set_Class;

   ---------------
   -- Set_Trace --
   ---------------

   procedure Set_Trace (Trace : Boolean) is
   begin
      Debug := Trace;
   end Set_Trace;

   -------------
   -- Addr_Of --
   -------------

   function Addr_Of (Typ : Type_Id) return Type_Id is
   begin
      if Typ = Boolean_Type then
         return Bool_Addrof_Type;
      elsif Typ = Byte_Type then
         return Uint8_Addrof_Type;
      elsif Typ = SByte_Type then
         return Int8_Addrof_Type;
      elsif Typ = Char_Type then
         return Char_Addrof_Type;
      elsif Typ = Short_Type then
         return Int16_Addrof_Type;
      elsif Typ = Int_Type then
         return Int32_Addrof_Type;
      elsif Typ = UInt_Type then
         return Uint32_Addrof_Type;
      elsif Typ = Long_Type then
         return Int64_Addrof_Type;
      elsif Typ = ULong_Type then
         return Uint64_Addrof_Type;
      elsif Typ = Float_Type then
         return Float32_Addrof_Type;
      elsif Typ = Double_Type then
         return Float64_Addrof_Type;
      else
         pragma Assert (False);
         raise Program_Error;
         return Null_Type;
      end if;
   end Addr_Of;

   --  In CIL, we have the ability to have enumeration types
   --  which are loaded and stored as integers, but have a valuetype
   --  signature for being passed to/from methods.

   -------------------
   -- New_Enum_Type --
   -------------------

   function New_Enum_Type (E : Entity_Id) return Type_Id is
      Result : constant Type_Id := New_Type (Int_Kind);
   begin
      String_To_Name_Buffer
        (Sinfo.Strval (Einfo.Interface_Name (Sinfo.Scope (E))));
      --  Just use Interface_Name, as it already contains the valuetype
      --  signature.
      Set_Name (Result, Name (Name_Buffer (1 .. Name_Len)));

      return Result;
   end New_Enum_Type;

   ---------------
   -- New_Class --
   ---------------

   function New_Class
     (Ada_Ent     : Entity_Id;
      Name        : Name_Id;
      Pkg_Name    : String_Id := No_String;
      Src_Name    : Name_Id   := No_Name;
      Super       : Class_Id  := Java_Lang_Object;
      Outer_Class : Class_Id  := Null_Class;
      Public      : Boolean   := True;
      Abstrct     : Boolean   := False;
      Final       : Boolean   := False) return Class_Id
   is
      Class       : constant Class_Id := New_Class;
      Class_Type  : constant Type_Id  := New_Type (Class_Kind);

      Init_Method : Method_Id;
      JVM_Formal  : Local_Var_Id;
      pragma Unreferenced (JVM_Formal);

   begin
      pragma Assert (Name /= No_Name);

      --  When we are parsing the mssyst files, we don't want to
      --  create a new class for string, which was already declared
      --  in jvm-api.adb (avoids type mismatch)

      if Name_String (Name) = "String" then
         String_To_Name_Buffer (Pkg_Name);

         if Name_Buffer (1 .. Name_Len) = "[mscorlib]System" then
            return Java_Lang_String;
         end if;
      end if;

      Set_Name (Class, Name);
      Set_Name (Class_Type, Name);

      Set_Pkg_Name (Class, Pkg_Name);
      Set_Src_Name (Class, Src_Name);
      Set_Superclass (Class, Super);
      Set_Outer_Class (Class, Outer_Class);

      if Outer_Class /= Null_Class then
         Add_Nested_Class (Outer_Class, Class);
      end if;

      Set_Is_Public (Class, Public);
      Set_Is_Abstract (Class, Abstrct);
      Set_Is_Final (Class, Final);
      Set_Is_Open (Class, False);
      Set_Is_Built (Class, False);

      --  Associate the new class and its class type with one another

      Set_Class_Type (Class, Class_Type);
      Set_Class (Class_Type, Class);

      --  Associate the new class with its corresponding Ada entity.
      --  Skip packages because in such case the Ada_Entity is associated
      --  with the Class instead of the Class_Type (handle it here???)

      if Present (Ada_Ent)
        and then not Ekind_In (Ada_Ent,
                       E_Package,   E_Generic_Package,
                       E_Procedure, E_Generic_Procedure,
                       E_Function,  E_Generic_Function)
      then
         Set_Map (Ada_Ent, Class_Type);
      end if;

      --  Create the default constructor method for the class

      if Super = System_Delegate then
         Init_Method :=
           New_Method (Class, J_String.Name (".ctor"), Void_Type, False,
                       Delegate => True);
         JVM_Formal := New_Method_Parameter
           (Init_Method, "object", JVM.Java_Lang_Object_Type);
         JVM_Formal := New_Method_Parameter
           (Init_Method, "method", Native_Int_Type);
      else
         Init_Method :=
           New_Method (Class, J_String.Name (".ctor"), Void_Type, False);
      end if;

      return Class;
   end New_Class;

   -------------------
   -- New_Interface --
   -------------------

   function New_Interface
     (Ada_Ent  : Entity_Id;
      Name     : Name_Id;
      Pkg_Name : String_Id := No_String;
      Src_Name : Name_Id   := No_Name;
      Public   : Boolean   := True) return Class_Id
   is
      Intrface      : constant Class_Id := New_Interface;
      Intrface_Type : constant Type_Id  := New_Type (Class_Kind);

   begin
      pragma Assert (Name /= No_Name);

      Set_Name (Intrface, Name);
      Set_Name (Intrface_Type, Name);
      Set_Pkg_Name (Intrface, Pkg_Name);
      Set_Src_Name (Intrface, Src_Name);
      Set_Superclass (Intrface, Java_Lang_Object);
      Set_Outer_Class (Intrface, Null_Class);
      Set_Is_Public (Intrface, Public);
      Set_Is_Abstract (Intrface, True);

      --  Associate the new class and its class type with one another

      Set_Class_Type (Intrface, Intrface_Type);
      Set_Class (Intrface_Type, Intrface);

      --  Associate the new class with its corresponding Ada entity

      if Present (Ada_Ent) then
         Set_Map (Ada_Ent, Intrface_Type);
      end if;

      return Intrface;
   end New_Interface;

   -------------------------
   -- Associate_Interface --
   -------------------------

   procedure Associate_Interface (Class : Class_Id; Intrface : Class_Id) is
   begin
      pragma Assert (Is_Interface (Intrface));
      Add_Interface (Class, Intrface);
   end Associate_Interface;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (Class : Class_Id) return Boolean is
   begin
      return JVM.Info.Is_Abstract (Class);
   end Is_Abstract;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (Method : Method_Id) return Boolean is
   begin
      return JVM.Info.Is_Abstract (Method);
   end Is_Abstract;

   -------------------------
   -- Is_Array_Descriptor --
   -------------------------

   function Is_Array_Descriptor (Typ : Type_Id) return Boolean is
   begin
      return JVM.Type_Kind (Typ) = Class_Kind
        and then Typ /= Any_Ref_Type
        and then JVM.Info.Is_Array_Descriptor (Typ);
   end Is_Array_Descriptor;

   --------------
   -- Is_Built --
   --------------

   function Is_Built (Class : Class_Id) return Boolean is
   begin
      return JVM.Info.Is_Built (Class);
   end Is_Built;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor (Class : Class_Id; Target : Class_Id) return Boolean is
      Super_Class  : Class_Id := Class;
      Intrface_Ref : JVM_Entity_Ref;

   begin
      while Super_Class /= Target
        and then Super_Class /= Java_Lang_Object
      loop
         --  Check also implemented interface
         Intrface_Ref := First_Interface_Ref (Super_Class);

         while Intrface_Ref /= Null_Entity_Ref loop
            if Is_Ancestor (Get_Interface (Intrface_Ref), Target) then
               return True;
            end if;

            Intrface_Ref := Next_Interface_Ref (Intrface_Ref);
         end loop;

         Super_Class := Superclass (Super_Class);
      end loop;

      return Super_Class = Target;
   end Is_Ancestor;

   -------------------
   -- Is_Descriptor --
   -------------------

   function Is_Descriptor (Typ : Type_Id) return Boolean is
   begin
      return JVM.Type_Kind (Typ) = Class_Kind
        and then Typ /= Any_Ref_Type
        and then JVM.Info.Is_Descriptor (Typ);
   end Is_Descriptor;

   ------------------
   -- Is_Interface --
   ------------------

   function Is_Interface (Class_Or_Interface : Class_Id) return Boolean is
   begin
      return JVM.Info.Is_Interface (Class_Or_Interface);
   end Is_Interface;

   --------------
   -- Is_Param --
   --------------

   function Is_Param (Local : Local_Var_Id) return Boolean is
   begin
      return JVM.Info.Is_Param (Local);
   end Is_Param;

   -------------------------
   -- Change_To_Interface --
   -------------------------

   procedure Change_To_Interface (Class : Class_Id) is
   begin
      Set_Is_Interface (Class);
   end Change_To_Interface;

   ----------------------
   -- Begin_Class_File --
   ----------------------

   procedure Begin_Class_File (Class : Class_Id) is
   begin
      pragma Assert (not Is_Built (Class));
      Set_Is_Open (Class);

      if Debug then
         Print ("*** Opening class file for ");
         Print (Name (Class));
         Print_Line;
         Print_Line;
         Print ("class ");
         Print (Name (Class));

         if Superclass (Class) /= Java_Lang_Object then
            Print (" extends ");
            Print (Name (Superclass (Class)));
         end if;

         Print_Line (" {");
         Print_Line;
      end if;
   end Begin_Class_File;

   --------------------
   -- End_Class_File --
   --------------------

   procedure End_Class_File (Class : Class_Id) is
      Method : Method_Id := First_Method (Class);

   begin
      --  All methods must be either abstract or closed at this point

      while Method /= Null_Method loop
         pragma Assert (Is_Abstract (Method)
                        or else Is_Delegate (Method)
                        or else not Is_Open (Method));
         Method := Next_Method (Method);
      end loop;

      --  Generate the physical class file

      if Outer_Class (Class) = Null_Class then
         JVM.Emit.Produce_Class_File (Class);
      end if;

      Set_Is_Open (Class, False);
      Set_Is_Built (Class);

      if Debug then
         Print_Line;
         Print ("} // ");
         Print (Name (Class));
         Print_Line;
         Print_Line;
         Print ("*** Closing class file for ");
         Print (Name (Class));
         Print_Line;
      end if;
   end End_Class_File;

   --------------------------
   -- Generate_Empty_Class --
   --------------------------

   procedure Generate_Empty_Class is
   begin
      JVM.Emit.CIL.Produce_Empty_File;
   end Generate_Empty_Class;

   ------------------------
   -- Class_File_Is_Open --
   ------------------------

   function Class_File_Is_Open (Class : Class_Id) return Boolean is
   begin
      return Is_Open (Class);
   end Class_File_Is_Open;

   ----------
   -- Name --
   ----------

   function Name (Class : Class_Id) return Name_Id is
   begin
      return JVM.Info.Name (Class);
   end Name;

   ----------------
   -- Superclass --
   ----------------

   function Superclass (Class : Class_Id) return Class_Id is
   begin
      return JVM.Info.Superclass (Class);
   end Superclass;

   -----------------
   -- Outer_Class --
   -----------------

   function Outer_Class (Class : Class_Id) return Class_Id is
   begin
      return JVM.Info.Outer_Class (Class);
   end Outer_Class;

   ---------------------
   -- Is_Parent_Class --
   ---------------------

   function Is_Parent_Class (Test_Class, Child : Class_Id) return Boolean is
   begin
      return Child /= Null_Class
        and then (Test_Class = Superclass (Child)
                   or else Is_Parent_Class (Test_Class, Superclass (Child)));
   end Is_Parent_Class;

   -------------
   -- Type_Of --
   -------------

   function Type_Of (Class : Class_Id) return Type_Id is
   begin
      return Class_Type (Class);
   end Type_Of;

   -----------------
   -- First_Field --
   -----------------

   function First_Field (Class : Class_Id) return Field_Id is
   begin
      return JVM.Info.First_Field (Class);
   end First_Field;

   ------------------------
   -- First_Nested_Class --
   ------------------------

   function First_Nested_Class (Class : Class_Id) return Class_Id is
   begin
      return JVM.Info.First_Nested_Class (Class);
   end First_Nested_Class;

   ------------------
   -- First_Method --
   ------------------

   function First_Method (Class : Class_Id) return Method_Id is
   begin
      return JVM.Info.First_Method (Class);
   end First_Method;

   -------------------------
   -- Default_Constructor --
   -------------------------

   function Default_Constructor (Class : Class_Id) return Method_Id is
   begin
      return First_Method (Class);
   end Default_Constructor;

   --------------------
   -- Set_Superclass --
   --------------------

   procedure Set_Superclass (Class : Class_Id; Super : Class_Id) is
   begin
      JVM.Info.Set_Superclass (Class, Super);
   end Set_Superclass;

   ------------------
   -- Set_Abstract --
   ------------------

   procedure Set_Abstract (Class : Class_Id; Abstrct : Boolean := True) is
   begin
      JVM.Info.Set_Is_Abstract (Class, Abstrct);
   end Set_Abstract;

   ---------------------
   -- Set_Is_Abstract --
   ---------------------

   procedure Set_Is_Abstract (Method : Method_Id; Value : Boolean := True) is
   begin
      JVM.Info.Set_Is_Abstract (Method, Value);
   end Set_Is_Abstract;

   ---------------------------------------------------
   -- Java Type-related declarations and operations --
   ---------------------------------------------------

   function Descriptor_Type (Typ : Type_Id) return Type_Id is
   begin
      return JVM.Info.Descriptor_Type (Typ);
   end Descriptor_Type;

   -----------------------------
   -- Set_Is_Array_Descriptor --
   -----------------------------

   procedure Set_Is_Array_Descriptor
     (Typ : Type_Id; Value : Boolean := True) is
   begin
      JVM.Info.Set_Is_Array_Descriptor (Typ, Value);
   end Set_Is_Array_Descriptor;

   -----------------------
   -- Set_Is_Descriptor --
   -----------------------

   procedure Set_Is_Descriptor (Typ : Type_Id; Value : Boolean := True) is
   begin
      JVM.Info.Set_Is_Descriptor (Typ, Value);
   end Set_Is_Descriptor;

   ---------------
   -- Type_Kind --
   ---------------

   function Type_Kind (Typ : Type_Id) return JVM_Type_Kind is
   begin
      return JVM.Info.Type_Kind (Typ);
   end Type_Kind;

   ----------
   -- Name --
   ----------

   function Name (Typ : Type_Id) return Name_Id is
   begin
      return JVM.Info.Name (Typ);
   end Name;

   -----------------------
   -- Is_Primitive_Type --
   -----------------------

   function Is_Primitive_Type (Typ : Type_Id) return Boolean is
   begin
      return Typ /= Null_Type
        and then Type_Kind (Typ) in Int_Kind .. Double_Kind;
   end Is_Primitive_Type;

   -----------------------
   -- Is_Reference_Type --
   -----------------------

   function Is_Reference_Type (Typ : Type_Id) return Boolean is
   begin
      return Typ /= Null_Type
        and then Type_Kind (Typ) in Array_Kind .. Class_Kind;
   end Is_Reference_Type;

   --------------------
   -- New_Array_Type --
   --------------------

   function New_Array_Type
     (Element_Type : Type_Id;
      Dimensions   : Pos_8   := 1;
      Type_Name    : Name_Id := No_Name) return Type_Id
   is
      Array_Type : constant Type_Id := New_Type (Array_Kind);

   begin
      Set_Name (Array_Type, Type_Name);
      Set_Element_Type (Array_Type, Element_Type);
      Set_Dimensions (Array_Type, Dimensions);

      return Array_Type;
   end New_Array_Type;

   ------------------
   -- Element_Type --
   ------------------

   function Element_Type (Arr_Type : Type_Id) return Type_Id is
   begin
      return JVM.Info.Element_Type (Arr_Type);
   end Element_Type;

   --------------------
   -- Dimensionality --
   --------------------

   function Dimensionality (Arr_Type : Type_Id) return Pos_8 is
   begin
      return Dimensions (Arr_Type);
   end Dimensionality;

   -------------------
   -- Class_Of_Type --
   -------------------

   function Class_Of_Type (Class_Type : Type_Id) return Class_Id is
   begin
      return Class (Class_Type);
   end Class_Of_Type;

   ----------------------------
   -- Literal_Needs_Pool_Ref --
   ----------------------------

   function Literal_Needs_Pool_Ref
     (I_Type  : Type_Id;
      Literal : Uint) return Boolean
   is
   begin
      if Type_Kind (I_Type) = Long_Kind then
         return Literal /= Uint_0 and then Literal /= Uint_1;

      --  Without check below, was getting bug box for mod 2**32 literal
      --  that wouldn't fit in an integer

      elsif not UI_Is_In_Int_Range (Literal) then
         return True;
      else
         return Literal < Int (-(2 ** 15))
           or else Literal > Int ((2 ** 15) - 1);
      end if;
   end Literal_Needs_Pool_Ref;

   ----------------------------
   -- Literal_Needs_Pool_Ref --
   ----------------------------

   function Literal_Needs_Pool_Ref
     (F_Type  : Type_Id;
      Literal : Ureal) return Boolean
   is
   begin
      pragma Assert (Type_Kind (F_Type) in Float_Kind .. Double_Kind);

      if Type_Kind (F_Type) = Float_Kind then
         return Literal < Ureal_0 and then Literal > Ureal_2;
      else
         return Literal /= Ureal_0 and then Literal /= Ureal_1;
      end if;
   end Literal_Needs_Pool_Ref;

   -------------------------
   -- Set_Descriptor_Type --
   -------------------------

   procedure Set_Descriptor_Type
     (Typ : Type_Id; Value : Type_Id) is
   begin
      JVM.Info.Set_Descriptor_Type (Typ, Value);
   end Set_Descriptor_Type;

   ------------------------------------------
   -- Operations for defining class fields --
   ------------------------------------------

   ---------------
   -- New_Field --
   ---------------

   function New_Field
     (Class    : Class_Id;
      Name     : Name_Id;
      Ftype    : Type_Id;
      Static   : Boolean;
      Final    : Boolean       := False;
      Volatile : Boolean       := False;
      Acc_Mode : Member_Access := Public_Access) return Field_Id
   is
      Field : constant Field_Id := New_Field;

   begin
      Set_Name        (Field, Name);
      Set_Class       (Field, Class);
      Set_Field_Type  (Field, Ftype);
      Set_Is_Static   (Field, Static);
      Set_Is_Final    (Field, Final);
      Set_Is_Volatile (Field, Volatile);
      Set_Access_Mode (Field, Acc_Mode);

      Add_Field (Class, Field);

      if Debug then
         Print ("   ");
         if Static then
            Print ("static ");
         end if;
         if Final then
            Print ("final ");
         end if;
         Print (JVM.Info.Name (Ftype));
         Print (" ");
         Print (Name);
         Print_Line (";");
      end if;

      return Field;
   end New_Field;

   ----------
   -- Name --
   ----------

   function Name (Field : Field_Id) return Name_Id is
   begin
      return JVM.Info.Name (Field);
   end Name;

   ----------------
   -- Next_Field --
   ----------------

   function Next_Field (Field : Field_Id) return Field_Id is
   begin
      return JVM.Info.Next_Field (Field);
   end Next_Field;

   -----------------------
   -- Next_Nested_Class --
   -----------------------

   function Next_Nested_Class (Class : Class_Id) return Class_Id is
   begin
      return JVM.Info.Next_Nested_Class (Class);
   end Next_Nested_Class;

   -------------
   -- Type_Of --
   -------------

   function Type_Of (Field : Field_Id) return Type_Id is
   begin
      return Field_Type (Field);
   end Type_Of;

   ---------------
   -- Is_Static --
   ---------------

   function Is_Static (Field : Field_Id) return Boolean is
   begin
      return JVM.Info.Is_Static (Field);
   end Is_Static;

   -----------
   -- Field --
   -----------

   function Field (Class : Class_Id; Name : Name_Id) return Field_Id is
      Fld : Field_Id := First_Field (Class);

   begin
      while Fld /= Null_Field loop
         if JVM.Info.Name (Fld) = Name then
            return Fld;
         end if;
         Fld := Next_Field (Fld);
      end loop;

      if Superclass (Class) = Null_Class then
         return Null_Field;
      else
         return Field (Superclass (Class), Name);
      end if;
   end Field;

   -----------
   -- Field --
   -----------

   function Field (Class : Class_Id; Name : String) return Field_Id is
   begin
      return Field (Class, J_String.Name (Name));
   end Field;

   ---------------------------------------------------------------
   -- Operations for defining class methods and local variables --
   ---------------------------------------------------------------

   --------------------------------
   -- Class_Of_Wrapped_Interface --
   --------------------------------

   function Class_Of_Wrapped_Interface (Method : Method_Id) return Class_Id is
   begin
      return JVM.Info.Class_Of_Wrapped_Interface (Method);
   end Class_Of_Wrapped_Interface;

   ----------------------
   -- Has_AR_SL_Formal --
   ----------------------

   function Has_AR_SL_Formal (Method : Method_Id) return Boolean is
   begin
      return JVM.Info.Has_AR_SL_Formal (Method);
   end Has_AR_SL_Formal;

   --------------------------
   -- Is_Interface_Wrapper --
   --------------------------

   function Is_Interface_Wrapper (Method : Method_Id) return Boolean is
   begin
      return JVM.Info.Is_Interface_Wrapper (Method);
   end Is_Interface_Wrapper;

   ----------------
   -- New_Method --
   ----------------

   function New_Method
     (Class         : Class_Id;
      Name          : Name_Id;
      Result        : Type_Id;
      Static        : Boolean;
      Abstrct       : Boolean       := False;
      Final         : Boolean       := False;
      Synch         : Boolean       := False;
      Acc_Mode      : Member_Access := Public_Access;
      Parent        : Method_Id     := Null_Method;
      Exp_Stdcall   : String_Id     := No_String;
      Skip_Arg_This : Boolean       := False;
      Delegate      : Boolean       := False;
      Is_AR_Method  : Boolean       := False) return Method_Id
   is
      Method : constant Method_Id := New_Method;
      This   : Local_Var_Id;
      pragma Unreferenced (This);

   begin
      Set_Name             (Method, Name);
      Set_Class            (Method, Class);
      Set_Result_Type      (Method, Result);
      Set_Is_Abstract      (Method, Abstrct);
      Set_Is_Delegate      (Method, Delegate);
      Set_Is_AR_Method     (Method, Is_AR_Method);
      Set_Is_Static        (Method, Static);
      Set_Is_Final         (Method, Final);
      Set_Is_Synchronized  (Method, Synch);
      Set_Access_Mode      (Method, Acc_Mode);
      Set_Parent_Method    (Method, Parent);
      Set_Exported_Stdcall (Method, Exp_Stdcall);

      Set_Is_Open (Method);

      --  Add the 'this' parameter in the case of an instance method

      if not Static and then not Skip_Arg_This then
         This :=
           New_Method_Parameter
             (Method, J_String.Name ("$this"), Type_Of (Class));
      end if;

      Add_Method (Class, Method);

      return Method;
   end New_Method;

   -----------------
   -- Next_Method --
   -----------------

   function Next_Method (Method : Method_Id) return Method_Id is
   begin
      return JVM.Info.Next_Method (Method);
   end Next_Method;

   --------------------------
   -- New_Method_Parameter --
   --------------------------

   function New_Method_Parameter
     (Method : Method_Id;
      Name   : Name_Id;
      Ptype  : Type_Id) return Local_Var_Id
   is
      Param : constant Local_Var_Id := New_Local_Var;

   begin
      Set_Name          (Param, Name);
      Set_Local_Index   (Param, Next_Local_Index (Method));
      Set_Method        (Param, Method);
      Set_Variable_Type (Param, Ptype);
      Set_Is_Param      (Param);

      Add_Local_Var     (Method, Param);

      return Param;
   end New_Method_Parameter;

   --------------------------
   -- New_Method_Parameter --
   --------------------------

   function New_Method_Parameter
     (Method : Method_Id;
      Name   : String;
      Ptype  : Type_Id) return Local_Var_Id
   is
   begin
      return New_Method_Parameter (Method, J_String.Name (Name), Ptype);
   end New_Method_Parameter;

   ----------
   -- Name --
   ----------

   function Name (Method : Method_Id) return Name_Id is
   begin
      return JVM.Info.Name (Method);
   end Name;

   -----------------
   -- Open_Method --
   -----------------

   procedure Open_Method (Method : Method_Id) is
      Code_Seq     : Code_Sequence;
      Handler_Seq  : Handler_Sequence;
      Method_Stack : Op_Stack_Id;

   begin
      pragma Assert (Is_Open (Class (Method))
                       and then Is_Open (Method)
                       and then not Is_Abstract (Method));

      --  Note: In the current implementation model, the method is
      --  marked open when the method is created, so it doesn't need
      --  to be opened again here; not clear if this is best, but
      --  allows for the check that the method hasn't been closed
      --  without requiring an additional Boolean attribute (the
      --  reason we set the method to open instead of closed on
      --  a call to New_Method is to disallow reopening a method
      --  for code generation once it has been closed).

      Set_Is_Open (Method);

      --  Initialize the method's code sequence

      Code_Seq := Method_Code (Method);
      Start_Sequence (Code_Seq);
      Set_Code (Method, Code_Seq);

      --  Initialize the method's exception handler entry sequence

      Handler_Seq := Method_Handlers (Method);
      Start_Sequence (Handler_Seq);
      Set_Handlers (Method, Handler_Seq);

      --  Initialize the method's operand type stack

      Method_Stack := New_Stack (200);
      Set_Op_Stack (Method, Method_Stack);

      if Debug then
         Print ("   ");

         if Access_Mode (Method) = Public_Access then
            Print ("public ");
         elsif Access_Mode (Method) = Protected_Access then
            Print ("protected ");
         elsif Access_Mode (Method) = Private_Access then
            Print ("private ");
         end if;

         if Is_Abstract (Method) then
            Print ("abstract ");
         end if;

         if Is_Static (Method) then
            Print ("static ");
         end if;

         Print (Name (Result_Type (Method)));
         Print (" ");
         Print (Name (Method));
         Print (" (");

         declare
            Param : Local_Var_Id := First_Local_Var (Method);
         begin
            --  Skip any 'this' parameter

            if not Is_Static (Method) then
               Param := Next_Local_Var (Param);
            end if;

            while Param /= Null_Local_Var and then Is_Param (Param) loop
               Print (Name (Type_Of (Param)));
               Print (" ");
               Print (Name (Param));
               Param := Next_Local_Var (Param);

               if Param /= Null_Local_Var then
                  Print (" ");
               end if;
            end loop;
         end;

         Print_Line (") {");
      end if;
   end Open_Method;

   ------------------
   -- Close_Method --
   ------------------

   procedure Close_Method (Method : Method_Id) is
   begin
      pragma Assert (Is_Open (Method) and then Is_Empty (Op_Stack (Method)));
      pragma Assert (Active_Subroutine (Method) = Null_Subroutine);

      --  If the method does not end with a return or throw instruction,
      --  then we generate a raise of Program_Error at the method end.
      --  Note that this can occur in cases where an Assert pragma occurs
      --  at the end of a subprogram, in which case the front end does
      --  not generate a return statement. But the Java verifier
      --  will complain about falling off the end of the code unless
      --  we end the method with a return or throw. This is one case
      --  where we let knowledge of Ada semantics (Program_Error) leak
      --  through to the JVM package, because this is the easiest place
      --  to detect this situation.

      if Get (Last (Active_Seq)).Op /= RET
        and then Get (Last (Active_Seq)).Op /= THROW
      then
         Gen_Default_Object (API_Class (Ada_Program_Error));
         Gen_Exception_Throw;
      end if;

      --  Check that all targeted labels have been generated

      declare
         Label : Label_Id := First_Label (Method);

      begin
         while Label /= Null_Label loop
            pragma Assert
              (not Is_Targeted (Label) or else Location (Label) /= Null_Instr);

            --  If the label instruction has no successor, then it
            --  must be at the end of the method and so we have to
            --  generate an extra instruction in order to satisfy
            --  the verifier. This instruction should not be reachable,
            --  even though it's targeted. This can occur due to
            --  cases like 'if' statements where each branch of
            --  the statement contains a return, but we have still
            --  generated a branch out of the statement (ideally
            --  we should optimize away such branches, but that's
            --  a bit tricky ???).

            if Is_Targeted (Label)
              and then Get (Location (Label)).Next = Null_Instr
            then
               Gen_NOP;
            end if;

            Label := Next_Label (Label);
         end loop;
      end;

      --  Record the maximum word depth of the stack before it gets
      --  deallocated.

      Set_Max_Stack_Depth (Method, Max_Depth (Op_Stack (Method)));

      --  If this is the current active method then update the method's
      --  code and handler sequences and reset the active method and its
      --  associated variables.

      if Active_Method = Method then
         Set_Code (Method, Active_Seq);
         Set_Handlers (Method, Active_Handlers);
         Active_Method := Null_Method;
         Active_Class  := Null_Class;
         Active_Subr   := Null_Subroutine;
         Active_Stack  := Null_Op_Stack;
      end if;

      --  Ensure that the method's operand stack is freed

      Set_Op_Stack (Method, Null_Op_Stack);

      Set_Is_Open (Method, False);

      if Debug then
         Print ("   } // ");
         Print (Name (Method));
         Print_Line;
      end if;
   end Close_Method;

   ------------------------------------
   -- Set_Class_Of_Wrapped_Interface --
   ------------------------------------

   procedure Set_Class_Of_Wrapped_Interface
     (Method      : Method_Id;
      Iface_Class : Class_Id) is
   begin
      pragma Assert (Is_Interface (Iface_Class));
      pragma Assert (Is_Interface_Wrapper (Method));

      JVM.Info.Set_Class_Of_Wrapped_Interface (Method, Iface_Class);
   end Set_Class_Of_Wrapped_Interface;

   ------------------------
   -- Set_Current_Method --
   ------------------------

   procedure Set_Current_Method (Method : Method_Id) is
   begin
      pragma Assert (Active_Subr = Null_Subroutine);
      pragma Assert (Is_Open (Class (Method)) and then Is_Open (Method));
      pragma Assert (not Entry_Seq_Active);

      --  Save away the current method's code sequence and operand stack

      if Active_Method /= Null_Method then
         Set_Code (Active_Method, Active_Seq);
         Set_Handlers (Active_Method, Active_Handlers);
         Set_Op_Stack (Active_Method, Active_Stack);
         Set_Stack_Checking (Active_Method, Stack_Checking);
      end if;

      Active_Method   := Method;
      Active_Class    := Class (Method);
      Active_Seq      := Method_Code (Method);
      Active_Handlers := Method_Handlers (Method);

      Active_Stack    := Op_Stack (Method);
      Set_Stack_Method (Active_Stack, Method);

      Active_Subr     := Null_Subroutine;
      Stack_Checking  := Is_Stack_Checking (Method);
      Stack_Marked    := Marked (Active_Stack);

      if Debug then
         Print ("*** Switching to method ");
         Print (Name (Method));
         Print_Line;
      end if;
   end Set_Current_Method;

   --------------------------
   -- Set_Has_AR_SL_Formal --
   --------------------------

   procedure Set_Has_AR_SL_Formal
     (Method : Method_Id; Value : Boolean := True) is
   begin
      JVM.Info.Set_Has_AR_SL_Formal (Method, Value);
   end Set_Has_AR_SL_Formal;

   ------------------------------
   -- Set_Is_Interface_Wrapper --
   ------------------------------

   procedure Set_Is_Interface_Wrapper
     (Method : Method_Id; Value : Boolean := True) is
   begin
      JVM.Info.Set_Is_Interface_Wrapper (Method, Value);
   end Set_Is_Interface_Wrapper;

   --------------------
   -- Current_Method --
   --------------------

   function Current_Method return Method_Id is
   begin
      return Active_Method;
   end Current_Method;

   -------------------------------
   -- Start_Entry_Code_Sequence --
   -------------------------------

   procedure Start_Entry_Code_Sequence is
   begin
      pragma Assert (not Entry_Seq_Active);

      Saved_Seq := Active_Seq;
      Active_Seq := Empty_Sequence;
      Start_Sequence (Active_Seq);
      Entry_Seq_Active := True;
   end Start_Entry_Code_Sequence;

   -----------------------------
   -- End_Entry_Code_Sequence --
   -----------------------------

   procedure End_Entry_Code_Sequence is
   begin
      Prepend (Active_Seq, Saved_Seq);
      Active_Seq := Saved_Seq;
      Entry_Seq_Active := False;
   end End_Entry_Code_Sequence;

   ------------------
   -- Is_AR_Method --
   ------------------

   function Is_AR_Method (Method : Method_Id) return Boolean is
   begin
      return JVM.Info.Is_AR_Method (Method);
   end Is_AR_Method;

   -----------------
   -- Is_Delegate --
   -----------------

   function Is_Delegate (Method : Method_Id) return Boolean is
   begin
      return JVM.Info.Is_Delegate (Method);
   end Is_Delegate;

   ---------------
   -- Is_Static --
   ---------------

   function Is_Static (Method : Method_Id) return Boolean is
   begin
      return JVM.Info.Is_Static (Method);
   end Is_Static;

   ------------
   -- Method --
   ------------

   function Method (Class : Class_Id; Name : Name_Id) return Method_Id is
      Mthd : Method_Id := First_Method (Class);

   begin
      while Mthd /= Null_Method loop
         if JVM.Info.Name (Mthd) = Name then
            return Mthd;
         end if;
         Mthd := Next_Method (Mthd);
      end loop;

      if Superclass (Class) = Null_Class then
         return Null_Method;
      else
         return Method (Superclass (Class), Name);
      end if;
   end Method;

   ------------
   -- Method --
   ------------

   function Method (Class : Class_Id; Name : String) return Method_Id is
   begin
      return Method (Class, J_String.Name (Name));
   end Method;

   ------------
   -- Method --
   ------------

   function Method
     (Class   : Class_Id;
      Name    : Name_Id;
      Result  : Type_Id;
      Param_0 : Type_Id := Null_Type;
      Param_1 : Type_Id := Null_Type) return Method_Id
   is
      Mthd  : Method_Id := First_Method (Class);
      Param : Local_Var_Id;

   begin
      while Mthd /= Null_Method loop
         if JVM.Info.Name (Mthd) = Name
           and then Result_Type (Mthd) = Result
         then
            Param := First_Local_Var (Mthd);

            --  If searching for a parameterless method and Mthd
            --  has no parameters, then return the method.

            if Param_0 = Null_Type then
               if Param = Null_Local_Var or else not Is_Param (Param) then
                  return Mthd;
               end if;

            elsif Param /= Null_Local_Var
              and then Is_Param (Param)
              and then Type_Of (Param) = Param_0
            then
               Param := Next_Local_Var (Param);

               --  If searching for a one-parameter method and Mthd has
               --  exactly one parameter whose type matches Param_0, then
               --  return the method.

               if Param_1 = Null_Type then
                  if Param = Null_Local_Var or else not Is_Param (Param) then
                     return Mthd;
                  end if;

               --  If searching for a two-parameter method and Mthd has
               --  a second parameter, with no successor parameter, whose
               --  type matches Param_1, then return the method.

               elsif Param /= Null_Local_Var
                 and then Is_Param (Param)
                 and then Type_Of (Param) = Param_1
                 and then (Next_Local_Var (Param) = Null_Local_Var
                            or else not Is_Param (Next_Local_Var (Param)))
               then
                  return Mthd;
               end if;
            end if;
         end if;

         Mthd := Next_Method (Mthd);
      end loop;

      if Superclass (Class) = Null_Class then
         return Null_Method;
      else
         return Method (Superclass (Class), Name, Result, Param_0, Param_1);
      end if;
   end Method;

   ------------
   -- Method --
   ------------

   function Method
     (Class   : Class_Id;
      Name    : String;
      Result  : Type_Id;
      Param_0 : Type_Id := Null_Type;
      Param_1 : Type_Id := Null_Type) return Method_Id
   is
   begin
      return Method (Class, J_String.Name (Name), Result, Param_0, Param_1);
   end Method;

   --------------
   -- Class_Of --
   --------------

   function Class_Of (Method : Method_Id) return Class_Id is
   begin
      return Class (Method);
   end Class_Of;

   -------------------
   -- Parent_Method --
   -------------------

   function Parent_Method (Method : Method_Id) return Method_Id is
   begin
      return JVM.Info.Parent_Method (Method);
   end Parent_Method;

   ------------------
   -- Is_Completed --
   ------------------

   function Is_Completed (Method : Method_Id) return Boolean is
   begin
      --  If Is_Open return False, then the method has been fully
      --  generated and closed (note that method's are initially
      --  marked as open, even though Open_Method has not been
      --  called on the method).

      return not Is_Open (Method);
   end Is_Completed;

   ----------------------------
   -- Set_Current_Source_Loc --
   ----------------------------

   Current_Source_Loc : Source_Ptr;

   procedure Set_Current_Source_Loc (Sloc : Source_Ptr) is
   begin
      Current_Source_Loc := Sloc;
      Gen_Label (New_Label, Sloc);
   end Set_Current_Source_Loc;

   ----------------------------
   -- Get_Current_Source_Loc --
   ----------------------------

   function Get_Current_Source_Loc return Source_Ptr is
   begin
      return Current_Source_Loc;
   end Get_Current_Source_Loc;

   -------------------
   -- New_Local_Var --
   -------------------

   function New_Local_Var
     (Method : Method_Id;
      Name   : Name_Id;
      Ltype  : Type_Id) return Local_Var_Id
   is
      Local : constant Local_Var_Id := New_Local_Var;

   begin
      pragma Assert (Is_Open (Class (Method)) and then Is_Open (Method));
      pragma Assert (Ltype /= Any_Ref_Type);

      Set_Name          (Local, Name);
      Set_Method        (Local, Method);
      Set_Local_Index   (Local, Next_Local_Index (Method));
      Set_Variable_Type (Local, Ltype);
      Set_Is_Param      (Local, False);

      Add_Local_Var     (Method, Local);

      if Debug then
         Print ("      ");
         Print (JVM.Info.Name (Ltype));
         Print (" ");
         Print (Name);
         Print_Line (";");
      end if;

      return Local;
   end New_Local_Var;

   -------------------
   -- New_Local_Var --
   -------------------

   function New_Local_Var
     (Name  : String;
      Ltype : Type_Id) return Local_Var_Id
   is
      Ret : Local_Var_Id;
   begin
      --  We really don't need duplicates for exc_var or retval
      --  since they can't be different types, so just lookup
      --  the right one, creating if it doesn't exist.

      if Name = "_retval" or Name = "_exc_var" then
         Ret := Local_Var (Current_Method, Name);

         if Ret = Null_Local_Var then
            return New_Local_Var
              (Current_Method, J_String.Name (Name), Ltype);
         else
            return Ret;
         end if;

      --  If the name is otherwise, the types might be different so
      --  tack on the type before the lookup (converted to an integer)

      elsif Name = "_duptmp1" or Name = "_duptmp2" then
         Ret := Local_Var (Current_Method, Name & Generate_String (Ltype));

         if Ret = Null_Local_Var then
            return New_Local_Var
              (Current_Method,
               J_String.Name (Name & Generate_String (Ltype)),
               Ltype);
         else
            return Ret;
         end if;

      else
         return New_Local_Var
           (Current_Method,
            J_String.Name
              (Name & Generate_String (Ltype) & Generate_Serial_String),
            Ltype);
      end if;
   end New_Local_Var;

   ---------------------
   -- First_Local_Var --
   ---------------------

   function First_Local_Var (Method : Method_Id) return Local_Var_Id is
   begin
      return JVM.Info.First_Local_Var (Method);
   end First_Local_Var;

   --------------------
   -- Next_Local_Var --
   --------------------

   function Next_Local_Var (Local : Local_Var_Id) return Local_Var_Id is
   begin
      return JVM.Info.Next_Local_Var (Local);
   end Next_Local_Var;

   ----------
   -- Name --
   ----------

   function Name (Local : Local_Var_Id) return Name_Id is
   begin
      return JVM.Info.Name (Local);
   end Name;

   ----------------
   -- This_Local --
   ----------------

   function This_Local (Method : Method_Id) return Local_Var_Id is
   begin
      pragma Assert (not Is_Static (Method));

      return First_Local_Var (Method);
   end This_Local;

   ---------------
   -- Local_Var --
   ---------------

   function Local_Var (Method : Method_Id; Name : Name_Id) return Local_Var_Id
   is
      Local : Local_Var_Id := First_Local_Var (Method);

   begin
      while Local /= Null_Local_Var loop
         if JVM.Info.Name (Local) = Name then
            return Local;
         end if;

         Local := Next_Local_Var (Local);
      end loop;

      return Null_Local_Var;
   end Local_Var;

   ---------------
   -- Local_Var --
   ---------------

   function Local_Var (Method : Method_Id; Name : String) return Local_Var_Id
   is
   begin
      return Local_Var (Method, J_String.Name (Name));
   end Local_Var;

   -------------
   -- Type_Of --
   -------------

   function Type_Of (Local : Local_Var_Id) return Type_Id is
   begin
      return Variable_Type (Local);
   end Type_Of;

   ---------------
   -- Method_Of --
   ---------------

   function Method_Of (Local : Local_Var_Id) return Method_Id is
   begin
      return Method (Local);
   end Method_Of;

   -------------------------
   -- New_String_Constant --
   -------------------------

   function New_String_Constant (Str : String_Id) return String_Const_Id is
   begin
      pragma Assert (Active_Class /= Null_Class);

      return String_Const_Id (String_Item (Active_Class, Str));
   end New_String_Constant;

   -----------------
   -- Result_Type --
   -----------------

   function Result_Type (Method : Method_Id) return Type_Id is
   begin
      return JVM.Info.Result_Type (Method);
   end Result_Type;

   --------------------------------------------------------
   -- Operations for generating constant pool references --
   --------------------------------------------------------

   function New_Class_Ref (Class : Class_Id) return Pool_Id;
   --  Creates a constant pool item with tag CONSTANT_Class
   --  denoting Class and associates it with the current
   --  active class.

   function New_Array_Ref (Array_Typ : Type_Id) return Pool_Id;
   --  Creates a constant pool item with tag CONSTANT_Class
   --  associated with the current active class, representing
   --  the class of an array type.

   function New_Method_Ref (Method : Method_Id) return Pool_Id;
   --  Creates a constant pool item with tag CONSTANT_Methodref
   --  associated with the current active class, representing
   --  a reference to Method.

   -------------------
   -- New_Class_Ref --
   -------------------

   function New_Class_Ref (Class : Class_Id) return Pool_Id is
   begin
      return Class_Item (Active_Class, Class);
   end New_Class_Ref;

   -------------------
   -- New_Array_Ref --
   -------------------

   function New_Array_Ref (Array_Typ : Type_Id) return Pool_Id is
   begin
      return Array_Item (Active_Class, Array_Typ);
   end New_Array_Ref;

   --------------------
   -- New_Method_Ref --
   --------------------

   function New_Method_Ref (Method : Method_Id) return Pool_Id is
   begin
      return Method_Item (Active_Class, Method);
   end New_Method_Ref;

   -----------------------------------------------------------------
   -- Operations for generating Java Virtual Machine instructions --
   -----------------------------------------------------------------

   procedure Gen_Instr (Instr : JVM.Code.Instruction);
   --  Appends Instr as a new instruction in the currently active
   --  code sequence of. Emits a symbolic representation of the
   --  instruction if Debug is true.

   procedure Gen_Instr (Op : JVM.Code.CIL_Operation);
   --  Generates an instruction with opcode Op in the currently active
   --  code sequence. Op must indicate a one-byte opcode (i.e., needs
   --  no operands).

   procedure Gen_Instr (Op : CIL_Operation; Sint : Int_16);
   --  Generates an instruction with opcode Op and operand Sint in the
   --  currently active code sequence. Op must be either Bipush or Sipush.

   procedure Gen_Instr (Op : CIL_Operation; Local : Local_Var_Id);
   --  Generates an instruction with opcode Op and operand Local in the
   --  currently active code sequence. Op must be an instruction with
   --  a local variable index as its single operand (xload, xstore, ret).

   procedure Gen_Instr
     (Op : CIL_Operation; Field : Field_Id; Class : Class_Id);
   --  Generates an instruction with opcode Op and operands for
   --  the class and field in the currently active code sequence.
   --  Op must be an instruction such as LDFLD.

   procedure Gen_Instr (Op : CIL_Operation; Pool_Item : Pool_Id);
   --  Generates an instruction with opcode Op and operand Pool_Item in
   --  the currently active code sequence. Op must be an instruction with
   --  a constant pool reference as its single operand.

   procedure Gen_Instr (Op : CIL_Operation; Target : Label_Id);
   --  Generates an instruction with opcode Op and branch label Target in
   --  the currently active code sequence. Op must be a branch instruction.

   procedure Gen_Instr (Op : CIL_Operation; Subroutine : Subroutine_Id);
   --  Generates an instruction with opcode Op and target Subroutine in
   --  the currently active code sequence. Op must be either Jsr or Jsr_W.

   procedure Gen_Newarray (Elmt_Type : Type_Id);
   --  Generates a Newarray instruction for an array with the given element
   --  type in the currently active code sequence.

   procedure Gen_Multianewarray (Arr_CP : Pool_Id; Dimensions : Pos_8;
      Elmt_Type : Type_Id);
   --  Generates a Multianewarray instruction for the array type denoted
   --  by Arr_CP, using the specified number of Dimensions, in the currently
   --  active code sequence.

   procedure Push (Operand_Type : Type_Id);
   --  Push the given operand type on the active method's stack

   procedure Pop (Count : Stack_Range := 1);
   --  Pop the given number of types off of the active method's stack

   function Check_Top (T : Type_Id) return Boolean;
   --  Check that T is compatible with the top-of-stack type

   function Check_Top (Kind : JVM_Type_Kind) return Boolean;
   --  Check that Kind is compatible with the top-of-stack type

   function Check_Top (S : Op_Stack_Id; T : Type_Id) return Boolean;
   --  Check that T is compatible with the top-of-stack type

   function Check_Top (S : Op_Stack_Id; Kind : JVM_Type_Kind) return Boolean;
   --  Check that Kind is compatible with the top-of-stack type

   function Check_Next (T : Type_Id) return Boolean;
   --  Check that T is compatible with the next to top-of-stack type

   function Check_Next (Kind : JVM_Type_Kind) return Boolean;
   --  Check that Kind is compatible with the next to top-of-stack type

   --------------------
   -- Gen_Annotation --
   --------------------

   procedure Gen_Annotation (A : Annotation_Kind) is
      Instr : JVM.Code.Instruction (NOP);
   begin
      Instr.Annotation := A;
      Gen_Instr (Instr);
   end Gen_Annotation;

   ---------------
   -- Gen_Instr --
   ---------------

   procedure Gen_Instr (Instr : JVM.Code.Instruction) is
   begin
      pragma Assert (Active_Method /= Null_Method);

      if Debug and then First (Active_Seq) = Null_Instr then
         Print_Line ("   // begin");
      end if;

      Append (Active_Seq, Instr);

      --  Facilitate location of CIL statements with the debugger

      Instruction_Breakpoint (Active_Method, Count_Sequence (Active_Seq));
   end Gen_Instr;

   ---------------
   -- Gen_Instr --
   ---------------

   procedure Gen_Instr
     (Op : CIL_Operation; Field : Field_Id; Class : Class_Id)
   is
      Instr : JVM.Code.Instruction (Op);
   begin
      pragma Assert
        (Op = LDSFLD or else Op = LDFLD or else Op = STSFLD
         or else Op = STFLD or else Op = LDFLDA);
      Instr.Field := Field;
      Instr.Class := Class;
      Gen_Instr (Instr);
   end Gen_Instr;

   ---------------
   -- Gen_Instr --
   ---------------

   procedure Gen_Instr (Op : CIL_Operation) is
      Instr : JVM.Code.Instruction (Op);
   begin
      Instr.Next := Null_Instr;  --  so GNAT doesn't complain
      Gen_Instr (Instr);
   end Gen_Instr;

   ---------------
   -- Gen_Instr --
   ---------------

   procedure Gen_Instr (Op : CIL_Operation; Sint : Int_16) is
      Sint_Instr : JVM.Code.Instruction (Op);

   begin
      pragma Assert (Op = LDC_I4 or else Op = LDC_I4_S);

      Sint_Instr.Sint := Sint;
      Gen_Instr (Sint_Instr);
   end Gen_Instr;

   ---------------
   -- Gen_Instr --
   ---------------

   procedure Gen_Instr (Op : CIL_Operation; Local : Local_Var_Id) is
      Local_Instr : JVM.Code.Instruction (Op);

   begin
      Local_Instr.Local := Local;
      Gen_Instr (Local_Instr);
   end Gen_Instr;

   ---------------
   -- Gen_Instr --
   ---------------

   procedure Gen_Instr (Op : CIL_Operation; Pool_Item : Pool_Id) is
      Pool_Ref_Instr : JVM.Code.Instruction (Op);

   begin
      Pool_Ref_Instr.Pool_Item := Pool_Item;
      Gen_Instr (Pool_Ref_Instr);
   end Gen_Instr;

   ---------------
   -- Gen_Instr --
   ---------------

   procedure Gen_Instr (Op : CIL_Operation; Target : Label_Id) is
      Branch_Instr : JVM.Code.Instruction (Op);

   begin
      Branch_Instr.Target := Target;
      Gen_Instr (Branch_Instr);
      if Target /= Null_Label then
         Set_Is_Targeted (Target);
      end if;
   end Gen_Instr;

   ---------------
   -- Gen_Instr --
   ---------------

   procedure Gen_Instr (Op : CIL_Operation; Subroutine : Subroutine_Id) is
      JSR_Instr : JVM.Code.Instruction (Op);

   begin
      JSR_Instr.Target := Subroutine_Label (Subroutine);
      Gen_Instr (JSR_Instr);
      Set_Is_Targeted (JSR_Instr.Target);
   end Gen_Instr;

   ------------------
   -- Gen_Newarray --
   ------------------

   procedure Gen_Newarray (Elmt_Type : Type_Id) is
      Newarray_Instr : JVM.Code.Instruction (NEWARR);

   begin
      Newarray_Instr.Element_Type := Elmt_Type;
      Newarray_Instr.Dimensions := 1;
      Gen_Instr (Newarray_Instr);
   end Gen_Newarray;

   ------------------------
   -- Gen_Multianewarray --
   ------------------------

   procedure Gen_Multianewarray
     (Arr_CP : Pool_Id; Dimensions : Pos_8; Elmt_Type : Type_Id)
   is
      Multianewarray_Instr : JVM.Code.Instruction (NEWARR);

   begin
      Multianewarray_Instr.Array_Class := Arr_CP;
      Multianewarray_Instr.Dimensions := Dimensions;
      Multianewarray_Instr.Element_Type := Elmt_Type;

      --  Make sure maxstack gets appropriately updated

      Push_Type (String_Type);
      Pop_Type;
      Gen_Instr (Multianewarray_Instr);
   end Gen_Multianewarray;

   ----------
   -- Push --
   ----------

   procedure Push (Operand_Type : Type_Id) is
   begin
      Push (Active_Stack, Operand_Type);
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Count : Stack_Range := 1) is
   begin
      Pop (Active_Stack, Count);
   end Pop;

   ---------------
   -- Check_Top --
   ---------------

   function Check_Top (S : Op_Stack_Id; T : Type_Id) return Boolean is
      Stack_Type  : constant Type_Id       := Top (S);
      T_Kind      : constant JVM_Type_Kind := Type_Kind (T);
      Result      : Boolean;
      Stack_Kind  : JVM_Type_Kind;
      Super_Class : Class_Id;

   begin
      if Stack_Type = Any_Ref_Type then
         return T_Kind in Array_Kind .. Class_Kind;

      --  Relax the matching for Boolean, Byte, Char, and Short

      elsif T_Kind = Int_Kind or else T_Kind = Long_Kind then
         return Type_Kind (Stack_Type) = Int_Kind
           or else Type_Kind (Stack_Type) = Long_Kind;

      elsif T_Kind = Class_Kind then

         if Stack_Type = T then
            return True;

         --  Allow an array to match Java.Lang.Object

         elsif Type_Kind (Stack_Type) = Array_Kind then
            return Class_Of_Type (T) = Java_Lang_Object;

         --  Array descriptors

         elsif Is_Array_Descriptor (Top_Type)
           and then Is_Array_Descriptor (T)
         then
            --  No obvious test for now because types associated with the
            --  descriptors differ. More work needed here???

            --  return Type_Of (First_Field (Class_Of_Type (Stack_Type)))
            --    = Type_Of (First_Field (Class_Of_Type (T)));

            return True;

         --  Allow the top-of-stack class to match any ancestor class

         else
            if Stack_Type = Native_Int_Type then
               Super_Class := Java_Lang_Object;
            elsif Type_Kind (Stack_Type) = Class_Kind then
               Super_Class := Class_Of_Type (Stack_Type);
            else
               return False;
            end if;

            while Super_Class /= Class_Of_Type (T)
              and then Super_Class /= Java_Lang_Object
            loop
               Super_Class := Superclass (Super_Class);
            end loop;

            Result := Super_Class = Class_Of_Type (T);

            if Result then
               Stack_Kind := Type_Kind (Stack_Type);
               pragma Assert (Stack_Kind = Class_Kind
                 or else Stack_Kind = Int_Kind
                 or else Stack_Kind = Long_Kind
                 or else Stack_Kind = Float_Kind
                 or else Stack_Kind = Double_Kind);
               null;
            end if;

            return Result;
         end if;

      elsif T_Kind = Array_Kind then
         return Type_Kind (Stack_Type) = Array_Kind
           and then Dimensions (T) = Dimensions (Stack_Type)
           and then Element_Type (T) = Element_Type (Stack_Type);

      --  Otherwise require exact type match

      else
         return Stack_Type = T;
      end if;
   end Check_Top;

   ---------------
   -- Check_Top --
   ---------------

   function Check_Top (S : Op_Stack_Id; Kind : JVM_Type_Kind) return Boolean is
      Top_Type : constant Type_Id       := Top (S);
      Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Type);

   begin
      if Top_Type = Any_Ref_Type then
         return Top_Kind in Array_Kind .. Class_Kind;
      else
         return Top_Kind = Kind;
      end if;
   end Check_Top;

   ---------------
   -- Check_Top --
   ---------------

   function Check_Top (T : Type_Id) return Boolean is
   begin
      return Check_Top (Active_Stack, T);
   end Check_Top;

   ---------------
   -- Check_Top --
   ---------------

   function Check_Top (Kind : JVM_Type_Kind) return Boolean is
   begin
      return Check_Top (Active_Stack, Kind);
   end Check_Top;

   ----------------
   -- Check_Next --
   ----------------

   function Check_Next (T : Type_Id) return Boolean is
      Stack_Type  : constant Type_Id := Next_To_Top (Active_Stack);
      T_Kind      : constant JVM_Type_Kind := Type_Kind (T);
      Super_Class : Class_Id;

   begin
      if Stack_Type = Any_Ref_Type then
         return T_Kind in Array_Kind .. Class_Kind;

      --  Relax the matching for Boolean, Byte, Char, and Short

      elsif T_Kind = Int_Kind then
         return Check_Top (T_Kind);

      elsif T_Kind = Class_Kind then

         if Stack_Type = T then
            return True;

         --  Allow an array to match Java.Lang.Object

         elsif Type_Kind (Stack_Type) = Array_Kind then
            return Class_Of_Type (T) = Java_Lang_Object;

         --  Otherwise allow the top-of-stack class to match any ancestor class

         else
            Super_Class := Class_Of_Type (Stack_Type);
            while Super_Class /= Class_Of_Type (T)
              and then Super_Class /= Java_Lang_Object
            loop
               Super_Class := Superclass (Super_Class);
            end loop;

            return Super_Class = Class_Of_Type (T);
         end if;

      --  Otherwise require exact type match

      else
         return Stack_Type = T;
      end if;
   end Check_Next;

   ----------------
   -- Check_Next --
   ----------------

   function Check_Next (Kind : JVM_Type_Kind) return Boolean is
   begin
      return Type_Kind (Next_To_Top (Active_Stack)) = Kind;
   end Check_Next;

   -------------------------------------------
   -- Operations for pushing literal values --
   -------------------------------------------

   -------------------
   -- Gen_Push_Null --
   -------------------

   procedure Gen_Push_Null is
   begin
      Gen_Instr (LDNULL);
      Push (Any_Ref_Type);
   end Gen_Push_Null;

   ------------------
   -- Gen_Push_Int --
   ------------------

   procedure Gen_Push_Int (Value : Uint) is
      Int_Value : constant Int := UI_To_Int (Value);
   begin
      if Int_Value in -1 .. 8 then
         case Int_Value is
            when -1 =>     Gen_Instr (LDC_I4_M1);
            when  0 =>     Gen_Instr (LDC_I4_0);
            when  1 =>     Gen_Instr (LDC_I4_1);
            when  2 =>     Gen_Instr (LDC_I4_2);
            when  3 =>     Gen_Instr (LDC_I4_3);
            when  4 =>     Gen_Instr (LDC_I4_4);
            when  5 =>     Gen_Instr (LDC_I4_5);
            when  6 =>     Gen_Instr (LDC_I4_6);
            when  7 =>     Gen_Instr (LDC_I4_7);
            when  8 =>     Gen_Instr (LDC_I4_8);
            when others =>
               pragma Assert (False);
               raise Program_Error;
         end case;

      elsif Int_Value in -128 .. +127 then
         Gen_Instr (LDC_I4_S, Int_16 (Int_Value));

      --  A constant pool entry is required to represent the value

      else
         Gen_Instr (LDC_I4, Integer_Item (Active_Class, Value));
      end if;

      Push (Int_Type);
   end Gen_Push_Int;

   ------------------
   -- Gen_Push_Int --
   ------------------

   procedure Gen_Push_Int (Value : Uint; N : Node_Id) is
   begin
      if UI_Is_In_Int_Range (Value) then
         Gen_Push_Int (Value);
      else
         Error_Msg_N ("capacity exceeded: type too large", N);
         Gen_Push_Int (UI_From_Int (Int'Last));
      end if;
   end Gen_Push_Int;

   -------------------
   -- Gen_Push_Long --
   -------------------

   procedure Gen_Push_Long (Value : Uint) is
   begin
      Gen_Instr (LDC_I8, Long_Item (Active_Class, Value));

      Push (Long_Type);
   end Gen_Push_Long;

   --------------------
   -- Gen_Push_Float --
   --------------------

   procedure Gen_Push_Float (Value : Ureal) is
   begin
      Gen_Instr (LDC_R4, Float_Item (Active_Class, Value));

      Push (Float_Type);
   end Gen_Push_Float;

   ---------------------
   -- Gen_Push_Double --
   ---------------------

   procedure Gen_Push_Double (Value : Ureal) is
   begin
      Gen_Instr (LDC_R8, Double_Item (Active_Class, Value));

      Push (Double_Type);
   end Gen_Push_Double;

   ---------------------------
   -- Gen_Push_String_Const --
   ---------------------------

   procedure Gen_Push_String_Const (Str : String_Const_Id) is
   begin
      pragma Assert (Parent_Class (Pool_Id (Str)) = Active_Class);

      Gen_Instr (LDSTR, Pool_Id (Str));
      Push (String_Type);
   end Gen_Push_String_Const;

   ---------------------------
   -- Gen_Push_String_Const --
   ---------------------------

   procedure Gen_Push_String_Const (S : String) is
   begin
      Gen_Instr (LDSTR, String_Item (Active_Class, Str_Id (S)));
      Push (String_Type);
   end Gen_Push_String_Const;

   ---------------------------------------------------------
   -- Operations for loading and updating local variables --
   ---------------------------------------------------------

   --------------------
   -- Gen_Load_Local --
   --------------------

   procedure Gen_Load_Local (Local : Local_Var_Id) is
      The_Class : Class_Id;
      Address   : Boolean := False;
   begin
      pragma Assert (Method (Local) = Active_Method);

      if Valuetype_Address_Only
        and then Type_Kind (Type_Of (Local)) = Class_Kind
      then
         The_Class := Class_Of_Type (Type_Of (Local));

         if Is_Value_Type (The_Class) then
            Address := True;
         end if;
      end if;

      --  Here's a difference between JVM and CIL instructions
      --  the JVM doesn't distinguish between local vars and
      --  parameters, while CIL does.

      if Address then
         if Is_Param (Local) then
            Gen_Instr (LDARGA, Local);
         else
            Gen_Instr (LDLOCA, Local);
         end if;
      else
         if Is_Param (Local) then
            Gen_Instr (LDARG, Local);
         else
            Gen_Instr (LDLOC, Local);
         end if;
      end if;

      Push (Type_Of (Local));
   end Gen_Load_Local;

   ----------------------------
   -- Gen_Load_Local_Address --
   ----------------------------

   procedure Gen_Load_Local_Address (Local : Local_Var_Id) is
   begin
      pragma Assert (Method (Local) = Active_Method);

      if Is_Param (Local) then
         Gen_Instr (LDARGA, Local);
      else
         Gen_Instr (LDLOCA, Local);
      end if;

      Push (Int_Type);
   end Gen_Load_Local_Address;

   -------------------------
   -- Gen_Store_Valuetype --
   -------------------------

   procedure Gen_Store_Valuetype (T : Type_Id) is
      The_Class : Class_Id;
   begin
      Pop (1);  --  Pop the element value
      pragma Assert
        (Type_Kind (Top (Active_Stack)) = Int_Kind
         and then Is_Value_Type (Class_Of_Type (T)));

      The_Class := Class_Of_Type (T);
      Gen_Instr (STOBJ, Class_Item (The_Class, The_Class));
      Pop (1);
      return;
   end Gen_Store_Valuetype;

   ---------------------
   -- Gen_Store_Local --
   ---------------------

   procedure Gen_Store_Local (Local : Local_Var_Id) is
   begin
      pragma Assert (Method (Local) = Active_Method);

      Gen_Instr (STLOC, Local);
      Pop;
   end Gen_Store_Local;

   -----------------------
   -- Gen_Load_Indirect --
   -----------------------

   procedure Gen_Load_Indirect (T : Type_Id) is
   begin
      pragma Assert (Check_Top (Int_Type));
      Pop;

      case Type_Kind (T) is
         when Byte_Kind =>
            Gen_Instr (LDIND_I1);
         when Char_Kind =>
            Gen_Instr (LDIND_U2);
         when Short_Kind =>
            Gen_Instr (LDIND_I2);
         when Int_Kind =>
            if T = Int_Type then
               Gen_Instr (LDIND_I4);
            elsif T = UInt_Type then
               Gen_Instr (LDIND_U4);
            elsif T = Boolean_Type then
               Gen_Instr (LDIND_I1);
            elsif T = Byte_Type then
               Gen_Instr (LDIND_U1);
            elsif T = SByte_Type then
               Gen_Instr (LDIND_I1);
            elsif T = Short_Type then
               Gen_Instr (LDIND_I2);
            elsif T = Char_Type then
               Gen_Instr (LDIND_U1);
            else
               Gen_Instr (LDIND_I4);
               --  May be a valuetype
            end if;

         when Long_Kind =>
            Gen_Instr (LDIND_I8);
         when Float_Kind =>
            Gen_Instr (LDIND_R4);
         when Double_Kind =>
            Gen_Instr (LDIND_R8);
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Push_Type (T);
   end Gen_Load_Indirect;

   --------------------------------------------------------
   -- Operations for loading and updating array elements --
   --------------------------------------------------------

   ------------------------------------
   -- Gen_Load_Array_Element_Address --
   ------------------------------------

   procedure Gen_Load_Array_Element_Address is
      Elt_Type  : constant Type_Id :=
                    Element_Type (Next_To_Top (Active_Stack));
      The_Class : Class_Id := Java_Lang_Object;

   begin
      pragma Assert
        (Check_Top (JVM.Int_Type) and then Check_Next (Array_Kind));

      if Type_Kind (Elt_Type) = Class_Kind then
         The_Class := Class_Of_Type (Elt_Type);
      end if;

      Gen_Instr (LDELEMA, Type_Item (The_Class, Elt_Type));

      Pop (2);  --  Pop the index and array reference
      Push (Int_Type);  --  Push an int type (address)
   end Gen_Load_Array_Element_Address;

   ----------------------------
   -- Gen_Load_Array_Element --
   ----------------------------

   procedure Gen_Load_Array_Element (Ref_Only : Boolean := False) is
      Elt_Type : constant Type_Id :=
                   Element_Type (Next_To_Top (Active_Stack));

   begin
      pragma Assert
        (Check_Top (JVM.Int_Type) and then Check_Next (Array_Kind));

      case Type_Kind (Elt_Type) is
         when Void_Kind =>
            pragma Assert (False);
            raise Program_Error;

         when Boolean_Kind | Byte_Kind | Char_Kind | Short_Kind =>
            pragma Assert (False);
            raise Program_Error;

         when Int_Kind =>
            if Elt_Type = Int_Type then
               Gen_Instr (LDELEM_I4);
            elsif Elt_Type = UInt_Type then
               Gen_Instr (LDELEM_U4);
            elsif Elt_Type = Boolean_Type then
               Gen_Instr (LDELEM_I1);
            elsif Elt_Type = Byte_Type then
               Gen_Instr (LDELEM_I1);
               Gen_Instr (CONV_U1);     --  Explicitly convert to unsigned
            elsif Elt_Type = SByte_Type then
               Gen_Instr (LDELEM_I1);
            elsif Elt_Type = Short_Type then
               Gen_Instr (LDELEM_I2);
            elsif Elt_Type = Char_Type then
               Gen_Instr (LDELEM_U2);
            else
               pragma Assert (False);
               raise Program_Error;
            end if;

         when Long_Kind =>
            Gen_Instr (LDELEM_I8);

         when Float_Kind =>
            Gen_Instr (LDELEM_R4);

         when Double_Kind =>
            Gen_Instr (LDELEM_R8);

         when Array_Kind =>
            Gen_Instr (LDELEM_REF);

         when Class_Kind =>
            declare
               The_Class : Class_Id;
            begin
               The_Class := Class_Of_Type (Elt_Type);

               if Is_Value_Type (The_Class) then
                  Gen_Instr (LDELEMA, Type_Item (The_Class, Elt_Type));

                  if not Valuetype_Address_Only and not Ref_Only then
                     Gen_Instr (LDOBJ, Class_Item (The_Class, The_Class));
                  end if;
               else
                  Gen_Instr (LDELEM_REF);
               end if;
            end;

         when Return_Addr_Kind =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop (2);  --  Pop the index and array reference
      Push (Elt_Type);
   end Gen_Load_Array_Element;

   -----------------------------------
   -- Gen_Load_Subarray_Reference --
   -----------------------------------

   procedure Gen_Load_Subarray_Reference is
      New_Instr : JVM.Code.Instruction (LDELEM_REF);

   begin
      pragma Assert
        (Check_Top (JVM.Int_Type) and then Check_Next (Array_Kind));

      --  We add the element type to facilitate checking the CIL
      --  code in the verifier.

      New_Instr.Element_Type := Top_Type (1);
      Gen_Instr (New_Instr);

      Pop;  --  Pop the index but leave the array reference type
   end Gen_Load_Subarray_Reference;

   -----------------------------
   -- Gen_Store_Array_Element --
   -----------------------------

   procedure Gen_Store_Array_Element is
      Elt_Type  : Type_Id;
      The_Class : Class_Id;
   begin
      Pop (1);  --  Pop the element value

      --  ??? what we did here is fix Load_Array_Element so that if we're here
      --  with a Ref_Only, we can just do the stobj

      if Type_Kind (Top (Active_Stack)) = Class_Kind
        and then Is_Value_Type (Class_Of_Type (Top (Active_Stack)))
      then
         The_Class := Class_Of_Type (Top (Active_Stack));
         Gen_Instr (STOBJ, Class_Item (The_Class, The_Class));
         Pop (1);
         return;
      end if;

      pragma Assert
        (Check_Top (JVM.Int_Type) and then Check_Next (Array_Kind));

      Elt_Type := Element_Type (Next_To_Top (Active_Stack));

      case Type_Kind (Elt_Type) is
         when Void_Kind =>
            pragma Assert (False);
            raise Program_Error;

         when Boolean_Kind | Byte_Kind | Char_Kind | Short_Kind =>
            pragma Assert (False);
            raise Program_Error;

         when Int_Kind =>
            if Elt_Type = Int_Type then
               Gen_Instr (STELEM_I4);
            elsif Elt_Type = UInt_Type then
               Gen_Instr (STELEM_I4);
            elsif Elt_Type = Boolean_Type
              or else Elt_Type = Byte_Type
              or else Elt_Type = SByte_Type
            then
               Gen_Instr (STELEM_I1);
            elsif Elt_Type = Short_Type then
               Gen_Instr (STELEM_I2);
            elsif Elt_Type = Char_Type then
               Gen_Instr (STELEM_I2);
            else
               pragma Assert (False);
               raise Program_Error;
            end if;

         when Long_Kind =>
            Gen_Instr (STELEM_I8);

         when Float_Kind =>
            Gen_Instr (STELEM_R4);

         when Double_Kind =>
            Gen_Instr (STELEM_R8);

         when Array_Kind =>
            Gen_Instr (STELEM_REF);

         when Class_Kind =>
            declare
               The_Class : Class_Id;
            begin
               The_Class := Class_Of_Type (Elt_Type);

               if Is_Value_Type (The_Class) then
                  Gen_Instr (STOBJ, Class_Item (The_Class, The_Class));
               else
                  Gen_Instr (STELEM_REF);
               end if;
            end;

         when Return_Addr_Kind =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop (2);  --  Pop the index and array reference
   end Gen_Store_Array_Element;

   --------------------------------------------------------
   -- Operations for accessing and updating class fields --
   --------------------------------------------------------

   -------------------
   -- Gen_Get_Field --
   -------------------

   procedure Gen_Get_Field (Field : Field_Id) is
   begin
      if Is_Static (Field) then
         Gen_Get_Static_Field (Field);
      else
         Gen_Get_Object_Field (Field);
      end if;
   end Gen_Get_Field;

   -------------------
   -- Gen_Put_Field --
   -------------------

   procedure Gen_Put_Field (Field : Field_Id) is
   begin
      if Is_Static (Field) then
         Gen_Put_Static_Field (Field);
      else
         Gen_Put_Object_Field (Field);
      end if;
   end Gen_Put_Field;

   --------------------------
   -- Gen_Get_Static_Field --
   --------------------------

   procedure Gen_Get_Static_Field (Field : Field_Id) is
      The_Class : Class_Id;
      Address   : Boolean := False;
   begin
      if Valuetype_Address_Only
        and then Type_Kind (Type_Of (Field)) = Class_Kind
      then
         The_Class := Class_Of_Type (Type_Of (Field));

         if Is_Value_Type (The_Class) then
            Address := True;
         end if;
      end if;

      pragma Assert (Is_Static (Field));

      if Address then
         Gen_Instr (LDSFLDA, Field, Class (Field));
      else
         Gen_Instr (LDSFLD, Field, Class (Field));
      end if;

      Push (Type_Of (Field));
   end Gen_Get_Static_Field;

   --------------------------
   -- Gen_Put_Static_Field --
   --------------------------

   procedure Gen_Put_Static_Field (Field : Field_Id) is
   begin
      pragma Assert (Is_Static (Field));

      Gen_Instr (STSFLD, Field, Class (Field));
      Pop;  --  Pop the field
   end Gen_Put_Static_Field;

   --------------------------
   -- Gen_Get_Object_Field --
   --------------------------

   procedure Gen_Get_Object_Field (Field : Field_Id) is
      The_Class : Class_Id;
      Address   : Boolean := False;
   begin
      if Valuetype_Address_Only
        and then Type_Kind (Type_Of (Field)) = Class_Kind
      then
         The_Class := Class_Of_Type (Type_Of (Field));

         if The_Class /= Null_Class
           and then Superclass (The_Class) /= Null_Class
           and then Is_Value_Type (The_Class)
         then
            Address := True;
         end if;
      end if;

      pragma Assert (not Is_Static (Field));

      if Address then
         Gen_Instr (LDFLDA, Field, Class (Field));
      else
         Gen_Instr (LDFLD, Field, Class (Field));
      end if;
      Pop;  --  Pop the instance reference
      Push (Type_Of (Field));
   end Gen_Get_Object_Field;

   ----------------------------------
   -- Gen_Get_Object_Field_Address --
   ----------------------------------

   procedure Gen_Get_Object_Field_Address (Field : Field_Id) is
   begin
      pragma Assert (not Is_Static (Field));
      pragma Assert (Check_Top (Type_Of (Class (Field))));

      Gen_Instr (LDFLDA, Field, Class (Field));
      Pop;  --  Pop the instance reference
      Push (Int_Type);
   end Gen_Get_Object_Field_Address;

   --------------------------
   -- Gen_Put_Object_Field --
   --------------------------

   procedure Gen_Put_Object_Field (Field : Field_Id) is
      Field_Typ : constant Type_Id := Type_Of (Field);
      Class_Typ : constant Type_Id := Type_Of (Class (Field));

   begin
      pragma Assert (not Is_Static (Field));

      --  Temporarily disable these assertions
      pragma Assert (True or else Check_Top (Field_Typ));
      pragma Assert (True or else Check_Next (Class_Typ));

      Gen_Instr (STFLD, Field, Class (Field));
      Pop (2);  --  Pop the field and the instance reference
   end Gen_Put_Object_Field;

   ------------------------------------------------
   -- Operations for creating objects and arrays --
   ------------------------------------------------

   --------------------
   -- Gen_New_Object --
   --------------------

   procedure Gen_New_Object (Class : Class_Id) is
      pragma Unreferenced (Class);
   begin
      pragma Assert (False);
      raise Program_Error;
   end Gen_New_Object;

   --------------------
   -- Gen_New_Object --
   --------------------

   procedure Gen_New_Object (Class : Class_Id; Method : Method_Id) is
      Param : Local_Var_Id := First_Local_Var (Method);

   begin
      pragma Assert (not Is_Abstract (Class) and not Is_Interface (Class));
      pragma Assert (not Is_Static (Method));

      --  NOTE: Ideally we would check that the type of each
      --  operand on the stack matches the corresponding formal
      --  type, but that would mean traversing the formals in
      --  reverse order, which is a bit of a pain...

      --  This initial push is a bit cheesy, but the constructor
      --  method "expects" this object type
      Push (Type_Of (Class));

      while Param /= Null_Local_Var and then Is_Param (Param) loop
         Pop;
         Param := Next_Local_Var (Param);
      end loop;

      Gen_Instr (NEWOBJ, New_Method_Ref (Method));

      Push (Type_Of (Class));
   end Gen_New_Object;

   ------------------------
   -- Gen_Default_Object --
   ------------------------

   procedure Gen_Default_Object (Class : Class_Id) is
   begin
      Gen_New_Object (Class, Default_Constructor (Class));
   end Gen_Default_Object;

   -------------------
   -- Gen_New_Array --
   -------------------

   procedure Gen_New_Array
     (Array_Type : Entity_Id;
      Object     : Entity_Id := Empty)
   is
      Array_JType : constant Type_Id := JVM_Type (Array_Type);
      Elmt_Type   : constant Type_Id := Element_Type (Array_JType);

   begin
      pragma Assert (Check_Top (Int_Type));

      if Dimensions (Array_JType) = 1 then
         Gen_Newarray (Elmt_Type);

      --  Generate an allocation of a multidimensional array
      --  NOTE: Not yet implemented (use Gen_New_Multiarray).

      else
         if Present (Object) then
            Error_Msg_N
              ("allocation of multidimensional array not supported", Object);
         else
            Error_Msg_N
              ("allocation of multidimensional array not supported",
               Array_Type);
         end if;
      end if;

      --  Pop the array length value(s)

      Pop (Stack_Range (Dimensions (Array_JType)));
      Push (Array_JType);
   end Gen_New_Array;

   ------------------------
   -- Gen_New_Multiarray --
   ------------------------

   procedure Gen_New_Multiarray (Array_Type : Type_Id) is
      Dimension_Count : Pos_8;
      Elt_Type        : Type_Id;

   begin
      pragma Assert
        (Check_Top (Int_Type) and then Dimensions (Array_Type) > 1);

      Dimension_Count := Dimensions (Array_Type);
      Elt_Type        := Element_Type (Array_Type);

      Gen_Multianewarray
        (New_Array_Ref (Array_Type),
         Dimension_Count, Elt_Type);

      --  Pop the array length value(s)

      Pop (Stack_Range (Dimension_Count));
      Push (Array_Type);
   end Gen_New_Multiarray;

   ----------------------
   -- Gen_Array_Length --
   ----------------------

   procedure Gen_Array_Length is
   begin
      pragma Assert (Check_Top (Array_Kind));

      Gen_Instr (LDLEN);

      Pop;  --  Pop the array reference
      Push (Int_Type);
   end Gen_Array_Length;

   ---------------------------
   -- Arithmetic operations --
   ---------------------------

   -------------
   -- Gen_Add --
   -------------

   procedure Gen_Add
     (Modular        : Boolean;
      Integer_Type   : Boolean;
      Overflow_Check : Boolean := False) is
   begin
      pragma Assert (Check_Top (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind | Float_Kind | Double_Kind =>
            if Generate_Overflow_Check
                 (Modular, Integer_Type, Overflow_Check)
            then
               Gen_Instr (ADD_OVF);
            else
               Gen_Instr (ADD);
            end if;
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop;  --  Pop the top-of-stack type (leaving the second operand type)
   end Gen_Add;

   -------------
   -- Gen_Sub --
   -------------

   procedure Gen_Sub
     (Modular        : Boolean;
      Integer_Type   : Boolean;
      Overflow_Check : Boolean := False) is
   begin
      pragma Assert (Check_Top (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind | Float_Kind | Double_Kind =>
            if Generate_Overflow_Check
                 (Modular, Integer_Type, Overflow_Check)
            then
               Gen_Instr (SUB_OVF);
            else
               Gen_Instr (SUB);
            end if;
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop;  --  Pop the top-of-stack type (leaving the second operand type)
   end Gen_Sub;

   -------------
   -- Gen_Mul --
   -------------

   procedure Gen_Mul
     (Modular        : Boolean;
      Integer_Type   : Boolean;
      Overflow_Check : Boolean := False) is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind | Float_Kind | Double_Kind =>
            --  mul.ovf only works on integer values.
            if Generate_Overflow_Check
                 (Modular, Integer_Type, Overflow_Check)
            then
               Gen_Instr (MUL_OVF);
            else
               Gen_Instr (MUL);
            end if;
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop;  --  Pop the top-of-stack type (leaving the second operand type)
   end Gen_Mul;

   -------------
   -- Gen_Div --
   -------------

   procedure Gen_Div is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                       Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind | Float_Kind | Double_Kind =>
            if Next_To_Top (Active_Stack) = UInt_Type
              or Next_To_Top (Active_Stack) = ULong_Type
            then
               Gen_Instr (DIV_UN);
            else
               Gen_Instr (DIV);
            end if;
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop;  --  Pop the top-of-stack type (leaving the second operand type)
   end Gen_Div;

   -------------
   -- Gen_Rem --
   -------------

   procedure Gen_Rem is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind | Float_Kind | Double_Kind =>
            if Next_To_Top (Active_Stack) = UInt_Type
              or else Next_To_Top (Active_Stack) = ULong_Type
            then
               Gen_Instr (REM_UN);
            else
               Gen_Instr (REM_k);
            end if;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop;  --  Pop the top of stack (use the first operand type as the result)
   end Gen_Rem;

   -------------
   -- Gen_Neg --
   -------------

   procedure Gen_Neg is
   begin
      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind | Float_Kind | Double_Kind =>
            Gen_Instr (NEG);
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Gen_Neg;

   --------------------
   -- Gen_Incr_Local --
   --------------------

   procedure Gen_Incr_Local (Local : Local_Var_Id; Value : Uint) is
   begin
      Gen_Load_Local (Local);
      Gen_Push_Int (Value);
      Gen_Conversion (Type_Of (Local));
      Gen_Add (True, True);
      Gen_Store_Local (Local);
   end Gen_Incr_Local;

   ----------------------------------
   -- Logical and shift operations --
   ----------------------------------

   -------------
   -- Gen_And --
   -------------

   procedure Gen_And is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind =>
            Gen_Instr (and_k);
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop;  --  Pop the top of stack (use the first operand type as the result)
   end Gen_And;

   ------------
   -- Gen_Or --
   ------------

   procedure Gen_Or is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind =>
            Gen_Instr (or_k);
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop;  --  Pop the top of stack (use the first operand type as the result)
   end Gen_Or;

   -------------
   -- Gen_Xor --
   -------------

   procedure Gen_Xor is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind =>
            Gen_Instr (xor_k);
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop;  --  Pop the top of stack (use the first operand type as the result)
   end Gen_Xor;

   -------------
   -- Gen_Not --
   -------------

   procedure Gen_Not is
   begin
      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (LDC_I4_M1);
            Push (Int_Type);
            Gen_Instr (xor_k);
            Pop;

         when Long_Kind =>
            Gen_Instr (LDC_I4_M1);
            Push (Int_Type);
            Gen_Conversion (Long_Type);
            Gen_Instr (xor_k);
            Pop;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Gen_Not;

   --------------------
   -- Gen_Shift_Left --
   --------------------

   procedure Gen_Shift_Left (Size : Uint) is
   begin
      Pop;  --  Pop the top of stack (use the first operand type as the result)

      --  If the nominal size of the value is less than the size of
      --  its containing type, then we have to mask of the high bits
      --  off the left-shifted value.

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (SHL);

            if Size < 32 then
               Gen_Push_Int (Uint_2 ** Size - 1);
               Gen_And;
            end if;

         when Long_Kind =>
            Gen_Instr (SHL);

            if Size < 64 then
               Gen_Push_Int (Uint_2 ** Size - 1);
               Gen_And;
            end if;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Gen_Shift_Left;

   --------------------------------
   -- Gen_Shift_Right_Arithmetic --
   --------------------------------

   procedure Gen_Shift_Right_Arithmetic (Typ : Entity_Id) is
      Size : constant Uint := RM_Size (Typ);

      procedure Unsigned_Shift;
      --  Routine that implements ARM B.2(9): Shift_Right_Arithmetic must shift
      --  1 values in if Value is at least half the modulus. Needed because the
      --  CIL instruction only shifts in zero bits.

      procedure Unsigned_Shift is
         LV_Amount   : constant Local_Var_Id :=
                         New_Local_Var ("_c", Top_Type);
         Ge_Label    : constant Label_Id := New_Label;
         Exit_Label  : constant Label_Id := New_Label;
         Check_State : Boolean;

      begin
         --  Stack contents: Top-1 = Value
         --                  Top   = Amount
         --  Generate:
         --         LV_Amount : Amount'Type := Amount;
         --
         --         if LV_Amount /= 0 then
         --            if Value < (Modulus (Typ) / 2) then
         --               SHR (Value, LV_Amount);
         --            else
         --               loop
         --                  SHR (Value, 1);
         --                  OR  (Value, 16#80000000#);
         --                  LV_Amount := LV_Amount - 1;
         --                  exit when LV_Amount = 0;
         --               end loop;
         --            end if;
         --         end if;

         Suppress_Stack_Checking (Check_State);

         Gen_Store_Local (LV_Amount);

         Gen_Load_Local  (LV_Amount);
         Gen_Branch_Equal (Exit_Label);

         Gen_Duplicate;
         Gen_Push_Long (Modulus (Typ) / 2);

         if Size = 32 then
            Convert_To_UInteger (False);
         else pragma Assert (Size = 64);
            Convert_To_ULong (False);
         end if;

         Gen_Compare_Branch_Greater_Equal (Ge_Label);

         Gen_Load_Local (LV_Amount);
         Gen_Instr (SHR_UN);
         Pop_Type;

         Gen_Goto (Exit_Label);
         Gen_Label (Ge_Label);

         Gen_Push_Int (Uint_1);
         Gen_Instr (SHR_UN);
         Pop_Type;

         Gen_Push_Int (UI_From_Int (Int'First));

         if Size = 32 then
            Convert_To_UInteger (False);
         else pragma Assert (Size = 64);
            Convert_To_ULong (False);
         end if;

         Gen_Or;

         Gen_Load_Local (LV_Amount);
         Gen_Push_Int (Uint_1);
         Gen_Sub (False, True);
         Gen_Duplicate;
         Gen_Store_Local (LV_Amount);

         Gen_Branch_Not_Equal (Ge_Label);

         Gen_Label (Exit_Label);
         Restore_Stack_Checking (Check_State);
      end Unsigned_Shift;

   --  Start of processing for Gen_Shift_Right_Arithmetic

   begin
      --  For sizes less than the JVM type size, it's necessary
      --  to shift the low-order bits to the leftmost end of the
      --  integer and then shift back, followed by a mask, to ensure
      --  proper sign extension.

      case Type_Kind (Top (Active_Stack, 1)) is
         when Int_Kind =>
            if Size = 32 then
               if Top (Active_Stack, 1) /= UInt_Type then
                  Pop;  --  Use the first operand type as the result
                  Gen_Instr (SHR);
               else
                  Unsigned_Shift;
               end if;

            else
               pragma Assert (Size < 32);

               --  Compute the bit count for the right shift. We make
               --  this one less than it should be, because of the
               --  edge case where the shift is the full size of the
               --  value and we need sign extension. The JVM will treat
               --  that case as a zero bit shift, so unfortunately
               --  we have to compensate by shifting two times (we
               --  shift the remaining bit at the end before masking).

               Gen_Push_Int (32 - Size - 1);
               Gen_Add (True, True);
               Gen_Swap;

               --  Shift the low-order Size bits to the high-order
               --  end of the integer.

               Gen_Push_Int (32 - Size);
               Gen_Instr (SHL);
               Pop (2);  --  Pop the shift operands
               Push (Int_Type); --  Push the shift result: an int32 value

               --  Now shift right to get sign extension

               Gen_Swap;
               Gen_Instr (SHR);
               Pop;  --  Pop the shift count

               --  Shift the final bit (see explanation above)

               Gen_Push_Int (Uint_1);
               Gen_Instr (SHR);
               Pop;  --  Pop the shift count

               --  Finally mask off the high order bits to get the result

               Gen_Push_Int (Uint_2 ** Size - 1);
               Gen_And;
            end if;

         when Long_Kind =>
            if Size = 64 then
               if Top (Active_Stack) /= ULong_Type then
                  Pop;  --  Use the first operand type as the result
                  Gen_Instr (SHR);
               else
                  Unsigned_Shift;
               end if;

            else
               pragma Assert (Size < 64);

               --  Compute the bit count for the right shift. We make
               --  this one less than it should be, because of the
               --  edge case where the shift is the full size of the
               --  value and we need sign extension. The JVM will treat
               --  that case as a zero bit shift, so unfortunately
               --  we have to compensate by shifting two times (we
               --  shift the remaining bit at the end before masking).

               Gen_Push_Int (64 - Size - 1);
               Gen_Add (True, True);
               Gen_Swap;

               --  Shift the low-order Size bits to the high-order
               --  end of the integer.

               Gen_Push_Int (64 - Size);
               Gen_Instr (SHL);
               Pop;  --  Pop the shift count

               --  Now shift right to get sign extension

               Gen_Swap;
               Gen_Instr (SHR);
               Pop;  --  Pop the shift count

               --  Shift the final bit (see explanation above)

               Gen_Push_Int (Uint_1);
               Gen_Instr (SHR);
               Pop;  --  Pop the shift count

               --  Finally mask off the high order bits to get the result

               Gen_Push_Int (Uint_2 ** Size - 1);
               Gen_And;
            end if;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Gen_Shift_Right_Arithmetic;

   -----------------------------
   -- Gen_Shift_Right_Logical --
   -----------------------------

   procedure Gen_Shift_Right_Logical is
   begin
      Pop;  --  Pop the top of stack (use the first operand type as the result)

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (SHR_UN);
         when Long_Kind =>
            Gen_Instr (SHR_UN);
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Gen_Shift_Right_Logical;

   ---------------------
   -- Gen_Rotate_Left --
   ---------------------

   procedure Gen_Rotate_Left (Size : Uint) is
      Count_Tmp : constant Local_Var_Id :=
                    New_Local_Var ("_rotcnt", Int_Type);
      Prim_Type : constant JVM_Type_Kind :=
                    Type_Kind (Next_To_Top (Active_Stack));

   begin
      --  If Size is less than the primitive type size, then
      --  we need to normalize the shift count value to the
      --  range 0 .. Size - 1, in case it's bigger than Size
      --  (which would otherwise lead to incorrect rotation
      --  results).

      if (Prim_Type = Int_Kind and then Size < 32)
        or else (Prim_Type = Long_Kind and then Size < 64)
      then
         Gen_Push_Int (Size);
         Gen_Rem;
      end if;

      --  Save the normalized count and push an extra copy of the operand value

      Gen_Store_Local (Count_Tmp);
      Gen_Duplicate;

      --  Left shift the right portion of the value into position
      --  (Gen_Shift_Left will take care of any needed masking).

      Gen_Load_Local (Count_Tmp);
      Gen_Shift_Left (Size);

      --  Swap the left-shifted value with the original value and
      --  push the shift count.

      Gen_Swap;
      Gen_Load_Local (Count_Tmp);

      --  Compute the proper right shift (size of value minus shift count)

      Gen_Push_Int (Size);
      Gen_Sub (True, True);
      Gen_Neg;

      --  Do the right shift and combine with the left-shifted portion

      Gen_Shift_Right_Logical;
      Gen_Or;
   end Gen_Rotate_Left;

   ----------------------
   -- Gen_Rotate_Right --
   ----------------------

   procedure Gen_Rotate_Right (Size : Uint) is
      Count_Tmp : constant Local_Var_Id :=
                    New_Local_Var ("_rotcnt", Int_Type);
      Prim_Type : constant JVM_Type_Kind :=
                    Type_Kind (Next_To_Top (Active_Stack));

   begin
      --  If Size is less than the primitive type size, then
      --  we need to normalize the shift count value to the
      --  range 0 .. Size - 1, in case it's bigger than Size
      --  (which would otherwise lead to incorrect rotation
      --  results).

      if (Prim_Type = Int_Kind and then Size < 32)
        or else (Prim_Type = Long_Kind and then Size < 64)
      then
         Gen_Push_Int (Size);
         Gen_Rem;
      end if;

      --  Save the normalized count and push an extra copy of the operand value

      Gen_Store_Local (Count_Tmp);
      Gen_Duplicate;

      --  Right shift the left portion of the value into position

      Gen_Load_Local (Count_Tmp);
      Gen_Shift_Right_Logical;

      --  Swap the right-shifted value with the original value and
      --  push the shift count.

      Gen_Swap;
      Gen_Load_Local (Count_Tmp);

      --  Compute the proper left shift (size of value minus shift count)

      Gen_Push_Int (Size);
      Gen_Sub (True, True);
      Gen_Neg;

      --  Do the left shift and combine with the right-shifted portion
      --  (Gen_Shift_Left will take care of any needed masking).

      Gen_Shift_Left (Size);
      Gen_Or;
   end Gen_Rotate_Right;

   -----------------------------------
   -- Numeric conversion operations --
   -----------------------------------

   procedure Round_Float is
   begin
      case Type_Kind (Top (Active_Stack)) is
         when Float_Kind =>
            Gen_Instr (CONV_R8);
            Gen_Instr (RND_R8);
         when Double_Kind =>
            Gen_Instr (RND_R8);
         when others =>
            null;
      end case;
      Pop;
      Push (Double_Type);
   end Round_Float;

   ------------------------
   -- Convert_To_Integer --
   ------------------------

   procedure Convert_To_Integer (Rounded : Boolean) is
   begin
      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            if Top (Active_Stack) = UInt_Type then
               Gen_Instr (CONV_I4);
            else
               null;  --  Already long, nothing to do
            end if;

         when Long_Kind =>
            Gen_Instr (CONV_I4);

         when Float_Kind | Double_Kind =>
            if Rounded then
               Round_Float;
            end if;

            Gen_Instr (CONV_I4);

         when Class_Kind =>
            pragma Assert (Top (Active_Stack) = Native_Int_Type);
            null;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop;
      Push (Int_Type);
   end Convert_To_Integer;

   -------------------------
   -- Convert_To_UInteger --
   -------------------------

   procedure Convert_To_UInteger (Rounded : Boolean) is
   begin
      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            if Top (Active_Stack) = UInt_Type then
               null;  --  Already long, nothing to do
            else
               Gen_Instr (CONV_U4);
            end if;

         when Long_Kind =>
            Gen_Instr (CONV_U4);

         when Float_Kind | Double_Kind =>
            if Rounded then
               Round_Float;
            end if;
            Gen_Instr (CONV_U4);

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop;
      Push (UInt_Type);
   end Convert_To_UInteger;

   ---------------------
   -- Convert_To_Long --
   ---------------------

   procedure Convert_To_Long (Rounded : Boolean) is
   begin
      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (CONV_I8);

         when Float_Kind | Double_Kind =>
            if Rounded then
               Round_Float;
            end if;
            Gen_Instr (CONV_I8);

         when Long_Kind =>
            if Top (Active_Stack) = ULong_Type then
               Gen_Instr (CONV_I8);
            else
               null;  --  Already long, nothing to do
            end if;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop;
      Push (Long_Type);
   end Convert_To_Long;

   ----------------------
   -- Convert_To_ULong --
   ----------------------

   procedure Convert_To_ULong (Rounded : Boolean) is
   begin
      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (CONV_U8);

         when Float_Kind | Double_Kind =>
            if Rounded then
               Round_Float;
            end if;
            Gen_Instr (CONV_U8);

         when Long_Kind =>
            if Top (Active_Stack) = ULong_Type then
               null;  --  Already long, nothing to do
            else
               Gen_Instr (CONV_U8);
            end if;
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop;
      Push (ULong_Type);
   end Convert_To_ULong;

   ----------------------
   -- Convert_To_Float --
   ----------------------

   procedure Convert_To_Float is
   begin
      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind | Double_Kind =>
            if Top (Active_Stack) = UInt_Type
              or else Top (Active_Stack) = ULong_Type
            then
               Gen_Instr (CONV_R_UN);
            end if;

            Gen_Instr (CONV_R4);

         when Float_Kind =>
            null;  --  Already float, nothing to do

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop;
      Push (Float_Type);
   end Convert_To_Float;

   -----------------------
   -- Convert_To_Double --
   -----------------------

   procedure Convert_To_Double is
   begin
      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind | Float_Kind =>
            if Top (Active_Stack) = UInt_Type
              or else Top (Active_Stack) = ULong_Type
            then
               Gen_Instr (CONV_R_UN);
            end if;

            Gen_Instr (CONV_R8);

         when Double_Kind =>
            null;  --  Already double, nothing to do (or raise error???)

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Pop;
      Push (Double_Type);
   end Convert_To_Double;

   ------------------------------
   -- Gen_Unchecked_Conversion --
   ------------------------------

   procedure Gen_Class_Conversion (Target_Type : Type_Id) is
      Top_Kind     : constant JVM_Type_Kind := JVM.Type_Kind (Top_Type);
      pragma Assert (Top_Kind = Class_Kind);
      Stack_Class  : constant Class_Id := Class_Of_Type (Top (Active_Stack));
      Target_Class : constant Class_Id := Class_Of_Type (Target_Type);

   begin
      --  No code emitted if the target type is an ancestor of the current type

      if Is_Ancestor (Stack_Class, Target_Class) then
         Pop;
         Push (Target_Type);
      else
         --  Push a dummy type to account for the "ldtoken" instruction
         --  generated by UNCHECKED_CONVERSION
         Push (Void_Type);
         Pop;
         Gen_Instr
           (CLASS_CONVERSION, New_Class_Ref (Class_Of_Type (Target_Type)));
      end if;
   end Gen_Class_Conversion;

   -------------
   -- Gen_Box --
   -------------

   procedure Gen_Box (Target_Type : Type_Id) is
   begin
      --  Push a dummy type to account for the "ldtoken" instruction
      --  generated by UNCHECKED_CONVERSION
      Push (Void_Type);
      Pop;
      Gen_Instr
        (BOX, New_Class_Ref (Class_Of_Type (Target_Type)));
   end Gen_Box;

   ---------------
   -- Gen_Unbox --
   ---------------

   procedure Gen_Unbox (Target_Type : Type_Id) is
   begin
      --  Push a dummy type to account for the "ldtoken" instruction
      --  generated by UNCHECKED_CONVERSION
      Push (Void_Type);
      Pop;
      Gen_Instr
        (UNBOX, New_Class_Ref (Class_Of_Type (Target_Type)));
   end Gen_Unbox;

   --------------------
   -- Gen_Conversion --
   --------------------

   procedure Gen_Conversion
     (Target_Type : Type_Id;
      Round       : Boolean := False)
   is
      Target_Class : Class_Id;
      Stack_Class  : Class_Id;

   begin
      case Type_Kind (Target_Type) is
         when Int_Kind | Boolean_Kind | Byte_Kind | Char_Kind | Short_Kind =>
            if Target_Type /= UInt_Type then
               Convert_To_Integer (Round);
            else
               Convert_To_UInteger (Round);
            end if;

         when Long_Kind =>
            if Target_Type /= ULong_Type then
               Convert_To_Long (Round);
            else
               Convert_To_ULong (Round);
            end if;

         when Float_Kind =>
            Convert_To_Float;

         when Double_Kind =>
            Convert_To_Double;

         when Array_Kind =>
            Gen_Check_Cast (Target_Type);

         when Class_Kind =>
            if not (Is_Reference_Type (Top (Active_Stack))
                     or else Top (Active_Stack) = Int_Type
                     or else Top (Active_Stack) = UInt_Type
                     or else Top (Active_Stack) = Native_Int_Type)
            then
               pragma Assert (False);
               raise Program_Error;

            --  Support 'Addresses' convertions.
            elsif Top (Active_Stack) = Native_Int_Type
              or else Top (Active_Stack) = UInt_Type
              or else Top (Active_Stack) = Int_Type
            then
               --  Do nothing here, this kind of conversion should always be
               --  done with more context than we have here.
               null;

            --  Arrays convertions: no action required
            elsif Type_Kind (Top (Active_Stack)) = Array_Kind then
               pragma Assert (Class_Of_Type (Target_Type) = Java_Lang_Object);

               null;

            --  Regular Class case:
            elsif Top (Active_Stack) /= Target_Type
              and then Top (Active_Stack) /= Any_Ref_Type
            then
               Stack_Class  := Class_Of_Type (Top (Active_Stack));
               Target_Class := Class_Of_Type (Target_Type);

               --  No action done for now with array descriptors. More work
               --  needed here???

               if Is_Array_Descriptor (Top (Active_Stack))
                 and then Is_Array_Descriptor (Target_Type)
               then
                  null;

               --  Interface conversions are handled by the VM machine. No
               --  extra code generation is required.

               elsif Is_Interface (Stack_Class)
                 or else Is_Interface (Target_Class)
               then
                  null;

               --  Check if Target_Class is an ancestor of top-of-the-task
               --  type's class.

               --  If Target_Type's class is not an ancestor of
               --  the top-of-stack type's class, then this must
               --  a downward conversion, so apply a checkcast.
               --  Else, we don't need to do anything.

               elsif not Is_Ancestor (Stack_Class, Target_Class) then
                  --  If Target_Type's class is a regular class, we verify that
                  --  top-of-stack type's class is an ancestor of Target_Type's
                  --  class. Throw an assert failure if not.

                  pragma Assert (Is_Ancestor (Target_Class, Stack_Class));

                  Gen_Instr
                    (CASTCLASS, New_Class_Ref (Target_Class));
               end if;

               Pop;
               Push (Target_Type);
            end if;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Gen_Conversion;

   ---------------------------------
   -- Branch and label generation --
   ---------------------------------

   --  procedure Gen_Compare_Vs_Zero (Jtype : JVM_Type_Kind);
   --  Generates code to compare a numeric value against zero.
   --  If Jtype is Int_Kind, then does nothing. Intended for
   --  use by Gen_Branch_* routines to handle conditional
   --  branches for Long, Float, and Double values.

   ---------------
   -- New_Label --
   ---------------

   function New_Label return Label_Id is
      Label : constant Label_Id := JVM.Info.New_Label;

   begin
      Set_Method      (Label, Active_Method);
      Set_Location    (Label, Null_Instr);
      Set_Is_Targeted (Label, False);

      Add_Label (Active_Method, Label);

      return Label;
   end New_Label;

   ---------------
   -- Gen_Label --
   ---------------

   procedure Gen_Label
     (Label       : Label_Id;
      Line_Number : Source_Ptr := No_Location)
   is
      Label_Instr : JVM.Code.Instruction (NOP);
      Instr       : JVM.Code.Instruction;
      M           : constant Method_Id := Method (Label);

   begin
      pragma Assert (M = Active_Method);
      pragma Assert
        (Method (Label) = Active_Method
         and then Location (Label) = Null_Instr
         and then (not Stack_Checking or else Is_Empty (Active_Stack)));

      --  If we want to create a new label for a line number, and if the
      --  previous instruction was already a label associated with a line
      --  number, just replace the line number

      if Last (Active_Seq) /= Null_Instr
        and then Line_Number /= No_Location
        and then Get (Last (Active_Seq)).Op = NOP
        and then Get (Last (Active_Seq)).Line_Number /= No_Location
      then
         Instr := Get (Last (Active_Seq));
         Instr.Line_Number := Line_Number;
         Put (Last (Active_Seq), Instr);

      --  Else create a new label

      else

         Label_Instr.Label_Def := Label;
         Label_Instr.Line_Number := Line_Number;
         Gen_Instr (Label_Instr);
         Set_Location (Label, Last (Active_Seq));
      end if;

      --  Facilitate location of CIL statements with the debugger

      Instruction_Breakpoint (Active_Method, Count_Sequence (Active_Seq));
   end Gen_Label;

   --------------
   -- Gen_Zero --
   --------------

   procedure Gen_Zero (Jtype : JVM_Type_Kind) is
   begin
      case Jtype is
         when Int_Kind =>
            Gen_Push_Int (Uint_0);
         when Long_Kind =>
            Gen_Push_Long (Uint_0);
         when Float_Kind =>
            Gen_Push_Float (Ureal_0);
         when Double_Kind =>
            Gen_Push_Double (Ureal_0);
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Gen_Zero;

   ----------------------
   -- Gen_Branch_Equal --
   ----------------------

   procedure Gen_Branch_Equal (Label : Label_Id) is
   begin
      Gen_Zero (Type_Kind (Top (Active_Stack)));

      Gen_Instr (BEQ, Label);
      Pop (2);

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_Equal;

   --------------------------
   -- Gen_Branch_Not_Equal --
   --------------------------

   procedure Gen_Branch_Not_Equal (Label : Label_Id) is
   begin
      Gen_Zero (Type_Kind (Top (Active_Stack)));

      Gen_Instr (BNE_UN, Label);
      Pop (2);

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_Not_Equal;

   ---------------------
   -- Gen_Branch_Less --
   ---------------------

   procedure Gen_Branch_Less (Label : Label_Id) is
   begin
      Gen_Zero (Type_Kind (Top (Active_Stack)));

      if Next_To_Top (Active_Stack) = UInt_Type or
         Next_To_Top (Active_Stack) = ULong_Type then
         Gen_Instr (BLT_UN, Label);
      else
         Gen_Instr (BLT, Label);
      end if;

      Pop (2);

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_Less;

   ---------------------------
   -- Gen_Branch_Less_Equal --
   ---------------------------

   procedure Gen_Branch_Less_Equal (Label : Label_Id) is
   begin
      Gen_Zero (Type_Kind (Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind =>
            if Next_To_Top (Active_Stack) = UInt_Type or
               Next_To_Top (Active_Stack) = ULong_Type then
               Gen_Instr (BLE_UN, Label);
            else
               Gen_Instr (BLE, Label);
            end if;

         when Float_Kind | Double_Kind =>
            Gen_Instr (BLE_UN, Label);

         when others =>
            raise Incompatible_Types;
      end case;

      Pop (2);

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_Less_Equal;

   ------------------------
   -- Gen_Branch_Greater --
   ------------------------

   procedure Gen_Branch_Greater (Label : Label_Id) is
   begin
      Gen_Zero (Type_Kind (Top (Active_Stack)));

      if Next_To_Top (Active_Stack) = UInt_Type or
         Next_To_Top (Active_Stack) = ULong_Type then
         Gen_Instr (BGT_UN, Label);
      else
         Gen_Instr (BGT, Label);
      end if;
      Pop (2);

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_Greater;

   ------------------------------
   -- Gen_Branch_Greater_Equal --
   ------------------------------

   procedure Gen_Branch_Greater_Equal (Label : Label_Id) is
   begin
      Gen_Zero (Type_Kind (Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind =>
            if Next_To_Top (Active_Stack) = UInt_Type or
               Next_To_Top (Active_Stack) = ULong_Type then
               Gen_Instr (BGE_UN, Label);
            else
               Gen_Instr (BGE, Label);
            end if;

         when Float_Kind | Double_Kind =>
            Gen_Instr (BGE_UN, Label);

         when others =>
            Pop (2);
            Gen_Push_Int (Uint_0);
            raise Incompatible_Types;
      end case;

      Pop (2);

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_Greater_Equal;

   ------------------------------
   -- Gen_Compare_Branch_Equal --
   ------------------------------

   procedure Gen_Compare_Branch_Equal (Label : Label_Id) is
   begin
      if not
        (Type_Kind (Top (Active_Stack)) =
         Type_Kind (Next_To_Top (Active_Stack))
           or else (Is_Reference_Type (Top (Active_Stack))
                    and then Is_Reference_Type (Next_To_Top (Active_Stack))))
      then
         --  Pop both operands and generate false result to allow compilation
         --  continue.

         Pop (2);
         Gen_Push_Int (Uint_0);
         raise Incompatible_Types;
      end if;

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind =>
            Gen_Instr (BEQ, Label);
            Pop (2);  --  Pop both operands
         when Float_Kind | Double_Kind =>
            Gen_Instr (BEQ, Label);
            Pop (2);  --  Pop both operands
         when Array_Kind | Class_Kind =>
            Gen_Instr (BEQ, Label);
            Pop (2);  --  Pop both operands
         when others =>
            Pop (2);
            Gen_Push_Int (Uint_0);
            raise Incompatible_Types;
      end case;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Compare_Branch_Equal;

   ----------------------------------
   -- Gen_Compare_Branch_Not_Equal --
   ----------------------------------

   procedure Gen_Compare_Branch_Not_Equal (Label : Label_Id) is
   begin
      if not
        (Type_Kind (Top (Active_Stack)) =
         Type_Kind (Next_To_Top (Active_Stack))
           or else (Is_Reference_Type (Top (Active_Stack))
                    and then Is_Reference_Type (Next_To_Top (Active_Stack))))
      then
         --  Pop both operands and generate false result to allow compilation
         --  continue.

         Pop (2);
         Gen_Push_Int (Uint_0);
         raise Incompatible_Types;
      end if;

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind =>
            Gen_Instr (BNE_UN, Label);
            Pop (2);  --  Pop both operands
         when Float_Kind | Double_Kind =>
            Gen_Instr (BNE_UN, Label);
            Pop (2);  --  Pop both operands
         when Array_Kind | Class_Kind =>
            Gen_Instr (BNE_UN, Label);
            Pop (2);  --  Pop both operands
         when others =>
            Pop (2);
            Gen_Push_Int (Uint_0);
            raise Incompatible_Types;
      end case;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Compare_Branch_Not_Equal;

   -----------------------------
   -- Gen_Compare_Branch_Less --
   -----------------------------

   procedure Gen_Compare_Branch_Less
     (Label     : Label_Id;
      Unordered : Boolean := False)
   is
   begin
      pragma Assert
        (not Stack_Checking
          or else Type_Kind (Top (Active_Stack))
                   = Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind =>
            if Next_To_Top (Active_Stack) = UInt_Type
              or else Next_To_Top (Active_Stack) = ULong_Type
            then
               Gen_Instr (BLT_UN, Label);
            else
               Gen_Instr (BLT, Label);
            end if;

            Pop (2);  --  Pop both operands

         when Float_Kind | Double_Kind =>
            if Unordered then
               Gen_Instr (BLT_UN, Label);
            else
               Gen_Instr (BLT, Label);
            end if;
            Pop (2);  --  Pop both operands

         when Array_Kind | Class_Kind =>
            Gen_Instr (BLT, Label);
            Pop (2);  --  Pop both operands

         when others =>
            raise Incompatible_Types;
      end case;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Compare_Branch_Less;

   -----------------------------------
   -- Gen_Compare_Branch_Less_Equal --
   -----------------------------------

   procedure Gen_Compare_Branch_Less_Equal
     (Label     : Label_Id;
      Unordered : Boolean := False)
   is
   begin
      pragma Assert
        (not Stack_Checking
          or else Type_Kind (Top (Active_Stack)) =
                    Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind =>
            if Next_To_Top (Active_Stack) = UInt_Type or
               Next_To_Top (Active_Stack) = ULong_Type then
               Gen_Instr (BLE_UN, Label);
            else
               Gen_Instr (BLE, Label);
            end if;

            Pop (2);  --  Pop both operands

         when Float_Kind | Double_Kind =>
            if Unordered then
               Gen_Instr (BLE_UN, Label);
            else
               Gen_Instr (BLE, Label);
            end if;
            Pop (2);  --  Pop both operands

         when Array_Kind =>
            Gen_Instr (BLE, Label);
            Pop (2);  --  Pop both operands

         when Class_Kind =>
            --  Occurs with System.Address comparisons (e.g., in the run-time).
            --  Just compare for equal since there's not much else we can do.

            Gen_Instr (BEQ, Label);
            Pop (2);  --  Pop both operands
         when others =>
            raise Incompatible_Types;
      end case;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Compare_Branch_Less_Equal;

   --------------------------------
   -- Gen_Compare_Branch_Greater --
   --------------------------------

   procedure Gen_Compare_Branch_Greater
     (Label     : Label_Id;
      Unordered : Boolean := False)
   is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind =>
            if Next_To_Top (Active_Stack) = UInt_Type or
               Next_To_Top (Active_Stack) = ULong_Type then
               Gen_Instr (BGT_UN, Label);
            else
               Gen_Instr (BGT, Label);
            end if;

            Pop (2);  --  Pop both operands

         when Float_Kind | Double_Kind =>
            if Unordered then
               Gen_Instr (BGT_UN, Label);
            else
               Gen_Instr (BGT, Label);
            end if;
            Pop (2);  --  Pop both operands

         when Array_Kind =>
            Gen_Instr (BGT, Label);
            Pop (2);  --  Pop both operands

         when Class_Kind =>
            --  Occurs with System.Address comparisons (e.g., in the run-time).
            --  Just compare for equal since there's not much else we can do.

            Gen_Instr (BEQ, Label);
            Pop (2);  --  Pop both operands

         when others =>
            raise Incompatible_Types;
      end case;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Compare_Branch_Greater;

   --------------------------------------
   -- Gen_Compare_Branch_Greater_Equal --
   --------------------------------------

   procedure Gen_Compare_Branch_Greater_Equal
     (Label     : Label_Id;
      Unordered : Boolean := False)
   is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind | Long_Kind =>
            if Next_To_Top (Active_Stack) = UInt_Type or
               Next_To_Top (Active_Stack) = ULong_Type then
               Gen_Instr (BGE_UN, Label);
            else
               Gen_Instr (BGE, Label);
            end if;

            Pop (2);  --  Pop both operands

         when Float_Kind | Double_Kind =>
            if Unordered then
               Gen_Instr (BGE_UN, Label);
            else
               Gen_Instr (BGE, Label);
            end if;
            Pop (2);  --  Pop both operands

         when Array_Kind =>
            Gen_Instr (BGE, Label);
            Pop (2);  --  Pop both operands

         when Class_Kind =>
            --  Occurs with System.Address comparisons (e.g., in the run-time).
            --  Just compare for equal since there's not much else we can do.

            Gen_Instr (BEQ, Label);
            Pop (2);  --  Pop both operands

         when others =>
            raise Incompatible_Types;
      end case;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Compare_Branch_Greater_Equal;

   ------------------------
   -- Gen_Branch_If_Null --
   ------------------------

   procedure Gen_Branch_If_Null (Label : Label_Id) is
   begin
      pragma Assert (Check_Top (Array_Kind) or else Check_Top (Class_Kind));

      Gen_Instr (BRFALSE, Label);
      Pop;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_If_Null;

   ------------------------------------
   -- Gen_Compare_Branch_If_Not_Null --
   ------------------------------------

   procedure Gen_Branch_If_Not_Null (Label : Label_Id) is
   begin
      pragma Assert (Check_Top (Array_Kind) or else Check_Top (Class_Kind));

      Gen_Instr (BRTRUE, Label);
      Pop;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_If_Not_Null;

   --------------
   -- Gen_Goto --
   --------------

   procedure Gen_Goto (Label : Label_Id) is
   begin
      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));

      Gen_Instr (BR, Label);
   end Gen_Goto;

   ---------------
   -- Gen_Leave --
   ---------------

   procedure Gen_Leave (Label : Label_Id) is
   begin
      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));

      Gen_Instr (LEAVE, Label);
   end Gen_Leave;

   ------------------------
   -- Set_Stack_Checking --
   ------------------------

   procedure Set_Stack_Checking (Enable : Boolean) is
   begin
      pragma Assert (Stack_Checking /= Enable);

      Stack_Checking := Enable;
   end Set_Stack_Checking;

   -----------------------------
   -- Suppress_Stack_Checking --
   -----------------------------

   procedure Suppress_Stack_Checking (Check_State : out Boolean) is
   begin
      Check_State := Stack_Checking;
      Stack_Checking := False;
   end Suppress_Stack_Checking;

   ----------------------------
   -- Restore_Stack_Checking --
   ----------------------------

   procedure Restore_Stack_Checking (Check_State : Boolean) is
   begin
      Stack_Checking := Check_State;
   end Restore_Stack_Checking;

   ----------------
   -- Mark_Stack --
   ----------------

   procedure Mark_Stack is
   begin
      pragma Assert (not Stack_Checking);

      Mark (Active_Stack);
      Stack_Marked := True;
   end Mark_Stack;

   -------------------
   -- Release_Stack --
   -------------------

   procedure Release_Stack is
   begin
      pragma Assert (not Stack_Checking and then Stack_Marked);

      Release (Active_Stack);
      Stack_Marked := Marked (Active_Stack);
   end Release_Stack;

   -----------------
   -- Reset_Stack --
   -----------------

   procedure Reset_Stack is
   begin
      Reset (Active_Stack);
      Stack_Checking := True;
   end Reset_Stack;

   ---------------
   -- Push_Type --
   ---------------

   procedure Push_Type (JVM_Type : Type_Id) is
   begin
      Push (JVM_Type);
   end Push_Type;

   --------------
   -- Pop_Type --
   --------------

   procedure Pop_Type (Count : Positive := 1) is
   begin
      Pop (Stack_Range (Count));
   end Pop_Type;

   ------------------
   -- Stack_Heigth --
   ------------------

   function Stack_Heigth return Natural is
   begin
      return Num_Elements (Active_Stack);
   end Stack_Heigth;

   --------------
   -- Top_Type --
   --------------

   function Top_Type (Disp : Natural := 0) return Type_Id is
   begin
      return Top (Active_Stack, Stack_Range (Disp));
   end Top_Type;

   -----------------------------------
   -- Subroutine-related operations --
   -----------------------------------

   --------------------
   -- New_Subroutine --
   --------------------

   function New_Subroutine return Subroutine_Id is
      Subroutine : constant Subroutine_Id := JVM.Info.New_Subroutine;

   begin
      pragma Assert (Active_Method /= Null_Method);

      Add_Subroutine (Active_Method, Subroutine);
      Set_Method (Subroutine, Active_Method);
      Set_Is_Open (Subroutine);

      return Subroutine;
   end New_Subroutine;

   ---------------------
   -- Open_Subroutine --
   ---------------------

   procedure Open_Subroutine (Subroutine : Subroutine_Id) is
      Code_Seq   : Code_Sequence;
      Subr_Stack : Op_Stack_Id;

   begin
      pragma Assert (Is_Open (Subroutine));

      pragma Assert (Active_Method /= Null_Method
            and then Method (Subroutine) = Active_Method
            and then Active_Subroutine (Active_Method) = Null_Subroutine);

      Set_Active_Subroutine (Active_Method, Subroutine);
      Active_Subr := Subroutine;

      --  Save the active method's code sequence and operand stack

      Set_Code (Active_Method, Active_Seq);
      Set_Op_Stack (Active_Method, Active_Stack);

      Start_Sequence (Code_Seq);
      Active_Seq := Code_Seq;

      Subr_Stack := New_Stack (200);
      Active_Stack := Subr_Stack;
      Set_Stack_Method (Subr_Stack, Active_Method);

      --  Create a label for the subroutine entry point

      Set_Subroutine_Label (Subroutine, New_Label);

      --  Upon entering a subroutine, the return address is on top of
      --  the stack, so we have to push the return address type here
      --  to ensure consistency. The subroutine should store the address
      --  away in a local variable on entry.

      Push (Subr_Stack, Retaddr_Type);
   end Open_Subroutine;

   ----------------------
   -- Close_Subroutine --
   ----------------------

   procedure Close_Subroutine is
   begin
      pragma Assert (Active_Subr /= Null_Subroutine
                      and then Is_Empty (Active_Stack));

      --  Record the maximum word depth of the stack before it gets
      --  deallocated.

      Set_Max_Stack_Depth (Active_Subr, Max_Depth (Op_Stack (Active_Subr)));
      Free_Stack (Active_Stack);

      Set_Code (Active_Subr, Active_Seq);

      Set_Active_Subroutine (Active_Method, Null_Subroutine);

      --  Restore the active code sequence and operand stack to those
      --  of the active method.

      Set_Is_Open (Active_Subr, False);

      Active_Seq := Method_Code (Active_Method);
      Active_Stack := Op_Stack (Active_Method);

      --  Update the maximum recorded stack depth for the active method
      --  if the subroutine's max depth is greater than the current max
      --  for the method. (Note: we currently require that the stack be
      --  empty when generating a subroutine call (Jsr), which allows
      --  us to update the maximum depth at this point rather than on
      --  subroutine calls. If we relax that restriction, then the stack
      --  will need to be incremented by a called subroutine's max stack
      --  depth at the point of generating a Jsr instruction. ???

      if Max_Stack_Depth (Active_Subr) > Max_Depth (Active_Stack) then
         Set_Max_Depth (Active_Stack, Max_Stack_Depth (Active_Subr));
      end if;

      Active_Subr := Null_Subroutine;
   end Close_Subroutine;

   -------------
   -- Gen_JSR --
   -------------

   procedure Gen_JSR (Subroutine : Subroutine_Id) is
   begin
      pragma Assert (not Is_Open (Subroutine));
      pragma Assert (Is_Empty (Active_Stack));

      Gen_Instr (JMP, Subroutine);
   end Gen_JSR;

   --------------------------------
   -- Gen_Save_Subroutine_Return --
   --------------------------------

   procedure Gen_Save_Subroutine_Return (Local : Local_Var_Id) is
   begin
      pragma Assert (Is_Empty (Active_Stack));

      Push (Retaddr_Type);
      Gen_Store_Local (Local);
   end Gen_Save_Subroutine_Return;

   ---------------------------
   -- Gen_Subroutine_Return --
   ---------------------------

   procedure Gen_Subroutine_Return (Local : Local_Var_Id) is
   begin
      pragma Assert (Is_Empty (Active_Stack));

      Gen_Instr (RET, Local);
   end Gen_Subroutine_Return;

   ---------------------------
   -- Case table generation --
   ---------------------------

   ------------------------
   -- Start_Switch_Table --
   ------------------------

   procedure Start_Switch_Table (Default : Label_Id) is
      New_Switch_Stmt : JVM.Code.Instruction (SWITCH);

   begin
      pragma Assert (Active_Switch_Stmt.Op = UNUSED24);
      pragma Assert (Check_Top (Int_Type));

      --  First we make sure that the index on top of the stack is an int32

      if Type_Kind (Top (Active_Stack)) = Long_Kind then
         Gen_Conversion (Int_Type);
      end if;

      --  An Int will be pushed on the stack before the switch instruction,
      --  so take it into account when computing Stack.Curr_Depth

      Push (Int_Type);
      Pop;

      Active_Switch_Stmt := New_Switch_Stmt;
      Active_Switch_Stmt.Default_Label := Default;
      Start_Switch_List (Active_Switch_Stmt.Switch_Pairs);
   end Start_Switch_Table;

   ---------------------
   -- Add_Switch_Pair --
   ---------------------

   procedure Add_Switch_Pair
     (Match_Low  : Uint;
      Match_High : Uint;
      Target     : Label_Id) is
   begin
      pragma Assert (Active_Switch_Stmt.Op = SWITCH
             or else Active_Switch_Stmt.Op = SWITCH);

      for Value in UI_To_Int (Match_Low) .. UI_To_Int (Match_High) loop
         Add_Switch_Pair
           (Active_Switch_Stmt.Switch_Pairs, Int_32 (Value), Target);
      end loop;
   end Add_Switch_Pair;

   ----------------------
   -- End_Switch_Table --
   ----------------------

   procedure End_Switch_Table is
   begin
      pragma Assert (Active_Switch_Stmt.Op = SWITCH
             or else Active_Switch_Stmt.Op = SWITCH);

      Gen_Instr (Active_Switch_Stmt);

      Active_Switch_Stmt := (Op => UNUSED24, Next => Null_Instr);

      Pop;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end End_Switch_Table;

   -------------------------
   -- Cancel_Switch_Table --
   -------------------------

   procedure Cancel_Switch_Table is
   begin
      pragma Assert (Active_Switch_Stmt.Op = SWITCH
             or else Active_Switch_Stmt.Op = SWITCH);

      Active_Switch_Stmt := (Op => UNUSED24, Next => Null_Instr);
   end Cancel_Switch_Table;

   --------------------
   -- Gen_Block_Copy --
   --------------------

   procedure Gen_Block_Copy is
   begin
      Gen_Instr (CPBLK);
      Pop (3);
   end Gen_Block_Copy;

   ----------------
   -- Gen_Sizeof --
   ----------------
   procedure Gen_Sizeof (JVM_Type : Type_Id) is
   begin
      Gen_Instr (SIZEOF, Type_Item (Active_Class, JVM_Type));
      Push (Int_Type);
   end Gen_Sizeof;

   ------------------------------
   -- Method return operations --
   ------------------------------

   procedure Declare_Ret_Val is
      Return_Value : Local_Var_Id;
   begin
      if Result_Type (Active_Method) /= Void_Type then
         Return_Value := Local_Var (Active_Method, "_retval");

         if Return_Value = Null_Local_Var then
            Return_Value :=
              New_Local_Var ("_retval", Result_Type (Active_Method));
         end if;
      end if;
   end Declare_Ret_Val;

   -----------------------
   -- Gen_Method_Return --
   -----------------------

   procedure Gen_Method_Return is
   begin
      if Inside_Try_Catch_Finally then
         if not Is_Empty (Active_Stack) and
            Result_Type (Active_Method) /= Void_Type then
            Gen_Store_Local (Local_Var (Active_Method, "_retval"));
         end if;

         Gen_Instr (LEAVE, Null_Label);
         return;
      end if;

      if Result_Type (Active_Method) = Void_Type then
         Gen_Instr (RET);

      else
         pragma Assert (Check_Top (Result_Type (Active_Method))
           or else
             (Type_Kind (Result_Type (Active_Method)) = Class_Kind
                and then
              Is_Interface (Class_Of_Type (Result_Type (Active_Method)))));

         case Type_Kind (Result_Type (Active_Method)) is
            when Int_Kind =>
               Gen_Instr (RET);
            when Long_Kind =>
               Gen_Instr (RET);
            when Float_Kind =>
               Gen_Instr (RET);
            when Double_Kind =>
               Gen_Instr (RET);
            when Array_Kind | Class_Kind =>
               Gen_Instr (RET);
            when others =>
               pragma Assert (False);
               raise Program_Error;
         end case;

         Pop;  --  Pop the method result for consistency
      end if;
   end Gen_Method_Return;

   ----------------------------------
   -- Method invocation operations --
   ----------------------------------

   -----------------------
   -- Gen_Invoke_Method --
   -----------------------

   procedure Gen_Invoke_Method (Method : Method_Id) is
   begin
      if Is_Static (Method) then
         Gen_Invoke_Static (Method);

      elsif Name (Method) = Name ("<access to subp>") then
         Gen_Invoke_Indirect (Method);

      elsif Name (Method) = Name (".ctor") then
         Gen_Invoke_Special (Method);

      elsif Is_Interface (Class_Of (Method)) then
         Gen_Invoke_Interface (Method);

      else
         Gen_Invoke_Virtual (Method);
      end if;
   end Gen_Invoke_Method;

   ------------------------
   -- Gen_Invoke_Virtual --
   ------------------------

   procedure Gen_Invoke_Virtual (Method : Method_Id) is
      First_Param : constant Local_Var_Id := First_Local_Var (Method);

   begin
      pragma Assert (not Is_Static (Method));

      Check_Arguments_Type (Method);

      if Name (Method) = Snames.Name_Op_Eq
        and then Superclass (Class_Of_Type (Variable_Type (First_Param))) > 0
        and then Is_Value_Type (Class_Of_Type (Variable_Type (First_Param)))
      then
         Gen_Instr (CEQ);

      --  ValueType methods don't really dispatch

      elsif Superclass (Class_Of_Type (Variable_Type (First_Param))) > 0
        and then Is_Value_Type (Class_Of_Type (Variable_Type (First_Param)))
      then
         Gen_Instr (CALL, New_Method_Ref (Method));
      else
         Gen_Instr (CALLVIRT, New_Method_Ref (Method));
      end if;

      if Result_Type (Method) /= Void_Type then
         Push (Result_Type (Method));
      end if;
   end Gen_Invoke_Virtual;

   ------------------------
   -- Gen_Invoke_Special --
   ------------------------

   procedure Gen_Invoke_Special (Method : Method_Id) is
   begin
      pragma Assert (not Is_Static (Method));

      Check_Arguments_Type (Method);
      Gen_Instr (CALL, New_Method_Ref (Method));

      if Result_Type (Method) /= Void_Type then
         Push (Result_Type (Method));
      end if;
   end Gen_Invoke_Special;

   -----------------------
   -- Gen_Invoke_Static --
   -----------------------

   procedure Gen_Invoke_Static (Method : Method_Id) is
   begin
      pragma Assert (Is_Static (Method));

      Check_Arguments_Type (Method);
      Gen_Instr (CALL, New_Method_Ref (Method));

      if Result_Type (Method) /= Void_Type then
         Push (Result_Type (Method));
      end if;
   end Gen_Invoke_Static;

   -------------------------
   -- Gen_Invoke_Indirect --
   -------------------------

   procedure Gen_Invoke_Indirect (Method : Method_Id) is
   begin
      pragma Assert (not Is_Static (Method));

      Check_Arguments_Type (Method);
      Gen_Instr (CALLI, New_Method_Ref (Method));

      if Result_Type (Method) /= Void_Type then
         Push (Result_Type (Method));
      end if;
   end Gen_Invoke_Indirect;

   --------------------------
   -- Gen_Invoke_Interface --
   --------------------------

   procedure Gen_Invoke_Interface (Method : Method_Id) is
   begin
      pragma Assert (not Is_Static (Method));

      Check_Arguments_Type (Method);
      Gen_Instr (CALLVIRT, New_Method_Ref (Method));

      if Result_Type (Method) /= Void_Type then
         Push (Result_Type (Method));
      end if;
   end Gen_Invoke_Interface;

   -------------------------------------------------
   -- Miscellaneous stack manipulation operations --
   -------------------------------------------------

   -------------
   -- Gen_Pop --
   -------------

   procedure Gen_Pop (Items : Positive := 1) is
   begin
      for Item in 1 .. Items loop
         if Word_Size (Pop (Active_Stack)) = 1 then
            Gen_Instr (POP);
         else
            Gen_Instr (POP);
         end if;
      end loop;
   end Gen_Pop;

   -------------------
   -- Gen_Duplicate --
   -------------------

   procedure Gen_Duplicate is
   begin
      if Word_Size (Top (Active_Stack)) = 1 then
         Gen_Instr (DUP);
      else
         Gen_Instr (DUP);
      end if;

      Push (Top (Active_Stack));
   end Gen_Duplicate;

   --------------------------
   -- Gen_Double_Duplicate --
   --------------------------

   procedure Gen_Double_Duplicate is
      Dup_Tmp1 : constant Local_Var_Id :=
                   New_Local_Var ("_duptmp1", Top (Active_Stack));
      Dup_Tmp2 : constant Local_Var_Id :=
                   New_Local_Var ("_duptmp2", Top (Active_Stack));

   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      Gen_Store_Local (Dup_Tmp1);
      Gen_Store_Local (Dup_Tmp2);
      Gen_Load_Local (Dup_Tmp2);
      Gen_Load_Local (Dup_Tmp1);
      Gen_Load_Local (Dup_Tmp2);
      Gen_Load_Local (Dup_Tmp1);
   end Gen_Double_Duplicate;

   --------------
   -- Gen_Swap --
   --------------

   procedure Gen_Swap is
      Dup_Tmp1 : constant Local_Var_Id :=
                   New_Local_Var ("_swaptmp1", Top (Active_Stack));
      Dup_Tmp2 : constant Local_Var_Id :=
                   New_Local_Var ("_swaptmp2", Next_To_Top (Active_Stack));
   begin
      Gen_Store_Local (Dup_Tmp1);
      Gen_Store_Local (Dup_Tmp2);
      Gen_Load_Local (Dup_Tmp1);
      Gen_Load_Local (Dup_Tmp2);
   end Gen_Swap;

   ----------------------
   -- Other operations --
   ----------------------

   -------------------------
   -- Gen_Exception_Throw --
   -------------------------

   procedure Gen_Exception_Throw is
   begin
      pragma Assert (Check_Top (Class_Kind));

      Gen_Instr (THROW);

      Pop;  --  Pop the exception instance reference (for consistency)
   end Gen_Exception_Throw;

   ---------------------------
   -- Gen_Exc_Handler_Entry --
   ---------------------------

   procedure Gen_Exc_Handler_Entry
     (Exc_Class       : Class_Id;
      Start_Lbl       : Label_Id;
      End_Lbl         : Label_Id;
      Handler_Lbl     : Label_Id;
      End_Handler_Lbl : Label_Id;
      Kind            : Handler_Kind;
      Filter_Lbl      : Label_Id)
   is
      Exc_Pool_Id : Pool_Id;
      New_Handler : Handler_Id;

   begin
      if Exc_Class = Null_Class then
         Exc_Pool_Id := Null_Pool_Item;
      else
         Exc_Pool_Id := New_Class_Ref (Exc_Class);
      end if;

      New_Handler
         := New_Handler_Entry (Exc_Pool_Id, Start_Lbl, End_Lbl,
            Handler_Lbl, End_Handler_Lbl, Kind, Filter_Lbl);

      if Debug then
         Print (">>> Exception handler entry for ");

         if Exc_Class = Null_Class then
            Print ("<others handler>");
         else
            Print (Name (Exc_Class));
         end if;

         Print_Line;
         Print ("      range" & Label_Number (Start_Lbl)'Img & "$ ..");
         Print (Label_Number (End_Lbl)'Img & "$ handled by ");
         Print (Label_Number (Handler_Lbl)'Img & "$");
         Print_Line;
      end if;

      Append (Active_Handlers, New_Handler);
   end Gen_Exc_Handler_Entry;

   ----------------------
   -- Other operations --
   ----------------------

   --------------------------
   -- Check_Arguments_Type --
   --------------------------

   procedure Check_Arguments_Type (Method : Method_Id) is

      function Is_Parent (T1, T2 : Type_Id) return Boolean;
      --  Returns true if T1 is a parent type of T2

      function Is_Standard_CIL_Array_Element
        (Elem_Typ : Type_Id) return Boolean;
      --  Returns true if Elem_Typ is a valid element of a predefined CIL
      --  array type. See file cil_types.ads

      function Is_Valid_Argument (Formal_Typ : Type_Id) return Boolean;
      --  Return true if the type of the argument stored in the top of the
      --  Active_Stack is covered by Formal_Typ

      -----------------------------------
      -- Is_Standard_CIL_Array_Element --
      -----------------------------------

      function Is_Standard_CIL_Array_Element
        (Elem_Typ : Type_Id) return Boolean is
      begin
         return Elem_Typ = JVM_Type (Stand.Standard_Float)
           or else Elem_Typ = JVM_Type (Stand.Standard_Long_Float)
           or else Elem_Typ = JVM_Type (Stand.Standard_Boolean)
           or else Elem_Typ = JVM_Type (Stand.Standard_Short_Short_Integer)
           or else Elem_Typ = JVM_Type (Stand.Standard_Wide_Character)
           or else Elem_Typ = JVM_Type (Stand.Standard_Short_Integer)
           or else Elem_Typ = JVM_Type (Stand.Standard_Integer)
           or else Elem_Typ = JVM_Type (Stand.Standard_Long_Long_Integer)
           or else Elem_Typ = Native_Int_Type

           or else (Type_Kind (Elem_Typ) = Class_Kind
                      and then Class_Of_Type (Elem_Typ)
                                  = API_Class (Ada_UInt));
      end Is_Standard_CIL_Array_Element;

      ---------------
      -- Is_Parent --
      ---------------

      function Is_Parent (T1, T2 : Type_Id) return Boolean is
         Super_Class : Class_Id;

      begin
         if T1 = T2 then
            return True;
         end if;

         if T2 = Native_Int_Type then
            Super_Class := Java_Lang_Object;
         else
            Super_Class := Class_Of_Type (T2);
         end if;

         if Super_Class = System_Delegate then
            return False;
         end if;

         while Super_Class /= Class_Of_Type (T1)
           and then Super_Class /= Java_Lang_Object
         loop
            Super_Class := Superclass (Super_Class);
         end loop;

         return Super_Class = Class_Of_Type (T1);
      end Is_Parent;

      -----------------------
      -- Is_Valid_Argument --
      -----------------------

      function Is_Valid_Argument (Formal_Typ : Type_Id) return Boolean is
         Actual_Typ : constant Type_Id := Top (Active_Stack);

      begin
         --  The formal accepts any object

         if Type_Kind (Formal_Typ) = Class_Kind
           and then Class_Of_Type (Formal_Typ) = Java_Lang_Object
         then
            return True;

         --  Arguments of derived types

         elsif Type_Kind (Formal_Typ) = Class_Kind
           and then Type_Kind (Actual_Typ) = Class_Kind
           and then Actual_Typ /= Any_Ref_Type
           and then Is_Parent (Actual_Typ, Formal_Typ)
         then
            return True;

         --  Derivations of Ada_AR

         elsif Type_Kind (Formal_Typ) = Class_Kind
           and then Type_Kind (Actual_Typ) = Class_Kind
           and then Actual_Typ /= Any_Ref_Type
           and then Superclass (Class_Of_Type (Formal_Typ))
                      = API_Class (Ada_Activation_Rec)
           and then Superclass (Class_Of_Type (Actual_Typ))
                      = API_Class (Ada_Activation_Rec)
         then
            return True;

         --  Valuetypes.
         --  ??? Too permissive for now, we should have a better test here ...

         elsif Type_Kind (Formal_Typ) = Class_Kind
           and then Is_Value_Type (Class_Of_Type (Formal_Typ))
         then
            return True;

         --  Argument of dispatching call throught interface type

         elsif Type_Kind (Formal_Typ) = Class_Kind
           and then Type_Kind (Actual_Typ) = Class_Kind
           and then Actual_Typ /= Any_Ref_Type
           and then Is_Interface (Class_Of_Type (Actual_Typ))
         then
            return True;

         --  Interface type argument

         elsif Type_Kind (Formal_Typ) = Class_Kind
           and then Is_Interface (Class_Of_Type (Formal_Typ))
           and then Type_Kind (Actual_Typ) = Class_Kind
           and then Actual_Typ /= Any_Ref_Type
           and then Present (Ada_Entity (Class_Of_Type (Actual_Typ)))
           and then Is_Record_Type (Ada_Entity (Class_Of_Type (Actual_Typ)))
         then
            return True;

         --  Integer object

         elsif Type_Kind (Formal_Typ) = Class_Kind
           and then (Class_Of_Type (Formal_Typ) = API_Class (Ada_Int)
                       or else
                     Class_Of_Type (Formal_Typ) = API_Class (Ada_UInt))
           and then Type_Kind (Actual_Typ) = Int_Kind
         then
            return True;

         --  Long integer object

         elsif Type_Kind (Formal_Typ) = Class_Kind
           and then (Class_Of_Type (Formal_Typ) = API_Class (Ada_Lng)
                       or else
                     Class_Of_Type (Formal_Typ) = API_Class (Ada_ULng))
           and then Type_Kind (Actual_Typ) = Long_Kind
         then
            return True;

         --  Float object

         elsif Type_Kind (Formal_Typ) = Class_Kind
           and then Class_Of_Type (Formal_Typ) = API_Class (Ada_Flt)
           and then Type_Kind (Actual_Typ) = Float_Kind
         then
            return True;

         --  Double object

         elsif Type_Kind (Formal_Typ) = Class_Kind
           and then Class_Of_Type (Formal_Typ) = API_Class (Ada_Dbl)
           and then Type_Kind (Actual_Typ) = Double_Kind
         then
            return True;

         --  Handle array arguments -----------------------------------

         --  The formal accepts any array of objects

         elsif Type_Kind (Formal_Typ) = Class_Kind
           and then Class_Of_Type (Formal_Typ) = API_Class (System_Array)
           and then Type_Kind (Actual_Typ) = Array_Kind
         then
            return True;

         --  Relax matching for arrays of Boolean, Byte, Char and Short

         elsif Type_Kind (Formal_Typ) = Array_Kind
           and then Type_Kind (Actual_Typ) = Array_Kind
           and then Dimensions (Formal_Typ) = Dimensions (Actual_Typ)
           and then (Type_Kind (Element_Type (Formal_Typ)) = Int_Kind
                       or else
                     Type_Kind (Element_Type (Formal_Typ)) = Long_Kind)
           and then (Type_Kind (Element_Type (Actual_Typ)) = Int_Kind
                       or else
                     Type_Kind (Element_Type (Actual_Typ)) = Long_Kind
                       or else
                     (Type_Kind (Element_Type (Actual_Typ)) = Class_Kind
                        and then Class_Of_Type (Element_Type (Actual_Typ))
                                   = API_Class (Ada_Int)))
         then
            return True;

         --  Relax matching for arrays of integers

         elsif Type_Kind (Formal_Typ) = Array_Kind
           and then Type_Kind (Actual_Typ) = Array_Kind
           and then Dimensions (Formal_Typ) = Dimensions (Actual_Typ)
           and then Type_Kind (Element_Type (Formal_Typ)) = Int_Kind
           and then Type_Kind (Element_Type (Actual_Typ)) = Class_Kind
           and then Class_Of_Type (Element_Type (Actual_Typ))
                      = API_Class (Ada_Int)
         then
            return True;

         --  Relax matching for CIL array types

         elsif Type_Kind (Formal_Typ) = Class_Kind
           and then Class_Of_Type (Formal_Typ) = API_Class (System_Array)
           and then Type_Kind (Actual_Typ) = Array_Kind
           and then Dimensions (Actual_Typ) = 1
           and then (Actual_Typ = JVM_Type (Stand.Standard_String)
                       or else Is_Standard_CIL_Array_Element
                                (Element_Type (Actual_Typ)))
         then
            return True;

         --  Handle actuals that are access to array types

         elsif Type_Kind (Formal_Typ) = Array_Kind
           and then Type_Kind (Actual_Typ) = Class_Kind
           and then Actual_Typ /= Any_Ref_Type
           and then Present (JVM.Map.Ada_Entity (Class_Of_Type (Actual_Typ)))
           and then Is_Access_Type
                      (JVM.Map.Ada_Entity (Class_Of_Type (Actual_Typ)))
           and then Ekind (Directly_Designated_Type
                            (JVM.Map.Ada_Entity (Class_Of_Type (Actual_Typ))))
                      = E_Array_Type
         then
            return True;

         --  String types

         elsif Type_Kind (Formal_Typ) = Array_Kind
           and then Formal_Typ = JVM_Type (Stand.Standard_String)
           and then Type_Kind (Actual_Typ) = Class_Kind
           and then Class_Of_Type (Actual_Typ) = API_Class (Ada_String)
         then
            return True;

         --  Relax access types. Needed to allow passing objects to which
         --  the 'unrestricted_access or 'unchecked_access attributes is
         --  applied.

         elsif Type_Kind (Formal_Typ) = Class_Kind
           and then Present (Ada_Entity (Class_Of_Type (Formal_Typ)))
           and then Is_Access_Type (Ada_Entity (Class_Of_Type (Formal_Typ)))
           and then Type_Kind (Actual_Typ) = Class_Kind
         then
            return True;

         --  This case needs further investigation???

         elsif Type_Kind (Formal_Typ) = Float_Kind
           and then Type_Kind (Actual_Typ) = Int_Kind
         then
            return True;

         --  Access to array types. More work needed here to check the
         --  dimensions and the element type???

         elsif Type_Kind (Formal_Typ) = Class_Kind
           and then Present (Ada_Entity (Class_Of_Type (Formal_Typ)))
           and then Is_Access_Type (Ada_Entity (Class_Of_Type (Formal_Typ)))
           and then Ekind
                     (Designated_Type
                      (Ada_Entity
                       (Class_Of_Type (Formal_Typ)))) = E_Array_Type
           and then Type_Kind (Actual_Typ) = Array_Kind
         then
            return True;

         --  Access to private array types. Similar to previous case more
         --  work needed here???

         elsif Type_Kind (Formal_Typ) = Class_Kind
           and then Present (Ada_Entity (Class_Of_Type (Formal_Typ)))
           and then Is_Access_Type (Ada_Entity (Class_Of_Type (Formal_Typ)))
           and then Is_Private_Type
                     (Designated_Type
                      (Ada_Entity (Class_Of_Type (Formal_Typ))))
           and then Ekind
                     (Full_View
                      (Designated_Type
                       (Ada_Entity
                        (Class_Of_Type (Formal_Typ))))) = E_Array_Type
           and then Type_Kind (Actual_Typ) = Array_Kind
         then
            return True;

         elsif not Check_Top (Active_Stack, Formal_Typ) then
            return False;
         end if;

         return True;
      end Is_Valid_Argument;

      --  Local variables

      Formal_Typ : Type_Id;
      Param      : Local_Var_Id := First_Local_Var (Method);

   --  Start of processing for Check_Arguments_Type

   begin
      --  Given that actuals are stored in reverse order we use an auxiliary
      --  stack to reverse the order of the formals.

      while Param /= Null_Local_Var and then Is_Param (Param) loop
         Push (Aux_Stack, Type_Of (Param));
         Param := Next_Local_Var (Param);
      end loop;

      --  Check that argument types match

      while not Is_Empty (Aux_Stack) loop
         Formal_Typ := Top (Aux_Stack);

         --  In previous versions of this backend we did not check the type
         --  of the arguments. In this version we add the missing check as
         --  an assertion to internally tune these routines.

         pragma Assert (Is_Valid_Argument (Formal_Typ));

         Pop (Aux_Stack, 1);
         Pop (Active_Stack, 1);
      end loop;
   end Check_Arguments_Type;

   -------------------------------
   -- Gen_Load_Function_Pointer --
   -------------------------------

   procedure Gen_Load_Function_Pointer (Method : Method_Id) is
   begin
      Gen_Instr (LDFTN, New_Method_Ref (Method));
      Push (Int_Type);
   end Gen_Load_Function_Pointer;

   --------------------
   -- Gen_Check_Cast --
   --------------------

   procedure Gen_Check_Cast (Class : Class_Id) is
   begin
      pragma Assert (Check_Top (Class_Kind));

      Gen_Instr (CASTCLASS, New_Class_Ref (Class));

      --  Change the stack type to Class's type

      Pop;
      Push (Type_Of (Class));
   end Gen_Check_Cast;

   --------------------
   -- Gen_Check_Cast --
   --------------------

   procedure Gen_Check_Cast (JVM_Type : Type_Id) is
   begin
      pragma Assert (Check_Top (Class_Kind) or else Check_Top (Array_Kind));

      if Type_Kind (JVM_Type) = Array_Kind then
         Gen_Instr (CASTCLASS, New_Array_Ref (JVM_Type));

         --  Change the stack type to the target type
         Pop;
         Push (JVM_Type);
      else
         --  Gen_Conversion will issue a CASTCLASS when possible, or throw
         --  an assert failure.
         Gen_Conversion (JVM_Type);
      end if;
   end Gen_Check_Cast;

   ---------------------
   -- Gen_Instance_Of --
   ---------------------

   procedure Gen_Instance_Of (Class : Class_Id) is
      My_Label1   : Label_Id;
      My_Label2   : Label_Id;
      Check_State : Boolean;

   begin
      --  Warning: This assertion is over-restrictive if in the future we need
      --  to check if the contents in the top of the stack is a derivation of
      --  an Array or a derivation of an Object.

      pragma Assert (Check_Top (Class_Kind));

      Gen_Instr (ISINST, New_Class_Ref (Class));
      My_Label1 := New_Label;
      My_Label2 := New_Label;

      Suppress_Stack_Checking (Check_State);
      Gen_Branch_If_Not_Null (My_Label1);
      --  instance reference popped here

      Gen_Push_Int (Uint_0);
      Pop;
      Gen_Goto (My_Label2);

      Gen_Label (My_Label1);
      Gen_Push_Int (Uint_1);
      Pop;

      Gen_Label (My_Label2);
      Restore_Stack_Checking (Check_State);
      Push_Type (Int_Type);
   end Gen_Instance_Of;

   -------------
   -- Gen_NOP --
   -------------

   procedure Gen_NOP is
   begin
      Gen_Instr (NOP);
   end Gen_NOP;

   -----------------------
   -- Gen_Monitor_Enter --
   -----------------------

   procedure Gen_Monitor_Enter is
   begin
      --  Should never be called in CIL
      pragma Assert (False);
      raise Program_Error;
   end Gen_Monitor_Enter;

   ----------------------
   -- Gen_Monitor_Exit --
   ----------------------

   procedure Gen_Monitor_Exit is
   begin
      --  Should never be called in CIL
      pragma Assert (False);
      raise Program_Error;
   end Gen_Monitor_Exit;

   --------------------
   -- Gen_End_Filter --
   --------------------

   procedure Gen_End_Filter is
   begin
      pragma Assert (Check_Top (Int_Kind));

      Gen_Instr (ENDFILTER);

      Pop;  --  Pop the instance reference
   end Gen_End_Filter;

   ---------------------
   -- Is_Generic_Type --
   ---------------------

   function Is_Generic_Type (Typ : Type_Id) return Boolean is
   begin
      return Typ >= Generic_Types (Generic_Types'First) and
         Typ <= Generic_Types (Generic_Types'Last);
   end Is_Generic_Type;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Predef_Type  : Type_Id;
      Predef_Class : Class_Id;
      Predef_Param : Local_Var_Id;

      Predef_Method : Method_Id;
      pragma Unreferenced (Predef_Param);
      pragma Warnings (Off, Predef_Method);

   begin
      Predef_Type := New_Type (Void_Kind);
      pragma Assert (Predef_Type = Void_Type);
      Set_Name      (Void_Type, Name ("void"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Boolean_Type);
      Set_Name      (Boolean_Type, Name ("bool"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Byte_Type);
      Set_Name      (Byte_Type, Name ("unsigned int8"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = SByte_Type);
      Set_Name      (SByte_Type, Name ("int8"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Char_Type);
      Set_Name      (Char_Type, Name ("char"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Short_Type);
      Set_Name      (Short_Type, Name ("int16"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Int_Type);
      Set_Name      (Int_Type, Name ("int32"));

      Predef_Type := New_Type (Long_Kind);
      pragma Assert (Predef_Type = Long_Type);
      Set_Name      (Long_Type, Name ("int64"));

      Predef_Type := New_Type (Float_Kind);
      pragma Assert (Predef_Type = Float_Type);
      Set_Name      (Float_Type, Name ("float32"));

      Predef_Type := New_Type (Double_Kind);
      pragma Assert (Predef_Type = Double_Type);
      Set_Name      (Double_Type, Name ("float64"));

      Predef_Type := New_Type (Return_Addr_Kind);
      pragma Assert (Predef_Type = Retaddr_Type);
      Set_Name      (Retaddr_Type, Name ("jvm_return_address"));

      Predef_Type := New_Type (Class_Kind);
      pragma Assert (Predef_Type = Any_Ref_Type);
      Set_Name      (Any_Ref_Type, Name ("<any_ref>"));

      Predef_Class := New_Class;
      pragma Assert (Predef_Class = Java_Lang_Object);
      Set_Name       (Java_Lang_Object, Name ("Object"));
      Set_Pkg_Name   (Java_Lang_Object, Str_Id ("[mscorlib]System"));
      Set_Superclass (Java_Lang_Object, Null_Class);
      Set_Is_Public  (Java_Lang_Object);

      Predef_Type := New_Type (Class_Kind);
      pragma Assert (Predef_Type = Java_Lang_Object_Type);
      Set_Name      (Java_Lang_Object_Type, Name ("[mscorlib]System.Object"));
      Set_Class     (Java_Lang_Object_Type, Java_Lang_Object);

      Predef_Class := New_Class;
      pragma Assert (Predef_Class = Java_Lang_String);
      Set_Name       (Java_Lang_String, Name ("String"));
      Set_Pkg_Name   (Java_Lang_String, Str_Id ("[mscorlib]System"));
      Set_Superclass (Java_Lang_String, Java_Lang_Object);
      Set_Is_Public  (Java_Lang_String);

      Predef_Type := New_Type (Class_Kind);
      pragma Assert (Predef_Type = String_Type);
      Set_Name      (String_Type, Name ("string"));
      Set_Class     (String_Type, Java_Lang_String);

      Predef_Type := New_Type (Class_Kind);
      pragma Assert (Predef_Type = JVM_String_Type);
      Set_Name      (JVM_String_Type, Name ("[mscorlib]System.String"));
      Set_Class     (JVM_String_Type, Java_Lang_String);

      Predef_Class := New_Class;
      pragma Assert (Predef_Class = Java_Lang_Native_Int);
      Set_Name       (Java_Lang_Native_Int, Name ("native int"));
      Set_Pkg_Name   (Java_Lang_Native_Int, Str_Id (""));
      Set_Superclass (Java_Lang_Native_Int, Null_Class);
      Set_Is_Public  (Java_Lang_Native_Int);

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Native_Int_Type);
      Set_Name  (Native_Int_Type, Name ("native int"));

      Predef_Class := New_Class;
      pragma Assert (Predef_Class = System_Delegate);
      Set_Name       (System_Delegate, Name ("MulticastDelegate"));
      Set_Pkg_Name   (System_Delegate, Str_Id ("[mscorlib]System"));
      Set_Superclass (System_Delegate, Java_Lang_Object);
      Set_Is_Public  (System_Delegate);

      Predef_Type := New_Type (Class_Kind);
      pragma Assert (Predef_Type = System_Delegate_Type);
      Set_Name      (System_Delegate_Type,
                     Name ("[mscorlib]System.MulticastDelegate"));
      Set_Class     (System_Delegate_Type, System_Delegate);

      Predef_Class := New_Class;
      pragma Assert (Predef_Class = System_Valuetype);
      Set_Name       (System_Valuetype, Name ("ValueType"));
      Set_Pkg_Name   (System_Valuetype, Str_Id ("[mscorlib]System"));
      Set_Superclass (System_Valuetype, Java_Lang_Object);
      Set_Is_Public  (System_Valuetype);

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Uint8_Addrof_Type);
      Set_Name      (Uint8_Addrof_Type, Name ("unsigned int8&"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Uint16_Addrof_Type);
      Set_Name      (Uint16_Addrof_Type, Name ("unsigned int16&"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Uint32_Addrof_Type);
      Set_Name      (Uint32_Addrof_Type, Name ("unsigned int32&"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Uint64_Addrof_Type);
      Set_Name      (Uint64_Addrof_Type, Name ("unsigned int64&"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Int8_Addrof_Type);
      Set_Name      (Int8_Addrof_Type, Name ("int8&"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Int16_Addrof_Type);
      Set_Name      (Int16_Addrof_Type, Name ("int16&"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Int32_Addrof_Type);
      Set_Name      (Int32_Addrof_Type, Name ("int32&"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Int64_Addrof_Type);
      Set_Name      (Int64_Addrof_Type, Name ("int64&"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Float32_Addrof_Type);
      Set_Name      (Float32_Addrof_Type, Name ("float32&"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Float64_Addrof_Type);
      Set_Name      (Float64_Addrof_Type, Name ("float64&"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Bool_Addrof_Type);
      Set_Name      (Bool_Addrof_Type, Name ("bool&"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Char_Addrof_Type);
      Set_Name      (Char_Addrof_Type, Name ("char&"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = UInt_Type);
      Set_Name      (UInt_Type, Name ("unsigned int32"));

      Predef_Type := New_Type (Long_Kind);
      pragma Assert (Predef_Type = ULong_Type);
      Set_Name      (ULong_Type, Name ("unsigned int64"));

      Predef_Class := New_Class;
      pragma Assert (Predef_Class = Async_Result);
      Set_Name       (Async_Result, Name ("IAsyncResult"));
      Set_Pkg_Name   (Async_Result, Str_Id ("[mscorlib]System"));
      Set_Superclass (Async_Result, Java_Lang_Object);
      Set_Is_Public  (Async_Result);

      Predef_Type := New_Type (Class_Kind);
      pragma Assert (Predef_Type = Async_Result_Type);
      Set_Name      (Predef_Type, Name ("[mscorlib]System.IAsyncResult"));
      Set_Class     (Predef_Type, Async_Result);

      Predef_Class := New_Class;
      pragma Assert (Predef_Class = Async_Callback);
      Set_Name       (Async_Callback, Name ("AsyncCallback"));
      Set_Pkg_Name   (Async_Callback, Str_Id ("[mscorlib]System"));
      Set_Superclass (Async_Callback, Java_Lang_Object);
      Set_Is_Public  (Async_Callback);

      Predef_Type := New_Type (Class_Kind);
      pragma Assert (Predef_Type = Async_Callback_Type);
      Set_Name      (Predef_Type, Name ("[mscorlib]System.AsyncCallback"));
      Set_Class     (Predef_Type, Async_Callback);

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Generic_Type0);
      Set_Name      (Generic_Type0, Name ("!0"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Generic_Type1);
      Set_Name      (Generic_Type1, Name ("!1"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Generic_Type2);
      Set_Name      (Generic_Type2, Name ("!2"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Generic_Type3);
      Set_Name      (Generic_Type3, Name ("!3"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Generic_Type4);
      Set_Name      (Generic_Type4, Name ("!4"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Generic_Type5);
      Set_Name      (Generic_Type5, Name ("!5"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Generic_Type6);
      Set_Name      (Generic_Type6, Name ("!6"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Generic_Type7);
      Set_Name      (Generic_Type7, Name ("!7"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Generic_Type8);
      Set_Name      (Generic_Type8, Name ("!8"));

      --  Declare the default constructor for java.lang.Object

      Predef_Method :=
        New_Method (Java_Lang_Object, Name (".ctor"), Void_Type, False);
      Predef_Method :=
        New_Method (System_Delegate, Name (".ctor"), Void_Type, False,
                    Delegate => True);
      Predef_Param := New_Method_Parameter
        (Predef_Method, "object", Java_Lang_Object_Type);
      Predef_Param := New_Method_Parameter
        (Predef_Method, "method", Native_Int_Type);

      --  Initialize the auxiliary stack

      Aux_Stack := New_Stack (200);
   end Initialize;

   -----------------
   -- Print_Stack --
   -----------------

   procedure Print_Stack is
   begin
      Print_Stack (Active_Stack);
   end Print_Stack;

end JVM;
