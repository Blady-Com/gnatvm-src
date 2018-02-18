------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M . I N F O                              --
--                                                                          --
--                                 S p e c                                  --
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

--  JVM.Info defines the JVM entities that serve as the JGNAT back end's symbol
--  table. The package exports operations for creating, updating, and querying
--  entities for the several forms of JVM 'id' types declared in the
--  specification of the JVM package, which correspond to classes, fields,
--  methods, types, local variables code labels, and subroutines. Note that
--  JVM.Info is currently defined as a private child unit of JVM, and thus is
--  available only to the implemention of the JVM interface. The package may
--  later be made public if we determine that the upper levels of the back end
--  require the means for direct access and update of JVM entity information.

with JVM.Code;  use JVM.Code;
with JVM.Stack; use JVM.Stack;
with JVM_File;  use JVM_File;

private
package JVM.Info is

   ------------------------------------
   -- JVM Entity Creation Operations --
   ------------------------------------

   function New_Class return Class_Id;
   --  Creates a class entity and returns a reference to it

   function New_Entity_Ref return JVM_Entity_Ref;
   --  Creates a node that constains a reference to a JVM entity

   function New_Field return Field_Id;
   --  Creates a field entity and returns a reference to it

   function New_Interface return Class_Id;
   --  Creates an interface entity and returns a reference to it

   function New_Label return Label_Id;
   --  Creates a code label and returns a reference to it

   function New_Local_Var return Local_Var_Id;
   --  Creates a local variable entity and returns a reference to it

   function New_Method return Method_Id;
   --  Creates a method entity and returns a reference to it

   function New_Subroutine return Subroutine_Id;
   --  Creates a subroutine entity and returns a reference to it

   function New_Type (Kind : JVM_Type_Kind) return Type_Id;
   --  Creates a type entity of the given kind and returns a reference to it

   ------------------------------------
   -- Class and Interface Attributes --
   ------------------------------------

   procedure Add_Field (Class : Class_Id; Field : Field_Id);
   --  Add Field to the list of fields associated with Class

   procedure Add_Interface (Class : Class_Id; Intrface : Class_Id);
   --  Add an interface to the set of interfaces that class (or interface)
   --  Class implements.

   procedure Add_Method (Class : Class_Id; Method : Method_Id);
   --  Add Method to the list of methods associated with Class

   procedure Add_Nested_Class (Class : Class_Id; Nested_Class : Class_Id);
   --  Add Nested_Class to the list of nested classes associated with class

   procedure Add_Pool_Item (Class : Class_Id; CP_Item : Pool_Id);
   --  Add CP_Item to the list of pool items associated with Class

   procedure Add_Pool_Ref (Class : Class_Id; CP_Item : Pool_Id);
   --  Adds a constant pool reference item to the list of constant pool items
   --  denoting references to Class.

   function Class_Type (Class : Class_Id) return Type_Id;
   --  Returns the class type associated with C

   function First_Field (Class : Class_Id) return Field_Id;
   --  Returns the first in the list of fields of class C; returns Null_Field
   --  if the class has no fields.

   function First_Interface_Ref (Class : Class_Id) return JVM_Entity_Ref;
   --  Returns the first of the list of references to interfaces that the class
   --  (or interface) implements; returns Null_Entity_Ref if there is no such
   --  interface.

   function First_Method (Class : Class_Id) return Method_Id;
   --  Returns the first in the list of methods of Class; returns Null_Method
   --  if the class has no methods.

   function First_Nested_Class (Class : Class_Id) return Class_Id;
   --  Returns the first in the list of inner classes of class Class; returns
   --  Null_Class if the class has no inner classes.

   function First_Pool_Item (Class : Class_Id) return Pool_Id;
   --  Returns the first in the list of pool items of Class; returns
   --  Null_Pool_Item if the class has no pool items.

   function First_Pool_Ref (Class : Class_Id) return Pool_Id;
   --  Returns a reference to the first in a list of constant pool items
   --  denoting references to Class (or Null_Pool_Item if none).

   function Get_Interface (I_Ref : JVM_Entity_Ref) return Class_Id;
   --  Returns the Class_Id for the interface entity referenced by I_Ref;
   --  raises an exception if I_Ref is null or if I_Ref does not refer to an
   --  interface entity.

   function Is_Abstract (Class : Class_Id) return Boolean;
   --  Returns true if and only if Class denotes an abstract class

   function Is_Built (Class : Class_Id) return Boolean;
   --  Returns true if and only if the class file of Class has been generated
   function Is_Final (Class : Class_Id) return Boolean;
   --  Returns true if and only if Class denotes a final class

   function Is_Interface (Class : Class_Id) return Boolean;
   --  Returns true if and only if Class denotes an interface

   function Is_Open (Class : Class_Id) return Boolean;
   --  Returns true if and only if Class has been opened via a call to
   --  Set_Is_Open (and has not yet been closed).

   function Is_Public (Class : Class_Id) return Boolean;
   --  Returns true if and only if Class denotes a public class

   function Name (Class : Class_Id) return Name_Id;
   --  Returns the simple name of the class

   function Next_Interface_Ref (I_Ref : JVM_Entity_Ref) return JVM_Entity_Ref;
   --  Returns the next reference in a list of interface references; returns
   --  Null_Entity_Ref if I_Ref has no successor.

   function Outer_Class (Class : Class_Id) return Class_Id;
   --  Returns an inner class's enclosing class; returns Null_Class if C does
   --  not denote an inner class.

   function Pkg_Name (Class : Class_Id) return String_Id;
   --  Returns the name of the package associated with the class

   procedure Set_Class_Type (Class : Class_Id; Class_Type : Type_Id);
   --  Associates Class with the given class type

   procedure Set_Is_Abstract (Class : Class_Id; Value : Boolean := True);
   --  Defines whether the class is abstract

   procedure Set_Is_Built (Class : Class_Id; Value : Boolean := True);
   --  Defines whether the class file of Class has been generated

   procedure Set_Is_Final (Class : Class_Id; Value : Boolean := True);
   --  Defines whether the class is a final class

   procedure Set_Is_Interface (Class : Class_Id; Value : Boolean := True);
   --  Defines whether the class denotes a Java interface

   procedure Set_Is_Open (Class : Class_Id; Value : Boolean := True);
   --  Establishes the class as open (Value = True) or closed (Value = False)

   procedure Set_Is_Public (Class : Class_Id; Value : Boolean := True);
   --  Defines whether the class is public

   procedure Set_Name (Class : Class_Id; Name : Name_Id);
   --  Sets the simple name of the class

   procedure Set_Outer_Class (Class : Class_Id; Parent : Class_Id);
   --  Sets the outer class of Class to Parent. A call to this procedure has
   --  the effect of defining Class to be an inner class.

   procedure Set_Pkg_Name (Class : Class_Id; Name : String_Id);
   --  Sets the name of the package associated with the class

   procedure Set_Src_Name (Class : Class_Id; Name : Name_Id);
   --  Sets the name of the source file associated with the class

   procedure Set_Superclass (Class : Class_Id; Superclass : Class_Id);
   --  Sets the superclass of Class to Superclass

   function Src_Name (Class : Class_Id) return Name_Id;
   --  Returns the name of the source file associated with the class

   function Superclass (Class : Class_Id) return Class_Id;
   --  Returns the class's superclass

   -------------------------
   -- JVM Type Attributes --
   -------------------------

   type JVM_Type_Size is range 1 .. 2;  -- Sizes are either one or two words

   procedure Add_Pool_Ref (Typ : Type_Id; CP_Item : Pool_Id);
   --  Adds a constant pool reference item to the list of constant pool items
   --  denoting references to Typ.

   function Class (Class_Type : Type_Id) return Class_Id;
   --  Returns a reference to the class entity associated with a class type;
   --  raises an exception if Class_Type does not denote a class type.

   function Descriptor_Type (Typ : Type_Id) return Type_Id;
   --  Returns the descriptor associated with Typ (if available). Otherwise
   --  returns Null_Type.

   function Dimensions (Array_Type : Type_Id) return Pos_8;
   --  Returns the number of dimensions of an array type; raises an exception
   --  if Array_Type does not denote an array type.

   function Element_Type (Array_Type : Type_Id) return Type_Id;
   --  Returns the element type of an array type; raises an exception if
   --  Array_Type does not denote an array type.

   function First_Pool_Ref (Typ : Type_Id) return Pool_Id;
   --  Returns a reference to the first in a list of constant pool items
   --  denoting references to Typ (or Null_Pool_Item if none).

   function Is_Array_Descriptor (Typ : Type_Id) return Boolean;
   --  Returns true if and only if Typ denotes the descriptor of an array

   function Is_Descriptor (Typ : Type_Id) return Boolean;
   --  Returns true if Typ denotes a descriptor

   function Name (Typ : Type_Id) return Name_Id;
   --  Returns the simple name of the type entity

   procedure Set_Class (Class_Type : Type_Id; Class : Class_Id);
   --  Associates a class with its class type; raises an exception if
   --  Class_Type does not denote a class type.

   procedure Set_Descriptor_Type (Typ : Type_Id; Value : Type_Id);
   --  Registers the descriptor of Typ

   procedure Set_Dimensions (Array_Type : Type_Id; Dimensions : Pos_8);
   --  Sets the number of dimensions of an array type; raises an exception if
   --  Array_Type does not denote an array type.

   procedure Set_Element_Type (Array_Type : Type_Id; Elmt_Type : Type_Id);
   --  Sets the element type of an array type; raises an exception if
   --  Array_Type does not denote an array type.

   procedure Set_Is_Array_Descriptor (Typ : Type_Id; Value : Boolean := True);
   --  Defines whether the class denotes an array descriptor

   procedure Set_Is_Descriptor (Typ : Type_Id; Value : Boolean := True);
   --  Defines wether the type denotes a descriptor

   procedure Set_Name (Typ : Type_Id; Name : Name_Id);
   --  Sets the simple name of the type entity

   function Type_Kind (Typ : Type_Id) return JVM_Type_Kind;
   --  Returns the type's associated kind

   function Word_Size (Typ : Type_Id) return JVM_Type_Size;
   --  Returns the size allocated in words for variables and stack elements of
   --  the given type.

   ----------------------
   -- Field Attributes --
   ----------------------

   function Access_Mode (Field : Field_Id) return Member_Access;
   --  Returns the access mode associated with the field

   procedure Add_Pool_Ref (Field : Field_Id; CP_Item : Pool_Id);
   --  Adds a constant pool reference item to the list of constant pool items
   --  denoting references to Field.

   function Class (Field : Field_Id) return Class_Id;
   --  Returns the field's associated class

   function Field_Type (Field : Field_Id) return Type_Id;
   --  Returns the type of the field

   function First_Pool_Ref (Field : Field_Id) return Pool_Id;
   --  Returns a reference to the first in a list of constant pool items
   --  denoting references to Field (or Null_Pool_Item if none).

   function Is_Final (Field : Field_Id) return Boolean;
   --  Returns true if and only if Field denotes a final field

   function Is_Static (Field : Field_Id) return Boolean;
   --  Returns true if and only if Field denotes a static field

   function Is_Volatile (Field : Field_Id) return Boolean;
   --  Returns true if and only if Field denotes a volatile field

   function Name (Field : Field_Id) return Name_Id;
   --  Returns the simple name of the field

   function Next_Field (Field : Field_Id) return Field_Id;
   --  Returns a reference to the field that is the successor of Field; returns
   --  Null_Field if Field has no successor.

   function Next_Nested_Class (Class : Class_Id) return Class_Id;
   --  Returns a reference to the inner class that is the successor of Class;
   --  returns Null_Class is Class has no successor.

   procedure Set_Access_Mode (Field : Field_Id; Acc_Mode : Member_Access);
   --  Defines the access mode of the field

   procedure Set_Class (Field : Field_Id; Class : Class_Id);
   --  Associates the field with Class

   procedure Set_Field_Type (Field : Field_Id; Typ : Type_Id);
   --  Sets the type of the field

   procedure Set_Is_Final (Field : Field_Id; Value : Boolean := True);
   --  Defines whether the field is a final field

   procedure Set_Is_Static (Field : Field_Id; Value : Boolean := True);
   --  Defines whether the field is static

   procedure Set_Is_Volatile (Field : Field_Id; Value : Boolean := True);
   --  Defines whether the field is volatile

   procedure Set_Name (Field : Field_Id; Name : Name_Id);
   --  Sets the simple name of the field

   -----------------------
   -- Method Attributes --
   -----------------------

   function Access_Mode (Method : Method_Id) return Member_Access;
   --  Returns the access mode associated with the method

   function Active_Subroutine (Method : Method_Id) return Subroutine_Id;
   --  Returns the active subroutine for the given method; returns
   --  Null_Subroutine if no subroutine is desigated as active.

   procedure Add_Label (Method : Method_Id; Label : Label_Id);
   --  Adds a new label to the set of labels of the method

   procedure Add_Local_Var (Method : Method_Id; Local : Local_Var_Id);
   --  Adds Local as the next local variable of the method. Implicitly
   --  increments the value of the method's local variable index according to
   --  the size of the variable.

   procedure Add_Pool_Ref (Method : Method_Id; CP_Item : Pool_Id);
   --  Adds a constant pool reference item to the list of constant pool items
   --  denoting references to Method.

   procedure Add_Subroutine (Method : Method_Id; Subr : Subroutine_Id);
   --  Adds a new subroutine to the method

   function Class (Method : Method_Id) return Class_Id;
   --  Returns the method's associated class

   function Class_Of_Wrapped_Interface (Method : Method_Id) return Class_Id;
   --  Given an interface wrapper returns the class_id of the wrapped interface
   --  Note that Class (Method) returns the target class associated with the
   --  interface wrapper.

   function Exported_Stdcall (Method : Method_Id) return String_Id;
   --  Returns /= No_String if Exported Stdcall method

   function First_Label (Method : Method_Id) return Label_Id;
   --  Returns the first code label of the method; returns Null_Label if the
   --  method has no labels

   function First_Local_Var (Method : Method_Id) return Local_Var_Id;
   --  Returns the first local variable of the method; returns Null_Local_Var
   --  if the method has no local variables

   function First_Pool_Ref (Method : Method_Id) return Pool_Id;
   --  Returns a reference to the first in a list of constant pool items
   --  denoting references to Method (or Null_Pool_Item if none).

   function First_Subroutine (Method : Method_Id) return Subroutine_Id;
   --  Returns the first subroutine of the method; returns Null_Subroutine if
   --  the method has no subroutines

   function Has_AR_SL_Formal (Method : Method_Id) return Boolean;
   --  Returns true if the method has an extra formal containing the static
   --  link

   function Is_Abstract (Method : Method_Id) return Boolean;
   --  Returns true if and only if Method denotes an abstract method

   function Is_AR_Method (Method : Method_Id) return Boolean;
   --  Returns true if and only if Method is a method of an AR class

   function Is_Delegate (Method : Method_Id) return Boolean;
   --  Returns true if and only if Method denotes a delegate constructor

   function Is_Exception_Block (Method : Method_Id) return Boolean;
   --  Returns true if and only if Method is currently generating handled code

   function Is_Final (Method : Method_Id) return Boolean;
   --  Returns true if and only if Method denotes a final method

   function Is_Interface_Wrapper (Method : Method_Id) return Boolean;
   --  Returns true if and only if Method denotes an interface wrapper

   function Is_Open (Method : Method_Id) return Boolean;
   --  Returns true if and only if Method has been opened via a call to
   --  Set_Is_Open (and has not yet been closed).

   function Is_Stack_Checking (Method : Method_Id) return Boolean;
   --  Returns true if and only if Method has operand stack checking enabled

   function Is_Static (Method : Method_Id) return Boolean;
   --  Returns true if and only if Method denotes a static method

   function Is_Synchronized (Method : Method_Id) return Boolean;
   --  Returns true if and only if Method denotes a synchronized method

   function Max_Stack_Depth (Method : Method_Id) return Depth_Range;
   --  Returns the maximum word depth of the method's associated operand stack
   --  (only valid to call this after the method's code has been fully
   --  generated and the method is closed).

   function Method_Code (Method : Method_Id) return Code_Sequence;
   --  Returns the code sequence associated with the method

   function Method_Handlers (Method : Method_Id) return Handler_Sequence;
   --  Returns the exception handler sequence associated with the method

   function Name (Method : Method_Id) return Name_Id;
   --  Returns the simple name of the method

   function Next_Local_Index (Method : Method_Id) return Local_Variable_Index;
   --  Returns the next available index for local variables of the method. The
   --  index of a method is initially 0, and the index gets incremented
   --  implicitly by calls to Add_Local_Var.

   function Next_Method (Method : Method_Id) return Method_Id;
   --  Returns a reference to the method that is the successor of Method;
   --  returns Null_Method if Method has no successor.

   function Op_Stack (Method : Method_Id) return Op_Stack_Id;
   --  Returns the method's associated operand type stack

   function Parent_Method (Method : Method_Id) return Method_Id;
   --  Returns the Method_Id of the parent method associated Method, else
   --  returns Null_Method if there is none.

   function Result_Type (Method : Method_Id) return Type_Id;
   --  Returns the result type of the method

   procedure Set_Access_Mode (Method : Method_Id; Acc_Mode : Member_Access);
   --  Defines the access mode of the method

   procedure Set_Active_Subroutine (Method : Method_Id; Subr : Subroutine_Id);
   --  Establishes the active subroutine for the given method

   procedure Set_Class (Method : Method_Id; Class : Class_Id);
   --  Associates the method with Class

   procedure Set_Class_Of_Wrapped_Interface
     (Method : Method_Id; Class : Class_Id);
   --  Associates the method with the class of its wrapped interface

   procedure Set_Code (Method : Method_Id; Code : Code_Sequence);
   --  Associates the method with the given code sequence

   procedure Set_Exception_Block (Method : Method_Id; Value : Boolean := True);
   --  Establishes if method is currently generating handled code

   procedure Set_Exported_Stdcall (Method : Method_Id; Val : String_Id);
   --  Sets if is an Exported_Stdcall method

   procedure Set_Handlers (Method : Method_Id; Handlers : Handler_Sequence);
   --  Associates the method with the given handler sequence

   procedure Set_Has_AR_SL_Formal
     (Method : Method_Id; Value : Boolean := True);
   --  Defines whether the method has an extra formal with the static link

   procedure Set_Is_Abstract (Method : Method_Id; Value : Boolean := True);
   --  Defines whether the method is abstract

   procedure Set_Is_AR_Method (Method : Method_Id; Value : Boolean := True);
   --  Defines whether the method is part of an AR class

   procedure Set_Is_Delegate (Method : Method_Id; Value : Boolean := True);
   --  Defines whether the method is part of a delegate class

   procedure Set_Is_Final (Method : Method_Id; Value : Boolean := True);
   --  Defines whether the method is a final method

   procedure Set_Is_Interface_Wrapper
     (Method : Method_Id; Value : Boolean := True);
   --  Defines whether the method is an interface wrapper

   procedure Set_Is_Open (Method : Method_Id; Value : Boolean := True);
   --  Establishes the method as open (Value = True) or closed (Value = False)

   procedure Set_Is_Static (Method : Method_Id; Value : Boolean := True);
   --  Defines whether the method is static

   procedure Set_Is_Synchronized (Method : Method_Id; Value : Boolean := True);
   --  Defines whether the method is a synchronized method

   procedure Set_Max_Stack_Depth (Method : Method_Id; Depth : Depth_Range);
   --  Establishes the maximum word depth of the method's associated operand
   --  stack.

   procedure Set_Name (Method : Method_Id; Name : Name_Id);
   --  Sets the simple name of the method

   procedure Set_Op_Stack (Method : Method_Id; Stack : Op_Stack_Id);
   --  Associates the method with the given operand type stack;
   --  if Stack = Null_Op_Stack then any stack currently associated with the
   --  method will be freed.

   procedure Set_Parent_Method (Method : Method_Id; Parent : Method_Id);
   --  Associates Method with a parent method denoted by Parent

   procedure Set_Result_Type (Method : Method_Id; Typ : Type_Id);
   --  Sets the result type of the method

   procedure Set_Stack_Checking (Method : Method_Id; Value : Boolean := True);
   --  Establishes the operand stack checking state of the method. The default
   --  state for a method is to have stack checking enabled.

   -------------------------------
   -- Local Variable Attributes --
   -------------------------------

   function Is_Param (Local : Local_Var_Id) return Boolean;
   --  Returns true if and only if Local denotes a parameter

   function Local_Index (Local : Local_Var_Id) return Local_Variable_Index;
   --  Returns the index associated with the local variable

   function Method (Local : Local_Var_Id) return Method_Id;
   --  Returns the method associated with the local variable

   function Name (Local : Local_Var_Id) return Name_Id;
   --  Returns the name of the local variable

   function Next_Local_Var (Local : Local_Var_Id) return Local_Var_Id;
   --  Returns a reference to the local variable that is the successor of
   --  Local; returns Null_Local_Var if Local has no successor.

   procedure Set_Is_Param (Local : Local_Var_Id; Value : Boolean := True);
   --  Defines whether the local variable is a parameter

   procedure Set_Local_Index
     (Local : Local_Var_Id;
      Index : Local_Variable_Index);
   --  Sets the index of the local variable

   procedure Set_Method (Local : Local_Var_Id; Method : Method_Id);
   --  Associates the method with the local variable

   procedure Set_Name (Local : Local_Var_Id; Name : Name_Id);
   --  Sets the name of the local variable

   procedure Set_Variable_Type (Local : Local_Var_Id; Typ : Type_Id);
   --  Sets the type of the local variable

   function Variable_Type (Local : Local_Var_Id) return Type_Id;
   --  Returns the type of the local variable

   ----------------------
   -- Label Attributes --
   ----------------------

   function Code_Index (Label : Label_Id) return Instruction_Index;
   --  Returns the byte code index for the label

   function Is_Targeted (Label : Label_Id) return Boolean;
   --  Returns true if and only if an instruction targeting the label has been
   --  generated.

   function Label_Number (Label : Label_Id) return Natural;
   --  Returns the identifying number associated with the label

   function Location (Label : Label_Id) return Instr_Id;
   --  Returns the id for the instruction associated with the label

   function Method (Label : Label_Id) return Method_Id;
   --  Returns the method associated with the label

   function Name (Label : Label_Id) return Name_Id;
   --  Returns the name of the label

   function Next_Label (Label : Label_Id) return Label_Id;
   --  Returns the label that is the successor of Label; returns Null_Label if
   --  Label has no successor.

   procedure Set_Code_Index (Label : Label_Id; Index : Instruction_Index);
   --  Sets the byte code index for the label

   procedure Set_Is_Targeted (Label : Label_Id; Value : Boolean := True);
   --  Defines whether the label has been targeted by some instruction

   procedure Set_Label_Number (Label : Label_Id; N : Natural);
   --  Associates an identifying number with Label

   procedure Set_Location (Label : Label_Id; Instr : Instr_Id);
   --  Associates the label with the instruction denoted by I

   procedure Set_Method (Label : Label_Id; Method : Method_Id);
   --  Associates the label with the method M

   procedure Set_Name (Label : Label_Id; Name : Name_Id);
   --  Sets the name of the label

   ---------------------------
   -- Subroutine Attributes --
   ---------------------------

   function Is_Open (Subr : Subroutine_Id) return Boolean;
   --  Returns True if and only if the subroutine has not yet been closed via a
   --  call to Close_Subroutine.

   function Max_Stack_Depth (Subr : Subroutine_Id) return Depth_Range;
   --  Returns the maximum word depth of the subroutine's associated operand
   --  stack (only valid to call this after the subroutine's code has been
   --  fully generated and the subroutine is closed).

   function Method (Subr : Subroutine_Id) return Method_Id;
   --  Returns the method associated with the subroutine

   function Name (Subr : Subroutine_Id) return Name_Id;
   --  Returns the name of the subroutine

   function Next_Subroutine (Subr : Subroutine_Id) return Subroutine_Id;
   --  Returns a reference to the subroutine that is the successor of Subr;
   --  returns Null_Subroutine if Subr has no successor.

   function Op_Stack (Subr : Subroutine_Id) return Op_Stack_Id;
   --  Returns the subroutine's associated operand type stack

   procedure Set_Code (Subr : Subroutine_Id; Code : Code_Sequence);
   --  Associates the subroutine with the given code sequence

   procedure Set_Is_Open (Subr : Subroutine_Id; Value : Boolean := True);
   --  Marks the subroutine as open (Value = True) or closed (Value = False)

   procedure Set_Max_Stack_Depth (Subr : Subroutine_Id; Depth : Depth_Range);
   --  Establishes the maximum word depth of the subroutine's associated
   --  operand stack.

   procedure Set_Method (Subr : Subroutine_Id; Method : Method_Id);
   --  Associates the subroutine with Method

   procedure Set_Name (Subr : Subroutine_Id; Name : Name_Id);
   --  Sets the name of the subroutine

   procedure Set_Op_Stack (Subr : Subroutine_Id; Stack : Op_Stack_Id);
   --  Associates the subroutine with the given operand type stack;
   --  if Stack = Null_Op_Stack then any stack currently associated with the
   --  method will be freed.

   procedure Set_Subroutine_Label (Subr : Subroutine_Id; Label : Label_Id);
   --  Associates Label with the subroutine (the label marks the entry point of
   --  the subroutine).

   function Subroutine_Code (Subr : Subroutine_Id) return Code_Sequence;
   --  Returns the code sequence associated with the subroutine

   function Subroutine_Label (Subr : Subroutine_Id) return Label_Id;
   --  Returns the label associated with the entry point of the subroutine

   ---------------------------
   -- JVM Entity References --
   ---------------------------

   function Denoted_Entity (E_Ref : JVM_Entity_Ref) return JVM_Id;
   --  Returns a reference to the JVM entity denoted by E_Ref

   function Get_Entity_Kind (Id : JVM_Id) return JVM_Entity_Kind;
   --  Returns the kind of a JVM entity

   function Next_Entity_Ref (E_Ref : JVM_Entity_Ref) return JVM_Id;
   --  Returns a reference to the successor of E_Ref

   procedure Set_Denoted_Entity (E_Ref : JVM_Entity_Ref; Entity : JVM_Id);
   --  Associates the entity reference E_Ref with Entity

   procedure Set_Next_Entity_Ref
     (E_Ref : JVM_Entity_Ref; Next_E_Ref : JVM_Entity_Ref);
   --  Establishes Next_E_Ref as the successor of E_Ref

   -------------------------
   -- Debugging Utilities --
   -------------------------

   procedure Print_JVM_Entity (Id : JVM_Id);
   --  Print debugging info about a JVM entity

   procedure PJ (Id : JVM_Id);
   --  The same as Print_JVM_Entity (shortened name for debugging convenience)

end JVM.Info;
