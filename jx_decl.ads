------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J X _ D E C L                               --
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

with Einfo;    use Einfo;
with JVM;      use JVM;
with J_Stack;
with Types;    use Types;
with Namet;    use Namet;

package Jx_Decl is

   subtype Wrappable_Kind is Elementary_Kind;
   --  This subtype covers the type kinds that require a wrapper for aliased
   --  objects and out parameters.

   function Associated_Class (Ada_Entity : Entity_Id) return Class_Id;
   --  Locates the entity, which may be a library package or type, that is
   --  associated with the JVM class of Ada_Entity, and returns the Class_Id.
   --  Creates the JVM class entity if it does not already exist. Raises an
   --  exception if Ada_Entity has no sensible associated entity corresponding
   --  to a JVM class.

   package Class_Stack is new J_Stack (Class_Id, 200);
   --  Manages a stack of the classes being generated. In general, each
   --  processed class must be stacked because nested classes can occur due to
   --  nested Ada constructs (such as record types within library packages).

   function Current_Class return Class_Id renames Class_Stack.Top;
   --  This is a convenient name for the class actively being generated.

   Current_Compilation_Class : Class_Id := Null_Class;
   --  This denotes the class associated with the compilation unit currently
   --  being compiled. This may be different from Current_Class, since, for
   --  example, classes are generated for types nested within a compilation
   --  unit.

   procedure Declare_Access_Type (Acc_Type : Entity_Id);
   --  Associates Acc_Type with the JVM type corresponding to its designated
   --  type.

   procedure Declare_Array_Type (Arr_Type : Entity_Id);
   --  Creates a new JVM array type entity and associates it with Arr_Type.
   --  Raises an exception if Arr_Type already has an associated JVM type.

   procedure Declare_Discrete_Type (Disc_Type : Entity_Id);
   --  Associates Disc_Type with an appropriate JVM integer type. Raises an
   --  exception if the discrete type already has an associated JVM type.

   procedure Declare_Exception_Class (Exc : Entity_Id);
   --  Creates a new JVM class entity and associates it with Exc. Raises an
   --  exception if the exception already has an associated JVM class.

   procedure Declare_Field (Class : Class_Id; Obj_Or_Comp : Entity_Id);
   --  Creates a new JVM field entity for the given class, whose type is Etype
   --  (Obj_Or_Comp), and associates it with Obj_Or_Comp, which must be an
   --  object or component entity. Raises an exception if Obj_Or_Comp already
   --  has an associated JVM field.

   procedure Declare_Fixed_Point_Type (Fixed_Type : Entity_Id);
   --  Associates Fixed_Type with an appropriate JVM integer type. Raises an
   --  exception if the fixed-point type already has an associated JVM type.

   procedure Declare_Floating_Point_Type (Flt_Type : Entity_Id);
   --  Associates Flt_Type with an appropriate JVM floating point type. Raises
   --  an exception if the discrete type already has an associated JVM type.

   function Declare_Interface_Wrapper_Method
     (Wrapped_Prim : Entity_Id;
      Target_Prim  : Entity_Id) return Method_Id;
   --  Creates a new JVM method associated with the class of Target_Prim, and
   --  associate it the JVM parameters required to match the JVM parameters of
   --  Wrapped_Prim (according to the VM rules). Create also its body with a
   --  static call to Target_Prim.

   procedure Declare_Label (Label : Entity_Id);
   --  Creates a new JVM label entity and associates it with Label, which must
   --  be an E_Label entity. Raises an exception if Label already has an
   --  associated JVM label.

   procedure Declare_Local_Variable (Object : Entity_Id);
   --  Creates a new JVM local variable entity whose type is Etype (Object),
   --  and associates it with Obj_Or_Comp, which must be an E_Variable or
   --  E_Component. Raises an exception if Obj_Or_Comp already has an
   --  associated JVM local variable.

   procedure Declare_Method (Class : Class_Id; Subp : Entity_Id);
   --  Creates a new JVM method entity for the given class, along with its
   --  associated JVM parameters, and associates it with Subp, which must be a
   --  subprogram entity. Raises an exception if Obj_Or_Comp already has an
   --  associated JVM method.

   procedure Declare_Package_Class (Pkg_Spec : Entity_Id);
   --  Creates a new JVM class entity and associates it with Pkg_Spec. Raises
   --  an exception if the package already has an associated JVM class.

   procedure Declare_Record_Class (Rec_Type : Entity_Id);
   --  Creates a new JVM class entity and associates it with Rec_Type. Raises
   --  an exception if the record type already has an associated JVM class.

   procedure Declare_Type (Typ : Entity_Id);
   --  Creates a new JVM type entity and associates it with Typ. Raises an
   --  exception if the type already has an associated JVM type.

   function Deep_Clone_Class (Typ : Entity_Id) return Class_Id;
   --  Returns the Class_Id for the class associated with the deep clone method
   --  of Typ, which must be a record type.

   function Deep_Clone_Method_Name (Typ : Entity_Id) return Name_Id;
   --  Returns a Name_Id for the deep clone method to be associated with Typ,
   --  which must be a record type.

   function Deep_Copy_Class (Typ : Entity_Id) return Class_Id;
   --  Returns the Class_Id for the class associated with the deep copy method
   --  of Typ, which must be an array type or a record type.

   function Deep_Copy_Method_Name (Typ : Entity_Id) return Name_Id;
   --  Returns a Name_Id for the deep copy method to be associated with Typ,
   --  which must be an array type or a record type.

   function Full_Type (Ada_Node : Node_Id) return Entity_Id;
   --  Utility function that returns the underlying type of the base type of
   --  the expression or typed entity denoted by Ada_Node.

   function Full_Subtype (Ada_Node : Node_Id) return Entity_Id;
   --  Utility function that returns the underlying subtype of the expression
   --  or typed entity denoted by Ada_Node. This is used when a subtype is
   --  required (e.g., for retrieving array bounds) and the full view of a
   --  private type is a subtype.

   procedure Generate_Array_Bounds_Formals
     (Formal : Entity_Id;
      Method : Method_Id);
   --  Creates the lower and upper bound formals for each dimension of a formal
   --  of an unconstrained array type and associates them as the next formal
   --  parameters of the given method.

   procedure Generate_Class_Init_Method (Class : Class_Id);
   --  Creates the <clinit> method for the class

   procedure Generate_Default_Constructor (Class : Class_Id);
   --  Creates the class's <init> method

   procedure Handle_Pragma (P : Node_Id);
   --  Handle given pragma node if needed

   function Has_Nondispatching_Method (Subp : Entity_Id) return Boolean;
   --  Returns True if and only if Subp is a dispatching subprogram that
   --  requires an associated nondispatching method. Such a method will be
   --  associated with Subp (by Declare_Method) as long as Subp denotes a
   --  nonabstract dispatching operation that is not declared within a
   --  Java-convention scope.

   function Is_Value_Type (Class : Class_Id) return Boolean;
   --  Returns True if Class represents a value type.

   function JVM_Class (Ada_Entity : Entity_Id) return Class_Id;
   --  Returns the Class_Id associated with the given Ada entity. Creates a new
   --  JVM class entity if one is not already associated with Ada_Entity.

   function JVM_Entity_Name
     (E        : Entity_Id;
      Undotted : Boolean := False) return Name_Id;
   --  Returns a name id denoting the appropriate JVM name assigned to an Ada
   --  entity. This is only defined for entities that are compilation units,
   --  types for which a class will be generated, subprograms, exceptions,
   --  components and global variables that are associated with Java fields.

   function JVM_Expanded_Name
     (E     : Entity_Id;
      Full  : Boolean := True)
      return  String;
   --  Returns an expanded name based on concatentating the entity's simple
   --  name with a prefix given by the expanded name of the entity's containing
   --  scope, using a '$' character as a separator between simple names. If
   --  Full is False, then doesn't append the expanded name of the innermost
   --  enclosing compilation unit, but only expands to the outermost scope
   --  nested within the compilation unit (but this option only applies if
   --  the entity is not itself a compilation unit).

   function JVM_Field (Ada_Entity : Entity_Id) return Field_Id;
   --  Returns the Field_Id associated with the given Ada entity, which must
   --  have an Ekind which is either E_Variable, E_Constant, E_Component, or
   --  E_Discriminant. Creates a new JVM field entity if one is not already
   --  associated with Ada_Entity. Also creates and associates a new class
   --  entity for the innermost enclosing library package or record type if
   --  such a class entity does not already exist. ???

   function JVM_Label (Label : Entity_Id) return Label_Id;
   --  Returns the Label_Id associated with the given E_Label entity. Creates
   --  a new JVM label entity if one is not already associated with Label.

   function JVM_Local_Var (Ada_Entity : Entity_Id) return Local_Var_Id;
   --  Returns the Local_Var_Id associated with the given Ada entity, which
   --  must have an Ekind which is either E_Variable or E_Constant. Creates
   --  a new JVM local variable entity if one is not already associated with
   --  Ada_Entity.

   function JVM_Method (Ada_Entity : Entity_Id) return Method_Id;
   --  Returns the Method_Id associated with the given Ada entity, which must
   --  have an Ekind which is E_Procedure or E_Function. Creates a new JVM
   --  method entity if one is not already associated with Ada_Entity. Also
   --  creates and associates a new class entity for the innermost enclosing
   --  library package or associated tagged type if such a class entity does
   --  not already exist. ???

   function JVM_Type (Ada_Node : Node_Id) return Type_Id;
   --  Returns the Type_Id associated with the Etype of the given node (which
   --  must be a typed entity or expression node). Creates a new JVM type
   --  entity if one is not already associated with the Etype. If Ada_Node has
   --  an empty Etype, then returns Void_Type (e.g., E_Procedure).

   package Method_Stack is new J_Stack (Method_Id, 200);
   --  Manages a stack of methods being generated. In general, methods must be
   --  stacked because of the order of processing, which involves traversing
   --  the Ada unit recursively, and since subprograms can be nested.

   function Overrides_Interface_Op
     (New_Subp     : Entity_Id;
      Intface_Subp : Entity_Id)
      return         Boolean;
   --  Returns true if and only if the subprogram Intface_Subp belonging to a
   --  tagged type mapped onto a Java interface can be overridden by New_Subp.
   --  This requires that the first parameter of each subprogram is a
   --  controlling formal and that all succeeding parameter and result types of
   --  New_Subp are the same as the corresponding parameter and result types of
   --  Intface_Subp.

   function Package_Name (N : Node_Id) return String_Id;
   --  Returns a String_Id denoting the symbolic name for the Java package that
   --  should be associated with a class generated for N. Returns No_String for
   --  the normal case where no such package is predefined. (Currently the
   --  cases where this returns a String_Id other than No_String are for GNAT
   --  library units, which are associated with the Java package defined by the
   --  string constant JVM.API.Ada_Lib_Package, and for tagged types declared
   --  within a child unit which is imported with convention Java.)

end Jx_Decl;
