------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 4                                --
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

--  This package implements translation of Ada names and expressions
--  into their corresponding JVM equivalents, as well as providing
--  various utility routines and types used by other packages.

with JVM;     use JVM;
with Types;   use Types;

package Jx_Ch4 is

   --  Descriptor of the value stored in the J-code stack by Evaluate_Addr. It
   --  allows the generation of additional references to such entity without
   --  the need to re-evaluate it.

   type Address_Kind is
     (No_Address,
      Local_Address,
      Valuetype_Address,
      Field_Address,
      Indexed_Address,
      Object_Address,
      Array_Address);

   type Address_Descriptor (Addr_Kind : Address_Kind := No_Address) is record
      case Addr_Kind is
         when No_Address =>
            null;

         when Local_Address =>
            Local_Var        : Local_Var_Id;

         when Valuetype_Address =>
            EType            : Type_Id;

         when Field_Address =>
            Field            : Field_Id;

         when Indexed_Address =>
            Comp_Type        : Type_Id;

         when Array_Address =>
            Is_Slice         : Boolean;
            Descriptor_Class : Class_Id;

         when Object_Address =>
            null;
      end case;
   end record;

   procedure Add_Implicit_Index_Conversion;
   --  In the VM target the index of arrays must be an integer; if the value
   --  stored in the top of the stack is a Long_Integer then generate a
   --  conversion to Integer. Required by the Java Verifier.

   function Array_Index_First (Index : Node_Id) return Node_Id;
   --  Returns the low bound associated with the index of an array subtype

   function Array_Index_Last (Index : Node_Id) return Node_Id;
   --  Returns the high bound associated with the index of an array subtype

   procedure Check_Flat_Array
     (Arr_LV      : Local_Var_Id;
      Is_Flat_Lbl : Label_Id);
   --  Generates code to check if the array stored in Arr_LV is a flat array.
   --  If true the flow of control of the generated code jumps to label
   --  Is_Flat_Lbl.

   function Evaluate_Addr (Obj_Name : Node_Id) return Address_Descriptor;
   --  Generates J-code to evaluate the given object name (if needed) and
   --  returns an Address_Descriptor describing the location of the object.
   --  This routine is mainly used to evaluate the Ada attributes 'Access and
   --  'Address as well as arguments of subprogram calls.

   procedure Evaluate_Array_Address (Arr : Node_Id);
   --  Generates code to load the array reference denoted by the name Arr.
   --  If Arr is an N_Explicit_Dereference for an access-to-unconstrained
   --  array then this will load the 'all' field from the referenced array
   --  descriptor. This routine should only be called if such a dereferencing
   --  is appropriate (i.e., no bound information will be needed by the
   --  context), otherwise either Evaluate_Addr or Evaluate_Expr should
   --  be called. Raises an exception if Arr does not denote an array.

   procedure Evaluate_Expr (Expr : Node_Id);
   --  Generates J-code to evaluate Expr.

   procedure Evaluate_Expr (Expr : Node_Id; Check_Subtype : Entity_Id);
   --  Generates J-code to evaluate Expr and perform a scalar range check
   --  against the range of Check_Subtype when Do_Range_Check is set on Expr.

   procedure Evaluate_Expr
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean);
   --  Generates J-code to evaluate Expr. Label is used to provide a target for
   --  the evaluation of a conditional expression (Label = Null_Label if no
   --  target available). If Label /= Null_Label, then True_Branch indicates
   --  whether the branch should occur when the condition is True or when it's
   --  False.
   --
   --  Notes: The evaluation of explicit dereferences of unconstrained arrays
   --  does NOT generate code referencing the "all" field of the array
   --  descriptor to handle attributes 'first and 'last.

   --  1. For slices, this routine only evaluation the prefix. The caller must
   --     take care of the slice itself.

   --  2. For dereferences of unconstrained arrays this routine does not
   --     generate code to dereference the array descriptor. Reason: The caller
   --     may need the descriptor to evaluate the array attributes.

   --     Example: Ptr_Unconstr_Table.all'First

   procedure Evaluate_String_Literal
     (Str_Lit  : String_Id;
      Str_Type : Entity_Id);
   --  Creates a constant pool entry for Str_Lit of type Str_Type and
   --  generates code to load its value and convert it to an Ada string.

   procedure Evaluate_With_Copy (Expr : Node_Id);
   --  Generates code to evaluate Expr, performing a copy of the value in the
   --  case of an expression with a composite type (except in the case of an
   --  expression given by an aggregate or function call, in which case no copy
   --  is needed). This form of expression evaluation can be used in contexts
   --  that require a new object to be created, such as initialized object
   --  declarations, aggregate associations, and function return statements.

   procedure Gen_Array_Subscript
     (Prefix          : Node_Id;
      First_Subscript : Node_Id;
      Array_Var       : Local_Var_Id := Null_Local_Var);
   --  Generates code to evaluate the array index expression and normalize it
   --  (that is, adjust it by the lower bound of the array object denoted by
   --  Prefix or by the contents of the array descriptor stored in the top of
   --  the stack). If this is a case of indexing a multidimensional array, then
   --  the remainder of the subscripts (successors of First_Subscript) will
   --  also be evaluated to produce the address of the final indexed array
   --  element.
   --
   --  Note: The caller is responsible of passing the array (or the array
   --        descriptor) on the stack; otherwise Array_Var must be non null
   --        and designate a local variable containing the array.
   --
   --  Note: When an array descriptor is passed in the stack, it is internally
   --        used by this routine to obtain the value of the lower bound of the
   --        array. On exit the array descriptor is dereferenced therefore
   --        replaced by the associated array.
   --
   --  Note: Do not call Evaluate_Array_Addresss before calling this routine
   --        because, in the general case, the evaluation of the array prefix
   --        may leave an array descriptor in the stack and this routine needs
   --        it to know the value of the attributes (and Evaluate_Array_Address
   --        implicitly dereferences it).

   procedure Gen_Scalar_Subtype_Check
     (Scalar_Subt : Entity_Id;
      Node : Node_Id);
   --  Generate code to check the top-of-stack value against the constraint
   --  (if any) of the scalar subtype Scalar_Subt. Errors are reported relative
   --  to Node.

   function Index_First (Array_Subtype : Entity_Id) return Node_Id;
   --  Returns the low bound associated with the first dimension of the given
   --  array subtype.

   function Index_Last (Array_Subtype : Entity_Id) return Node_Id;
   --  Returns the high bound associated with the first dimension of the given
   --  array subtype.

   function JVM_Expr_Type (Expr : Node_Id) return Type_Id;
   --  Returns the JVM type entity associated with the type of the expression

   procedure Load_Array_Bounds (Arr_Expr : Node_Id; Dimension : Pos);
   --  Loads the bounds of the array denoted by Arr_Expr. Assumes that Arr_Expr
   --  has already been evaluated, with the resulting reference currently on
   --  the top of the stack. Dimension is the array's dimension.

   procedure Load_Index_Length
     (Index  : Node_Id;
      Obj_LV : Local_Var_Id := Null_Local_Var);
   --  Generates J-code to load the length of the given array index (computed
   --  as Max (0, high bound - low bound + 1)). If Obj_LV is not null, then any
   --  discriminant bounds will be obtained by loading the discriminant from
   --  the object denoted by the the local variable.

   procedure Store_Elementary_Value (Target : Address_Descriptor);
   --  Generates J-code to store the top-of-stack value into the object whose
   --  location is described by Target. Mainly used in the code generation of
   --  the assignment statement.

   procedure Test_For_Slice
     (N     : Node_Id;
      Slice : out Boolean;
      Prfix : out Node_Id;
      Subt  : out Entity_Id);
   --  Tests if the expression denoted by N is a slice or a qualified
   --  expression or conversion with a slice argument, and if so returns
   --  True in Slice, the slice prefix in Prfix, and the subtype of the
   --  slice in Subt. Otherwise, returns False in Slice, N in Prfix, and
   --  Etype (N) in Subt.

end Jx_Ch4;
