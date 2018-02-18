------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 4                                --
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

with Atree;         use Atree;
with Debug;         use Debug;
with Debug_A;       use Debug_A;
with Einfo;         use Einfo;
with Elists;        use Elists;
with Errout;        use Errout;
with Exp_Tss;       use Exp_Tss;
with Exp_Util;      use Exp_Util;
with J_Basics;      use J_Basics;
with J_Descriptors; use J_Descriptors;
with J_String;      use J_String;
with J_Types;       use J_Types;
with JVM.API;       use JVM.API;
with Jx_Ch3;        use Jx_Ch3;
with Jx_Ch5;        use Jx_Ch5;
with Jx_Ch6;        use Jx_Ch6;
with Jx_Ch7;        use Jx_Ch7;
with Jx_Ch11;       use Jx_Ch11;
with Jx_Decl;       use Jx_Decl;
with Jx_Drive;      use Jx_Drive;
with Jx_Uplev;      use Jx_Uplev;
with Lib;           use Lib;
with Namet;         use Namet;
with Nlists;        use Nlists;
with Osint;
with Sem_Aux;       use Sem_Aux;
with Sem_Disp;      use Sem_Disp;
with Sem_Eval;      use Sem_Eval;
with Sem_Util;      use Sem_Util;
with Sinfo;         use Sinfo;
with Snames;        use Snames;
with Stand;         use Stand;
with Stringt;       use Stringt;
with Targparm;      use Targparm;
with Uintp;         use Uintp;
with Urealp;        use Urealp;

package body Jx_Ch4 is

   procedure Check_For_Overflow (N : Node_Id);
   --  Generates code to check that the integer value on the top of stack is in
   --  the base range of the type of N, but only if Do_Overflow_Check is set on
   --  N. Currently requires that N denote an N_Type_Conversion.

   procedure Evaluate_Acc_Prot_Subp_Comparison
     (Op          : Node_Kind;
      Acc_Type    : Entity_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean);
   --  Generates code to evaluate a comparison of values of type Acc_Type
   --  which must be an access-to-protected-subprogram type. This requires
   --  individually comparing the two access components of such values. It's
   --  too bad that the front end doesn't expand such comparisons into
   --  component comparisons as it does for other record types. ???

   procedure Evaluate_Aggregate (Aggr : Node_Id);
   --  Generates code to evaluate an Ada aggregate, producing an initialized
   --  object and pushing a reference to the object onto the stack.

   procedure Evaluate_Allocator (Allocator : Node_Id);
   --  Generates code to evaluate an Ada allocator.

   procedure Evaluate_And_Then
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean);
   --  Generates code to evaluates an "and then" short-circuit control form.
   --  Label is used to provide a target for the evaluation. If Label /=
   --  Null_Label, then True_Branch indicates whether the branch should
   --  occur when the condition is True or when it's False. If Label is
   --  Null_Label, then the operation will simply produce a Boolean result.

   procedure Evaluate_Array_Aggregate (Aggr : Node_Id);
   --  Generates code to evaluate an array aggregate

   procedure Evaluate_Array_Comparison
     (Op        : Node_Kind;
      Left_Arr  : Node_Id;
      Right_Arr : Node_Id;
      Jtype     : Type_Id);
   --  Generates code to evaluate an array comparison for array type Jtype.
   --  Requires that loads of references to the two arrays have been generated
   --  immediately prior to calling.

   procedure Evaluate_Attribute (Attr : Node_Id);
   --  Generates code to evaluate an Ada attribute

   function Evaluate_Integer_Literal
     (Literal : Node_Id;
      Typ     : Entity_Id) return Uint;
   --  Return the value generated for an integer literal of type Typ

   procedure Evaluate_Integer_Literal (Literal : Node_Id; Typ : Entity_Id);
   --  Generates code to push the value of an integer literal of type Typ

   procedure Evaluate_Membership_Test
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean);
   --  Generates code for a membership test. Label is used to provide a target
   --  for the evaluation. If Label /= Null_Label, then True_Branch indicates
   --  whether the branch should occur when the condition is True or when it's
   --  False. If Label is Null_Label, then the operation will simply produce a
   --  Boolean result.

   procedure Evaluate_Operator
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean);
   --  Generates code to evaluate an Ada operator. Label is used to provide
   --  a target for the evaluation of a conditional expression (Label =
   --  Null_Label if no target available). If Label /= Null_Label, then
   --  True_Branch indicates whether the branch should occur when the
   --  condition is True or when it's False.

   procedure Evaluate_Or_Else
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean);
   --  Generates code to for an "or else" short-circuit control form. Label is
   --  used to provide a target for the evaluation. If Label /= Null_Label,
   --  then True_Branch indicates whether the branch should occur when the
   --  condition is True or when it's False. If Label is Null_Label, then the
   --  operation will simply produce a Boolean result.

   procedure Evaluate_Real_Literal (Literal : Node_Id; Typ : Entity_Id);
   --  Generates code to push the value of a real literal of type Typ

   procedure Evaluate_Record_Aggregate
     (Aggr : Node_Id; Aggr_LV : Local_Var_Id := Null_Local_Var);
   --  Generates code to evaluate a record aggregate. If the Aggr_LV parameter
   --  is null, then the aggregate associations will be evaluated into the
   --  the object designated by Aggr_LV and it is the responsibility of the
   --  caller to load the result of the aggregate evaluation on the stack.
   --  This is currently used to support the evaluation of parent aggregates
   --  in the case of a tagged type extension (see the body of this procedure).

   procedure Evaluate_Subprogram_Access
     (Subp_Name     : Node_Id;
      Subp_Acc_Type : Entity_Id);
   --  Generates code to evaluate an access to a subprogram by creating a
   --  subclass of the class of the attribute's access-to-subprogram type with
   --  a method that will invoke the subprogram. An object of the subclass is
   --  created and its reference is pushed on the stack.

   procedure Evaluate_Unconstrained_Array_Ref
     (Arr_Expr : Node_Id;
      Acc_Type : Entity_Id);
   --  Generates code to construct a compound reference object that denotes
   --  an array value needed in the context of an unconstrained access type
   --  Acc_Type. Used when evaluating N_References and access attributes.

   function In_Runtime (E : Entity_Id) return Boolean;
   --  Check if E is defined in the RTL (in a child of Ada or System). Used
   --  in the JVM compiler to generate the prefix JGNAT.ADALIB in tagged
   --  types declared in the runtime.

   type Array_Attribute_Kind is (First, Last);

   procedure Load_Array_Attr
     (Arr_Expr   : Node_Id;
      Array_Attr : Array_Attribute_Kind;
      Dimension  : Pos_8);
   --  Generates code to load an attribute ('First, 'Last, or 'Length) of the
   --  given array object's specified Dimension.

   -----------------------------------
   -- Add_Implicit_Index_Conversion --
   -----------------------------------

   procedure Add_Implicit_Index_Conversion is
   begin
      pragma Assert (JVM.Type_Kind (Top_Type) = Int_Kind
        or else JVM.Type_Kind (Top_Type) = Long_Kind);

      if JVM.Type_Kind (Top_Type) = Long_Kind then
         Gen_Conversion (Int_Type);
      end if;
   end Add_Implicit_Index_Conversion;

   -----------------------
   -- Array_Index_First --
   -----------------------

   function Array_Index_First (Index : Node_Id) return Node_Id is
   begin
      case Nkind (Index) is
         when N_Range =>
            return Low_Bound (Index);

         when N_Identifier | N_Expanded_Name =>
            if Nkind (Scalar_Range (Entity (Index)))
              = N_Subtype_Indication
            then
               return Array_Index_First (Scalar_Range (Entity (Index)));
            else
               return Low_Bound (Scalar_Range (Entity (Index)));
            end if;

         when N_Subtype_Indication =>
            return Low_Bound (Scalar_Range (Etype (Index)));

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Array_Index_First;

   ----------------------
   -- Array_Index_Last --
   ----------------------

   function Array_Index_Last (Index : Node_Id) return Node_Id is
   begin
      case Nkind (Index) is
         when N_Range =>
            return High_Bound (Index);

         when N_Identifier | N_Expanded_Name =>
            if Nkind (Scalar_Range (Entity (Index)))
              = N_Subtype_Indication
            then
               return Array_Index_Last (Scalar_Range (Entity (Index)));
            else
               return High_Bound (Scalar_Range (Entity (Index)));
            end if;

         when N_Subtype_Indication =>
            return High_Bound (Scalar_Range (Etype (Index)));

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Array_Index_Last;

   ------------------------
   -- Check_For_Overflow --
   ------------------------

   procedure Check_For_Overflow (N : Node_Id) is
      Check_Type  : constant Entity_Id := Full_Type (N);
      TOS_Type    : constant Entity_Id := Full_Type (Expression (N));
      Check_State : Boolean;
      OK_Label    : Label_Id;
      Raise_Label : Label_Id;
      Test_Needed : Boolean;

      procedure Generate_High_Bound_Check;
      --  Generate code which checks that TOS value <= Check_Type'Last

      procedure Skip_High_Bound_Check;
      --  No action if the lower bound test was not generated; otherwise
      --  generate a jump to skip the statement raising the exception

      -------------------------------
      -- Generate_High_Bound_Check --
      -------------------------------

      procedure Generate_High_Bound_Check is
      begin
         Test_Needed := True;

         Gen_Duplicate;

         if JVM_Type (TOS_Type) /= JVM_Type (Check_Type) then
            Gen_Conversion (JVM_Type (Check_Type));
         end if;

         Evaluate_Integer_Literal
           (Type_High_Bound (Check_Type), Check_Type);
         Gen_Compare_Branch_Less_Equal (OK_Label);
      end Generate_High_Bound_Check;

      ---------------------------
      -- Skip_High_Bound_Check --
      ---------------------------

      procedure Skip_High_Bound_Check is
      begin
         if Test_Needed then
            Gen_Goto (OK_Label);
         end if;
      end Skip_High_Bound_Check;

   --  Start of processing for Check_For_Overflow

   begin
      pragma Assert (Nkind (N) = N_Type_Conversion);

      if not Do_Overflow_Check (N) then
         return;

      --  For now we don't do overflow checks for floating point conversions,
      --  and in any case we don't perform them if the JVM types are the same
      --  (will this miss some fixed point cases that need to be checked ???).

      elsif Is_Floating_Point_Type (Check_Type)
        or else Is_Floating_Point_Type (TOS_Type)
        or else JVM_Type (Check_Type) = JVM_Type (TOS_Type)
      then
         return;
      end if;

      pragma Assert (Ekind (Check_Type) in Integer_Kind
         or else Ekind (Check_Type) in Fixed_Point_Kind);
      pragma Assert (Ekind (TOS_Type) in Integer_Kind
         or else Ekind (TOS_Type) in Fixed_Point_Kind);

      --  Generate code which compares the top-of-stack value against the
      --  bounds of the target type's base range and raise Constraint_Error
      --  if out of bounds.

      Raise_Label := New_Label;
      OK_Label    := New_Label;
      Test_Needed := False;

      Suppress_Stack_Checking (Check_State);

      --  Stage 1: Testing the low bound ******************************

      --  No need to check the lower bound if both types are modulars

      if Ekind (Check_Type) in Modular_Integer_Kind
        and then Ekind (TOS_Type) in Modular_Integer_Kind
      then
         null;

      else
         Test_Needed := True;

         Gen_Duplicate;

         if JVM_Type (TOS_Type) /= JVM_Type (Check_Type) then
            Gen_Conversion (JVM_Type (Check_Type));
         end if;

         Evaluate_Integer_Literal
           (Type_Low_Bound (Check_Type), Check_Type);
         Gen_Compare_Branch_Less (Raise_Label);
      end if;

      --  Stage 2: Testing the high bound ******************************

      --  Case 2.1: Both types are modulars

      if Ekind (Check_Type) in Modular_Integer_Kind
        and then Ekind (TOS_Type) in Modular_Integer_Kind
      then
         --  No runtime test needed if Check_Type'Last >= TOS_Type'Last

         if Intval (Type_High_Bound (Check_Type))
           >= Intval (Type_High_Bound (TOS_Type))
         then
            Skip_High_Bound_Check;
         else
            Generate_High_Bound_Check;
         end if;

      --  Case 2.2: Check_Type is modular and TOS_Type is a signed type or
      --  a fixed type

      elsif Ekind (Check_Type) in Modular_Integer_Kind
        and then Ekind (TOS_Type) not in Modular_Integer_Kind
      then
         --  No runtime check needed if Check_Type covers the range of positive
         --  values of TOS_Type

         if Ekind (TOS_Type) not in Fixed_Point_Kind
           and then Intval (Type_High_Bound (Check_Type))
                      >= Intval (Type_High_Bound (TOS_Type))
         then
            Skip_High_Bound_Check;

         elsif Ekind (TOS_Type) in Fixed_Point_Kind
           and then Intval (Type_High_Bound (Check_Type))
                      >= Corresponding_Integer_Value
                           (Type_High_Bound (TOS_Type))
         then
            Skip_High_Bound_Check;

         --  If Check_Type does not cover the range of positive values of
         --  TOS_Type and the evaluation of Check_Type'Last is a negative
         --  number then we evaluate the integer literal Check_Type'Last using
         --  the signed type (TOS_Type). Thus we avoid generating extra code to
         --  convert it to TOS_Type to perform the comparison using signed
         --  types.

         elsif Evaluate_Integer_Literal
                 (Type_High_Bound (Check_Type), Check_Type) < 0
         then
            Test_Needed := True;

            Gen_Duplicate;
            Evaluate_Integer_Literal
              (Type_High_Bound (Check_Type), TOS_Type);
            Gen_Compare_Branch_Less_Equal (OK_Label);

         --  Check_Type'Last is a positive number

         else
            Generate_High_Bound_Check;
         end if;

      --  Case 2.3: Check_Type is signed or fixed and TOS_Type is modular

      elsif Ekind (Check_Type) not in Modular_Integer_Kind
        and then Ekind (TOS_Type) in Modular_Integer_Kind
      then
         Test_Needed := True;

         Gen_Duplicate;

         if JVM_Type (TOS_Type) /= JVM_Type (Check_Type) then
            Gen_Conversion (JVM_Type (Check_Type));

         --  The value in the top of the stack may be a negative number
         --  representing a modular number. Hence we force a conversion
         --  to handle both values as signed values.

         elsif Evaluate_Integer_Literal
                 (Type_High_Bound (TOS_Type), TOS_Type) < 0
         then
            Gen_Conversion (JVM_Type (Check_Type));
         end if;

         Evaluate_Integer_Literal
           (Type_High_Bound (Check_Type), Check_Type);
         Gen_Compare_Branch_Less_Equal (OK_Label);

      --  Case 3.4: Both types are signed or fixed types

      else
         Generate_High_Bound_Check;
      end if;

      --  Generate a raise of Constraint_Error

      if Test_Needed then
         Gen_Label (Raise_Label);

         Generate_Exception_And_Throw
           (API_Class (Ada_Constraint_Error), Sloc (N),
            UI_From_Int
              (RT_Exception_Code'Pos (CE_Overflow_Check_Failed)));

         Gen_Label (OK_Label);
      end if;

      Restore_Stack_Checking (Check_State);
   end Check_For_Overflow;

   ----------------------
   -- Check_Flat_Array --
   ----------------------

   procedure Check_Flat_Array
     (Arr_LV      : Local_Var_Id;
      Is_Flat_Lbl : Label_Id)
   is
      JVM_Arr_Typ    : Type_Id;
      Num_Dimensions : Int;

   begin
      Gen_Load_Local (Arr_LV);
      Gen_Branch_If_Null (Is_Flat_Lbl);

      Gen_Load_Local (Arr_LV);

      if Is_Array_Descriptor (Type_Of (Arr_LV)) then
         Gen_Get_Field (Descriptor_Field (Type_Of (Arr_LV)));
      end if;

      JVM_Arr_Typ := Top_Type;

      if JVM_Arr_Typ = Type_Of (API_Class (Ada_String))
        or else JVM_Arr_Typ = Type_Of (API_Class (Ada_Wide_String))
        or else JVM_Arr_Typ = Type_Of (API_Class (Ada_Wide_Wide_String))
      then
         Num_Dimensions := 1;
      else
         Num_Dimensions := Int (Dimensionality (JVM_Arr_Typ));
      end if;

      Gen_Array_Length;
      Gen_Branch_Equal (Is_Flat_Lbl);

      if Num_Dimensions > 1 then
         declare
            Next_Idx_Lbl : Label_Id;
            Typ          : Type_Id;

         begin
            Gen_Load_Local (Arr_LV);

            for J in 2 .. Num_Dimensions loop
               Gen_Push_Int (Uint_0);
               Gen_Load_Subarray_Reference;

               Gen_Duplicate;
               Gen_Array_Length;

               Next_Idx_Lbl := New_Label;
               Gen_Branch_Not_Equal (Next_Idx_Lbl);
               Typ := Top_Type;
               Gen_Pop;

               Gen_Goto (Is_Flat_Lbl);
               Push_Type (Typ);

               Gen_Label (Next_Idx_Lbl);
            end loop;

            Gen_Pop;
         end;
      end if;
   end Check_Flat_Array;

   ---------------------------------------
   -- Evaluate_Acc_Prot_Subp_Comparison --
   ---------------------------------------

   procedure Evaluate_Acc_Prot_Subp_Comparison
     (Op          : Node_Kind;
      Acc_Type    : Entity_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean)
   is
      pragma Unreferenced (Label);       -- Should this parameter be used???
      pragma Unreferenced (True_Branch); -- Should this parameter be used???

      Rec_Type      : constant Entity_Id    := Equivalent_Type (Acc_Type);
      JVM_Rec_Type  : constant Type_Id      := JVM_Type (Rec_Type);
      Left_Temp     : constant Local_Var_Id :=
                        New_Local_Var ("_lft_tmp", JVM_Rec_Type);
      Right_Temp    : constant Local_Var_Id :=
                        New_Local_Var ("_rgt_tmp", JVM_Rec_Type);
      First_Comp    : constant Entity_Id    := First_Entity (Rec_Type);
      Second_Comp   : constant Entity_Id    := Next_Entity (First_Comp);
      First_Field   : constant Field_Id     := JVM_Field (First_Comp);
      Second_Field  : constant Field_Id     := JVM_Field (Second_Comp);
      Lbl_T1        : constant Label_Id := New_Label;
      Lbl_T2        : constant Label_Id := New_Label;
      Lbl_F         : constant Label_Id := New_Label;
      Check_State   : Boolean;

   begin
      Gen_Duplicate;
      Gen_Store_Local (Right_Temp);
      Gen_Swap;
      Gen_Duplicate;
      Gen_Store_Local (Left_Temp);
      Gen_Get_Field (First_Field);
      Gen_Swap;
      Gen_Get_Field (First_Field);

      Suppress_Stack_Checking (Check_State);

      if Op = N_Op_Eq then
         Gen_Compare_Branch_Equal (Lbl_T1);
         Gen_Push_Int (Uint_0);
         Gen_Goto (Lbl_F);

      else
         pragma Assert (Op = N_Op_Ne);
         Gen_Compare_Branch_Equal (Lbl_T1);
         Gen_Push_Int (Uint_1);
         Gen_Goto (Lbl_F);
      end if;

      Gen_Label (Lbl_T1);

      Gen_Load_Local (Left_Temp);
      Gen_Get_Field  (Second_Field);
      Gen_Load_Local (Right_Temp);
      Gen_Get_Field  (Second_Field);

      if Op = N_Op_Eq then
         Gen_Compare_Branch_Equal (Lbl_T2);
      else
         pragma Assert (Op = N_Op_Ne);
         Gen_Compare_Branch_Not_Equal (Lbl_T2);
      end if;

      Mark_Stack;
      Gen_Push_Int (Uint_0);
      Release_Stack;
      Gen_Goto (Lbl_F);

      Gen_Label (Lbl_T2);

      Mark_Stack;
      Gen_Push_Int (Uint_1);
      Release_Stack;

      Gen_Label (Lbl_F);

      Restore_Stack_Checking (Check_State);
   end Evaluate_Acc_Prot_Subp_Comparison;

   -------------------
   -- Evaluate_Addr --
   -------------------

   function Evaluate_Addr (Obj_Name : Node_Id) return Address_Descriptor is
      Addr : Address_Descriptor;

   begin
      Debug_A_Entry ("(Ada-to-JVM) ", Obj_Name);

      case Nkind (Obj_Name) is
         when N_Identifier | N_Expanded_Name =>
            declare
               Ada_Obj        : constant Entity_Id := Entity (Obj_Name);
               Ada_Type       : Entity_Id          := Full_Type (Ada_Obj);
               Formal_LV      : Local_Var_Id;
               Obj_Field      : Field_Id;
               Up_Level_Field : Field_Id;

            begin
               case Ekind (Ada_Obj) is
                  when E_Variable | E_Constant =>

                     --  Handle renaming of explicit dereference

                     if Present (Renamed_Object (Ada_Obj))
                       and then Nkind (Renamed_Object (Ada_Obj))
                                  = N_Explicit_Dereference
                     then
                        return Evaluate_Addr (Renamed_Object (Ada_Obj));

                     --  Handle renamings of elementary types

                     elsif Present (Renamed_Object (Ada_Obj))
                       and then Ekind (Ada_Type) in Elementary_Kind
                     then
                        --  If this is a renaming an elementary entity, then
                        --  return the address of the renamed entity

                        if Is_Entity_Name (Renamed_Object (Ada_Obj)) then
                           return Evaluate_Addr (Renamed_Object (Ada_Obj));

                        --  If the renamed object is a selected or indexed
                        --  component or an explicit dereference, change the
                        --  type to indicate that we want to load the reference
                        --  to the record object containing the component. The
                        --  address of the elementary component field itself
                        --  will be established further below, after the
                        --  containing object has been loaded.

                        elsif Nkind_In (Renamed_Object (Ada_Obj),
                                        N_Selected_Component,
                                        N_Indexed_Component,
                                        N_Explicit_Dereference)
                        then
                           Ada_Type :=
                             Full_Type (Prefix (Renamed_Object (Ada_Obj)));

                           if Is_Access_Type (Ada_Type) then
                              Ada_Type :=
                                Full_Type
                                  (Directly_Designated_Type (Ada_Type));
                           end if;

                        --  ???
                        --  Blow up in other cases, which aren't supported yet.

                        else
                           Addr := (Addr_Kind => Object_Address);
                           pragma Assert (False);
                           raise Program_Error;
                        end if;
                     end if;

                     --  Handle library level entities

                     if Is_Global_Entity (Ada_Obj)
                       or else Is_Imported (Ada_Obj)
                     then
                        Obj_Field := JVM_Field (Ada_Obj);

                        --  If the Ada object denotes a JVM object, then we
                        --  must load the object reference since the object
                        --  must be addressed indirectly.

                        if Ekind (Ada_Type) not in Wrappable_Kind
                          and then not Is_Value_Type (Ada_Type)
                        then
                           Gen_Get_Static_Field (Obj_Field);

                        --  A wrapped scalar or access object requires
                        --  loading the reference to the wrapper

                        elsif Needs_Access_Descriptor (Ada_Obj) then
                           Gen_Get_Static_Field (Obj_Field);
                           pragma Assert (Is_Descriptor (Top_Type));
                           Obj_Field := Descriptor_Field (Ada_Obj);
                        end if;

                        Addr := (Field_Address, Obj_Field);

                     --  Handle entities defined in current nested subprogram

                     elsif Enclosing_Method (Ada_Obj) = Current_Method then

                        --  If the local variable has been allocated to a field
                        --  in the current method's AR object, then form an
                        --  address denoting that field if its type is scalar
                        --  or access (otherwise we can get the value from the
                        --  local variable since it's effectively a constant).

                        if Access_From_Current_AR (Ada_Obj) then
                           Gen_Load_Local (AR_Stack.Top.AR_Obj);

                           Up_Level_Field := AR_Field (AR_Stack.Top, Ada_Obj);

                           --  If the Ada object denotes a JVM object, then we
                           --  must load the object reference since the object
                           --  must be addressed indirectly.

                           if Ekind (Ada_Type) not in Wrappable_Kind then
                              Gen_Get_Object_Field (Up_Level_Field);

                           --  A wrapped scalar or access object requires
                           --  loading the reference to the wrapper.

                           elsif Needs_Access_Descriptor (Ada_Obj) then
                              Gen_Get_Object_Field (Up_Level_Field);
                              pragma Assert (Is_Descriptor (Top_Type));
                              Up_Level_Field := Descriptor_Field (Ada_Obj);
                           end if;

                           Addr := (Field_Address, Up_Level_Field);

                        else
                           --  If the Ada object denotes a JVM object, then we
                           --  must load the object reference since the object
                           --  must be addressed indirectly, except in the
                           --  case of valuetypes.

                           if Ekind (Ada_Type) not in Wrappable_Kind then
                              if not Is_Value_Type (Ada_Type) then
                                 Addr := (Local_Address,
                                          JVM_Local_Var (Ada_Obj));
                              else
                                 Gen_Load_Local_Address
                                   (JVM_Local_Var (Ada_Obj));
                                 Addr := (Valuetype_Address,
                                          JVM_Type (Ada_Obj));
                              end if;

                           --  A wrapped scalar or access object requires
                           --  loading the reference to the wrapper.

                           elsif Needs_Access_Descriptor (Ada_Obj) then
                              Gen_Load_Local (JVM_Local_Var (Ada_Obj));
                              pragma Assert (Is_Descriptor (Top_Type));
                              Addr :=
                                (Field_Address, Descriptor_Field (Ada_Obj));

                           --  The object must be an unwrapped scalar/access,
                           --  so its address is the local variable itself.

                           else
                              Addr := (Local_Address, JVM_Local_Var (Ada_Obj));
                           end if;
                        end if;

                     --  Handle entities defined in some enclosing scope

                     else
                        --  We have to address the local variable in an
                        --  up-level activation frame object.

                        Up_Level_Field := Access_AR_Field (Ada_Obj);

                        --  If the Ada object denotes a JVM object, then we
                        --  must load the object reference since the object
                        --  must be addressed indirectly.

                        if Ekind (Ada_Type) not in Wrappable_Kind then
                           Gen_Get_Object_Field (Up_Level_Field);

                        --  A wrapped scalar or access object requires
                        --  loading the reference to the wrapper.

                        elsif Needs_Access_Descriptor (Ada_Obj) then
                           Gen_Get_Object_Field (Up_Level_Field);
                           pragma Assert (Is_Descriptor (Top_Type));
                           Up_Level_Field := Descriptor_Field (Ada_Obj);
                        end if;

                        Addr := (Field_Address, Up_Level_Field);
                     end if;

                     --  Could we add a recursive call to handle the general
                     --  case???

                     --  If the object is a renaming of an elementary object,
                     --  then at this point it must be the renaming of an
                     --  object denoted by a selected component. In this
                     --  case the reference to the component's containing
                     --  object has been loaded, and we now change the
                     --  address to denote the component itself. Support
                     --  for other cases will be added later. ???

                     if Present (Renamed_Object (Ada_Obj))
                       and then Ekind (Full_Type (Ada_Obj)) in Elementary_Kind
                     then
                        case Nkind (Renamed_Object (Ada_Obj)) is
                        when N_Indexed_Component =>
                           if Addr.Addr_Kind = Local_Address then
                              Gen_Load_Local (Addr.Local_Var);
                           end if;

                           Gen_Array_Subscript
                             (Prefix (Renamed_Object (Ada_Obj)),
                              First (Expressions (Renamed_Object (Ada_Obj))));

                           if Ekind (Full_Type (Renamed_Object (Ada_Obj)))
                             not in Wrappable_Kind
                           then
                              Gen_Load_Array_Element (Ref_Only => True);
                              Addr :=
                                (Indexed_Address,
                                 JVM_Expr_Type (Renamed_Object (Ada_Obj)));

                           elsif Has_Aliased_Components
                             (Full_Type (Prefix (Renamed_Object (Ada_Obj))))
                           then
                              Gen_Load_Array_Element;
                              pragma Assert (Is_Descriptor (Top_Type));
                              Addr :=
                                (Field_Address,
                                 Descriptor_Field
                                   (Full_Type (Renamed_Object (Ada_Obj))));

                           else
                              Addr :=
                                (Indexed_Address,
                                 JVM_Expr_Type (Renamed_Object (Ada_Obj)));
                           end if;

                        when N_Selected_Component =>
                           declare
                              Selector : constant Entity_Id :=
                                           Entity
                                            (Selector_Name (Renamed_Object
                                              (Ada_Obj)));
                              J_Field  : Field_Id := JVM_Field (Selector);

                           begin
                              if Addr.Addr_Kind = Local_Address then
                                 Gen_Load_Local (Addr.Local_Var);
                              end if;

                              if Needs_Access_Descriptor (Selector) then
                                 Gen_Get_Object_Field (J_Field);
                                 pragma Assert (Is_Descriptor (Top_Type));
                                 J_Field := Descriptor_Field (Selector);
                              end if;

                              Addr := (Field_Address, J_Field);
                           end;

                        --  Renamings of explicit dereferences have been
                        --  handled recursively (see above).

                        when N_Explicit_Dereference =>
                           pragma Assert (False);
                           null;

                        when others =>
                           Addr := (Addr_Kind => Object_Address);

                           pragma Assert (False);
                           raise Program_Error;
                        end case;
                     end if;

                  when Formal_Kind =>

                     --  Formal of the current method

                     if JVM_Method (Enclosing_Subprogram (Ada_Obj))
                       = Current_Method
                     then
                        Formal_LV := JVM_Local_Var (Ada_Obj);

                        if Needs_Access_Descriptor (Ada_Obj) then
                           Gen_Load_Local (Formal_LV);
                           pragma Assert (Is_Descriptor (Top_Type));
                           Addr := (Field_Address, Descriptor_Field (Ada_Obj));

                        elsif Is_Value_Type (Etype (Ada_Obj)) then
                           Gen_Load_Local_Address (Formal_LV);
                           Addr := (Valuetype_Address,
                                    JVM_Type (Ada_Obj));

                        else
                           Addr := (Local_Address, Formal_LV);
                        end if;

                     --  Formal of some enclosing method

                     else
                        --  We have to address the local variable in an
                        --  up-level activation frame object.

                        Up_Level_Field := Access_AR_Field (Ada_Obj);
                        Gen_Get_Object_Field (Up_Level_Field);

                        if Ekind (Ada_Obj) /= E_In_Parameter
                          and then Ekind (Ada_Type) in Elementary_Kind
                        then
                           pragma Assert (Is_Descriptor (Top_Type));
                           Addr := (Field_Address, Descriptor_Field (Ada_Obj));
                        else
                           Addr := (Field_Address, Up_Level_Field);
                        end if;
                     end if;

                     --  If the formal is a controlling parameter of a
                     --  dispatching operation, then its JVM type may have
                     --  been changed to some parent type (in the case
                     --  of an overriding operation). In that case we
                     --  have to cast the parameter value to the JVM
                     --  type associated with the formal's Ada type.

                     if Is_Controlling_Formal (Ada_Obj)
                       and then
                         JVM_Type (Find_Dispatching_Type (Scope (Ada_Obj)))
                           /= Type_Of (JVM_Local_Var (Ada_Obj))
                     then
                        Gen_Check_Cast
                          (Class_Of_Type (JVM_Expr_Type (Ada_Obj)));
                     end if;

                  when E_Exception =>
                     Gen_Default_Object (JVM_Class (Ada_Obj));
                     --  No Addr value set in this case???

                  when E_Procedure | E_Function =>
                     pragma Assert
                       (Nkind (Parent (Obj_Name)) = N_Attribute_Reference);

                     --  Normally an Address attribute applied to a
                     --  subprogram is not defined for the JVM, but the GNAT
                     --  frontend sometimes expands such attributes as the
                     --  argument to an unchecked conversion, so we support
                     --  that case.

                     declare
                        Attr_Name : constant Name_Id :=
                                      Attribute_Name (Parent (Obj_Name));
                        Attr_Id   : constant Attribute_Id :=
                                      Get_Attribute_Id (Attr_Name);

                     begin
                        if Attr_Id = Attribute_Address
                          or else Attr_Id = Attribute_Code_Address
                        then

                           if Nkind (Parent (Parent (Obj_Name)))
                             /= N_Unchecked_Type_Conversion
                           then
                              --  For an Address attribute applied to a
                              --  subprogram without any access-to-subprogram
                              --  type context, simply push a null, since this
                              --  operation isn't sensibly supported for the
                              --  JVM.

                              --  For CIL, we can load a function ptr as a
                              --  native int

                              if VM_Target = JVM_Target then
                                 Gen_Push_Null;
                              else
                                 Gen_Load_Function_Pointer
                                   (JVM_Method (Ada_Obj));
                              end if;

                           elsif VM_Target = CLI_Target then

                              --  Create a delegate from the actual method

                              if Present (Enclosing_Subprogram (Ada_Obj)) then
                                 Load_Static_Link (Enclosing_Method (Ada_Obj));
                              else
                                 Gen_Push_Null;
                              end if;

                              Gen_Load_Function_Pointer (JVM_Method (Ada_Obj));
                              Gen_Default_Object
                                (JVM_Class (Full_Type
                                              (Parent (Parent (Obj_Name)))));

                           else
                              Evaluate_Subprogram_Access
                                (Obj_Name,
                                 Full_Type (Parent (Parent (Obj_Name))));
                           end if;

                           --  Otherwise assume that the attribute is one of
                           --  the flavors of access attributes, in which case
                           --  we get the subprogram access type from the
                           --  attribute's Etype.

                        elsif VM_Target = CLI_Target then

                           --  Create a delegate from the actual method

                           if Present (Enclosing_Subprogram (Ada_Obj)) then
                              Load_Static_Link (Enclosing_Method (Ada_Obj));
                           else
                              Gen_Push_Null;
                           end if;

                           Gen_Load_Function_Pointer (JVM_Method (Ada_Obj));
                           Gen_Default_Object
                             (JVM_Class (Full_Type (Parent (Obj_Name))));

                        else
                           Evaluate_Subprogram_Access
                             (Obj_Name, Full_Type (Parent (Obj_Name)));
                        end if;

                     end;

                  when others =>
                     Addr := (Addr_Kind => Object_Address);
                     pragma Assert (False);
                     raise Program_Error;
               end case;
            end;

         when N_Selected_Component =>
            declare
               Selector   : constant Entity_Id :=
                              Entity (Selector_Name (Obj_Name));
               J_Field    : Field_Id;
               Desig_Type : Entity_Id;

            begin
               --  References to the _tag field should never occur
               --  in an address context. For the case of _tag in
               --  a value context see Evaluate_Expr.

               pragma Assert (Chars (Selector) /= Name_uTag);

               --  If the selector is a _parent field, then we elide the
               --  field selection and simply evaluate the prefix. This
               --  usage should only occur in contexts where passing the
               --  containing object is appropriate in any case (the
               --  selection of the _parent field is equivalent to a
               --  conversion to the parent type).

               if Chars (Selector) = Name_uParent then
                  Addr := Evaluate_Addr (Obj_Name);

               else
                  Evaluate_Expr (Prefix (Obj_Name));
                  J_Field := JVM_Field (Selector);

                  --  If the selected component denotes a JVM object, then we
                  --  always load the object reference since the object must be
                  --  addressed indirectly in any case.

                  if Ekind (Full_Type (Obj_Name)) not in Wrappable_Kind
                    and then not Is_Value_Type (Full_Type (Obj_Name))
                  then
                     Gen_Get_Object_Field (J_Field);

                     --  If the selector is an access discriminant linked to
                     --  a parent discriminant, then we force the type (via
                     --  a checkcast) to be the selector's designated type
                     --  in the case where it has a different type than the
                     --  discriminant that it constrains. This ensures
                     --  compatibility with the type required by the context.

                     if Ekind (Selector) = E_Discriminant
                       and then Is_Access_Type (Etype (Selector))
                       and then Present (Corresponding_Discriminant (Selector))
                       and then
                         Etype (Selector)
                           /= Etype (Corresponding_Discriminant (Selector))
                     then
                        Desig_Type :=
                          Directly_Designated_Type (Etype (Selector));
                        Gen_Check_Cast (Class_Of_Type (JVM_Type (Desig_Type)));
                     end if;

                  --  A wrapped scalar or access component requires loading
                  --  the reference to the wrapper.

                  elsif Needs_Access_Descriptor (Selector) then
                     Gen_Get_Object_Field (J_Field);
                     pragma Assert (Is_Descriptor (Top_Type));
                     J_Field := Descriptor_Field (Selector);
                  end if;

                  Addr := (Field_Address, J_Field);
               end if;
            end;

         when N_Indexed_Component =>
            Evaluate_Expr (Prefix (Obj_Name));

            Gen_Array_Subscript
              (Prefix (Obj_Name), First (Expressions (Obj_Name)));

            --  If the indexed component denotes a JVM object, then we
            --  always load the object reference since the object must be
            --  addressed indirectly in any case.

            if Ekind (Full_Type (Obj_Name)) not in Wrappable_Kind then
               Gen_Load_Array_Element (Ref_Only => True);
               Addr := (Indexed_Address, JVM_Expr_Type (Obj_Name));

            --  A wrapped scalar or access component requires loading
            --  the reference to the wrapper and forming a field address.

            elsif Has_Aliased_Components (Full_Type (Prefix (Obj_Name))) then
               Gen_Load_Array_Element;
               pragma Assert (Is_Descriptor (Top_Type));
               Addr :=
                 (Field_Address, Descriptor_Field (Full_Type (Obj_Name)));

            else
               Addr := (Indexed_Address, JVM_Expr_Type (Obj_Name));
            end if;

         when N_Slice =>
            Evaluate_Expr (Prefix (Obj_Name));

            Addr := (Addr_Kind        => Array_Address,
                     Is_Slice         => True,
                     Descriptor_Class => Null_Class); --  why Null_Class???

         when N_Explicit_Dereference =>
            declare
               Designated_Atype : constant Entity_Id :=
                                    Underlying_Type
                                      (Designated_Type
                                        (Full_Type (Prefix (Obj_Name))));
               Designated_Jtype : constant Type_Id :=
                                    JVM_Type (Designated_Atype);

            begin
               Evaluate_Expr (Prefix (Obj_Name));

               if JVM.Type_Kind (Designated_Jtype) = JVM.Array_Kind then
                  if Is_Constrained (Designated_Atype)
                    or else
                      Convention (Scope (Full_Type (Prefix (Obj_Name))))
                        = Convention_VM
                  then
                     Addr := (Array_Address, Is_Slice => False,
                              Descriptor_Class => Null_Class);
                  else
                     Addr :=
                       (Array_Address,
                        Is_Slice => False,
                        Descriptor_Class =>
                         Class_Of_Type (JVM_Expr_Type (Prefix (Obj_Name))));
                  end if;

               elsif Ekind (Designated_Atype) in Wrappable_Kind then
                  Addr := (Field_Address, Descriptor_Field (Designated_Atype));

               elsif JVM.Type_Kind (Designated_Jtype) = JVM.Class_Kind then
                  Addr := (Addr_Kind => Object_Address);

               elsif Ekind (Designated_Atype) = E_Subprogram_Type then
                  Addr := (Addr_Kind => Object_Address);

               else
                  Addr := (Addr_Kind => Object_Address);
                  pragma Assert (False);
                  raise Program_Error;
               end if;
            end;

         when N_Type_Conversion =>
            Addr := Evaluate_Addr (Expression (Obj_Name));

            --  When evaluating the address of a conversion, the only
            --  time a JVM conversion may be needed is in the case of
            --  class and array types (cases corresponding to view
            --  conversions). In the case of elementary prefixes,
            --  the value of the elementary object will not be on
            --  the stack in any case, so there won't be anything
            --  to convert.

            --  In case of Local variable, we don't generate conversions as
            --  we don't have evaluated the variable.

            if Addr.Addr_Kind /= Local_Address
              and then Ekind (Full_Type (Obj_Name)) not in Wrappable_Kind
            then
               Gen_Conversion (JVM_Expr_Type (Obj_Name));
            end if;

         when N_Unchecked_Type_Conversion =>
            Addr := Evaluate_Addr (Expression (Obj_Name));

            if Addr.Addr_Kind /= Local_Address then
               --  If the target is a class or array type, then we may have
               --  to apply a cast on downward conversions, even though the
               --  conversion is unchecked, in order to satisfy the Java
               --  verifier.

               if JVM.Type_Kind (JVM_Expr_Type (Obj_Name)) = Class_Kind
                 or else JVM.Type_Kind (JVM_Expr_Type (Obj_Name)) =
                   JVM.Array_Kind
               then
                  Gen_Conversion (JVM_Expr_Type (Obj_Name));

               else
                  --  We have to explicitly change the top-of-stack type to
                  --  match the target of the unchecked conversion.

                  Pop_Type;
                  Push_Type (JVM_Expr_Type (Obj_Name));
               end if;
            end if;

         when N_Qualified_Expression =>
            Addr := Evaluate_Addr (Expression (Obj_Name));

         when others =>
            if Is_Value_Type (Etype (Obj_Name))
              or else
                (Is_Class_Wide_Type (Etype (Obj_Name))
                   and then Is_Value_Type (Etype (Etype (Obj_Name))))
            then
               declare
                  Local_Var : Local_Var_Id;
               begin
                  Local_Var :=
                    New_Local_Var ("_valtype_tmp", JVM_Type (Obj_Name));
                  Gen_Load_Local_Address (Local_Var);
                  Evaluate_Expr (Obj_Name);
                  Gen_Store_Valuetype (JVM_Type (Obj_Name));
                  Gen_Load_Local_Address (Local_Var);

                  return (Valuetype_Address, JVM_Type (Obj_Name));
               end;
            end if;

            pragma Assert (False);
            raise Program_Error;
      end case;

      Debug_A_Exit ("(Ada-to-JVM) ", Obj_Name, " (done)");
      return Addr;
   end Evaluate_Addr;

   ------------------------
   -- Evaluate_Aggregate --
   ------------------------

   procedure Evaluate_Aggregate (Aggr : Node_Id) is
      Aggr_Type : constant Entity_Id := Underlying_Type (Etype (Aggr));

   begin
      if Ekind (Aggr_Type) in Einfo.Array_Kind then
         Evaluate_Array_Aggregate (Aggr);

      elsif Ekind (Aggr_Type) in Record_Kind then
         Evaluate_Record_Aggregate (Aggr);

      elsif Ekind (Aggr_Type) = E_Access_Protected_Subprogram_Type then
         Evaluate_Record_Aggregate (Aggr);

      else
         pragma Assert (False);
         raise Program_Error;
      end if;
   end Evaluate_Aggregate;

   ------------------------
   -- Evaluate_Allocator --
   ------------------------

   procedure Evaluate_Allocator (Allocator : Node_Id) is
      Acc_Type   : constant Entity_Id := Full_Type (Allocator);
      Desig_Subt : constant Entity_Id :=
                     Underlying_Type (Designated_Type (Acc_Type));
      Desig_Type : constant Entity_Id := Full_Type (Desig_Subt);
      Expr       : constant Node_Id   := Expression (Allocator);
      Alloc_Subt : Entity_Id;
      Alloc_Type : Entity_Id;

   begin
      if Nkind (Expr) = N_Qualified_Expression then
         Alloc_Subt := Full_Subtype (Entity (Subtype_Mark (Expr)));

      elsif Nkind (Expr) = N_Subtype_Indication then
         Alloc_Subt := Full_Subtype (Entity (Subtype_Mark (Expr)));

      else
         Alloc_Subt := Full_Subtype (Entity (Expr));
      end if;

      Alloc_Type := Full_Type (Alloc_Subt);

      --  For now we allow the allocation of tagged objects whose
      --  type has convention Java. This prevents problems with types
      --  which do not have no-arg constructors. Perhaps we should
      --  relax this restriction at some point, checking for the
      --  presence of a no-arg constructor for the type. Would it
      --  be better to simply change this to a warning for now and
      --  leave it up to the user to determine the safety rather
      --  than being overly restrictive ???

      if Is_Tagged_Type (Alloc_Type)
        and then Convention (Alloc_Type) = Convention_VM
      then
         Error_Msg_N
           ("?allocator-created object of Java-convention tagged" &
            " type may fail.", Expr);
         Error_Msg_N
           ("?Verify that the allocator does not require parameters",
            Expr);
      end if;

      case Ekind (Desig_Type) is
         when E_Record_Type | E_Class_Wide_Type
            | E_Task_Type   | E_Protected_Type =>
            --  First case is an allocator with a type mark or subtype
            --  indication.

            if Nkind (Expr) /= N_Qualified_Expression then
               Gen_Invoke_Init (JVM_Class (Alloc_Type), Alloc_Type);

               --  If the allocator is marked as having no initialization,
               --  then we need to ensure that any composite components
               --  get allocated for it in the case where the type's
               --  <init> method does not do it (which is currently only
               --  the case when the type *has* an init_proc). This handles
               --  situations where the front end has expanded in-place
               --  initialization of an aggregate, in which case we need
               --  ensure here that the object's composite components
               --  are allocated. See comments with additional explanation
               --  on similar handling in jx_ch3.adb for object declarations.
               --  It would be nice to find a cleaner approach than
               --  doing the component allocations here as in-line code. ???

               if No_Initialization (Allocator)
                  and then Has_Non_Null_Base_Init_Proc (Alloc_Type)
               then
                  declare
                     Alloc_LV : constant Local_Var_Id
                       := New_Local_Var ("_alloc_tmp", JVM_Type (Alloc_Type));

                  begin
                     Gen_Store_Local (Alloc_LV);

                     --  We have to initialize any discriminants of the new
                     --  object before allocating its discriminant-dependent
                     --  components, otherwise Allocate_Composite_Components
                     --  has no way of determining the size of discriminant-
                     --  dependent arrays. This should probably be checking
                     --  for Is_Constrained (Desig_Subt), as is done for
                     --  object declarations, as this will incorrectly
                     --  initialize using the defaulted discriminants in
                     --  some cases (e.g., see c34007p); a better approach
                     --  is needed ???

                     if Has_Discriminants (Alloc_Type) then

                        --  If there's a discriminant constraint, then we
                        --  evaluate and store the values given in the
                        --  constraint into the new object.

                        if Present (Discriminant_Constraint (Alloc_Subt))
                          and then Is_Constrained (Alloc_Subt)
                        then
                           declare
                              Discr : Entity_Id :=
                                        First_Discriminant (Alloc_Type);
                              Assn  : Elmt_Id :=
                                        First_Elmt
                                          (Discriminant_Constraint
                                            (Alloc_Subt));
                           begin
                              while Present (Assn) loop
                                 Gen_Load_Local (Alloc_LV);
                                 Evaluate_Expr (Node (Assn));
                                 Gen_Put_Field (JVM_Field (Discr));

                                 Next_Discriminant (Discr);
                                 Next_Elmt (Assn);
                              end loop;
                           end;

                        --  The object's subtype is unconstrained, but there's
                        --  no initialization from which to extract the values
                        --  of the discriminants. This should never occur, or
                        --  if it does then we don't have anyplace where we
                        --  can get the discriminants.

                        else
                           pragma Assert (False);
                           raise Program_Error;
                        end if;
                     end if;

                     Allocate_Composite_Components (Alloc_Type, Alloc_LV);
                     Gen_Load_Local (Alloc_LV);
                  end;
               end if;

            --  If the allocator is initialized with an aggregate, then
            --  just use the result of evaluating the aggregate directly
            --  as the allocator result.

            elsif Nkind (Expression (Expr)) = N_Aggregate then
               Evaluate_Expr (Expression (Expr));

            --  The allocator has a qualified expression and requires a full
            --  copy of the result of the expression. In the non-class-wide
            --  case we push a null target value, evaluate the expression, and
            --  generate a call to the type's deep copy operation.

            elsif not Is_Class_Wide_Type (Desig_Type)
              and then not Is_Class_Wide_Type (Etype (Expr))
            then
               Gen_Push_Null;
               Evaluate_Expr (Expr);
               Gen_Invoke_Deep_Copy (Desig_Type);

            --  Otherwise, for the class-wide case, we evaluate and pass
            --  the source expression to the record type's deep clone
            --  operation to create and initialize the new object. We
            --  use deep clone instead of deep copy here in order to
            --  handle the case of allocators for class-wide types,
            --  which necessitate a dispatch on the source object.

            else
               Evaluate_Expr (Expression (Expr));
               Gen_Invoke_Deep_Clone (Desig_Type);
            end if;

         when E_Array_Type | E_String_Type =>
            if Is_Constrained (Desig_Subt)
              or else Convention (Scope (Acc_Type)) = Convention_VM
            then
               if Nkind (Expr) = N_Qualified_Expression then
                  Evaluate_With_Copy (Expression (Expr));

               else
                  declare
                     Comp_Type : constant Entity_Id :=
                                   Component_Type (Desig_Type);
                     Array_LV  : Local_Var_Id;

                  begin
                     if Number_Dimensions (Alloc_Type) = 1 then
                        Load_Index_Length (First_Index (Alloc_Subt));
                        Gen_New_Array (Alloc_Type);

                     --  Multidimensional array case

                     else
                        Allocate_Multiarray
                          (Subtyp  => Alloc_Subt,
                           JVM_Typ => JVM_Type (Alloc_Type));
                     end if;

                     --  An array with composite or aliased components
                     --  requires a traversal of the array and allocation
                     --  of objects for all its components. For now this
                     --  is generated inline, but eventually we should
                     --  encapsulate it in a method associated with the
                     --  array type.

                     if Ekind (Full_Type (Comp_Type)) in Composite_Kind
                       or else Has_Aliased_Components (Desig_Type)
                     then
                        Array_LV :=
                          New_Local_Var ("_a_arr_tmp", JVM_Type (Desig_Type));
                        Gen_Store_Local (Array_LV);
                        Allocate_Array_Components (Desig_Type, Array_LV);
                        Gen_Load_Local (Array_LV);
                     end if;
                  end;
               end if;

            --  Designated type is unconstrained so must allocate an array
            --  descriptor to contain the bounds.

            else
               declare
                  Arr_Ref_Typ   : constant Type_Id  := JVM_Type (Acc_Type);
                  Arr_Ref_Class : constant Class_Id :=
                                    Class_Of_Type (Arr_Ref_Typ);
                  Alloc_LV      : constant Local_Var_Id :=
                                    New_Local_Var ("_alloc_tmp", Arr_Ref_Typ);
                  Dimensions    : constant Pos_8 :=
                                    Pos_8 (Number_Dimensions (Alloc_Type));

               begin
                  pragma Assert (Is_Array_Descriptor (Arr_Ref_Typ));

                  Gen_Default_Object (Arr_Ref_Class);
                  Gen_Store_Local (Alloc_LV);

                  --  Allocate the array object (by evaluation in the case
                  --  of an qualified expression with an aggregate) and
                  --  save its reference and bounds in the appropriate
                  --  fields of the array descriptor.

                  Gen_Load_Local (Alloc_LV);

                  if Nkind (Expr) = N_Qualified_Expression then

                     --  Evaluate the array expression followed by loading
                     --  its bounds

                     Evaluate_With_Copy (Expression (Expr));
                     Load_Array_Bounds (Expression (Expr), Pos (Dimensions));

                     --  Save the bounds in the new compound array
                     --  reference object. In the multidimensional
                     --  case save each pair of bounds, counting
                     --  down the dimensions in reverse.

                     for D in reverse 2 .. Dimensions loop
                        Gen_Load_Local (Alloc_LV);
                        Gen_Swap;
                        Gen_Put_Field
                          (Field (Arr_Ref_Class, "last_" & Image (D)));

                        Gen_Load_Local (Alloc_LV);
                        Gen_Swap;
                        Gen_Put_Field
                          (Field (Arr_Ref_Class, "first_" & Image (D)));
                     end loop;

                     Gen_Load_Local (Alloc_LV);
                     Gen_Swap;
                     Gen_Put_Field (Field (Arr_Ref_Class, "last"));

                     Gen_Load_Local (Alloc_LV);
                     Gen_Swap;
                     Gen_Put_Field (Field (Arr_Ref_Class, "first"));

                     --  Save the new array reference in the 'all' field
                     --  of the new compound reference object.

                     pragma Assert (Is_Descriptor (Top_Type (Disp => 1)));
                     Gen_Put_Field (Descriptor_Field (Arr_Ref_Typ));

                  --  Case of allocator with an index constraint

                  else
                     declare
                        Index     : Node_Id := First_Index (Alloc_Subt);
                        Comp_Type : constant Entity_Id :=
                                      Component_Type (Desig_Type);
                        Array_LV  : Local_Var_Id;

                     begin
                        if Number_Dimensions (Alloc_Type) = 1 then
                           Load_Index_Length (Index);
                           Gen_New_Array (Alloc_Type);

                        --  Multidimensional array case

                        else
                           Allocate_Multiarray
                             (Subtyp  => Alloc_Subt,
                              JVM_Typ => JVM_Type (Alloc_Type));
                        end if;

                        pragma Assert (Is_Descriptor (Top_Type (Disp => 1)));
                        Gen_Put_Field (Descriptor_Field (Arr_Ref_Typ));

                        --  Save the bounds for each dimension in the new
                        --  compound array reference object.

                        Index := First_Index (Alloc_Subt);

                        Gen_Load_Local (Alloc_LV);
                        Evaluate_Expr (Array_Index_First (Index));
                        Add_Implicit_Index_Conversion;
                        Gen_Put_Field (Field (Arr_Ref_Class, "first"));

                        Gen_Load_Local (Alloc_LV);
                        Evaluate_Expr (Array_Index_Last (Index));
                        Add_Implicit_Index_Conversion;
                        Gen_Put_Field (Field (Arr_Ref_Class, "last"));

                        for D in 2 .. Dimensions loop
                           Next_Index (Index);

                           Gen_Load_Local (Alloc_LV);
                           Evaluate_Expr (Array_Index_First (Index));
                           Add_Implicit_Index_Conversion;
                           Gen_Put_Field
                             (Field (Arr_Ref_Class, "first_" & Image (D)));

                           Gen_Load_Local (Alloc_LV);
                           Evaluate_Expr (Array_Index_Last (Index));
                           Add_Implicit_Index_Conversion;
                           Gen_Put_Field
                             (Field (Arr_Ref_Class, "last_" & Image (D)));
                        end loop;

                        --  An array with composite or aliased components
                        --  requires a traversal of the array and allocation
                        --  of objects for all its components. For now this
                        --  is generated inline, but eventually we should
                        --  encapsulate it in a method associated with the
                        --  array type.

                        if Ekind (Full_Type (Comp_Type)) in Composite_Kind
                          or else Has_Aliased_Components (Desig_Type)
                        then
                           Array_LV :=
                             New_Local_Var
                               ("_a_arr_tmp", JVM_Type (Desig_Type));
                           Gen_Load_Local (Alloc_LV);
                           Gen_Get_Field (Descriptor_Field (Arr_Ref_Typ));
                           Gen_Store_Local (Array_LV);
                           Allocate_Array_Components (Desig_Type, Array_LV);
                        end if;
                     end;
                  end if;

                  Gen_Load_Local (Alloc_LV);
               end;
            end if;

         --  If the designated type is a scalar or (non-subprogram) access
         --  type then a wrapper object must be allocated.

         when Wrappable_Kind =>
            Gen_Default_Object (Class_Of_Type (Descriptor_Type (Desig_Type)));

            if Nkind (Expr) = N_Qualified_Expression then
               Gen_Duplicate;
               Evaluate_Expr (Expression (Expr));
               pragma Assert (Is_Descriptor (Top_Type (Disp => 1)));
               Gen_Put_Field (Descriptor_Field (Desig_Type));
            end if;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Evaluate_Allocator;

   -----------------------
   -- Evaluate_And_Then --
   -----------------------

   procedure Evaluate_And_Then
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean)
   is
   begin
      if Label /= Null_Label then
         if True_Branch then
            declare
               False_Label       : constant Label_Id := New_Label;
               Save_False_Label  : Label_Id := False_Label;
               Save_True_Label   : Label_Id := Label;

            begin
               Evaluate_Expr (Left_Opnd (Expr), Save_False_Label, False);

               if Save_False_Label /= Null_Label then
                  Gen_Branch_Equal (False_Label);
               end if;

               Evaluate_Expr (Right_Opnd (Expr), Save_True_Label, True);

               if Save_True_Label /= Null_Label then
                  Gen_Branch_Not_Equal (Label);
               end if;

               Gen_Label (False_Label);
               Label := Null_Label;
            end;

         else
            declare
               Save_Label : Label_Id := Label;

            begin
               Evaluate_Expr (Left_Opnd (Expr), Save_Label, False);

               if Save_Label /= Null_Label then
                  Gen_Branch_Equal (Label);
               end if;

               Save_Label := Label;
               Evaluate_Expr (Right_Opnd (Expr), Save_Label, False);

               if Save_Label /= Null_Label then
                  Gen_Branch_Equal (Label);
               end if;

               Label := Null_Label;
            end;
         end if;

      --  If no branch label is available, we have to produce a Boolean
      --  result (e.g., for a statement like B0 := B1 and then B2).

      else
         declare
            False_Label      : constant Label_Id := New_Label;
            True_Label       : constant Label_Id := New_Label;
            Save_False_Label : Label_Id := False_Label;
            Check_State      : Boolean;

         begin
            Suppress_Stack_Checking (Check_State);
            Evaluate_Expr (Left_Opnd (Expr), Save_False_Label, False);

            if Save_False_Label /= Null_Label then
               Gen_Branch_Equal (False_Label);
            end if;

            Save_False_Label := False_Label;
            Evaluate_Expr (Right_Opnd (Expr), Save_False_Label, False);

            if Save_False_Label /= Null_Label then
               Gen_Branch_Equal (False_Label);
            end if;

            Gen_Push_Int (Uint_1);
            Gen_Goto (True_Label);

            Gen_Label (False_Label);
            Mark_Stack;
            Gen_Push_Int (Uint_0);
            Release_Stack;

            Gen_Label (True_Label);
            Restore_Stack_Checking (Check_State);
         end;
      end if;
   end Evaluate_And_Then;

   ----------------------------
   -- Evaluate_Array_Address --
   ----------------------------

   procedure Evaluate_Array_Address (Arr : Node_Id) is
      Arr_Subtype : constant Entity_Id := Underlying_Type (Etype (Arr));

   begin
      pragma Assert (Ekind (Arr_Subtype) in Einfo.Array_Kind);

      --  Recursively evaluate the address of the conversion argument or
      --  qualified expression

      if Nkind_In (Arr,
           N_Type_Conversion,
           N_Qualified_Expression)
      then
         Evaluate_Array_Address (Expression (Arr));

      --  Recursively evaluate the address of the renamed entity

      elsif Is_Entity_Name (Arr)
        and then Present (Renamed_Object (Entity (Arr)))
      then
         Evaluate_Array_Address (Renamed_Object (Entity (Arr)));

      else
         Evaluate_Expr (Arr);

         --  If the previous evaluate leaves an array descriptor in the stack
         --  then dereference it and load its 'all' field.

         if Is_Array_Descriptor (Top_Type) then
            Gen_Get_Field (Descriptor_Field (Top_Type));
         end if;
      end if;
   end Evaluate_Array_Address;

   ------------------------------
   -- Evaluate_Array_Aggregate --
   ------------------------------

   procedure Evaluate_Array_Aggregate (Aggr : Node_Id) is
      Aggr_Type  : constant Entity_Id := Underlying_Type (Etype (Aggr));
      J_Type     : constant Type_Id   := JVM_Type (Aggr);
      Dimensions : constant Pos_8     := Dimensionality (J_Type);

      procedure Evaluate_Subaggregates (Aggr : Node_Id; Dimensions : Pos_8);
      --  Traverses the subaggregates of the remaining Dimensions number
      --  of dimensions of an array aggregate, generating code to compute
      --  the indexes of each element of the aggregate object and finally
      --  initializing each element from the expressions at the deepest level.
      --  Requires that a reference to an aggregate (or subaggregate) is
      --  on the top of stack.

      ----------------------------
      -- Evaluate_Subaggregates --
      ----------------------------

      procedure Evaluate_Subaggregates (Aggr : Node_Id; Dimensions : Pos_8) is
         New_Type  : constant Type_Id :=
                       New_Array_Type (Element_Type (J_Type), Dimensions);
         Aggr_Temp : constant Local_Var_Id :=
                       New_Local_Var ("_es_aggr", New_Type);
         Comp_Expr : Node_Id := First (Expressions (Aggr));
         Index     : Uint    := Uint_0;
         End_Lbl   : constant Label_Id := New_Label;

         Check_State : Boolean;

      begin
         Suppress_Stack_Checking (Check_State);

         --  Save away the currently computed subarray reference

         Gen_Store_Local (Aggr_Temp);

         --  Generate:
         --    if aggr /= null then
         --       ...
         --    end if;

         Check_Flat_Array
           (Arr_LV      => Aggr_Temp,
            Is_Flat_Lbl => End_Lbl);

         --  Loop through the list of subaggregates or element expressions

         while Present (Comp_Expr) loop

            --  If this is the deepest dimension of the aggregate, then
            --  generate the final index into the array and store the
            --  the result of evaluating the element expression.

            if Dimensions = 1 then
               Gen_Load_Local (Aggr_Temp);
               Gen_Push_Int (Index);

               if Is_Value_Type (Etype (Comp_Expr)) then
                  Gen_Load_Array_Element (Ref_Only => True);
               end if;

               Evaluate_With_Copy (Comp_Expr);
               Gen_Store_Array_Element;

            --  If Dimensions > 1, then we generate the index of a subarray
            --  and recurse.

            else
               Gen_Load_Local (Aggr_Temp);
               Gen_Push_Int (Index);
               Gen_Load_Subarray_Reference;

               Evaluate_Subaggregates (Comp_Expr, Dimensions - 1);
            end if;

            Index     := Index + 1;
            Comp_Expr := Next (Comp_Expr);
         end loop;

         Gen_Label (End_Lbl);

         Restore_Stack_Checking (Check_State);
      end Evaluate_Subaggregates;

   --  Start of processing for Evaluate_Array_Aggregate

   begin
      --  For now we don't support array aggregates with composite components,
      --  which requires traversing the array to allocate each component. ???

      if Dimensions = 1 then
         Load_Index_Length (Aggregate_Bounds (Aggr));
         Gen_New_Array (Aggr);
         Gen_Duplicate;

      else
         Allocate_Multiarray
           (Subtyp  => Aggr_Type,
            JVM_Typ => J_Type);
         Gen_Duplicate;
      end if;

      Evaluate_Subaggregates (Aggr, Dimensions);
   end Evaluate_Array_Aggregate;

   -------------------------------
   -- Evaluate_Array_Comparison --
   -------------------------------

   procedure Evaluate_Array_Comparison
     (Op        : Node_Kind;
      Left_Arr  : Node_Id;
      Right_Arr : Node_Id;
      Jtype     : Type_Id)
   is
      Lft_Is_Slice : Boolean;
      Lft_Prefix   : Node_Id;
      Lft_Subt     : Entity_Id;
      Rgt_Is_Slice : Boolean;
      Rgt_Prefix   : Node_Id;
      Rgt_Subt     : Entity_Id;

      Lbl_F        : constant Label_Id := New_Label;
      Lbl_T        : constant Label_Id := New_Label;
      Lbl_End      : constant Label_Id := New_Label;
      Check_State  : Boolean;

      procedure Compare_Subarrays (Dimensions : Pos_8);
      --  Generates code to compare the subarrays of the remaining Dimensions
      --  number of dimensions of two array objects. Requires that references
      --  to the two arrays are on the stack.

      -----------------------
      -- Compare_Subarrays --
      -----------------------

      procedure Compare_Subarrays (Dimensions : Pos_8) is
         Elt_Type    : constant Type_Id := Element_Type (Jtype);
         Left_Temp   : constant Local_Var_Id :=
                         New_Local_Var ("_l_eqtmp", Jtype);
         Right_Temp  : constant Local_Var_Id :=
                         New_Local_Var ("_r_eqtmp", Jtype);
         Length      : constant Local_Var_Id :=
                         New_Local_Var ("_index_max", Int_Type);
         Counter     : constant Local_Var_Id :=
                         New_Local_Var ("_loop_cnt", Int_Type);
         Loop_Head   : constant Label_Id := New_Label;
         Left_Index  : Local_Var_Id := Counter;
         Right_Index : Local_Var_Id := Counter;

      begin
         --  If the right operand is a slice then initialize its index temp
         --  before saving the array reference (this is necessary because
         --  Gen_Array_Subscript requires the array reference to be on the
         --  stack in certain cases).

         if Rgt_Is_Slice then
            Right_Index := New_Local_Var ("_r_index", Int_Type);
            Gen_Array_Subscript (Rgt_Prefix, Index_First (Rgt_Subt));
            Gen_Store_Local (Right_Index);
         end if;

         --  Now save the reference to the right array operand

         Gen_Store_Local (Right_Temp);

         --  If the left operand is a slice then initialize its index temp
         --  before saving the array reference (this is necessary because
         --  Gen_Array_Subscript requires the array reference to be on the
         --  stack in certain cases).

         if Lft_Is_Slice then
            Left_Index := New_Local_Var ("_l_index", Int_Type);
            Gen_Array_Subscript (Lft_Prefix, Index_First (Lft_Subt));
            Gen_Store_Local (Left_Index);
         end if;

         --  Now save the reference to the left array operand

         Gen_Store_Local (Left_Temp);

         --  First compare the array lengths and skip the array comparison if
         --  the lengths differ

         if Lft_Is_Slice then
            Load_Index_Length (First_Index (Lft_Subt));
         else
            Gen_Load_Local (Left_Temp);
            Gen_Array_Length;
         end if;

         Gen_Duplicate;
         Gen_Store_Local (Length);

         if Rgt_Is_Slice then
            Load_Index_Length (First_Index (Rgt_Subt));
         else
            Gen_Load_Local (Right_Temp);
            Gen_Array_Length;
         end if;

         if Op = N_Op_Eq then
            Gen_Compare_Branch_Not_Equal (Lbl_F);

         elsif Op = N_Op_Ne then
            Gen_Compare_Branch_Not_Equal (Lbl_T);

         else
            pragma Assert (False);
            raise Program_Error;
         end if;

         --  Generate a loop to compare subarrays or array elements

         --  Initialize loop counter to zero

         Gen_Push_Int (Uint_0);
         Gen_Store_Local (Counter);

         Gen_Label (Loop_Head);

         --  Check for end of loop (Counter = Length)

         Gen_Load_Local (Counter);
         Gen_Load_Local (Length);

         if Op = N_Op_Eq then
            Gen_Compare_Branch_Equal (Lbl_T);

         elsif Op = N_Op_Ne then
            Gen_Compare_Branch_Equal (Lbl_F);

         else
            pragma Assert (False);
            raise Program_Error;
         end if;

         --  When Dimensions = 1 we are at the bottommost dimension of
         --  the arrays so we now compare actual array elements.

         if Dimensions = 1 then
            --  Load left array element

            Gen_Load_Local (Left_Temp);
            Gen_Load_Local (Left_Index);
            Gen_Load_Array_Element;

            if Is_Descriptor (Elt_Type) then
               Gen_Get_Field (Descriptor_Field (Elt_Type));
            end if;

            --  Load right array element

            Gen_Load_Local (Right_Temp);
            Gen_Load_Local (Right_Index);
            Gen_Load_Array_Element;

            if Is_Descriptor (Elt_Type) then
               Gen_Get_Field (Descriptor_Field (Elt_Type));
            end if;

            --  Compare and exit if L(I) /= R(I)

            if Op = N_Op_Eq then
               Gen_Compare_Branch_Not_Equal (Lbl_F);

            elsif Op = N_Op_Ne then
               Gen_Compare_Branch_Not_Equal (Lbl_T);

            else
               pragma Assert (False);
               raise Program_Error;
            end if;

         --  If Dimensions > 1, then we load references to the subarrays
         --  and recurse to generate a comparison of the subarrays of the
         --  next dimension.

         else
            --  Load left subarray reference

            Gen_Load_Local (Left_Temp);
            Gen_Load_Local (Counter);
            Gen_Load_Subarray_Reference;

            --  Load right subarray reference

            Gen_Load_Local (Right_Temp);
            Gen_Load_Local (Counter);
            Gen_Load_Subarray_Reference;

            Compare_Subarrays (Dimensions - 1);
         end if;

         --  Increment loop counter and indexes, then iterate

         Gen_Incr_Local (Counter, Uint_1);

         if Lft_Is_Slice then
            Gen_Incr_Local (Left_Index, Uint_1);
         end if;

         if Rgt_Is_Slice then
            Gen_Incr_Local (Right_Index, Uint_1);
         end if;

         Gen_Goto (Loop_Head);
      end Compare_Subarrays;

      --  Local variables

      JVM_L_Typ : Type_Id := Top_Type (Disp => 1);
      JVM_R_Typ : Type_Id := Top_Type;

   --  Start of processing for Evaluate_Array_Comparison

   begin
      --  Handle expressions rewriten by the frontend as CE

      if Nkind (Left_Arr) = N_Raise_Constraint_Error
        or else Nkind (Left_Arr) = N_Raise_Constraint_Error
      then
         Gen_Pop;
         Gen_Pop;
         Gen_Push_Int (Uint_0);
         return;
      end if;

      Suppress_Stack_Checking (Check_State);

      Test_For_Slice (Left_Arr,  Lft_Is_Slice, Lft_Prefix, Lft_Subt);
      Test_For_Slice (Right_Arr, Rgt_Is_Slice, Rgt_Prefix, Rgt_Subt);

      if Top_Type (Disp => 1) = Any_Ref_Type then
         JVM_L_Typ := JVM_Type (Lft_Subt);
      end if;

      if Top_Type = Any_Ref_Type then
         JVM_R_Typ := JVM_Type (Rgt_Subt);
      end if;

      --  Handle comparison against null array

      declare
         L_Arr_LV         : constant Local_Var_Id :=
                              New_Local_Var ("_eac_l_arr", JVM_L_Typ);
         R_Arr_LV         : constant Local_Var_Id :=
                              New_Local_Var ("_eac_r_arr", JVM_R_Typ);
         Op1_Not_Null_Lbl : constant Label_Id := New_Label;
         Ret_False_Lbl    : constant Label_Id := New_Label;
         L_Null_Lbl       : constant Label_Id := New_Label;
         R_Null_Lbl       : constant Label_Id := New_Label;

      begin
         Gen_Store_Local (R_Arr_LV);
         Gen_Store_Local (L_Arr_LV);

         --  Check if the first argument is null or an array

         Check_Flat_Array
           (Arr_LV      => L_Arr_LV,
            Is_Flat_Lbl => L_Null_Lbl);

         Gen_Goto  (Op1_Not_Null_Lbl);
         Gen_Label (L_Null_Lbl);

         --  First argument is null; check second argument

         Check_Flat_Array
           (Arr_LV      => R_Arr_LV,
            Is_Flat_Lbl => R_Null_Lbl);

         Gen_Goto  (Ret_False_Lbl);
         Gen_Label (R_Null_Lbl);

         --  Both arguments are null

         if Op = N_Op_Eq then
            Gen_Push_Int (Uint_1);

         else pragma Assert (Op = N_Op_Ne);
            Gen_Push_Int (Uint_0);
         end if;

         Gen_Goto (Lbl_End);
         Pop_Type;

         --  Only one argument is null

         Gen_Label (Ret_False_Lbl);

         if Op = N_Op_Eq then
            Gen_Push_Int (Uint_0);

         else pragma Assert (Op = N_Op_Ne);
            Gen_Push_Int (Uint_1);
         end if;

         Gen_Goto (Lbl_End);
         Pop_Type;

         Gen_Label (Op1_Not_Null_Lbl);

         --  First argument is not null; check second argument

         Gen_Load_Local (R_Arr_LV);
         Gen_Branch_If_Null (Ret_False_Lbl);

         --  Both arguments are not null

         Gen_Load_Local (L_Arr_LV);
         Gen_Load_Local (R_Arr_LV);
      end;

      Compare_Subarrays (Dimensionality (Jtype));

      --  Generate a push of True and False values for the result of the
      --  comparison. The comparisons generated by Compare_Subarrays will
      --  branch to the code following the goto to Lbl_End as soon as a result
      --  is known (or else the code will fall through to the code preceding
      --  the Lbl_End goto to push the result).

      if Op = N_Op_Eq then
         Gen_Label (Lbl_T);
         Gen_Push_Int (Uint_1);

      elsif Op = N_Op_Ne then
         Gen_Label (Lbl_F);
         Gen_Push_Int (Uint_0);

      else
         pragma Assert (False);
         raise Program_Error;
      end if;

      Gen_Goto (Lbl_End);
      Mark_Stack;

      if Op = N_Op_Eq then
         Gen_Label (Lbl_F);
         Gen_Push_Int (Uint_0);

      elsif Op = N_Op_Ne then
         Gen_Label (Lbl_T);
         Gen_Push_Int (Uint_1);

      else
         pragma Assert (False);
         raise Program_Error;
      end if;

      Release_Stack;
      Gen_Label (Lbl_End);

      Restore_Stack_Checking (Check_State);
   end Evaluate_Array_Comparison;

   ------------------------
   -- Evaluate_Attribute --
   ------------------------

   procedure Evaluate_Attribute (Attr : Node_Id) is
      Attr_Kind   : constant Attribute_Id :=
                      Get_Attribute_Id (Attribute_Name (Attr));
      Attr_Prefix : constant Node_Id   := Prefix (Attr);
      Prefix_Type : constant Entity_Id :=
                      Underlying_Type (Etype (Attr_Prefix));

      function Dimension_Value (Attr : Node_Id) return Pos_8;
      --  Returns the static dimension specified in the attribute,
      --  or one if none was specified.

      procedure Evaluate_First_Last (Kind : Attribute_Id);
      --  Evaluate a 'First or 'Last attribute expression

      procedure Evaluate_Length;
      --  Evaluates attribute Length handling flat arrays. It assumes that the
      --  values of First and Last are in the stack: TOS -1 = Last; TOS = First
      --     Generates:
      --        if Last >= First then
      --           TOS = Last - First + 1;
      --        else
      --           TOS = 0
      --        end if;

      procedure Load_Descriptor_Attribute (Kind : Attribute_Id);
      --  Load the attribute from the JVM array descriptor stored in the stack.
      --  The descriptor is removed from the stack.

      ---------------------
      -- Dimension_Value --
      ---------------------

      function Dimension_Value (Attr : Node_Id) return Pos_8 is
         Dimension_Exp : Node_Id;
         Dimension     : Pos_8 := 1;

      begin
         if Present (Expressions (Attr)) then
            Dimension_Exp := First (Expressions (Attr));
            Dimension := Pos_8 (UI_To_Int (Expr_Value (Dimension_Exp)));
         end if;

         return Dimension;
      end Dimension_Value;

      -------------------------
      -- Evaluate_First_Last --
      -------------------------

      procedure Evaluate_First_Last (Kind : Attribute_Id) is
         Dimension : Pos_8;

      begin
         if Is_Scalar_Type (Prefix_Type) then
            if Kind = Attribute_First then
               Evaluate_Expr (Type_Low_Bound (Prefix_Type));
            else
               Evaluate_Expr (Type_High_Bound (Prefix_Type));
            end if;

         else
            Dimension := Dimension_Value (Attr);

            if Kind = Attribute_First then
               Load_Array_Attr (Attr_Prefix, First, Dimension);
            else
               Load_Array_Attr (Attr_Prefix, Last, Dimension);
            end if;
         end if;
      end Evaluate_First_Last;

      ---------------------
      -- Evaluate_Length --
      ---------------------

      procedure Evaluate_Length is
         Check_State : Boolean;
         Exit_Label  : Label_Id;
         Zero_Label  : Label_Id;

      begin
         Suppress_Stack_Checking (Check_State);

         Exit_Label := New_Label;
         Zero_Label := New_Label;

         Gen_Double_Duplicate;
         Gen_Compare_Branch_Less (Zero_Label);

         Gen_Sub (Modular => False, Integer_Type => True);
         Gen_Push_Int (Uint_1);
         Gen_Add (Modular => False, Integer_Type => True);
         Gen_Goto (Exit_Label);
         Pop_Type;

         Gen_Label (Zero_Label);
         Push_Type (Int_Type);
         Push_Type (Int_Type);

         Gen_Pop (2);
         Gen_Push_Int (Uint_0);

         Gen_Label (Exit_Label);

         Restore_Stack_Checking (Check_State);
      end Evaluate_Length;

      -------------------------------
      -- Load_Descriptor_Attribute --
      -------------------------------

      procedure Load_Descriptor_Attribute (Kind : Attribute_Id) is
         pragma Assert (Is_Array_Descriptor (Top_Type));
         pragma Assert (not Is_Scalar_Type (Prefix_Type));
         pragma Assert (Kind /= Attribute_Length);

         Descr_Class : constant Class_Id := Class_Of_Type (Top_Type);
         Dimension   : constant Pos_8 := Dimension_Value (Attr);

      begin
         if Dimension = 1 then
            if Kind = Attribute_First then
               Gen_Get_Field (Field (Descr_Class, "first"));
            else
               Gen_Get_Field (Field (Descr_Class, "last"));
            end if;
         else
            if Kind = Attribute_First then
               Gen_Get_Field
                (Field (Descr_Class, "first_" & Image (Int_8 (Dimension))));
            else
               Gen_Get_Field
                (Field (Descr_Class, "last_" & Image (Int_8 (Dimension))));
            end if;
         end if;
      end Load_Descriptor_Attribute;

      --  Local variables

      Addr        : Address_Descriptor;
      Index_Range : Node_Id;

   --  Start of processing for Evaluate_Attribute

   begin
      case Attr_Kind is
         when Attribute_First | Attribute_Last =>
            Evaluate_First_Last (Attr_Kind);

            if Top_Type /= JVM_Expr_Type (Attr) then
               Gen_Conversion (JVM_Expr_Type (Attr));
            end if;

         when Attribute_Length =>
            declare
               Dimension : Pos_8;

            begin
               Dimension := Dimension_Value (Attr);

               if Is_Entity_Name (Attr_Prefix)
                 and then Is_Type (Entity (Attr_Prefix))
               then
                  Index_Range := First_Index (Prefix_Type);

                  --  Retrieve the index corresponding to the
                  --  specified dimension

                  while Dimension > 1 loop
                     Next_Index (Index_Range);
                     Dimension := Dimension - 1;
                  end loop;

                  Load_Index_Length (Index_Range);

               elsif Nkind (Attr_Prefix) = N_Slice
                 and then not
                   (Is_Access_Type (Prefix_Type)
                      and then
                    Convention (Scope (Prefix_Type)) /= Convention_VM)
               then
                  Evaluate_First_Last (Attribute_Last);
                  Evaluate_First_Last (Attribute_First);
                  Evaluate_Length;

               --  Function returning unconstrained array type

               elsif Nkind (Attr_Prefix) = N_Function_Call
                 and then Is_Array_Type (Etype (Attr_Prefix))
                 and then not Is_Constrained (Etype (Attr_Prefix))
               then
                  Evaluate_First_Last (Attribute_Last);
                  Evaluate_First_Last (Attribute_First);
                  Evaluate_Length;

               else
                  Evaluate_Expr (Attr_Prefix);

                  if Is_Array_Descriptor (Top_Type) then
                     Gen_Duplicate;
                     Load_Descriptor_Attribute (Attribute_Last);
                     Gen_Swap;
                     Load_Descriptor_Attribute (Attribute_First);
                  else
                     Evaluate_First_Last (Attribute_Last);
                     Gen_Swap;

                     Evaluate_First_Last (Attribute_First);
                     Gen_Swap;

                     --  Remove the array from the stack

                     Gen_Pop;
                  end if;

                  Evaluate_Length;
               end if;

               --  If the type is Universal_Integer then for now we always
               --  convert to long, though this is sometimes pessimistic,
               --  but Universal_Integer expressions can require the largest
               --  range in some cases. Normally this will only be in the
               --  context of relational expressions. Eventually we should
               --  try to recognize cases where the expression only requires
               --  int range. ???

               if Etype (Attr) = Universal_Integer then
                  Gen_Conversion (Long_Type);
               end if;
            end;

         when Attribute_Max | Attribute_Min =>
            declare
               Compare_Label : constant Label_Id := New_Label;
               Check_State   : Boolean;

            begin
               Evaluate_Expr (First (Expressions (Attr)));

               if Top_Type /= JVM_Expr_Type (Attr) then
                  Gen_Conversion (JVM_Expr_Type (Attr));
               end if;

               Evaluate_Expr (Last (Expressions (Attr)));

               if Top_Type /= JVM_Expr_Type (Attr) then
                  Gen_Conversion (JVM_Expr_Type (Attr));
               end if;

               Gen_Double_Duplicate;

               Suppress_Stack_Checking (Check_State);

               if Attr_Kind = Attribute_Max then
                  Gen_Compare_Branch_Greater_Equal (Compare_Label);
               else
                  Gen_Compare_Branch_Less_Equal (Compare_Label);
               end if;

               Gen_Swap;
               Gen_Label (Compare_Label);
               Gen_Pop;

               Restore_Stack_Checking (Check_State);
            end;

         when Attribute_Pos =>
            Evaluate_Expr (First (Expressions (Attr)));

            --  If the Pos attribute appears within a context requiring a
            --  universal or 64-bit value, then ensure that its type is long by
            --  conversion if necessary, since the argument of the attribute
            --  can be a 32-bit int and the enclosing operation may require the
            --  larger range.

            if Top_Type /= JVM_Expr_Type (Attr) then
               Gen_Conversion (JVM_Expr_Type (Attr));
            end if;

         when Attribute_Pred =>
            Evaluate_Expr (First (Expressions (Attr)));

            if not Is_Modular_Integer_Type (Etype (Attr)) then
               Gen_Push_Int (Uint_1);
               Gen_Conversion (JVM_Type (First (Expressions (Attr))));
               Gen_Sub (Modular => False, Integer_Type => True);

            else
               --  Generate:
               --         dup
               --         push Low_Bound (Mod_Typ)
               --         je Eq_Label
               --         push_int 1
               --         sub
               --         jmp Exit_Label
               --     Eq_Label:
               --         pop
               --         push High_Bound (Mod_Typ)
               --     Exit_Label:

               declare
                  Eq_Label    : constant Label_Id := New_Label;
                  Exit_Label  : constant Label_Id := New_Label;
                  Check_State : Boolean;

               begin
                  Suppress_Stack_Checking (Check_State);

                  Gen_Duplicate;

                  Evaluate_Expr (Low_Bound (Scalar_Range (Etype (Attr))));
                  Gen_Compare_Branch_Equal (Eq_Label);

                  Gen_Push_Int (Uint_1);
                  Gen_Conversion (JVM_Type (First (Expressions (Attr))));
                  Gen_Sub (Modular => True, Integer_Type => True);

                  Gen_Goto (Exit_Label);
                  Gen_Label (Eq_Label);

                  Gen_Pop;
                  Evaluate_Expr (High_Bound (Scalar_Range (Etype (Attr))));

                  Gen_Label (Exit_Label);

                  Restore_Stack_Checking (Check_State);
               end;
            end if;

         when Attribute_Range_Length =>
            Load_Index_Length (Full_Subtype (Prefix (Attr)));

            --  If the type is Universal_Integer then for now we always
            --  convert to long, though this is sometimes pessimistic,
            --  but Universal_Integer expressions can require the largest
            --  range in some cases. Normally this will only be in the
            --  context of relational expressions. Eventually we should
            --  try to recognize cases where the expression only requires
            --  int range. ???

            if Etype (Attr) = Universal_Integer then
               Gen_Conversion (Long_Type);
            end if;

         when Attribute_Succ =>
            Evaluate_Expr (First (Expressions (Attr)));

            if not Is_Modular_Integer_Type (Etype (Attr)) then
               Gen_Push_Int (Uint_1);
               Gen_Conversion (JVM_Type (First (Expressions (Attr))));
               Gen_Add (Modular => False, Integer_Type => True);

            else
               --  Generate:
               --        dup
               --        push High_Bound (Mod_Typ)
               --        je Eq_Label
               --        push_int 1
               --        add
               --        jmp Exit_Label
               --    Eq_Label:
               --        pop
               --        push Low_Bound (Mod_Typ)
               --    Exit_Label:

               declare
                  Eq_Label    : constant Label_Id := New_Label;
                  Exit_Label  : constant Label_Id := New_Label;
                  Check_State : Boolean;

               begin
                  Suppress_Stack_Checking (Check_State);

                  Gen_Duplicate;

                  Evaluate_Expr (High_Bound (Scalar_Range (Etype (Attr))));
                  Gen_Compare_Branch_Equal (Eq_Label);

                  Gen_Push_Int (Uint_1);
                  Gen_Conversion (JVM_Type (First (Expressions (Attr))));
                  Gen_Add (Modular => True, Integer_Type => True);

                  Gen_Goto (Exit_Label);
                  Gen_Label (Eq_Label);

                  Gen_Pop;
                  Evaluate_Expr (Low_Bound (Scalar_Range (Etype (Attr))));
                  Gen_Label (Exit_Label);

                  Restore_Stack_Checking (Check_State);
               end;
            end if;

         when Attribute_Size | Attribute_Max_Size_In_Storage_Elements =>

            --  The Size attribute is only partially supported, in particular
            --  when the size is statically known. Other cases will require
            --  specialized type handling, so generate a warning message for
            --  now. ???

            declare
               Size : Uint;

            begin
               if Is_Entity_Name (Attr_Prefix)
                 and then Is_Type (Entity (Attr_Prefix))
               then
                  Size := RM_Size (Full_Subtype (Attr_Prefix));

                  if Size = Uint_0
                    and then Comes_From_Source (Attr)
                  then
                     if VM_Target = CLI_Target then
                        Gen_Sizeof (JVM_Type (Full_Subtype (Attr_Prefix)));
                        Gen_Push_Int (Uint_8);
                        Gen_Mul (False, True);

                        if JVM_Expr_Type (Attr) = Long_Type then
                           Gen_Conversion (Long_Type);
                        end if;

                     else
                        Error_Msg_N
                          ("?size attribute is unsupported for this type",
                           Attr);
                        Gen_Push_Int (Uint_0);
                     end if;

                  else
                     if JVM_Expr_Type (Attr) = Int_Type then
                        Gen_Push_Int (Size, Attr);
                     else
                        Gen_Push_Long (Size);
                     end if;
                  end if;

               else
                  Size := Esize (Full_Subtype (Attr_Prefix));

                  if Size = Uint_0
                    and then Comes_From_Source (Attr)
                  then
                     Error_Msg_N ("unsupported attribute in this context",
                                  Attr);
                  end if;

                  if JVM_Expr_Type (Attr) = Int_Type then
                     Gen_Push_Int (Size, Attr);
                  else
                     Gen_Push_Long (Size);
                  end if;
               end if;

               --  It's not clear what we should use for the JVM's
               --  storage element size. Arrays of bytes and characters
               --  are supported, but normal scalar objects require
               --  a minimum of 32 bits. For now we use Character'Size.

               if Attr_Kind = Attribute_Max_Size_In_Storage_Elements then
                  if VM_Target = CLI_Target then
                     if JVM_Expr_Type (Attr) = Int_Type then
                        Gen_Push_Int (RM_Size (Standard_Character));
                     else
                        Gen_Push_Long (RM_Size (Standard_Character));
                     end if;

                  else
                     if Top_Type = Int_Type then
                        Gen_Push_Int (RM_Size (Standard_Character));
                     else
                        Gen_Push_Long (RM_Size (Standard_Character));
                     end if;
                  end if;

                  Gen_Div;
               end if;
            end;

         when Attribute_Tag =>
            --  The case of <object_name>'Tag is expanded by the front end
            --  into a selection of the object's "_tag" field, which is
            --  handled in Evaluate_Expression (N_Selected_Component).
            --  Here we just handle the case of <type_name>'Tag, whose
            --  expansion is suppressed for VMs in Expand_N_Attribute.

            pragma Assert (Is_Entity_Name (Attr_Prefix)
                             and then Is_Type (Entity (Attr_Prefix)));

            --  Get the class of the prefix type via java.lang.Class.forName

            case VM_Target is
               when CLI_Target =>
                  Gen_Push_String_Const
                    (Name_String (Name (JVM_Class (Entity (Attr_Prefix)))));
                  Gen_Invoke_API_Method (Class_forName);

               when JVM_Target =>
                  declare
                     S : constant String :=
                           Name_String
                            (Name (JVM_Class (Entity (Attr_Prefix))));
                  begin
                     --  In the JVM target we must add prefix JGNAT.ADALIB to
                     --  generate the Expanded_Name of tagged types declared in
                     --  the runtime.

                     if In_Runtime (Entity (Attr_Prefix)) then
                        Gen_Push_String_Const ("jgnat.adalib." & S);
                     else
                        Gen_Push_String_Const (S);
                     end if;

                     Gen_Invoke_API_Method (Class_forName);
                  end;

               when No_VM =>
                  pragma Assert (False);
                  raise Program_Error;
            end case;

         when Attribute_Val =>
            Evaluate_Expr (First (Expressions (Attr)));

            --  The result of the attribute argument's evaluation can be
            --  universal integer, in which case a type conversion may
            --  be needed.

            if Top_Type /= JVM_Expr_Type (Attr) then
               Gen_Conversion (JVM_Expr_Type (Attr));
            end if;

         when Attribute_Access |
              Attribute_Unchecked_Access |
              Attribute_Unrestricted_Access |
              Attribute_Address |
              Attribute_Code_Address =>

            --  If this is an unconstrained array access attribute, then we
            --  have to construct a compound pointer object that contains
            --  both the array reference and its bounds.

            if Attr_Kind /= Attribute_Address
              and then Attr_Kind /= Attribute_Code_Address
              and then
                Ekind (Designated_Type (Full_Type (Attr))) in Einfo.Array_Kind
              and then not Is_Constrained (Designated_Type (Full_Type (Attr)))
              and then Convention (Scope (Full_Type (Attr))) /= Convention_VM
            then
               Evaluate_Unconstrained_Array_Ref
                 (Attr_Prefix, Full_Type (Attr));

            --  In all other cases simply evaluate the prefix

            elsif Attr_Kind = Attribute_Unrestricted_Access
              and then Ekind (Prefix_Type) in Elementary_Kind
              and then not Is_Aliased_View (Attr_Prefix)
              and then not Comes_From_Source (Attr)
            then
               --  The frontend uses lots of unrestricted_access for 'Valid
               --  evaluation. We can safely use the following code in this
               --  case

               Addr := Evaluate_Addr (Attr_Prefix);

               case Addr.Addr_Kind is
                  when Local_Address =>
                     Gen_Default_Object
                       (Class_Of_Type (Descriptor_Type (Prefix_Type)));
                     Gen_Duplicate;
                     Gen_Load_Local (Addr.Local_Var);

                     pragma Assert (Is_Descriptor (Top_Type (Disp => 1)));
                     Gen_Put_Object_Field (Descriptor_Field (Prefix_Type));

                  when Indexed_Address =>
                     Gen_Load_Array_Element (Ref_Only => True);

                  when others =>
                     null;
               end case;

            elsif Attr_Kind /= Attribute_Address then
               --  ??? Very uncomfortable with this case: Addr is never used
               --  afterwards, so we won't know how to access the potential
               --  object on the stack ...
               Addr := Evaluate_Addr (Attr_Prefix);

               if Addr.Addr_Kind = Local_Address then
                  Gen_Load_Local (Addr.Local_Var);
               end if;

            --  In the case of Address applied to an array we call
            --  Evaluate_Array_Address in order to step past the the ".all"
            --  component of access-to-unconstrained objects so that we get a
            --  true array reference.

            elsif Ekind (Prefix_Type) in Einfo.Array_Kind then
               Evaluate_Array_Address (Attr_Prefix);

            --  In the case of Address applied to a subprogram name,
            --  Evaluate_Address will handle producing a subprogram access
            --  value. (We check this here to avoid falling into the following
            --  check for elementary objects in the case of a prefix denoting a
            --  function returning an elementary type.)

            elsif VM_Target = JVM_Target
               and then Nkind (Attr_Prefix) in N_Has_Entity
               and then Ekind (Entity (Attr_Prefix)) in Subprogram_Kind
            then
               Addr := Evaluate_Addr (Attr_Prefix);

            --  For CIL, load a function pointer for the Invoke method

            elsif VM_Target = CLI_Target
              and then Ekind (Prefix_Type) = E_Access_Subprogram_Type
            then
               declare
                  Subp : Method_Id;
               begin
                  Subp := JVM.Method (JVM_Class (Prefix_Type), "Invoke");
                  Gen_Load_Function_Pointer (Subp);
               end;

            --  If this is an unwrapped elementary object, then we can't take
            --  its address, so we generate an error.

            elsif Ekind (Prefix_Type) in Elementary_Kind
              and then not Is_Aliased_View (Attr_Prefix)
            then
               Gen_Push_Null;
               Error_Msg_N
                 ("unsupported construct, use aliased on object " &
                  "declaration", Attr);

            --  Otherwise it's safe to simply get the address

            else
               Addr := Evaluate_Addr (Attr_Prefix);

               if Addr.Addr_Kind = Local_Address then
                  Gen_Load_Local (Addr.Local_Var);
               end if;
            end if;

         when Attribute_Alignment =>
            if JVM_Expr_Type (Attr) = Int_Type then
               Gen_Push_Int (Alignment (Full_Subtype (Attr_Prefix)));
            else
               Gen_Push_Long (Alignment (Full_Subtype (Attr_Prefix)));
            end if;

         when others =>
            if JVM_Expr_Type (Attr) = Int_Type then
               Gen_Push_Int (Uint_0);
            else
               Gen_Push_Long (Uint_0);
            end if;

            Error_Msg_Name_1 := Attribute_Name (Attr);
            Error_Msg_N ("unimplemented attribute: %", Attr);
      end case;
   end Evaluate_Attribute;

   -------------------
   -- Evaluate_Expr --
   -------------------

   procedure Evaluate_Expr (Expr : Node_Id) is
      No_Label : Label_Id := Null_Label;
      Paren    : Node_Id;

   begin
      Evaluate_Expr (Expr, No_Label, False);

      if Do_Range_Check (Expr) then
         Paren := Parent (Expr);

         case Nkind (Paren) is
            when N_Qualified_Expression | N_Attribute_Reference =>
               Gen_Scalar_Subtype_Check (Etype (Paren), Paren);
            when others =>
               Gen_Scalar_Subtype_Check (Etype (Expr), Paren);
         end case;
      end if;
   end Evaluate_Expr;

   -------------------
   -- Evaluate_Expr --
   -------------------

   procedure Evaluate_Expr (Expr : Node_Id; Check_Subtype : Entity_Id) is
      No_Label : Label_Id := Null_Label;

   begin
      Evaluate_Expr (Expr, No_Label, False);

      if Do_Range_Check (Expr) then
         Gen_Scalar_Subtype_Check (Check_Subtype, Expr);
      end if;
   end Evaluate_Expr;

   -------------------
   -- Evaluate_Expr --
   -------------------

   procedure Evaluate_Expr
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean)
   is
      Exp_Kind    : constant Node_Kind := Nkind (Expr);
      Ada_Entity  : Entity_Id;
      A_Type      : Entity_Id := Full_Type (Expr);
      Etyp        : Entity_Id := Etype (Expr);
      Formal_Type : Type_Id;
      J_Type      : Type_Id;

   begin
      Debug_A_Entry ("(Ada-to-JVM) ", Expr);

      --  Handle private types

      if Present (Full_View (Etyp)) then
         Etyp := Full_View (Etyp);
      end if;

      case Exp_Kind is
         when N_Integer_Literal =>

            --  If literal is part of a bound, then the actual type to use is
            --  the type of the bound, not the type of the literal, forced
            --  sometimes to universal_integer.

            if Present (Parent (Expr))
              and then Nkind (Parent (Expr)) = N_Range
              and then Present (Etype (Parent (Expr)))
            then
               A_Type := Full_Type (Parent (Expr));
            end if;

            Evaluate_Integer_Literal (Expr, A_Type);

         when N_Real_Literal =>
            Evaluate_Real_Literal (Expr, A_Type);

         when N_Null =>
            Gen_Push_Null;

         when N_Identifier | N_Character_Literal | N_Expanded_Name =>
            Ada_Entity := Entity (Expr);
            J_Type := JVM_Expr_Type (Expr);

            if Exp_Kind = N_Character_Literal
              and then not Present (Ada_Entity)
            then
               Gen_Push_Int (Char_Literal_Value (Expr));

            --  If the entity denotes a static scalar constant whose value
            --  does not require a constant pool reference to load, then
            --  push the constant's corresponding literal value directly.
            --  Otherwise we load it from the field or local variable
            --  associated with the constant.

            elsif Compile_Time_Known_Value (Expr)
              and then Ekind (A_Type) in Scalar_Kind
              and then
                ((Ekind (A_Type) not in Einfo.Float_Kind
                   and then (J_Type = Type_Of (Java_Lang_Object)
                     or else
                       not Literal_Needs_Pool_Ref (J_Type, Expr_Value (Expr))))
                 or else (Ekind (A_Type) in Einfo.Float_Kind
                   and then
                     not Literal_Needs_Pool_Ref (J_Type, Expr_Value_R (Expr))))
            then
               --  The special case of System.Null_Address is caught
               --  here, and we load a null instead of the value zero.

               if J_Type = Type_Of (Java_Lang_Object) then
                  Gen_Push_Null;

               else
                  case JVM.Type_Kind (J_Type) is
                     when Boolean_Kind .. Int_Kind =>
                        Gen_Push_Int (Expr_Rep_Value (Expr), Expr);

                     when Long_Kind =>
                        Gen_Push_Long (Expr_Rep_Value (Expr));

                     when JVM.Float_Kind =>
                        Gen_Push_Float (Expr_Value_R (Expr));

                     when Double_Kind =>
                        Gen_Push_Double (Expr_Value_R (Expr));

                     when others =>
                        pragma Assert (False);
                        raise Program_Error;
                  end case;
               end if;

            else
               case Ekind (Ada_Entity) is
                  when E_Variable | E_Constant =>
                     if Present (Renamed_Object (Ada_Entity))
                       and then Ekind (A_Type) in Elementary_Kind
                     then
                        --  If this is a renaming of an elementary entity,
                        --  simply evaluate the renamed object and return.

                        if Is_Entity_Name (Renamed_Object (Ada_Entity)) then
                           Evaluate_Expr (Renamed_Object (Ada_Entity));
                           return;

                        --  If the renamed object is a
                        --  selected/indexed/explicit component or an unchecked
                        --  type conversion then continue, and let the
                        --  following code take care of loading the reference
                        --  to the containing object, which is the JVM entity
                        --  associated with the renaming declaration entity
                        --  (see jx_ch8).
                        --  The loading of the elementary component field
                        --  itself will happen further below, after loading
                        --  the containing object's reference.

                        --  If this is a reference to a renamed function
                        --  call, then the renaming is simply an elementary
                        --  entity that contains the result, so we let the
                        --  processing continue as for a normal evaluation.

                        elsif Nkind_In (Renamed_Object (Ada_Entity),
                                N_Selected_Component,
                                N_Explicit_Dereference,
                                N_Indexed_Component,
                                N_Unchecked_Type_Conversion,
                                N_Function_Call)
                        then
                           null;

                        --  Blow up in other cases (e.g., indexed components),
                        --  which aren't supported yet. ???

                        else
                           pragma Assert (False);
                           raise Program_Error;
                        end if;

                     --  AARM 8.5.1 (6/2): Constraints implied by the renaming
                     --  declaration are ignored.

                     elsif Present (Renamed_Object (Ada_Entity))
                       and then Nkind (Renamed_Object (Ada_Entity)) = N_Slice
                     then
                        Evaluate_Expr (Renamed_Object (Ada_Entity));
                        return;
                     end if;

                     --  Library level entity

                     if Is_Global_Entity (Ada_Entity)
                       or else Is_Imported (Ada_Entity)
                     then
                        declare
                           The_Field : constant Field_Id :=
                                         JVM_Field (Ada_Entity);
                        begin
                           Gen_Get_Static_Field (The_Field);
                           Formal_Type := Type_Of (The_Field);
                        end;

                     --  Entity defined in the current scope

                     elsif Enclosing_Method (Ada_Entity) = Current_Method then

                        --  If the local variable has been allocated to a field
                        --  in the current method's AR object, then load its
                        --  value from that field if its type is scalar or
                        --  access (otherwise we can get the value from the
                        --  local variable since it's effectively a constant).

                        if Access_From_Current_AR (Ada_Entity) then
                           Gen_Load_Local (AR_Stack.Top.AR_Obj);

                           declare
                              The_Field : constant Field_Id :=
                                           AR_Field (AR_Stack.Top, Ada_Entity);
                           begin
                              Gen_Get_Field (The_Field);
                              Formal_Type := Type_Of (The_Field);
                           end;

                        else
                           Gen_Load_Local (JVM_Local_Var (Ada_Entity));
                           Formal_Type := Type_Of (JVM_Local_Var (Ada_Entity));
                        end if;

                     --  Entity defined in an enclosing scope. Load the local
                     --  variable from an up-level activation frame object.

                     else
                        declare
                           AR_Field : constant Field_Id :=
                                        Access_AR_Field (Ada_Entity);
                        begin
                           Gen_Get_Field (AR_Field);
                           Formal_Type := Type_Of (AR_Field);
                        end;
                     end if;

                     --  In case of renamed dereferenced object, we don't get
                     --  the object, but a pointer on this object. We need to
                     --  dereference it now.

                     if Ekind (Full_Type (Ada_Entity)) not in Composite_Kind
                       and then Present (Renamed_Object (Ada_Entity))
                       and then not
                         Is_Entity_Name (Renamed_Object (Ada_Entity))
                       and then Nkind (Renamed_Object (Ada_Entity))
                                  = N_Explicit_Dereference
                     then
                        pragma Assert (Is_Descriptor (Top_Type));
                        Gen_Get_Field (Descriptor_Field (Formal_Type));

                        --  If the dereferenced type is of type Access_Type,
                        --  then we need to cast: this happens when we have
                        --  a double dereference.
                        --  However in some cases, the corresponding J_Type
                        --  is an array type, so account also for this case.

                        if Ekind (A_Type) in Access_Kind then
                           J_Type := JVM_Type (A_Type);

                           if JVM.Type_Kind (J_Type) = Class_Kind then
                              Gen_Check_Cast (Class_Of_Type (J_Type));
                           end if;
                        end if;
                     end if;

                     --  If the object is a renaming of an elementary object,
                     --  then at this point it must be the renaming of a
                     --  a selected component (or function call, see above).
                     --  In this case the reference to the component's
                     --  containing object has been loaded, and so we now
                     --  load the component itself. Support for other cases,
                     --  such as renamings of indexed components, will be
                     --  added later. ???

                     if Present (Renamed_Object (Ada_Entity))
                       and then Ekind (A_Type) in Elementary_Kind
                       and then Nkind (Renamed_Object (Ada_Entity))
                                  /= N_Function_Call
                     then
                        if Nkind (Renamed_Object (Ada_Entity))
                          = N_Selected_Component
                        then
                           declare
                              Selector : constant Entity_Id :=
                                           Entity
                                            (Selector_Name
                                              (Renamed_Object (Ada_Entity)));

                           begin
                              Gen_Get_Object_Field (JVM_Field (Selector));

                              --  Change the Ada_Entity to the selected
                              --  component to allow handling the case
                              --  where it's wrapped.

                              Ada_Entity := Selector;
                           end;

                        elsif Nkind (Renamed_Object (Ada_Entity))
                          = N_Indexed_Component
                        then
                           Gen_Array_Subscript
                             (Prefix (Renamed_Object (Ada_Entity)),
                              First
                                (Expressions (Renamed_Object (Ada_Entity))));

                           Gen_Load_Array_Element;

                        elsif Nkind_In (Renamed_Object (Ada_Entity),
                                        N_Explicit_Dereference,
                                        N_Unchecked_Type_Conversion)
                        then
                           null;

                        else
                           pragma Assert (False);
                           raise Program_Error;
                        end if;
                     end if;

                     --  If this is an aliased object with a wrapper type,
                     --  then load its 'all' field.

                     if Needs_Access_Descriptor (Ada_Entity) then
                        pragma Assert (Is_Descriptor (Top_Type));
                        Gen_Get_Object_Field (Descriptor_Field (Ada_Entity));

                        --  The 'all' component for an access type wrapper is
                        --  of class Object, so a downcast to the formal type
                        --  is needed to satisfy consistency checking of the
                        --  operand stack.

                        if Ekind (Full_Type (Ada_Entity)) in Access_Kind then
                           Gen_Check_Cast
                             (Class_Of_Type
                               (JVM_Type (Full_Type (Ada_Entity))));
                        end if;
                     end if;

                  when Formal_Kind =>
                     if Enclosing_Method (Ada_Entity) = Current_Method then
                        Gen_Load_Local (JVM_Local_Var (Ada_Entity));

                     --  Load the local variable from an up-level activation
                     --  frame object

                     else
                        Gen_Get_Field (Access_AR_Field (Ada_Entity));
                     end if;

                     if Needs_Access_Descriptor (Ada_Entity) then
                        pragma Assert (Is_Descriptor (Top_Type));
                        Gen_Get_Field (Descriptor_Field (Ada_Entity));

                        --  The 'all' component for an access type wrapper is
                        --  of class Object, so a downcast to the formal type
                        --  is needed to satisfy consistency checking of the
                        --  operand stack.

                        if Ekind (Full_Type (Ada_Entity)) in Access_Kind then
                           if JVM.Type_Kind (JVM_Type (Full_Type (Ada_Entity)))
                             = JVM.Class_Kind
                           then
                              Gen_Check_Cast
                                (Class_Of_Type
                                   (JVM_Type (Full_Type (Ada_Entity))));
                           else
                              Gen_Check_Cast
                                (JVM_Type (Full_Type (Ada_Entity)));
                           end if;
                        end if;

                     --  If the formal is a controlling parameter of a
                     --  dispatching operation, then its JVM type may have
                     --  been changed to some parent type (in the case
                     --  of an overriding operation). In that case we
                     --  have to cast the parameter value to the JVM
                     --  type associated with the formal's Ada type.

                     elsif Is_Controlling_Formal (Ada_Entity)
                       and then
                         JVM_Type (Find_Dispatching_Type (Scope (Ada_Entity)))
                           /= Type_Of (JVM_Local_Var (Ada_Entity))
                     then
                        Gen_Check_Cast
                          (Class_Of_Type (JVM_Type (Full_Type (Ada_Entity))));
                     end if;

                  when E_Loop_Parameter =>

                     --  If the loop parameter is global (such as declared
                     --  in a library-level package), then we still treat
                     --  it as a local variable since it will just be used
                     --  within an elaboration method. The check for the
                     --  Enclosing_Subprogram is a kludge to prevent
                     --  blow-ups in Enclosing_Method if the enclosing
                     --  "subprogram" is a task entry (seems like this
                     --  shouldn't occur in expanded code, but it does
                     --  for some reason -- see c95086f); in that case
                     --  we assume it's not an up-level reference. ???

                     if (Is_Global_Entity (Ada_Entity)
                         and then Parent_Method (Current_Method) = Null_Method)
                       or else Ekind (Enclosing_Subprogram (Ada_Entity))
                                 not in Subprogram_Kind
                       or else Enclosing_Method (Ada_Entity) = Current_Method
                     then
                        --  If the local variable has been allocated to
                        --  a field in the current method's AR object,
                        --  then load its value from that field, otherwise
                        --  we can get the value from the local variable.

                        if Access_From_Current_AR (Ada_Entity) then
                           Gen_Load_Local (AR_Stack.Top.AR_Obj);
                           Gen_Get_Field
                             (AR_Field (AR_Stack.Top, Ada_Entity));
                        else
                           Gen_Load_Local (JVM_Local_Var (Ada_Entity));
                        end if;

                     else
                        --  We have to load the local variable from
                        --  an up-level activation frame object.

                        Gen_Get_Field (Access_AR_Field (Ada_Entity));
                     end if;

                  when E_Discriminant =>
                     pragma Assert (Present (Discriminal (Ada_Entity)));
                     Gen_Load_Local (JVM_Local_Var (Discriminal (Ada_Entity)));

                  when E_Enumeration_Literal =>
                     Gen_Push_Int (Enumeration_Rep (Ada_Entity));

                  --  When evaluating an exception we want to push its
                  --  associated class, not an instance of the exception,
                  --  so we generate an instance and call getClass.
                  --  This should only occur for constructs such as
                  --  exc'Identity (which gets mapped to exc'Reference
                  --  by the front end). Is there an easier way to
                  --  generate the class that doesn't require creating
                  --  an exception instance ???

                  when E_Exception =>
                     Gen_Default_Object (JVM_Class (Ada_Entity));
                     Gen_Invoke_API_Method (Object_getClass);

                  when others =>
                     pragma Assert (False);
                     raise Program_Error;
               end case;
            end if;

         when N_Selected_Component =>
            declare
               Selector   : constant Entity_Id :=
                              Entity (Selector_Name (Expr));
               J_Field    : Field_Id;
               Desig_Type : Entity_Id;

            begin
               --  If the selector is an access discriminant designating a Java
               --  interface type, then treat the selection specially by simply
               --  evaluating the prefix and changing the type of the result to
               --  the interface type. This kind of selection is treated as a
               --  conversion of the prefix object to the Java interface type.

               if Ekind (Selector) = E_Discriminant
                 and then Is_Access_Type (Etype (Selector))
               then
                  Desig_Type := Directly_Designated_Type (Etype (Selector));

                  if Is_Tagged_Type (Desig_Type)
                    and then Is_Interface (JVM_Class (Full_Type (Desig_Type)))
                  then
                     Evaluate_Expr (Prefix (Expr));
                     Pop_Type;
                     Push_Type (JVM_Type (Desig_Type));

                     return;
                  end if;
               end if;

               Evaluate_Expr (Prefix (Expr));

               --  References to the _tag component of a tagged object are
               --  converted to calls to the API Object.getClass method,
               --  which returns a reference to the java.lang.Class object
               --  associated with the prefix object's JVM class.

               if Chars (Selector) = Name_uTag then
                  Gen_Invoke_API_Method (Object_getClass);

               --  If the selector is a _parent field, then we elide the field
               --  selection and only evaluate the prefix. This usage should
               --  only occur in contexts where passing the containing object
               --  is appropriate in any case (the selection of the _parent
               --  field is equivalent to a conversion to the parent type).

               elsif Chars (Selector) /= Name_uParent then
                  J_Field := JVM_Field (Selector);
                  Gen_Get_Object_Field (J_Field);

                  if Needs_Access_Descriptor (Selector) then
                     pragma Assert (Is_Descriptor (Top_Type));
                     Gen_Get_Object_Field (Descriptor_Field (Selector));

                     --  The 'all' component for an access type wrapper is of
                     --  class Object, so a downcast to the class of the access
                     --  type is needed to satisfy consistency checking of the
                     --  operand stack.

                     if Ekind (A_Type) in Access_Kind then
                        Gen_Check_Cast (Class_Of_Type (JVM_Type (A_Type)));
                     end if;

                  --  If the selector is an access discriminant linked to a
                  --  parent discriminant, then we force the type (via a
                  --  checkcast) to be the selector's designated type in the
                  --  case where it has a different type than the discriminant
                  --  that it constrains. This ensures compatibility with the
                  --  type required by the context.

                  elsif Ekind (Selector) = E_Discriminant
                    and then Is_Access_Type (Etype (Selector))
                    and then Present (Corresponding_Discriminant (Selector))
                    and then Etype (Selector)
                              /= Etype (Corresponding_Discriminant (Selector))
                  then
                     Gen_Check_Cast (Class_Of_Type (JVM_Type (Desig_Type)));
                  end if;
               end if;
            end;

         when N_Indexed_Component =>
            Evaluate_Expr (Prefix (Expr));
            Gen_Array_Subscript (Prefix (Expr), First (Expressions (Expr)));
            Gen_Load_Array_Element;

            --  A wrapped scalar or access component requires loading the 'all'
            --  field from the wrapper object.

            if Has_Aliased_Components (Full_Type (Prefix (Expr)))
              and then Ekind (Full_Type (Expr)) in Wrappable_Kind
            then
               pragma Assert (Is_Descriptor (Top_Type));
               Gen_Get_Object_Field (Descriptor_Field (Full_Type (Expr)));

               --  The 'all' component for an access type wrapper is of class
               --  Object, so a downcast to the class of the access type is
               --  needed to satisfy consistency checking of the operand stack.

               if Ekind (A_Type) in Access_Kind then
                  Gen_Check_Cast (Class_Of_Type (JVM_Type (A_Type)));
               end if;

            --  If the JVM array element type is byte and the array component
            --  type is unsigned with an upper bound greater than 127, then we
            --  have to mask the result of the load, which will be
            --  unconditionally sign extended by the 'baload' instruction.

            else
               declare
                  Comp_Type : constant Entity_Id :=
                                Full_Type
                                 (Component_Type (Full_Type (Prefix (Expr))));
               begin
                  if JVM_Type (Comp_Type) = Byte_Type
                    and then Is_Unsigned_Type (Comp_Type)
                    and then
                      (Nkind (Type_High_Bound (Comp_Type)) /= N_Integer_Literal
                        or else Expr_Value (Type_High_Bound (Comp_Type)) > 127)
                  then
                     Gen_Push_Int (UI_From_Int (2 ** 8 - 1));
                     Gen_And;
                  end if;
               end;
            end if;

         when N_Slice =>

            --  Check if the expressions of the slice range were replaced by
            --  the frontend by CE. Otherwise we only perform the evaluation of
            --  the prefix. It is then up to the demanding expression to take
            --  into account the slice itself.

            declare
               D  : constant Node_Id := Discrete_Range (Expr);
               LB : Node_Id := Empty;
               HB : Node_Id := Empty;

            begin
               if Nkind (D) = N_Range then
                  LB := Low_Bound (D);
                  HB := High_Bound (D);

               elsif Nkind (D) = N_Subtype_Indication then
                  LB := Low_Bound (Range_Expression (Constraint (D)));
                  HB := High_Bound (Range_Expression (Constraint (D)));
               end if;

               --  If some bound raises CE we must evaluate the node to
               --  generate the code that raises the exception and we must
               --  also replace the integer value in the top of the stack by
               --  "null" because the caller expects to receive the address
               --  of the slice

               if Present (LB)
                 and then Nkind (LB) = N_Raise_Constraint_Error
               then
                  Evaluate_Expr (LB);

                  Gen_Pop;
                  Gen_Push_Null;
                  return;

               elsif Present (HB)
                 and then Nkind (HB) = N_Raise_Constraint_Error
               then
                  Evaluate_Expr (HB);

                  Gen_Pop;
                  Gen_Push_Null;
                  return;
               end if;

               Evaluate_Expr (Prefix (Expr));
            end;

         when N_Explicit_Dereference =>
            Evaluate_Expr (Prefix (Expr));

            --  If this is a dereference of an access value designating a
            --  scalar or access value then load the 'all' field from
            --  the wrapper object.

            if Ekind (A_Type) in Wrappable_Kind then
               Gen_Get_Field (Descriptor_Field (A_Type));

               --  The 'all' component for an access type wrapper is of class
               --  Object, so a downcast to the class of the access type is
               --  needed to satisfy consistency checking of the operand stack.

               if Ekind (A_Type) in Access_Kind then
                  if JVM.Type_Kind (JVM_Type (A_Type)) = JVM.Class_Kind then
                     Gen_Check_Cast (Class_Of_Type (JVM_Type (A_Type)));
                  else
                     Gen_Check_Cast (JVM_Type (A_Type));
                  end if;
               end if;
            end if;

         when N_Function_Call =>

            --  For a call to a Java-imported function returning an
            --  access-to-unconstrained array result, we need to construct a
            --  wrapper object for the array and its bounds (unless the access
            --  type itself is declared in a package with convention Java).

            if Is_Entity_Name (Name (Expr))
              and then Convention (Entity (Name (Expr))) = Convention_VM
              and then Ekind (Full_Type (Expr)) in Access_Kind
              and then Ekind (Designated_Type (Full_Type (Expr)))
                         in Einfo.Array_Kind
              and then not Is_Constrained (Designated_Type (Full_Type (Expr)))
              and then Convention (Scope (Full_Type (Expr))) /= Convention_VM
            then
               Evaluate_Unconstrained_Array_Ref (Expr, Etype (Expr));
               pragma Assert (Is_Array_Descriptor (Top_Type));

            else
               Translate_Subprogram_Call (Expr);
            end if;

         when N_Procedure_Call_Statement =>
            Translate_Subprogram_Call (Expr);

         when N_Attribute_Reference =>
            if Nkind (Prefix (Expr)) = N_Expression_With_Actions then
               Evaluate_Expr (Prefix (Expr));
               Gen_Pop;
            end if;

            Evaluate_Attribute (Expr);

         when N_Binary_Op | N_Unary_Op =>
            Evaluate_Operator (Expr, Label, True_Branch);

         when N_And_Then =>
            Evaluate_And_Then (Expr, Label, True_Branch);

         when N_Or_Else =>
            Evaluate_Or_Else (Expr, Label, True_Branch);

         when N_In | N_Not_In =>
            Evaluate_Membership_Test (Expr, Label, True_Branch);

         when N_If_Expression =>
            declare
               Condition   : constant Node_Id  := First (Expressions (Expr));
               Then_Expr   : constant Node_Id  := Next (Condition);
               Else_Expr   : constant Node_Id  := Next (Then_Expr);
               False_Label : constant Label_Id := New_Label;
               Save_Label  : Label_Id          := False_Label;
               Exit_Label  : constant Label_Id := New_Label;
               Check_State : Boolean;

            begin
               Suppress_Stack_Checking (Check_State);

               Evaluate_Expr (Condition, Save_Label, False);

               --  If Save_Label was unused during the condition evaluation,
               --  then generate a branch to it now based on the Boolean
               --  top-of-stack value.

               if Save_Label /= Null_Label then
                  Gen_Branch_Equal (False_Label);
               end if;

               Evaluate_Expr (Then_Expr);

               --  For strings the evaluation of Else_Expr may generate an
               --  array descriptor. Given that we cannot know in advance if it
               --  will be generated, we assume the worst case and enforce the
               --  generation of array descriptors for both string expressions.

               if Etype (Expr) = Standard_String
                 and then not Is_Array_Descriptor (Top_Type)
               then
                  Generate_Array_Descriptor (Expression (Then_Expr));
               end if;

               Gen_Goto (Exit_Label);

               Gen_Label (False_Label);
               Mark_Stack;

               Evaluate_Expr (Else_Expr);

               if Etype (Expr) = Standard_String
                 and then not Is_Array_Descriptor (Top_Type)
               then
                  Generate_Array_Descriptor (Expression (Else_Expr));
               end if;

               Release_Stack;

               Gen_Label (Exit_Label);

               Restore_Stack_Checking (Check_State);
            end;

         when N_Expression_With_Actions =>
            declare
               Check_State : Boolean;
            begin
               --  Evaluate it with stack consistency turned off. Required to
               --  support this kind of node as the actual of a subprogram call

               Suppress_Stack_Checking (Check_State);
               Translate_Statements (Actions (Expr));
               Evaluate_Expr (Expression (Expr));
               Restore_Stack_Checking (Check_State);
            end;

         when N_Qualified_Expression =>
            --  NOTE: Eventually need to perform subtype checks ???
            Evaluate_Expr (Expression (Expr));

         when N_Reference =>

            --  If this is an unconstrained array reference, then we
            --  have to construct a compound pointer object that contains
            --  both the array reference and its bounds (unless the
            --  access type comes from a Java-convention package).

            if Ekind (Designated_Type (Etype (Expr))) in Einfo.Array_Kind
              and then not Is_Constrained (Designated_Type (Etype (Expr)))
            then
               Evaluate_Unconstrained_Array_Ref (Prefix (Expr), Etype (Expr));

            --  In all other cases simply evaluate the prefix

            else
               Evaluate_Expr (Prefix (Expr));

               --  In the CLI target handle reference to ultra-flat array
               --  generating a pointer to null. This code is a kludge which
               --  is rejected by the JVM Verifier but fortunately it is not
               --  needed in the JVM target.

               if VM_Target = CLI_Target
                 and then Is_Array_Type (Designated_Type (Etype (Expr)))
               then
                  --  Generate:
                  --     if top = null then
                  --        pop;
                  --        push new Ada_Acc'(all=>null);
                  --     end if;

                  declare
                     Top_T : constant Type_Id := Top_Type;
                     Jtyp  : constant Type_Id := Type_Of (API_Class (Ada_Acc));
                     Lbl   : constant Label_Id := New_Label;
                     State : Boolean;

                  begin
                     Suppress_Stack_Checking (State);
                     Gen_Duplicate;
                     Gen_Branch_If_Not_Null (Lbl);
                     Gen_Pop;

                     Gen_Default_Object (API_Class (Ada_Acc));
                     Gen_Duplicate;

                     Gen_Push_Null;
                     Gen_Put_Field (Field (Class_Of_Type (Jtyp), "all"));
                     Gen_Label (Lbl);
                     Restore_Stack_Checking (State);

                     Pop_Type;
                     Push_Type (Top_T);
                  end;
               end if;
            end if;

         when N_String_Literal =>
            Evaluate_String_Literal (Strval (Expr), A_Type);

         when N_Type_Conversion =>
            if Ekind (A_Type) in Decimal_Fixed_Point_Kind
              and then Ekind (Etype (Expression (Expr))) in Einfo.Float_Kind
            then
               Evaluate_Expr (Expression (Expr));
               Gen_Conversion
                 (JVM_Expr_Type (Expr), not Float_Truncate (Expr));

            elsif Ekind (A_Type) in Einfo.Float_Kind
              or else Ekind (A_Type) in Discrete_Kind
            then
               Evaluate_Expr (Expression (Expr));
               Check_For_Overflow (Expr);
               Gen_Conversion
                 (JVM_Expr_Type (Expr), not Float_Truncate (Expr));

               --  If this conversion is from an unsigned 32-bit integer to a
               --  64-bit integer (signed or unsigned), then we need to mask
               --  the result to clear any sign extension (the JVM operations
               --  only handle signed types).

               if Ekind (Etype (Expression (Expr))) in Modular_Integer_Kind
                 and then
                   (JVM.Type_Kind (JVM_Type (A_Type)) = Long_Kind
                     and then JVM.Type_Kind (JVM_Expr_Type (Expression (Expr)))
                                = Int_Kind)
               then
                  Gen_Push_Long (Uint_2 ** 32 - Uint_1);
                  Gen_And;
               end if;

               --  The range check must be performed after the conversion
               --  instead of being done by the expression evaluation because
               --  the conversion may involve a representation change and
               --  the value must be checked against the bounds of the
               --  target type.

               if Do_Range_Check (Expression (Expr)) then
                  Gen_Scalar_Subtype_Check (Etype (Expr), Expr);
               end if;

               if Ekind (A_Type) in Modular_Integer_Kind
                 and then Modulus (A_Type) < 2 ** Uint_32
               then
                  declare
                     Exc_Label    : constant Label_Id := New_Label;
                     No_Exc_Label : constant Label_Id := New_Label;
                     Check_State  : Boolean;

                  begin
                     Suppress_Stack_Checking (Check_State);

                     --  If the result of the conversion is outside the base
                     --  range of the modular type then raise Constraint_Error.

                     Gen_Duplicate;
                     Gen_Branch_Less (Exc_Label);
                     Gen_Duplicate;
                     Gen_Push_Int (Modulus (A_Type), A_Type);
                     Gen_Compare_Branch_Less (No_Exc_Label);
                     Gen_Label (Exc_Label);
                     Generate_Exception_And_Throw
                       (API_Class (Ada_Constraint_Error), Sloc (Expr),
                        UI_From_Int
                          (RT_Exception_Code'Pos (CE_Range_Check_Failed)));
                     Gen_Label (No_Exc_Label);

                     Restore_Stack_Checking (Check_State);
                  end;
               end if;

            else
               Evaluate_Expr (Expression (Expr));

               --  Evaluation of explicit dereferences of unconstrained array
               --  types do not generate code referencing the "all" field
               --  because attributes 'first and 'last may be needed at later
               --  stages. Therefore, for type conversion of unconstrained
               --  array into constrained array must generate code now
               --  referencing the "all" field.

               if Nkind (Expression (Expr)) = N_Explicit_Dereference
                 and then Is_Array_Type (Etype (Expr))
                 and then Is_Constrained (Etype (Expr))
                 and then not Is_Constrained (Etype (Expression (Expr)))
               then
                  pragma Assert (Is_Array_Descriptor (Top_Type));
                  Gen_Get_Field (Descriptor_Field (Top_Type));
               end if;

               --  Casts should never be needed when the result is an array
               --  type, so skip the conversion, which avoids problems when
               --  the conversion argument is a dereference of an access-to-
               --  unconstrained array (since the stack result in that case
               --  is an access to a wrapped pointer and should not be
               --  converted since it will need to be dereferenced at a
               --  later point).

               if not Is_Array_Type (A_Type) then
                  Gen_Conversion (JVM_Expr_Type (Expr));
               end if;

               --  Are there any cases where additional actions are required,
               --  or does front end expansion take care of those ???
            end if;

         when N_Unchecked_Type_Conversion =>

            --  If the target is a class or array type, then we grab the
            --  address of the first local and do a block copy, unless of
            --  course we are doing a conversion on the pointer itself.

            if JVM_Expr_Type (Expr) = JVM_Expr_Type (Expression (Expr)) then
               Evaluate_Expr (Expression (Expr));

               --  Handle unchecked conversions that involve array descriptors

               if Top_Type /= JVM_Expr_Type (Expr)
                 and then Is_Array_Descriptor (Top_Type)
               then
                  pragma Assert
                    (Top_Type = Descriptor_Type (JVM_Expr_Type (Expr)));
                  Gen_Get_Field (Descriptor_Field (Top_Type));
               end if;

            --  Handle special case of conversion to System.Address because it
            --  is currently mapped to java.lang.Object (instead of
            --  native_int_type). See Jx_Decl.Declare_Discrete_Type.

            elsif Is_Descendent_Of_Address (Etyp) then
               pragma Assert
                 (JVM_Expr_Type (Expr) = Type_Of (Java_Lang_Object));

               Evaluate_Expr (Expression (Expr));

               if JVM.Type_Kind (Top_Type) = Long_Kind then
                  Gen_Conversion (Int_Type);
               end if;

               Gen_Conversion (JVM_Expr_Type (Expr));
               Gen_Annotation (Top_Is_Address);

            elsif JVM.Type_Kind (JVM_Expr_Type (Expr)) = Class_Kind
              or else JVM.Type_Kind (JVM_Expr_Type (Expr)) = JVM.Array_Kind
            then
               --  Here we just hope that the arrays are fundamentally
               --  represented the same way and that we can get away with not
               --  doing anything

               if Ekind (Etyp) = E_Array_Subtype then

                  --  Unchecked conversion of elementary type to arrays require
                  --  support for pragma Pack and this feature is currently
                  --  unsupported in .NET.

                  if Ekind (Etype (Expression (Expr))) in Elementary_Kind then

                     --  We should handle here Error_Msg_N but doing it we
                     --  would generate a duplicate error message because it
                     --  has been previously reported when processing the
                     --  N_Unchecked_Type_Conversion node.

                     --  We evaluate the expression and change the top-of-stack
                     --  type to allow the backend to continue processing the
                     --  tree and report other errors (if any).

                     Evaluate_Expr (Expression (Expr));
                     Pop_Type;
                     Push_Type (JVM_Expr_Type (Expr));

                     return;
                  else
                     Evaluate_Expr (Expression (Expr));
                  end if;

               elsif Ekind (Etyp) = E_Access_Subtype
                 or else Ekind (Etyp) = E_Access_Type
                 or else Ekind (Etyp) = E_General_Access_Type
                 or else Ekind (Etyp) = E_Anonymous_Access_Type
                 or else First_Field (Class_Of_Type (JVM_Expr_Type (Expr)))
                          = Null_Field
                 or else Nkind (Parent (Expr)) /= N_Assignment_Statement
               then
                  Evaluate_Expr (Expression (Expr));

                  --  No code emitted for unchecked conversion of null to
                  --  access type

                  if Top_Type = Any_Ref_Type then
                     Pop_Type;
                     Push_Type (JVM_Expr_Type (Expr));

                  --  UC of array to array type

                  elsif JVM.Type_Kind (Top_Type) = JVM.Array_Kind
                    and then JVM.Type_Kind (JVM_Expr_Type (Expr))
                               = JVM.Array_Kind
                  then
                     Pop_Type;
                     Push_Type (JVM_Expr_Type (Expr));

                  --  UC of array type to record type. More work needed here???

                  elsif JVM.Type_Kind (Top_Type) = JVM.Array_Kind
                    and then JVM.Type_Kind (JVM_Expr_Type (Expr))
                               = JVM.Class_Kind
                  then
                     Pop_Type;
                     Push_Type (JVM_Expr_Type (Expr));

                  --  UC of record type to record type

                  elsif JVM.Type_Kind (Top_Type) = Class_Kind
                    and then JVM.Type_Kind (JVM_Expr_Type (Expr)) = Class_Kind
                  then
                     --  Currently we do not have support for unchecked
                     --  conversion of record types and we just generate a
                     --  runtime check which requires both types to be follow
                     --  the rules of the .NET CLR.

                     --  In CLI target, in order to support reading record
                     --  types with Ada.Sequential_IO and Ada.Direct_IO we add
                     --  here an exception which assumes that the type used to
                     --  read the file contents matches the type used to write
                     --  their contents. It must be removed when we incorporate
                     --  support for unchecked conversion???

                     --  We cannot add this exception in the JVM target since
                     --  the code is rejected at runtime by the Verifier.

                     declare
                        E      : constant Entity_Id :=
                                   Cunit_Entity
                                     (Get_Source_Unit (Sloc (Etype (Expr))));
                        E_Name : constant String := Name_String (Chars (E));

                     begin
                        if VM_Target = CLI_Target
                          and then In_Runtime (E)
                          and then Chars (Scope (E)) = Name_Ada
                          and then (E_Name = "sequential_io"
                                      or else E_Name = "direct_io")
                          and then Name_String (Chars (Scope (Etyp))) = "read"
                        then
                           null;
                        else
                           Gen_Class_Conversion (JVM_Expr_Type (Expr));
                        end if;
                     end;
                  else
                     begin
                        Gen_Conversion (JVM_Expr_Type (Expr));
                     exception
                        when others =>
                           Error_Msg_N
                             ("unsupported construct in this context",
                              Expr);
                     end;
                  end if;

               elsif VM_Target = CLI_Target then
                  --  load dest (done in parent), then source, then size
                  --  for cpblk

                  Gen_Get_Object_Field_Address
                    (First_Field (Class_Of_Type (JVM_Expr_Type (Expr))));
                  Evaluate_Expr (Expression (Expr));

                  Gen_Get_Object_Field_Address
                    (First_Field (Class_Of_Type (JVM_Expr_Type
                      (Expression (Expr)))));

                  Gen_Push_Int (Esize (Etype (Expression (Expr))));
                  Gen_Block_Copy;
                  Push_Type (JVM_Expr_Type (Expr));
               end if;

            elsif JVM.Type_Kind (JVM_Expr_Type (Expr)) = JVM.Byte_Kind
              or else JVM.Type_Kind (JVM_Expr_Type (Expr)) = JVM.Char_Kind
              or else JVM.Type_Kind (JVM_Expr_Type (Expr)) = JVM.Short_Kind
              or else JVM.Type_Kind (JVM_Expr_Type (Expr)) = JVM.Int_Kind
              or else JVM.Type_Kind (JVM_Expr_Type (Expr)) = JVM.Long_Kind
              or else JVM.Type_Kind (JVM_Expr_Type (Expr)) = JVM.Float_Kind
              or else JVM.Type_Kind (JVM_Expr_Type (Expr)) = JVM.Double_Kind
            then
               if JVM.Type_Kind (JVM_Expr_Type (Expression (Expr)))
                 in Byte_Kind .. Double_Kind
               then
                  --  Conversion int to int, handled by CIL/JVM

                  Evaluate_Expr (Expression (Expr));
                  Gen_Conversion (JVM_Expr_Type (Expr));

               elsif VM_Target = CLI_Target then
                  --  Unchecked conversion...

                  declare
                     Exp    : constant Node_Id := Expression (Expr);
                     Local2 : constant Local_Var_Id :=
                                New_Local_Var
                                  ("_conv_tmp2", JVM_Expr_Type (Exp));
                  begin
                     Evaluate_Expr (Exp);

                     --  ??? here we hope that the objects have the same size.
                     --  if not, the rest if filled with 0, which can be
                     --  a problem with negative values.

                     --  In case of conversion from an array, then we load the
                     --  address of the first element instead of the address of
                     --  the array object.

                     if Ekind (Full_Type (Exp)) in Einfo.Array_Kind then
                        Gen_Push_Int (Uint_0);
                        Gen_Load_Array_Element_Address;

                     else
                        Gen_Store_Local (Local2);
                        Gen_Load_Local_Address (Local2);
                     end if;

                     Gen_Load_Indirect (JVM_Expr_Type (Expr));
                  end;

               else
                  Push_Type (JVM_Expr_Type (Expr));
                  Error_Msg_N
                    ("unsupported form of unchecked_conversion", Expr);
               end if;

            elsif VM_Target = CLI_Target then
               --  If it is a local variable, then grab the addresses and
               --  do a block copy

               declare
                  Local  : constant Local_Var_Id :=
                             New_Local_Var
                               ("_conv_tmp", JVM_Expr_Type (Expr));
                  Local2 : constant Local_Var_Id :=
                             New_Local_Var
                               ("_conv_tmp2",
                                JVM_Expr_Type (Expression (Expr)));

               begin
                  --  Load dest, then source, then size for cpblk

                  Evaluate_Expr (Expression (Expr));
                  Gen_Store_Local (Local2);

                  Gen_Load_Local_Address (Local);
                  Gen_Load_Local_Address (Local2);
                  Gen_Sizeof (JVM_Expr_Type (Expr));
                  Gen_Block_Copy;
                  Gen_Load_Local (Local);
               end;
            end if;

            --  In case of dereferenced arrays, we have on the stack an object
            --  instead of the raw array. We then need to extract the ::all
            --  field of this object.

            if Ekind (Etype (Expr)) = E_Array_Subtype
              and then Nkind (Expression (Expr)) = N_Explicit_Dereference
              and then Is_Descriptor (Top_Type)
            then
               Gen_Get_Field
                 (Descriptor_Field
                   (JVM_Expr_Type (Full_Type (Prefix (Expression (Expr))))));
            end if;

            --  We have to explicitly change the top-of-stack type to
            --  match the target of the unchecked conversion.

            Pop_Type;
            Push_Type (JVM_Expr_Type (Expr));

         when N_Aggregate | N_Extension_Aggregate =>
            Evaluate_Aggregate (Expr);

         when N_Allocator =>
            Evaluate_Allocator (Expr);

         when N_Raise_xxx_Error =>
            Translate_Predefined_Raise (Expr);

            --  Since this is an expression evaluation, the context will
            --  require a result on the stack, so we must push a dummy
            --  value compatible with the expected type to ensure stack
            --  consistency. This will only occur in cases of programs
            --  which have unconditional errors.

            case JVM.Type_Kind (JVM_Type (Expr)) is
               when Boolean_Kind | Byte_Kind | Char_Kind
                  | Short_Kind   | Int_Kind  =>
                  Gen_Push_Int (Uint_0);

               when Long_Kind =>
                  Gen_Push_Long (Uint_0);

               when JVM.Float_Kind =>
                  Gen_Push_Float (Ureal_0);

               when Double_Kind =>
                  Gen_Push_Double (Ureal_0);

               when JVM.Array_Kind | Class_Kind =>
                  Gen_Push_Null;

               when others =>
                  pragma Assert (False);
                  raise Program_Error;
            end case;

         when others =>
            if Debug_Flag_JJ then
               Osint.Fail
                 ("*** Unsupported expression node: " &
                  Node_Kind'Image (Nkind (Expr)));
            else
               pragma Assert (False);
               raise Program_Error;
            end if;
      end case;

      Debug_A_Exit ("(Ada-to-JVM) ", Expr, " (done)");
   end Evaluate_Expr;

   ------------------------------
   -- Evaluate_Integer_Literal --
   ------------------------------

   function Evaluate_Integer_Literal
     (Literal : Node_Id;
      Typ     : Entity_Id) return Uint
   is
      J_Type : constant Type_Id := JVM_Type (Typ);
      Value  : Uint;

   begin
      if Ekind (Typ) in Fixed_Point_Kind then
         Value := Corresponding_Integer_Value (Literal);
      else
         Value := Intval (Literal);
      end if;

      if JVM.Type_Kind (J_Type) in Byte_Kind .. Int_Kind then

         --  If the integer value is greater than Integer'Last, then the
         --  literal must be of a modular type, so normalize the value to the
         --  equivalent signed (negative) value.

         if Ekind (Typ) in Modular_Integer_Kind
           and then Value >= 2 ** (Uint_32 - 1)
         then
            return Value - (2 ** Uint_32);
         else
            return Value;
         end if;

      elsif J_Type = Long_Type or else J_Type = ULong_Type then

         --  If the integer value is greater than Long_Integer'Last, then the
         --  literal must be of a modular type, so normalize the value to the
         --  equivalent signed (negative) value.

         if Ekind (Typ) in Modular_Integer_Kind
           and then Value >= 2 ** (Uint_64 - 1)
         then
            return Value - (2 ** Uint_64);
         else
            return Value;
         end if;

      --  This literal may originate from the Null_Address value for type
      --  System.Address, which should only occur inside the declarative region
      --  of package System.

      elsif J_Type = Type_Of (Java_Lang_Object) then
         pragma Assert (Intval (Literal) = Uint_0);
         return Uint_0;

      else
         pragma Assert (False);
         raise Program_Error;
      end if;
   end Evaluate_Integer_Literal;

   ------------------------------
   -- Evaluate_Integer_Literal --
   ------------------------------

   procedure Evaluate_Integer_Literal (Literal : Node_Id; Typ : Entity_Id) is
      J_Type : constant Type_Id := JVM_Type (Typ);
      Value  : constant Uint := Evaluate_Integer_Literal (Literal, Typ);

   begin
      if JVM.Type_Kind (J_Type) in Byte_Kind .. Int_Kind then
         Gen_Push_Int (Value);

         --  If the integer value is greater than Integer'Last, then the
         --  literal must be of a modular type, so normalize the value to the
         --  equivalent signed (negative) value.

         if Intval (Literal) >= 2 ** (Uint_32 - 1) then
            Gen_Conversion (JVM_Type (Typ));
         end if;

      elsif J_Type = Long_Type or else J_Type = ULong_Type then
         Gen_Push_Long (Value);

      --  This literal may originate from the Null_Address value for type
      --  System.Address, which should only occur inside the declarative region
      --  of package System.

      elsif J_Type = Type_Of (Java_Lang_Object) then
         Gen_Push_Null;

      else
         pragma Assert (False);
         raise Program_Error;
      end if;
   end Evaluate_Integer_Literal;

   ------------------------------
   -- Evaluate_Membership_Test --
   ------------------------------

   procedure Evaluate_Membership_Test
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean)
   is
      Test_Expr   : constant Node_Id   := Left_Opnd (Expr);
      Test_Type   : constant Entity_Id := Full_Type (Test_Expr);
      Member_Type : constant Entity_Id := Full_Type (Right_Opnd (Expr));
      Test_LV     : constant Local_Var_Id :=
                      New_Local_Var ("_test_tmp", JVM_Type (Test_Type));

      False_Label : Label_Id;
      True_Label  : Label_Id;
      Low         : Node_Id;
      High        : Node_Id;
      Check_State : Boolean;
      Test_Range  : Node_Id;
      Test_Kind   : Node_Kind;

   begin
      if Ekind (Test_Type) in Scalar_Kind then
         Test_Range := Right_Opnd (Expr);

         case Nkind (Right_Opnd (Expr)) is
            when N_Range =>
               Low  := Low_Bound (Test_Range);
               High := High_Bound (Test_Range);

            when N_Identifier | N_Expanded_Name =>
               Low  := Low_Bound (Scalar_Range (Entity (Test_Range)));
               High := High_Bound (Scalar_Range (Entity (Test_Range)));

            when N_Subtype_Indication =>
               Low  := Low_Bound (Scalar_Range (Full_Type (Test_Range)));
               High := High_Bound (Scalar_Range (Full_Type (Test_Range)));

            when others =>
               pragma Assert (False);
               raise Program_Error;
         end case;

         --  Evaluate and save away the test expression (but duplicate it so
         --  it's on the stack).

         Evaluate_Expr (Test_Expr);
         Gen_Store_Local (Test_LV);

         Suppress_Stack_Checking (Check_State);

         if Label /= Null_Label then

            --  If not True_Branch, then invert the Expr Node_Kind test.
            if True_Branch then
               Test_Kind := Nkind (Expr);
            else
               case Nkind (Expr) is
                  when N_In =>
                     Test_Kind := N_Not_In;
                  when N_Not_In =>
                     Test_Kind := N_In;
                  when others =>
                     pragma Assert (False);
                     raise Program_Error;
               end case;
            end if;

            --  Evaluate the low bound of the range and compare against the
            --  test expression.

            Evaluate_Expr (Low);
            Gen_Load_Local (Test_LV);

            if Test_Kind = N_In then
               False_Label := New_Label;
               --  False label if Low > Expr
               --  If Test_LV is NaN, this will also evaluate as False
               Gen_Compare_Branch_Greater (False_Label, Unordered => True);
            else  --  N_Not_In
               --  OK if Low > Expr (Expr not in range)
               --  If Test_LV is NaN, this will also go to Label
               Gen_Compare_Branch_Greater (Label, Unordered => True);
            end if;

            --  Evaluate the high bound of the range and compare against
            --  the test expression.

            Gen_Load_Local (Test_LV);
            Evaluate_Expr (High);

            if Test_Kind = N_In then
               --  OK if Expr <= High
               --  Case where Expr is NaN is already resolved by the first test
               Gen_Compare_Branch_Less_Equal (Label);
               Gen_Label (False_Label);
            else  --  Nkind (Expr) = N_Not_In
               --  OK if Expr > High
               --  Case where Expr is NaN is already resolved by the first test
               Gen_Compare_Branch_Greater (Label);
            end if;

            Label := Null_Label;

         --  Label = Null_Label, so compute a Boolean result on the stack

         else
            False_Label := New_Label;
            True_Label  := New_Label;

            --  Compare Test_Expr vs. Low

            Evaluate_Expr (Low);
            Gen_Load_Local (Test_LV);

            --  If Low > Expr, branch to False_Label
            --  This will also branch to False_Label if Expr is NaN

            Gen_Compare_Branch_Greater (False_Label, Unordered => True);

            --  Compare Test_Expr vs. High

            Gen_Load_Local (Test_LV);
            Evaluate_Expr (High);
            --  Expr > High ? False_Label
            Gen_Compare_Branch_Greater (False_Label);

            if Nkind (Expr) = N_In then
               Gen_Push_Int (Uint_1);
            else  --  Nkind (Expr) = N_Not_In
               Gen_Push_Int (Uint_0);
            end if;

            Gen_Goto (True_Label);
            Gen_Label (False_Label);
            Mark_Stack;

            if Nkind (Expr) = N_In then
               Gen_Push_Int (Uint_0);
            else  --  Nkind (Expr) = N_Not_In
               Gen_Push_Int (Uint_1);
            end if;

            Release_Stack;
            Gen_Label (True_Label);
         end if;

         Restore_Stack_Checking (Check_State);

      elsif Is_Tagged_Type (Test_Type) then
         if Is_Class_Wide_Type (Member_Type) then
            Evaluate_Expr (Test_Expr);
            Gen_Instance_Of (JVM_Class (Member_Type));

            if Label /= Null_Label then
               if True_Branch then
                  Gen_Branch_Not_Equal (Label);
               else
                  Gen_Branch_Equal (Label);
               end if;
            end if;

            Label := Null_Label;

         --  Generate a comparison of the class of the object against the class
         --  of the membership type.

         else
            --  First get the reference for the class associated with Test_Expr
            --  (but if the Test_Expr is statically tagged it's not always
            --  correct to call getClass, e.g., this won't work for the case
            --  of formal parameters ???).

            Evaluate_Expr (Test_Expr);
            Gen_Invoke_API_Method (Object_getClass);

            --  Then push the reference for the class of the membership type

            Gen_Push_String_Const
              (Name_String (Name (JVM_Class (Member_Type))));
            Gen_Invoke_API_Method (Class_forName);

            --  Now check if the class references are equal

            if Label /= Null_Label then
               if True_Branch then
                  Gen_Compare_Branch_Equal (Label);
               else
                  Gen_Compare_Branch_Not_Equal (Label);
               end if;

               Label := Null_Label;

            else
               False_Label := New_Label;
               True_Label  := New_Label;

               Suppress_Stack_Checking (Check_State);

               Gen_Compare_Branch_Equal (True_Label);
               Gen_Push_Int (Uint_0);
               Gen_Goto (False_Label);

               Gen_Label (True_Label);
               Mark_Stack;
               Gen_Push_Int (Uint_1);
               Release_Stack;

               Gen_Label (False_Label);

               Restore_Stack_Checking (Check_State);
            end if;
         end if;

      --  All other cases should be handled by the front end (statically or by
      --  expansion).

      else
         pragma Assert (False);
         raise Program_Error;
      end if;
   end Evaluate_Membership_Test;

   -----------------------
   -- Evaluate_Operator --
   -----------------------

   procedure Evaluate_Operator
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean)
   is
      Opnd_Kind   : constant Entity_Kind :=
                      Ekind (Full_Type (Right_Opnd (Expr)));
      Check_State : Boolean;
      Do_Round    : Boolean;
      Exp_Jtype   : Type_Id;
      Exp_Kind    : Node_Kind := Nkind (Expr);

      procedure Check_For_Unsigned_Comparison;
      --  Checks if the operands are full-width modular values and if so then
      --  generates a test and swaps the operands if they differ in sign (which
      --  requires a reversal of the comparison).

      function Invalid_Address_Comparison return Boolean;
      --  Checks if this is an operator applied to System.Address operands, and
      --  in that case it emits a warning and pushes a False result on the
      --  stack after popping the operands. Only intended to be called in the
      --  case of relational operators that are not supported for object
      --  references on the JVM (>, <, >=, <=). Returns True if the comparison
      --  is invalid, otherwise returns False.

      procedure JVM_Long_Int_To_Modular;
      --  In the JVM backend, emit code which converts the positive value of
      --  type long integer available at the top of the stack (value associated
      --  with a modular type) into the corresponding signed integer value.

      procedure JVM_Modular_To_Long_Int;
      --  In the JVM backend, emit code which converts the signed integer value
      --  available at the top of the stack (value associated with a modular
      --  type) into the corresponding positive value of type long integer.

      procedure Normalize_Modular;
      --  If the type of the result is a modular integer value with type's
      --  modulus < 2**Uint_32 then normalize the result by computing its value
      --  modulo the type's modulus.

      -----------------------------------
      -- Check_For_Unsigned_Comparison --
      -----------------------------------

      procedure Check_For_Unsigned_Comparison is
         No_Swap_Label : constant Label_Id := New_Label;
         Check_State   : Boolean;

      begin
         if Opnd_Kind in Modular_Integer_Kind
           and then Modulus (Full_Type (Right_Opnd (Expr))) >= 2 ** Uint_32
         then
            Suppress_Stack_Checking (Check_State);

            Gen_Double_Duplicate;
            Gen_Xor;
            Gen_Branch_Greater_Equal (No_Swap_Label);
            Gen_Swap;
            Gen_Label (No_Swap_Label);

            Restore_Stack_Checking (Check_State);
         end if;
      end Check_For_Unsigned_Comparison;

      --------------------------------
      -- Invalid_Address_Comparison --
      --------------------------------

      function Invalid_Address_Comparison return Boolean is
      begin
         --  Address comparisons other than "=" and "/=" do not make sense on
         --  the JVM, so issue a warning, and generate a false result anyway to
         --  allow compilation to continue.

         if JVM_Type (Left_Opnd (Expr)) = Type_Of (Java_Lang_Object) then
            Error_Msg_N ("unsupported form of address comparison", Expr);
            Gen_Pop (2);
            Gen_Push_Int (Uint_0);

            return True;

         else
            return False;
         end if;
      end Invalid_Address_Comparison;

      -----------------------------
      -- JVM_Long_Int_To_Modular --
      -----------------------------

      procedure JVM_Long_Int_To_Modular is
         Label       : constant Label_Id := New_Label;
         Check_State : Boolean;

      begin
         Suppress_Stack_Checking (Check_State);

         Gen_Duplicate;
         Gen_Push_Long (Modulus (Full_Type (Expr)) / 2);
         Gen_Compare_Branch_Less (Label);
         Gen_Push_Long (Modulus (Full_Type (Expr)));
         Gen_Sub (Modular => False, Integer_Type => True);
         Gen_Label (Label);

         Restore_Stack_Checking (Check_State);

         Gen_Conversion (Int_Type);
      end JVM_Long_Int_To_Modular;

      -----------------------------
      -- JVM_Modular_To_Long_Int --
      -----------------------------

      procedure JVM_Modular_To_Long_Int is
      begin
         pragma Assert (VM_Target = JVM_Target
           and then Is_Modular_Integer_Type (Full_Type (Expr)));

         Gen_Conversion (Long_Type);
         Gen_Push_Long (Modulus (Full_Type (Expr)));
         Gen_Add (Modular => False, Integer_Type => True);
         Gen_Push_Long (Modulus (Full_Type (Expr)));
         Gen_Rem;
      end JVM_Modular_To_Long_Int;

      -----------------------
      -- Normalize_Modular --
      -----------------------

      procedure Normalize_Modular is
      begin
         if Opnd_Kind in Modular_Integer_Kind
           and then Modulus (Full_Type (Expr)) < 2 ** Uint_32
         then
            Gen_Push_Int (Modulus (Full_Type (Expr)));
            Gen_Rem;
         end if;
      end Normalize_Modular;

   --  Start of processing for Evaluate_Operator

   begin
      if Exp_Kind = N_Op_Divide
        and then Rounded_Result (Expr)
      then
         Do_Round := True;
      else
         Do_Round := False;
      end if;

      if Exp_Kind in N_Binary_Op then
         if Opnd_Kind in Einfo.Array_Kind then

            if Nkind (Left_Opnd (Expr)) = N_Slice then
               Evaluate_Expr (Left_Opnd (Expr));
            else
               Evaluate_Array_Address (Left_Opnd (Expr));
            end if;
         else
            Evaluate_Expr (Left_Opnd (Expr));

            --  In the JVM target we implement modular types using signed
            --  integers. In order to handle arithmetic operations in
            --  modular types whose range covers all the values of signed
            --  integers we convert the signed integer value in the top
            --  of the stack into a positive value of type long integer.

            if VM_Target = JVM_Target
              and then Is_Modular_Integer_Type (Full_Type (Expr))
              and then Nkind_In (Expr,
                         N_Op_Add,
                         N_Op_Subtract,
                         N_Op_Multiply,
                         N_Op_Divide)
              and then Modulus (Full_Type (Expr)) = 2 ** Uint_32
            then
               JVM_Modular_To_Long_Int;
            end if;
         end if;
      end if;

      --  In case of rounded integer divisions, we need to convert the operands
      --  to double. Once the division is performed and rounding is done, then
      --  we will convert the result back to the original type.

      if Do_Round then
         Gen_Conversion (Double_Type);
      end if;

      --  We handle "not" specially, see below.

      if Exp_Kind /= N_Op_Not then
         if Opnd_Kind in Einfo.Array_Kind then
            Evaluate_Array_Address (Right_Opnd (Expr));
         else
            Evaluate_Expr (Right_Opnd (Expr));

            --  In the JVM target we implement modular types using signed
            --  integers. In order to handle arithmetic operations in
            --  modular types whose range covers all the values of signed
            --  integers we convert the signed integer value in the top
            --  of the stack into a positive value of type long integer.

            if VM_Target = JVM_Target
              and then Is_Modular_Integer_Type (Full_Type (Expr))
              and then Nkind_In (Expr,
                         N_Op_Add,
                         N_Op_Subtract,
                         N_Op_Multiply,
                         N_Op_Divide)
              and then Modulus (Full_Type (Expr)) = 2 ** Uint_32
            then
               JVM_Modular_To_Long_Int;
            end if;
         end if;
      end if;

      if Do_Round then
         Gen_Conversion (Double_Type);
      end if;

      --  If this is a "/=" operator expanded to 'not "="', then we handle this
      --  specially, treating it directly as if it were N_Op_Ne.

      if Exp_Kind = N_Op_Eq
        and then Nkind (Parent (Expr)) = N_Op_Not
      then
         Exp_Kind := N_Op_Ne;
      end if;

      case Exp_Kind is
         when N_Op_Abs =>
            declare
               Non_Neg_Lbl : constant Label_Id := New_Label;

            begin
               Gen_Duplicate;
               Suppress_Stack_Checking (Check_State);
               Gen_Branch_Greater_Equal (Non_Neg_Lbl);
               Gen_Neg;
               Gen_Label (Non_Neg_Lbl);
               Restore_Stack_Checking (Check_State);
            end;

         when N_Op_Add =>

            --  Test if operand is Discrete or fixed point type to generate
            --  the overflow check

            Gen_Add
              (Modular        => Opnd_Kind in Modular_Integer_Kind,
               Integer_Type   => Opnd_Kind in Discrete_Or_Fixed_Point_Kind,
               Overflow_Check => Do_Overflow_Check (Expr));

            if Is_Modular_Integer_Type (Full_Type (Expr)) then
               if VM_Target = JVM_Target
                 and then Modulus (Full_Type (Expr)) = 2 ** Uint_32
               then
                  JVM_Long_Int_To_Modular;
               else
                  Normalize_Modular;
               end if;
            end if;

         when N_Op_And =>
            Gen_And;

         when N_Op_Divide =>

            --  Let's generate a rounded division if needed

            Gen_Div;

            if VM_Target = JVM_Target
              and then Is_Modular_Integer_Type (Etype (Expr))
              and then Modulus (Full_Type (Expr)) = 2 ** Uint_32
            then
               JVM_Long_Int_To_Modular;
            end if;

            if Rounded_Result (Expr) then

               --  We need to convert the result back to the original type

               Gen_Conversion (JVM_Expr_Type (Expr), Round => True);
            end if;

         when N_Op_Eq =>
            if Opnd_Kind in Einfo.Array_Kind then
               Exp_Jtype := JVM_Type (Full_Type (Right_Opnd (Expr)));
               Evaluate_Array_Comparison
                 (N_Op_Eq, Left_Opnd (Expr), Right_Opnd (Expr), Exp_Jtype);

            elsif Opnd_Kind = E_Access_Protected_Subprogram_Type then
               Evaluate_Acc_Prot_Subp_Comparison
                 (N_Op_Eq, Full_Type (Right_Opnd (Expr)), Label, True_Branch);

            else
               if Label /= Null_Label then
                  begin
                     if True_Branch then
                        Gen_Compare_Branch_Equal (Label);
                     else
                        Gen_Compare_Branch_Not_Equal (Label);
                     end if;

                  exception
                     when Incompatible_Types =>
                        --  ??? Need better error recovery here, since the
                        --  stack is probably inconsistent at this stage

                        Error_Msg_N
                          ("unsupported form of address comparison", Expr);
                  end;

                  Label := Null_Label;
               else
                  declare
                     Lbl_T : constant Label_Id := New_Label;
                     Lbl_F : constant Label_Id := New_Label;

                  begin
                     Suppress_Stack_Checking (Check_State);

                     Gen_Compare_Branch_Equal (Lbl_T);
                     Gen_Push_Int (Uint_0);
                     Gen_Goto (Lbl_F);

                     Gen_Label (Lbl_T);
                     Mark_Stack;
                     Gen_Push_Int (Uint_1);
                     Release_Stack;

                     Gen_Label (Lbl_F);

                     Restore_Stack_Checking (Check_State);
                  end;
               end if;
            end if;

         when N_Op_Ge =>
            if Invalid_Address_Comparison then
               return;
            end if;

            Check_For_Unsigned_Comparison;

            if Label /= Null_Label then
               if True_Branch then
                  Gen_Compare_Branch_Greater_Equal (Label);
               else
                  Gen_Compare_Branch_Less (Label, True);
               end if;

               Label := Null_Label;
            else
               declare
                  Lbl_T : constant Label_Id := New_Label;
                  Lbl_F : constant Label_Id := New_Label;

               begin
                  Suppress_Stack_Checking (Check_State);

                  Gen_Compare_Branch_Greater_Equal (Lbl_T);
                  Gen_Push_Int (Uint_0);
                  Gen_Goto (Lbl_F);

                  Gen_Label (Lbl_T);
                  Mark_Stack;
                  Gen_Push_Int (Uint_1);
                  Release_Stack;

                  Gen_Label (Lbl_F);

                  Restore_Stack_Checking (Check_State);
               end;
            end if;

         when N_Op_Gt =>
            if Invalid_Address_Comparison then
               return;
            end if;

            Check_For_Unsigned_Comparison;

            if Label /= Null_Label then
               if True_Branch then
                  Gen_Compare_Branch_Greater (Label);
               else
                  Gen_Compare_Branch_Less_Equal (Label, True);
               end if;

               Label := Null_Label;
            else
               declare
                  Lbl_T : constant Label_Id := New_Label;
                  Lbl_F : constant Label_Id := New_Label;

               begin
                  Suppress_Stack_Checking (Check_State);

                  Gen_Compare_Branch_Greater (Lbl_T);
                  Gen_Push_Int (Uint_0);
                  Gen_Goto (Lbl_F);

                  Gen_Label (Lbl_T);
                  Mark_Stack;
                  Gen_Push_Int (Uint_1);
                  Release_Stack;

                  Gen_Label (Lbl_F);

                  Restore_Stack_Checking (Check_State);
               end;
            end if;

         when N_Op_Le =>
            if Invalid_Address_Comparison then
               return;
            end if;

            Check_For_Unsigned_Comparison;

            if Label /= Null_Label then
               if True_Branch then
                  Gen_Compare_Branch_Less_Equal (Label);
               else
                  Gen_Compare_Branch_Greater (Label, True);
               end if;

               Label := Null_Label;
            else
               declare
                  Lbl_T : constant Label_Id := New_Label;
                  Lbl_F : constant Label_Id := New_Label;

               begin
                  Suppress_Stack_Checking (Check_State);

                  Gen_Compare_Branch_Less_Equal (Lbl_T);
                  Gen_Push_Int (Uint_0);
                  Gen_Goto (Lbl_F);

                  Gen_Label (Lbl_T);
                  Mark_Stack;
                  Gen_Push_Int (Uint_1);
                  Release_Stack;

                  Gen_Label (Lbl_F);

                  Restore_Stack_Checking (Check_State);
               end;
            end if;

         when N_Op_Lt =>
            if Invalid_Address_Comparison then
               return;
            end if;

            Check_For_Unsigned_Comparison;

            if Label /= Null_Label then
               if True_Branch then
                  Gen_Compare_Branch_Less (Label);
               else
                  Gen_Compare_Branch_Greater_Equal (Label, True);
               end if;

               Label := Null_Label;
            else
               declare
                  Lbl_T : constant Label_Id := New_Label;
                  Lbl_F : constant Label_Id := New_Label;

               begin
                  Suppress_Stack_Checking (Check_State);

                  Gen_Compare_Branch_Less (Lbl_T);
                  Gen_Push_Int (Uint_0);
                  Gen_Goto (Lbl_F);

                  Gen_Label (Lbl_T);
                  Mark_Stack;
                  Gen_Push_Int (Uint_1);
                  Release_Stack;

                  Gen_Label (Lbl_F);

                  Restore_Stack_Checking (Check_State);
               end;
            end if;

         when N_Op_Ne =>
            if Opnd_Kind in Einfo.Array_Kind then
               Exp_Jtype := JVM_Type (Full_Type (Right_Opnd (Expr)));
               Evaluate_Array_Comparison
                 (N_Op_Ne, Left_Opnd (Expr), Right_Opnd (Expr), Exp_Jtype);

            elsif Opnd_Kind = E_Access_Protected_Subprogram_Type then
               Evaluate_Acc_Prot_Subp_Comparison
                 (N_Op_Ne, Full_Type (Right_Opnd (Expr)), Label, True_Branch);

            else
               if Label /= Null_Label then
                  begin
                     if True_Branch then
                        Gen_Compare_Branch_Not_Equal (Label);
                     else
                        Gen_Compare_Branch_Equal (Label);
                     end if;

                  exception
                     when Incompatible_Types =>
                        --  ??? Need better error recovery here, since the
                        --  stack is probably inconsistent at this stage

                        Error_Msg_N
                          ("unsupported form of address comparison", Expr);
                  end;

                  Label := Null_Label;

               else
                  declare
                     Lbl_T : constant Label_Id := New_Label;
                     Lbl_F : constant Label_Id := New_Label;

                  begin
                     Suppress_Stack_Checking (Check_State);

                     Gen_Compare_Branch_Not_Equal (Lbl_T);
                     Gen_Push_Int (Uint_0);
                     Gen_Goto (Lbl_F);

                     Gen_Label (Lbl_T);
                     Mark_Stack;
                     Gen_Push_Int (Uint_1);
                     Release_Stack;

                     Gen_Label (Lbl_F);

                     Restore_Stack_Checking (Check_State);
                  end;
               end if;
            end if;

         when N_Op_Mod =>

            --  No need to handle negative arguments in modular types

            if Is_Modular_Integer_Type (Full_Type (Etype (Expr))) then
               Gen_Rem;

            else
               declare
                  Mod_Jtype   : constant Type_Id :=
                                  JVM_Type (Full_Type (Right_Opnd (Expr)));
                  Dividend_LV : constant Local_Var_Id :=
                                  New_Local_Var ("_mod_l", Mod_Jtype);
                  Divisor_LV  : constant Local_Var_Id :=
                                  New_Local_Var ("_mod_r", Mod_Jtype);
                  Normal_Lbl  : constant Label_Id := New_Label;
                  Exit_Lbl    : constant Label_Id := New_Label;
                  Check_State : Boolean;

               begin
                  Suppress_Stack_Checking (Check_State);

                  Gen_Store_Local (Divisor_LV);
                  Gen_Store_Local (Dividend_LV);

                  --  Check if both arguments are positive

                  Gen_Load_Local (Dividend_LV);
                  Gen_Load_Local (Divisor_LV);
                  Gen_Xor;
                  Gen_Branch_Greater_Equal (Normal_Lbl);

                  Gen_Load_Local (Dividend_LV);
                  Gen_Load_Local (Divisor_LV);
                  Gen_Rem;
                  Gen_Duplicate;
                  Gen_Branch_Equal (Exit_Lbl);

                  Gen_Load_Local (Divisor_LV);
                  Gen_Add (Modular => True, Integer_Type => True);

                  Gen_Goto (Exit_Lbl);

                  Gen_Label (Normal_Lbl);

                  Mark_Stack;
                  Gen_Load_Local (Dividend_LV);
                  Gen_Load_Local (Divisor_LV);
                  Gen_Rem;
                  Release_Stack;

                  Gen_Label (Exit_Lbl);

                  Restore_Stack_Checking (Check_State);
               end;
            end if;

         when N_Op_Multiply =>

            --  Test if operand is Discrete or fixed point type to generate the
            --  overflow check

            Gen_Mul (Opnd_Kind in Modular_Integer_Kind,
                     Opnd_Kind in Discrete_Or_Fixed_Point_Kind,
                     Overflow_Check => Do_Overflow_Check (Expr));

            if Is_Modular_Integer_Type (Full_Type (Expr)) then
               if VM_Target = JVM_Target
                 and then Modulus (Full_Type (Expr)) = 2 ** Uint_32
               then
                  JVM_Long_Int_To_Modular;
               else
                  Normalize_Modular;
               end if;
            end if;

         when N_Op_Or =>
            Gen_Or;
            Normalize_Modular;

         when N_Op_Rem =>
            Gen_Rem;

         when N_Op_Subtract =>

            --  Test if operand is Discrete or fixed point type to generate
            --  the overflow check

            Gen_Sub
             (Modular        => Opnd_Kind in Modular_Integer_Kind,
              Integer_Type   => Opnd_Kind in Discrete_Or_Fixed_Point_Kind,
              Overflow_Check => Do_Overflow_Check (Expr));

            if Is_Modular_Integer_Type (Full_Type (Expr)) then
               if VM_Target = JVM_Target
                 and then Modulus (Full_Type (Expr)) = 2 ** Uint_32
               then
                  JVM_Long_Int_To_Modular;

               --  If this is a modular integer subtraction, then normalize
               --  by first adding the modulus (to compensate for a possible
               --  negative difference) and then taking the result modulo the
               --  type's modulus.

               elsif Modulus (Full_Type (Expr)) < 2 ** Uint_32 then
                  Gen_Push_Int (Modulus (Full_Type (Expr)));
                  Gen_Add (Modular => True, Integer_Type => True);
                  Gen_Push_Int (Modulus (Full_Type (Expr)));
                  Gen_Rem;
               end if;
            end if;

         when N_Op_Xor =>
            Gen_Xor;
            Normalize_Modular;

         when N_Op_Minus =>

            --  If this is a modular integer negation, then subtract the
            --  operand from the modulus, except that -0 equals 0.

            if Opnd_Kind in Modular_Integer_Kind
              and then Modulus (Full_Type (Expr)) < 2 ** Uint_32
            then
               declare
                  Zero_Label  : constant Label_Id := New_Label;
                  Check_State : Boolean;

               begin
                  Suppress_Stack_Checking (Check_State);

                  Gen_Duplicate;
                  Gen_Branch_Equal (Zero_Label);
                  Gen_Push_Int (Modulus (Full_Type (Expr)));
                  Gen_Swap;
                  Gen_Sub (Modular => True, Integer_Type => True);
                  Gen_Label (Zero_Label);

                  Restore_Stack_Checking (Check_State);
               end;

            --  Otherwise simply negate the operand

            else
               Gen_Neg;
            end if;

         when N_Op_Not =>

            --  If this is a rewritten "/=" operator then we pass along any
            --  label and let the evaluation of the "=" detect that the parent
            --  is "not", so as to generate more efficient code.

            if Nkind (Right_Opnd (Expr)) = N_Op_Eq then
               Evaluate_Expr (Right_Opnd (Expr), Label, True_Branch);

            --  If the type is modular, then simply subtract from the upper
            --  bound of the base range, unless this is a full-length type,
            --  in which case we can simply apply a full-width complement.

            elsif Opnd_Kind in Modular_Integer_Kind then
               Evaluate_Expr (Right_Opnd (Expr));

               if Modulus (Full_Type (Expr)) >= 2 ** Uint_32 then
                  Gen_Not;

               else
                  Gen_Push_Int (Modulus (Base_Type (Full_Type (Expr))) - 1);
                  Gen_Swap;
                  Gen_Sub (Modular => True, Integer_Type => True);
               end if;

            else
               --  Note: We can't call Gen_Not here, because that will perform
               --  a full-word bit complement, which is not correct for results
               --  of type Standard.Boolean. Perhaps there should be an
               --  operation called Gen_Boolean_Not in JVM which would generate
               --  the XOR with 1. ???

               Evaluate_Expr (Right_Opnd (Expr));
               Gen_Push_Int (Uint_1);
               Gen_Xor;
            end if;

         when N_Op_Plus =>
            null;

         when N_Op_Shift_Left =>
            Gen_Shift_Left (RM_Size (Etype (Left_Opnd (Expr))));

         when N_Op_Shift_Right_Arithmetic =>
            Gen_Shift_Right_Arithmetic (Etype (Left_Opnd (Expr)));

         when N_Op_Shift_Right =>
            Gen_Shift_Right_Logical;

         when N_Op_Rotate_Left =>
            Gen_Rotate_Left (RM_Size (Etype (Left_Opnd (Expr))));

         when N_Op_Rotate_Right =>
            Gen_Rotate_Right (RM_Size (Etype (Left_Opnd (Expr))));

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Evaluate_Operator;

   ----------------------
   -- Evaluate_Or_Else --
   ----------------------

   procedure Evaluate_Or_Else
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean)
   is
   begin
      if Label /= Null_Label then
         if True_Branch then
            declare
               Save_Label : Label_Id := Label;

            begin
               Evaluate_Expr (Left_Opnd (Expr), Save_Label, True);

               if Save_Label /= Null_Label then
                  Gen_Branch_Not_Equal (Label);
               end if;

               Save_Label := Label;
               Evaluate_Expr (Right_Opnd (Expr), Save_Label, True);

               if Save_Label /= Null_Label then
                  Gen_Branch_Not_Equal (Label);
               end if;

               Label := Null_Label;
            end;
         else
            declare
               True_Label       : constant Label_Id := New_Label;
               Save_True_Label  : Label_Id := True_Label;
               Save_False_Label : Label_Id := Label;

            begin
               Evaluate_Expr (Left_Opnd (Expr), Save_True_Label, True);

               if Save_True_Label /= Null_Label then
                  Gen_Branch_Not_Equal (True_Label);
               end if;

               Evaluate_Expr (Right_Opnd (Expr), Save_False_Label, False);

               if Save_False_Label /= Null_Label then
                  Gen_Branch_Equal (Label);
               end if;

               Gen_Label (True_Label);
               Label := Null_Label;
            end;
         end if;

      --  If no branch label is available, we have to produce a Boolean
      --  result (e.g., for a statement like B0 := B1 or else B2).

      else
         declare
            True_Label      : constant Label_Id := New_Label;
            False_Label     : constant Label_Id := New_Label;
            Save_True_Label : Label_Id := True_Label;
            Check_State     : Boolean;

         begin
            Suppress_Stack_Checking (Check_State);
            Evaluate_Expr (Left_Opnd (Expr), Save_True_Label, True);

            if Save_True_Label /= Null_Label then
               Gen_Branch_Not_Equal (True_Label);
            end if;

            Save_True_Label := True_Label;
            Evaluate_Expr (Right_Opnd (Expr), Save_True_Label, True);

            if Save_True_Label /= Null_Label then
               Gen_Branch_Not_Equal (True_Label);
            end if;

            Gen_Push_Int (Uint_0);
            Gen_Goto (False_Label);

            Gen_Label (True_Label);
            Mark_Stack;
            Gen_Push_Int (Uint_1);
            Release_Stack;

            Gen_Label (False_Label);
            Restore_Stack_Checking (Check_State);
         end;
      end if;
   end Evaluate_Or_Else;

   ---------------------------
   -- Evaluate_Real_Literal --
   ---------------------------

   procedure Evaluate_Real_Literal (Literal : Node_Id; Typ : Entity_Id) is
      J_Type : constant Type_Id := JVM_Type (Typ);

   begin
      if J_Type = Float_Type then
         Gen_Push_Float (Realval (Literal));

      elsif J_Type = Double_Type then
         Gen_Push_Double (Realval (Literal));

      --  The real literal is associated with a fixed-point type, in which case
      --  we push the literal's associated integer value.

      elsif J_Type = Int_Type then
         Gen_Push_Int (Corresponding_Integer_Value (Literal));

      elsif J_Type = Long_Type then
         Gen_Push_Long (Corresponding_Integer_Value (Literal));

      else
         pragma Assert (False);
         raise Program_Error;
      end if;
   end Evaluate_Real_Literal;

   -------------------------------
   -- Evaluate_Record_Aggregate --
   -------------------------------

   procedure Evaluate_Record_Aggregate
     (Aggr : Node_Id; Aggr_LV : Local_Var_Id := Null_Local_Var)
   is
      J_Type     : constant Type_Id := JVM_Type (Aggr);
      Aggr_Local : Local_Var_Id     := Aggr_LV;
      Comp_Assn  : Node_Id;
      Selector   : Entity_Id;

   begin
      if Aggr_Local = Null_Local_Var then
         Aggr_Local := New_Local_Var ("_era_aggr", J_Type);

         Gen_Default_Object (Class_Of_Type (J_Type));
         Gen_Store_Local (Aggr_Local);
      end if;

      Comp_Assn := First (Component_Associations (Aggr));

      while Present (Comp_Assn) loop
         Selector := Entity (First (Choices (Comp_Assn)));

         --  If the association is for the parent part of a tagged aggregate,
         --  then recursively evaluate the subaggregate into the same aggregate
         --  object.

         if Chars (Selector) = Name_uParent then
            Evaluate_Record_Aggregate (Expression (Comp_Assn), Aggr_Local);

         else
            --  Load the reference to the aggregate followed by the value of
            --  the component expression.

            Gen_Load_Local (Aggr_Local);

            --  If the component is aliased, then we have to create a wrapper
            --  object and initialize it from the expression, leaving a
            --  reference to the wrapper on the stack.

            if Needs_Access_Descriptor (Selector) then
               Gen_Default_Object (Class_Of_Type (Descriptor_Type (Selector)));
               Gen_Duplicate;
               Evaluate_Expr (Expression (Comp_Assn));
               pragma Assert (Is_Descriptor (Top_Type (Disp => 1)));
               Gen_Put_Object_Field (Descriptor_Field (Selector));

            else
               Evaluate_With_Copy (Expression (Comp_Assn));
            end if;

            --  Store the result of the component association into the
            --  associated field of the aggregate object.

            Gen_Put_Field (JVM_Field (Selector));
         end if;

         Comp_Assn := Next (Comp_Assn);
      end loop;

      --  If Aggr_LV is non-null then the caller must take care of loading the
      --  aggregate reference (e.g., for the case of the evaluation of a tagged
      --  parent aggregate, the evaluation of the enclosing aggregate will take
      --  care of it).

      if Aggr_LV = Null_Local_Var then
         Gen_Load_Local (Aggr_Local);
      end if;
   end Evaluate_Record_Aggregate;

   -----------------------------
   -- Evaluate_String_Literal --
   -----------------------------

   procedure Evaluate_String_Literal
     (Str_Lit  : String_Id;
      Str_Type : Entity_Id)
   is
      Lit_Length : constant Int := String_Length (Str_Lit);

   begin
      --  Allocate the array object of the appropriate type

      Gen_Push_Int (UI_From_Int (Lit_Length));
      Gen_New_Array (Str_Type);

      for Index in 0 .. Lit_Length - 1 loop
         --  We need to initialize the string character by character as
         --  using string literal might cause issues with non-ASCII
         --  characters.
         --  Note that this is faster anyway, as we directly do here what
         --  was done in GNAT_libc.get_bytes, without the overhead of using
         --  .NET's strings.
         Gen_Duplicate;
         Gen_Push_Int (UI_From_Int (Index));

         --  Str_Type can have aliased components when defined as
         --  interfaces.c.char_array for example. In this case, we need to
         --  allocate the descriptor.
         if Has_Aliased_Components (Str_Type) then
            Gen_Default_Object
              (Class_Of_Type (Descriptor_Type (Component_Type (Str_Type))));
            Gen_Duplicate;
         end if;

         Gen_Push_Int (UI_From_CC (Get_String_Char (Str_Lit, Index + 1)));

         if Has_Aliased_Components (Str_Type) then
            Gen_Put_Object_Field
              (Descriptor_Field (Component_Type (Str_Type)));
         end if;

         Gen_Store_Array_Element;
      end loop;
   end Evaluate_String_Literal;

   --------------------------------
   -- Evaluate_Subprogram_Access --
   --------------------------------

   Counter : Natural := 0;

   procedure Evaluate_Subprogram_Access
     (Subp_Name     : Node_Id;
      Subp_Acc_Type : Entity_Id)
   is
      Subp_Entity    : constant Entity_Id := Entity (Subp_Name);
      Subp_Acc_Class : constant Class_Id  := JVM_Class (Subp_Acc_Type);

      function Fixup (Name : String) return String;
      --  Replaces '-' with '_' in a filename as well as replacing '.' with '_'

      function Build_Class_Name return String;
      --  Build a class name suitable for VM_Target based on Sub_Acc_Class

      -----------
      -- Fixup --
      -----------

      function Fixup (Name : String) return String is
         Result : String := Name;
      begin
         for J in Result'Range loop
            case Result (J) is
               when '-' | '.' => Result (J) := '_';
               when others    => null;
            end case;
         end loop;

         return Result;
      end Fixup;

      ----------------------
      -- Build_Class_Name --
      ----------------------

      function Build_Class_Name return String is
      begin
         case VM_Target is
            when JVM_Target =>
               return Name_String (Name (Subp_Acc_Class))
                 & "$" & JVM_Expanded_Name (Subp_Entity);

            when CLI_Target =>
               return Fixup (Name_String (Name (Subp_Acc_Class))
                             & "." & JVM_Expanded_Name (Subp_Entity) & "."
                             & Name_String (Source_Name (Sloc (Subp_Name)))
                             & Strip (Natural'Image (Counter)));

            when No_VM =>
               pragma Assert (False);
               raise Program_Error;
         end case;
      end Build_Class_Name;

      --  Local variables

      Subp_Profile   : constant Entity_Id :=
                         Directly_Designated_Type (Subp_Acc_Type);
      Class_Name     : constant String := Build_Class_Name;
      Acc_Attr_Class : constant Class_Id :=
                         New_Class
                           (Ada_Ent  => Empty,
                            Name     => Name (Class_Name),
                            Pkg_Name => Package_Name (Subp_Name),
                            Src_Name => Source_Name (Sloc (Subp_Name)),
                            Super    => Subp_Acc_Class);
      Acc_Method     : Method_Id;
      Formal         : Entity_Id := First_Formal (Subp_Profile);
      Subp_Formal    : Entity_Id;
      Ctrl_Formal    : Entity_Id;
      Acc_Formal     : Local_Var_Id;
      Call_Formal    : Local_Var_Id;
      AR_Field       : Field_Id;
      Result_Typ     : Type_Id;

   --  Start of processing for Evaluate_Subprogram_Access

   begin
      Result_Typ := JVM_Expr_Type (Subp_Profile);

      if Is_Array_Type (Etype (Subp_Profile))
        and then not Is_Constrained (Etype (Subp_Profile))
        and then Convention (Subp_Profile) = Convention_Ada
      then
         pragma Assert (Descriptor_Type (Result_Typ) /= Null_Type);
         Result_Typ := Descriptor_Type (Result_Typ);
      end if;

      Acc_Method :=
        New_Method
          (Class  => Acc_Attr_Class,
           Name   => J_String.Name ("Invoke"),
           Result => Result_Typ,
           Static => False);

      --  ??? Was having a problem where the same subprogram if 'access was
      --  used twice then was getting two identical classes. It would probably
      --  be ideal to just combine these into one, but it is easier to just
      --  uniquely name them

      Counter := Counter + 1;

      --  Create the formal parameters for the abstract invocation method
      --  associated with the access-to-subprogram type's class.

      while Present (Formal) loop
         Acc_Formal := New_Method_Parameter
                         (Acc_Method, Chars (Formal), JVM_Type (Formal));

         if Ekind (Etype (Formal)) in Einfo.Array_Kind
           and then not Is_Constrained (Etype (Formal))
           and then not Is_Imported (Subp_Acc_Type)
         then
            Generate_Array_Bounds_Formals (Formal, Acc_Method);
         end if;

         Formal := Next_Formal_With_Extras (Formal);
      end loop;

      --  Generate the access attribute's class (a subclass of the class
      --  associated with attribute's access-to-subprogram type).

      Class_Stack.Push (Acc_Attr_Class);
      Begin_Class_File (Acc_Attr_Class);

      --  Generate trivial methods for <clinit> and <init>

      Generate_Class_Init_Method   (Acc_Attr_Class);
      Generate_Default_Constructor (Acc_Attr_Class);

      --  For now generate a trivial code body for the method
      --  (which should really be abstract). ???

      Open_Method (Acc_Method);
      Set_Current_Method (Acc_Method);
      Method_Stack.Push (Acc_Method);

      --  Retrieve the first parameter to pass to the accessed method's
      --  corresponding parameter as well as the first Ada formal of
      --  the subprogram.

      Acc_Formal  := Next_Local_Var (This_Local (Acc_Method));
      Subp_Formal := First_Formal (Subp_Entity);
      Call_Formal := First_Local_Var (JVM_Method (Subp_Entity));

      --  For dispatching operations, retrieve the first controlling
      --  formal, if any, with the method's 'this' argument.

      if Is_Dispatching_Operation (Subp_Entity) then
         while Present (Subp_Formal)
           and then not Is_Controlling_Formal (Subp_Formal)
         loop
            Acc_Formal  := Next_Local_Var (Acc_Formal);
            Next_Formal (Subp_Formal);
         end loop;

         --  If the method has any controlling formals then the first of
         --  these is the method's 'this' argument. Load the corresponding
         --  formal parameter of the enclosing Invoke method to pass to
         --  as the 'this' argument of the subprogram to call indirectly.

         if Present (Subp_Formal) then
            Ctrl_Formal := Subp_Formal;
            Gen_Load_Local (Acc_Formal);

            if JVM.Type_Kind (Type_Of (Acc_Formal)) = Class_Kind then
               --  Needed for peverify
               Gen_Conversion (Type_Of (JVM_Local_Var (Subp_Formal)));
            end if;

         --  If there is no controlling formal available, then this must
         --  be a function with a controlling result. Create an object of
         --  the specific tagged type for which the subprogram is a
         --  primitive and pass the new object as the call's 'this'
         --  argument to produce nondispatching semantics.

         else
            pragma Assert (Base_Type (Etype (Subp_Entity))
                            = Find_Dispatching_Type (Subp_Entity));

            Gen_Default_Object
              (JVM_Class (Find_Dispatching_Type (Subp_Entity)));
            Call_Formal := Next_Local_Var (Call_Formal);
         end if;

         --  Reset to the first formal parameters of both the wrapper
         --  and the subprogram to call.

         Acc_Formal  := Next_Local_Var (This_Local (Acc_Method));
         Subp_Formal := First_Formal (Subp_Entity);
      end if;

      --  Load all (non-this) formals of the containing method as actual
      --  parameters to pass to the invoked method.

      while Acc_Formal /= Null_Local_Var loop
         --  Skip the load of the wrapper's formal if it corresponds to
         --  the controlling formal of the subprogram to call, since it
         --  was already loaded as the 'this' parameter above.

         if not Present (Ctrl_Formal) or else Subp_Formal /= Ctrl_Formal then
            Gen_Load_Local (Acc_Formal);

            --  In certain cases involving front-end generated unchecked
            --  conversions of Subp'Address (e.g., for tasking), the formals
            --  of the subprogram access type may not match the types of
            --  accessed subprogram. In that case a downcast will be required
            --  in order to satisfy the verifier (note that the call to
            --  Gen_Conversion will only emit a checkcast if it's actually
            --  needed).

            if JVM.Type_Kind (Type_Of (Acc_Formal)) = Class_Kind then
               Gen_Conversion (Type_Of (JVM_Local_Var (Subp_Formal)));
            end if;
         end if;

         if Ekind (Etype (Subp_Formal)) in Einfo.Array_Kind
           and then not Is_Constrained (Etype (Subp_Formal))
         then
            for Dimension in 1 .. Number_Dimensions (Etype (Subp_Formal)) loop
               --  Step to lower bound formal and generate a load of it

               Acc_Formal  := Next_Local_Var (Acc_Formal);
               Call_Formal := Next_Local_Var (Call_Formal);
               Gen_Load_Local (Acc_Formal);

               --  Step to upper bound formal and generate a load of it

               Acc_Formal  := Next_Local_Var (Acc_Formal);
               Call_Formal := Next_Local_Var (Call_Formal);
               Gen_Load_Local (Acc_Formal);
            end loop;
         end if;

         Next_Formal_With_Extras (Subp_Formal);
         Acc_Formal  := Next_Local_Var (Acc_Formal);
         Call_Formal := Next_Local_Var (Call_Formal);
      end loop;

      --  If this is a call to a nested method, then create an object field
      --  to hold the static link to the subprogram's parent and pass the
      --  static link field saved in the access-to-subprogram object.

      if Present (Enclosing_Subprogram (Subp_Entity)) then
         pragma Assert (Call_Formal /= Null_Local_Var);

         --  Use the type of the static link formal (denoted
         --  by the last formal of the subprogram) in creating
         --  the static link field.

         AR_Field :=
           New_Field
             (Class  => Acc_Attr_Class,
              Name   => Name ("_static_link"),
              Ftype  => Type_Of (Call_Formal),
              Static => False);

         Gen_Load_Local (This_Local (Acc_Method));
         Gen_Get_Field (AR_Field);
      end if;

      --  Generate a call to the method to which the access attribute applies

      Gen_Invoke_Method (JVM_Method (Subp_Entity));
      Gen_Method_Return;
      Method_Stack.Pop;
      Close_Method (Acc_Method);

      if not Method_Stack.Empty then
         Set_Current_Method (Method_Stack.Top);
      end if;

      Generate_Null_Methods (Acc_Attr_Class);
      End_Class_File (Acc_Attr_Class);
      Class_Stack.Pop;

      --  Finally, generate an object of the new subclass and leave its
      --  reference on the stack as the result of the access attribute.

      Gen_Default_Object (Acc_Attr_Class);

      --  If this is an access to a nested method, then save the reference to
      --  the activation record associated with the method's static parent
      --  in the static link field of the access-to-subprogram object.

      if Present (Enclosing_Subprogram (Subp_Entity)) then
         Gen_Duplicate;
         Load_Static_Link (Enclosing_Method (Subp_Entity));
         Gen_Conversion (Type_Of (AR_Field));
         Gen_Put_Field (AR_Field);
      end if;
   end Evaluate_Subprogram_Access;

   --------------------------------------
   -- Evaluate_Unconstrained_Array_Ref --
   --------------------------------------

   procedure Evaluate_Unconstrained_Array_Ref
     (Arr_Expr : Node_Id;
      Acc_Type : Entity_Id)
   is
      Arr_Ref_Typ   : constant Type_Id  := JVM_Expr_Type (Acc_Type);
      Arr_Ref_Class : constant Class_Id := Class_Of_Type (Arr_Ref_Typ);
      Alloc_LV      : constant Local_Var_Id :=
                        New_Local_Var ("_alloc_tmp", Arr_Ref_Typ);
      Arr_Typ       : constant Entity_Id := Designated_Type (Acc_Type);
      Dimensions    : Pos_8 := Pos_8 (Number_Dimensions (Arr_Typ));

   begin
      pragma Assert (Is_Array_Descriptor (Arr_Ref_Typ));

      if Nkind (Arr_Expr) = N_Slice
        and then Nkind (Prefix (Arr_Expr)) = N_Explicit_Dereference
      then
         Evaluate_Expr (Arr_Expr);
         Gen_Store_Local (Alloc_LV);

      else
         --  Allocate the unconstrained array pointer object

         Gen_Default_Object (Arr_Ref_Class);
         Gen_Store_Local (Alloc_LV);

         --  Evaluate the prefix and load its bounds

         if Nkind (Arr_Expr) /= N_Function_Call then
            Evaluate_Expr (Arr_Expr);

            if Nkind (Arr_Expr) in N_Raise_xxx_Error then
               return;
            end if;

            Load_Array_Bounds (Arr_Expr, Pos (Dimensions));

         --  Handle indirect call to a function returning unconstrained array
         --  type. We disable generation of array descriptor for functions
         --  returning unconstrained arrays because the descriptor is built by
         --  the called subprogram.

         elsif Is_Array_Type (Etype (Arr_Expr))
           and then not Is_Constrained (Etype (Arr_Expr))
           and then Nkind (Name (Arr_Expr)) = N_Explicit_Dereference
           and then Convention (Etype (Name (Arr_Expr))) = Convention_Ada
         then
            Translate_Subprogram_Call (Arr_Expr);
            pragma Assert (Is_Array_Descriptor (Top_Type));
            return;

         --  Handle function returning unconstrained array type. We disable
         --  generation of array descriptor for functions returning
         --  unconstrained arrays because the descriptor is built by the called
         --  subprogram.

         elsif Is_Array_Type (Etype (Arr_Expr))
           and then not Is_Constrained (Etype (Arr_Expr))
           and then Convention (Entity (Name (Arr_Expr))) = Convention_Ada
         then
            Translate_Subprogram_Call (Arr_Expr);
            pragma Assert (Is_Array_Descriptor (Top_Type));
            return;

         --  The result of the function is a bare array reference and we
         --  construct the bounds wrapper here. We call
         --  Translate_Subprogram_Call here to avoid recursion. Eventually this
         --  code will only be used for calls to imported Java functions
         --  returning arrays, but Ada functions with unconstrained result
         --  subtypes do not currently return bounds information, so we
         --  artificially create them here. This is not correct in general of
         --  course, but works for most practical purposes. ???

         else
            Translate_Subprogram_Call (Arr_Expr);
            pragma Assert (not Is_Array_Descriptor (Top_Type));
            Load_Array_Bounds (Arr_Expr, Pos (Dimensions));
         end if;

         --  Save the upper bounds, lower bounds, and array reference in
         --  reverse order into the unconstrained array pointer.

         while Dimensions > 1 loop
            Gen_Load_Local (Alloc_LV);
            Gen_Swap;
            Gen_Put_Field
              (Field (Arr_Ref_Class, "last_" & Image (Dimensions)));

            Gen_Load_Local (Alloc_LV);
            Gen_Swap;
            Gen_Put_Field
              (Field (Arr_Ref_Class, "first_" & Image (Dimensions)));

            Dimensions := Dimensions - 1;
         end loop;

         Gen_Load_Local (Alloc_LV);
         Gen_Swap;
         Gen_Put_Field (Field (Arr_Ref_Class, "last"));

         Gen_Load_Local (Alloc_LV);
         Gen_Swap;
         Gen_Put_Field (Field (Arr_Ref_Class, "first"));

         --  Finally save the pointer to the array in field "all"

         Gen_Load_Local (Alloc_LV);
         Gen_Swap;

         pragma Assert (Is_Descriptor (Top_Type (Disp => 1)));
         Gen_Put_Field (Descriptor_Field (Arr_Ref_Typ));
      end if;

      --  Leave a reference to the array descriptor on the stack

      Gen_Load_Local (Alloc_LV);
   end Evaluate_Unconstrained_Array_Ref;

   ------------------------
   -- Evaluate_With_Copy --
   ------------------------

   procedure Evaluate_With_Copy (Expr : Node_Id) is
      Exp_Type     : constant Entity_Id := Underlying_Type (Etype (Expr));
      Arr_Name     : Node_Id;
      Arr_Subt     : Entity_Id;
      Is_Slice     : Boolean;
      Sub_Expr     : Node_Id := Expr;
      Sub_Exp_Type : Entity_Id := Exp_Type;

      Is_Return_Stmt : constant Boolean :=
        Present (Parent (Expr))
          and then Nkind (Parent (Expr)) = Sinfo.N_Simple_Return_Statement;

   begin
      if Nkind (Expr) = N_Qualified_Expression then
         Sub_Expr     := Expression (Expr);
         Sub_Exp_Type := Full_Type (Sub_Expr);
      end if;

      case Ekind (Exp_Type) is
         when Elementary_Kind =>
            Evaluate_Expr (Expr);

         when Einfo.Record_Kind =>
            --  Extract the innermost subexpression and its type from within
            --  any containing qualifications or conversions. This is important
            --  for the case of applying a static deep copy for a tagged type
            --  (we want to invoke the deep copy associated with the type of
            --  the innermost object to preserve its tag in the case of
            --  initializing a class-wide object). This is still not right for
            --  the case of a formal parameter whose underlying object has a
            --  different tag (seems to require either knowing that the context
            --  is a class-wide object initialization or else always doing a
            --  clone, even when a dispatch is not needed). ???

            while Nkind_In (Sub_Expr, N_Qualified_Expression,
                                      N_Type_Conversion)
              and then not Is_Interface (Full_Type (Expression (Sub_Expr)))
            loop
               Sub_Expr     := Expression (Sub_Expr);
               Sub_Exp_Type := Full_Type (Sub_Expr);
            end loop;

            --  If the expression is an aggregate or a function result, then
            --  simply evaluate without making a copy (which would be
            --  redundant).

            if Nkind_In (Sub_Expr, N_Aggregate, N_Function_Call)
              or else Is_Imported (Exp_Type)
            then
               Evaluate_Expr (Expr);

            --  Otherwise the result of the expression needs to be copied.

            --  For non-tagged types we push a null target value, evaluate the
            --  expression, and generate a call to the type's deep copy
            --  operation.

            elsif not Is_Tagged_Type (Sub_Exp_Type) then
               Gen_Push_Null;
               Evaluate_Expr (Expr);
               Gen_Invoke_Deep_Copy (Sub_Exp_Type);

            --  For unchecked type conversion of tagged types we invoke the
            --  non-overridden method _deep_copy to ensure that the tag of
            --  the resulting object is correct.

            elsif Nkind (Expr) = N_Unchecked_Type_Conversion
               and then not Is_Class_Wide_Type (Sub_Exp_Type)
            then
               Gen_Push_Null;
               Evaluate_Expr (Expr);
               Gen_Invoke_Deep_Copy (Sub_Exp_Type);

            --  For tagged types, then evaluate the expression and generate a
            --  call to the type's deep clone operation. This is a dispatching
            --  call, which is necessary to handle cases involving class-wide
            --  object creation, and initialization.

            else
               Evaluate_Expr (Expr);
               Gen_Invoke_Deep_Clone (Sub_Exp_Type);
            end if;

         when Einfo.Array_Kind =>
            if Nkind_In (Sub_Expr, N_Aggregate, N_Function_Call) then
               Evaluate_Expr (Expr);

            --  If the value is of a one-dimensional scalar array type or the
            --  temporary variable generated for an VM_By_Copy actual, then
            --  allocate a new array object for the result and copy in the
            --  value by calling java.lang.System.arrayCopy.

            elsif Number_Dimensions (Exp_Type) = 1
               and then (Ekind (Full_Type (Component_Type (Exp_Type)))
                           in Elementary_Kind
                         or else Ekind (Full_Type (Component_Type (Exp_Type)))
                           = E_String_Type

                           --  There is no easy way to know if the expression
                           --  corresponds with a temporary variable generated
                           --  for an VM_By_Copy actual.

                         or else
                           (Is_VM_By_Copy_Actual (Expr)
                              and then Nkind (Expr) /= N_Slice
                              and then Nkind (Parent (Expr))
                                          = N_Object_Declaration
                              and then Aliased_Present (Parent (Expr))
                              and then Assignment_OK (Parent (Expr))
                              and then
                                Is_Internal_Name
                                  (Chars
                                    (Defining_Identifier (Parent (Expr))))))
               and then not
                 Has_Aliased_Components (Full_Type (Component_Type (Exp_Type)))
            then
               declare
                  Arr_LV   : constant Local_Var_Id :=
                               New_Local_Var
                                 ("_ewc_arr_tmp_1", JVM_Type (Exp_Type));
                  Arr_Name : Node_Id;
                  Arr_Subt : Entity_Id;
                  Is_Slice : Boolean;

               begin
                  Test_For_Slice (Expr, Is_Slice, Arr_Name, Arr_Subt);
                  Evaluate_Expr (Expr);

                  --  Load the starting index of the evaluated array followed
                  --  by the array length

                  if Is_Slice then
                     Gen_Array_Subscript (Arr_Name, Index_First (Arr_Subt));
                     Load_Index_Length (First_Index (Arr_Subt));
                  else
                     if Is_Array_Descriptor (Top_Type) then

                        --  Handle cases in which we must copy the array
                        --  descriptor and its associated array. That is,
                        --  return statement and assignment of array
                        --  descriptors.

                        if (Is_Return_Stmt
                              and then Is_Array_Descriptor
                                        (Result_Type (Current_Method)))
                           --  Handle assignment of array descriptors
                          or else JVM_Type (Exp_Type) = Top_Type
                        then
                           Deep_Copy_Array_Descriptor (Expr);
                           return;
                        end if;

                        Gen_Get_Field (Descriptor_Field (Top_Type));
                     end if;

                     pragma Assert (not Is_Array_Descriptor (Top_Type));
                     Gen_Duplicate;
                     Gen_Array_Length;
                     Gen_Push_Int (Uint_0);
                     Gen_Swap;
                  end if;

                  --  Allocate the new object and save its reference in a
                  --  temporary.

                  if JVM.Type_Kind (JVM_Type (Arr_Subt)) /= JVM.Array_Kind then
                     Arr_Subt := Etype (Expr);
                     Error_Msg_N
                       ("unsupported construct in this context", Expr);
                  end if;

                  Gen_New_Array (Arr_Subt);
                  Gen_Duplicate;
                  Gen_Store_Local (Arr_LV);

                  --  Load the offset of the target array object (always 0)

                  Gen_Push_Int (Uint_0);

                  --  Generate the array length again and call the copy routine

                  Gen_Load_Local (Arr_LV);
                  Gen_Array_Length;
                  Gen_Invoke_API_Method (System_arraycopy);

                  --  Leave a reference to the new array object on the stack

                  Gen_Load_Local (Arr_LV);
               end;

            --  If the result of the expression needs to be copied, then push a
            --  null target value, evaluate the expression, and generate a call
            --  to the type's deep copy operation.

            else
               Evaluate_Expr (Expr);

               declare
                  Is_Descriptor : constant Boolean :=
                                    Is_Array_Descriptor (Top_Type);
                  Check_Source  : constant Boolean := not Is_Descriptor;
                  No_Copy_Lbl   : constant Label_Id := New_Label;
                  Exit_Lbl      : constant Label_Id := New_Label;
                  Arr_Typ       : Type_Id;
                  Arr_LV        : Local_Var_Id;
                  Check_State   : Boolean;

               begin
                  Suppress_Stack_Checking (Check_State);

                  if Is_Descriptor then
                     Arr_Typ := Top_Type;
                  else
                     Arr_Typ := JVM_Type (Exp_Type);
                  end if;

                  Arr_LV := New_Local_Var ("_ewc_arr_tmp_2", Arr_Typ);
                  Gen_Store_Local (Arr_LV);

                  --  Check null source

                  if Check_Source then
                     Check_Flat_Array
                       (Arr_LV      => Arr_LV,
                        Is_Flat_Lbl => No_Copy_Lbl);
                  end if;

                  Test_For_Slice (Expr, Is_Slice, Arr_Name, Arr_Subt);

                  --  Load the array length followed by the starting index of
                  --  the evaluated array.

                  if Is_Slice then
                     Gen_Push_Null;
                     Gen_Push_Int (Uint_0);
                     Gen_Load_Local (Arr_LV);

                     Gen_Array_Subscript (Arr_Name, Index_First (Arr_Subt));
                     Load_Index_Length (First_Index (Arr_Subt));
                     Gen_Swap;

                  elsif Is_Descriptor
                    and then Stack_Heigth > 0
                    and then Is_Array_Descriptor (Top_Type)
                    and then ((Is_Return_Stmt
                                 and then Is_Array_Descriptor
                                            (Result_Type (Current_Method)))
                                or else JVM_Type (Exp_Type) = Arr_Typ)
                  then
                     Gen_Load_Local (Arr_LV);
                     Deep_Copy_Array_Descriptor (Expr);
                     return;

                  else
                     Gen_Push_Null;
                     Gen_Push_Int (Uint_0);
                     Gen_Load_Local (Arr_LV);

                     if Is_Array_Descriptor (Top_Type) then
                        pragma Assert (Is_Array_Descriptor (Top_Type));
                        Gen_Get_Field (Descriptor_Field (Top_Type));
                     end if;

                     pragma Assert (not Is_Array_Descriptor (Top_Type));
                     Gen_Duplicate;
                     Gen_Array_Length;
                     Gen_Push_Int (Uint_0);
                  end if;

                  Gen_Invoke_Deep_Copy (Full_Type (Exp_Type));
                  Gen_Goto (Exit_Lbl);

                  if Check_Source then
                     Pop_Type;
                     Gen_Label (No_Copy_Lbl);
                     Gen_Load_Local (Arr_LV);
                  end if;

                  Gen_Label (Exit_Lbl);
                  Restore_Stack_Checking (Check_State);
               end;
            end if;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Evaluate_With_Copy;

   -------------------------
   -- Gen_Array_Subscript --
   -------------------------

   procedure Gen_Array_Subscript
     (Prefix          : Node_Id;
      First_Subscript : Node_Id;
      Array_Var       : Local_Var_Id := Null_Local_Var)
   is
      Arr_Subtype     : constant Entity_Id := Underlying_Type (Etype (Prefix));
      Acc_Unconstr    : Boolean := False;
      Arr_Descriptor  : Local_Var_Id;
      Descriptor_Type : Type_Id;
      Dimension       : Pos_8   := 1;
      Formal_LV       : Local_Var_Id;
      Formal_First    : Local_Var_Id;
      Index_Range     : Node_Id;
      Low_Bound       : Node_Id;
      Parent_Method   : Method_Id;
      Prefix_Is_Slice : Boolean := False;
      Slice_Prfx      : Node_Id;
      Slice_Subt      : Entity_Id;
      Subscript       : Node_Id := First_Subscript;
      Unconstr_Init   : Boolean := False;

   begin
      --  Add implicit conversion if the evaluation of Prefix left a
      --  Java.Lang.Object in the stack.

      if Stack_Heigth > 0
        and then Top_Type = Java_Lang_Object_Type
      then
         Gen_Check_Cast (JVM_Type (Etype (Prefix)));
      end if;

      --  Either this routine has called itself recursively and the top of the
      --  stack contains the index of the array or it is called with an array
      --  passed in argument Array_Var or else the array (or the array
      --  descriptor) is stored in the top of the JVM stack.

      --  Note: The check on Null_Local_Var must be the first one executed in
      --  the followin assertion because if Array_Var is present then the stack
      --  may be empty and the call to Top_Type may crash the compiler.

      pragma Assert (Array_Var /= Null_Local_Var
        or else JVM.Type_Kind (Top_Type) = JVM.Int_Kind
        or else JVM.Type_Kind (Top_Type) = JVM.Array_Kind
        or else Is_Array_Descriptor (Top_Type));

      --  Handle array descriptors stored in the top of the JVM stack. Save
      --  the descriptor in a local variable to have it available when the
      --  array attributes are needed and leave the actual array address in
      --  the top of the stack.

      if Array_Var = Null_Local_Var
        and then Is_Array_Descriptor (Top_Type)
      then
         Acc_Unconstr    := True;
         Descriptor_Type := Top_Type;
         Arr_Descriptor  := New_Local_Var ("_arr_desc", Descriptor_Type);

         Gen_Duplicate;
         Gen_Store_Local (Arr_Descriptor);
         Gen_Get_Field (Descriptor_Field (Descriptor_Type));
      end if;

      Test_For_Slice (Prefix, Prefix_Is_Slice, Slice_Prfx, Slice_Subt);

      Index_Range := First_Index (Arr_Subtype);

      --  Unconstrained array formal. In this case the values of the array
      --  attributes are extra formals.

      if Is_Entity_Name (Prefix)
        and then Ekind (Entity (Prefix)) in Formal_Kind
        and then not Is_Constrained (Etype (Entity (Prefix)))
      then
         if Chars (Entity (Prefix)) = Name_uInit then
            Unconstr_Init := True;

            --  If the formal parameter has the name "_init", then the
            --  Front End will not have created an actual subtype for it,
            --  so we have to special case the indexing by explicitly
            --  grabbing the 'First formals via successors of the "_init"
            --  parameter.

            Formal_LV    := JVM_Local_Var (Entity (Prefix));
            Formal_First := Next_Local_Var (Formal_LV);
         else
            Index_Range :=
              First_Index (Underlying_Type (Actual_Subtype (Entity (Prefix))));
         end if;
      end if;

      while Present (Subscript) loop

         --  For an unconstrained _init parameter, the normalization of the
         --  subscript by the lower bound is performed by explicitly loading
         --  and subtracting the _init formal's lower bound parameter.

         if Unconstr_Init then
            Evaluate_Expr (Subscript);
            Add_Implicit_Index_Conversion;
            Parent_Method := Enclosing_Method (Entity (Prefix));

            if Parent_Method = Current_Method then
               Gen_Load_Local (Formal_First);
            else
               Register_Up_Level_Reference (Parent_Method, Formal_First);
               Load_Up_Level_Field (Parent_Method, Name (Formal_First));
            end if;

            Add_Implicit_Index_Conversion;
            Gen_Sub (Modular => False, Integer_Type => True);

         --  For array descriptors the value of attribute First is taken
         --  from the descriptor

         elsif Acc_Unconstr then
            Evaluate_Expr (Subscript);
            Add_Implicit_Index_Conversion;

            --  Load the appropriate lower bound from the array descriptor
            --  and subtract from the subscript.

            Gen_Load_Local (Arr_Descriptor);

            if Dimension = 1 then
               Gen_Get_Field
                 (Field (Class_Of_Type (Descriptor_Type), "first"));
            else
               Gen_Get_Field
                 (Field (Class_Of_Type (Descriptor_Type),
                         "first_" & Image (Dimension)));
            end if;

            Add_Implicit_Index_Conversion;
            Gen_Sub (Modular => False, Integer_Type => True);

         --  Normal indexing case; we recognize cases where the lower bound
         --  adjustment can be calculated statically

         else
            --  In case of static string literals, the Index_Range field is not
            --  present. Get directly the low_bound field in this case.

            if Present (Index_Range) then
               Low_Bound := Array_Index_First (Index_Range);
            else
               Low_Bound := String_Literal_Low_Bound (Arr_Subtype);
            end if;

            if not Compile_Time_Known_Value (Low_Bound)
              or else Expr_Value (Low_Bound) /= Uint_0
            then
               if Compile_Time_Known_Value (Subscript)
                 and then Compile_Time_Known_Value (Low_Bound)
               then
                  Gen_Push_Int
                    (Expr_Value (Subscript) - Expr_Value (Low_Bound), Prefix);

               --  We have to normalize the subscript by subtracting the lower
               --  bound at run time.

               else
                  Evaluate_Expr (Subscript);
                  Add_Implicit_Index_Conversion;

                  --  If the lower bound is a bare discriminant, then we have
                  --  to load it from the object containing the array.
                  --  Unfortunately the reference to that object is no longer
                  --  around (due to selecting the array from it), so we have
                  --  to reevaluate the record name prefix and select the
                  --  discriminant. This is not correct though in the case
                  --  where the record name evaluation has side effects. Not
                  --  clear how to cleanly handle this (perhaps need to have
                  --  caller pass in a local variable containing the prefix,
                  --  but that's very awkward!). ???

                  if Is_Entity_Name (Low_Bound)
                    and then Ekind (Entity (Low_Bound)) = E_Discriminant
                    and then Nkind (Prefix) = N_Selected_Component
                    and then
                      JVM_Method (Base_Init_Proc (Scope (Entity (Low_Bound))))
                        /= Current_Method
                  then
                     Evaluate_Expr (Sinfo.Prefix (Prefix));
                     Gen_Get_Field (JVM_Field (Entity (Low_Bound)));

                  --  In the case of a static lower bound, we push the the
                  --  value directly rather than letting it get handled by
                  --  Evaluate_Expr. This is a little odd, but there are cases
                  --  where this is a bound of an integer type declaration and
                  --  it may have type universal_integer, which will result in
                  --  a value of type long being pushed rather than an int, but
                  --  JVM arrays are always indexed by int, so we work around
                  --  this case.

                  elsif Compile_Time_Known_Value (Low_Bound) then
                     Gen_Push_Int (Expr_Value (Low_Bound), Prefix);

                  else
                     Evaluate_Expr (Low_Bound);
                  end if;

                  Add_Implicit_Index_Conversion;
                  Gen_Sub (Modular => False, Integer_Type => True);
               end if;

               --  If prefix is itself a slice, then subscript are added.

               if Prefix_Is_Slice then
                  Gen_Array_Subscript (Slice_Prfx, Index_First (Slice_Subt));
                  Gen_Add (Modular => False, Integer_Type => True);
               end if;

            else
               Evaluate_Expr (Subscript);
               Add_Implicit_Index_Conversion;
            end if;

            if Present (Index_Range) then
               Next_Index (Index_Range);
            end if;
         end if;

         --  In the VM backends the index of arrays is an integer.

         pragma Assert (JVM.Type_Kind (Top_Type) = Int_Kind);

         --  If this is a slice indexing then the subscript isn't part of an
         --  expression list, so simply exit.

         if not Is_List_Member (Subscript) then
            return;
         end if;

         Subscript := Next (Subscript);

         --  If Subscript is not empty, then the preceding subscript is a
         --  subscript (not yet the last) for a multidimensional array and
         --  the reference to the next dimension's subarray must be loaded.

         if Present (Subscript) then
            Gen_Load_Subarray_Reference;

            --  For a multidimensional _init parameter we have to step to
            --  the next implicit bounds parameter (while first stepping
            --  past the upper bound parameter of the preceding dimension).

            if Unconstr_Init then
               Formal_First := Next_Local_Var (Next_Local_Var (Formal_First));
            end if;
         end if;

         Dimension := Dimension + 1;
      end loop;
   end Gen_Array_Subscript;

   ------------------------------
   -- Gen_Scalar_Subtype_Check --
   ------------------------------

   procedure Gen_Scalar_Subtype_Check
     (Scalar_Subt : Entity_Id;
      Node : Node_Id)
   is
      Low_Bnd     : Node_Id;
      High_Bnd    : Node_Id;
      Raise_Label : Label_Id;
      OK_Label    : Label_Id;
      Check_State : Boolean;

   begin
      if Present (Scalar_Range (Scalar_Subt))
        and then Is_Constrained (Scalar_Subt)
      then
         Low_Bnd  := Low_Bound (Scalar_Range (Scalar_Subt));
         High_Bnd := High_Bound (Scalar_Range (Scalar_Subt));

         Raise_Label := New_Label;
         OK_Label    := New_Label;

         --  Duplicate the expression value in order to leave the expression
         --  on the stack after the check (as well as providing the value
         --  for the upper bound check).

         Gen_Duplicate;

         Suppress_Stack_Checking (Check_State);

         --  Evaluate the low bound and test against the expression. If the
         --  bound is a literal, then its type may be universal (e.g., if
         --  it occurs in the range of an integer_type_definition), in which
         --  case we have to force it to be evaluated with the proper type.

         if Nkind (Low_Bnd) = N_Integer_Literal then
            Evaluate_Integer_Literal (Low_Bnd, Scalar_Subt);

         elsif Nkind (Low_Bnd) = N_Real_Literal then
            Evaluate_Real_Literal (Low_Bnd, Scalar_Subt);

         else
            Evaluate_Expr (Low_Bnd);
         end if;

         Gen_Compare_Branch_Less (Raise_Label);

         --  If the subtype to check against is modular with an upper bound
         --  that is greater than 2**31 - 1 then we can't do a signed
         --  comparison (because it will compare against a negative signed
         --  value). It's not clear how to cleanly handle this case ??? This
         --  suppression also isn't right for the 64-bit modular case, since
         --  the comparison should work fine for upper bounds up to 2**63 - 1.
         --  ???

         if Ekind (Scalar_Subt) in Modular_Integer_Kind
           and then Nkind (High_Bnd) = N_Integer_Literal
           and then Intval (High_Bnd) >= 2 ** (Uint_32 - 1)
         then
            Gen_Goto (OK_Label);

         --  Evaluate the high bound and test against the expression. If the
         --  bound is a literal, then its type may be universal (e.g., if
         --  it occurs in the range of an integer_type_definition), in which
         --  case we have to force it to be evaluated with the proper type.

         else
            Gen_Duplicate;

            if Nkind (High_Bnd) = N_Integer_Literal then
               Evaluate_Integer_Literal (High_Bnd, Scalar_Subt);

            elsif Nkind (High_Bnd) = N_Real_Literal then
               Evaluate_Real_Literal (High_Bnd, Scalar_Subt);

            else
               Evaluate_Expr (High_Bnd);
            end if;

            Gen_Compare_Branch_Less_Equal (OK_Label);
         end if;

         --  Generate a raise of Constraint_Error

         Gen_Label (Raise_Label);
         Generate_Exception_And_Throw
           (API_Class (Ada_Constraint_Error), Sloc (Node),
            UI_From_Int (RT_Exception_Code'Pos (CE_Range_Check_Failed)));

         Gen_Label (OK_Label);

         Restore_Stack_Checking (Check_State);
      end if;
   end Gen_Scalar_Subtype_Check;

   -----------------
   -- Index_First --
   -----------------

   function Index_First (Array_Subtype : Entity_Id) return Node_Id is
      Index : constant Node_Id :=
                First_Index (Underlying_Type (Array_Subtype));
   begin
      return Array_Index_First (Index);
   end Index_First;

   ----------------
   -- Index_Last --
   ----------------

   function Index_Last (Array_Subtype : Entity_Id) return Node_Id is
      Index : constant Node_Id :=
                First_Index (Underlying_Type (Array_Subtype));
   begin
      return Array_Index_Last (Index);
   end Index_Last;

   ----------------
   -- In_Runtime --
   ----------------

   function In_Runtime (E : Entity_Id) return Boolean is
      S1 : Entity_Id := E;

   begin
      while Present (Scope (S1))
        and then Scope (S1) /= Standard_Standard
      loop
         S1 := Scope (S1);
      end loop;

      return Chars (S1) = Name_System or else Chars (S1) = Name_Ada;
   end In_Runtime;

   -------------------
   -- JVM_Expr_Type --
   -------------------

   function JVM_Expr_Type (Expr : Node_Id) return Type_Id is
   begin
      return JVM_Type (Etype (Expr));
   end JVM_Expr_Type;

   ---------------------
   -- Load_Array_Attr --
   ---------------------

   procedure Load_Array_Attr
     (Arr_Expr   : Node_Id;
      Array_Attr : Array_Attribute_Kind;
      Dimension  : Pos_8)
   is
      Desig_Type    : Entity_Id;
      Index_Range   : Node_Id;
      Index_Bound   : Node_Id;
      Acc_Type      : Entity_Id;
      Prefix_Obj    : Entity_Id;
      Parent_Method : Method_Id;
      Formal_LV     : Local_Var_Id;
      Formal_First  : Local_Var_Id;
      Formal_Last   : Local_Var_Id;

      procedure Load_Attribute_Simple;
      --  Simple common case of handling array attribute

      procedure Load_Descriptor_Attribute;
      --  Load the attribute from the JVM array descriptor stored in the stack

      procedure Load_VM_Bound;
      --  For array types with convention VM evaluate attribute 'first as the
      --  integer value 0, and attribute 'last as attribute 'length minus 1.

      ---------------------------
      -- Load_Attribute_Simple --
      ---------------------------

      procedure Load_Attribute_Simple is
      begin
         Index_Range := First_Index (Full_Subtype (Arr_Expr));

         --  Retrieve the index corresponding to the specified dimension

         for Dim in 2 .. Dimension loop
            Next_Index (Index_Range);
         end loop;

         case Array_Attr is
            when First =>
               Index_Bound := Array_Index_First (Index_Range);
            when Last =>
               Index_Bound := Array_Index_Last (Index_Range);
         end case;

         --  If the bound is a bare discriminant, then evaluate the prefix of
         --  the attribute's selected component prefix and load the
         --  discriminant

         if Nkind (Arr_Expr) = N_Selected_Component
           and then Is_Entity_Name (Index_Bound)
           and then Ekind (Entity (Index_Bound)) = E_Discriminant
         then
            Evaluate_Expr (Prefix (Arr_Expr));
            Gen_Get_Object_Field (JVM_Field (Entity (Index_Bound)));

         --  Otherwise, evaluate the bound expression

         else
            Evaluate_Expr (Index_Bound);
         end if;

         Add_Implicit_Index_Conversion;
      end Load_Attribute_Simple;

      -------------------------------
      -- Load_Descriptor_Attribute --
      -------------------------------

      procedure Load_Descriptor_Attribute is
      begin
         pragma Assert (Is_Array_Descriptor (Top_Type));

         case Array_Attr is
            when First =>
               if Dimension = 1 then
                  Gen_Get_Field
                    (Field (Class_Of_Type (Top_Type), "first"));
               else
                  Gen_Get_Field
                    (Field (Class_Of_Type (Top_Type),
                            "first_" & Image (Dimension)));
               end if;

            when Last =>
               if Dimension = 1 then
                  Gen_Get_Field
                    (Field (Class_Of_Type (Top_Type), "last"));
               else
                  Gen_Get_Field
                    (Field (Class_Of_Type (Top_Type),
                            "last_" & Image (Dimension)));
               end if;
         end case;
      end Load_Descriptor_Attribute;

      -------------------
      -- Load_VM_Bound --
      -------------------

      procedure Load_VM_Bound is
      begin
         pragma Assert (JVM.Type_Kind (Top_Type) = JVM.Array_Kind);

         case Array_Attr is
            when First =>
               --  Pop prefix evaluation and push zero
               Gen_Pop;
               Gen_Push_Int (Uint_0);

            when Last =>
               for Dim in 2 .. Dimension loop
                  Gen_Push_Int (Uint_0);
                  Gen_Load_Subarray_Reference;
               end loop;

               Gen_Array_Length;
               Gen_Push_Int (Uint_1);
               Gen_Sub (Modular => False, Integer_Type => True);
         end case;
      end Load_VM_Bound;

   --  Start of processing for Load_Array_Attr

   begin
      if Is_Access_Type (Underlying_Type (Etype (Arr_Expr))) then
         Acc_Type   := Underlying_Type (Etype (Arr_Expr));
         Desig_Type := Full_Subtype (Designated_Type (Acc_Type));

      elsif Nkind (Arr_Expr) = N_Explicit_Dereference then
         Acc_Type   := Full_Type (Prefix (Arr_Expr));
         Desig_Type := Full_Subtype (Designated_Type (Acc_Type));
      end if;

      --  String literals

      if Ekind (Full_Subtype (Arr_Expr)) = E_String_Literal_Subtype then
         declare
            Arr_Typ    : constant Entity_Id := Full_Subtype (Arr_Expr);
            String_Low : constant Node_Id :=
                           String_Literal_Low_Bound (Arr_Typ);
         begin
            case Array_Attr is
               when First =>
                  Evaluate_Expr (String_Low);
                  Add_Implicit_Index_Conversion;

               when Last =>
                  if Compile_Time_Known_Value (String_Low) then
                     Gen_Push_Int
                       (Expr_Value (String_Low)
                         + String_Literal_Length (Arr_Typ) - 1);
                  else
                     Evaluate_Expr (String_Low);
                     Add_Implicit_Index_Conversion;
                     Gen_Push_Int (String_Literal_Length (Arr_Typ) - 1);
                     Gen_Add (Modular => False, Integer_Type => True);
                  end if;
            end case;
         end;

      --  In the case of a renamed entity, recursively load the attribute of
      --  the renamed entity

      elsif Is_Entity_Name (Arr_Expr)
        and then Present (Renamed_Object (Entity (Arr_Expr)))
      then
         Load_Array_Attr (Renamed_Object (Entity (Arr_Expr)),
                          Array_Attr, Dimension);

      --  If the expression has a subexpression with an unconstrained type,
      --  then we need to load the bounds of the subexpression.

      elsif Nkind_In (Arr_Expr, N_Type_Conversion,
                                N_Unchecked_Type_Conversion,
                                N_Qualified_Expression,
                                N_Expression_With_Actions)
        and then not Is_Constrained (Full_Subtype (Arr_Expr))
      then
         Load_Array_Attr (Expression (Arr_Expr), Array_Attr, Dimension);

      --  Access to unconstrained arrays

      elsif Is_Access_Type (Underlying_Type (Etype (Arr_Expr)))
        and then not Is_Constrained (Desig_Type)
      then
         Evaluate_Expr (Arr_Expr);

         if Convention (Scope (Acc_Type)) = Convention_VM
           or else (Is_Local_Anonymous_Access (Acc_Type)
                    and then Convention (Scope (Desig_Type)) = Convention_VM)
         then
            Load_VM_Bound;
            return;
         end if;

         pragma Assert (Is_Array_Descriptor (Top_Type));
         pragma Assert (Top_Type = JVM_Type (Acc_Type));

         --  Normal Ada access-to-unconstrained case

         Load_Descriptor_Attribute;

      elsif Nkind (Arr_Expr) = N_Explicit_Dereference
        and then not Is_Constrained (Desig_Type)
      then
         Evaluate_Expr (Prefix (Arr_Expr));

         --  If this a dereference of an access-to-unconstrained array value
         --  for a type from a Java-Convention package, then use zero for the
         --  lower bound and (length - 1) for the upper bound (no wrapper
         --  object is available in this case).

         if not Is_Array_Descriptor (Top_Type) then
            Load_VM_Bound;

         else
            pragma Assert (Is_Array_Descriptor (Top_Type));
            pragma Assert (Top_Type = JVM_Type (Acc_Type));

            --  Normal Ada access-to-unconstrained case

            Load_Descriptor_Attribute;
         end if;

      elsif Is_Constrained (Full_Subtype (Arr_Expr)) then
         Load_Attribute_Simple;

      --  Function returning unconstrained array

      elsif Nkind (Arr_Expr) = N_Function_Call
        and then Is_Array_Type (Etype (Arr_Expr))
        and then not Is_Constrained (Full_Subtype (Arr_Expr))
      then
         if Convention (Entity (Name (Arr_Expr))) = Convention_Ada then
            Evaluate_Expr (Arr_Expr);
            pragma Assert (Is_Array_Descriptor (Top_Type));
            Load_Descriptor_Attribute;
         else
            Load_Attribute_Simple;
         end if;

      elsif Nkind (Arr_Expr) = N_Selected_Component then
         Load_Attribute_Simple;

      elsif Nkind (Arr_Expr) = N_If_Expression then
         Load_Attribute_Simple;

      else pragma Assert (Is_Entity_Name (Arr_Expr));
         pragma Assert (not Is_Constrained (Full_Subtype (Arr_Expr)));
         Prefix_Obj := Entity (Arr_Expr);

         if Ekind (Prefix_Obj) not in Formal_Kind then
            Load_Attribute_Simple;

         else
            Parent_Method := Enclosing_Method (Prefix_Obj);
            Formal_LV     := JVM_Local_Var (Prefix_Obj);
            Formal_First  := Next_Local_Var (Formal_LV);

            --  Retrieve the formal parameter corresponding to the lower bound
            --  of the specified dimension. Note that it's necessary to step
            --  over the upper bound parameter of each dimension to get to the
            --  next lower bound parameter.

            for Dim in 2 .. Dimension loop
               Formal_First := Next_Local_Var (Next_Local_Var (Formal_First));
            end loop;

            case Array_Attr is
               when First =>
                  if Parent_Method = Current_Method then
                     Gen_Load_Local (Formal_First);
                  else
                     Register_Up_Level_Reference (Parent_Method, Formal_First);
                     Load_Up_Level_Field (Parent_Method, Name (Formal_First));
                  end if;

               when Last =>
                  Formal_Last := Next_Local_Var (Formal_First);

                  if Parent_Method = Current_Method then
                     Gen_Load_Local (Formal_Last);
                  else
                     Register_Up_Level_Reference (Parent_Method, Formal_Last);
                     Load_Up_Level_Field (Parent_Method, Name (Formal_Last));
                  end if;
            end case;
         end if;
      end if;
   end Load_Array_Attr;

   -----------------------
   -- Load_Array_Bounds --
   -----------------------

   procedure Load_Array_Bounds (Arr_Expr : Node_Id; Dimension : Pos) is

      procedure Get_VM_Bounds;
      --  Evaluate the bounds of an array type that has convention VM

      -------------------
      -- Get_VM_Bounds --
      -------------------

      procedure Get_VM_Bounds is
         Saved_Array : Local_Var_Id;
         Saved_LB    : Local_Var_Id;
         Typ         : Entity_Id;
         Idx         : Node_Id;
         Low_Bound   : Node_Id;

      begin
         pragma Assert (JVM.Type_Kind (Top_Type) = JVM.Array_Kind);
         pragma Assert (not Is_Array_Descriptor (Top_Type));

         Typ := Full_Type (Arr_Expr);

         if Is_Access_Type (Typ) then
            Typ := Full_Type (Designated_Type (Typ));
         end if;

         Idx := First_Index (Typ);
         Low_Bound := Array_Index_First (Idx);

         Saved_Array := New_Local_Var ("_gb_arr_tmp", Top_Type);

         Gen_Duplicate;
         Gen_Store_Local (Saved_Array);

         for D in Int_8 range 1 .. Int_8 (Dimension) loop
            --  First

            Evaluate_Expr (Low_Bound);

            --  Store the low bound for further use
            if Nkind (Low_Bound) /= N_Integer_Literal
              or else Intval (Low_Bound) /= Uint_1
            then
               Saved_LB := New_Local_Var ("_arr_lb", Top_Type);
               Gen_Duplicate;
               Gen_Store_Local (Saved_LB);
            end if;

            --  Last: calculate as being first + length - 1

            Gen_Load_Local (Saved_Array);
            for Dim in 2 .. D loop
               Gen_Push_Int (Uint_0);
               Gen_Load_Subarray_Reference;
            end loop;

            Gen_Array_Length;

            if Nkind (Low_Bound) /= N_Integer_Literal
              or else Intval (Low_Bound) /= Uint_1
            then
               --  Push Last as First + Length - 1
               Gen_Load_Local (Saved_LB);
               Gen_Add (False, True);
               Gen_Push_Int (Uint_1);
               Gen_Sub (False, True);
            end if;

            Next_Index (Idx);

            if Present (Idx) then
               Low_Bound := Array_Index_First (Idx);
            end if;
         end loop;
      end Get_VM_Bounds;

   --  Start of processing for Load_Array_Bounds

   begin
      --  String literals

      if Ekind (Full_Subtype (Arr_Expr)) = E_String_Literal_Subtype then
         declare
            Arr_Typ    : constant Entity_Id := Full_Subtype (Arr_Expr);
            String_Low : constant Node_Id :=
                           String_Literal_Low_Bound (Arr_Typ);

         begin
            Evaluate_Expr (String_Low);
            Add_Implicit_Index_Conversion;

            if Compile_Time_Known_Value (String_Low) then
               Gen_Push_Int
                 (Expr_Value (String_Low)
                   + String_Literal_Length (Arr_Typ) - 1);
            else
               Gen_Duplicate;
               Gen_Push_Int (String_Literal_Length (Arr_Typ) - 1);
               Gen_Add (Modular => False, Integer_Type => True);
            end if;
         end;

      --  If we have an array descriptor available in the top of the stack
      --  there is no need to do any further processing; we just directly
      --  provide its attribute values.

      elsif Is_Array_Descriptor (Top_Type) then
         declare
            Descr_Typ   : constant Type_Id  := Top_Type;
            Descr_Class : constant Class_Id := Class_Of_Type (Descr_Typ);
            Descr_LV    : constant Local_Var_Id :=
                            New_Local_Var ("_desc_tmp", Descr_Typ);

         begin
            Gen_Store_Local (Descr_LV);

            Gen_Load_Local (Descr_LV);
            Gen_Get_Field (Field (Descr_Class, "all"));

            Gen_Load_Local (Descr_LV);
            Gen_Get_Field (Field (Descr_Class, "first"));

            Gen_Load_Local (Descr_LV);
            Gen_Get_Field (Field (Descr_Class, "last"));

            for D in 2 .. Dimension loop
               Gen_Load_Local (Descr_LV);
               Gen_Get_Field
                 (Field (Descr_Class, "first_" & Image (Int_8 (D))));

               Gen_Load_Local (Descr_LV);
               Gen_Get_Field
                 (Field (Descr_Class, "last_" & Image (Int_8 (D))));
            end loop;
         end;

      --  For entity renamings recursively load the bounds from the renamed
      --  entity because constraints implied by the renaming declaration must
      --  be ignored -- AARM 8.5.1 (6/2).

      elsif Is_Entity_Name (Arr_Expr)
        and then Present (Renamed_Object (Entity (Arr_Expr)))
      then
         Load_Array_Bounds (Renamed_Object (Entity (Arr_Expr)), Dimension);

      --  If the expression has a subexpression with an unconstrained type,
      --  then we need to load the bounds of the subexpression.

      elsif Nkind_In (Arr_Expr, N_Type_Conversion,
                                N_Unchecked_Type_Conversion,
                                N_Qualified_Expression,
                                N_Expression_With_Actions)
        and then not Is_Constrained (Full_Subtype (Arr_Expr))
      then
         Load_Array_Bounds (Expression (Arr_Expr), Dimension);

      --  Dereference of an access-to-unconstrained array value

      elsif Nkind (Arr_Expr) = N_Explicit_Dereference
        and then not Is_Constrained (Full_Subtype (Arr_Expr))
      then
         Load_Array_Bounds (Prefix (Arr_Expr), Dimension);

      --  Java-convention array

      elsif Is_Array_Type (Full_Subtype (Arr_Expr))
        and then not Is_Constrained (Full_Subtype (Arr_Expr))
        and then Convention (Scope (Full_Type (Arr_Expr))) = Convention_VM
      then
         Get_VM_Bounds;

      --  Imported function returning unconstrained array

      elsif Nkind (Arr_Expr) = N_Function_Call
        and then Is_Imported (Entity (Name (Arr_Expr)))
      then
         Get_VM_Bounds;

      --  If the node has been replaced by the frontend by a CE then we
      --  generate dummy values for all the bounds

      elsif Nkind (Arr_Expr) = N_Raise_Constraint_Error then
         for D in Int_8 range 1 .. Int_8 (Dimension) loop
            Gen_Push_Int (Uint_0);
            Gen_Push_Int (Uint_1);
         end loop;

      --  If the expression is a constrained array or a formal parameter, we
      --  directly evaluate the bounds

      elsif Is_Constrained (Full_Subtype (Arr_Expr))
        or else (Is_Entity_Name (Arr_Expr)
                 and then Ekind (Entity (Arr_Expr)) in Formal_Kind)
      then
         for D in Int_8 range 1 .. Int_8 (Dimension) loop
            Load_Array_Attr (Arr_Expr, First, D);
            Load_Array_Attr (Arr_Expr, Last, D);
         end loop;

      --  Load the low and high bounds from the VM object on the stack

      else
         --  We don't use Get_VM_Bounds here as it is not able to retrieve
         --  the correct lower bound in case of renamed objects. To do this,
         --  we need to call Load_Array_Attr (See bellow).

         --  ??? Those renamed arrays should really contain the bounds instead
         --  of just forgetting about them by calling Evaluate_Array_Address.
         --  Because of that, we need to reevaluate the original expression
         --  each time we need the bounds (as done by Load_Array_Attr)
         declare
            Saved_Array : Local_Var_Id;
            Saved_LB    : Local_Var_Id;

         begin
            Saved_Array := New_Local_Var ("_lab_arr_tmp", Top_Type);
            Gen_Duplicate;
            Gen_Store_Local (Saved_Array);

            for D in Int_8 range 1 .. Int_8 (Dimension) loop
               --  Push First and save it
               Load_Array_Attr (Arr_Expr, First, D);

               Saved_LB := New_Local_Var ("_arr_lb", Top_Type);
               Gen_Duplicate;
               Gen_Store_Local (Saved_LB);

               --  Push subarray, and retrieve the current length
               Gen_Load_Local (Saved_Array);
               for Dim in 2 .. D loop
                  Gen_Push_Int (Uint_0);
                  Gen_Load_Subarray_Reference;
               end loop;

               Gen_Array_Length;

               --  Push Last as First + Length - 1
               Gen_Load_Local (Saved_LB);
               Gen_Add (False, True);
               Gen_Push_Int (Uint_1);
               Gen_Sub (False, True);
            end loop;
         end;
      end if;
   end Load_Array_Bounds;

   -----------------------
   -- Load_Index_Length --
   -----------------------

   procedure Load_Index_Length
     (Index  : Node_Id;
      Obj_LV : Local_Var_Id := Null_Local_Var)
   is
      Low    : Node_Id;
      High   : Node_Id;
      Length : Uint;

      procedure Evaluate_Bound
        (Bound  : Node_Id;
         Obj_LV : Local_Var_Id := Null_Local_Var);
      --  Evaluates a bound expression. If the bound is a discriminant
      --  and Obj_LV is present, then load it will be loaded from the
      --  discriminant field of the denoted record object.

      --------------------
      -- Evaluate_Bound --
      --------------------

      procedure Evaluate_Bound
        (Bound  : Node_Id;
         Obj_LV : Local_Var_Id := Null_Local_Var)
      is
      begin
         --  If the bound is a discriminant and there is an object
         --  available, then load the discriminant from the object,
         --  unless this is within an init_proc, in which case we
         --  simply evaluate the discriminant directly (which will
         --  result in loading the value from the corresponding
         --  discriminal parameter). Note that the test for being
         --  within an init_proc is more complex than simply testing
         --  that the current method name is Name_uInit_Proc, since
         --  the name of the method may have been expanded to include
         --  the enclosing scope as a prefix.

         if Obj_LV /= Null_Local_Var
           and then Is_Entity_Name (Bound)
           and then Ekind (Entity (Bound)) = E_Discriminant
           and then JVM_Method (Base_Init_Proc (Scope (Entity (Bound))))
                      /= Current_Method
         then
            Gen_Load_Local (Obj_LV);
            Gen_Get_Field (JVM_Field (Entity (Bound)));

         else
            Evaluate_Expr (Bound);
         end if;

         Add_Implicit_Index_Conversion;
      end Evaluate_Bound;

   --  Start of processing for Load_Index_Length

   begin
      case Nkind (Index) is
         when N_Defining_Identifier =>
            Load_Index_Length (Scalar_Range (Index), Obj_LV);

         when N_Identifier | N_Expanded_Name =>
            Load_Index_Length (Scalar_Range (Entity (Index)), Obj_LV);

         when N_Subtype_Indication =>
            Load_Index_Length (Range_Expression (Constraint (Index)), Obj_LV);

         when N_Range | N_Signed_Integer_Type_Definition =>
            Low  := Low_Bound (Index);
            High := High_Bound (Index);

            --  When both bounds are static we compute the length
            --  at compile time and push the value.

            if Compile_Time_Known_Value (High)
              and then Compile_Time_Known_Value (Low)
            then
               Length := Expr_Value (High) - Expr_Value (Low) + 1;

               if Length < Uint_0 then
                  Length := Uint_0;
               end if;

               Gen_Push_Int (Length, Index);

            --  Nonstatic bounds case...

            else
               declare
                  Lbl         : constant Label_Id := New_Label;
                  Check_State : Boolean;

               begin
                  Evaluate_Bound (High, Obj_LV);

                  --  Compute the length of the array dimension:
                  --        High_Bound - Low_Bound + 1
                  --  Note that for one-based arrays we simply use the high
                  --  bound as is.

                  if Compile_Time_Known_Value (Low) then

                     --  In case of compile time known values we do the
                     --  following minor optimization:
                     --       High_Bound - (Low_Bound - 1)

                     --  Case 1: Low_Bound is 1

                     if Expr_Value (Low) = Uint_1 then
                        null;

                     --  Case 2: Low_Bound is zero

                     elsif Expr_Value (Low) = Uint_0 then
                        Gen_Push_Int (Uint_1);
                        Gen_Add (Modular        => False,
                                 Integer_Type   => True,
                                 Overflow_Check => True);

                     --  Case 2: Low_Bound is Integer'First. In this case we
                     --  cannot do the optimization because the result of
                     --  Integer'First minus 1 is out of range.

                     elsif Expr_Value (Low)
                            = Expr_Value
                               (Low_Bound (Scalar_Range (Standard_Integer)))
                     then
                        Gen_Push_Int (Expr_Value (Low), Index);
                        Gen_Sub (Modular        => False,
                                 Integer_Type   => True,
                                 Overflow_Check => True);

                        Gen_Push_Int (Uint_1);
                        Gen_Add (Modular        => False,
                                 Integer_Type   => True,
                                 Overflow_Check => True);

                     --  Case 3: General case

                     else
                        Gen_Push_Int (Expr_Value (Low) - 1, Index);
                        Gen_Sub (Modular        => False,
                                 Integer_Type   => True,
                                 Overflow_Check => True);
                     end if;

                  else
                     Evaluate_Bound (Low, Obj_LV);
                     Gen_Sub (Modular        => False,
                              Integer_Type   => True,
                              Overflow_Check => True);

                     Gen_Push_Int (Uint_1);

                     Gen_Add (Modular        => False,
                              Integer_Type   => True,
                              Overflow_Check => True);
                  end if;

                  --  We have to account for the possibility of array indexes
                  --  that are "super-null" ranges ('Last - 'First + 1 is
                  --  negative), in which case we still want to produce a zero
                  --  length result. For that case, the following generates a
                  --  conditional that will replace the computed value with
                  --  zero.

                  Suppress_Stack_Checking (Check_State);

                  Gen_Duplicate;
                  Gen_Branch_Greater_Equal (Lbl);
                  Gen_Pop;
                  Gen_Push_Int (Uint_0);
                  Gen_Label (Lbl);

                  Restore_Stack_Checking (Check_State);
               end;
            end if;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Load_Index_Length;

   ----------------------------
   -- Store_Elementary_Value --
   ----------------------------

   procedure Store_Elementary_Value (Target : Address_Descriptor) is
   begin
      case Target.Addr_Kind is
         when No_Address =>
            pragma Assert (False);
            raise Program_Error;

         when Local_Address =>
            Gen_Store_Local (Target.Local_Var);

         when Field_Address =>
            Gen_Put_Field (Target.Field);

         when Indexed_Address =>
            Gen_Store_Array_Element;

         when Array_Address =>
            --  Store the value in the field "all"

            if Target.Descriptor_Class /= Null_Class then
               Gen_Put_Field
                 (Descriptor_Field (Type_Of (Target.Descriptor_Class)));
            end if;

         when Valuetype_Address =>
            Gen_Store_Valuetype (Target.EType);

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Store_Elementary_Value;

   --------------------
   -- Test_For_Slice --
   --------------------

   procedure Test_For_Slice
     (N     : Node_Id;
      Slice : out Boolean;
      Prfix : out Node_Id;
      Subt  : out Entity_Id)
   is
   begin
      if Nkind (N) = N_Slice then
         Slice := True;
         Prfix := Prefix (N);
         Subt  := Etype (N);

      elsif Nkind_In (N, N_Type_Conversion,
                         N_Unchecked_Type_Conversion,
                         N_Qualified_Expression)
      then
         Test_For_Slice (Expression (N), Slice, Prfix, Subt);

      elsif Is_Entity_Name (N)
        and then Present (Renamed_Object (Entity (N)))
      then
         Test_For_Slice (Renamed_Object (Entity (N)), Slice, Prfix, Subt);

      else
         Slice := False;
         Prfix := N;
         Subt  := Etype (N);
      end if;
   end Test_For_Slice;

end Jx_Ch4;
