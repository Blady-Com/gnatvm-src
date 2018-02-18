------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         J _ D E S C R I P T O R S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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

with Atree;    use Atree;
with Einfo;    use Einfo;
with JVM.API;  use JVM.API;
with J_String; use J_String;
with J_Types;  use J_Types;
with Jx_Ch3;   use Jx_Ch3;
with Jx_Ch4;   use Jx_Ch4;
with Jx_Decl;  use Jx_Decl;
with Lib;      use Lib;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Uintp;    use Uintp;

package body J_Descriptors is

   ----------------------------
   -- Build_Array_Descriptor --
   ----------------------------

   procedure Build_Array_Descriptor (Typ : Entity_Id) is
      J_Type        : constant Type_Id := JVM_Type (Typ);
      Arr_Ref_Class : Class_Id;
      Arr_Ref_Field : Field_Id;
      Dimensions    : Pos;

      pragma Unreferenced (Arr_Ref_Field);

   begin
      --  If several functions return the same type of unconstrained array type
      --  we avoid generating the array descriptor twice.

      if Descriptor_Type (J_Type) /= Null_Type then
         pragma Assert (False);
         return;
      end if;

      Arr_Ref_Class :=
        New_Class
          (Ada_Ent  => Empty,
           Name     => JVM_Entity_Name (Typ),
           Pkg_Name => Package_Name (Typ));

      Arr_Ref_Field :=
        New_Field (Arr_Ref_Class, Name ("all"), J_Type, Static => False);

      Arr_Ref_Field :=
        New_Field (Arr_Ref_Class, Name ("first"), Int_Type, Static => False);

      Arr_Ref_Field :=
        New_Field (Arr_Ref_Class, Name ("last"), Int_Type, Static => False);

      --  For multidimensional arrays, create additional fields for each
      --  dimension's upper and lower bound.

      Dimensions := Number_Dimensions (Typ);
      while Dimensions > 1 loop
         Arr_Ref_Field :=
           New_Field
             (Arr_Ref_Class, Name ("first_" & Image (Int_8 (Dimensions))),
              Int_Type, Static => False);

         Arr_Ref_Field :=
           New_Field
             (Arr_Ref_Class, Name ("last_" & Image (Int_8 (Dimensions))),
              Int_Type, Static => False);

         Dimensions := Dimensions - 1;
      end loop;

      --  Link the JVM type of Typ with the new class and define the new class
      --  as an array descriptor.

      Set_Descriptor_Type (J_Type, Type_Of (Arr_Ref_Class));
      Set_Is_Array_Descriptor (Type_Of (Arr_Ref_Class));

      --  Generate the class file of this descriptor. This is only done if this
      --  descriptor corresponds with an entity defined in the main compilation
      --  unit to avoid duplicating the generation of descriptors associated
      --  with withed units.

      if Entity_Is_In_Main_Unit (Typ)
        and then not Is_Built (Arr_Ref_Class)
      then
         Begin_Class_File             (Arr_Ref_Class);
         Generate_Default_Constructor (Arr_Ref_Class);
         End_Class_File               (Arr_Ref_Class);
      end if;
   end Build_Array_Descriptor;

   --------------------------------
   -- Deep_Copy_Array_Descriptor --
   --------------------------------

   procedure Deep_Copy_Array_Descriptor (Expr : Node_Id) is
      Exp_Type    : constant Entity_Id := Underlying_Type (Etype (Expr));
      Saved_Top   : constant Type_Id   := Top_Type;
      LV_Arr      : Local_Var_Id;
      LV_Descr    : Local_Var_Id;
      LV_Arrlen   : Local_Var_Id;

   begin
      pragma Assert (Is_Array_Descriptor (Top_Type));

      --  Copy descriptor

      LV_Descr := New_Local_Var ("_des_tmp", Top_Type);
      Gen_Duplicate;
      Gen_Store_Local (LV_Descr);

      --  Copy the reference to the array contents

      LV_Arr := New_Local_Var ("_arr_tmp", JVM_Type (Exp_Type));
      Gen_Get_Field (Descriptor_Field (Top_Type));
      Gen_Duplicate;
      Gen_Store_Local (LV_Arr);

      --  Save the length of the array

      LV_Arrlen := New_Local_Var ("_arr_len", Int_Type);
      Gen_Array_Length;
      Gen_Store_Local (LV_Arrlen);

      --  For unidimensional arrays generate:

      --    Arr_LV :=
      --      System_array_copy
      --        (src       => LV_Arr,
      --         srcOffset => 0,
      --         dst       => new_array (LV_Arr'length),
      --         dstOffset => 0,
      --         length    => LV_Arr'length);

      if Number_Dimensions (Exp_Type) = 1 then
         Gen_Load_Local (LV_Arr);                 --  src
         Gen_Push_Int   (Uint_0);                 --  srcOffset
         Gen_Load_Local (LV_Arrlen);
         Gen_New_Array  (Expr);                   --  dst

         Gen_Duplicate;
         Gen_Store_Local (LV_Arr);

         Gen_Push_Int (Uint_0);                   --  dstOffset
         Gen_Load_Local (LV_Arrlen);              --  length
         Gen_Invoke_API_Method (System_arraycopy);

      --  For multidimensional arrays generate:

      --    Arr_LV :=
      --      Deep_Copy
      --        (Target   => null,
      --         TrgStart => 0,
      --         Source   => LV_Arr,
      --         SrcCount => LV_Arr'length,
      --         SrcStart => 0)

      else
         Gen_Push_Null;                           --  Target
         Gen_Push_Int (Uint_0);                   --  TrgStart

         Gen_Load_Local (LV_Arr);                 --  Source
         Gen_Load_Local (LV_Arrlen);              --  SrcCount
         Gen_Push_Int (Uint_0);                   --  SrcStart

         Gen_Invoke_Deep_Copy (Full_Type (Exp_Type));
         Gen_Store_Local (LV_Arr);
      end if;

      --  Save the reference to the copy of the array in the descriptor

      Gen_Load_Local (LV_Descr);
      Gen_Load_Local (LV_Arr);
      Gen_Put_Field (Field (Class_Of_Type (Saved_Top), "all"));

      --  Leave the reference to the array descriptor on the stack

      Gen_Load_Local (LV_Descr);
   end Deep_Copy_Array_Descriptor;

   ----------------------
   -- Descriptor_Field --
   ----------------------

   function Descriptor_Field (JVM_Type : Type_Id) return Field_Id is
   begin
      return Field (Class_Of_Type (JVM_Type), "all");
   end Descriptor_Field;

   function Descriptor_Field (Obj_Or_Type : Entity_Id) return Field_Id is
   begin
      return Descriptor_Field (Descriptor_Type (Obj_Or_Type));
   end Descriptor_Field;

   ---------------------
   -- Descriptor_Type --
   ---------------------

   function Descriptor_Type (Obj_Or_Type : Entity_Id) return Type_Id is
      J_Typ : Type_Id;

   begin
      if Is_Type (Obj_Or_Type) then
         pragma Assert (Ekind (Full_Type (Obj_Or_Type)) in Wrappable_Kind);

         if Ekind (Full_Type (Obj_Or_Type)) in Access_Kind then
            return Type_Of (API_Class (Ada_Acc));

         else
            J_Typ := Descriptor_Type (JVM_Type (Full_Type (Obj_Or_Type)));

            case JVM.Type_Kind (JVM_Type (Full_Type (Obj_Or_Type))) is
               when Int_Kind =>
                  if JVM_Type (Full_Type (Obj_Or_Type)) = UInt_Type then
                     pragma Assert (J_Typ = Type_Of (API_Class (Ada_UInt)));
                     return Type_Of (API_Class (Ada_UInt));
                  else
                     pragma Assert (J_Typ = Type_Of (API_Class (Ada_Int)));
                     return Type_Of (API_Class (Ada_Int));
                  end if;

               when Long_Kind =>
                  if JVM_Type (Full_Type (Obj_Or_Type)) = ULong_Type then
                     pragma Assert (J_Typ = Type_Of (API_Class (Ada_ULng)));
                     return Type_Of (API_Class (Ada_ULng));
                  else
                     pragma Assert (J_Typ = Type_Of (API_Class (Ada_Lng)));
                     return Type_Of (API_Class (Ada_Lng));
                  end if;

               when JVM.Float_Kind =>
                  pragma Assert (J_Typ = Type_Of (API_Class (Ada_Flt)));
                  return Type_Of (API_Class (Ada_Flt));

               when Double_Kind =>
                  pragma Assert (J_Typ = Type_Of (API_Class (Ada_Dbl)));
                  return Type_Of (API_Class (Ada_Dbl));

               --  The 'others' alternative handles the special case of type
               --  System.Address, which gets mapped to java.lang.Object and
               --  doesn't get caught by the test for Access_Kind above.

               when others =>
                  return Type_Of (API_Class (Ada_Acc));
            end case;
         end if;

      else
         pragma Assert (Needs_Access_Descriptor (Obj_Or_Type));

         --  Recursive call. Note that we don't call JVM_Type here. This would
         --  be okay, but it would be unclean since JVM_Type invokes Descriptor
         --  _Type itself on types, and it's better to avoid the possibility of
         --  infinite recursion altogether in case any change is ever made to
         --  JVM_Type to call Descriptor_Type directly on objects.

         return Descriptor_Type (Etype (Obj_Or_Type));
      end if;
   end Descriptor_Type;

   -------------------------------
   -- Generate_Array_Descriptor --
   -------------------------------

   procedure Generate_Array_Descriptor (Expr : Node_Id) is
      Exp_Type : constant Entity_Id := Underlying_Type (Etype (Expr));

      pragma Assert (Descriptor_Type (JVM_Type (Exp_Type)) /= Null_Type);

      Descr_Class : constant Class_Id :=
                     Class_Of_Type (Descriptor_Type (JVM_Type (Exp_Type)));
      Descr_Typ   : constant Type_Id := Type_Of (Descr_Class);
      LV_Descr    : constant Local_Var_Id :=
                      New_Local_Var ("_desc_tmp", Descr_Typ);
      Dimensions  : constant Pos_8 := Pos_8 (Number_Dimensions (Exp_Type));

      LV_Arr      : Local_Var_Id;
      Idx      : Node_Id;
   begin
      --  Handle standard strings. Needed to handle return statement of an
      --  string formal parameter. For example:

      --    function Self (Str : String) return string is
      --    begin
      --       return Str;
      --    end;

      if not Is_Constrained (Exp_Type)
        or else Exp_Type = Standard_String
        or else Exp_Type = Standard_Wide_String
        or else Exp_Type = Standard_Wide_Wide_String
      then
         Gen_Default_Object (Descr_Class);
         Gen_Store_Local (LV_Descr);

         Load_Array_Bounds (Expr, Pos (Dimensions));

         for D in reverse 2 .. Dimensions loop
            Gen_Load_Local (LV_Descr);
            Gen_Swap;
            Gen_Put_Field (Field (Descr_Class, "last_" & Image (D)));

            Gen_Load_Local (LV_Descr);
            Gen_Swap;
            Gen_Put_Field (Field (Descr_Class, "first_" & Image (D)));
         end loop;

         Gen_Load_Local (LV_Descr);
         Gen_Swap;
         Gen_Put_Field (Field (Descr_Class, "last"));

         Gen_Load_Local (LV_Descr);
         Gen_Swap;
         Gen_Put_Field (Field (Descr_Class, "first"));

         Gen_Load_Local (LV_Descr);
         Gen_Swap;
         Gen_Put_Field (Field (Descr_Class, "all"));

         Gen_Load_Local (LV_Descr);
         return;
      end if;

      LV_Arr := New_Local_Var ("_arr_tmp", JVM_Type (Exp_Type));
      Gen_Store_Local (LV_Arr);

      Gen_Default_Object (Descr_Class);
      Gen_Store_Local (LV_Descr);

      Gen_Load_Local (LV_Descr);
      Gen_Load_Local (LV_Arr);
      Gen_Put_Field  (Field (Descr_Class, "all"));

      Gen_Load_Local (LV_Descr);

      if Ekind (Exp_Type) = E_String_Literal_Subtype then
         Evaluate_Expr
           (String_Literal_Low_Bound (Exp_Type));
      else
         Evaluate_Expr
           (Array_Index_First (First_Index (Exp_Type)));
      end if;

      Add_Implicit_Index_Conversion;
      Gen_Put_Field (Field (Descr_Class, "first"));

      Gen_Load_Local (LV_Descr);

      if Ekind (Exp_Type) = E_String_Literal_Subtype then
         Evaluate_Expr
           (String_Literal_Low_Bound (Exp_Type));
         Gen_Push_Int
           (String_Literal_Length (Exp_Type));
         Gen_Add (Modular => False, Integer_Type => True);
         Gen_Push_Int (Uint_1);
         Gen_Sub (Modular => False, Integer_Type => True);
      else
         Evaluate_Expr
           (Array_Index_Last (First_Index (Exp_Type)));
      end if;

      Add_Implicit_Index_Conversion;
      Gen_Put_Field (Field (Descr_Class, "last"));

      if not Is_String_Type (Exp_Type)
        and then Number_Dimensions (Exp_Type) > 1
      then
         Idx := Next_Index (First_Index (Exp_Type));

         for D in 2 .. Number_Dimensions (Exp_Type) loop
            Gen_Load_Local (LV_Descr);
            Evaluate_Expr (Array_Index_First (Idx));
            Gen_Put_Field
              (Field (Descr_Class, "first_" & Image (Int_8 (D))));

            Gen_Load_Local (LV_Descr);
            Evaluate_Expr (Array_Index_Last (Idx));
            Gen_Put_Field
              (Field (Descr_Class, "last_" & Image (Int_8 (D))));

            Idx := Next_Index (Idx);
         end loop;
      end if;

      --  Leave the reference to the array descriptor on the stack

      Gen_Load_Local (LV_Descr);
   end Generate_Array_Descriptor;

   --------------------------
   -- Is_Access_Descriptor --
   --------------------------

   function Is_Access_Descriptor (Obj_Or_Type : Type_Id) return Boolean is
   begin
      return Obj_Or_Type = Type_Of (API_Class (Ada_Int))
        or else Obj_Or_Type = Type_Of (API_Class (Ada_Lng))
        or else Obj_Or_Type = Type_Of (API_Class (Ada_UInt))
        or else Obj_Or_Type = Type_Of (API_Class (Ada_ULng))
        or else Obj_Or_Type = Type_Of (API_Class (Ada_Flt))
        or else Obj_Or_Type = Type_Of (API_Class (Ada_Dbl))
        or else Obj_Or_Type = Type_Of (API_Class (Ada_Acc));
   end Is_Access_Descriptor;

   -----------------------------
   -- Needs_Access_Descriptor --
   -----------------------------

   function Needs_Access_Descriptor (Ada_Entity : Entity_Id) return Boolean is
   begin
      return (Is_Aliased (Ada_Entity)
        or else Ekind (Ada_Entity) in E_Out_Parameter .. E_In_Out_Parameter)
          and then
            Ekind (Underlying_Type (Etype (Ada_Entity))) in Wrappable_Kind;
   end Needs_Access_Descriptor;

end J_Descriptors;
