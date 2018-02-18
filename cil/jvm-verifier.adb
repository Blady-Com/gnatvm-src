------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         J V M . V E R I F I E R                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2010-2013, AdaCore                     --
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

with Ada.Text_IO; use Ada.Text_IO;
with Atree;       use Atree;
with Einfo;       use Einfo;
with J_String;    use J_String;
with JVM.API;     use JVM.API;
with JVM.Code;    use JVM.Code;
with JVM.Dbg;     use JVM.Dbg;
with JVM_File;    use JVM_File;
with JVM.Info;    use JVM.Info;
with JVM.Map;     use JVM.Map;
with JVM.Pool;    use JVM.Pool;
with JVM.Stack;   use JVM.Stack;
with Sinfo;       use Sinfo;
with Unchecked_Deallocation;

package body JVM.Verifier is
   Stack_Size          : constant := 50;
   Stack               : Op_Stack_Id;
   --  Note: Must not invoke here to New_Stack to avoid circularity
   --  problems.

   Array_Type          : Type_Id      := Null_Type;
   Verifying_Method    : Method_Id    := Null_Method;
   Verifying_Local     : Local_Var_Id := Null_Local_Var;
   GNAT_Verifier_Error : exception;

   ----------------
   -- DF_Analyze --
   ----------------

   procedure DF_Analyze
     (Line  : Natural;
      Instr : Instr_Id);
   --  Subsidiary routine of Verify that performs the symbolic analysis of a
   --  single CIL instruction using Stack.

   procedure DF_Analyze
     (Line  : Natural;
      Instr : Instr_Id)
   is
      type Type_Strictness is (Severe, Medium, Relaxed);
      --  Kind of strictness required to check the type compatibility.
      --    - Severe: type kinds must match.
      --    - Medium: Array_Kind allowed for Class_Kind types.
      --    - Relaxed: Array_Kind allowed for Class_Kind types and
      --      Class_Kind allowed for Array_Kind types.

      procedure Check_Top
        (Stack      : in out Op_Stack_Id;
         Op         : CIL_Operation;
         T          : Type_Id;
         Disp       : Stack_Range := 0;
         Strictness : Type_Strictness := Severe);
      --  This is the basic type compatibility routine. T is the expected type,
      --  imposed by context, and Stack (Top - Disp) is the actual type.

      function Is_AR_Param (LV : Local_Var_Id) return Boolean;
      --  Returns true if LV is an Activation Record parameter

      function Is_AR_Type (Typ : Type_Id) return Boolean;
      --  Returns true if Type is Activation Record type parameter

      procedure Verifier_Error
        (Expected_Typ : Type_Id := Null_Type;
         Stack_Typ    : Type_Id := Null_Type);
      --  This routine is invoked by DF_Analyze in case of critical error.
      --  Used to place a breakpoint for debugging purposes.

      ---------------
      -- Check_Top --
      ---------------

      procedure Check_Top
        (Stack      : in out Op_Stack_Id;
         Op         : CIL_Operation;
         T          : Type_Id;
         Disp       : Stack_Range := 0;
         Strictness : Type_Strictness := Severe)
      is
         T_Kind   : constant JVM_Type_Kind := Type_Kind (T);
         Top_Typ  : constant Type_Id := Top (Stack, Disp);
         Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);

         pragma Unreferenced (Op);
      begin
         if Top_Kind = T_Kind then
            return;

         --  Object (O) allowed always in array-kind context

         elsif T_Kind = Array_Kind
            and then Top_Typ = Java_Lang_Object_Type
         then
            return;

         elsif Strictness > Severe
           and then T_Kind = Class_Kind
           and then Top_Kind = Array_Kind
         then
            return;

         elsif Strictness = Relaxed
           and then T_Kind = Array_Kind
           and then Top_Kind = Class_Kind
         then
            return;
         end if;

         Verifier_Error (T, Top_Typ);
      end Check_Top;

      -----------------
      -- Is_AR_Param --
      -----------------

      function Is_AR_Param (LV : Local_Var_Id) return Boolean is
      begin
         return Is_Param (LV) and then Is_AR_Type (Type_Of (LV));
      end Is_AR_Param;

      ----------------
      -- Is_AR_Type --
      ----------------

      function Is_AR_Type (Typ : Type_Id) return Boolean is
         Type_Name : constant String := Name_String (Name (Typ));

      begin
         return Type_Name'Length > 5
           and then Type_Name (Type_Name'First .. Type_Name'First + 4)
                      =  "__AR_";
      end Is_AR_Type;

      --------------------
      -- Verifier_Error --
      --------------------

      procedure Verifier_Error
        (Expected_Typ : Type_Id := Null_Type;
         Stack_Typ    : Type_Id := Null_Type) is
      begin
         Print_Line;
         Print (">>> GNAT VERIFIER Error: ");
         Print (Name (Verifying_Method));
         Print_Line;

         Print_Method (Verifying_Method);
         Print_Line ("Line: " & Line'Img);

         if Verifying_Local /= Null_Local_Var then
            Print_Local_Var (Verifying_Local);
         end if;

         Print (Line'Img & ":");
         Print_Instruction (Get (Instr));

         if Expected_Typ /= Null_Type then
            Print_Line ("Expected_Typ: ");
            Print_Type (Expected_Typ);

            Print_Line ("Stack_Typ:");
            Print_Type (Stack_Typ);
            Print_Line;
         end if;

         Print_Stack (Stack, 6);

         pragma Assert (False);
         raise GNAT_Verifier_Error;
      end Verifier_Error;

      --  Local variables

      I  : JVM.Code.Instruction;

   --  Start of processing for DF_Analyze

   begin
      I := Get (Instr);

      --  Facilitate location of CIL statements with the debugger

      Instruction_Breakpoint (Verifying_Method, Line);

      --  Source: JVM.Code and jvm.adb

      --  Any_Ref_Type represents a managed pointer
      --  Java_Lang_Object_Type represents an object reference (O)

      case I.Op is

         --  No operation

         when NOP =>
            if I.Annotation = Top_Is_Address then
               --  Check_Top (Stack, I.Op, Int_Type);
               Pop  (Stack);
               Push (Stack, Java_Lang_Object_Type);
            end if;

         --  Breakpoint instruction

         when BREAK =>
            null;

         --  Load argument onto the stack
         --    ... -> ... value

         when LDARG   | LDARG_S |
              LDARG_0 | LDARG_1 | LDARG_2 | LDARG_3 |
              LDLOC   | LDLOC_S |
              LDLOC_0 | LDLOC_1 | LDLOC_2 | LDLOC_3 =>
            Push (Stack, Type_Of (I.Local));

         --  Store a value in an argument slot
         --    ... value -> ...

         when STARG   | STARG_S |
              STLOC   | STLOC_S |
              STLOC_0 | STLOC_1 | STLOC_2 | STLOC_3 =>
            Check_Top (Stack, I.Op, Type_Of (I.Local),
              Strictness => Relaxed);
            Pop (Stack);

         --  Load argument address
         --    ... -> ... address (managed pointer)

         when LDARGA  | LDARGA_S |
              LDLOCA  | LDLOCA_S |
              LDNULL =>
            Push (Stack, Any_Ref_Type);

         --  Load numeric constant
         --    ... -> ... num (int32)

         when LDC_I4   | LDC_I4_M1 |
              LDC_I4_0 | LDC_I4_1 | LDC_I4_2 | LDC_I4_3 | LDC_I4_4 |
              LDC_I4_5 | LDC_I4_6 | LDC_I4_7 | LDC_I4_8 | LDC_I4_S =>
            Push (Stack, Int_Type);

         --  Load numeric constant
         --    ... -> ... num (int64)

         when LDC_I8 =>
            Push (Stack, Long_Type);

         --  Load numeric constant
         --    ... -> ... num (float32)

         when LDC_R4 =>
            Push (Stack, Float_Type);

         --  Load numeric constant
         --    ... -> ... num (float64)

         when LDC_R8 =>
            Push (Stack, Double_Type);

         --  Duplicate the top value of the stack
         --    ... value -> ... value value

         when DUP =>
            Push (Stack, Top (Stack));

         --  Remove the top element of the stack
         --    ... value -> ...

         when POP =>
            Pop (Stack);

         --  Jump to method
         --    ... -> ...

         when JMP =>
            null;

         --  CALL: method call
         --    ... arg1, arg2, ... argn -> ... [retval]

         --  CALLI: Indirect method call
         --    ... arg1, arg2, ... argn, ftn -> ... [retval]

         --  CALLVIRT: call a method associated with a method
         --    ... obj, arg1, arg2, ... argn -> ... [retval]

         when CALL | CALLI | CALLVIRT =>

            --  Warning: CALLI has an additional argument in the stack.
            --  The pointer to the invoked function.

            declare
               Method : constant Method_Id := Ref_Method (I.Pool_Item);

               Actual_Type   : Type_Id;
               Ada_Ent       : Entity_Id;
               Expected_Type : Type_Id;
               LV            : Local_Var_Id;
               N_Args        : Natural := 0;
               N_Args_C      : Natural := 0;
               Disp          : Natural := 0;
               Stack_Pos     : Stack_Range;

            begin
               --  Count number of arguments

               LV := First_Local_Var (Method);
               while LV /= Null_Local_Var
                 and then Is_Param (LV)
               loop
                  N_Args := N_Args + 1;
                  LV     := Next_Local_Var (LV);
               end loop;

               Verifying_Local := First_Local_Var (Method);

               --  Check "this" argument in dispatching calls

               if I.Op = CALLVIRT then
                  pragma Assert (Type_Kind (Type_Of (Verifying_Local))
                                  = Type_Kind (Java_Lang_Object_Type));

                  Expected_Type := Type_Of (Verifying_Local);
                  Actual_Type   := Top (Stack, Stack_Range (N_Args - 1));

                  if Is_AR_Param (Verifying_Local)
                    and then Is_AR_Type (Actual_Type)
                  then
                     null;

                  elsif Actual_Type /= Expected_Type
                    and then not
                      Is_Parent_Class
                        (Class_Of_Type (Expected_Type),
                         Class_Of_Type (Actual_Type))
                  then
                     Verifier_Error (Expected_Type, Actual_Type);
                  end if;
               end if;

               --  Handle arguments displacement in indirect calls

               if I.Op = CALLI then
                  Disp := 1;
               end if;

               LV := First_Local_Var (Method);
               for J in 1 .. N_Args loop
                  Verifying_Local := LV;

                  Stack_Pos     := Stack_Range (N_Args + Disp - N_Args_C - 1);
                  Expected_Type := Type_Of (LV);
                  Actual_Type   := Top (Stack, Stack_Pos);

                  --  Enforce type check strictnesss for methods that
                  --  correspond with Ada sources.

                  if Name_String (Name (Method)) /= "Invoke"
                    and then Type_Kind (Expected_Type) = Class_Kind
                  then
                     begin
                        --  Ada_Ent := Ada_Entity (LV);
                        Ada_Ent := Ada_Entity (Type_Of (LV));

                        --  Tagged types require exact type match!

                        pragma Assert (Present (Ada_Ent)
                          or else Is_API_Class (Class_Of_Type (Expected_Type))
                          or else Name_String (Name (Method)) = "_deep_copy"
                          or else (Name_String (Name (Method)) = ".ctor"
                                    and then Name_String (Name (LV)) = "$this")
                          or else Is_AR_Param (LV));

                        --  Handle special cases

                        if Name_String (Name (Method)) = ".ctor"
                          and then Name_String (Name (LV)) = "$this"
                        then
                           if Actual_Type /= Expected_Type
                             and then not
                               Is_Parent_Class
                                 (Class_Of_Type (Expected_Type),
                                  Class_Of_Type (Actual_Type))
                           then
                              Verifier_Error (Expected_Type, Actual_Type);
                           end if;

                        elsif Name_String (Name (Method)) = "_deep_copy" then

                           --  Deep copy may be invoked with "null" in its
                           --  actuals (see routines Generate_Deep_Clone and
                           --  Assign_Record_Value when Source)

                           if Actual_Type = Any_Ref_Type then
                              null;

                           elsif Actual_Type /= Expected_Type
                             and then not
                               Is_Parent_Class
                                 (Class_Of_Type (Expected_Type),
                                  Class_Of_Type (Actual_Type))
                           then
                              Verifier_Error (Expected_Type, Actual_Type);
                           end if;

                        elsif Present (Ada_Ent)
                          and then Is_Tagged_Type (Etype (Ada_Ent))
                        then
                           if Actual_Type /= Expected_Type
                             and then not
                               Is_Parent_Class
                                 (Class_Of_Type (Expected_Type),
                                  Class_Of_Type (Actual_Type))
                           then
                              Verifier_Error (Expected_Type, Actual_Type);
                           end if;

                        else
                           Check_Top
                             (Stack     => Stack,
                              Op        => I.Op,
                              T         => Expected_Type,
                              Disp      => Stack_Pos,
                              Strictness => Relaxed);
                        end if;
                     end;

                  --  other types only require type-kind match

                  else
                     Check_Top
                       (Stack      => Stack,
                        Op         => I.Op,
                        T          => Expected_Type,
                        Disp       => Stack_Pos,
                        Strictness => Relaxed);
                  end if;

                  N_Args_C := N_Args_C + 1;
                  LV       := Next_Local_Var (LV);
               end loop;

               Verifying_Local := Null_Local_Var;

               Pop (Stack, Stack_Range (N_Args));

               if Result_Type (Method) /= Void_Type then
                  Push (Stack, Result_Type (Method));
               end if;

            end;

         --  Return from method

         when RET =>
            if Result_Type (Verifying_Method) /= Void_Type then
               declare
                  Top_Typ  : constant Type_Id := Top (Stack);
                  Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);
                  Res_Typ  : constant Type_Id := Result_Type
                                                   (Verifying_Method);
                  Res_Kind : constant JVM_Type_Kind := Type_Kind (Res_Typ);

               begin
                  if Top_Kind = Array_Kind
                    and then Res_Kind = Class_Kind
                  then
                     null;

                  elsif Res_Kind = Array_Kind
                    and then Top_Kind = Class_Kind
                  then
                     null;

                  --  The types must match!

                  elsif Top_Kind /= Res_Kind then
                     Verifier_Error;
                  end if;

                  Pop (Stack);
               end;
            end if;

         when BR_S | BR =>
            null;

         --  Branch on false, null, or zero
         --    ... value -> ...
         --  type int32, int64, object reference, managed pointer,
         --  unmanaged pointer or native int.

         when BRTRUE  | BRTRUE_S  |
              BRFALSE | BRFALSE_S =>
            declare
               Top_Typ  : constant Type_Id := Top (Stack);
               Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);

            begin
               if Top_Kind /= Int_Kind
                 and then Top_Kind /= Long_Kind
                 and then Top_Kind /= Double_Kind

                  --  Object references
                 and then Top_Kind /= Array_Kind
                 and then Top_Kind /= Class_Kind
                  --  Class_Kind covers Any_Ref_Type
               then
                  Verifier_Error;
               end if;

               Pop (Stack);
            end;

         --  Branch on condition
         --    ... value value -> ...

         when BEQ_S    | BGE_S    | BGT_S    | BLE_S    | BLT_S    |
              BNE_UN_S | BGE_UN_S | BGT_UN_S | BLE_UN_S | BLT_UN_S |
              BEQ      | BGE      | BGT      | BLE      | BLT      |
              BNE_UN   | BGE_UN   | BGT_UN   | BLE_UN   | BLT_UN   =>
            declare
               Top_Typ_1  : constant Type_Id := Top (Stack, 1);
               Top_Kind_1 : constant JVM_Type_Kind := Type_Kind (Top_Typ_1);
               Top_Typ_0  : constant Type_Id := Top (Stack);
               Top_Kind_0 : constant JVM_Type_Kind := Type_Kind (Top_Typ_0);

            begin
               --  Handle Partition III CIL, Section 1.5, Table 4, Page 12
               --  "Binary comparison or branch operations"

               if Top_Kind_1 /= Int_Kind
                 and then Top_Kind_1 /= Long_Kind
                 and then Top_Kind_1 /= Float_Kind
                 and then Top_Kind_1 /= Double_Kind

                  --  Object references
                 and then Top_Kind_1 /= Array_Kind
                 and then Top_Kind_1 /= Class_Kind
                  --  Class_Kind covers Any_Ref_Type
               then
                  Verifier_Error;

               elsif Top_Kind_0 /= Int_Kind
                 and then Top_Kind_0 /= Long_Kind
                 and then Top_Kind_0 /= Float_Kind
                 and then Top_Kind_0 /= Double_Kind

                  --  Object references
                 and then Top_Kind_0 /= Array_Kind
                 and then Top_Kind_0 /= Class_Kind
                  --  Class_Kind covers Any_Ref_Type
               then
                  Verifier_Error;

               --  BEQ_UN not handled because it is not defined in
               --  the CIL_Operation type

               elsif Top_Typ_1 = Any_Ref_Type
                 and then Top_Kind_0 = Int_Kind
                 and then (I.Op = BEQ or else I.Op = BEQ_S)
               then
                  null;

               --  BEQ_UN not handled because it is not defined in
               --  the CIL_Operation type

               elsif Top_Kind_1 = Int_Kind
                 and then Top_Typ_0 = Any_Ref_Type
                 and then (I.Op = BEQ or else I.Op = BEQ_S)
               then
                  null;

               --  Object references

               elsif Top_Kind_1 = Array_Kind
                 and then Top_Kind_0 = Class_Kind
               then
                  null;

               elsif Top_Kind_0 = Array_Kind
                 and then Top_Kind_1 = Class_Kind
               then
                  null;

               --  The types must match!

               elsif Top_Kind_0 /= Top_Kind_1 then
                  Verifier_Error;
               end if;

               Pop (Stack);
               Pop (Stack);
            end;

         --  Table switch on value
         --    ... value -> ...

         when SWITCH =>
            Check_Top (Stack, I.Op, Int_Type);
            Pop (Stack);

         --  Load value indirect onto the stack
         --    ... addr -> ... value

         --  add: unmanaged pointer, native int, or managed pointer, &

         when LDIND_I  | LDIND_I1 | LDIND_I2 | LDIND_I4 |
              LDIND_U1 | LDIND_U2 | LDIND_U4 |
              LDIND_I8 |
              LDIND_R4 | LDIND_R8 |
              LDIND_REF =>
            declare
               Top_Typ  : constant Type_Id := Top (Stack);
               Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);

            begin
               if Top_Kind /= Int_Kind
                 and then Top_Typ /= Any_Ref_Type
               then
                  Verifier_Error;
               end if;

               Pop (Stack);

               case I.Op is
                  when LDIND_I8 =>
                     Push (Stack, Long_Type);
                  when LDIND_R4 =>
                     Push (Stack, Float_Type);
                  when LDIND_R8 =>
                     Push (Stack, Double_Type);
                  when LDIND_REF =>
                     Push (Stack, Java_Lang_Object_Type);
                  when others =>
                     Push (Stack, Int_Type);
               end case;
            end;

         --  Store value indirect from stack
         --    ... addr, val -> ... value

         --  add: unmanaged pointer, native int, or managed pointer, &

         when STIND_I  | STIND_I1 | STIND_I2 | STIND_I4 |
              STIND_I8 |
              STIND_R4 | STIND_R8 |
              STIND_REF =>

            declare
               Top_Typ_1  : constant Type_Id := Top (Stack, 1);
               Top_Kind_1 : constant JVM_Type_Kind := Type_Kind (Top_Typ_1);
               Top_Typ    : constant Type_Id := Top (Stack);
               Top_Kind   : constant JVM_Type_Kind := Type_Kind (Top_Typ);

            begin
               if Top_Kind_1 /= Int_Kind
                 and then Top_Typ_1 /= Any_Ref_Type
               then
                  Verifier_Error;
               end if;

               Pop (Stack);

               case I.Op is
                  when LDIND_I8 =>
                     Check_Top (Stack, I.Op, Long_Type);

                  when LDIND_R4 =>
                     Check_Top (Stack, I.Op, Float_Type);

                  when LDIND_R8 =>
                     Check_Top (Stack, I.Op, Double_Type);

                  when STIND_REF =>
                     if Top_Kind /= Array_Kind
                       and then Top_Kind /= Class_Kind
                     then
                        Verifier_Error;
                     end if;

                  when others =>
                     Check_Top (Stack, I.Op, Int_Type);
               end case;

               Pop (Stack);
               Pop (Stack);
            end;

         --  Numeric value operators
         --    ... value value -> ... value

         when ADD | ADD_OVF | ADD_OVF_UN |
              SUB | SUB_OVF | SUB_OVF_UN |
              MUL | MUL_OVF | MUL_OVF_UN |
              DIV | DIV_UN  | REM_k      | REM_UN =>
            declare
               Top_Typ_1  : constant Type_Id := Top (Stack, 1);
               Top_Kind_1 : constant JVM_Type_Kind := Type_Kind (Top_Typ_1);
               Top_Typ_0  : constant Type_Id := Top (Stack);
               Top_Kind_0 : constant JVM_Type_Kind := Type_Kind (Top_Typ_0);

            begin
               --  Handle Partition III CIL, Section 1.5, Table 2, Page 11
               --  "Binary numeric operations"

               if Top_Kind_1 = Int_Kind
                 and then Top_Typ_0 = Any_Ref_Type
                 and then I.Op = ADD
               then
                  Pop (Stack);
                  Pop (Stack);
                  Push (Stack, Any_Ref_Type);

               elsif Top_Typ_1 = Any_Ref_Type
                 and then Top_Kind_0 = Int_Kind
                 and then (I.Op = ADD or else I.Op = SUB)
               then
                  Pop (Stack);
                  Pop (Stack);
                  Push (Stack, Any_Ref_Type);

               elsif Top_Typ_1 = Any_Ref_Type
                 and then Top_Typ_0 = Any_Ref_Type
                 and then I.Op = SUB
               then
                  Pop (Stack);
                  Pop (Stack);
                  Push (Stack, Int_Type);

               --  Both operands must match!

               else
                  Check_Top (Stack, I.Op, Top (Stack), Disp => 1);
                  Pop (Stack);
               end if;
            end;

         --  Bitwise operators
         --    ... value value -> ... value

         when and_k | or_k | xor_k =>
            declare
               Top_Typ_1  : constant Type_Id := Top (Stack, 1);
               Top_Kind_1 : constant JVM_Type_Kind := Type_Kind (Top_Typ_1);
               Top_Typ_0  : constant Type_Id := Top (Stack);
               Top_Kind_0 : constant JVM_Type_Kind := Type_Kind (Top_Typ_0);

            begin
               --  Handle Partition III CIL, Section 1.5, Table 5, Page 12
               --  "Integer Operations"

               if Top_Kind_1 = Int_Kind
                 and then Top_Kind_0 = Int_Kind
               then
                  null;

               elsif Top_Kind_1 = Long_Kind
                 and then Top_Kind_0 = Long_Kind
               then
                  null;

               else
                  Verifier_Error;
               end if;

               Pop (Stack);
            end;

         --  Shift operators
         --    ... value shiftAmount -> ... value

         when SHL | SHR | SHR_UN =>
            declare
               Top_Typ_1  : constant Type_Id := Top (Stack, 1);
               Top_Kind_1 : constant JVM_Type_Kind := Type_Kind (Top_Typ_1);
               Top_Typ_0  : constant Type_Id := Top (Stack);
               Top_Kind_0 : constant JVM_Type_Kind := Type_Kind (Top_Typ_0);

            begin
               --  Handle Partition III CIL, Section 1.5, Table 6, Page 13
               --  "Shift operations"

               --  ShiftAmount must be an integer

               if Top_Kind_0 /= Int_Kind then
                  Verifier_Error;
               end if;

               if Top_Kind_1 = Int_Kind then
                  Pop (Stack);
                  Pop (Stack);
                  Push (Stack, Int_Type);

               elsif Top_Kind_1 = Long_Kind then
                  Pop (Stack);
                  Pop (Stack);
                  Push (Stack, Long_Type);

               else
                  Verifier_Error;
               end if;
            end;

         --  Negate
         --    ... value -> ... value

         when NEG =>
            declare
               Top_Typ  : constant Type_Id := Top (Stack);
               Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);

            begin
               --  Handle Partition III CIL, Section 1.5, Table 3, Page 11
               --  "Unary numeric operations"

               if Top_Kind /= Int_Kind
                 and then Top_Kind /= Long_Kind
                 and then Top_Kind /= Float_Kind
                 and then Top_Kind /= Double_Kind
               then
                  Verifier_Error;
               end if;
            end;

         --  Bitwise complement
         --    ... value -> ... result

         when not_k =>
            declare
               Top_Typ  : constant Type_Id := Top (Stack);
               Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);

            begin
               --  Handle Partition III CIL, Section 1.5, Table 5, Page 12
               --  "Integer Operations"

               if Top_Kind /= Int_Kind
                 and then Top_Kind /= Long_Kind
               then
                  Verifier_Error;
               end if;
            end;

         --  Data conversion
         --    ... value -> ... result

         when CONV_I         |
              CONV_I1        | CONV_I2        | CONV_I4        |

              CONV_OVF_I     |
              CONV_OVF_I1    | CONV_OVF_I2    | CONV_OVF_I4    |

              CONV_OVF_I_UN  |
              CONV_OVF_I1_UN | CONV_OVF_I2_UN | CONV_OVF_I4_UN |

              CONV_U         |
              CONV_U1        | CONV_U2        | CONV_U4        |

              CONV_OVF_U     |
              CONV_OVF_U1    | CONV_OVF_U2    | CONV_OVF_U4    |

              CONV_OVF_U_UN  |
              CONV_OVF_U1_UN | CONV_OVF_U2_UN | CONV_OVF_U4_UN |

              CONV_U8        | CONV_I8        |
              CONV_OVF_I8    | CONV_OVF_U8    |
              CONV_OVF_I8_UN | CONV_OVF_U8_UN =>

            declare
               Top_Typ  : constant Type_Id := Top (Stack);
               Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);

            begin
               --  Handle Partition III CIL, Section 1.5, Table 8, Page 13
               --  "Conversion operations"

               if Top_Kind /= Int_Kind
                 and then Top_Kind /= Long_Kind
                 and then Top_Kind /= Float_Kind
                 and then Top_Kind /= Double_Kind

                  --  Object references
                 and then Top_Kind /= Array_Kind
                 and then Top_Kind /= Class_Kind
                  --  Class_Kind covers Any_Ref_Type
               then
                  Verifier_Error;
               end if;

               Pop (Stack);

               if I.Op = CONV_U8
                 or else I.Op = CONV_I8
                 or else I.Op = CONV_OVF_I8
                 or else I.Op = CONV_OVF_U8
                 or else I.Op = CONV_OVF_I8_UN
                 or else I.Op = CONV_OVF_U8_UN
               then
                  Push (Stack, Long_Type);
               else
                  Push (Stack, Int_Type);
               end if;
            end;

         --  Data conversion
         --    ... value -> ... result

         when CONV_R4 | CONV_R8 =>
            declare
               Top_Typ  : constant Type_Id := Top (Stack);
               Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);

            begin
               --  Handle Partition III CIL, Section 1.5, Table 8, Page 13
               --  "Conversion operations"

               if Top_Kind /= Int_Kind
                 and then Top_Kind /= Long_Kind
                 and then Top_Kind /= Float_Kind
                 and then Top_Kind /= Double_Kind
               then
                  Verifier_Error;
               end if;

               Pop (Stack);

               if I.Op = CONV_R4 then
                  Push (Stack, Float_Type);
               else
                  Push (Stack, Double_Type);
               end if;
            end;

         --  Convert unsigned integer to floating point

         when CONV_R_UN =>
            Check_Top (Stack, I.Op, Int_Type);
            Push (Stack, Float_Type);

         --  Copy a value type
         --    ... destValObj srcValObj -> ...

         when CPOBJ =>
            declare
               Top_Typ_1  : constant Type_Id := Top (Stack, 1);
               Top_Kind_1 : constant JVM_Type_Kind := Type_Kind (Top_Typ_1);
               Top_Typ_0  : constant Type_Id := Top (Stack);
               Top_Kind_0 : constant JVM_Type_Kind := Type_Kind (Top_Typ_0);

            begin
               if Top_Kind_1 /= Int_Kind
                 and then Top_Typ_1 /= Any_Ref_Type
               then
                  Verifier_Error;
               end if;

               if Top_Kind_0 /= Int_Kind
                 and then Top_Typ_0 /= Any_Ref_Type
               then
                  Verifier_Error;
               end if;

               Pop (Stack);
               Pop (Stack);
            end;

         --  Copy value type to the stack
         --    ... addrofValObj -> ... valObj

         when LDOBJ =>
            declare
               Top_Typ  : constant Type_Id := Top (Stack);
               Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);

            begin
               if Top_Kind /= Int_Kind
                 and then Top_Typ /= Any_Ref_Type
               then
                  Verifier_Error;
               end if;

               Pop (Stack);
               Push (Stack, Type_Of (I.Local));
            end;

         --  Load a literal string
         --    ... -> ... string

         when LDSTR =>
            Push (Stack, Array_Type);

         --  Create a new object
         --    ... arg1, arg2, ... argn -> ... obj

         when NEWOBJ =>
            --  Must use the associated pool to know how many
            --  arguments requires its constructor.

            declare
               M : Method_Id;
               L : Local_Var_Id;

            begin
               M := Ref_Method (I.Pool_Item);

               --  From the constructor's point of view, the uninitialized
               --  new object is argument 0. Hence, from the caller point
               --  of view we must skip the first argument.

               L := Next_Local_Var (First_Local_Var (M));

               --  Missing check on the type of each parameter???

               while L /= Null_Local_Var
                 and then Is_Param (L)
               loop
                  Pop (Stack);
                  L := Next_Local_Var (L);
               end loop;

               Push (Stack, Type_Of (First_Local_Var (M)));
            end;

         --  Cast an object to a class
         --    ... obj -> ... obj2

         --  Test if an object is an instance of a class or interface
         --    ... obj -> ... result

         when CASTCLASS | ISINST =>
            declare
               Top_Typ  : constant Type_Id := Top (Stack);
               Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);
               Pool_Tag : constant CP_Tag := Pool_Item_Tag (I.Pool_Item);

            begin
               if Top_Kind /= Array_Kind
                 and then Top_Kind /= Class_Kind
               then
                  Verifier_Error;
               end if;

               Pop (Stack);

               pragma Assert (Pool_Tag = CONSTANT_Class);
               Push (Stack, Ref_Class_Type (I.Pool_Item));
            end;

         --  Convert boxed value type to its raw form
         --    ... obj -> ... ValueTypePtr
         --  ValuetypePtr is a managed pointer

         when UNBOX =>
            declare
               Top_Typ  : constant Type_Id := Top (Stack);
               Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);

            begin
               if Top_Kind /= Array_Kind
                 and then Top_Kind /= Class_Kind
               then
                  Verifier_Error;
               end if;

               Pop (Stack);
               Push (Stack, Any_Ref_Type);
            end;

         --  Throw an exception
         --    ... obj -> ...

         when THROW =>
            Check_Top (Stack, I.Op, Java_Lang_Object_Type);
            Pop (Stack);

         --  Load field of an object
         --    ... obj -> ... value

         --  obj must be an object, a managed pointer, an unmanaged pointer,
         --  or an instance of a value type.

         when LDFLD =>
            declare
               Top_Typ  : constant Type_Id := Top (Stack);
               Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);

            begin
               if Top_Kind /= Array_Kind
                 and then Top_Kind /= Class_Kind
                  --  Class_Kind covers Any_Ref_Type
                 and then Top_Kind /= Int_Kind
               then
                  Verifier_Error;
               end if;

               Pop (Stack);
               Push (Stack, Type_Of (I.Field));
            end;

         --  Load field address
         --    ... obj -> ... address

         --  obj must be an object, a managed pointer, or an unmanaged
         --  pointer.
         --  address is a managed pointer

         when LDFLDA =>
            declare
               Top_Typ  : constant Type_Id := Top (Stack);
               Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);

            begin
               if Top_Kind /= Array_Kind
                 and then Top_Kind /= Class_Kind
                  --  Class_Kind covers Any_Ref_Type
                 and then Top_Kind /= Int_Kind
               then
                  Verifier_Error;
               end if;

               Pop (Stack);

               --  The value returned is a managed pointer unless obj is
               --  an unmanaged pointer.

               if Top_Kind = Int_Kind then
                  Push (Stack, Int_Type);
               else
                  Push (Stack, Any_Ref_Type);
               end if;
            end;

         --  Store into a field of an object
         --    ... obj value -> ...
         --  obj is a managed or unmanaged pointer

         when STFLD =>
            declare
               Top_Typ_1  : constant Type_Id := Top (Stack, 1);
               Top_Kind_1 : constant JVM_Type_Kind := Type_Kind (Top_Typ_1);
               Top_Typ_0  : constant Type_Id := Top (Stack);
               Top_Kind_0 : constant JVM_Type_Kind := Type_Kind (Top_Typ_0);
               Field_Typ  : constant Type_Id := Type_Of (I.Field);
               Field_Kind : constant JVM_Type_Kind := Type_Kind (Field_Typ);

            begin
               if Top_Typ_1 = Any_Ref_Type
                 and then Top_Kind_1 /= Int_Kind
               then
                  Verifier_Error;
               end if;

               --  Object references

               if Top_Kind_0 = Array_Kind
                 and then Field_Kind = Class_Kind
               then
                  null;

               elsif Top_Kind_0 = Class_Kind
                 and then Field_Kind = Array_Kind
               then
                  null;

               --  The types must match!

               elsif Top_Kind_0 /= Field_Kind then
                  Verifier_Error;
               end if;

               Pop (Stack);
               Pop (Stack);
            end;

         --  Load a static field of a class
         --    ... -> ... value

         when LDSFLD =>
            Push (Stack, Type_Of (I.Field));

         --  Load static field address
         --    ... -> ... address
         --  addres is a managed pointer if field refers to a type
         --  whose memory is managed; otherwise it is an unmanaged
         --  pointer.

         when LDSFLDA =>
            --  We represent it always as managed pointer
            Push (Stack, Any_Ref_Type);

         --  Store a static field of a class
         --    ... val -> ...

         when STSFLD =>
            Check_Top (Stack, I.Op, Type_Of (I.Field),
              Strictness => Medium);
            Pop (Stack);

         --  Store a value type from the stack into memory
         --    ... addr valObj -> ...

         --  addr is a pointer of type native int or &

         when STOBJ =>
            declare
               Top_Typ_1  : constant Type_Id := Top (Stack, 1);
               Top_Kind_1 : constant JVM_Type_Kind := Type_Kind (Top_Typ_1);

            begin
               if Top_Kind_1 /= Int_Kind
                 and then Top_Typ_1 /= Any_Ref_Type
               then
                  Verifier_Error;
               end if;

               Check_Top (Stack, I.Op, Type_Of (Ref_Field (I.Pool_Item)));
               Pop (Stack);
               Pop (Stack);
            end;

         --  Convert value type to object reference
         --    ... valueType -> ... obj

         when BOX =>
            Pop (Stack);
            Push (Stack, Java_Lang_Object_Type);

         --  Create a zero-based, one dimensional array
         --    ... numElems -> ... array

         when NEWARR =>
            for J in 1 .. I.Dimensions loop
               Check_Top (Stack, I.Op, Int_Type);
               Pop (Stack);
            end loop;

            --  Reuse the reference type if the element type maches. Note
            --  that the dimensions field is always set to 1 because after
            --  this instruction the verifier does not use this field.

            if Type_Kind (I.Element_Type) = Int_Kind then
               Push (Stack, Array_Type);
            else
               declare
                  New_Arr_Typ : Type_Id;
                  Array_Name  : Name_Id;

               begin
                  Array_Name :=
                    Name (Name_String (JVM.Name (I.Element_Type)) & "[]");
                  New_Arr_Typ :=
                    New_Array_Type (I.Element_Type, 1, Array_Name);
                  Push (Stack, New_Arr_Typ);
               end;
            end if;

         --  Load the length of an array
         --    ... array -> ... length

         when LDLEN =>

            --  Arrays are objects and hence represented by a value of type O
            --  (CIL-Ref, page 106)

            Check_Top (Stack, I.Op, Array_Type,
              Strictness => Relaxed);
            Pop (Stack);
            Push (Stack, Int_Type);

         --  Load the address of an element of an array
         --    ... array index -> ... address

         --  address is a managed pointer

         when LDELEMA =>
            Check_Top (Stack, I.Op, Array_Type, Disp => 1);
            Check_Top (Stack, I.Op, Int_Type);
            Pop (Stack);
            Pop (Stack);
            Push (Stack, Any_Ref_Type);

         --  Load an element of an array
         --    ... array index -> ... value

         when LDELEM_I  | LDELEM_I1 | LDELEM_I2 | LDELEM_I4 |
              LDELEM_U1 | LDELEM_U2 | LDELEM_U4 =>
            Check_Top (Stack, I.Op, Array_Type, Disp => 1);
            Check_Top (Stack, I.Op, Int_Type);
            Pop (Stack);
            Pop (Stack);
            Push (Stack, Int_Type);

         --  Load an element of an array
         --    ... array index -> ... value

         when LDELEM_I8 =>
            Check_Top (Stack, I.Op, Array_Type, Disp => 1);
            Check_Top (Stack, I.Op, Int_Type);
            Pop (Stack);
            Pop (Stack);
            Push (Stack, Long_Type);

         --  Load an element of an array
         --    ... array index -> ... value

         when LDELEM_R4 =>
            Check_Top (Stack, I.Op, Array_Type, Disp => 1);
            Check_Top (Stack, I.Op, Int_Type);
            Pop (Stack);
            Pop (Stack);
            Push (Stack, Float_Type);

         --  Load an element of an array
         --    ... array index -> ... value

         when LDELEM_R8 =>
            Check_Top (Stack, I.Op, Array_Type, Disp => 1);
            Check_Top (Stack, I.Op, Int_Type);
            Pop (Stack);
            Pop (Stack);
            Push (Stack, Double_Type);

         --  Load an element of an array
         --    ... array index -> ... value

         when LDELEM_REF =>
            declare
               Elmt_Type : constant Type_Id := Element_Type (Top (Stack, 1));

            begin
               Check_Top (Stack, I.Op, Array_Type, Disp => 1);
               Check_Top (Stack, I.Op, Int_Type);
               Pop  (Stack);
               Pop  (Stack);

               if I.Element_Type = Null_Type then
                  Push (Stack, Elmt_Type);

               --  Handle subarray references (see Gen_Load_Subarray_Reference)

               else
                  Push (Stack, I.Element_Type);
               end if;
            end;

         --  Store an element of an array
         --    ... array index value -> ... value

         when STELEM_I  | STELEM_I1 | STELEM_I2 | STELEM_I4 =>
            Check_Top (Stack, I.Op, Array_Type, Disp => 2);
            Check_Top (Stack, I.Op, Int_Type,   Disp => 1);
            Check_Top (Stack, I.Op, Int_Type);
            Pop (Stack);
            Pop (Stack);
            Pop (Stack);

         --  Store an element of an array
         --    ... array index value -> ... value

         when STELEM_I8 =>
            Check_Top (Stack, I.Op, Array_Type, Disp => 2);
            Check_Top (Stack, I.Op, Int_Type,   Disp => 1);
            Check_Top (Stack, I.Op, Long_Type);
            Pop (Stack);
            Pop (Stack);
            Pop (Stack);

         --  Store an element of an array
         --    ... array index value -> ... value

         when STELEM_R4 =>
            Check_Top (Stack, I.Op, Array_Type, Disp => 2);
            Check_Top (Stack, I.Op, Int_Type,   Disp => 1);
            Check_Top (Stack, I.Op, Float_Type);
            Pop (Stack);
            Pop (Stack);
            Pop (Stack);

         --  Store an element of an array
         --    ... array index value -> ... value

         when STELEM_R8 =>
            Check_Top (Stack, I.Op, Array_Type, Disp => 2);
            Check_Top (Stack, I.Op, Int_Type,   Disp => 1);
            Check_Top (Stack, I.Op, Double_Type);
            Pop (Stack);
            Pop (Stack);
            Pop (Stack);

         --  Store an element of an array
         --    ... array index value -> ...

         when STELEM_REF =>
            declare
               Top_Typ  : constant Type_Id := Top (Stack);
               Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);

            begin
               Check_Top (Stack, I.Op, Array_Type, Disp => 2);
               Check_Top (Stack, I.Op, Int_Type,   Disp => 1);

               if Top_Kind /= Array_Kind
                 and then Top_Kind /= Class_Kind
               then
                  Verifier_Error;
               end if;

               Pop (Stack);
               Pop (Stack);
               Pop (Stack);
            end;

         --  Load the address out of a typed reference
         --    ... TypedRef -> ... address
         --  address is a managed pointer

         when REFANYVAL =>
            Pop (Stack);
            Push (Stack, Any_Ref_Type);

         --  Check for a finite real number
         --    ... value -> ... value

         when CKFINITE =>
            declare
               Top_Typ  : constant Type_Id := Top (Stack);
               Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);

            begin
               if Top_Kind /= Float_Kind
                 and then Top_Kind /= Double_Kind
               then
                  Verifier_Error;
               end if;
            end;

         --  Exit a protected region of code
         --    ... -> ...

         when LEAVE | LEAVE_S =>
            null;

         --  Comparison operators
         --    ... value value -> ... result

         when CEQ |
              CGT | CGT_UN |
              CLT | CLT_UN =>
            declare
               Top_Typ_1  : constant Type_Id := Top (Stack, 1);
               Top_Kind_1 : constant JVM_Type_Kind := Type_Kind (Top_Typ_1);
               Top_Typ_0  : constant Type_Id := Top (Stack);
               Top_Kind_0 : constant JVM_Type_Kind := Type_Kind (Top_Typ_0);

            begin
               --  Handle Partition III CIL, Section 1.5, Table 4, Page 12
               --  "Binary comparison or branch operations"

               if Top_Kind_1 /= Int_Kind
                 and then Top_Kind_1 /= Long_Kind
                 and then Top_Kind_1 /= Float_Kind
                 and then Top_Kind_1 /= Double_Kind

                  --  Object references
                 and then Top_Kind_1 /= Array_Kind
                 and then Top_Kind_1 /= Class_Kind
                  --  Class_Kind covers Any_Ref_Type
               then
                  Verifier_Error;

               elsif Top_Kind_0 /= Int_Kind
                 and then Top_Kind_0 /= Long_Kind
                 and then Top_Kind_0 /= Float_Kind
                 and then Top_Kind_0 /= Double_Kind

                  --  Object references
                 and then Top_Kind_0 /= Array_Kind
                 and then Top_Kind_0 /= Class_Kind
                  --  Class_Kind covers Any_Ref_Type
               then
                  Verifier_Error;

               elsif Top_Typ_1 = Any_Ref_Type
                 and then Top_Kind_0 = Int_Kind
                 and then I.Op = CEQ
               then
                  null;

               elsif Top_Kind_1 = Int_Kind
                 and then Top_Typ_0 = Any_Ref_Type
                 and then I.Op = CEQ
               then
                  null;

               --  Object references

               elsif Top_Kind_1 = Array_Kind
                 and then Top_Kind_0 = Class_Kind
               then
                  null;

               elsif Top_Kind_0 = Array_Kind
                 and then Top_Kind_1 = Class_Kind
               then
                  null;

               --  The types must match!

               elsif Top_Kind_0 /= Top_Kind_1 then
                  Verifier_Error;
               end if;

               Pop (Stack);
               Pop (Stack);
               Push (Stack, Int_Type);
            end;

         --  Load method pointer
         --    ... -> ... ftn
         --  ftn is an unmanaged pointer type (native int)

         when LDFTN =>
            Push (Stack, Int_Type);

         --  Load a virtual method pointer
         --    ... object -> ... ftn
         --  ftn is an unmanaged pointer type (native int)

         when LDVIRTFTN =>
            Check_Top (Stack, I.Op, Java_Lang_Object_Type);
            Push (Stack, Int_Type);

         --  Allocate space in the local dynamic memory pool
         --    ... size -> ... address
         --  address is a managed pointer

         when LOCALLOC =>
            Check_Top (Stack, I.Op, Int_Type);
            Pop (Stack);
            Push (Stack, Any_Ref_Type);

         --  End filter clause of SEH exception handling
         --    ... value -> ...

         when ENDFILTER =>
            Check_Top (Stack, I.Op, Int_Type);
            Pop (Stack);

         --  Initialize a value type
         --    ... addrofValObj -> ...
         --  addrofValObj is a managed or unmanaged pointer

         when INITOBJ =>
            declare
               Top_Typ  : constant Type_Id := Top (Stack);
               Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Typ);

            begin
               if Top_Kind /= Int_Kind
                 and then Top_Typ /= Any_Ref_Type
               then
                  Verifier_Error;
               end if;

               Pop (Stack);
            end;

         --  Copy data from memory to memory
         --    ..., destaddr, srcaddr, size -> ...

         when CPBLK =>
            declare
               Top_Typ_2 : constant Type_Id := Top (Stack, 2);
               Top_Kind_2 : constant JVM_Type_Kind := Type_Kind (Top_Typ_2);
               Top_Typ_1 : constant Type_Id := Top (Stack, 1);
               Top_Kind_1 : constant JVM_Type_Kind := Type_Kind (Top_Typ_1);

            begin
               if Top_Kind_2 /= Int_Kind
                 and then Top_Typ_2 /= Any_Ref_Type
               then
                  Verifier_Error;
               end if;

               if Top_Kind_1 /= Int_Kind
                 and then Top_Typ_1 /= Any_Ref_Type
               then
                  Verifier_Error;
               end if;

               Check_Top (Stack, I.Op, Int_Type);
               Pop (Stack);
               Pop (Stack);
               Pop (Stack);
            end;

         --  Initialize a block of memory to a value
         --    ..., addr, value, size -> ...

         when INITBLK =>
            declare
               Top_Typ_2  : constant Type_Id := Top (Stack, 2);
               Top_Kind_2 : constant JVM_Type_Kind := Type_Kind (Top_Typ_2);

            begin
               if Top_Kind_2 /= Int_Kind
                 and then Top_Typ_2 /= Any_Ref_Type
               then
                  Verifier_Error;
               end if;

               Check_Top (Stack, I.Op, Int_Type, Disp => 1);
               Check_Top (Stack, I.Op, Int_Type);
               Pop (Stack);
               Pop (Stack);
               Pop (Stack);
            end;

         --  Rethrow the current exception

         when RETHROW =>
            --  No check needed
            null;

         --  Load the size in bytes of a value type
         --    ... -> ... size (4 bytes, unsigned)

         when SIZEOF =>
            Push (Stack, Int_Type);

         --  ******************************************************
         --          Instructions that invoke the runtime
         --  ******************************************************

         --  Calls mgnat.adalib

         when CLASS_CONVERSION =>
            declare
               Target_Class : constant Class_Id := Ref_Class (I.Pool_Item);
            begin
               Pop (Stack);
               Push (Stack, Type_Of (Target_Class));
            end;

         when RND_R8 =>
            --  Check_Top (Stack, I.Op, Double_Type);
            null;

         --  ******************************************************
         --  CIL Instructions that are prefix of other instructions
         --  ******************************************************

         --  Call terminates current method

         when TAIL =>
            null;

         --  Pointer instruction may be unaligned
         --    ... addr -> ... addr

         when UNALIGNED =>
            null;

         --  Pointer reference is volatile
         --    ... addr -> ... addr

         when VOLATILE =>
            null;

         --  *****************************************************
         --  Warning: Instructions currently not generated by GNAT
         --  *****************************************************

         when ARGLIST =>
            pragma Assert (False);
            null;

         --  End the finally or faulty clause of an exception block
         --    ... -> ...

         when ENDFINALLY =>
            null;

         --  Load the runtime representation of a metadata token
         --    ... ptr -> ... typedRef

         when LDTOKEN =>
            pragma Assert (False);
            null;

         --  Push a typed reference on the stack
         --    ... ptr -> ... typedRef

         when MKREFANY =>
            pragma Assert (False);
            null;

         --  Load the type out of a typed reference
         --    ... TypedRef -> ... Type

         when REFANYTYPE =>
            pragma Assert (False);
            null;

         when UNUSED24 |
              UNUSED77 | UNUSED78 |
              UNUSEDA3 | UNUSEDA4 | UNUSEDA5 | UNUSEDA6 | UNUSEDA7 |
              UNUSEDA8 | UNUSEDA9 | UNUSEDAA | UNUSEDAB | UNUSEDAC |
              UNUSEDAD | UNUSEDAE | UNUSEDAF | UNUSEDB0 | UNUSEDB1 |
              UNUSEDB2 |
              UNUSEDBB | UNUSEDBC | UNUSEDBD | UNUSEDBE | UNUSEDBF |
              UNUSEDC0 | UNUSEDC1 |
              UNUSEDC4 | UNUSEDC5 |
              UNUSEDC7 | UNUSEDC8 | UNUSEDC9 | UNUSEDCA | UNUSEDCB |
              UNUSEDCC | UNUSEDCD | UNUSEDCE | UNUSEDCF |

              UNUSED_UNTIL_FE =>
            null;

      end case;
   end DF_Analyze;

   ------------
   -- Verify --
   ------------
   --  Reference: Generate_Code

   procedure Verify (Method : Method_Id) is
      type Line_Table is array (Positive range <>) of Natural;

      type S_R (Num : Natural) is record
         R : Line_Table (1 .. Num);
      end record;

      type S_Ptr is access S_R;
      procedure Free is new Unchecked_Deallocation (S_R, S_Ptr);

      type Jmp_Rec is record
         R         : Integer := 0;     -- Label
         F         : Boolean := False; -- is inconditional jump
         S         : S_Ptr   := null;
      end record;

      Method_Seq : constant Code_Sequence := Method_Code (Method);
      Max_Lines  : constant Natural := Count_Sequence (Method_Seq);
      DF_Table   : array (1 .. Max_Lines) of Boolean;
      Jmp_Table  : array (1 .. Max_Lines) of Jmp_Rec;
      JmpX_Table : array (1 .. Max_Lines) of Jmp_Rec;
      Lbl_Table  : array (1 .. Max_Lines) of Integer;

      -----------------
      -- DF_Traverse --
      -----------------

      procedure df_traverse
        (Line  : Natural;
         Level : Natural);
      --  Data flow traversal

      ------------------
      -- DF_Traversal --
      ------------------

      procedure df_traverse
        (Line  : Natural;
         Level : Natural)
      is
         Saved_Stack : Op_Stack_Id := New_Stack
                                        (Stack_Range (Num_Elements (Stack)));
         Current     : Natural  := 1;
         Instr       : Instr_Id := First (Method_Seq);

      begin
         Copy (Stack, Saved_Stack);

         --  Locate the instruction of the current line

         while Instr /= Null_Instr
           and then Current /= Line
         loop
            Current := Current + 1;
            Instr := Get (Instr).Next;
         end loop;

         while DF_Table (Current) = False         --  not analyzed
           and then JmpX_Table (Current).R = 0    --  no jmp/ret available
           and then JmpX_Table (Current).S = null --  no switch available
           and then Current <= Max_Lines
         loop
            DF_Analyze (Current, Instr);
            DF_Table   (Current) := True; -- analyzed

            Current := Current + 1;
            Instr := Get (Instr).Next;
         end loop;

         if DF_Table (Current) = False           -- not analyzed
           and then JmpX_Table (Current).R = -1  -- Returns/athrow are -1
           and then Current <= Max_Lines
         then
            DF_Analyze (Current, Instr);
            DF_Table (Current) := True;
         end if;

         if DF_Table (Current) = False
           and then JmpX_Table (Current).R > 0  -- jmp/conditional jmp
           and then Current <= Max_Lines
         then
            DF_Analyze (Current, Instr);
            DF_Table (Current) := True;

            if not JmpX_Table (Current).F  --  not inconditional jmp
              and then not DF_Table (Current + 1) -- not analyzed
              and then Current + 1 <= Max_Lines
            then
               df_traverse (Current + 1, Level + 1);
            end if;

            if JmpX_Table (Current).R > 0 --  return is -1
              and then not DF_Table (JmpX_Table (Current).R)
            then
               pragma Assert (JmpX_Table (Current).R <= Max_Lines);
               df_traverse (JmpX_Table (Current).R, Level + 1);
            end if;
         end if;

         if DF_Table (Current) = False
           and then JmpX_Table (Current).S /= null -- switch instruction
           and then Current <= Max_Lines
         then
            --  Analyze the switch instruction

            DF_Analyze (Current, Instr);
            DF_Table (Current) := True;

            --  Traverse all the switch alternatives

            declare
               Line : Natural;

            begin
               for J in 1 .. JmpX_Table (Current).S.Num loop
                  Line := JmpX_Table (Current).S.R (J);

                  if DF_Table (Line) = False then
                     df_traverse (Line, Level + 1);
                  end if;
               end loop;
            end;
         end if;

         Copy (Saved_Stack, Stack);
         Free_Stack (Saved_Stack);

         --  Put (Tab (1 .. 4 * Level + 1));
--       Print_Indent (Level);
--       Put_Line ("DF_Traverse " & Line'Img & " " & Level'Img & " (end)");
--       Print_Line ("----------------------------------------------- (end)");
      end df_traverse;

      function Line_Number (Label_Number : Natural) return Natural;
      --  Search line of label_Number in the Lbl_Table

      function Line_Number (Label_Number : Natural) return Natural is
      begin
         for k in Lbl_Table'Range loop
            if Lbl_Table (k) = Label_Number then
               return k;
            end if;
         end loop;

         pragma Assert (False);
         return 0;
      end Line_Number;

      --  Local variables

      Instr   : Instr_Id := First (Method_Seq);
      Count   : Positive;
      I       : JVM.Code.Instruction;
      Num_Jmp : Natural := 0;

   --  Start of processing for Verify

   begin
      --  Initialize the JVM type that will be used to represent arrays
      --  during the data-flow traversal.

      if Array_Type = Null_Type then
         declare
            Elmt_Type  : constant Type_Id := Int_Type;
            Array_Name : constant Name_Id :=
                           Name (Name_String (JVM.Name (Elmt_Type)) & "[]");
         begin
            --  Set number of dimensions to 1 as it is not checked by
            --  the verifier.

            Array_Type := New_Array_Type (Elmt_Type, Pos_8 (1), Array_Name);
         end;
      end if;

      --  ---------------------------------------------------------------
      --  Start of Verifier
      --  ---------------------------------------------------------------

      if False then
         Put ("Verifying method: ");
         Print (Name (Method));
         Print (": " & Max_Lines'Img & " instructions");
         Print_Line;
      end if;

      --  ------------------------------------------------------

      --  Initialize the data-flow structures

      Lbl_Table  := (others => 0);
      Jmp_Table  := (others => (0, False, null));
      JmpX_Table := (others => (0, False, null));
      DF_Table   := (others => False);
      Stack      := New_Stack (Stack_Size);

      Set_Stack_Method (Stack, Method);

      --  Collect labels in Lbl_Table

      Count := 1;
      Instr := First (Method_Seq);
      while Instr /= Null_Instr loop
         I := Get (Instr);

         if I.Op = NOP
           and then I.Label_Def /= Null_Label
         then
            Lbl_Table (Count) := Label_Number (I.Label_Def);
         end if;

         Count := Count + 1;
         Instr := Get (Instr).Next;
      end loop;

      --  Collect jumps to labels in Jmp_Table

      Count := 1;
      Instr := First (Method_Seq);
      while Instr /= Null_Instr loop
         I := Get (Instr);

         case I.Op is

            --  Conditional jumps

            when BRFALSE_S | BRTRUE_S | BEQ_S    | BGE_S    | BGT_S    |
                 BLE_S     | BLT_S    | BNE_UN_S | BGE_UN_S | BGT_UN_S |
                 BLE_UN_S  | BLT_UN_S |

                 BRFALSE   | BRTRUE   | BEQ      | BGE      | BGT      |
                 BLE       | BLT      | BNE_UN   | BGE_UN   | BGT_UN   |
                 BLE_UN    | BLT_UN   =>
               Jmp_Table (Count).R := Label_Number (I.Target);

               Num_Jmp := Num_Jmp + 1;

            --  Jumps with label

            when BR_S |   BR  | JMP =>
               Jmp_Table (Count).F := True; --  unconditional jmp
               Jmp_Table (Count).R := Label_Number (I.Target);

               Num_Jmp := Num_Jmp + 1;

            --  Jumps without label

            when RET | THROW | RETHROW | LEAVE =>
               Jmp_Table (Count).F := True; --  unconditional jmp
               Jmp_Table (Count).R := -1;

               Num_Jmp := Num_Jmp + 1;

            when SWITCH =>
               declare
                  Switch_Pair : Switch_Pair_Id;
                  Switch_C    : Natural := 1;
                  P           : S_Ptr;

               begin
                  --  Count is initialized to 1 because the default
                  --  alternative is always present.

                  --  Count number of labels

                  Switch_Pair := First_Pair (I.Switch_Pairs);
                  Switch_C := Switch_C + 1;

                  loop
                     Switch_Pair := Next_Pair (Switch_Pair);
                     exit when Switch_Pair = Null_Switch_Pair;
                     Switch_C := Switch_C + 1;
                  end loop;

                  --  Allocate the record to save the labels

                  P := new S_R (Switch_C);

                  --  Fill the record with the labels

                  Switch_Pair := First_Pair (I.Switch_Pairs);
                  P.R (1) :=
                    Line_Number (Label_Number (Match_Label (Switch_Pair)));

                  Switch_C := 1;
                  loop
                     Switch_Pair := Next_Pair (Switch_Pair);
                     exit when Switch_Pair = Null_Switch_Pair;

                     Switch_C := Switch_C + 1;
                     P.R (Switch_C) :=
                       Line_Number (Label_Number (Match_Label (Switch_Pair)));
                  end loop;

                  Switch_C := Switch_C + 1;
                  P.R (Switch_C) :=
                    Line_Number (Label_Number (I.Default_Label));

                  --  Link the record with the data-flow structure

                  Jmp_Table (Count).S := P;
               end;

            when others =>
               null;
         end case;

         Count := Count + 1;
         Instr := Get (Instr).Next;
      end loop;

      --  Unify contents of previous tables in JmpX_Table because
      --  we must convert labels into lines to facilitate the
      --  traversal of the code.

      for j in Jmp_Table'Range loop
         JmpX_Table (j).S := Jmp_Table (j).S;

         if Jmp_Table (j).R /= 0 then
            JmpX_Table (j).F := Jmp_Table (j).F;

            if Jmp_Table (j).R < 0 then --  return
               JmpX_Table (j).R := Jmp_Table (j).R;

            --  Search line of label Jmp_Table (j)

            else
               JmpX_Table (j).R := Line_Number (Jmp_Table (j).R);
            end if;
         end if;
      end loop;

      Verifying_Method := Method;
      df_traverse (Line => 1, Level => 0);
      Verifying_Method := Null_Method;

      --  Set_Stack_Method (Stack, Null_Method);
      pragma Assert (Is_Empty (Stack));

      Free_Stack (Stack);

      --  Free allocated memory for switch structure

      for J in Jmp_Table'Range loop
         if Jmp_Table (J).S /= null then
            Free (Jmp_Table (J).S);
         end if;
      end loop;
   end Verify;

end JVM.Verifier;
