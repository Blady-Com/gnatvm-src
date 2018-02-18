------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 5                                --
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

with Ada.Exceptions; use Ada.Exceptions;
with Atree;          use Atree;
with Debug;          use Debug;
with Debug_A;        use Debug_A;
with Einfo;          use Einfo;
with Exp_Util;       use Exp_Util;
with JVM;            use JVM;
with JVM.API;        use JVM.API;
with JVM.Dbg;        use JVM.Dbg;
with JVM.Map;        use JVM.Map;
with J_Descriptors;  use J_Descriptors;
with J_String;       use J_String;
with J_Types;        use J_Types;
with Jx_Ch3;         use Jx_Ch3;
with Jx_Ch4;         use Jx_Ch4;
with Jx_Ch6;         use Jx_Ch6;
with Jx_Ch11;        use Jx_Ch11;
with Jx_Decl;        use Jx_Decl;
with Jx_Drive;       use Jx_Drive;
with Jx_Swtch;       use Jx_Swtch;
with Jx_Uplev;       use Jx_Uplev;
with Nlists;         use Nlists;
with Osint;
with Output;         use Output;
with Sem_Aux;        use Sem_Aux;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinput;         use Sinput;
with Snames;         use Snames;
with Uintp;          use Uintp;

package body Jx_Ch5 is

   procedure Assign_Array_Value (Target, Source : Node_Id);
   --  Generates code to copy the Source array value to Target
   --  (using java.lang.System.arraycopy).

   procedure Assign_Multiarray (Target : Node_Id; Source : Node_Id);
   --  Generates code to copy the highest-dimension subarrays from
   --  the multidimensional Source array to Target.

   procedure Assign_Record_Value (Target, Source : Node_Id);
   --  Generates code to copy Source's fields to Target.

   procedure Generate_Assignment (Assignment_Stmt : Node_Id);
   --  Generates code to evaluate an Ada assignment statement

   procedure Generate_At_End_Calls (Stmt : Node_Id);
   --  Traverse the statements enclosing Stmt to generate calls to all at-end
   --  procedures that are applicable for the given statement. In the case
   --  where Stmt is an N_Handled_Sequence_Of_Statements, generates a call to
   --  the associated At_End_Proc, if any. In the case of an exit statement,
   --  calls are generated to all At_End_Procs intervening between the exit
   --  statement and the loop being exited. In the case of a goto statement,
   --  calls are generated to all At_End_Procs intervening between the goto
   --  statement and the location of the target label. In the case of a return
   --  statement, generates calls to all at-end procedures within the enclosing
   --  subprogram for statements enclosing the point of the return statement.
   --  The only Nkinds supported for Stmt are N_Handled_Sequence_Of_Statement,
   --  N_Exit_Statement, N_Goto_Statement and N_Simple_Return_Statement.

   procedure Generate_Case_Statement (Case_Stmt : Node_Id);
   --  Generates code for an Ada case statement

   procedure Generate_Exit_Statement (Exit_Stmt : Node_Id);
   --  Generates code for a loop exit statement

   procedure Generate_If_Statement (If_Stmt : Node_Id);
   --  Generates code for an if statement

   procedure Generate_Loop_Statement (Loop_Stmt : Node_Id);
   --  Generates code for an Ada loop statement

   procedure Generate_Return (Return_Stmt : Node_Id);
   --  Generates code for a subprogram return statement

   function In_Exception_Handler (Stmt : Node_Id) return Boolean;
   --  Returns True if Stmt is located inside a block that has exception
   --  handlers

   procedure Translate_Case_Alternative
     (Altern : Node_Id;
      Obj    : Local_Var_Id := Null_Local_Var);
   --  Call-through procedure to translate the set of statements associated
   --  with a case statement alternative. This procedure is passed to
   --  Generate_Switch from Generate_Case_Statement. The Obj parameter is
   --  ignored and is only present in order to match the profile of the access
   --  type Switch_Action used by Generate_Switch.

   ------------------------
   -- Assign_Array_Value --
   ------------------------

   procedure Assign_Array_Value (Target, Source : Node_Id) is
      Dimensions    : constant Pos_8 :=
                        Pos_8 (Number_Dimensions (Full_Type (Target)));
      Target_Type   : constant Entity_Id := Full_Type (Etype (Target));
      Source_Prefix : Node_Id;
      Source_Slice  : Boolean;
      Source_Subt   : Entity_Id;
      Target_Prefix : Node_Id;
      Target_Slice  : Boolean;
      Target_Subt   : Entity_Id;

   begin
      --  No copy needed if Source was replaced by the frontend by CE

      if Nkind (Source) = N_Raise_Constraint_Error then
         Evaluate_Expr (Source);
         Gen_Pop;
         return;
      end if;

      Test_For_Slice (Target, Target_Slice, Target_Prefix, Target_Subt);
      Test_For_Slice (Source, Source_Slice, Source_Prefix, Source_Subt);

      if Dimensions > 1 then
         Assign_Multiarray (Target, Source);

      --  One-dimensional arrays with elementary components and VM_By_Copy
      --  actuals of entry calls are copied using java.lang.System.arraycopy

      elsif Number_Dimensions (Target_Type) = 1
        and then
          (Ekind (Full_Type (Component_Type (Target_Type))) in Elementary_Kind
             or else
               (Is_VM_By_Copy_Actual (Target)
                  and then Nkind (Source) = N_Explicit_Dereference
                  and then Nkind (Prefix (Source)) = N_Selected_Component
                  and then Present
                             (Entry_Formal
                               (Entity (Selector_Name (Prefix (Source)))))))
        and then not Has_Aliased_Components (Full_Type ((Target_Type)))
      then
         declare
            Source_LV : Local_Var_Id;
            Target_LV : Local_Var_Id;
            Length_LV : constant Local_Var_Id :=
                          New_Local_Var ("_aav1_len", Int_Type);
            End_Lbl   : constant Label_Id := New_Label;

         begin
            --  Generate:
            --    trg : target_typ := <Evaluate Target>;
            --    len : Int        := trg'length;
            --    src : source_typ := <Evaluate Source>;

            if Target_Slice then
               Evaluate_Expr (Target);
               Load_Index_Length (First_Index (Target_Subt));
            else
               Evaluate_Array_Address (Target);
               pragma Assert (not Is_Array_Descriptor (Top_Type));
               Gen_Duplicate;
               Gen_Array_Length;
            end if;

            Gen_Store_Local (Length_LV);

            if Top_Type = Any_Ref_Type then
               pragma Assert (False);
               Target_LV := New_Local_Var ("_aav1_trg", JVM_Type (Target));
            else
               Target_LV := New_Local_Var ("_aav1_trg", Top_Type);
            end if;

            Gen_Store_Local (Target_LV);

            if Source_Slice then
               Evaluate_Expr (Source);
            else
               Evaluate_Array_Address (Source);
               pragma Assert (not Is_Array_Descriptor (Top_Type));
            end if;

            if Top_Type = Any_Ref_Type then
               Source_LV := New_Local_Var ("_aav1_src", JVM_Type (Source));
            else
               Source_LV := New_Local_Var ("_aav1_src", Top_Type);
            end if;

            Gen_Store_Local (Source_LV);

            --  Generate:

            --    if trg /= null
            --      and then len > 0
            --      and then src /= null
            --    then
            --       arraycopy (src, srcoffset, dest, dstoffset, length)
            --       ...
            --    end if;

            --  The source and target offsets are both zero for the normal
            --  array assignment case. In the case of slice assignments we
            --  compute the starting offset within the containing array by
            --  calling Gen_Array_Subscript. The length of the target array
            --  is used to determine the number of elements to copy.

            Gen_Load_Local (Target_LV);
            Gen_Branch_If_Null (End_Lbl);

            Gen_Load_Local (Length_LV);
            Gen_Branch_Less_Equal (End_Lbl);

            Gen_Load_Local (Source_LV);
            Gen_Branch_If_Null (End_Lbl);

            --  src

            Gen_Load_Local (Source_LV);

            --  srcoffset

            if Source_Slice then
               Gen_Array_Subscript (Source_Prefix, Index_First (Source_Subt));
            else
               Gen_Push_Int (Uint_0);
            end if;

            --  dest

            Gen_Load_Local (Target_LV);

            --  destoffset

            if Target_Slice then
               Gen_Array_Subscript (Target_Prefix, Index_First (Target_Subt));
            else
               Gen_Push_Int (Uint_0);
            end if;

            --  length

            Gen_Load_Local (Length_LV);

            Gen_Invoke_API_Method (System_arraycopy);
            Gen_Label (End_Lbl);
         end;

      --  If the array has composite components or aliased elementary
      --  components, then the array components must be copied by invoking
      --  the array type's deep copy operation.

      else
         declare
            Source_LV : constant Local_Var_Id :=
                          New_Local_Var ("_aav_src", JVM_Type (Source));
            End_Lbl   : constant Label_Id := New_Label;

         begin
            --  Generate:
            --    if source /= null then
            --       _deep_copy (target, 0, source, source'length, 0)
            --    end if;

            if Source_Slice then
               Evaluate_Expr (Source);
            else
               Evaluate_Array_Address (Source);
            end if;

            Gen_Store_Local (Source_LV);

            Gen_Load_Local (Source_LV);
            Gen_Branch_If_Null (End_Lbl);

            --  Evaluate the reference to the target array and push the
            --  starting index for the target (dstOffset).

            if Target_Slice then
               Evaluate_Expr (Target);
               Gen_Array_Subscript (Target_Prefix, Index_First (Target_Subt));
            else
               Evaluate_Array_Address (Target);
               pragma Assert (not Is_Array_Descriptor (Top_Type));
               Gen_Push_Int (Uint_0);
            end if;

            --  Load the array length followed by the starting index of the
            --  evaluated array.

            Gen_Load_Local (Source_LV);

            if Source_Slice then
               Gen_Array_Subscript (Source_Prefix, Index_First (Source_Subt));
               Load_Index_Length (First_Index (Source_Subt));
               Gen_Swap;
            else
               pragma Assert (not Is_Array_Descriptor (Top_Type));
               Gen_Duplicate;
               Gen_Array_Length;
               Gen_Push_Int (Uint_0);
            end if;

            Gen_Invoke_Deep_Copy (Base_Type (Target_Subt));

            --  Pop the resulting reference, since the target can never be
            --  null in this context (so the deep copy cannot have allocated
            --  a new array object that needs to be saved back in the target).

            Gen_Pop;

            Gen_Label (End_Lbl);
         end;
      end if;
   end Assign_Array_Value;

   -----------------------
   -- Assign_Multiarray --
   -----------------------

   procedure Assign_Multiarray (Target : Node_Id; Source : Node_Id) is
      Source_LV : constant Local_Var_Id :=
                    New_Local_Var ("_am_src", JVM_Type (Source));
      Target_LV : constant Local_Var_Id :=
                    New_Local_Var ("_am_targ", JVM_Type (Target));
      End_Lbl   : constant Label_Id := New_Label;

   begin
      --  Evaluate the target and source arrays, plus push the extraneous
      --  parameters currently used for all array deep copy methods (even
      --  though they're only needed for one-dimensional arrays). ???

      Evaluate_Array_Address (Source);
      Gen_Store_Local (Source_LV);

      Evaluate_Array_Address (Target);
      Gen_Store_Local (Target_LV);

      --  Generate:
      --    if source /= null then
      --       _deep_copy (target, 0, source, source'length, 0)
      --    end if;

      Gen_Load_Local (Source_LV);
      Gen_Branch_If_Null (End_Lbl);

      --  Call the array type's deep copy method and pop the result (since
      --  target will never be null in an assignment statement).

      --     _deep_copy (target, 0, source, source'length, 0)

      Gen_Load_Local (Target_LV);
      Gen_Push_Int (Uint_0);

      Gen_Load_Local (Source_LV);
      Gen_Duplicate;
      Gen_Array_Length;
      Gen_Push_Int (Uint_0);

      Gen_Invoke_Deep_Copy (Full_Type (Target));
      Gen_Pop;

      Gen_Label (End_Lbl);
   end Assign_Multiarray;

   -------------------------
   -- Assign_Record_Value --
   -------------------------

   procedure Assign_Record_Value (Target, Source : Node_Id) is
   begin
      --  No copy needed if Source was replaced by the frontend by CE

      if Nkind (Source) = N_Raise_Constraint_Error then
         Evaluate_Expr (Source);
         Gen_Pop;
         return;
      end if;

      --  Evaluate the source and target object references. If Source or
      --  target are N_Raise_Constraint_Error nodes then their evaluation
      --  leaves "null" in the stack.

      Evaluate_Expr (Target);
      Evaluate_Expr (Source);

      --  Invoke the record type's deep copy method

      if Nkind (Source) = N_Unchecked_Type_Conversion then
         Gen_Class_Conversion (JVM_Type (Full_Type (Target)));
      end if;

      Gen_Invoke_Deep_Copy (Full_Type (Target));

      --  Throw away the result (which is simply the value of target passed
      --  in on the call).

      Gen_Pop;
   end Assign_Record_Value;

   -------------------------
   -- Generate_Assignment --
   -------------------------

   procedure Generate_Assignment (Assignment_Stmt : Node_Id) is
      Source : constant Node_Id := Expression (Assignment_Stmt);
      Target : constant Node_Id := Name (Assignment_Stmt);
      Addr   : Address_Descriptor;
      Ignore : Boolean := False;

   begin
      case Ekind (Full_Type (Target)) is
         when Elementary_Kind =>

            --  In case of assignment to an unreferenced entity, we don't
            --  generate the assignment, we just evaluate the right-hand side
            --  for potential side effects.
            --  This makes the verifyer happier ...

            if Nkind (Target) in N_Has_Entity
              and then Has_Pragma_Unreferenced (Entity (Target))
            then
               Ignore := True;
            end if;

            if not Ignore then
               Addr := Evaluate_Addr (Target);
            end if;

            Evaluate_Expr (Source, Check_Subtype => Etype (Target));

            if not Ignore then

               --  ??? this makes me a little nervous, but it makes methods for
               --  valuetypes work correctly (as below)

               if JVM_Type (Target) /= JVM_Type (Source) then
                  declare
                     Tk1, Tk2 : JVM_Type_Kind;
                  begin
                     Tk1 := JVM.Type_Kind (JVM_Type (Target));
                     Tk2 := JVM.Type_Kind (JVM_Type (Source));

                     if (Tk1 = Int_Kind or else Tk1 = Long_Kind)
                       and then (Tk2 = Int_Kind or else Tk2 = Long_Kind)
                     then
                        Gen_Conversion (JVM_Type (Target));
                     else
                        JVM.Pop_Type;
                        Push_Type (JVM_Type (Target));
                     end if;
                  end;
               end if;

               Store_Elementary_Value (Addr);

            else
               Gen_Pop;
            end if;

         when Einfo.Array_Kind =>
            Assign_Array_Value (Target, Source);

         when Einfo.Record_Kind =>

            --  If we have a valuetype, don't do a deep copy

            if Is_Value_Type (Full_Type (Target)) then
               Addr := Evaluate_Addr (Target);
               Evaluate_Expr (Source, Check_Subtype => Etype (Target));
               JVM.Pop_Type;
               Push_Type (JVM_Type (Target));
               Store_Elementary_Value (Addr);

            else
               Assign_Record_Value (Target, Source);
            end if;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Generate_Assignment;

   ---------------------------
   -- Generate_At_End_Calls --
   ---------------------------

   procedure Generate_At_End_Calls (Stmt : Node_Id) is
      At_End_Subp : Entity_Id;
      Parent_Stmt : Node_Id;

   begin
      --  Search for the innermost enclosing handled sequence of statements
      --  with an associated At_End_Proc and generate a call to the procedure.

      Parent_Stmt := Stmt;
      while Present (Parent_Stmt)
        and then Nkind (Parent_Stmt) not in N_Proper_Body
      loop
         if Nkind (Parent_Stmt) = N_Handled_Sequence_Of_Statements
           and then Present (At_End_Proc (Parent_Stmt))
         then
            At_End_Subp := Entity (At_End_Proc (Parent_Stmt));

            --  If this is a call to a nested clean-up method, then we must
            --  pass a static link parameter.

            if Present (Enclosing_Subprogram (At_End_Subp))
              and then not Is_Imported (At_End_Subp)
            then
               Load_Static_Link (Enclosing_Method (At_End_Subp));
            end if;

            Gen_Invoke_Method (JVM_Method (At_End_Subp));
         end if;

         --  Return immediately if the statement passed in is a handled
         --  sequence of statements, since we've generated the call to
         --  its At_End_Proc, if it had one.

         if Nkind (Stmt) = N_Handled_Sequence_Of_Statements then
            return;

         --  For a return statement, the At_End_Procs of all of the
         --  enclosing handled sequences of statements must be executed.

         elsif Nkind (Stmt) = N_Simple_Return_Statement then
            null;  -- Continue looping through all enclosing statements

         --  For an exit statement, we stop the traversal as soon as we
         --  encounter the loop being exited.

         elsif Nkind (Stmt) = N_Exit_Statement then
            if Nkind (Parent_Stmt) = N_Loop_Statement then
               if Present (Name (Stmt)) then
                  if Present (Identifier (Parent_Stmt))
                    and then
                      Entity (Name (Stmt)) = Entity (Identifier (Parent_Stmt))
                  then
                     return;
                  end if;

               --  In the case of an exit statement with no loop name, stop
               --  traversing as soon we encounter the innermost enclosing loop
               --  statement.

               else
                  return;
               end if;
            end if;

         --  Handle goto statements, stopping when we encounter the statement
         --  where the goto label is located.

         elsif Nkind (Stmt) = N_Goto_Statement then

            --  Search for the label in the current list of statements. Note
            --  that instead of searching for the implicit label declaration we
            --  search for the label occurrence. This is done because the
            --  expander declares implicit labels in declaration lists of
            --  enclosing blocks as well as in lists of statements. Hence,
            --  searching for a label's declaration requires traversing two
            --  lists, whereas searching for a label occurrence only requires
            --  traversing a single list.

            if Is_List_Member (Parent_Stmt) then
               declare
                  S : Node_Id;

               begin
                  S := First (List_Containing (Parent_Stmt));
                  while Present (S) loop
                     if Nkind (S) = N_Label
                       and then Entity (Identifier (S)) = Entity (Name (Stmt))
                     then
                        return;
                     end if;

                     Next (S);
                  end loop;
               end;
            end if;
         end if;

         Parent_Stmt := Parent (Parent_Stmt);
      end loop;
   end Generate_At_End_Calls;

   -----------------------------
   -- Generate_Case_Statement --
   -----------------------------

   procedure Generate_Case_Statement (Case_Stmt : Node_Id) is
   begin
      Evaluate_Expr (Expression (Case_Stmt));
      Generate_Switch
        (Alternatives (Case_Stmt), Translate_Case_Alternative'Access);
   end Generate_Case_Statement;

   -----------------------------
   -- Generate_Exit_Statement --
   -----------------------------

   procedure Generate_Exit_Statement (Exit_Stmt : Node_Id) is
      Exit_Cond   : constant Node_Id := Condition (Exit_Stmt);
      Exit_Label  : Label_Id;
      Loop_Id     : Entity_Id := Empty;
      Parent_Stmt : Node_Id   := Parent (Exit_Stmt);

   begin
      if Present (Name (Exit_Stmt)) then
         Loop_Id := Entity (Name (Exit_Stmt));

      --  This is an exit from the innermost enclosing loop. Locate the loop
      --  statement and pick up its loop identifier entity.

      else
         while Present (Parent_Stmt)
           and then Nkind (Parent_Stmt) /= N_Loop_Statement
         loop
            Parent_Stmt := Parent (Parent_Stmt);
         end loop;

         pragma Assert (Nkind (Parent_Stmt) = N_Loop_Statement);

         if Present (Identifier (Parent_Stmt)) then
            Loop_Id := Entity (Identifier (Parent_Stmt));
         end if;
      end if;

      --  If the loop being exited has a loop identifier, then create an exit
      --  label for the loop if it doesn't already have one and associate it
      --  with the loop identifier.

      if Present (Loop_Id) then
         if JVM_Entity (Loop_Id) = Null_Label then
            Set_Map (Loop_Id, New_Label);
         end if;

         Exit_Label := JVM_Entity (Loop_Id);

      --  If there is not a loop id, then associate the enclosing loop
      --  statement node itself with the exit label.

      elsif JVM_Entity (Parent_Stmt) = Null_Label then
         Set_Map (Parent_Stmt, New_Label);
         Exit_Label := JVM_Entity (Parent_Stmt);

      --  Need an exit label from the parent if this is the 2nd exit for the
      --  same parent loop

      else
         Exit_Label := JVM_Entity (Parent_Stmt);
      end if;

      --  Generate calls to the enclosing at-end procedures, if any, before
      --  exiting the pending exception scopes and exiting the loop.

      Generate_At_End_Calls (Exit_Stmt);

      --  Case 1: Exit statement located in a sequence of statements. We
      --  generate a branch to the corresponding label.

      if not In_Exception_Handler (Exit_Stmt) then
         if No (Exit_Cond) then
            Gen_Goto (Exit_Label);

         else
            Evaluate_Expr (Exit_Cond, Exit_Label, True);

            if Exit_Label /= Null_Label then
               Gen_Branch_Not_Equal (Exit_Label);
            end if;
         end if;

      --  Case 2: Exit statement located in the statements of an exception
      --  handler. In these cases the VM does not allow us to generate a
      --  branch; we must generate a leave instruction.

      else
         if No (Exit_Cond) then
            Gen_Leave (Exit_Label);

         else
            declare
               Aux_Label : constant Label_Id := New_Label;
               Dup_Label : Label_Id := Aux_Label;
               --  We save the new label because Evaluate_Expr changes its
               --  value to Null_Label

            begin
               Evaluate_Expr (Exit_Cond, Dup_Label, False);
               pragma Assert (Dup_Label = Null_Label);

               Gen_Leave (Exit_Label);
               Gen_Label (Aux_Label);
            end;
         end if;
      end if;
   end Generate_Exit_Statement;

   ---------------------------
   -- Generate_If_Statement --
   ---------------------------

   procedure Generate_If_Statement (If_Stmt : Node_Id) is
      Else_Stmts  : constant List_Id  := Else_Statements (If_Stmt);
      Elsifs      : constant List_Id  := Elsif_Parts (If_Stmt);
      Exit_Label  : constant Label_Id := New_Label;
      Elsif_Part  : Node_Id;
      False_Label : Label_Id := New_Label;
      Save_Label  : Label_Id := False_Label;

   begin
      Evaluate_Expr (Condition (If_Stmt), Save_Label, False);

      --  No further work needed if the expression was replaced by CE

      if Nkind (Condition (If_Stmt)) = N_Raise_Constraint_Error then
         Gen_Pop;
         return;
      end if;

      --  If Save_Label was unused during the condition evaluation, then
      --  generate a branch to it now based on the Boolean top-of-stack value.

      if Save_Label /= Null_Label then
         Gen_Branch_Equal (False_Label);
      end if;

      Translate_Statements (Then_Statements (If_Stmt));

      if Present (Elsifs) or else Present (Else_Stmts) then
         Gen_Goto (Exit_Label);
      end if;

      if Present (Elsifs) then
         Elsif_Part := First (Elsifs);
         while Present (Elsif_Part) loop
            Gen_Label (False_Label);
            False_Label := New_Label;
            Save_Label  := False_Label;
            Evaluate_Expr (Condition (Elsif_Part), Save_Label, False);

            if Save_Label /= Null_Label then
               Gen_Branch_Equal (False_Label);
            end if;

            Translate_Statements (Then_Statements (Elsif_Part));

            if Present (Next (Elsif_Part)) or else Present (Else_Stmts) then
               Gen_Goto (Exit_Label);
            end if;

            Elsif_Part := Next (Elsif_Part);
         end loop;
      end if;

      Gen_Label (False_Label);

      if Present (Else_Stmts) then
         Translate_Statements (Else_Statements (If_Stmt));
      end if;

      if Present (Elsifs) or else Present (Else_Stmts) then
         Gen_Label (Exit_Label);
      end if;
   end Generate_If_Statement;

   ---------------------------
   -- Generate_Loop_Statement --
   ---------------------------

   procedure Generate_Loop_Statement (Loop_Stmt : Node_Id) is
      Iter_Scheme  : constant Node_Id := Iteration_Scheme (Loop_Stmt);
      Repeat_Label : constant Label_Id := New_Label;
      Exit_Label   : Label_Id;
      High_Bnd     : Node_Id;
      Loop_Id      : Entity_Id;
      Loop_Index   : Local_Var_Id;
      Loop_Limit   : Local_Var_Id;
      Loop_Param   : Entity_Id;
      Loop_Subtype : Node_Id;
      Low_Bnd      : Node_Id;
      Param_Spec   : Node_Id := Empty;
      Save_Label   : Label_Id;

   begin
      if Present (Iter_Scheme) then
         pragma Assert (not Present (Condition_Actions (Iter_Scheme)));

         Param_Spec := Loop_Parameter_Specification (Iter_Scheme);

         if Present (Param_Spec) then
            Loop_Param   := Defining_Identifier (Param_Spec);
            Loop_Subtype := Discrete_Subtype_Definition (Param_Spec);

            if Nkind (Loop_Subtype) = N_Range then
               Low_Bnd  := Low_Bound (Loop_Subtype);
               High_Bnd := High_Bound (Loop_Subtype);

            elsif Nkind (Loop_Subtype) = N_Identifier then
               Low_Bnd  := Low_Bound (Scalar_Range (Entity (Loop_Subtype)));
               High_Bnd := High_Bound (Scalar_Range (Entity (Loop_Subtype)));

            elsif Nkind (Loop_Subtype) = N_Subtype_Indication then
               Low_Bnd  := Low_Bound (Scalar_Range (Etype (Loop_Subtype)));
               High_Bnd := High_Bound (Scalar_Range (Etype (Loop_Subtype)));

            else
               pragma Assert (False);
               raise Program_Error;
            end if;

            Loop_Index :=
              New_Local_Var ("_loop_param", JVM_Type (Loop_Param));
            Loop_Limit :=
              New_Local_Var ("_loop_limit", JVM_Type (Loop_Param));
            Set_Map (Loop_Param, Loop_Index);

            Evaluate_Expr (Low_Bnd);

            --  ??? sometimes we are getting 64 bit universal integers that
            --  need to be converted to I4

            if JVM_Expr_Type (Low_Bnd) /= JVM_Type (Loop_Param) then
               Gen_Conversion (JVM_Type (Loop_Param));
            end if;

            Evaluate_Expr (High_Bnd);

            if JVM_Expr_Type (High_Bnd) /= JVM_Type (Loop_Param) then
               Gen_Conversion (JVM_Type (Loop_Param));
            end if;

            --  for loops were as follows
            --  repeat_label: load loop_index
            --                load loop_limit
            --                bgt  exit_label
            --                loop statements
            --                increment loop_index
            --                goto repeat_label

            --  Instead we now do as follows to avoid the problem where it is
            --  impossible to increment past the end of the range:
            --                load loop_index
            --                load loop_limit
            --                bgt  exit_label
            --  repeat_label: loop statements
            --                load loop_index
            --                load loop_limit
            --                bge  exit_label
            --                increment loop_index
            --                goto repeat_label

            --  what happens on a reverse???
            --                load loop_limit
            --                load loop_index
            --                bgt  exit_label
            --  repeat_label: loop statements
            --                load loop_limit
            --                load loop_index
            --                bge  exit_label
            --                decrement loop_index
            --                goto repeat_label

            if Reverse_Present (Param_Spec) then
               Gen_Store_Local (Loop_Index);
               Gen_Store_Local (Loop_Limit);
               Gen_Load_Local (Loop_Limit);
               Gen_Load_Local (Loop_Index);
            else
               Gen_Store_Local (Loop_Limit);
               Gen_Store_Local (Loop_Index);
               Gen_Load_Local (Loop_Index);
               Gen_Load_Local (Loop_Limit);
            end if;

            Exit_Label := New_Label;
            Gen_Compare_Branch_Greater (Exit_Label);
            Gen_Label (Repeat_Label);

         elsif Present (Condition (Iter_Scheme)) then
            Gen_Label (Repeat_Label);
            Exit_Label := New_Label;
            Save_Label := Exit_Label;
            Evaluate_Expr (Condition (Iter_Scheme), Save_Label, False);

            --  If Save_Label was unused during the condition evaluation, then
            --  generate a branch to it now based on the Boolean top-of-stack
            --  value.

            if Save_Label /= Null_Label then
               Gen_Branch_Equal (Exit_Label);
            end if;

         else
            pragma Assert (False);
            raise Program_Error;
         end if;

      --  Endless loop case

      else
         Gen_Label (Repeat_Label);
      end if;

      Translate_Statements (Statements (Loop_Stmt));

      if Present (Param_Spec) then
         if Reverse_Present (Param_Spec) then
            Gen_Load_Local (Loop_Limit);
            Gen_Load_Local (Loop_Index);
            Gen_Compare_Branch_Greater_Equal (Exit_Label);
            Gen_Incr_Local (Loop_Index, Uint_Minus_1);
         else
            Gen_Load_Local (Loop_Index);
            Gen_Load_Local (Loop_Limit);
            Gen_Compare_Branch_Greater_Equal (Exit_Label);
            Gen_Incr_Local (Loop_Index, Uint_1);
         end if;

         --  If the local variable has been allocated to a field in the current
         --  method's AR object, then also save the new loop parameter value in
         --  the AR field. Any up-level references to the loop parameter will
         --  retrieve the value from the AR field.

         if Access_From_Current_AR (Loop_Param) then
            Gen_Load_Local (AR_Stack.Top.AR_Obj);
            Gen_Load_Local (Loop_Index);
            Gen_Put_Field  (AR_Field (AR_Stack.Top, Loop_Param));
         end if;
      end if;

      Gen_Goto (Repeat_Label);

      if Present (Iter_Scheme) then
         Gen_Label (Exit_Label);
      end if;

      --  If a JVM label is associated with the loop's identifier or the loop
      --  statement itself, then emit the label (it must be the target of an
      --  exit statement).

      if Present (Identifier (Loop_Stmt)) then
         Loop_Id := Entity (Identifier (Loop_Stmt));

         if JVM_Entity (Loop_Id) /= Null_Label then
            Gen_Label (JVM_Entity (Loop_Id));
         end if;
      elsif JVM_Entity (Loop_Stmt) /= Null_Label then
         Gen_Label (JVM_Entity (Loop_Stmt));
      end if;
   end Generate_Loop_Statement;

   ---------------------
   -- Generate_Return --
   ---------------------

   procedure Generate_Return (Return_Stmt : Node_Id) is
   begin
      --  Handle expression rewritten by the frontend in CE

      if Present (Expression (Return_Stmt))
        and then Nkind (Expression (Return_Stmt)) = N_Raise_Constraint_Error
      then
         Evaluate_Expr (Expression (Return_Stmt));

      --  If this is a return from a non-constructor function then evaluate the
      --  return expression (constructors have void results and so the return
      --  expression must be ignored).

      elsif Present (Expression (Return_Stmt))
        and then Name_String (Name (Current_Method)) /= ".ctor"
      then
         if Is_Immutably_Limited_Type (Etype (Expression (Return_Stmt))) then
            Evaluate_Expr (Expression (Return_Stmt));
         else
            Evaluate_With_Copy (Expression (Return_Stmt));
         end if;

         --  Generate array descriptor (if needed)

         if Is_Access_Type (Etype (Expression (Return_Stmt))) then
            null;

         elsif Is_Array_Type (Etype (Expression (Return_Stmt)))
           and then Is_Array_Descriptor (Result_Type (Current_Method))
           and then not Is_Array_Descriptor (Top_Type)
         then
            Generate_Array_Descriptor (Expression (Return_Stmt));
         end if;
      end if;

      --  Generate a call to the nearest enclosing at-end procedure, if any,
      --  prior to the method return.

      Generate_At_End_Calls (Return_Stmt);

      Gen_Method_Return;
   end Generate_Return;

   --------------------------
   -- In_Exception_Handler --
   --------------------------

   function In_Exception_Handler (Stmt : Node_Id) return Boolean is
      P : Node_Id := Parent (Stmt);

   begin
      while Present (P)
        and then not Nkind_In (P,
                       N_Exception_Handler,
                       N_Loop_Statement,
                       N_Subprogram_Body)
      loop
         P := Parent (P);
      end loop;

      return Nkind (P) = N_Exception_Handler;
   end In_Exception_Handler;

   --------------------------------
   -- Translate_Case_Alternative --
   --------------------------------

   procedure Translate_Case_Alternative
     (Altern : Node_Id;
      Obj    : Local_Var_Id := Null_Local_Var)
   is
      pragma Unreferenced (Obj);
      --  Parameter Obj needed to match profile of type Switch_Action

   begin
      Translate_Statements (Statements (Altern));
   end Translate_Case_Alternative;

   ----------------------------------
   -- Translate_Handled_Statements --
   ----------------------------------

   procedure Translate_Handled_Statements (Handled_Statements : Node_Id) is
      End_Lbl   : Label_Id;
      Exit_Lbl  : Label_Id;
      Handlers  : List_Id;
      Nested    : Boolean;
      Start_Lbl : Label_Id;

   begin
      Nested := Inside_Try_Catch_Finally;

      if Present (Handled_Statements) then
         Handlers := Exception_Handlers (Handled_Statements);

         if Present (Handlers) then
            Start_Lbl := New_Label;
            Gen_Label (Start_Lbl);
         end if;

         if Present (Handlers) then
            Inside_Try_Catch_Finally;
         end if;

         Translate_Statements (Statements (Handled_Statements));

         if Present (Handlers) then
            End_Lbl  := New_Label;
            Exit_Lbl := New_Label;
            Gen_Leave (Exit_Lbl);

            Gen_Label (End_Lbl);

            Translate_Exception_Handlers
              (Handlers, Start_Lbl, End_Lbl, Exit_Lbl);

            Gen_Label (Exit_Lbl);
         end if;

         --  Translate the at-end part (if any) of the statement sequence

         if Present (At_End_Proc (Handled_Statements)) then
            Generate_At_End_Calls (Handled_Statements);
         end if;

         if not Nested then
            Outside_Try_Catch_Finally;
         end if;
      end if;
   end Translate_Handled_Statements;

   -------------------------
   -- Translate_Statement --
   -------------------------

   procedure Translate_Statement (Stmt_Node : Node_Id) is
   begin
      Set_Current_Source_Loc (Sloc (Stmt_Node));

      case Nkind (Stmt_Node) is
         --  >>  when N_Abort_Statement =>

         when N_Assignment_Statement =>

            Generate_Assignment (Stmt_Node);

         when N_Case_Statement =>

            Generate_Case_Statement (Stmt_Node);

         --  >>  when N_Asynchronous_Select =>
         --  >>  when N_Code_Statement =>
         --  >>  when N_Conditional_Entry_Call =>
         --  >>  when N_Delay_Relative_Statement =>
         --  >>  when N_Delay_Until_Statement =>
         --  >>  when N_Entry_Call_Statement =>

         when N_If_Statement =>

            Generate_If_Statement (Stmt_Node);

         when N_Loop_Statement =>

            Generate_Loop_Statement (Stmt_Node);

         when N_Block_Statement =>

            Translate_Declarations (Declarations (Stmt_Node));
            Translate_Handled_Statements
              (Handled_Statement_Sequence (Stmt_Node));

         when N_Exit_Statement =>

            Generate_Exit_Statement (Stmt_Node);

         when N_Free_Statement =>

            --  For now we simply evaluate the argument of the
            --  unchecked deallocation and set it to null.
            --  Eventually we have to take account of the
            --  Storage_Pool and Procedure_To_Call attributes.

            declare
               Acc_Addr : constant Address_Descriptor
                 := Evaluate_Addr (Expression (Stmt_Node));
            begin
               Gen_Push_Null;
               Store_Elementary_Value (Acc_Addr);
            end;

         when N_Goto_Statement =>

            --  Generate calls to the nearest enclosing at-end procedures, if
            --  any, preceding the goto statement.

            Generate_At_End_Calls (Stmt_Node);

            --  Case 1: Goto statement located in a sequence of statements.
            --  We generate a branch to the corresponding label.

            if not In_Exception_Handler (Stmt_Node) then
               Gen_Goto (JVM_Label (Entity (Name (Stmt_Node))));

            --  Case 2: Goto statement located in the statements of exception
            --  handler. In this case the VM does not allow us to generate a
            --  branch; we must generate a leave instruction.

            else
               Gen_Leave (JVM_Label (Entity (Name (Stmt_Node))));
            end if;

         when N_Label =>
            declare
               Label_Id : constant Entity_Id :=
                            Entity (Identifier (Stmt_Node));
            begin
               Gen_Label (JVM_Label (Label_Id));
            end;

         when N_Implicit_Label_Declaration =>
            Translate (Stmt_Node);

         when N_Null_Statement =>
            null;

         when N_Number_Declaration =>
            null;

         when N_Procedure_Call_Statement =>

            Translate_Subprogram_Call (Stmt_Node);

         --  >>  when N_Requeue_Statement =>

         when N_Simple_Return_Statement =>

            Generate_Return (Stmt_Node);

         --  >>  when N_Selective_Accept =>
         --  >>  when N_Timed_Entry_Call =>

         when N_Raise_xxx_Error =>

            Translate_Predefined_Raise (Stmt_Node);

         when N_Raise_Statement =>

            Translate_Raise_Statement (Stmt_Node);

         when N_Exception_Declaration =>

            Translate (Stmt_Node);

         when N_Object_Declaration =>

            Translate (Stmt_Node);

         when N_Renaming_Declaration =>

            Translate (Stmt_Node);

         when N_Itype_Reference =>

            Translate (Stmt_Node);

         when N_Package_Declaration
            | N_Package_Body =>

            Translate (Stmt_Node);

         when N_Generic_Instantiation =>

            Translate (Stmt_Node);

         when N_Subprogram_Body =>

            Translate (Stmt_Node);

         when N_Subprogram_Declaration
            | N_Abstract_Subprogram_Declaration =>

            Translate (Specification (Stmt_Node));

         when N_Generic_Declaration =>

            Translate (Stmt_Node);

         when N_Subtype_Declaration =>

            Translate_Subtype (Defining_Entity (Stmt_Node));

         when N_Full_Type_Declaration
            | N_Private_Type_Declaration
            | N_Private_Extension_Declaration
            | N_Incomplete_Type_Declaration =>

            Translate (Stmt_Node);

         when N_Freeze_Entity =>

            --  For tagged types that implement interfaces generate required
            --  wrappers of interface primitives

            declare
               E : constant Entity_Id := Underlying_Type (Entity (Stmt_Node));

            begin
               if Is_Tagged_Type (E)
                 and then not Is_Class_Wide_Type (E)
                 and then Has_Interfaces (E)
                 and then not Is_Interface (E)
                 and then Convention (E) /= Convention_VM
               then
                  Generate_Interface_Wrappers (E);
               end if;
            end;

            Translate_Declarations (Actions (Stmt_Node));

         when N_Task_Type_Declaration | N_Task_Body | N_Task_Definition =>

            Translate (Stmt_Node);

         when N_Protected_Type_Declaration
            | N_Protected_Body
            | N_Protected_Definition =>

            Translate (Stmt_Node);

         when N_Use_Package_Clause | N_Use_Type_Clause =>

            null;

         when N_Pragma =>

            Translate (Stmt_Node);

         when N_Body_Stub =>

            Translate (Stmt_Node);

         when N_Validate_Unchecked_Conversion =>

            --  No checking for now ???

            null;

         when N_At_Clause |
              N_Component_Clause |
              N_Enumeration_Representation_Clause |
              N_Mod_Clause |
              N_Record_Representation_Clause |
              N_Attribute_Definition_Clause =>

            null;

         when N_Push_xxx_Label =>
            Push_Exception_Label_Stack (Stmt_Node);

         when N_Pop_xxx_Label =>
            Pop_Exception_Label_Stack (Stmt_Node);

         when others =>
            if Debug_Flag_JJ then
               Osint.Fail
                 ("*** unsupported statement node: " &
                  Node_Kind'Image (Nkind (Stmt_Node)));
            else
               pragma Assert (False);
               raise Program_Error;
            end if;
      end case;
   end Translate_Statement;

   --------------------------
   -- Translate_Statements --
   --------------------------

   procedure Translate_Statements (Statements : List_Id) is
      Stmt_Node : Node_Id := First (Statements);

   begin
      while Present (Stmt_Node) loop
         Debug_A_Entry ("(Ada-to-JVM) ", Stmt_Node);

         begin
            Print_Source_Line   (Stmt_Node);
            Translate_Statement (Stmt_Node);

         --  If an exception occurs during statement translation, then report
         --  the error and continue processing at the next statement. The stack
         --  is reset to avoid cascading stack errors.

         exception
            when Exc : others =>
               if Debug_Flag_JJ then
                  Reset_Stack;
                  Write_Str ("*** Unsupported feature at ");
                  Write_Location (Sloc (Stmt_Node));
                  Write_Eol;
                  Write_Str (">>> Exception raised at ");
                  Write_Str (Exception_Message (Exc));
                  Write_Eol;

               else
                  raise;
               end if;
         end;

         Stmt_Node := Next (Stmt_Node);

         Debug_A_Exit ("(Ada-to-JVM) ", Stmt_Node, " (done)");
      end loop;
   end Translate_Statements;

end Jx_Ch5;
