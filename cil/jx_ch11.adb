------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J X _ C H 1 1                               --
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

with Atree;    use Atree;
with Exp_Ch11; use Exp_Ch11;
with JVM.API;  use JVM.API;
with JVM.Map;  use JVM.Map;
with Jx_Ch4;   use Jx_Ch4;
with Jx_Ch5;   use Jx_Ch5;
with Jx_Decl;  use Jx_Decl;
with J_Stack;
with J_String; use J_String;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Stringt;  use Stringt;

package body Jx_Ch11 is

   Current_Exception : Local_Var_Id := Null_Local_Var;
   --  This variable holds the identity of a local variable containing
   --  the current active exception occurrence during the translation
   --  of an exception handler part. See Translate_Exception_Handlers
   --  and Gen_Load_Current_Exception.

   --  The exception label stacks below are associated with Constraint_Error,
   --  Storage_Error, and Program_Error, and used for pushing and popping the
   --  labels of N_Push_xxx_Label and N_Pop_xxx_Label nodes:

   package CE_Stack is new J_Stack (Label_Id, 50);

   package SE_Stack is new J_Stack (Label_Id, 50);

   package PE_Stack is new J_Stack (Label_Id, 50);

   function Local_Exception_Label (Predef_Exc : Node_Kind) return Label_Id;
   --  Returns a label, if available, for a local raise of a predefined
   --  exception of the given kind that is to be transformed to a goto.

   ----------------------------------
   -- Generate_Exception_And_Throw --
   ----------------------------------

   procedure Generate_Exception_And_Throw
     (Exc_Class : Class_Id;
      Raise_Loc : Source_Ptr;
      Reason    : Uint;
      Gen_Call  : Boolean := False)
   is
      Rcheck : Method_Id;
      Param  : Local_Var_Id;
      pragma Warnings (Off, Param);

      function Img (J : Uint) return String;
      --  returns a two digits img of J

      procedure Gen_Raise;
      --  Generate the raise of the top-of-the-stack exception

      ---------
      -- Img --
      ---------

      function Img (J : Uint) return String is
      begin
         UI_Image (J, Format => Decimal);
         if UI_Le (Reason, Uint_9) then
            return "0" & UI_Image_Buffer (1 .. UI_Image_Length);
         else
            return UI_Image_Buffer (1 .. UI_Image_Length);
         end if;
      end Img;

      ---------------
      -- Gen_Raise --
      ---------------

      procedure Gen_Raise is
      begin
         if Gen_Call then
            Gen_Invoke_API_Method (Reraise_No_Defer);
         else
            Gen_Exception_Throw;
         end if;
      end Gen_Raise;

   begin

      if Raise_Loc = No_Location then
         Gen_New_Object
           (Exc_Class,
            Method (Exc_Class,
                    ".ctor",
                    Result  => Void_Type,
                    Param_0 => Type_Of (Exc_Class)));
         Gen_Raise;

      elsif Reason /= No_Uint then
         --  Push a string literal parameter containing the file location
         Namet.Name_Len := 0;

         Get_Name_String
           (Reference_Name (Get_Source_File_Index (Raise_Loc)));
         Gen_Push_String_Const
           (New_String_Constant (String_From_Name_Buffer));

         --  Push the line number
         Gen_Push_Int
           (UI_From_Int (Int (Get_Logical_Line_Number (Raise_Loc))));

         --  And call the correct __gnat_rcheck method
         Rcheck := New_Method
           (API_Class (Ada_Exceptions),
            J_String.Name ("__gnat_rcheck_" & Img (Reason)),
            Result => Void_Type,
            Static => True);
         Param := New_Method_Parameter
           (Rcheck, "file", Java_Lang_Object_Type);
         Param := New_Method_Parameter
           (Rcheck, "line", Int_Type);

         Gen_Invoke_Method (Rcheck);

      else
         --  Push the raise location
         Namet.Name_Len := 0;
         Build_Location_String (Raise_Loc);
         Gen_Push_String_Const
           (New_String_Constant (String_From_Name_Buffer));

         --  Generate a call to the exception's string-parameterized
         --  constructor

         Gen_New_Object
           (Exc_Class,
            Method (Exc_Class,
                    ".ctor",
                    Result  => Void_Type,
                    Param_0 => Type_Of (Exc_Class),
                    Param_1 => Type_Of (API_Class (Lang_String))));
         Gen_Raise;
      end if;
   end Generate_Exception_And_Throw;

   ---------------------------
   -- Local_Exception_Label --
   ---------------------------

   function Local_Exception_Label (Predef_Exc : Node_Kind) return Label_Id is
   begin
      case Predef_Exc is
         when N_Raise_Constraint_Error =>
            if not CE_Stack.Empty then
               return CE_Stack.Top;
            end if;

         when N_Raise_Storage_Error =>
            if not SE_Stack.Empty then
               return SE_Stack.Top;
            end if;

         when N_Raise_Program_Error =>
            if not PE_Stack.Empty then
               return PE_Stack.Top;
            end if;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      --  Case where no label is available for the exception

      return Null_Label;
   end Local_Exception_Label;

   --------------------------------
   -- Push_Exception_Label_Stack --
   --------------------------------

   procedure Push_Exception_Label_Stack (Push_Node : Node_Id) is
      Exc_Label    : constant Entity_Id := Exception_Label (Push_Node);
      VM_Exc_Label : Label_Id           := Null_Label;

   begin
      if Present (Exc_Label) then
         VM_Exc_Label := JVM_Label (Exc_Label);
      end if;

      case Nkind (Push_Node) is
         when N_Push_Constraint_Error_Label =>
            CE_Stack.Push (VM_Exc_Label);

         when N_Push_Storage_Error_Label =>
            SE_Stack.Push (VM_Exc_Label);

         when N_Push_Program_Error_Label =>
            PE_Stack.Push (VM_Exc_Label);

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Push_Exception_Label_Stack;

   -------------------------------
   -- Pop_Exception_Label_Stack --
   -------------------------------

   procedure Pop_Exception_Label_Stack (Pop_Node : Node_Id) is
   begin
      case Nkind (Pop_Node) is
         when N_Pop_Constraint_Error_Label =>
            CE_Stack.Pop;

         when N_Pop_Storage_Error_Label =>
            SE_Stack.Pop;

         when N_Pop_Program_Error_Label =>
            PE_Stack.Pop;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Pop_Exception_Label_Stack;

   -------------------------------------
   -- Translate_Exception_Declaration --
   -------------------------------------

   procedure Translate_Exception_Declaration (Exc_Decl : Node_Id) is
      Exc_Entity : constant Entity_Id := Defining_Entity (Exc_Decl);
      Exc_Class  : Class_Id;
   begin
      Declare_Exception_Class (Exc_Entity);
      Exc_Class := Class_Of_Type (JVM_Entity (Exc_Entity));

      Class_Stack.Push (Exc_Class);
      Begin_Class_File (Exc_Class);

      --  For now we just generate trivial versions of
      --  the class's <clinit> and <init> methods.

      Generate_Class_Init_Method   (Exc_Class);
      Generate_Default_Constructor (Exc_Class);

      --  Generate the exception constructor which takes a message string

      declare
         Exc_Constr : constant Method_Id
           := Method (Exc_Class,
                      Name (".ctor"),
                      Result  => Void_Type,
                      Param_0 => Type_Of (Exc_Class),
                      Param_1 => Type_Of (API_Class (Lang_String)));

         Str_Param  : constant Local_Var_Id
           := Next_Local_Var (First_Local_Var (Exc_Constr));

      begin
         Open_Method (Exc_Constr);
         Set_Current_Method (Exc_Constr);
         Method_Stack.Push (Exc_Constr);
         Gen_Load_Local (This_Local (Exc_Constr));

         --  Pass the exception message parameter to the superclass constructor

         Gen_Load_Local (Str_Param);

         Gen_Invoke_Special
           (Method (Superclass (Exc_Class),
                    ".ctor",
                    Result  => Void_Type,
                    Param_0 => Type_Of (Superclass (Exc_Class)),
                    Param_1 => Type_Of (API_Class (Lang_String))));

         Gen_Method_Return;
         Method_Stack.Pop;
         Close_Method (Exc_Constr);

         if not Method_Stack.Empty then
            Set_Current_Method (Method_Stack.Top);
         end if;
      end;

      End_Class_File (Exc_Class);

      pragma Assert (Class_Stack.Top = Exc_Class);
      Class_Stack.Pop;
   end Translate_Exception_Declaration;

   -------------------------------
   -- Translate_Raise_Statement --
   -------------------------------

   procedure Translate_Raise_Statement (Raise_Stmt : Node_Id) is
      Exc_Id     : constant Entity_Id := Entity (Name (Raise_Stmt));
      Exc_Reason : Uint;

   begin
      if Nkind (Raise_Stmt) in N_Raise_xxx_Error then
         Exc_Reason := Reason (Raise_Stmt);

      else
         --  No reason
         Exc_Reason := No_Uint;
      end if;

      Generate_Exception_And_Throw
        (JVM_Class (Exc_Id), Sloc (Raise_Stmt), Exc_Reason);
   end Translate_Raise_Statement;

   --------------------------------
   -- Translate_Predefined_Raise --
   --------------------------------

   procedure Translate_Predefined_Raise (Raise_Node : Node_Id) is
      Raise_Kind    : constant Node_Kind := Nkind (Raise_Node);
      Local_Exc_Lbl : constant Label_Id  := Local_Exception_Label (Raise_Kind);
      False_Label   : constant Label_Id  := New_Label;
      Save_Label    : Label_Id           := False_Label;
      Exc_Class     : Class_Id;
      Exc_Reason    : constant Uint      := Reason (Raise_Node);

      procedure Gen_Local_Raise;
      --  If there is a goto label available associated with the exception,
      --  then generate a call to Local_Raise followed by a goto to the label.
      --  This is for support of local raises when the No_Exception_Propagation
      --  restriction is in effect.

      ---------------------
      -- Gen_Local_Raise --
      ---------------------

      procedure Gen_Local_Raise is
         Local_Raise : constant Entity_Id := Get_Local_Raise_Call_Entity;

      begin
         --  If Local_Raise is available, then generate a call to it, and pass
         --  an exception object. Currently Local_Raise isn't part of the VM
         --  run-time library, but we check for it anyway in case it ever gets
         --  added. ???

         if Present (Local_Raise) then
            Gen_New_Object (Exc_Class);
            Gen_Invoke_Method (JVM_Method (Local_Raise));
         end if;

         Gen_Goto (Local_Exc_Lbl);  -- Jump to the exception handler code
      end Gen_Local_Raise;

   --  Start of processing for Translate_Predefined_Raise

   begin
      case Raise_Kind is
         when N_Raise_Constraint_Error =>
            Exc_Class := API_Class (Ada_Constraint_Error);
         when N_Raise_Program_Error =>
            Exc_Class := API_Class (Ada_Program_Error);
         when N_Raise_Storage_Error =>
            Exc_Class := API_Class (Ada_Storage_Error);
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      --  A return statement may be replaced by a raise statement during
      --  expansion, so we also need to declare the ret_val variable if needed
      --  here.

      Declare_Ret_Val;

      if No (Condition (Raise_Node)) then

         --  If there's a local exception label, then generate a jump to it,
         --  otherwise generate a call to raise the exception.

         if Local_Exc_Lbl /= Null_Label then
            Gen_Local_Raise;

         else
            --  We request a call to be generated rather than a direct throw,
            --  to work around a JVM behavior (dare we say bug?) that causes it
            --  to associate an unconditionally raised exception with the wrong
            --  handler part in certain cases. This shows up commonly in ACATS
            --  tests, typically in the context of a block statement whose
            --  declarative part raises exceptions detected statically by the
            --  GNAT front end (the exceptions should be propagated out of the
            --  block, but were incorrectly being handled by the handler part
            --  of the block).

            Generate_Exception_And_Throw
              (Exc_Class, Sloc (Raise_Node), Exc_Reason, Gen_Call => True);
         end if;
      else
         Evaluate_Expr (Condition (Raise_Node), Save_Label, False);

         --  If Save_Label was unused during the condition evaluation,
         --  then generate a branch to it now based on the Boolean
         --  top-of-stack value.

         if Save_Label /= Null_Label then
            Gen_Branch_Equal (False_Label);
         end if;

         --  If there's a local exception label, then generate a jump to it,
         --  otherwise generate a throw of the exception.

         if Local_Exc_Lbl /= Null_Label then
            Gen_Local_Raise;
         else
            Generate_Exception_And_Throw
              (Exc_Class, Sloc (Raise_Node), Exc_Reason);
         end if;

         Gen_Label (False_Label);
      end if;
   end Translate_Predefined_Raise;

   ----------------------------------
   -- Translate_Exception_Handlers --
   ----------------------------------

   procedure Translate_Exception_Handlers
     (Handlers  : List_Id;
      Start_Lbl : Label_Id;
      End_Lbl   : Label_Id;
      Exit_Lbl  : Label_Id)
   is
      function Exception_Class (Exc_Choice : Node_Id) return Class_Id;
      --  Return the class of the exception associated with Exc_Choice

      procedure Translate_Filter (This_Exc_Class : Class_Id);
      --  Generate code to catch the exception This_Exc_Class in the
      --  current filter

      procedure Translate_Handler (Handler : Node_Id);
      --  Recursive routine that creates exception handler entries for each
      --  exception choice that cover the range of instructions to which the
      --  handler applies.

      ---------------------
      -- Exception_Class --
      ---------------------

      function Exception_Class (Exc_Choice : Node_Id) return Class_Id is
      begin
         if Nkind (Exc_Choice) /= N_Others_Choice then
            return JVM_Class (Entity (Exc_Choice));

         elsif All_Others (Exc_Choice) then
            return API_Class (Lang_Exception);

         else
            return API_Class (Lang_RuntimeException);
         end if;
      end Exception_Class;

      ----------------------
      -- Translate_Filter --
      ----------------------

      Handled_Exc : constant Local_Var_Id :=
                      New_Local_Var ("_exc_var",
                        Type_Of (API_Class (Lang_Throwable)));

      procedure Translate_Filter (This_Exc_Class : Class_Id) is
      begin
         Gen_Load_Local (Handled_Exc);
         Gen_Instance_Of (This_Exc_Class);
         Gen_Or;
      end Translate_Filter;

      -----------------------
      -- Translate_Handler --
      -----------------------

      procedure Translate_Handler (Handler : Node_Id) is
         Handler_Lbl        : constant Label_Id := New_Label;
         End_Of_Handler_Lbl : constant Label_Id := New_Label;

         Exc_Class   : Class_Id;
         Exc_Choice  : Node_Id;
         Filter_Lbl  : Label_Id     := Null_Label;
         Kind        : Handler_Kind := Non_Filter;

      begin
         if No (Handler) then
            return;
         end if;

         Exc_Choice := First (Exception_Choices (Handler));
         Exc_Class  := Exception_Class (Exc_Choice);

         --  If a handler catches several exceptions then we must use a filter

         if Present (Next (Exc_Choice))
           or else Exc_Class = API_Class (Ada_Constraint_Error)
           or else Exc_Class = API_Class (Ada_Storage_Error)
         then
            Kind := Filter;
         end if;

         --  Processing for filters

         if Kind = Filter then
            Filter_Lbl := New_Label;
            Gen_Label (Filter_Lbl);
            Push_Type (Type_Of (Handled_Exc));

            --  Conversion not really needed, but makes verifier happy as not
            --  all languages use children of system.exception

            Gen_Conversion (Type_Of (API_Class (Lang_Exception)));
            Gen_Store_Local (Handled_Exc);

            --  Push False in the stack (required by Translate_Filter)

            Gen_Push_Int (Uint_0);

            --  Loop through and add things to the filter (if needed)

            while Present (Exc_Choice) loop
               Exc_Class := Exception_Class (Exc_Choice);

               Translate_Filter (Exc_Class);

               --  For CE handler add handler entries for predefined .NET
               --  run-time exceptions that are mapped to Constraint_Error

               if Exc_Class = API_Class (Ada_Constraint_Error) then
                  Translate_Filter (API_Class (IndexOutOfBoundsException));
                  Translate_Filter (API_Class (NullPointerException));
                  Translate_Filter (API_Class (ArithmeticException));
                  Translate_Filter (API_Class (InvalidCastException));

               --  For SE handler add handler entries for OutOfMemoryError and
               --  StackOverflowError

               elsif Exc_Class = API_Class (Ada_Storage_Error) then
                  Translate_Filter (API_Class (Lang_OutOfMemoryError));
                  Translate_Filter (API_Class (Lang_StackOverflowError));
               end if;

               Exc_Choice := Next (Exc_Choice);
            end loop;

            Gen_End_Filter;
         end if;

         --  Handler code

         Gen_Label (Handler_Lbl);

         --  The exception is on the top of the stack

         Push_Type (Type_Of (Handled_Exc));

         --  Conversion not really needed, but makes verifier happy as not all
         --  languages use children of system.exception

         Gen_Conversion (Type_Of (API_Class (Lang_Exception)));
         Gen_Store_Local (Handled_Exc);

         --  Set Current_Exception so that calls to Gen_Load_Current_Exception
         --  will pick up the active exception occurrence.

         Current_Exception := Handled_Exc;

         --  Generate the exception handler code

         Translate_Statements (Statements (Handler));

         --  End of handler code

         Gen_Leave (Exit_Lbl);
         Gen_Label (End_Of_Handler_Lbl);

         Gen_Exc_Handler_Entry (Exc_Class, Start_Lbl, End_Lbl,
            Handler_Lbl, End_Of_Handler_Lbl, Kind, Filter_Lbl);

         Translate_Handler (Next (Handler));
      end Translate_Handler;

      --  Local variables

      Handler       : Node_Id;
      Save_Curr_Exc : constant Local_Var_Id := Current_Exception;
      --  Current_Exception is saved to handle nested exception handlers
      --  properly

   --  Start of processing for Translate_Exception_Handlers

   begin
      --  Skip pragmas in exception handling part

      Handler := First (Handlers);
      while Present (Handler)
        and then Nkind (Handler) /= N_Exception_Handler
      loop
         Handler := Next (Handler);
      end loop;

      Translate_Handler (Handler);

      --  Restore Current_Exception so it's value will reflect the current
      --  active exception occurrence associated with any enclosing handler

      Current_Exception := Save_Curr_Exc;
   end Translate_Exception_Handlers;

   --------------------------------
   -- Gen_Load_Current_Exception --
   --------------------------------

   procedure Gen_Load_Current_Exception is
   begin
      Gen_Load_Local (Current_Exception);
   end Gen_Load_Current_Exception;

end Jx_Ch11;
