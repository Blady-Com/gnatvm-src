------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 6                                --
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
with Einfo;         use Einfo;
with Elists;        use Elists;
with Errout;        use Errout;
with Exp_Disp;      use Exp_Disp;
with Exp_Tss;       use Exp_Tss;
with JVM;           use JVM;
with JVM.API;       use JVM.API;
with JVM.Map;       use JVM.Map;
with Jx_Ch3;        use Jx_Ch3;
with Jx_Ch4;        use Jx_Ch4;
with Jx_Ch5;        use Jx_Ch5;
with Jx_Ch7;        use Jx_Ch7;
with Jx_Ch11;       use Jx_Ch11;
with J_String;      use J_String;
with J_Descriptors; use J_Descriptors;
with Jx_Decl;       use Jx_Decl;
with Jx_Drive;      use Jx_Drive;
with Jx_Uplev;      use Jx_Uplev;
with Namet;         use Namet;
with Nlists;        use Nlists;
with Opt;           use Opt;
with Sem_Aux;       use Sem_Aux;
with Sem_Disp;      use Sem_Disp;
with Sem_Type;      use Sem_Type;
with Sem_Util;      use Sem_Util;
with Sinfo;         use Sinfo;
with Snames;        use Snames;
with Stand;         use Stand;
with Stringt;       use Stringt;
with Targparm;      use Targparm;
with Uintp;         use Uintp;

package body Jx_Ch6 is

   procedure Generate_Main_Entry_Point
     (Main      : Method_Id;
      Adainit   : Method_Id;
      Adafinal  : Method_Id;
      Call_Main : Boolean);
   --  Generate a main entry point method corresponding to the given Main unit.
   --  Adainit and Adafinal correspond to the adainit/adafinal methods that
   --  should be called by the main entry point. If Call_Main is True, Main
   --  will also be called.

   ---------------------------------
   -- Generate_Interface_Wrappers --
   ---------------------------------

   --  The rules overriding primitives of the VM machine do not match Ada
   --  rules. In order to provide the Ada semantics we generate wrappers that
   --  fulfill the rules of the VM machine and invoke the method required by
   --  Ada.

   procedure Generate_Interface_Wrappers (Tag_Typ : Entity_Id) is

      function Generate_Wrapper
        (Wrapped_Prim : Entity_Id;
         Target_Prim  : Entity_Id) return Method_Id;
      --  Generates the spec and body of the wrapper of Target_Prim

      function Generate_Wrapper
        (Wrapped_Prim : Entity_Id;
         Target_Prim  : Entity_Id) return Method_Id
      is
         Target_Method  : constant Method_Id := JVM_Method (Target_Prim);
         Formal_LV      : Local_Var_Id;
         Wrapper_Method : Method_Id;

      begin
         pragma Assert (Is_Static (Target_Method));

         Wrapper_Method :=
           Declare_Interface_Wrapper_Method (Wrapped_Prim, Target_Prim);

         --  Add the body of the wrapper

         Open_Method        (Wrapper_Method);
         Set_Current_Method (Wrapper_Method);
         Method_Stack.Push  (Wrapper_Method);

         Formal_LV := First_Local_Var (Wrapper_Method);
         while Formal_LV /= Null_Local_Var loop
            Gen_Load_Local (Formal_LV);
            Formal_LV := Next_Local_Var (Formal_LV);
         end loop;

         Gen_Invoke_Static (Target_Method);
         Gen_Method_Return;

         pragma Assert (Method_Stack.Top = Wrapper_Method);
         Method_Stack.Pop;
         Close_Method (Wrapper_Method);

         if not Method_Stack.Empty then
            Set_Current_Method (Method_Stack.Top);
         end if;

         return Wrapper_Method;
      end Generate_Wrapper;

      --  Local Variables

      Controlling_Formal : Entity_Id;
      Elmt               : Elmt_Id;
      Formal             : Entity_Id;
      Iface_Typ          : Entity_Id;
      Iface_Formal       : Entity_Id;
      Ifaces_List        : Elist_Id;
      Need_Wrapper       : Boolean;
      Prim               : Entity_Id;
      Root_Typ           : Entity_Id;
      Wrapper            : Method_Id;

      pragma Unreferenced (Wrapper);

   --  Start of processing for Generate_Interface_Wrappers

   begin
      pragma Assert (Is_Tagged_Type (Tag_Typ)
        and then not Is_Class_Wide_Type (Tag_Typ)
        and then Has_Interfaces (Tag_Typ)
        and then not Is_Interface (Tag_Typ)
        and then not Ekind_In (Tag_Typ,
                       E_Record_Type_With_Private,
                       E_Record_Subtype_With_Private));

      --  Check cases in which no wrapper is needed

      if Ekind (Tag_Typ) = E_Record_Subtype

         --  In case of synchronized types the wrappers are generated when the
         --  corresponding record type is frozen

        or else Is_Protected_Type (Tag_Typ)
        or else Is_Task_Type (Tag_Typ)
      then
         return;
      end if;

      --  Handle private types

      if Present (Full_View (Tag_Typ)) then
         Root_Typ := Root_Type (Full_View (Tag_Typ));
      else
         Root_Typ := Root_Type (Tag_Typ);
      end if;

      Collect_Interfaces (Tag_Typ, Ifaces_List,
        Exclude_Parents => False);

      Elmt := First_Elmt (Primitive_Operations (Tag_Typ));
      while Present (Elmt) loop
         Prim := Elists.Node (Elmt);

         if Is_Predefined_Dispatching_Operation (Prim)
           and then (No (Alias (Prim)))
         then
            --  Wrapper needed for the assignment and the equality operator of
            --  all the interfaces

            if Chars (Prim) = Name_Op_Eq
              or else Chars (Prim) = Name_uAssign
            then
               declare
                  Iface      : Elmt_Id;
                  Iface_Prim : Elmt_Id;

               begin
                  Iface := First_Elmt (Ifaces_List);
                  while Present (Iface) loop
                     if Etype (Node (Iface)) = Node (Iface)
                       and then Node (Iface) /= Root_Typ
                     then
                        --  Search for the partner entity in the interface type

                        Iface_Prim :=
                          First_Elmt (Primitive_Operations (Node (Iface)));
                        while Present (Iface_Prim)
                          and then (Chars (Prim) /= Chars (Node (Iface_Prim))
                                      or else
                                    Present (Alias (Node (Iface_Prim))))
                        loop
                           Next_Elmt (Iface_Prim);
                        end loop;

                        if Present (Iface_Prim) then
                           Wrapper :=
                             Generate_Wrapper
                               (Wrapped_Prim => Node (Iface_Prim),
                                Target_Prim  => Prim);
                        end if;
                     end if;

                     Next_Elmt (Iface);
                  end loop;
               end;
            end if;

         --  Search for entities associated with interface mappings

         elsif Present (Interface_Alias (Prim))

            --  No wrapper needed for primitives of interfaces that are parents
            --  of this tagged type

           and then not Is_Ancestor
                          (Find_Dispatching_Type (Interface_Alias (Prim)),
                           Tag_Typ)
           and then not Is_Abstract_Subprogram (Ultimate_Alias (Prim))

            --  No wrapper needed for indirectly inherited interface
            --  primitives; otherwise the wrappers would be duplicated!

           and then No (Alias (Interface_Alias (Prim)))
         then
            Need_Wrapper := False;
            Iface_Typ := Find_Dispatching_Type (Interface_Alias (Prim));

            --  Handle common case: the interface primitive is covered by
            --  a primitive defined for this tagged type (that is, the
            --  interface primitive is not covered by a primitive inherited
            --  from the parent of tagged type).

            if No (Alias (Alias (Prim))) then
               pragma Assert
                 (Underlying_Type (Find_Dispatching_Type (Alias (Prim)))
                    = Tag_Typ);

               --  Case 1: Wrapper needed in case of function covering an
               --  interface primitive whose returning type is the interface.
               --  For example:

               --     type Iface is interface;
               --     function Self return Iface;

               --     type DT is new Root and Iface with ...
               --     function Self return DT;

               --  The virtual machine requires the following profile
               --  to handle it properly:

               --     function Self return Iface;

               if Ekind (Prim) = E_Function
                 and then Etype (Interface_Alias (Prim)) = Iface_Typ
               then
                  Need_Wrapper := True;
               end if;

               --  Case 2: Wrapper needed in case of subprogram convering
               --  interface primitives that have several formals whose type
               --  is the type of the controlling argument. For example:

               --    type Iface is interface;
               --    procedure Copy (Source : Iface; Target : Iface);

               --  Such primitive is overriden in Ada as follows:

               --     type T is new Root and Iface with null record;
               --     procedure Copy (Source : T; Target : T);

               --  However, the virtual machine requires the following profile
               --  to handle it properly:

               --     procedure Copy (Source : T; Target : Iface);

               --  No need to invest time here if we already know that we
               --  must generate the interface wrapper

               if not Need_Wrapper then

                  --  Step 1: Search for the controlling formal

                  Formal := First_Formal (Prim);
                  while Present (Formal)
                    and then not Is_Controlling_Formal (Formal)
                  loop
                     Next_Formal (Formal);
                  end loop;

                  --  Step 2: Search for arguments that are not the
                  --  controlling formal but reference the interface type

                  if Present (Formal) then
                     Controlling_Formal := Formal;

                     Iface_Formal := First_Formal (Interface_Alias (Prim));
                     Formal       := First_Formal (Prim);
                     while Present (Formal) loop
                        if Formal /= Controlling_Formal
                          and then Underlying_Type (Etype (Formal)) = Tag_Typ
                        then
                           Need_Wrapper := True;
                           exit;
                        end if;

                        Next_Formal (Iface_Formal);
                        Next_Formal (Formal);
                     end loop;
                  end if;
               end if;

               --  Case 3: Wrapper needed if the CIL names associated with
               --  the primitives don't match. Its source is that the ordering
               --  of overloaded primitives in the interface type declaration
               --  and the ordering of such primitives in the tagged type that
               --  implements the interface type is different. This ordering
               --  difference causes the internal generation of different
               --  suffix numbers to handle overloaded primitives.

               if not Need_Wrapper then
                  declare
                     S1 : constant String :=
                            Name_String
                             (Name (Next_Method (JVM_Method (Alias (Prim)))));
                     S2 : constant String :=
                            Name_String
                             (Name (JVM_Method (Interface_Alias (Prim))));
                  begin
                     if S1 /= S2 then
                        Need_Wrapper := True;
                     end if;
                  end;
               end if;

            --  Handle cases in which the interface primitive is covered by
            --  some inherited primitive

            else
               --  Case 4: Wrapper needed if the name associated with the
               --  primitives don't match.

               declare
                  S1 : constant String :=
                         Name_String
                          (Name (Next_Method (JVM_Method (Alias (Prim)))));
                  S2 : constant String :=
                         Name_String
                          (Name (JVM_Method (Interface_Alias (Prim))));
               begin
                  if S1 /= S2 then
                     Need_Wrapper := True;
                  end if;
               end;
            end if;

            if Need_Wrapper then
               Wrapper :=
                 Generate_Wrapper
                   (Wrapped_Prim => Interface_Alias (Prim),
                    Target_Prim  => Alias (Prim));
            end if;
         end if;

         Next_Elmt (Elmt);
      end loop;

      --  Generate wrappers for _deep_clone

      declare
         JVM_Typ        : constant Type_Id  := JVM_Type (Tag_Typ);
         Typ_Class      : constant Class_Id := Class_Of_Type (JVM_Typ);
         Formal_LV      : Local_Var_Id;
         Iface          : Elmt_Id;
         Target_Method  : Method_Id;
         Target_Formal  : Local_Var_Id;
         Wrapper_Method : Method_Id;

      begin
         Target_Method := First_Method (Typ_Class);
         while Target_Method /= Null_Method loop
            if Get_Name_String (Name (Target_Method)) = "_deep_clone" then
               exit;
            end if;

            Target_Method := Next_Method (Target_Method);
         end loop;
         pragma Assert (Target_Method /= Null_Method);

         Iface := First_Elmt (Ifaces_List);
         while Present (Iface) loop
            if Etype (Node (Iface)) = Node (Iface)
              and then Node (Iface) /= Root_Typ
            then
               --  Declare the wrapper method

               declare
                  JVM_Iface_Typ : constant Type_Id  := JVM_Type (Node (Iface));
                  Iface_Class   : constant Class_Id :=
                                    Class_Of_Type (JVM_Iface_Typ);
               begin
                  Wrapper_Method :=
                    New_Method
                      (Class   => Typ_Class,
                       Name    => Name (Target_Method),
                       Result  => JVM_Iface_Typ,
                       Static  => False,
                       Abstrct => False,
                       Parent  => Null_Method,
                       Skip_Arg_This => True);

                  Target_Formal := First_Local_Var (Target_Method);
                  while Target_Formal /= Null_Local_Var
                    and then Is_Param (Target_Formal)
                  loop
                     Formal_LV :=
                       New_Method_Parameter
                         (Wrapper_Method,
                          Name (Target_Formal), Type_Of (Target_Formal));

                     Target_Formal := Next_Local_Var (Target_Formal);
                  end loop;

                  --  Register the wrapper in the backend. Done to improve the
                  --  output of the generated code

                  Set_Is_Interface_Wrapper (Wrapper_Method);
                  Set_Class_Of_Wrapped_Interface (Wrapper_Method, Iface_Class);
               end;

               --  Add the body of the wrapper

               Open_Method        (Wrapper_Method);
               Set_Current_Method (Wrapper_Method);
               Method_Stack.Push  (Wrapper_Method);

               Formal_LV := First_Local_Var (Wrapper_Method);
               while Formal_LV /= Null_Local_Var
                 and then Is_Param (Formal_LV)
               loop
                  Gen_Load_Local (Formal_LV);
                  Formal_LV := Next_Local_Var (Formal_LV);
               end loop;

               Gen_Invoke_Method (Target_Method);
               Gen_Method_Return;

               pragma Assert (Method_Stack.Top = Wrapper_Method);
               Method_Stack.Pop;
               Close_Method (Wrapper_Method);

               if not Method_Stack.Empty then
                  Set_Current_Method (Method_Stack.Top);
               end if;
            end if;

            Next_Elmt (Iface);
         end loop;
      end;
   end Generate_Interface_Wrappers;

   -------------------------------
   -- Generate_Main_Entry_Point --
   -------------------------------

   procedure Generate_Main_Entry_Point
     (Main      : Method_Id;
      Adainit   : Method_Id;
      Adafinal  : Method_Id;
      Call_Main : Boolean)
   is
      Main_Method : Method_Id;
      Main_Args   : Local_Var_Id;
      Main_Name   : Name_Id;

   begin
      case VM_Target is
         when JVM_Target => Main_Name := Name ("main");
         when CLI_Target => Main_Name := Name ("_main");
         when No_VM      => raise Program_Error;
      end case;

      Main_Method :=
        New_Method
          (Class  => Current_Compilation_Class,
           Name   => Main_Name,
           Result => Void_Type,
           Static => True);

      Main_Args :=
        New_Method_Parameter
          (Method => Main_Method,
           Name   => Name ("args"),
           Ptype  => New_Array_Type (Type_Of (API_Class (Lang_String))));

      Open_Method (Main_Method);
      Set_Current_Method (Main_Method);
      Method_Stack.Push (Main_Method);

      --  Save the command line arguments reference in GNAT_libc.gnat_argv

      Gen_Load_Local (Main_Args);
      Gen_Put_Static_Field (API_Field (Gnat_Argv));

      --  Save the main program name in GNAT_libc.command_name

      Gen_Push_String_Const
        (New_String_Constant (Str_Id (Name_String (Name (Main)))));
      Gen_Put_Static_Field (API_Field (Command_Name));

      --  Generate call to adainit, main subprogram and adafinal

      Gen_Invoke_Static (Adainit);

      if Call_Main then
         Gen_Invoke_Static (Main);
      end if;

      Gen_Invoke_Static (Adafinal);
      Gen_Method_Return;

      Method_Stack.Pop;
      Close_Method (Main_Method);
   end Generate_Main_Entry_Point;

   ---------------------
   -- Generate_Method --
   ---------------------

   procedure Generate_Method (Subp_Body : Node_Id) is
      Subp_Entity     : Entity_Id;
      Subp_Method     : Method_Id;
      First_Param     : Entity_Id;
      This_LV         : Local_Var_Id;
      Formal_LV       : Local_Var_Id;
      Has_ND_Method   : Boolean   := False;
      Empty_Init_Proc : Boolean   := False;
      Disp_Method     : Method_Id := Null_Method;
      Class           : Class_Id;

   begin
      if Acts_As_Spec (Subp_Body) then
         Subp_Entity := Defining_Entity (Specification (Subp_Body));
      else
         Subp_Entity := Corresponding_Spec (Subp_Body);
      end if;

      --  Suppress generation of method bodies for generic subprograms
      --  as well as subprograms to which a pragma Eliminate applies.

      if Ekind (Subp_Entity) in Generic_Unit_Kind
        or else Is_Eliminated (Subp_Entity)
      then
         return;

      --  Suppress generation of primitives of interface types. Done to avoid
      --  generating code for null interface primitives because they are not
      --  supported by the VM.

      elsif Is_Dispatching_Operation (Subp_Entity)
        and then Is_Interface (Find_Dispatching_Type (Subp_Entity))
      then
         return;
      end if;

      Subp_Method := JVM_Entity (Subp_Entity);

      --  If the subprogram is dispatching and nonabstract, then we need
      --  to generate the body for its associated nondispatching method.
      --  This method is suppressed, however, in the case where the tagged
      --  type's scope has convention Java since the corresponding Java class
      --  has no such operation.

      if Has_Nondispatching_Method (Subp_Entity) then
         Has_ND_Method := True;
         Disp_Method := Next_Method (Subp_Method);
      end if;

      Open_Method (Subp_Method);
      Set_Current_Method (Subp_Method);
      Method_Stack.Push (Subp_Method);

      --  If this is an _init_proc, then perform allocation of any composite
      --  components. Note that at present we don't handle array _init_procs
      --  here, though in general we do need to perform allocations of
      --  composite components for arrays. ???

      if Is_Init_Proc (Subp_Entity) then
         First_Param := First_Formal (Subp_Entity);
         This_LV := JVM_Entity (First_Param);

         --  Skip code generation inside the body of the init_proc of a type
         --  with convention Java. This avoids the problem of translating code
         --  that invokes the init_proc of a parent type that is a Java API, as
         --  well as prevents initialization of fictional discriminants of the
         --  type (no field is actually declared for interface 'self'
         --  discriminants or discriminants of types implementing Java
         --  interfaces). For now at least we want to simply disallow any
         --  stand-alone objects of types mapped to Java, and users should
         --  always initialize objects of such types by calling constructor
         --  functions. This is admittedly a kludge, and it would be nice to
         --  find a cleaner way to handle this or suppress the init_proc. ???

         if Is_Null_Init_Proc (Subp_Entity)
           or else (Is_Tagged_Type (Full_Type (First_Param))
                      and then Convention (Full_Type (First_Param)) =
                                                        Convention_VM)
         then
            Empty_Init_Proc := True;

         else
            Allocate_Composite_Components (Etype (First_Param), This_LV);
         end if;
      end if;

      if Ekind (Subp_Entity) = E_Function then
         Declare_Ret_Val;
      end if;

      --  Generate a return in the case of an empty init_proc, to prevent the
      --  generation of statement "raise Program_Error". (If we decide to allow
      --  stand-alone Java-convention tagged objects then we don't want an
      --  exception raised when the init_proc is called.)

      if Empty_Init_Proc then
         Gen_Method_Return;

      else
         Translate_Declarations (Declarations (Subp_Body));
         Translate_Handled_Statements (Handled_Statement_Sequence (Subp_Body));

         if not AR_Stack.Empty
           and then AR_Stack.Top.Method = Subp_Method
         then
            End_Activation_Record (Subp_Method);
         end if;
      end if;

      pragma Assert (Method_Stack.Top = Subp_Method);
      Method_Stack.Pop;
      Close_Method (Subp_Method);

      --  If the subprogram is a nonabstract dispatching operation, then
      --  generate the associated dispatching version of the method.
      --  This method has perforce already been declared and is obtained
      --  as the successor of the nondispatching method. The nondispatching
      --  method body contains all of the subprogram's code and is invoked
      --  by the dispatching method. (Note that we used to generate the
      --  code in the dispatching method and have the nondispatching method
      --  call the dispatching method via an Invokespecial, but that turned
      --  out not to work on certain JVMs because of an ambiguity in the
      --  definition of Invokespecial semantics, though it worked on the
      --  Sun JVM.)

      --  If the subprogram has an associated nondispatching method,
      --  then we simply generate a call to the nondispatching method,
      --  passing along all of the parameters.

      if Has_ND_Method then
         Open_Method (Disp_Method);
         Set_Current_Method (Disp_Method);
         Method_Stack.Push (Disp_Method);

         Formal_LV := First_Local_Var (Disp_Method);

         while Formal_LV /= Null_Local_Var loop
            Gen_Load_Local (Formal_LV);
            Formal_LV := Next_Local_Var (Formal_LV);
         end loop;

         Gen_Invoke_Static (Subp_Method);
         Gen_Method_Return;

         pragma Assert (Method_Stack.Top = Disp_Method);
         Method_Stack.Pop;
         Close_Method (Disp_Method);
      end if;

      if VM_Target = CLI_Target and then Is_Exported (Subp_Entity) then
         if Interface_Name (Subp_Entity) /= Empty then
            Class := Associated_Class (Subp_Entity);
            String_To_Name_Buffer (Strval (Interface_Name (Subp_Entity)));

            --  If current method is adainit, and Ada_Main_Program_Name is
            --  defined in the current class, it means a main entry point is
            --  required.

            if Name_Buffer (1 .. Name_Len) = "adainit"
              and then Field (Class, "__gnat_ada_main_program_name")
                         /= Null_Field
            then
               declare
                  Main      : constant String :=
                                Name_String (Name (Current_Compilation_Class));
                  Main_Name : constant Name_Id :=
                                Name (Main (Main'First + 4 .. Main'Last - 4));
                  Methd     : constant Method_Id :=
                                New_Method
                                  (Class  => New_Class
                                               (Ada_Ent => Empty,
                                                Name    => Main_Name),
                                   Name   => Main_Name,
                                   Result => Void_Type,
                                   Static => True);

               begin
                  Generate_Main_Entry_Point
                    (Methd, Subp_Method, Method (Class, "adafinal"),
                     Call_Main => False);
               end;
            end if;
         end if;
      end if;

      if not Method_Stack.Empty then
         Set_Current_Method (Method_Stack.Top);
      end if;
   end Generate_Method;

   -------------------------------
   -- Generate_Subprogram_Class --
   -------------------------------

   procedure Generate_Subprogram_Class (Comp_Unit : Node_Id) is
      Comp_Spec    : constant Node_Id := Library_Unit (Comp_Unit);
      Subp         : constant Node_Id := Unit (Comp_Unit);
      Subp_Entity  : Entity_Id        := Defining_Entity (Subp);
      Binder_Class : Class_Id;
      Subp_Class   : Class_Id;
      Elab_Method  : Method_Id;
      Other_Class  : Class_Id;

   begin
      if Nkind (Subp) = N_Subprogram_Body then
         if Acts_As_Spec (Subp) then
            Subp_Entity := Defining_Entity (Specification (Subp));
         else
            Subp_Entity := Corresponding_Spec (Subp);
         end if;
      end if;

      --  The call to Associated_Class will have the side effect
      --  of creating the subprogram's JVM class entity.

      Subp_Class := Associated_Class (Subp_Entity);

      Class_Stack.Push (Subp_Class);
      Begin_Class_File (Subp_Class);
      Current_Compilation_Class := Current_Class;

      Generate_Class_Init_Method (Subp_Class);
      Generate_Default_Constructor (Subp_Class);

      --  Generate the elaboration method for the subprogram spec if needed

      if Present (Comp_Spec) then
         Elab_Method :=
           New_Method (Subp_Class, Name ("_elabs"), Void_Type, True);

         Open_Method (Elab_Method);
         Set_Current_Method (Elab_Method);
         Method_Stack.Push (Elab_Method);

         if Present (Aux_Decls_Node (Comp_Spec)) then
            Translate_Declarations (Declarations (Aux_Decls_Node (Comp_Spec)));
            Translate_Statements   (Actions (Aux_Decls_Node (Comp_Spec)));
         end if;

         Gen_Method_Return;
         Method_Stack.Pop;
         Close_Method (Elab_Method);
      end if;

      --  Generate the elaboration method for the subprogram body

      Elab_Method := New_Method (Subp_Class, Name ("_elabb"), Void_Type, True);
      Open_Method (Elab_Method);
      Set_Current_Method (Elab_Method);
      Method_Stack.Push (Elab_Method);

      if Present (Aux_Decls_Node (Comp_Unit)) then
         Translate_Declarations (Declarations (Aux_Decls_Node (Comp_Unit)));
         Translate_Statements   (Actions (Aux_Decls_Node (Comp_Unit)));
      end if;

      Gen_Method_Return;
      Method_Stack.Pop;
      Close_Method (Elab_Method);

      --  Declare and generate code for the library subprogram

      Declare_Method (Subp_Class, Subp_Entity);
      Generate_Method (Subp);

      --  If the subprogram is a parameterless procedure and acts as a spec,
      --  then create a main method with the appropriate args parameter that
      --  contains calls to adainit, the Ada main subprogram, and adafinal.

      if VM_Target /= CLI_Target
        and then Ekind (Subp_Entity) = E_Procedure
        and then not Present (First_Formal (Subp_Entity))
        and then Acts_As_Spec (Subp)
      then
         Get_Name_String (Chars (Subp_Entity));
         Binder_Class :=
           New_Class
             (Ada_Ent => Empty,
              Name    => Name ("ada_" & Name_Buffer (1 .. Name_Len)));

         Generate_Main_Entry_Point
           (JVM_Method (Subp_Entity),
            New_Method
              (Binder_Class, Name ("adainit"), Void_Type, Static => True),
            New_Method
              (Binder_Class, Name ("adafinal"), Void_Type, Static => True),
            Call_Main => True);
      end if;

      pragma Assert (Method_Stack.Empty);

      --  Close any pending classes that may have been generated

      while Class_Stack.Top /= Subp_Class loop
         Other_Class := Class_Stack.Top;
         Generate_Null_Methods (Other_Class);
         End_Class_File (Other_Class);
         Class_Stack.Pop;
      end loop;

      pragma Assert (Class_Stack.Top = Subp_Class);

      Generate_Null_Methods (Subp_Class);
      End_Class_File (Subp_Class);
      Class_Stack.Pop;
   end Generate_Subprogram_Class;

   -------------------------------
   -- Translate_Subprogram_Call --
   -------------------------------

   procedure Translate_Subprogram_Call (Call : Node_Id) is
      Subp_Name       : constant Node_Id := Name (Call);
      Access_Call     : Boolean := False;
      Actual          : Node_Id;
      Actual_Addr     : Address_Descriptor;
      Constructor     : Boolean := False;
      Constr_Class    : Class_Id;
      Cntrl_Formal    : Entity_Id := Empty;
      Control_Arg     : Node_Id;
      Control_Type    : Entity_Id;
      Formal          : Entity_Id;
      Formal_Type     : Entity_Id;
      Is_Slice        : Boolean;
      Jtype           : Type_Id;
      Slice_Prefix    : Node_Id;
      Slice_Subt      : Entity_Id;
      Static_Link_Set : Boolean := False;
      Subp            : Entity_Id;
      Subp_Method     : Method_Id;
      Value_Copy      : Local_Var_Id := Null_Local_Var;

      --  Missing documentation on this structure???
      --  When are these variables needed???

      type Param_Info is record
         Addr_Kind : Address_Kind := No_Address;
         Copy_Var  : Local_Var_Id := Null_Local_Var;
         Ref_Var   : Local_Var_Id := Null_Local_Var;
         Field_Var : Field_Id     := Null_Field;
         Index_Var : Local_Var_Id := Null_Local_Var;
      end record;

      Max_Params   : constant := 255;  -- Max parameter words allowed by JVM
      Temp_Var     : array (1 .. Max_Params) of Param_Info;
      Temp_Count   : Integer := 0;

      procedure Gen_Save_Static_Link;
      --  For dispatching calls to nested primitives generate code that saves
      --  the static link in an extra field of the dispatching object. When
      --  this subprogram is called the dispatching object is in the top of
      --  the active stack.

      --------------------------
      -- Gen_Save_Static_Link --
      --------------------------

      procedure Gen_Save_Static_Link is
         Subp_Class : constant Class_Id := Class_Of (Subp_Method);

      begin
         --  Nothing to do if already set

         if Static_Link_Set then
            pragma Assert (False);
            return;

         --  Nothing to do when calling non-nested subprograms

         elsif not Present (Enclosing_Subprogram (Subp)) then
            return;

         --  Nothing to do for non-dispatching calls

         elsif not Is_Dispatching_Operation (Subp) then
            return;

         --  For dispatching calls through interface types we do not need to
         --  save the static link; we just propagate the static link value
         --  contained in the object.

         elsif Is_Interface (Subp_Class) then
            return;

         --  Nothing to do for imported subprograms because they are library
         --  level subprograms???

         elsif Is_Imported (Subp) then
            pragma Assert (False);
            return;

         --  In the JVM_Target, dispatching calls through access types don't
         --  have available the static link. Why??? Delegates???

         elsif VM_Target = JVM_Target and then Access_Call then
            return;

         else
            pragma Assert (JVM.Type_Kind (Top_Type) = Class_Kind);
            Gen_Duplicate; --  duplicate 'this'

            Load_Static_Link (Enclosing_Method (Subp));
            Gen_Put_Object_Field (Field (Subp_Class, Name ("__AR_SL")));

            Static_Link_Set := True;
         end if;
      end Gen_Save_Static_Link;

   --  Start of processing for Translate_Subprogram_Call

   begin
      case Nkind (Subp_Name) is
         when N_Identifier | N_Expanded_Name | N_Operator_Symbol =>
            Subp        := Entity (Subp_Name);
            Subp_Method := JVM_Method (Subp);

            if Is_Eliminated (Subp) then
               Error_Msg_NE
                 ("cannot call eliminated subprogram &!",
                  Call, Entity (Subp_Name));

            elsif Subp_Method = Null_Method
              and then not (Convention (Subp) = Convention_Assembler
                            or else Convention (Subp) = Convention_Intrinsic)
            then
               --  ??? should never happen, but currently does, e.g. in the
               --  context of interfaces, so generate an error message instead
               --  of crashing

               Error_Msg_N ("unsupported construct in this context", Call);
            end if;

         when N_Explicit_Dereference =>
            Access_Call := True;
            Subp := Directly_Designated_Type (Etype (Prefix (Subp_Name)));

            --  in normal case, we evaluate the subp expression, to access
            --  the Invoke method later.
            Evaluate_Expr (Prefix (Subp_Name));
            Subp_Method :=
              Method (JVM_Class (Etype (Prefix (Subp_Name))), "Invoke");

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      --  Calls to intrinsic assembly operations require specialized treatment

      if Convention (Subp) = Convention_Assembler
        or else Convention (Subp) = Convention_Intrinsic
      then
         --  Various calls such as to addressing operations (e.g., To_Pointer,
         --  address arithmetic) are not supported in general and will be
         --  caught by this warning check.

         if (not Is_Imported (Subp)
            or else not Present (Interface_Name (Subp)))
            and then Chars (Subp) /= Name_Op_Add
         then
            Error_Msg_N
              ("intrinsic subprogram call not supported?", Call);

         else
            --  The code for supporting the special "+" conversion
            --  operations on Java Strings is implemented here but
            --  not used yet, due to some front end issues that need
            --  to be resolved that currently prevent the operations
            --  from being declared as intrinsic. ???

            --  the comment above is no longer true.  By using
            --  pragma Convention (Intrinsic,"+") and then
            --  checking Chars instead of Interface_Name here, we
            --  can fix this

            if Chars (Subp) = Name_Op_Add then

               --  This is the special "+" operation for converting
               --  from an Ada String to a java.lang.String. Generate
               --  a call to the String constructor that takes a byte
               --  array and 'hibyte' parameter (zero).

               if Etype (First_Formal (Subp)) = Standard_String
                 and then Ekind (Etype (Subp)) in Access_Kind
               then
                  --  Slices are passed by copy to greatly simplify handling
                  --  their bounds. A temporary is created and the sliced
                  --  portion is assigned. In the case of out and in-out
                  --  parameters, we have to copy back after the call.

                  Test_For_Slice
                    (First_Actual (Call), Is_Slice, Slice_Prefix, Slice_Subt);

                  if Is_Slice then
                     Actual := First_Actual (Call);

                     declare
                        Slice_Temp : constant Local_Var_Id :=
                                       New_Local_Var
                                         ("_tsc_slicetmp1", JVM_Type (Actual));

                     begin
                        Load_Index_Length (First_Index (Slice_Subt));
                        Gen_New_Array (Actual);
                        Gen_Store_Local (Slice_Temp);

                        --  The actual array slice must be copied in.

                        Evaluate_Expr (Actual);

                        Gen_Array_Subscript
                          (Slice_Prefix, Index_First (Slice_Subt));

                        --  Now copy the slice into the temporary

                        Gen_Load_Local (Slice_Temp);
                        Gen_Push_Int (Uint_0);
                        Gen_Load_Local (Slice_Temp);
                        Gen_Array_Length;
                        Gen_Invoke_API_Method (System_arraycopy);

                        --  Finally, pass the reference to the slice
                        --  temporary

                        Gen_Load_Local (Slice_Temp);
                     end;

                  --  if not a slice, just evaluate the actual

                  else
                     Evaluate_Expr (First_Actual (Call));
                  end if;

                  --  Make sure that an actual array is on the stack, not an
                  --  array descriptor.

                  if Is_Descriptor (Top_Type) then
                     Gen_Get_Field (Descriptor_Field (Top_Type));
                  end if;

                  Gen_Invoke_API_Method (To_JVM_String);

               --  This is the special "+" operation for converting
               --  from a java.lang.String to an Ada String. Generate
               --  a call to the getBytes method to retrieve the
               --  Ada String value from a Java String.

               elsif Name_String (Chars (Etype (Subp))) = "string"
                 and then Ekind (Etype (First_Formal (Subp))) in Access_Kind
               then
                  Evaluate_Expr (First_Actual (Call));
                  Gen_Invoke_API_Method (String_getBytes);

               --  "+" functions used to box/unbox valuetypes
               --  This one handles boxing

               elsif Is_Value_Type (Etype (Subp))
                 and then Ekind (Etype (First_Formal (Subp))) in Access_Kind
               then
                  Gen_Box (JVM_Type (Subp));

               --  This one handles unboxing
               elsif Is_Value_Type (Etype (First_Formal (Subp)))
                 and then Ekind (Etype (Subp)) in Access_Kind
               then
                  Gen_Unbox (JVM_Type (First_Formal (Subp)));

               else
                  pragma Assert (False);
                  raise Program_Error;
               end if;

               return;
            end if;

            declare
               Java_Name : constant String
                 := Str (Strval (Interface_Name (Subp)));
            begin
               if Java_Name = "current_target_exception" then
                  Gen_Load_Current_Exception;

               elsif Java_Name = "monitorenter" then
                  Evaluate_Expr (First_Actual (Call));
                  Gen_Monitor_Enter;

               elsif Java_Name = "monitorexit" then
                  Evaluate_Expr (First_Actual (Call));
                  Gen_Monitor_Exit;

               elsif Java_Name = "throw" then
                  Evaluate_Expr (First_Actual (Call));
                  Gen_Exception_Throw;

               else
                  Error_Msg_Name_1 := Name (Java_Name);

                  if Convention (Subp) = Convention_Assembler then
                     Error_Msg_N
                       ("unsupported assembly subprogram: %%", Subp);
                  else
                     Error_Msg_N
                       ("unsupported intrinsic subprogram: %%", Subp);
                  end if;
               end if;

               --  Processing is complete

               return;
            end;
         end if;
      end if;

      --  If the subprogram is marked as a constructor then generate a new
      --  object of the constructor's class and push it on the stack.

      if Is_Constructor (Subp) then

         --  For now we only support the case of function constructors

         pragma Assert (Ekind (Subp) = E_Function);

         if Is_Value_Type (Etype (Subp))
           or else Ekind (Etype (Subp)) = E_Access_Subprogram_Type
         then
            Constr_Class := Class_Of_Type (JVM_Type (Etype (Subp)));
         else
            Constr_Class :=
              Class_Of_Type
                (JVM_Type (Directly_Designated_Type (Etype (Subp))));
         end if;

         --  Search for a formal named 'this'

         Actual       := First_Actual (Call);
         Cntrl_Formal := First_Formal (Subp);

         while Present (Cntrl_Formal)
           and then Name_String (Chars (Cntrl_Formal)) /= "this"
         loop
            Next_Actual (Actual);
            Next_Formal (Cntrl_Formal);
         end loop;

         if Present (Cntrl_Formal) then

            --  If the 'this' formal's corresponding actual is a null literal,
            --  then a new object is created and duplicated so that a reference
            --  to it will remain on the stack after the constructor call.

            if Nkind (Actual) = N_Null
              or else (Nkind (Actual) = N_Unchecked_Type_Conversion
                        and then Nkind (Expression (Actual)) = N_Null)
            then
               if VM_Target = JVM_Target then
                  Gen_New_Object (Constr_Class);
                  Gen_Duplicate;
               else
                  Constructor := True;
               end if;

            --  If the actual for the 'this' formal is not a null literal,
            --  then evaluate the actual and duplicate it. This occurs for
            --  cases of calls to a constructor's parent type constructor.

            else
               Evaluate_Expr (Actual);
               Gen_Duplicate;
            end if;

         elsif Is_Value_Type (Etype (Subp)) then
            Value_Copy :=
              New_Local_Var ("_tsc_valtemp", JVM_Type (Etype (Subp)));
            Gen_Load_Local_Address (Value_Copy);

            --  Replace the type stored in the active_stack with the type of
            --  the associated variable.

            Pop_Type;
            Push_Type (JVM_Type (Etype (Subp)));

         --  If the function has no formal named 'this', then we generate
         --  and duplicate the new object.

         else
            if VM_Target = JVM_Target then
               Gen_New_Object (Constr_Class);
            else
               Gen_Default_Object (Constr_Class);
            end if;

            Gen_Duplicate;
         end if;

         Gen_Save_Static_Link;

      --  If this is a call to a nested method, then pass a reference to
      --  the activation record as dispatching argument.

      elsif VM_Target = CLI_Target
        and then Present (Enclosing_Subprogram (Subp))
        and then not Is_Imported (Subp)
        and then not Is_Dispatching_Operation (Subp)
        and then Ekind (Subp) /= E_Subprogram_Type
      then
         Load_Static_Link (Enclosing_Method (Subp));

      --  If this is a call to a dispatching operation, then an
      --  appropriate value of the dispatching type must be pushed
      --  to control the dispatch (this value will correspond to
      --  the method's 'this' argument).  If the call is dispatching,
      --  then the first argument must be the value of the first
      --  controlling argument of the call (the reevaluation of
      --  that actual is suppressed when encountering it below
      --  within the main loop over actuals).
      --  Watch also for valuetype return which is not really dispatching

      elsif Is_Dispatching_Operation (Subp)
        and then
          (Convention (Subp) /= Convention_VM
            or else Ekind (Subp) /= E_Function
            or else not Has_Controlling_Result (Subp))
      then
         Control_Type := Full_Type (Find_Dispatching_Type (Subp));

         if Is_Value_Type (Control_Type) then
            --  Valuetypes are evaluated later with actuals evaluations.
            Cntrl_Formal := Empty;

         else
            Actual       := First_Actual (Call);
            Cntrl_Formal := First_Formal (Subp);

            while Present (Cntrl_Formal)
              and then not Is_Controlling_Formal (Cntrl_Formal)
            loop
               Next_Actual (Actual);
               Next_Formal (Cntrl_Formal);
            end loop;

            if Present (Cntrl_Formal) then
               Evaluate_Expr (Actual);

               --  If the subprogram has no controlling formal, then this must
               --  be a call to a function with controlling result, in which
               --  case we still have to pass a controlling actual for the
               --  'this' argument, obtained from the Controlling_Argument
               --  of the call. If the function call has no controlling
               --  argument, then it must be a nondispatching call, and
               --  in that case we have to construct a dummy object of the
               --  type to pass as an actual (the type is guaranteed to be
               --  nonabstract, so this will always work).

            else
               Control_Arg  := Controlling_Argument (Call);

               if Present (Control_Arg) then
                  --  VM don't dispatch on 'Tag, but on actual object (the
                  --  'this' parameter), so we generate the default object in
                  --  this case.
                  if Nkind (Control_Arg) = N_Attribute_Reference
                    and then Get_Attribute_Id (Attribute_Name (Control_Arg))
                               = Attribute_Tag
                  then
                     Gen_Default_Object
                       (JVM_Class
                          (Underlying_Type (Etype (Prefix (Control_Arg)))));
                  else
                     Evaluate_Expr (Control_Arg);
                  end if;

               else
                  Gen_Default_Object (JVM_Class (Control_Type));
               end if;
            end if;

            Gen_Save_Static_Link;
         end if;
      end if;

      Formal := First_Formal (Subp);
      Actual := First_Actual (Call);

      --  Note on the check on the availability of Formal in the following
      --  loop: Extra formals of build in place functions that are called
      --  through access to subprograms are not directly available in the tree.
      --  Given that this case is still unsupported (and the profile of the
      --  invoked JVM method does not have those extra arguments) currently we
      --  just avoid processing these extra formals to avoid crashing the
      --  backend. More work needed in this area???

      while Present (Actual)
        and then Present (Formal)
      loop
         Formal_Type := Full_Type (Etype (Formal));

         --  Slices are passed by copy to greatly simplify handling their
         --  bounds. A temporary is created and the sliced portion is assigned.
         --  In the case of out and in out parameters, we have to copy back
         --  after the call.

         Test_For_Slice (Actual, Is_Slice, Slice_Prefix, Slice_Subt);

         if Is_Slice then
            declare
               --  The actual array slice must be copied into the slice
               --  temporary unless the formal is an out-mode parameter and the
               --  array has scalar components. (Out parameters with nonscalar
               --  components may have defaults in which case Ada requires copy
               --  in, plus arrays of composite components contain references
               --  that always need to be copied across.)

               Copy_In         : constant Boolean :=
                                   Ekind (Component_Type (Slice_Subt))
                                     not in Scalar_Kind
                                   or else Ekind (Formal) /= E_Out_Parameter;
               End_Lbl         : constant Label_Id := New_Label;
               Slice_Length    : constant Local_Var_Id :=
                                   New_Local_Var
                                     ("_tsc_slicetmp2_len", Int_Type);
               Slice_Temp      : constant Local_Var_Id :=
                                   New_Local_Var
                                     ("_tsc_slicetmp2", JVM_Type (Actual));
               Slice_Index     : constant Local_Var_Id :=
                                   New_Local_Var ("_tsc_indextmp", Int_Type);
               Slice_Actual    : Local_Var_Id := Null_Local_Var;
               Array_Temp      : Local_Var_Id := Null_Local_Var;

            begin
               Load_Index_Length (First_Index (Slice_Subt));

               Gen_Duplicate;
               Gen_Store_Local (Slice_Length);

               Gen_New_Array   (Actual);
               Gen_Store_Local (Slice_Temp); --  dst

               --  Evaluate the slice

               Evaluate_Expr (Actual);

               --  Evaluate the slice index

               Gen_Array_Subscript
                 (Prefix          => Slice_Prefix,
                  First_Subscript => Index_First (Slice_Subt));
               Gen_Store_Local (Slice_Index);

               Slice_Actual := New_Local_Var ("_tsc_slice_act", Top_Type);
               Gen_Store_Local (Slice_Actual);

               --  Save the base of the slice in a temporary if it requires
               --  copy back

               if Ekind_In (Formal, E_Out_Parameter,
                                    E_In_Out_Parameter)
               then
                  --  Save the location of the temporary as well as the array
                  --  reference and index value so we can copy back into the
                  --  actual slice after the call.

                  Gen_Load_Local (Slice_Actual);  --  src

                  Array_Temp := New_Local_Var ("_tsc_arraytmp", Top_Type);
                  Gen_Store_Local (Array_Temp);   --  src'

                  Temp_Count := Temp_Count + 1;
                  Temp_Var (Temp_Count).Ref_Var   := Array_Temp;  --  src'
                  Temp_Var (Temp_Count).Index_Var := Slice_Index; --  srcOffset
                  Temp_Var (Temp_Count).Copy_Var  := Slice_Temp;  --  dst
                  Temp_Var (Temp_Count).Addr_Kind := No_Address;
               end if;

               --  Handle ultra-flat slice

               declare
                  State : Boolean;
               begin
                  Suppress_Stack_Checking (State);
                  Gen_Load_Local (Slice_Length);
                  Gen_Branch_Equal (End_Lbl);
                  Restore_Stack_Checking (State);
               end;

               --  Now copy the slice into the temporary

               if Copy_In then
                  Gen_Load_Local (Slice_Actual);  --  src
                  Gen_Load_Local (Slice_Index);   --  srcOffset
                  Gen_Load_Local (Slice_Temp);    --  dst
                  Gen_Push_Int   (Uint_0);        --  dstOffset
                  Gen_Load_Local (Slice_Length);  --  length

                  Gen_Invoke_API_Method (System_arraycopy);
               end if;

               --  Generate label of ultra-flat slices management

               declare
                  State : Boolean;
               begin
                  Suppress_Stack_Checking (State);
                  Gen_Label (End_Lbl);
                  Restore_Stack_Checking (State);
               end;

               --  Finally, pass the reference to the slice temporary

               Gen_Load_Local (Slice_Temp);
            end;

         --  Elementary formal of mode out require passing the actual by copy.
         --  The address of the actual has to be saved in a temporary and not
         --  reevaluated on the copy back.

         --  We create an object of the appropriate wrapper type and pass a
         --  reference to the wrapper, copying the actual value in for mode in
         --  out (and for mode out access parameters), and copying back from
         --  the wrapper object to the actual for both out and in out.

         elsif Ekind (Formal) /= E_In_Parameter
           and then Ekind (Formal_Type) in Elementary_Kind
         then
            Temp_Count := Temp_Count + 1;

            --  Note: We can't apply JVM_Type to Formal in this case because
            --  the JVM type associated with Formal_Type is a scalar type
            --  (e.g., int) rather than the wrapper type, so instead we have to
            --  get the wrapper type from the local variable associated with
            --  Formal.

            Jtype := Type_Of (JVM_Local_Var (Formal));

            --  Create the wrapper object for the formal parameter and
            --  duplicate the address so that it's on the stack for the call.

            Temp_Var (Temp_Count).Copy_Var :=
              New_Local_Var ("_tsc_out_tmp", Jtype);

            Gen_Default_Object (Class_Of_Type (Jtype));
            Gen_Store_Local (Temp_Var (Temp_Count).Copy_Var);

            Actual_Addr := Evaluate_Addr (Actual);

            Temp_Var (Temp_Count).Addr_Kind := Actual_Addr.Addr_Kind;

            --  For each kind of actual, save the result of any prefix or index
            --  calculation, and store the value of the actual into the wrapper
            --  object if needed (i.e., if the mode is in-out or if the formal
            --  is of an access type).

            case Actual_Addr.Addr_Kind is
               when Local_Address =>
                  if Ekind (Formal) = E_In_Out_Parameter
                    or else Ekind (Formal_Type) in Access_Kind
                  then
                     Gen_Load_Local (Temp_Var (Temp_Count).Copy_Var);
                     Gen_Load_Local (Actual_Addr.Local_Var);
                     pragma Assert (Is_Descriptor (Top_Type (Disp => 1)));
                     Gen_Put_Field  (Descriptor_Field (Jtype));
                  end if;

               when Field_Address =>

                  --  If the computed actual address does not have immunity
                  --  to possible side effects, then save it in a temp to
                  --  prevent reevaluation on copy back. (Indexed components
                  --  and dereferences can show up as field addresses when
                  --  the actual denotes a wrapped object.)

                  if Nkind (Actual) = N_Indexed_Component
                    or else
                      (Nkind_In (Actual,
                                 N_Selected_Component,
                                 N_Explicit_Dereference))
                  then
                     Temp_Var (Temp_Count).Ref_Var :=
                       New_Local_Var ("_tsc_ref_tmp", Top_Type);
                     Gen_Store_Local (Temp_Var (Temp_Count).Ref_Var);
                  else
                     Temp_Var (Temp_Count).Ref_Var := Null_Local_Var;
                  end if;

                  Temp_Var (Temp_Count).Field_Var := Actual_Addr.Field;

                  if Ekind (Formal) = E_In_Out_Parameter
                    or else Ekind (Formal_Type) in Access_Kind
                  then
                     Gen_Load_Local (Temp_Var (Temp_Count).Copy_Var);

                     if Temp_Var (Temp_Count).Ref_Var /= Null_Local_Var then
                        Gen_Load_Local (Temp_Var (Temp_Count).Ref_Var);

                     --  If a reference to the actual wasn't saved in a
                     --  temporary then we need to swap the addresses
                     --  before loading the actual value. (Also, if the
                     --  actual is a static field (i.e., global variable),
                     --  then it was not loaded by Evaluate_Addr.)

                     elsif not Is_Static (Actual_Addr.Field) then
                        Gen_Swap;
                     end if;

                     Gen_Get_Field (Actual_Addr.Field);
                     pragma Assert (Is_Descriptor (Top_Type (Disp => 1)));
                     Gen_Put_Field (Descriptor_Field (Jtype));

                  --  If the formal has mode out and the actual address
                  --  was not saved in a reference temporary, then we need
                  --  to pop off the address pushed earlier by Evaluate_Addr.
                  --  Would be nice to avoid this inefficiency, but it's
                  --  it's not clear how to cleanly avoid the earlier address
                  --  evaluation. ???

                  elsif Ekind (Formal) = E_Out_Parameter
                    and then Temp_Var (Temp_Count).Ref_Var = Null_Local_Var
                    and then not Is_Static (Actual_Addr.Field)
                  then
                     Gen_Pop;
                  end if;

               when Indexed_Address =>
                  Temp_Var (Temp_Count).Ref_Var :=
                    New_Local_Var ("_tsc_ref_tmp", JVM_Type (Prefix (Actual)));

                  Temp_Var (Temp_Count).Index_Var :=
                    New_Local_Var
                      ("_tsc_index_tmp",
                       JVM_Type (Full_Type (First (Expressions (Actual)))));

                  Gen_Store_Local (Temp_Var (Temp_Count).Index_Var);
                  Gen_Store_Local (Temp_Var (Temp_Count).Ref_Var);

                  if Ekind (Formal) = E_In_Out_Parameter
                    or else Ekind (Formal_Type) in Access_Kind
                  then
                     Gen_Load_Local (Temp_Var (Temp_Count).Copy_Var);
                     Gen_Load_Local (Temp_Var (Temp_Count).Ref_Var);
                     Gen_Load_Local (Temp_Var (Temp_Count).Index_Var);
                     Gen_Load_Array_Element;

                     pragma Assert (Is_Descriptor (Top_Type (Disp => 1)));
                     Gen_Put_Field (Descriptor_Field (Jtype));
                  end if;

               when Array_Address =>
                  Gen_Store_Local (Temp_Var (Temp_Count).Copy_Var);

               when others =>
                  pragma Assert (False);
                  raise Program_Error;
            end case;

            Gen_Load_Local (Temp_Var (Temp_Count).Copy_Var);

         --  If this is an access-to-unconstrained-array parameter for an
         --  imported subprogram with convention Java, then dereference
         --  the "all" field of the actual's compound pointer so as to pass
         --  just the reference to the array without passing the bounds
         --  (unless the access type comes from a Java-convention unit,
         --  in which case it's already a simple array reference).

         elsif Convention (Subp) = Convention_VM
           and then Ekind (Full_Type (Actual)) in Access_Kind
           and then Convention (Scope (Full_Type (Actual))) /= Convention_VM
           and then Ekind (Designated_Type (Full_Type (Actual)))
                      in Einfo.Array_Kind
           and then not Is_Constrained (Designated_Type (Full_Type (Actual)))
         then
            --  When the actual is an access attribute this is a bit
            --  inefficient since there's no need to construct the
            --  compound pointer in that case. We should eventually
            --  add special treatment for that case. ???

            Evaluate_Expr (Actual);
            pragma Assert (Is_Descriptor (Top_Type));
            Gen_Get_Field (Descriptor_Field (JVM_Type (Full_Type (Actual))));

         --  Evaluate the actual, unless Cntrl_Formal was set earlier
         --  and matches the current Formal, in which case the actual
         --  was evaluated earlier as the 'this' argument to control
         --  a dispatching call.

         elsif not Present (Cntrl_Formal)
            or else Formal /= Cntrl_Formal
         then
            --  Call Evaluate_Array_Address in the case of a array
            --  actual for an imported subprogram to ensure that
            --  explicit dereferences are handled properly (load of
            --  ".all" field).

            if Ekind (Formal_Type) in Einfo.Array_Kind
              and then (Is_Imported (Subp) or else Is_Exported (Subp))
            then
               Evaluate_Array_Address (Actual);

            --  This is a special handling for delegate constructors with
            --  dispatching calls. In this case, the evaluation of the
            --  dispatching method address needs to be done on the dispatching
            --  method, not the ND one.

            elsif Name_String (Chars (Etype (Formal))) = "native_int"
              and then Is_Imported (Subp)
              and then Nkind (Actual) = N_Attribute_Reference
              and then Get_Attribute_Id (Attribute_Name (Actual)) =
                        Attribute_Address
              and then Nkind (Prefix (Actual)) = N_Identifier
              and then Ekind (Entity (Prefix (Actual))) in
                        Einfo.Subprogram_Kind
            then
               declare
                  Method : constant Method_Id :=
                             JVM_Method (Entity (Prefix (Actual)));
               begin
                  if Has_Nondispatching_Method (Entity (Prefix (Actual))) then
                     Gen_Load_Function_Pointer (Next_Method (Method));
                  else
                     Gen_Load_Function_Pointer (Method);
                  end if;
               end;

            --  Dispatching calls with value type parameters expect an address
            --  to the formal type on the stack

            elsif Is_Dispatching_Operation (Subp)
              and then Is_Controlling_Formal (Formal)
              and then Is_Value_Type (Formal_Type)
            then
               if Value_Copy = Null_Local_Var then
                  Actual_Addr := Evaluate_Addr (Actual);

                  case Actual_Addr.Addr_Kind is
                     when Local_Address =>
                        Gen_Load_Local_Address (Actual_Addr.Local_Var);

                     when Valuetype_Address =>
                        null;

                     when Field_Address =>
                        Gen_Get_Object_Field_Address (Actual_Addr.Field);

                     when others =>
                        pragma Assert (False);
                        raise Program_Error;
                  end case;
               end if;

            else
               Evaluate_Expr (Actual, Check_Subtype => Etype (Formal));

               --  The previous evaluation of the actual may have have left in
               --  the top of the stack an array descriptor (required to handle
               --  out and inout arrays). However, if the actual requires no
               --  descriptor then we must pass the pointer to the base of the
               --  array.

               --  In case of conversion of an unconstrained array no action is
               --  required because the subsequent call to Load_Array_Bounds
               --  will take care of this case.

               if Nkind (Actual) = N_Type_Conversion
                 and then not Is_Constrained (Full_Subtype (Actual))
               then
                  null;

               elsif Is_Array_Type (Etype (Formal))
                 and then Is_Constrained (Etype (Formal))
                 and then not Is_Constrained (Etype (Actual))
                 and then Is_Array_Descriptor (Top_Type)
               then
                  Gen_Get_Field (Descriptor_Field (Top_Type));
               end if;
            end if;
         end if;

         --  If the formal subtype is an unconstrained array subtype, then pass
         --  the bounds of the actual as the subsequent parameters, except when
         --  the subprogram is imported.

         if Ekind (Formal_Type) in Einfo.Array_Kind
           and then not Is_Constrained (Full_Subtype (Formal))
           and then not Is_Imported (Subp)
           and then not Is_Exported (Subp)
         then
            Load_Array_Bounds (Actual, Number_Dimensions (Full_Type (Actual)));
         end if;

         Formal := Next_Formal_With_Extras (Formal);
         Actual := Next_Actual (Actual);
      end loop;

      --  Addition of the static link as an extra formal (if required)

      if Has_AR_SL_Formal (Subp_Method) then
         pragma Assert (VM_Target = JVM_Target
           and then Present (Enclosing_Subprogram (Subp))
           and then not Is_Imported (Subp)
           and then not Is_Dispatching_Operation (Subp));

         Load_Static_Link (Enclosing_Method (Subp));
      end if;

      if not Is_Dispatching_Operation (Subp) then
         if not Constructor then
            --  Cast of the exception into Object required by the verifier

            --  Gen_Conversion (Java_Lang_Object_Type);
            Gen_Invoke_Method (Subp_Method);
         else
            --  Call a constructor instead of invoking a method
            --  clear the flag so we know that we've covered all paths
            --  from the setting above

            Gen_New_Object (Constr_Class, Subp_Method);
            Constructor := False;
         end if;

      else
         --  If this is a dispatching call, and the subprogram has
         --  an associated nondispatching method, then generate a
         --  call to the successor of Subp_Method (which by convention
         --  must be the subprogram's dispatching method). Otherwise
         --  simply generate a call to Subp_Method, which will normally
         --  be the nondispatching method of the subprogram, except for
         --  the case where the subprogram is declared in a Java-convention
         --  scope so that it has no nondispatching method (so that case
         --  will result in a dispatching call).

         if Present (Controlling_Argument (Call)) then
            if Has_Nondispatching_Method (Subp) then
               Gen_Invoke_Method (Next_Method (Subp_Method));
            else
               Gen_Invoke_Method (Subp_Method);
            end if;

         --  If the called subprogram is declared in a Java-convention
         --  scope and we're inside a method of a class belonging to
         --  an immediate descendant of the called subprogram's controlling
         --  class, then generate an invokespecial to make a nondispatching
         --  call to the superclass's method. Otherwise we have no choice
         --  but to make a dispatching call to the method, even though
         --  that doesn't match Ada nondispatching semantics. ???

         elsif Convention (Scope (Subp)) = Convention_VM then
            --  Call to a superclass method

            if Superclass (Class_Of (Current_Method))
                 = Class_Of (Subp_Method)
            then
               Gen_Invoke_Special (Subp_Method);

            elsif Is_Parent_Class
                 (Class_Of (Subp_Method), Child => Class_Of (Current_Method))
            then
               --  Call to a higher parent, so warn user about nonstandard
               --  call semantics

               --  In CIL, we can perform non dispatching calls in this case,
               --  so we still respect Ada semantic, no need to warn.
               --  In JAVA however, the call is dispatching, so display a
               --  warning.

               if not GNAT_Mode and then VM_Target /= CLI_Target then
                  Error_Msg_N ("call will invoke superclass method?", Call);
               end if;

               Gen_Invoke_Special (Subp_Method);

            --  Allow the call, which will be a virtual call, even though
            --  the Ada call should be nondispatching, but give a warning.

            else
               --  For a valuetype, we did a load address, then called
               --  the constructor to put it into a temp, now we will
               --  copy it into the actual, so load it.

               if Value_Copy /= Null_Local_Var then
                  Set_Class
                    (Subp_Method, Class_Of_Type (Type_Of (Value_Copy)));
                  Gen_Invoke_Method (Subp_Method);
                  Gen_Load_Local (Value_Copy);

               elsif Superclass (Class_Of (Subp_Method)) /= Null_Class
                 and then Is_Value_Type (Class_Of (Subp_Method))
               then
                  Gen_Invoke_Method (Subp_Method);
               elsif not GNAT_Mode then
                  Error_Msg_N ("call to Java method will dispatch?", Call);
                  Gen_Invoke_Method (Subp_Method);
               else
                  Gen_Invoke_Method (Subp_Method);
               end if;
            end if;

         --  Normal nondispatching call case
         else
            Gen_Invoke_Method (Subp_Method);
         end if;
      end if;

      --  If this is a call to a dispatching function with a controlling
      --  controlling result, then the result JVM type may actually be
      --  one of the tagged type's parent types (in the case where the
      --  function is overriding), and so we may need to cast the result
      --  to the JVM class associated with function's Ada result type
      --  to avoid type mismatches with the expected type context.

      --  Got rid of conversions for convention_vm as this might be a
      --  constructor for a CIL class

      if Is_Dispatching_Operation (Subp)
        and then Ekind (Subp) = E_Function
        and then Has_Controlling_Result (Subp)
        and then Convention (Subp) /= Convention_VM
      then
         Gen_Conversion (JVM_Type (Etype (Subp)));
      end if;

      --  Out and in out mode parameters must be copied back into the
      --  elementary and array slice actuals after return from the call.

      if Temp_Count > 0 then
         Temp_Count := 0;

         Formal := First_Formal (Subp);
         Actual := First_Actual (Call);

         while Present (Actual) loop
            if Ekind (Formal) /= E_In_Parameter then
               Test_For_Slice (Actual, Is_Slice, Slice_Prefix, Slice_Subt);

               if Is_Slice then
                  Temp_Count := Temp_Count + 1;
                  Gen_Load_Local (Temp_Var (Temp_Count).Copy_Var);
                  Gen_Push_Int (Uint_0);
                  Gen_Load_Local (Temp_Var (Temp_Count).Ref_Var);
                  Gen_Load_Local (Temp_Var (Temp_Count).Index_Var);
                  Gen_Load_Local (Temp_Var (Temp_Count).Copy_Var);
                  Gen_Array_Length;
                  Gen_Invoke_API_Method (System_arraycopy);

               elsif Ekind (Full_Type (Formal)) in Elementary_Kind then
                  Temp_Count := Temp_Count + 1;
                  Jtype := Type_Of (JVM_Local_Var (Formal));

                  case Temp_Var (Temp_Count).Addr_Kind is
                     when Local_Address =>
                        Actual_Addr := Evaluate_Addr (Actual);
                        Gen_Load_Local (Temp_Var (Temp_Count).Copy_Var);
                        pragma Assert (Is_Descriptor (Top_Type));
                        Gen_Get_Field  (Descriptor_Field (Jtype));

                        --  The 'all' component is of class Object, so a
                        --  downcast to the formal type is needed to satisfy
                        --  consistency checking of the operand stack.

                        if Ekind (Full_Type (Formal)) in Access_Kind then
                           Gen_Check_Cast (JVM_Type (Full_Type (Formal)));
                        end if;

                        Gen_Store_Local (Actual_Addr.Local_Var);

                     when Field_Address =>
                        if Temp_Var (Temp_Count).Ref_Var /= Null_Local_Var then
                           Gen_Load_Local (Temp_Var (Temp_Count).Ref_Var);
                        else
                           Actual_Addr := Evaluate_Addr (Actual);
                        end if;

                        Gen_Load_Local (Temp_Var (Temp_Count).Copy_Var);
                        pragma Assert (Is_Descriptor (Top_Type));
                        Gen_Get_Field (Descriptor_Field (Jtype));

                        --  The 'all' component is of class Object, so a
                        --  downcast to the formal type is needed to satisfy
                        --  consistency checking of the operand stack.

                        if Ekind (Full_Type (Formal)) in Access_Kind then
                           Gen_Check_Cast (JVM_Type (Full_Type (Formal)));
                        end if;

                        Gen_Put_Field (Temp_Var (Temp_Count).Field_Var);

                     when Indexed_Address =>
                        Gen_Load_Local (Temp_Var (Temp_Count).Ref_Var);
                        Gen_Load_Local (Temp_Var (Temp_Count).Index_Var);
                        Gen_Load_Local (Temp_Var (Temp_Count).Copy_Var);
                        pragma Assert (Is_Descriptor (Top_Type));
                        Gen_Get_Field  (Descriptor_Field (Jtype));

                        --  The 'all' component is of class Object, so a
                        --  downcast to the formal type is needed to satisfy
                        --  consistency checking of the operand stack.

                        if Ekind (Full_Type (Formal)) in Access_Kind then
                           Gen_Check_Cast (JVM_Type (Full_Type (Formal)));
                        end if;

                        Gen_Store_Array_Element;

                     when Array_Address =>
                        null;

                     when others =>
                        pragma Assert (False);
                        raise Program_Error;
                  end case;
               end if;
            end if;

            Formal := Next_Formal_With_Extras (Formal);
            Actual := Next_Actual (Actual);
         end loop;
      end if;

      --  Make sure that if we set constructor that it got cleared
      --  (i.e. we really called a constructor above)
      pragma Assert (Constructor = False);
   end Translate_Subprogram_Call;

end Jx_Ch6;
