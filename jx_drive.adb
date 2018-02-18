------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J X _ D R I V E                              --
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

with Atree;    use Atree;
with Debug;    use Debug;
with Debug_A;  use Debug_A;
with Einfo;    use Einfo;
with Errout;   use Errout;
with JVM;      use JVM;
with JVM.API;  use JVM.API;
with JVM.Dbg;  use JVM.Dbg;
with JVM.Map;  use JVM.Map;
with J_String; use J_String;
with Jx_Ch3;   use Jx_Ch3;
with Jx_Ch5;   use Jx_Ch5;
with Jx_Ch6;   use Jx_Ch6;
with Jx_Ch7;   use Jx_Ch7;
with Jx_Ch8;   use Jx_Ch8;
with Jx_Ch11;  use Jx_Ch11;
with Jx_Ch12;  use Jx_Ch12;
with Jx_Decl;  use Jx_Decl;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Osint;    use Osint;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Switch;   use Switch;
with Targparm; use Targparm;

package body Jx_Drive is

   --   This is the main driver for the Ada-to-JVM_Back_End
   --   (aka "the Juggler" ;-). Jx_Drive is currently invoked
   --   directly by a hacked version of gnat1drv.adb.
   --
   --   A call to Jx_Drive.GNAT_To_JVM operates as follows:
   --
   --     Step 1: Process all predefined types (i.e., types in standard)
   --
   --       Associate appropriate JVM type entities with each predefined type
   --       (e.g., Standard.Integer => Int_Type, Standard.Boolean => Int_Type,
   --       etc.); also, create classes, methods, etc. for various of the
   --       predefined Java API classes (e.g., java.lang.String).
   --
   --     Step 2: Process the compilation unit designated by GNAT_Root
   --
   --       GNAT_To_JVM invokes the main Translate procedure within this
   --       package to recursively traverse the entire tree of the unit,
   --       creating and associating JVM entities (classes, fields, methods,
   --       types) with Ada entities and generating all of the class files
   --       for the unit (calling various Jx_Drive support packages which in
   --       turn make calls to the JVM interface to create JVM entities and
   --       to generate Java byte code). Translate is applied to the various
   --       forms of Ada declarations, and simply consists of a case statement
   --       that invokes appropriate specialized translation operations in
   --       other support packages.
   --
   --   The mapping between Ada entities (Entity_Ids) and JVM entities
   --   (Class_Ids, Field_Ids, Method_Ids, Local_Var_Ids, Type_Ids) is
   --   supported by the package JVM.Map.
   --
   --   Instead of processing all Ada entities in units depended on by
   --   the unit being compiled, we create JVM entities on demand for
   --   imported entities as they are referenced. This is more efficient
   --   in general and in fact simplifies the implementation. JVM entities
   --   are created for referenced Ada entities as a result of calls to
   --   the functions JVM_Class, JVM_Field, JVM_Method, and JVM_Type
   --   (see package Jx_Decl).

   procedure Initialize_Type_Map;
   --  Creates associations from the predefined Ada types in Standard to their
   --  corresponding JVM entities.

   procedure Initialize_Exception_Map;
   --  Creates associations from the predefined Ada exceptions in Standard to
   --  their corresponding JVM entities.

   ------------------------
   -- Is_Back_End_Switch --
   ------------------------

   function Is_Back_End_Switch (Switch : String) return Boolean is
      First  : constant Positive := Switch'First + 1;
      Last   : Natural           := Switch'Last;

   begin
      if Last >= First
         and then Switch (Last) = ASCII.NUL
      then
         Last := Last - 1;
      end if;

      if not Is_Switch (Switch) then
         return False;
      end if;

      --  Allow the -g/-O/-f/-W/-w switches (and basically ignore
      --  them, except for -g/-g0).

      case Switch (First) is
         when 'g' =>
            if Switch (First .. Last) = "g0" then
               --  Disable debugging info
               Opt.Debugger_Level := 0;
            else
               Opt.Debugger_Level := 1;
            end if;

            return True;

         when 'O' | 'f' | 'W' | 'w' =>
            --  Ignored
            return True;

         when others =>
            return False;
      end case;
   end Is_Back_End_Switch;

   -------------------------
   -- Initialize_Type_Map --
   -------------------------

   procedure Initialize_Type_Map is

      procedure Set_DT (Typ : Type_Id; Value : Type_Id);
      --  Register the descriptor type of Typ

      procedure Set_Type_Map (A_Type : Entity_Id; J_Type : Type_Id);
      --  Associates the base type of A_Type with the JVM type entity J_Type.

      ------------
      -- Set_DT --
      ------------

      procedure Set_DT (Typ : Type_Id; Value : Type_Id) is
      begin
         case VM_Target is
            when CLI_Target =>
               Set_Descriptor_Type (Typ, Value);

            when JVM_Target =>
               --  In the JVM backend some predefined types are not available
               --  (for example, unsigned types ---see jvm/jvm.ads for details)

               if Typ /= Null_Type then

                  --  In the JVM backend some predefined types are mapped to
                  --  the same JVM type (for example, Sbyte_Type).

                  if Descriptor_Type (Typ) = Null_Type then
                     Set_Descriptor_Type (Typ, Value);
                  else
                     pragma Assert (Descriptor_Type (Typ) = Value);
                     null;
                  end if;
               end if;

            when No_VM =>
               pragma Assert (False);
               raise Program_Error;
         end case;
      end Set_DT;

      ------------------
      -- Set_Type_Map --
      ------------------

      procedure Set_Type_Map (A_Type : Entity_Id; J_Type : Type_Id) is
      begin
         Set_Map (Base_Type (A_Type), J_Type);
      end Set_Type_Map;

      --  Local variables

      Str_Typ           : Type_Id;
      Str_Wide_Typ      : Type_Id;
      Str_Wide_Wide_Typ : Type_Id;

   --  Start of processing for Initialize_Type_Map

   begin
      Str_Typ :=
        New_Array_Type (JVM.Byte_Type, 1, Name ("Standard.String"));
      Set_Type_Map (Standard_String, Str_Typ);

      Str_Wide_Typ :=
        New_Array_Type (JVM.Char_Type, 1, Name ("Standard.Wide_String"));
      Set_Type_Map (Standard_Wide_String, Str_Wide_Typ);

      Str_Wide_Wide_Typ :=
        New_Array_Type (JVM.Int_Type, 1, Name ("Standard.Wide_Wide_String"));
      Set_Type_Map (Standard_Wide_Wide_String, Str_Wide_Wide_Typ);

      Set_Type_Map (Universal_Integer,            JVM.Long_Type);
      Set_Type_Map (Standard_Character,           JVM.Byte_Type);
      Set_Type_Map (Standard_Wide_Character,      JVM.Char_Type);
      Set_Type_Map (Standard_Wide_Wide_Character, JVM.Int_Type);
      Set_Type_Map (Standard_Boolean,             JVM.Boolean_Type);
      Set_Type_Map (Standard_Duration,            JVM.Long_Type);
      Set_Type_Map (Standard_Short_Float,         JVM.Float_Type);
      Set_Type_Map (Standard_Float,               JVM.Float_Type);
      Set_Type_Map (Standard_Long_Float,          JVM.Double_Type);
      Set_Type_Map (Standard_Long_Long_Float,     JVM.Double_Type);
      Set_Type_Map (Standard_Short_Short_Integer, JVM.SByte_Type);
      Set_Type_Map (Standard_Short_Integer,       JVM.Short_Type);
      Set_Type_Map (Standard_Integer,             JVM.Int_Type);
      Set_Type_Map (Standard_Long_Integer,        JVM.Long_Type);
      Set_Type_Map (Standard_Long_Long_Integer,   JVM.Long_Type);
      Set_Type_Map (Standard_Void_Type,           JVM.Void_Type);

      --  Link types and their descriptors

      Set_DT (Str_Typ,             Type_Of (API_Class (Ada_String)));
      Set_DT (Str_Wide_Typ,        Type_Of (API_Class (Ada_Wide_String)));
      Set_DT (Str_Wide_Wide_Typ,   Type_Of (API_Class (Ada_Wide_Wide_String)));

      Set_DT (UInt_Type,           Type_Of (API_Class (Ada_UInt)));

      Set_DT (Boolean_Type,        Type_Of (API_Class (Ada_Int)));
      Set_DT (Byte_Type,           Type_Of (API_Class (Ada_Int)));
      Set_DT (SByte_Type,          Type_Of (API_Class (Ada_Int)));
      Set_DT (Char_Type,           Type_Of (API_Class (Ada_Int)));
      Set_DT (Short_Type,          Type_Of (API_Class (Ada_Int)));
      Set_DT (Int_Type,            Type_Of (API_Class (Ada_Int)));
      Set_DT (Native_Int_Type,     Type_Of (API_Class (Ada_Int)));

      Set_DT (Uint8_Addrof_Type,   Type_Of (API_Class (Ada_Int)));
      Set_DT (Uint16_Addrof_Type,  Type_Of (API_Class (Ada_Int)));
      Set_DT (Uint32_Addrof_Type,  Type_Of (API_Class (Ada_Int)));
      Set_DT (Uint64_Addrof_Type,  Type_Of (API_Class (Ada_Int)));
      Set_DT (Int8_Addrof_Type,    Type_Of (API_Class (Ada_Int)));
      Set_DT (Int16_Addrof_Type,   Type_Of (API_Class (Ada_Int)));
      Set_DT (Int32_Addrof_Type,   Type_Of (API_Class (Ada_Int)));
      Set_DT (Int64_Addrof_Type,   Type_Of (API_Class (Ada_Int)));
      Set_DT (Float32_Addrof_Type, Type_Of (API_Class (Ada_Int)));
      Set_DT (Float64_Addrof_Type, Type_Of (API_Class (Ada_Int)));
      Set_DT (Bool_Addrof_Type,    Type_Of (API_Class (Ada_Int)));
      Set_DT (Char_Addrof_Type,    Type_Of (API_Class (Ada_Int)));

      Set_DT (ULong_Type,          Type_Of (API_Class (Ada_ULng)));
      Set_DT (Long_Type,           Type_Of (API_Class (Ada_Lng)));
      Set_DT (Float_Type,          Type_Of (API_Class (Ada_Flt)));
      Set_DT (Double_Type,         Type_Of (API_Class (Ada_Dbl)));
   end Initialize_Type_Map;

   ------------------------------
   -- Initialize_Exception_Map --
   ------------------------------

   procedure Initialize_Exception_Map is
   begin
      Set_Map (Standard_Entity (S_Constraint_Error),
               Type_Of (API_Class (Ada_Constraint_Error)));
      Set_Map (Standard_Entity (S_Program_Error),
               Type_Of (API_Class (Ada_Program_Error)));
      Set_Map (Standard_Entity (S_Storage_Error),
               Type_Of (API_Class (Ada_Storage_Error)));
      Set_Map (Standard_Entity (S_Tasking_Error),
               Type_Of (API_Class (Ada_Tasking_Error)));
   end Initialize_Exception_Map;

   -----------------
   -- GNAT_To_JVM --
   -----------------

   procedure GNAT_To_JVM (GNAT_Root : Node_Id) is
   begin
      --  Establish the JVM entities for the predefined classes, methods,
      --  types, etc.

      JVM.Initialize;
      JVM.API.Initialize;

      JVM.Set_Trace (Debug_Flag_JJ);

      --  Allocate a map big enough for all nodes (used for Entity_Id mapping)

      JVM.Map.Initialize_Map (Last_Node_Id);

      --  Establish mappings for the predefined Ada types

      Initialize_Type_Map;

      --  Establish mappings for the predefined Ada exceptions

      Initialize_Exception_Map;

      --  Set up for output of source line info when debugging

      Init_Source_Line_Output (GNAT_Root);

      --  Traverse the GNAT tree for the main compilation unit and
      --  generate all of its JVM classes.

      Translate (GNAT_Root);
   end GNAT_To_JVM;

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Node : Node_Id) is
   begin
      Debug_A_Entry ("(Ada-to-JVM) ", Node);

      case Nkind (Node) is
         when N_Statement_Other_Than_Procedure_Call |
              N_Procedure_Call_Statement =>

            Translate_Statement (Node);

         --  For now we ignore all pragmas ???

         when N_Pragma =>

            case Get_Pragma_Id (Pragma_Name (Node)) is
               when others =>
                  null;
            end case;

         --  For now we ignore representation clauses ???

         when N_At_Clause |
              N_Component_Clause |
              N_Enumeration_Representation_Clause |
              N_Mod_Clause |
              N_Record_Representation_Clause |
              N_Attribute_Definition_Clause =>

            null;

         when N_Raise_xxx_Error =>

            Translate_Statement (Node);

         when N_Freeze_Entity =>

            --  For tagged types that implement interfaces generate required
            --  wrappers of interface primitives. We use Underlying_Type to
            --  handle private types.

            declare
               E : constant Entity_Id := Underlying_Type (Entity (Node));

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

            Translate_Declarations (Actions (Node));

         when N_Full_Type_Declaration | N_Private_Type_Declaration
            | N_Private_Extension_Declaration | N_Task_Type_Declaration =>

            Translate_Type (Defining_Entity (Node));

         when N_Object_Declaration =>

            Translate_Object_Declaration (Node);

         when N_Object_Renaming_Declaration =>

            Translate_Object_Renaming (Node);

         when N_Number_Declaration =>

            null;

         when N_Case_Expression | N_Case_Expression_Alternative =>
            --  Expanded by the front-end into an N_Expression_With_Actions
            null;

         when N_Subtype_Declaration =>

            Translate_Subtype (Defining_Entity (Node));

         when N_Function_Specification | N_Procedure_Specification =>

            declare
               Subp_Entity : constant Entity_Id := Defining_Entity (Node);

            begin
               --  We apply special treatment for a subprogram that is
               --  a generic library instance. Associated_Class can't
               --  tell whether such instances should get a new class
               --  or be associated with the class of the containing
               --  (expander-generated) package because the instance
               --  looks like its parent scope is Standard rather than
               --  the enclosing package. Rather than add a specialized
               --  parameter to Associated_Class to indicate whether
               --  this is a declaration for a method being generated
               --  or simply referenced (in the former case we want
               --  to use the currently open class), we explicitly
               --  pass Current_Compilation_Class to Declare_Method.
               --  Perhaps there's a more elegant approach. ???

               if Is_Generic_Instance (Subp_Entity)
                 and then Is_Library_Level_Entity (Subp_Entity)
               then
                  Declare_Method (Current_Compilation_Class, Subp_Entity);
               else
                  Declare_Method (Associated_Class (Subp_Entity), Subp_Entity);
               end if;
            end;

         when N_Package_Body =>
            declare
               Def_Id : constant Entity_Id := Unique_Defining_Entity (Node);

            begin
               JVM_Scope_Stack.Push (Def_Id);

               if Ekind (Def_Id) not in Generic_Unit_Kind then
                  Translate_Declarations (Declarations (Node));
                  Translate_Handled_Statements
                    (Handled_Statement_Sequence (Node));
               end if;

               JVM_Scope_Stack.Pop;
            end;

         when N_Subprogram_Body =>
            declare
               Def_Id : constant Entity_Id := Unique_Defining_Entity (Node);

            begin
               JVM_Scope_Stack.Push (Def_Id);

               if Acts_As_Spec (Node) then
                  --  See comment above on N_Procedure_Specification
                  --  for an explanation of this special treatment
                  --  of generic library instances.

                  if Is_Generic_Instance (Def_Id)
                    and then Is_Library_Level_Entity (Def_Id)
                  then
                     Declare_Method (Current_Compilation_Class, Def_Id);
                  else
                     Declare_Method (Associated_Class (Def_Id), Def_Id);
                  end if;

                  Generate_Method (Node);

               elsif
                 Ekind (Corresponding_Spec (Node)) not in Generic_Unit_Kind
               then
                  Generate_Method (Node);
               end if;

               JVM_Scope_Stack.Pop;
            end;

         when N_Subprogram_Renaming_Declaration =>
            --  ??? Nothing for now, but eventually may have to evaluate
            --  the name in certain cases.

            null;

         when N_Procedure_Instantiation | N_Function_Instantiation =>
            --  ??? Nothing for now.

            null;

         when N_Package_Instantiation =>
            --  ??? Nothing for now.

            null;

         when N_Implicit_Label_Declaration =>
            Declare_Label (Defining_Entity (Node));

         --  ??? Actually some actions will be needed to handle incomplete
         --  types whose full type is deferred to a separate body. ???

         when N_Incomplete_Type_Declaration =>
            null;

         when N_Itype_Reference =>

            --  In some cases the referenced Itype has already been
            --  processed and has an associated JVM entity, so we skip
            --  the translation in that case (it's not clear in exactly
            --  what situations that occurs, but this showed up in bug
            --  7429-001).

            if JVM_Entity (Base_Type (Itype (Node))) = Null_Type then
               if Is_Itype (Base_Type (Itype (Node))) then
                  Translate_Type (Itype (Node));
               end if;
            end if;

         when N_Package_Declaration =>
            JVM_Scope_Stack.Push (Unique_Defining_Entity (Node));

            if Present (Generic_Parent (Specification (Node)))
              and then Convention (Generic_Parent (Specification (Node)))
                = Convention_VM
            then
               null;
            else
               Translate (Specification (Node));
            end if;

            JVM_Scope_Stack.Pop;

         when N_Subprogram_Declaration | N_Abstract_Subprogram_Declaration =>
            JVM_Scope_Stack.Push (Defining_Entity (Specification (Node)));
            Translate (Specification (Node));
            JVM_Scope_Stack.Pop;

         when N_Compilation_Unit =>

            case Nkind (Unit (Node)) is

               when N_Package_Declaration =>
                  JVM_Scope_Stack.Push (Defining_Entity (Unit (Node)));

                  if not Generate_Package_Class
                    (Specification (Unit (Node)))
                  then
                     Set_Has_No_Elaboration_Code (Node);
                  end if;

                  JVM_Scope_Stack.Pop;

               when N_Package_Body =>
                  declare
                     Def_Id : constant Entity_Id :=
                                Unique_Defining_Entity (Unit (Node));

                  begin
                     JVM_Scope_Stack.Push (Def_Id);

                     if Ekind (Def_Id) in Generic_Unit_Kind then
                        Generate_Generic_Unit_Class (Node);

                     elsif not Generate_Package_Class (Unit (Node)) then
                        Set_Has_No_Elaboration_Code (Node);
                     end if;

                     JVM_Scope_Stack.Pop;
                  end;

               when N_Subprogram_Body =>
                  declare
                     Id : constant Entity_Id :=
                            Unique_Defining_Entity (Unit (Node));
                  begin
                     JVM_Scope_Stack.Push (Id);

                     Set_Has_No_Elaboration_Code (Node);

                     if Acts_As_Spec (Unit (Node))
                       or else Ekind (Corresponding_Spec (Unit (Node)))
                                 not in Generic_Unit_Kind
                     then
                        Generate_Subprogram_Class (Node);
                     else
                        Generate_Generic_Unit_Class (Node);
                     end if;

                     JVM_Scope_Stack.Pop;
                  end;

               when N_Generic_Package_Declaration
                  | N_Generic_Subprogram_Declaration =>
                  Generate_Generic_Unit_Class (Node);

               when N_Package_Renaming_Declaration
                  | N_Subprogram_Renaming_Declaration
                  | N_Generic_Function_Renaming_Declaration
                  | N_Generic_Package_Renaming_Declaration
                  | N_Generic_Procedure_Renaming_Declaration
                  | N_Subprogram_Declaration =>
                  Set_Has_No_Elaboration_Code (Node);
                  Generate_Empty_Class;

               when others =>
                  pragma Assert (False);
                  raise Program_Error;
            end case;

         when N_Package_Specification =>

            Translate_Declarations (Visible_Declarations (Node));
            Translate_Declarations (Private_Declarations (Node));

         when N_Exception_Declaration =>

            Translate_Exception_Declaration (Node);

         when N_Exception_Renaming_Declaration =>

            null;

         when N_Package_Renaming_Declaration
            | N_Generic_Function_Renaming_Declaration
            | N_Generic_Package_Renaming_Declaration
            | N_Generic_Procedure_Renaming_Declaration =>

            null;

         when N_Generic_Package_Declaration
            | N_Generic_Subprogram_Declaration =>

            null;

         when N_Use_Package_Clause | N_Use_Type_Clause =>

            null;

         when N_Body_Stub =>

            if Nkind (Node) = N_Subprogram_Body_Stub
              and then Ekind (Defining_Unit_Name (Specification (Node)))
                not in Generic_Unit_Kind
            then
               --  Force the declaration of the subprogram by calling
               --  JVM_Method. It's not clear how to easily tell whether
               --  the subprogram stub has a preceding spec or acts as
               --  its own spec (Acts_As_Spec won't work here). ???

               if JVM_Method (Corresponding_Spec
                               (Proper_Body (Unit (Library_Unit (Node)))))
                 = Null_Method
               then
                  --  JVM_Method should never return Null_Method
                  pragma Assert (False);
                  raise Program_Error;
               end if;
            end if;

            Translate (Proper_Body (Unit (Library_Unit (Node))));

         when N_Task_Body | N_Task_Definition =>

            null;

         when N_Protected_Type_Declaration =>

            Translate (Protected_Definition (Node));

         when N_Protected_Definition =>

            --  Perform a specialized traversal, looking for and
            --  translating any type declarations contained within
            --  a protected definition. The front end sometimes
            --  expands nested type declarations in this context,
            --  but we don't want to do a normal traversal and
            --  translation of all nested declarations because
            --  this leads to complications of unwanted translations
            --  of certain declarations that are already translated
            --  as part of other expansions (e.g., protected components).
            --  Is there a cleaner way of handling this ???

            declare
               Decl : Node_Id;

            begin
               Decl := First (Visible_Declarations (Node));
               while Present (Decl) loop
                  if Nkind (Decl) = N_Full_Type_Declaration then
                     Translate_Type (Defining_Entity (Decl));
                  end if;
                  Decl := Next (Decl);
               end loop;

               Decl := First (Private_Declarations (Node));
               while Present (Decl) loop
                  if Nkind (Decl) = N_Full_Type_Declaration then
                     Translate_Type (Defining_Entity (Decl));
                  end if;
                  Decl := Next (Decl);
               end loop;
            end;

         when N_Protected_Body =>

            null;

         when N_Entry_Declaration =>

            null;

         when N_Validate_Unchecked_Conversion =>

            declare
               J_Source : constant Type_Id := JVM_Type (Source_Type (Node));
               J_Target : constant Type_Id := JVM_Type (Target_Type (Node));

            begin
               if J_Source = J_Target then
                  null;

               --  Always allow conversions when either the source or the
               --  target is java.lang.Object. (Not clear that it makes sense
               --  to always allow this since it normally only makes sense
               --  when Java classes are involved, in which case a normal
               --  tagged conversion should be possible. But another case
               --  is conversions generated by the front end involving
               --  System.Address, which is mapped to java.lang.Object,
               --  such as for subprogram'Address. ???)

               elsif J_Source = Type_Of (Java_Lang_Object)
                 or else J_Target = Type_Of (Java_Lang_Object)
               then
                  null;

               elsif JVM.Type_Kind (J_Target) in Byte_Kind .. Double_Kind then

                  --  ??? Consider using ldind instructions

                  null;

               --  We allow explicitely unchecked conversion from one delegate
               --  to another. This allows in particular to explicitely convert
               --  from ada access-to-subprogram types to MSSyst.Delegate.Ref
               --  type.

               elsif Is_Delegate (Source_Type (Node))
                 and then Is_Delegate (Target_Type (Node))
               then
                  null;

               --  We presently condition the check for invalid conversions
               --  on whether GNAT_Mode is set because there are still some
               --  cases of unsupportable unchecked conversions occurring
               --  in the GNAT run-time library (e.g., s-fatgen.Valid).
               --  We should probably check whether we're actually in the
               --  run-time rather than checking whether GNAT_Mode is set. ???

               elsif GNAT_Mode then
                  null;

               --  We disallow unchecked conversions for all composite
               --  types. This may be overly restrictive, but it's not
               --  clear which cases to allow and it seems that any cases
               --  that could reasonably be supported can be done using
               --  a normal type conversion. ???

               elsif JVM.Type_Kind (J_Source) /= JVM.Type_Kind (J_Target)
                 or else JVM.Type_Kind (J_Source) = JVM.Class_Kind
                 or else JVM.Type_Kind (J_Source) = JVM.Array_Kind
               then
                  Error_Msg_N
                    ("incompatible JVM types for unchecked conversion", Node);

                  --  Should we allow unchecked array conversions if the
                  --  dimensionality and element types are compatible. ???
                  --    Dimensionality (J_Source) = Dimensionality (J_Target)
                  --      and then Legal_Unchk_Conversion
                  --                 (Component_Type (Source_Type),
                  --                  Component_Type (Target_Type))
               end if;
            end;

         when N_Push_xxx_Label =>
            Push_Exception_Label_Stack (Node);

         when N_Pop_xxx_Label =>
            Pop_Exception_Label_Stack (Node);

         --  SCIL nodes are ony processed in the SCIL backend

         when N_SCIL_Dispatch_Table_Tag_Init
            | N_SCIL_Dispatching_Call
            | N_SCIL_Membership_Test =>
            null;

         when N_Unused_At_Start
            | N_Empty
            | N_Error
            | N_Pragma_Argument_Association
            | N_Defining_Character_Literal
            | N_Defining_Identifier
            | N_Defining_Operator_Symbol
            | N_Expanded_Name
            | N_Expression_With_Actions
            | N_Identifier
            | N_Iterator_Specification
            | N_Character_Literal
            | N_Operator_Symbol
            | N_Op_Add
            | N_Op_And
            | N_Op_Concat
            | N_Op_Divide
            | N_Op_Eq
            | N_Op_Expon
            | N_Op_Ge
            | N_Op_Gt
            | N_Op_Le
            | N_Op_Lt
            | N_Op_Mod
            | N_Op_Multiply
            | N_Op_Ne
            | N_Op_Or
            | N_Op_Rem
            | N_Op_Subtract
            | N_Op_Xor
            | N_Op_Rotate_Left
            | N_Op_Rotate_Right
            | N_Op_Shift_Left
            | N_Op_Shift_Right
            | N_Op_Shift_Right_Arithmetic
            | N_Op_Abs
            | N_Op_Minus
            | N_Op_Not
            | N_Op_Plus
            | N_Attribute_Reference
            | N_And_Then
            | N_If_Expression
            | N_Explicit_Dereference
            | N_Function_Call
            | N_In
            | N_Indexed_Component
            | N_Integer_Literal
            | N_Not_In
            | N_Null
            | N_Or_Else
            | N_Qualified_Expression
            | N_Quantified_Expression
            | N_Aggregate
            | N_Allocator
            | N_Extension_Aggregate
            | N_Range
            | N_Real_Literal
            | N_Reference
            | N_Selected_Component
            | N_Slice
            | N_String_Literal
            | N_Subprogram_Info
            | N_Type_Conversion
            | N_Unchecked_Expression
            | N_Unchecked_Type_Conversion
            | N_Subtype_Indication
            | N_Component_Declaration
            | N_Entry_Index_Specification
            | N_Formal_Object_Declaration
            | N_Formal_Type_Declaration
            | N_Loop_Parameter_Specification
            | N_Access_Function_Definition
            | N_Access_Procedure_Definition
            | N_Single_Task_Declaration
            | N_Constrained_Array_Definition
            | N_Unconstrained_Array_Definition
            | N_Abortable_Part
            | N_Accept_Alternative
            | N_Access_Definition
            | N_Aspect_Specification
            | N_Access_To_Object_Definition
            | N_Case_Statement_Alternative
            | N_Compilation_Unit_Aux
            | N_Component_Association
            | N_Component_Definition
            | N_Component_List
            | N_Contract
            | N_Derived_Type_Definition
            | N_Decimal_Fixed_Point_Definition
            | N_Defining_Program_Unit_Name
            | N_Delay_Alternative
            | N_Delta_Constraint
            | N_Designator
            | N_Digits_Constraint
            | N_Discriminant_Association
            | N_Discriminant_Specification
            | N_Elsif_Part
            | N_Enumeration_Type_Definition
            | N_Entry_Body
            | N_Entry_Body_Formal_Part
            | N_Entry_Call_Alternative
            | N_Exception_Handler
            | N_Floating_Point_Definition
            | N_Formal_Decimal_Fixed_Point_Definition
            | N_Formal_Derived_Type_Definition
            | N_Formal_Discrete_Type_Definition
            | N_Formal_Floating_Point_Definition
            | N_Formal_Modular_Type_Definition
            | N_Formal_Ordinary_Fixed_Point_Definition
            | N_Formal_Package_Declaration
            | N_Formal_Private_Type_Definition
            | N_Formal_Incomplete_Type_Definition
            | N_Formal_Signed_Integer_Type_Definition
            | N_Formal_Subprogram_Declaration
            | N_Generic_Association
            | N_Handled_Sequence_Of_Statements
            | N_Index_Or_Discriminant_Constraint
            | N_Iteration_Scheme
            | N_Label
            | N_Modular_Type_Definition
            | N_Ordinary_Fixed_Point_Definition
            | N_Others_Choice
            | N_Parameter_Association
            | N_Parameter_Specification
            | N_Expression_Function
            | N_Raise_Expression
            | N_Range_Constraint
            | N_Real_Range_Specification
            | N_Record_Definition
            | N_Signed_Integer_Type_Definition
            | N_Single_Protected_Declaration
            | N_Subunit
            | N_Terminate_Alternative
            | N_Triggering_Alternative
            | N_Variant
            | N_Variant_Part
            | N_With_Clause
            | N_Unused_At_End =>

            if Debug_Flag_JJ then
               Osint.Fail
                 ("*** Unsupported node: " & Node_Kind'Image (Nkind (Node)));
            else
               Error_Msg_N ("unsupported construct in this context", Node);
            end if;
      end case;

      Debug_A_Exit ("(Ada-to-JVM) ", Node, " (done)");
   end Translate;

begin

   --  Enable debugger output by default with the .NET compiler
   Opt.Debugger_Level := 1;

end Jx_Drive;
