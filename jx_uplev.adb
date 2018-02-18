------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J X _ U P L E V                              --
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
with JVM.API;       use JVM.API;
with JVM.Map;       use JVM.Map;
with J_Basics;      use J_Basics;
with J_Descriptors; use J_Descriptors;
with J_String;      use J_String;
with Jx_Ch7;        use Jx_Ch7;
with Jx_Decl;       use Jx_Decl;
with Sem_Util;      use Sem_Util;
with Sinfo;         use Sinfo;
with Targparm;      use Targparm;

package body Jx_Uplev is

   procedure Add_AR_Field (AR : AR_Access; Field : Field_Id);
   --  Adds a field to an AR class that represents the location of a variable
   --  addressed up-level by a nested subprogram.

   function AR_Field (AR : AR_Access; Name : Name_Id) return Field_Id;
   --  Returns the Field_Id associated with an up-level AR field with the given
   --  name.

   procedure Gen_Load_Current_Static_Link;
   --  Generate a load of the static link of the current method. Subsidiary of
   --  Load_Up_Level_Link and Make_Activation_Record.

   function Load_Up_Level_Link (Parent_Method : Method_Id) return AR_Access;
   --  Generate a load of the static link needed for accessing up-level
   --  variables within Parent_Method. This involves chaining up multiple
   --  activation records when the current method is nested multiple levels
   --  down from Parent_Method.

   function Undot (Name : String) return String;
   --  Replace '.' with '_' in a name for CIL
   --  Noop for JVM

   ---------------------
   -- Access_AR_Field --
   ---------------------

   function Access_AR_Field (Ada_Obj : Entity_Id) return Field_Id is
      AR_Entry : AR_Access;

   begin
      Register_Up_Level_Reference (Ada_Obj);
      AR_Entry := Load_Up_Level_Link (Enclosing_Method (Ada_Obj));

      return AR_Field (AR_Entry, Up_Level_Field_Name (Ada_Obj));
   end Access_AR_Field;

   ----------------------------
   -- Access_From_Current_AR --
   ----------------------------

   function Access_From_Current_AR (Ada_Obj : Entity_Id) return Boolean is
   begin
      return not AR_Stack.Empty
        and then AR_Stack.Top.Method = Current_Method
        and then Ekind (Full_Type (Ada_Obj)) in Wrappable_Kind
        and then not Needs_Access_Descriptor (Ada_Obj)
        and then
          AR_Field (AR_Stack.Top, Up_Level_Field_Name (Ada_Obj)) /= Null_Field;
   end Access_From_Current_AR;

   ------------------
   -- Add_AR_Field --
   ------------------

   procedure Add_AR_Field (AR : AR_Access; Field : Field_Id) is
   begin
      AR.Fields := new AR_Field_Rec'(Field, AR.Fields);
   end Add_AR_Field;

   --------------
   -- AR_Field --
   --------------

   function AR_Field (AR : AR_Access; Ada_Obj : Entity_Id) return Field_Id is
   begin
      return Field (AR.AR_Class, Up_Level_Field_Name (Ada_Obj));
   end AR_Field;

   --------------
   -- AR_Field --
   --------------

   function AR_Field (AR : AR_Access; Name : Name_Id) return Field_Id is
   begin
      return Field (AR.AR_Class, Name);
   end AR_Field;

   ----------------------
   -- Enclosing_Method --
   ----------------------

   function Enclosing_Method (E : Entity_Id) return Method_Id is
      Subp  : constant Entity_Id := Enclosing_Subprogram (E);

   begin
      --  Local variables are handled by directly retrieving their method,
      --  rather than applying JVM_Method to the result of
      --  Enclosing_Subprogram. This is because the enclosing subprogram is not
      --  always valid, e.g., in the case of variables associated with task
      --  types (the result of calling Enclosing_Subprogram may be a further
      --  outer subprogram enclosing the task). Similar difficulties could
      --  possibly occur for other entities such as subprograms, but it's not
      --  clear how to address those cases other than by using the enclosing
      --  subprogram. ???

      if Ekind (E) = E_Variable
        or else Ekind (E) = E_Constant
        or else Ekind (E) = E_Loop_Parameter
      then
         if Is_Global_Entity (E) then
            return Null_Method;
         else
            return Method_Of (JVM_Local_Var (E));
         end if;

      --  There are cases where Enclosing_Subprogram returns
      --  nonsubprogram entities for subprograms, such as E_Entry
      --  nodes, so we need to get the parent method from the
      --  subprogram's associated method. (This occurs in certain
      --  cases such as when a subprogram is created within a
      --  block statement nested in a subprogram expanded from an
      --  accept statement, because the E_Block's scope attribute
      --  is set to the entry of the accept statement rather than
      --  to the enclosing expander-generated subprogram. This
      --  seems like a bug, but we work around it for now by
      --  means of the JVM.Parent_Method function because there
      --  may be other cases like this where scopes are not set
      --  properly.) ???

      elsif Present (Subp) and then Ekind (Subp) in Subprogram_Kind then
         return JVM_Method (Subp);

      elsif Ekind (E) in Subprogram_Kind then
         return Parent_Method (JVM_Method (E));

      else
         return Null_Method;
      end if;
   end Enclosing_Method;

   ---------------------------
   -- End_Activation_Record --
   ---------------------------

   procedure End_Activation_Record (Method : Method_Id) is
      AR : constant AR_Access := AR_Stack.Pop;

   begin
      pragma Assert (AR.Method = Method);

      Generate_Null_Methods (AR.AR_Class);
      End_Class_File (AR.AR_Class);
   end End_Activation_Record;

   ----------------------------------
   -- Gen_Load_Current_Static_Link --
   ----------------------------------

   procedure Gen_Load_Current_Static_Link is
   begin
      --  In some platforms we generate nested subprograms as virtual methods
      --  of the AR class associated with their enclosing scope, and hence the
      --  SL is the dispatching object.

      if Is_AR_Method (Current_Method) then

         --  Currently this feature is currently only supported in the CIL
         --  backend

         pragma Assert (VM_Target = CLI_Target);
         Gen_Load_Local (Local_Var (Current_Method, "$this"));

      elsif Has_AR_SL_Formal (Current_Method) then
         Gen_Load_Local (Local_Var (Current_Method, "__AR_SL"));

      --  For dispatching subprograms the SL is stored in an extra field
      --  inside the dispatching object.

      else pragma Assert (not Is_Interface (Class_Of (Current_Method)));
         pragma Assert (Present (Ada_Entity (Current_Method))
           and then Is_Dispatching_Operation (Ada_Entity (Current_Method)));

         --  Constructors receive the SL in an extra argument and must not
         --  be handed here.

         pragma Assert
           ((VM_Target = CLI_Target
               and then Name (Current_Method) /= Name (".ctor"))
            or else
            (VM_Target = JVM_Target
               and then Name (Current_Method) /= Name ("<init>")));

         Gen_Load_Local (Local_Var (Current_Method, "$this"));
         Gen_Get_Object_Field
           (Field (Class_Of (Current_Method), Name ("__AR_SL")));
      end if;
   end Gen_Load_Current_Static_Link;

   ----------------------
   -- Is_Global_Entity --
   ----------------------

   function Is_Global_Entity (E : Entity_Id) return Boolean is
   begin
      return not Present (Enclosing_Subprogram (E));
   end Is_Global_Entity;

   ----------------------
   -- Load_Static_Link --
   ----------------------

   procedure Load_Static_Link (Parent_Method : Method_Id) is
      AR_Entry : AR_Access;
      pragma Unreferenced (AR_Entry);

   begin
      AR_Entry := Load_Up_Level_Link (Parent_Method);
   end Load_Static_Link;

   -------------------------
   -- Load_Up_Level_Field --
   -------------------------

   procedure Load_Up_Level_Field (Method : Method_Id; Name : Name_Id) is
      AR_Entry : constant AR_Access := Load_Up_Level_Link (Method);

   begin
      Gen_Get_Object_Field (AR_Field (AR_Entry, Name));
   end Load_Up_Level_Field;

   ------------------------
   -- Load_Up_Level_Link --
   ------------------------

   function Load_Up_Level_Link (Parent_Method : Method_Id) return AR_Access is
      AR_Entry : AR_Access := AR_Stack.Top;

   begin
      if Parent_Method = Current_Method then
         Gen_Load_Local (AR_Entry.AR_Obj);

      else
         --  Load the current method's static link parameter

         Gen_Load_Current_Static_Link;

         --  If the current method has an AR then we first step to
         --  the parent's AR.

         if AR_Entry.Method = Current_Method then
            AR_Entry := AR_Entry.Parent;
         end if;

         --  Now, if needed, follow the AR links until reaching the
         --  object's associated AR.

         while AR_Entry /= null
           and then AR_Entry.Method /= Parent_Method
         loop
            Gen_Get_Object_Field (API_Field (AR_Static_Link));
            AR_Entry := AR_Entry.Parent;
         end loop;

         pragma Assert (AR_Entry /= null);

         --  Cast the top-of-stack class reference (which denotes the top-level
         --  Ada activation record class) to the class of the located AR.

         Gen_Check_Cast (AR_Entry.AR_Class);
      end if;

      return AR_Entry;
   end Load_Up_Level_Link;

   ----------------------------
   -- Make_Activation_Record --
   ----------------------------

   Counter : Integer := 1;
   --  Counter used to generate unique names for activation record classes

   procedure Make_Activation_Record
     (Method : Method_Id;
      Outer  : Class_Id)
   is
      Save_Current_Method : constant Method_Id := Current_Method;
      New_AR              : constant AR_Access := new Active_Rec;

   begin
      pragma Assert (AR_Stack.Empty or else AR_Stack.Top.Method /= Method);

      if AR_Stack.Empty then
         New_AR.Parent := null;
      else
         New_AR.Parent := AR_Stack.Top;
      end if;

      AR_Stack.Push (New_AR);

      New_AR.Method := Method;

      --  Create a class that extends the predefined Ada activation record
      --  class. This class will have fields added to it for variables of
      --  the method that are referenced up-level from nested methods.

      New_AR.AR_Class :=
        New_Class
          (Ada_Ent     => Empty,
           Name        => J_String.Name
                            ("__AR_" &
                               Name_String (Name (Method)) &
                               Strip (Integer'Image (Counter))),
           Super       => API_Class (Ada_Activation_Rec),
           Outer_Class => Outer,
           Public      => True,
           Final       => True);

      Counter := Counter + 1;

      --  Open the class file and create its <clinit> and <init> methods.
      --  The class file will be closed upon reaching the end of the
      --  activation record's associated method.

      Begin_Class_File (New_AR.AR_Class);
      Generate_Class_Init_Method (New_AR.AR_Class);
      Generate_Default_Constructor (New_AR.AR_Class);

      --  Temporarily switch to the AR's method and generate code to allocate
      --  the method's AR object and save its reference in a new local variable
      --  of the method.

      Set_Current_Method (Method);

      New_AR.AR_Obj :=
        New_Local_Var
          (Method, J_String.Name ("__AR"), Type_Of (New_AR.AR_Class));

      --  Enter the code for creating the AR object as entry code for the
      --  method. This is necessary to ensure that the AR gets created outside
      --  of any conditional code sequences, since otherwise the AR reference
      --  could be uninitialized later in the method (this can occur when
      --  nested subprograms occur within blocks nested within conditional
      --  code). Note that a similar problem can happen with the fields within
      --  an AR, since they may be conditionally initialized and updated. So
      --  what we do is to set a flag Has_Up_Level_Access in the front-end, and
      --  use this flag to generate uplevel fields in advance see
      --  Jx_Ch3.Translate_Declarations and Translate_Object_Declaration.

      Start_Entry_Code_Sequence;

      Gen_Default_Object (New_AR.AR_Class);
      Gen_Store_Local (New_AR.AR_Obj);

      --  If the parent of the nested method is itself a nested method then
      --  load the parent method's static link parameter and save it as the
      --  static link in the AR.

      if New_AR.Parent /= null then
         Gen_Load_Local (New_AR.AR_Obj);
         Gen_Load_Current_Static_Link;
         Gen_Put_Object_Field (API_Field (AR_Static_Link));
      end if;

      End_Entry_Code_Sequence;

      --  Now switch back to the previous active method.

      Set_Current_Method (Save_Current_Method);
   end Make_Activation_Record;

   ---------------------------------
   -- Register_Up_Level_Reference --
   ---------------------------------

   procedure Register_Up_Level_Reference
     (Method : Method_Id;
      LV     : Local_Var_Id)
   is
      Local_Field : Field_Id;
      AR_Entry    : AR_Access;

   begin
      pragma Assert (not AR_Stack.Empty);

      --  Search for the AR of the enclosing method

      AR_Entry := AR_Stack.Top;
      while AR_Entry /= null and then AR_Entry.Method /= Method loop
         AR_Entry := AR_Entry.Parent;
      end loop;
      pragma Assert (AR_Entry /= null);

      --  If the object's local variable doesn't already exist in its
      --  parent's AR, then this is the first up-level reference to it,
      --  so we have to create a new field for it in the parent's AR
      --  class and copy its current value into the parent's AR object.

      if AR_Field (AR_Entry, Name (LV)) = Null_Field then
         Local_Field := New_Field (AR_Entry.AR_Class, Name (LV),
                                   Type_Of (LV), Static => False);

         Add_AR_Field (AR_Entry, Local_Field);

         --  Temporarily switch to the parent method and generate code
         --  to copy the up-level variable into its corresponding field
         --  in the parent's AR object.

         declare
            Save_Current_Method : constant Method_Id := Current_Method;

         begin
            Set_Current_Method (AR_Entry.Method);
            Gen_Load_Local (AR_Entry.AR_Obj);

            Gen_Load_Local (LV);
            Gen_Put_Field (AR_Field (AR_Entry, Name (LV)));

            Set_Current_Method (Save_Current_Method);
         end;
      end if;
   end Register_Up_Level_Reference;

   ---------------------------------
   -- Register_Up_Level_Reference --
   ---------------------------------

   procedure Register_Up_Level_Reference (Ada_Obj : Entity_Id) is
      Object_Method : constant Method_Id := Enclosing_Method (Ada_Obj);
      AR_Entry      : AR_Access;
      Local_Field   : Field_Id;

   begin
      pragma Assert (not AR_Stack.Empty);

      --  Search for the AR of the enclosing method

      AR_Entry := AR_Stack.Top;
      while AR_Entry /= null and then AR_Entry.Method /= Object_Method loop
         AR_Entry := AR_Entry.Parent;
      end loop;
      pragma Assert (AR_Entry /= null);

      --  If the object's local variable doesn't already exist in its parent's
      --  AR, then this is the first up-level reference to it, so we have to
      --  create a new field for it in the parent's AR class and copy its
      --  current value into the parent's AR object.

      if AR_Field (AR_Entry, Up_Level_Field_Name (Ada_Obj)) = Null_Field then
         Local_Field :=
           New_Field
             (AR_Entry.AR_Class,
              Up_Level_Field_Name (Ada_Obj),
              Type_Of (JVM_Local_Var (Ada_Obj)),
              Static => False);

         Add_AR_Field (AR_Entry, Local_Field);

         --  Temporarily switch to the parent method and generate code to copy
         --  the up-level variable into its corresponding field in the parent's
         --  AR object.

         declare
            Save_Current_Method : constant Method_Id := Current_Method;

         begin
            Set_Current_Method (AR_Entry.Method);
            Gen_Load_Local (AR_Entry.AR_Obj);

            Gen_Load_Local (JVM_Local_Var (Ada_Obj));
            Gen_Put_Field (Local_Field);

            Set_Current_Method (Save_Current_Method);
         end;
      end if;
   end Register_Up_Level_Reference;

   -----------
   -- Undot --
   -----------

   function Undot (Name : String) return String is
      Result : String := Name;
   begin
      case VM_Target is
         when CLI_Target =>
            for J in Result'Range loop
               if Result (J) = '.' then
                  Result (J) := '_';
               end if;
            end loop;

         when JVM_Target =>
            null;

         when No_VM =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      return Result;
   end Undot;

   -------------------------
   -- Up_Level_Field_Name --
   -------------------------

   function Up_Level_Field_Name (Local : Entity_Id) return Name_Id is
   begin
      if Ekind (Scope (Local)) in Subprogram_Kind then
         return Chars (Local);
      else
         return Name (Undot (JVM_Expanded_Name (Local)));
      end if;
   end Up_Level_Field_Name;

end Jx_Uplev;
