------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J X _ C H 1 2                               --
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
with JVM;      use JVM;
with J_String; use J_String;
with JVM.Map;  use JVM.Map;
with Jx_Ch3;   use Jx_Ch3;
with Jx_Ch5;   use Jx_Ch5;
with Jx_Ch7;   use Jx_Ch7;
with Jx_Decl;  use Jx_Decl;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;

package body Jx_Ch12 is

   procedure Generate_Generic_Unit_Class (Comp_Unit : Node_Id) is
      Gen_Node    : constant Node_Id := Unit (Comp_Unit);
      Gen_Spec    : Node_Id;
      Comp_Spec   : Node_Id;
      Comp_Body   : Node_Id := Empty;
      Gen_Entity  : Entity_Id;
      Gen_Class   : Class_Id;
      Elab_Method : Method_Id;

   begin
      if Nkind (Gen_Node) in N_Generic_Declaration then
         Gen_Spec  := Specification (Gen_Node);
         Comp_Spec := Comp_Unit;
      else
         Gen_Spec  := Parent (Corresponding_Spec (Gen_Node));
         Comp_Body := Comp_Unit;
      end if;

      --  Account for child unit

      if Nkind (Gen_Spec) = N_Defining_Program_Unit_Name then
         Gen_Spec := Parent (Gen_Spec);
      end if;

      --  Retrieve the compilation unit node

      Comp_Spec := Parent (Parent (Gen_Spec));

      Gen_Entity := Defining_Entity (Gen_Spec);

      Declare_Package_Class (Gen_Entity);
      Gen_Class := JVM_Class (Gen_Entity);

      Set_Map (Standard_Standard, Gen_Class);

      Class_Stack.Push (Gen_Class);
      Begin_Class_File (Gen_Class);
      Current_Compilation_Class := Current_Class;

      Generate_Class_Init_Method (Gen_Class);
      Generate_Default_Constructor (Gen_Class);

      Elab_Method := New_Method (Gen_Class, Name ("_elabs"), Void_Type, True);
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

      if Present (Comp_Body) then
         Elab_Method
           := New_Method (Gen_Class, Name ("_elabb"), Void_Type, True);
         Open_Method (Elab_Method);
         Set_Current_Method (Elab_Method);
         Method_Stack.Push (Elab_Method);

         if Present (Aux_Decls_Node (Comp_Body)) then
            Translate_Declarations (Declarations (Aux_Decls_Node (Comp_Body)));
            Translate_Statements   (Actions (Aux_Decls_Node (Comp_Body)));
         end if;

         Gen_Method_Return;
         Method_Stack.Pop;
         Close_Method (Elab_Method);
      end if;

      pragma Assert (Method_Stack.Empty);

      pragma Assert (Class_Stack.Top = Gen_Class);

      Generate_Null_Methods (Gen_Class);
      End_Class_File (Gen_Class);
      Class_Stack.Pop;
   end Generate_Generic_Unit_Class;

end Jx_Ch12;
