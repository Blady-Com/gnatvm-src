------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 8                                --
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
with Errout;        use Errout;
with J_Descriptors; use J_Descriptors;
with JVM;           use JVM;
with Jx_Ch4;        use Jx_Ch4;
with Jx_Decl;       use Jx_Decl;
with Jx_Uplev;      use Jx_Uplev;
with Sem_Util;      use Sem_Util;
with Sinfo;         use Sinfo;

package body Jx_Ch8 is

   -----------------------
   -- JVM_Current_Scope --
   -----------------------

   function JVM_Current_Scope return Entity_Id is
   begin
      return JVM_Scope_Stack.Top;
   end JVM_Current_Scope;

   -------------------------------
   -- Translate_Object_Renaming --
   -------------------------------

   procedure Translate_Object_Renaming (Obj_Renaming : Node_Id) is
      Obj_Entity : constant Entity_Id := Defining_Entity (Obj_Renaming);
      Is_Global  : constant Boolean   := Is_Global_Entity (Obj_Entity);
      Obj_Name   : constant Node_Id   := Renamed_Object (Obj_Entity);
      Obj_Type   : constant Entity_Id := Full_Type (Obj_Entity);
      Obj_Field  : Field_Id;
      Local_Var  : Local_Var_Id;

   begin
      --  Renamed Entity slices are evaluated when referenced

      if Nkind (Obj_Name) = N_Slice
        and then Is_Entity_Name (Prefix (Obj_Name))
      then
         return;

      --  The case of renamings of composite components is handled by
      --  evaluating the renamed object name and saving the reference
      --  in a new JVM entity associated with the renaming entity.

      elsif Ekind (Obj_Type) in Composite_Kind then

         if Ekind (Obj_Type) in Einfo.Array_Kind then
            --  ??? We lose the bounds here: do we really want this ?
            Evaluate_Array_Address (Obj_Name);
         else
            Evaluate_Expr (Obj_Name);
         end if;

         if Is_Global then
            Declare_Field (Current_Compilation_Class, Obj_Entity);
            Obj_Field := JVM_Field (Obj_Entity);
            Gen_Put_Static_Field (Obj_Field);

         else
            Declare_Local_Variable (Obj_Entity);
            Local_Var := JVM_Local_Var (Obj_Entity);
            Gen_Store_Local (Local_Var);
         end if;

      --  Handle renaming of elementary objects where the name of the renamed
      --  object is not an entity. For renamings of entities we simply
      --  reevaluate the object name on each reference to the renaming. The
      --  more general case requires saving a partial evaluation of the name
      --  and involves more complex actions on evaluating later references to
      --  the renaming. For now we only support non-entity renamings for
      --  selected components and function cals. The case where the object name
      --  is an indexed name will require saving both the reference to the
      --  array and the index, and reloading both of those when evaluating a
      --  reference to the renaming. It's not clear how to cleanly associate
      --  the renaming with two pieces of information in the indexed name case.

      --  There are also a few other cases we don't support yet, such as
      --  renaming of dereferenced access values. ???

      elsif not Is_Entity_Name (Obj_Name) then

         if Nkind_In (Obj_Name, N_Selected_Component, N_Indexed_Component,
                      N_Explicit_Dereference, N_Function_Call)
         then
            --  In case of explicit dereference, we will copy the pointer
            --  structure (e.g. prefix of the object) instead of the object
            --  itself. This will allow us to keep coherence between our
            --  variable and the original pointer.all

            --  Each time we will access this variable however, we will have
            --  to dereference it (see jx_ch4 for that)

            if Nkind_In (Obj_Name,
                         N_Selected_Component,
                         N_Indexed_Component,
                         N_Explicit_Dereference)
            then
               Evaluate_Expr (Prefix (Obj_Name));
               --  ??? In case of Indexed Component, we should evaluate the
               --  index and save it in someplace

            else
               Evaluate_Expr (Obj_Name);
            end if;

            if Top_Type /= JVM_Type (Obj_Entity)
              and then Is_Array_Descriptor (Top_Type)
            then
               Gen_Get_Field (Descriptor_Field (Top_Type));
            end if;

            if Is_Global then
               Declare_Field (Current_Compilation_Class, Obj_Entity);
               Obj_Field := JVM_Field (Obj_Entity);
               Gen_Put_Static_Field (Obj_Field);

            else
               Declare_Local_Variable (Obj_Entity);
               Local_Var := JVM_Local_Var (Obj_Entity);
               Gen_Store_Local (Local_Var);
            end if;

         elsif Nkind (Obj_Name) = N_Unchecked_Type_Conversion then
            Evaluate_Expr (Obj_Name);

            if Is_Global then
               Declare_Field (Current_Compilation_Class, Obj_Entity);
               Obj_Field := JVM_Field (Obj_Entity);
               Gen_Put_Static_Field (Obj_Field);

            else
               Declare_Local_Variable (Obj_Entity);
               Local_Var := JVM_Local_Var (Obj_Entity);
               Gen_Store_Local (Local_Var);
            end if;

         else
            --  ??? Should probably add a dummy declaration anyway
            Error_Msg_N
              ("unsupported form of scalar object renaming", Obj_Name);
         end if;
      end if;
   end Translate_Object_Renaming;

end Jx_Ch8;
