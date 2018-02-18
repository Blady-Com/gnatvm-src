------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J V M . M A P                               --
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

with Atree;      use Atree;
with Einfo;      use Einfo;
with JVM.Info;   use JVM.Info;
with Sinfo;      use Sinfo;
with GNAT.Table;

package body JVM.Map is

   package Entity_Map is new GNAT.Table (
     Table_Component_Type => JVM_Id,
     Table_Index_Type     => Entity_Id'Base,
     Table_Low_Bound      => Entity_Id'First,
     Table_Initial        => 10_000,
     Table_Increment      => 100);

   --------------------
   -- Initialize_Map --
   --------------------

   procedure Initialize_Map (Last_Entity : Entity_Id) is
   begin
      Entity_Map.Set_Last (Last_Entity);
      for Index in Entity_Id'First .. Last_Entity loop
         Entity_Map.Table (Index) := Null_JVM_Id;
      end loop;
   end Initialize_Map;

   ----------------
   -- Ada_Entity --
   ----------------

   function Ada_Entity (C : Class_Id) return Entity_Id is
   begin
      return Ada_Entity (Class_Type (C));
   end Ada_Entity;

   function Ada_Entity (L : Local_Var_Id) return Entity_Id is
   begin
      for J in Entity_Id'First .. Entity_Map.Last loop
         if Local_Var_Id (Entity_Map.Table (J)) = L then
            return J;
         end if;
      end loop;

      return Empty;
   end Ada_Entity;

   function Ada_Entity (T : Type_Id) return Entity_Id is
   begin
      for J in Entity_Id'First .. Entity_Map.Last loop
         if Type_Id (Entity_Map.Table (J)) = T then
            return J;
         end if;
      end loop;

      return Empty;
   end Ada_Entity;

   function Ada_Entity (M : Method_Id) return Entity_Id is
   begin
      for J in Entity_Id'First .. Entity_Map.Last loop
         if Method_Id (Entity_Map.Table (J)) = M then
            return J;
         end if;
      end loop;

      return Empty;
   end Ada_Entity;

   -------------
   -- Set_Map --
   -------------

   procedure Set_Map (Ada_Entity : Entity_Id; C : JVM.Class_Id) is
   begin
      pragma Assert (Entity_Map.Table (Ada_Entity) = Null_JVM_Id);
      pragma Assert (Ekind (Ada_Entity) = E_Package
                       or else Ekind (Ada_Entity) = E_Function
                       or else Ekind (Ada_Entity) = E_Procedure
                       or else Ekind (Ada_Entity) = E_Exception
                       or else Ekind (Ada_Entity) = E_Exception_Type
                       or else Ekind (Ada_Entity) = E_Access_Subprogram_Type
                       or else Ekind (Ada_Entity) in Einfo.Record_Kind
                       or else Ekind (Ada_Entity) in Einfo.Generic_Unit_Kind);

      Entity_Map.Table (Ada_Entity) := JVM_Id (C);
   end Set_Map;

   procedure Set_Map (Ada_Entity : Entity_Id; T : JVM.Type_Id) is
   begin
      pragma Assert (Entity_Map.Table (Ada_Entity) = Null_JVM_Id
        or else Entity_Map.Table (Ada_Entity) = JVM_Id (T));

      pragma Assert (Ekind (Ada_Entity) in Einfo.Type_Kind
        or else Ekind (Ada_Entity) = E_Exception
        or else Ekind (Ada_Entity) = E_Void);

      Entity_Map.Table (Ada_Entity) := JVM_Id (T);
   end Set_Map;

   procedure Set_Map (Ada_Entity : Entity_Id; F : JVM.Field_Id) is
   begin
      pragma Assert (Entity_Map.Table (Ada_Entity) = Null_JVM_Id);
      pragma Assert (Ekind (Ada_Entity) = E_Component
             or else Ekind (Ada_Entity) = E_Discriminant
             or else Ekind (Ada_Entity) = E_Variable
             or else Ekind (Ada_Entity) = E_Constant);

      Entity_Map.Table (Ada_Entity) := JVM_Id (F);
   end Set_Map;

   procedure Set_Map (Ada_Entity : Entity_Id; M : JVM.Method_Id) is
   begin
      pragma Assert (Entity_Map.Table (Ada_Entity) = Null_JVM_Id);
      pragma Assert (Ekind (Ada_Entity) = E_Function
             or else Ekind (Ada_Entity) = E_Procedure);

      Entity_Map.Table (Ada_Entity) := JVM_Id (M);
   end Set_Map;

   procedure Set_Map (Ada_Entity : Entity_Id; L : JVM.Local_Var_Id) is
   begin
      pragma Assert (Entity_Map.Table (Ada_Entity) = Null_JVM_Id);
      pragma Assert (Ekind (Ada_Entity) = E_Variable
             or else Ekind (Ada_Entity) = E_Constant
             or else Ekind (Ada_Entity) = E_Loop_Parameter
             or else Ekind (Ada_Entity) in Einfo.Formal_Kind);

      Entity_Map.Table (Ada_Entity) := JVM_Id (L);
   end Set_Map;

   procedure Set_Map (Ada_Node : Node_Id; L : JVM.Label_Id) is
   begin
      pragma Assert (Entity_Map.Table (Ada_Node) = Null_JVM_Id);
      pragma Assert (Nkind (Ada_Node) = N_Loop_Statement
             or else (Nkind (Ada_Node) in N_Entity
                       and then (Ekind (Ada_Node) = E_Label
                                  or else Ekind (Ada_Node) = E_Block
                                  or else Ekind (Ada_Node) = E_Loop)));

      Entity_Map.Table (Ada_Node) := JVM_Id (L);
   end Set_Map;

   ----------------
   -- JVM_Entity --
   ----------------

   function JVM_Entity (Ada_Entity : Entity_Id) return JVM.Class_Id is
   begin
      case Get_Entity_Kind (Entity_Map.Table (Ada_Entity)) is
         when Class_Entity =>
            return Class_Id (Entity_Map.Table (Ada_Entity));

         when No_Entity =>
            return Null_Class;

         when others =>
            pragma Assert (False);
            return Null_Class;
      end case;
   end JVM_Entity;

   function JVM_Entity (Ada_Entity : Entity_Id) return JVM.Type_Id is
   begin
      case Get_Entity_Kind (Entity_Map.Table (Ada_Entity)) is
         when Type_Entity =>
            return Type_Id (Entity_Map.Table (Ada_Entity));

         when No_Entity =>
            return Null_Type;

         when others =>
            pragma Assert (False);
            return Null_Type;
      end case;
   end JVM_Entity;

   function JVM_Entity (Ada_Entity : Entity_Id) return JVM.Field_Id is
   begin
      case Get_Entity_Kind (Entity_Map.Table (Ada_Entity)) is
         when Field_Entity =>
            return Field_Id (Entity_Map.Table (Ada_Entity));

         when No_Entity =>
            return Null_Field;

         when others =>
            pragma Assert (False);
            return Null_Field;
      end case;
   end JVM_Entity;

   function JVM_Entity (Ada_Entity : Entity_Id) return JVM.Method_Id is
   begin
      case Get_Entity_Kind (Entity_Map.Table (Ada_Entity)) is
         when Method_Entity =>
            return Method_Id (Entity_Map.Table (Ada_Entity));

         when No_Entity =>
            return Null_Method;

         when others =>
            pragma Assert (False);
            return Null_Method;
      end case;
   end JVM_Entity;

   function JVM_Entity (Ada_Entity : Entity_Id) return JVM.Local_Var_Id is
   begin
      case Get_Entity_Kind (Entity_Map.Table (Ada_Entity)) is
         when Local_Var_Entity =>
            return Local_Var_Id (Entity_Map.Table (Ada_Entity));

         when No_Entity =>
            return Null_Local_Var;

         when others =>
            pragma Assert (False);
            return Null_Local_Var;
      end case;
   end JVM_Entity;

   function JVM_Entity (Ada_Node : Node_Id) return JVM.Label_Id is
   begin
      case Get_Entity_Kind (Entity_Map.Table (Ada_Node)) is
         when Label_Entity =>
            return Label_Id (Entity_Map.Table (Ada_Node));

         when No_Entity =>
            return Null_Label;

         when others =>
            pragma Assert (False);
            return Null_Label;
      end case;
   end JVM_Entity;

end JVM.Map;
