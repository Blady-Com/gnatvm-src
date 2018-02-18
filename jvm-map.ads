------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J V M . M A P                               --
--                                                                          --
--                                 S p e c                                  --
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

package JVM.Map is

   --  This package implements a map from GNAT Ada entity nodes to
   --  JVM entity nodes. Checks are performed to help ensure consistent
   --  usage. In particular, the following associations are allowed
   --  and violations will cause an exception:
   --
   --    Class_Id     => E_Package
   --            or else E_Function
   --            or else E_Procedure
   --            or else E_Exception
   --            or else E_Exception_Type
   --            or else in Einfo.Record_Kind
   --            or else in Einfo.Generic_Unit_Kind
   --
   --    Type_Id      => in Einfo.Type_Kind
   --
   --    Field_Id     => E_Component
   --            or else E_Discriminant
   --            or else E_Variable or else E_Constant
   --
   --    Method_Id    => E_Function
   --            or else E_Procedure
   --            or else E_Entry
   --            or else E_Entry_Family
   --
   --    Local_Var_Id => E_Variable
   --            or else E_Constant
   --            or else E_Loop_Parameter
   --            or else E_In_Parameter
   --            or else E_Out_Parameter
   --            or else E_In_Out_Parameter
   --
   --    Label_Id     => E_Label
   --            or else E_Block
   --            or else E_Loop
   --            or else N_Loop_Statement

   procedure Initialize_Map (Last_Entity : Entity_Id);
   --  This procedure initializes the JVM entity map.
   --  It must be called prior to calling any other
   --  operations of this package.

   --  The Set_Map procedures establish an association between
   --  an Entity_Id and one of the five JVM entity kinds. The
   --  above-documented restrictions on valid associations are
   --  enforced. An exception will be raised if a restriction
   --  is violated or if the given Ada entity already has an
   --  an associated JVM entity.

   procedure Set_Map (Ada_Entity : Entity_Id; C : JVM.Class_Id);
   procedure Set_Map (Ada_Entity : Entity_Id; T : JVM.Type_Id);
   procedure Set_Map (Ada_Entity : Entity_Id; F : JVM.Field_Id);
   procedure Set_Map (Ada_Entity : Entity_Id; M : JVM.Method_Id);
   procedure Set_Map (Ada_Entity : Entity_Id; L : JVM.Local_Var_Id);
   procedure Set_Map (Ada_Node   : Node_Id;   L : JVM.Label_Id);

   --  The JVM_Entity functions take an Entity_Id and return
   --  its associated JVM entity id. An exception is raised
   --  if either the Ada entity has no associated JVM entity
   --  or if the Ada entities associated JVM entity is not
   --  appropriate for the returned JVM entity id type.

   function JVM_Entity (Ada_Entity : Entity_Id) return JVM.Class_Id;
   function JVM_Entity (Ada_Entity : Entity_Id) return JVM.Type_Id;
   function JVM_Entity (Ada_Entity : Entity_Id) return JVM.Field_Id;
   function JVM_Entity (Ada_Entity : Entity_Id) return JVM.Method_Id;
   function JVM_Entity (Ada_Entity : Entity_Id) return JVM.Local_Var_Id;
   function JVM_Entity (Ada_Node   : Node_Id)   return JVM.Label_Id;

   --  The following functions provide the reverse functionality: take a JVM
   --  entity id and return the corresponding Entity_Id. If the JVM entity has
   --  not been mapped then returns Empty.

   function Ada_Entity (C : Class_Id)     return Entity_Id;
   function Ada_Entity (M : Method_Id)    return Entity_Id;
   function Ada_Entity (T : Type_Id)      return Entity_Id;
   function Ada_Entity (L : Local_Var_Id) return Entity_Id;

end JVM.Map;
