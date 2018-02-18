------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 3                                --
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

--  This package implements translation of object and type declarations
--  into their corresponding JVM equivalents.

with JVM;    use JVM;
with Types;  use Types;

package Jx_Ch3 is

   procedure Allocate_Array_Components
     (Arr_Type    : Entity_Id;
      Obj         : Local_Var_Id;
      Orig_Object : Entity_Id := Empty);
   --  Generates a loop to allocate the composite components of an array
   --  object denoted by Obj whose Ada type is Arr_Type.
   --  Orig_Object is the object in the gnat tree corresponding to Obj if
   --  not empty.

   procedure Allocate_Composite_Components
     (Ada_Type : Entity_Id;
      Obj      : Local_Var_Id);
   --  Generates code to allocate any composite components of the record
   --  type Ada_Type for the object denoted by Obj. The allocation will
   --  happen either as part of the type's default <init> constructor or
   --  else in the _init_proc constructor of the type if it has one.

   procedure Allocate_Multiarray
     (Subtyp  : Entity_Id;
      JVM_Typ : Type_Id;
      Obj_LV  : Local_Var_Id := Null_Local_Var);
   --  Allocate memory for a multiarray handling ultra-flat arrays. Subtyp
   --  is the entity used to locate each index of the array. JVM_Typ is the
   --  JVM type of the allocated array object. If Obj_LV is not null, then
   --  discriminant bounds will be obtained by loading the discriminant from
   --  the object denoted by the the local variable.

   procedure Gen_Invoke_Deep_Clone (Typ : Entity_Id);
   --  Generates a call to the _deep_clone method associated with
   --  a record type. Raises an exception if no associated deep clone
   --  method exists.

   procedure Gen_Invoke_Deep_Copy (Typ : Entity_Id);
   --  Generates a call to the _deep_copy method associated with
   --  a record type, an array type whose components are composite,
   --  or an array type with an aliased elementary component type.
   --  Raises an exception if no associated deep copy method exists.

   procedure Gen_Invoke_Init (Class : Class_Id; Typ : Entity_Id);
   --  Generates a call to the <init> method associated with
   --  a record type. Takes care of passing a static link when
   --  needed.

   procedure Translate_Declarations (Declarations : List_Id);
   --  Translates a list of declarations into its JVM equivalent

   procedure Translate_Object_Declaration (Obj : Node_Id);
   --  Translates an object declaration into a static field
   --  of a package class (in the case of a library-level
   --  object) or into a local variable of the current method.

   procedure Translate_Subtype (S : Entity_Id);
   --  Processes the constraint of a subtype if needed. (NOTE: At
   --  present this procedure performs no actions. Not clear whether
   --  any will be needed in the future.) ???

   procedure Translate_Type (T : Entity_Id);
   --  Translates a full type declaration into its JVM equivalent,
   --  associating the Ada type with a corresponding JVM type entity
   --  and generating a new class if needed (e.g., for a record type).

end Jx_Ch3;
