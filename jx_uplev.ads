------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J X _ U P L E V                              --
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

--  This package provides support for nested subprograms. It enables the
--  generation of activation record classes and their fields. Also manages the
--  use of static links to support up-level addressing of AR fields from nested
--  subprograms.

with JVM;      use JVM;
with Namet;    use Namet;
with Types;    use Types;
with J_Stack;

package Jx_Uplev is

   type AR_Field_Rec;
   type AR_Field_Ref is access AR_Field_Rec;

   type AR_Field_Rec is record
      Field : Field_Id;
      Next  : AR_Field_Ref;
   end record;
   --  Objects of this type denote fields of Activation Record (AR) objects.
   --  Such fields represent the home location of a local variable that is
   --  referenced up-level from a nested method.

   type Active_Rec;
   type AR_Access is access Active_Rec;

   type Active_Rec is record
      Parent   : AR_Access;
      Method   : Method_Id;
      AR_Class : Class_Id;
      AR_Obj   : Local_Var_Id;
      Fields   : AR_Field_Ref;
   end record;
   --  Objects of this type hold information about a method's Activation
   --  Record, including a reference Parent to a further enclosing method's AR
   --  (if any), the method's AR class and object, and the list of fields for
   --  local variables referenced up-level from subprograms nested within
   --  Method.

   package AR_Stack is new J_Stack (AR_Access, 100);
   --  The stack of active activation records, which represents the static
   --  nesting of ARs for methods enclosing the current method.

   function Access_AR_Field (Ada_Obj : Entity_Id) return Field_Id;
   --  Loads the current method's static link parameter and, if necessary,
   --  generates code to chain up the static links of any enclosing methods'
   --  activation records until reaching the object's associated AR. Returns a
   --  Field_Id for the local's corresponding AR field.

   function Access_From_Current_AR (Ada_Obj : Entity_Id) return Boolean;
   --  Returns True if Ada_Obj must be accessed from the current method's
   --  activation record. This is only valid to call for objects that are
   --  local to the current method. Note that only scalar and access variables
   --  ever need to be retrieved from their method's activation record.

   function AR_Field (AR : AR_Access; Ada_Obj : Entity_Id) return Field_Id;
   --  Returns the Field_Id associated with an up-level AR field with the given
   --  entity.

   function Enclosing_Method (E : Entity_Id) return Method_Id;
   --  Utility function to return the JVM method entity of the subprogram
   --  enclosing the entity E, if any (returns a null id if no enclosing
   --  subprogram).

   procedure End_Activation_Record (Method : Method_Id);
   --  Completes generation of the AR class associated with Method.

   function Is_Global_Entity (E : Entity_Id) return Boolean;
   --  Returns true if and only if E is a library level entity or an entity
   --  declared within a block statement that is at the library level (such as
   --  a block within the statement part of a package body).

   procedure Load_Static_Link (Parent_Method : Method_Id);
   --  Generates code to load a reference to the activation record associated
   --  with the parent method of some entity being referenced by the current
   --  method (the entity must be either a local variable or subprogram of
   --  Parent_Method). This may cause generation of code to chain up a set of
   --  static links for methods enclosing the current method.

   procedure Load_Up_Level_Field (Method : Method_Id; Name : Name_Id);
   --  Generates code to load an up-level field with the given name that is
   --  contained within the AR associated with Method.

   procedure Make_Activation_Record (Method : Method_Id; Outer  : Class_Id);
   --  Creates an activation record (AR) class for Method. This AR class is
   --  created as inner class of Outer.

   procedure Register_Up_Level_Reference (Ada_Obj : Entity_Id);
   --  Registers an Ada object (Ada_Obj) as being referenced up-level from a
   --  nested subprogram (the current method). If this is the first up-level
   --  reference to the object, then code is generated to copy the original
   --  value of the variable into its corresponding field in the AR object of
   --  the associated parent method.

   procedure Register_Up_Level_Reference
     (Method : Method_Id;
      LV     : Local_Var_Id);
   --  Registers a local variable LV of Method as being referenced up-level
   --  from the current method. If this is the first up-level reference to the
   --  variable, then code is generated to copy the original value of the
   --  variable into its corresponding field in the AR object of Method.

   function Up_Level_Field_Name (Local : Entity_Id) return Name_Id;
   --  Returns the Name_Id to use for a field in an up-level activation record
   --  object. If Local is declared immediately within a subprogram then its
   --  simple name will be returned, otherwise its fully expanded name will be
   --  used. The full name is only needed when conflicts with simple names are
   --  possible, such as for variables declared within packages or blocks
   --  inside a subprogram.

end Jx_Uplev;
