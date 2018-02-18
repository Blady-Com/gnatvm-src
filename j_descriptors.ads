------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         J _ D E S C R I P T O R S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2009-2013, AdaCore                     --
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

--  This file contains general utilities used to handle descriptors

with JVM;   use JVM;
with Types; use Types;

package J_Descriptors is

   procedure Build_Array_Descriptor (Typ : Entity_Id);
   --  Create a descriptor class to encapsulate the array and its bounds. This
   --  subprogram takes care of building the class using the entity name and
   --  package name of Typ and linking the JVM type of Typ and the new class.

   procedure Deep_Copy_Array_Descriptor (Expr : Node_Id);
   --  Generates code that copies the array descriptor of Expr available in the
   --  top of the stack. It copies also the contents of the wrapped array.

   function Descriptor_Field (JVM_Type : Type_Id) return Field_Id;
   --  Returns the 'all' field associated with a JVM descriptor type

   function Descriptor_Field (Obj_Or_Type : Entity_Id) return Field_Id;
   --  Returns the 'all' field associated with an object or with a type that
   --  has an associated descriptor type.

   function Descriptor_Type (Obj_Or_Type : Entity_Id) return Type_Id;
   --  Returns the JVM descriptor type associated with an object or, in the
   --  case of a type argument, returns a descriptor type that has an 'all'
   --  component of the given type. Raises an exception if Obj_Or_Type has
   --  no associated descriptor type.

   procedure Generate_Array_Descriptor (Expr : Node_Id);
   --  This routine receives in the stack an array and replaces it by an array
   --  descriptor for Expr referencing such array.

   function Is_Access_Descriptor (Obj_Or_Type : Type_Id) return Boolean;
   --  Retuns True if Obj_Or_Type has an associated descriptor type

   function Needs_Access_Descriptor (Ada_Entity : Entity_Id) return Boolean;
   --  Returns True if and only if Ada_Entity is an out (or in-out) mode
   --  parameter, an aliased object, or an aliased component having an
   --  associated descriptor type. The type of such an object is necessarily
   --  a scalar or (non-subprogram) access type.

end J_Descriptors;
