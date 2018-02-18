------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M . P O O L                              --
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

--  JVM.Pool defines a set of operations for creating constant pool items that
--  will be mapped into the constant pool of a class file. The items are
--  created by JVM and get associated with JVM entities. They are later used to
--  generate actual CP_Info records in the class file. Each item is added to a
--  list of items associated with a single class. Additionally, items that are
--  references to classes, fields, methods, and types are added to a reference
--  list associated with the referenced JVM entity.

with JVM_File; use JVM_File;

private
package JVM.Pool is

   ---------------------------------
   -- Constant Pool Item Creation --
   ---------------------------------

   function New_Pool_Item (Class : Class_Id; Kind : CP_Tag) return Pool_Id;
   --  Creates a constant pool item for Class with the given tag and returns
   --  a reference to it.

   ---------------------------------------------
   -- Constant Pool Attributes and Operations --
   ---------------------------------------------

   function Array_Item (Parent : Class_Id; A_Type : Type_Id) return Pool_Id;
   --  Returns a reference to a CONSTANT_Class pool item for A_Type that
   --  belongs to class Parent; creates a new pool item for A_Type if one does
   --  not already exist. Raises an exception if A_Type does not denote an
   --  array type.

   function Class_Item (Parent : Class_Id; Class : Class_Id) return Pool_Id;
   --  Returns a reference to a CONSTANT_Class pool item for Class that belongs
   --  to class Parent; creates a new pool item for Class if one does not
   --  already exist.

   function Double_Item (Parent : Class_Id; Value : Ureal) return Pool_Id;
   --  Creates and a CONSTANT_Double pool item for Value that belongs to class
   --  Parent and returns a reference to it.

   function Field_Item (Parent : Class_Id; Field : Field_Id) return Pool_Id;
   --  Returns a reference to a CONSTANT_Fieldref pool item for Field that
   --  belongs to class Parent; creates a new pool item for Field if one does
   --  not already exist.

   function Float_Item (Parent : Class_Id; Value : Ureal) return Pool_Id;
   --  Creates and a CONSTANT_Float pool item for Value that belongs to class
   --  Parent and returns a reference to it.

   function Integer_Item (Parent : Class_Id; Value : Uint) return Pool_Id;
   --  Creates and a CONSTANT_Integer pool item for Value that belongs to class
   --  Parent and returns a reference to it.

   function Is_Array_Type (CP_Item : Pool_Id) return Boolean;
   --  Returns true if CP_Item is associated with an array type

   function Long_Item (Parent : Class_Id; Value : Uint) return Pool_Id;
   --  Creates and a CONSTANT_Long pool item for Value that belongs to class
   --  Parent and returns a reference to it.

   function Method_Item (Parent : Class_Id; Method : Method_Id) return Pool_Id;
   --  Returns a reference to a CONSTANT_Methodref pool item for Method that
   --  belongs to class Parent; creates a new pool item for Method if one does
   --  not already exist.

   function Next_Pool_Item (CP_Item : Pool_Id) return Pool_Id;
   --  Returns a reference to the pool item that is the successor of CP_Item;
   --  returns Null_Pool_Item if CP_Item has no successor.

   function Next_Pool_Ref (CP_Item : Pool_Id) return Pool_Id;
   --  Returns a reference to CP_Item's next related pool item (i.e., another
   --  pool reference associated with the same JVM entity, but typically
   --  belonging to a different class); returns Null_Pool_Item if there is no
   --  next such pool item.

   function Parent_Class (CP_Item : Pool_Id) return Class_Id;
   --  Returns the class to which CP_Item belongs

   function Pool_Double (CP_Item : Pool_Id) return Ureal;
   --  Returns the Ureal value associated with CP_Item. Raises an exception if
   --  the kind of CP_Item is not CONSTANT_Double.

   function Pool_Float (CP_Item : Pool_Id) return Ureal;
   --  Returns the Ureal value associated with CP_Item. Raises an exception if
   --  the kind of CP_Item is not CONSTANT_Float.

   function Pool_Index (CP_Item : Pool_Id) return CP_Index;
   --  Returns the constant pool index associated with CP_Item

   function Pool_Integer (CP_Item : Pool_Id) return Uint;
   --  Returns the Uint value associated with CP_Item. Raises an exception if
   --  the kind of CP_Item is not CONSTANT_Integer.

   function Pool_Item_Tag (CP_Item : Pool_Id) return CP_Tag;
   --  Returns a value indicating the tag of the constant pool item

   function Pool_Long (CP_Item : Pool_Id) return Uint;
   --  Returns the Uint value associated with CP_Item. Raises an exception if
   --  the kind of CP_Item is not CONSTANT_Long.

   function Pool_String (CP_Item : Pool_Id) return String_Id;
   --  Returns the String_Id value associated with CP_Item. Raises an exception
   --  if the kind of CP_Item is not CONSTANT_String.

   function Ref_Class (CP_Item : Pool_Id) return Class_Id;
   --  Returns the class associated with CP_Item. Raises an exception if the
   --  kind of CP_Item is not CONSTANT_Class or if the item is associated with
   --  an array type.

   function Ref_Class_Type (CP_Item : Pool_Id) return Type_Id;
   --  Returns the class or array type associated with CP_Item. Raises an
   --  exception if the kind of CP_Item is not CONSTANT_Class.

   function Ref_Field (CP_Item : Pool_Id) return Field_Id;
   --  Returns the field associated with CP_Item. Raises an exception if the
   --  kind of CP_Item is not CONSTANT_Fieldref.

   function Ref_Method (CP_Item : Pool_Id) return Method_Id;
   --  Returns the method associated with CP_Item. Raises an exception if the
   --  kind of CP_Item is not CONSTANT_Methodref.

   function Ref_Type (CP_Item : Pool_Id) return Type_Id;
   --  Returns the type associated with CP_Item. Raises an exception if the
   --  kind of CP_Item is not CONSTANT_Name_And_Type.

   procedure Set_Next_CP_Item (CP_Item : Pool_Id; Next_Item : Pool_Id);
   --  Establishes the pool item denoted by Next_Item as the successor of
   --  CP_Item.

   procedure Set_Next_Pool_Ref (CP_Item : Pool_Id; Next_Ref : Pool_Id);
   --  Establishes the pool item denoted by Next_Ref as the successor in the
   --  constant pool reference list containing CP_Item.

   procedure Set_Parent_Class (CP_Item : Pool_Id; Class : Class_Id);
   --  Associates the constant pool item denoted by CP with Class

   procedure Set_Pool_Index (CP_Item : Pool_Id; Index : CP_Index);
   --  Associates the given constant pool index with CP_Item

   procedure Set_Ref_Class_Type (CP_Item : Pool_Id; JVM_Type : Type_Id);
   --  Establishes the class type associated with CP_Item. Raises an exception
   --  if the kind of CP_Item is not CONSTANT_Class or if JVM_Type does not
   --  denote a type associated with a class or array.

   procedure Set_Ref_Field (CP_Item : Pool_Id; Field : Field_Id);
   --  Establishes the field associated with CP_Item. Raises an exception if
   --  the kind of CP_Item is not CONSTANT_Fieldref.

   procedure Set_Ref_Method (CP_Item : Pool_Id; Method : Method_Id);
   --  Establishes the method associated with CP_Item. Raises an exception if
   --  the kind of CP_Item is not CONSTANT_Methodref.

   procedure Set_Ref_Type (CP_Item : Pool_Id; JVM_Type : Type_Id);
   --  Establishes the type associated with CP_Item. Raises an exception if the
   --  kind of CP_Item is not CONSTANT_Name_And_Type.

   function String_Item (Parent : Class_Id; Str : String_Id) return Pool_Id;
   --  Creates and a CONSTANT_String pool item for Str that belongs to class
   --  Parent and returns a reference to it.

   function Type_Item (Parent : Class_Id; Typ : Type_Id) return Pool_Id;
   --  Returns a reference to a CONSTANT_Name_And_Type pool item for Typ that
   --  belongs to class Parent; creates a new pool item for Typ if one does not
   --  already exist. We use CONSTANT_Name_And_Type instead of CONSTANT_Utf8 in
   --  order to distinguish type pool items from other kinds of Utf8 items.
   --  Eventually when the pool item is emitted into the constant pool it will
   --  be properly assigned a CONSTANT_Utf8 tag. It's not clear whether this
   --  distinction is really needed though, and it would be nice to clean this
   --  up at some point. ???

end JVM.Pool;
