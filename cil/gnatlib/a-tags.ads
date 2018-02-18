------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                             A D A . T A G S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2011, AdaCore                     --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by --
-- AdaCore - http://www.adacore.com                                         --
--                                                                          --
------------------------------------------------------------------------------

--  .NET/JVM version of Ada.Tags

with System;
with System.HTable;

package Ada.Tags is
   pragma Preelaborate_05;
   --  In accordance with Ada 2005 AI-362

   type Tag is private;
   pragma Preelaborable_Initialization (Tag);

   No_Tag : constant Tag;

   function Expanded_Name (T : Tag) return String;

   function Wide_Expanded_Name (T : Tag) return Wide_String;
   pragma Ada_05 (Wide_Expanded_Name);

   function Wide_Wide_Expanded_Name (T : Tag) return Wide_Wide_String;
   pragma Ada_05 (Wide_Wide_Expanded_Name);

   function External_Tag (T : Tag) return String;

   function Internal_Tag (External : String) return Tag;

   function Descendant_Tag
     (External : String;
      Ancestor : Tag) return Tag;
   pragma Ada_05 (Descendant_Tag);

   function Is_Descendant_At_Same_Level
     (Descendant : Tag;
      Ancestor   : Tag) return Boolean;
   pragma Ada_05 (Is_Descendant_At_Same_Level);

   function Parent_Tag (T : Tag) return Tag;
   pragma Ada_05 (Parent_Tag);

   type Tag_Array is array (Positive range <>) of Tag;

   function Interface_Ancestor_Tags (T : Tag) return Tag_Array;
   pragma Ada_05 (Interface_Ancestor_Tags);

   function Type_Is_Abstract (T : Tag) return Boolean;
   pragma Ada_2012 (Type_Is_Abstract);

   Tag_Error : exception;

private
   --  Structure of the Type_Specific_Data

   --                            Type Specific Data
   --                          +-------------------+
   --  TSD_Ptr --------------> | inheritance depth |
   --                          +-------------------+
   --                          |   tag kind        |
   --                          +-------------------+
   --                          |   access level    |
   --                          +-------------------+
   --                          |     alignment     |
   --                          +-------------------+
   --                          |   hash table link |
   --                          +-------------------+
   --                          |  type_is_abstract |
   --                          +-------------------+
   --                          | type_is_lib_level |
   --                          +-------------------+
   --                          |   Ifaces_Table   ---->  Interfaces
   --                          +-------------------+   +-----------+
   --       Select_Table   <--------    SSD        |   | Nb_Ifaces |
   --     +---------------+    +-------------------+   +-----------+
   --     |     Nb_Prim   |    | table of          |   |I1'Tag,OSD1 --->
   --     +---------------+    :    ancestor       :   :           :
   --     |Index-1, Kind-1|    |       tags        |   |           |
   --     :               :    +-------------------+   +-----------+
   --     +---------------+

   type Tag is new System.Address;
   type Interface_Tag is new System.Address;
   --  Tag is really the equivalent of java.lang.Class. However we cannot
   --  define it as such because that would introduce a circularity since
   --  Ada.Tags would depend on Interfaces.Java.Lang.Class and conversely
   --  Interfaces.Java.Lang.Class would depend on Ada.Tags since it
   --  contains a tagged type. Thus we declare Tag as a System.Address
   --  which gets mapped by JGNAT into java.lang.Object. The body of
   --  Ada.Tags can then insert the appropriate conversions to and from
   --  java.lang.Class.

   No_Tag : constant Tag := Tag (System.Null_Address);

   type Object_Specific_Data_Array is array (Positive range <>) of Positive;

   type Object_Specific_Data (OSD_Num_Prims : Positive) is record
      OSD_Table : Object_Specific_Data_Array (1 .. OSD_Num_Prims);
      --  Table used to handle dispatching select through synchronized
      --  interfaces. Nb_Prim is the number of non-predefined primitive
      --  operations.
   end record;

   type Object_Specific_Data_Ptr is access all Object_Specific_Data;

   type Interface_Data_Element is record
      Iface_Tag : Tag;
      OSD       : Object_Specific_Data_Ptr;
   end record;

   type Interfaces_Array is array (Natural range <>) of Interface_Data_Element;

   type Interface_Data (Nb_Ifaces : Positive) is record
      Ifaces_Table : Interfaces_Array (1 .. Nb_Ifaces);
   end record;

   type Interface_Data_Ptr is access all Interface_Data;
   --  Table of abstract interfaces used to give support to backward interface
   --  conversions and also to IW_Membership.

   --  Primitive operation kinds. These values differentiate the kinds of
   --  callable entities stored in the dispatch table. Certain kinds may
   --  not be used, but are added for completeness.

   type Prim_Op_Kind is
     (POK_Function,
      POK_Procedure,
      POK_Protected_Entry,
      POK_Protected_Function,
      POK_Protected_Procedure,
      POK_Task_Entry,
      POK_Task_Function,
      POK_Task_Procedure);

   --  Tagged type kinds with respect to concurrency and limitedness

   type Tagged_Kind is
     (TK_Abstract_Limited_Tagged,
      TK_Abstract_Tagged,
      TK_Limited_Tagged,
      TK_Protected,
      TK_Tagged,
      TK_Task);

   --  Select specific data types

   type Select_Specific_Data_Element is record
      Index : Positive;
      Kind  : Prim_Op_Kind;
   end record;

   type Select_Specific_Data_Array is
     array (Positive range <>) of Select_Specific_Data_Element;

   type Select_Specific_Data (Nb_Prim : Positive) is record
      SSD_Table : Select_Specific_Data_Array (1 .. Nb_Prim);
      --  NOTE: Nb_Prim is the number of non-predefined primitive operations
   end record;

   type Select_Specific_Data_Ptr is access all Select_Specific_Data;

   --  A table used to store the primitive operation kind and entry index of
   --  primitive subprograms of a type that implements a limited interface.
   --  The Select Specific Data table resides in the Type Specific Data of a
   --  type. This construct is used in the handling of dispatching triggers
   --  in select statements.

   type Tag_Table is array (Natural range <>) of Tag;

   type Type_Specific_Data (Idepth : Natural);
   type Type_Specific_Data_Ptr is access all Type_Specific_Data;

   type Type_Specific_Data (Idepth : Natural) is record
   --  The discriminant Idepth is the Inheritance Depth Level: Used to
   --  implement the membership test associated with single inheritance of
   --  tagged types in constant-time. It also indicates the size of the
   --  Tags_Table component.

      Tag_Kind     : Tagged_Kind;

      Access_Level : Natural;
      --  Accessibility level required to give support to Ada 2005 nested type
      --  extensions. This feature allows safe nested type extensions by
      --  shifting the accessibility checks to certain operations, rather than
      --  being enforced at the type declaration. In particular, by performing
      --  run-time accessibility checks on class-wide allocators, class-wide
      --  function return, and class-wide stream I/O, the danger of objects
      --  outliving their type declaration can be eliminated (Ada 2005: AI-344)

      Alignment     : Natural;
      HT_Link       : Type_Specific_Data_Ptr;
      --  Components used to support to the Ada.Tags subprograms in RM 3.9

      Type_Is_Abstract : Boolean;
      --  True if the type is abstract (Ada 2012: AI05-0173)

      Type_Is_Library_Level : Boolean;
      --  True if the type is declared at library level

      Interfaces_Table : Interface_Data_Ptr;
      --  Pointer to the table of interface tags. It is used to implement the
      --  membership test associated with interfaces and also for backward
      --  abstract interface type conversions (Ada 2005:AI-251)

      SSD : Select_Specific_Data_Ptr;
      --  Pointer to a table of records used in dispatching selects. This field
      --  has a meaningful value for all tagged types that implement a limited,
      --  protected, synchronized or task interfaces and have non-predefined
      --  primitive operations.

      Tags_Table : Tag_Table (0 .. Idepth);
      --  Table of ancestor tags. Its size actually depends on the inheritance
      --  depth level of the tagged type.
   end record;

   procedure Check_Interface_Conversion (Src_Tag : Tag; Trg_Tag : Tag);
   --  Raises CE if Src_Tag is the tag of a type which does not cover the type
   --  of Trg_Tag.

   procedure Check_TSD (TSD : Type_Specific_Data_Ptr);
   --  Ada 2012 (AI-113): Raise Program_Error if the external tag of this TSD
   --  is the same as the external tag for some other tagged type declaration.

   function CW_Membership (Obj_Tag : Tag; Typ_Tag : Tag) return Boolean;
   pragma Inline_Always (CW_Membership);
   --  Given the tag of an object and the tag associated to a type, return
   --  true if Obj is in Typ'Class.

   function Get_Access_Level (T : Tag) return Natural;
   --  Return the accessibility level of T

   function Get_Alignment (T : Tag) return Natural;
   --  Return the alignment of T

   function Get_Entry_Index (T : Tag; Position : Positive) return Positive;
   --  Ada 2005 (AI-251): Return a primitive operation's entry index (if entry)
   --  given a dispatch table T and a position of a primitive operation in T.

   function Get_Offset_Index
     (Obj_Tag   : Tag;
      Iface_Tag : Tag;
      Position  : Positive) return Positive;
   --  Ada 2005 (AI-251): Given the tag of an object, the tag of one of its
   --  implemented interfaces, and the Position of an interface primitive,
   --  retrieve the index of this primitive in the Offset Specific Data
   --  table of Obj_Tag.

   function Get_Prim_Op_Kind
     (T        : Tag;
      Position : Positive) return Prim_Op_Kind;
   --  Ada 2005 (AI-251): Return a primitive operation's kind given a dispatch
   --  table T and a position of a primitive operation in T.

   function Get_Tagged_Kind (T : Tag) return Tagged_Kind;
   --  Ada 2005 (AI-345): Return the tagged kind of a type in the context of
   --  concurrency and limitedness.

   function IW_Membership (Obj_Tag : Tag; Typ_Tag : Tag) return Boolean;
   --  Ada 2005 (AI-251): General routine that checks if a given object
   --  implements a tagged type. Its common usage is to check if Obj is in
   --  Iface'Class, but it is also used to check if a class-wide interface
   --  implements a given type (Iface_CW_Typ in T'Class). For example:
   --
   --      type I is interface;
   --      type T is tagged ...
   --
   --      function Test (O : I'Class) is
   --      begin
   --         return O in T'Class.
   --      end Test;

   procedure Register_TSD (TSD : Type_Specific_Data_Ptr);
   --  Register the TSD in the runtime

   procedure Set_Entry_Index (T : Tag; Position : Positive; Value : Positive);
   --  Ada 2005 (AI-345): Set the entry index of a primitive operation in T's
   --  TSD table indexed by Position.

   procedure Set_Prim_Op_Kind
     (T        : Tag;
      Position : Positive;
      Value    : Prim_Op_Kind);
   --  Ada 2005 (AI-251): Set the kind of a primitive operation in T's TSD
   --  table indexed by Position.

   -----------------
   --  TSD_HTable --
   -----------------

   --  Declarations associated with the hash table used to register
   --  the TSDs in the runtime. These declarations cannot be located
   --  in the body of this package since the hash table must be
   --  available when the spec of package Ada.Tags is elaborated
   --  since the elaboration of the spec of other runtime packages
   --  invoke service Check_TSD.

   function Get_Key (T : Type_Specific_Data_Ptr) return Tag;
   --  Returns address of a null terminated string containing the external name

   type HTable_Headers is range 1 .. 64;

   --  Internal package that defines the routines used for the instantiation of
   --  a new System.HTable.Static_HTable (see below). See spec in g-htable.ads
   --  for details of usage.

   package HTable_Subprograms is
      procedure Set_Next
        (T    : Type_Specific_Data_Ptr;
         Next : Type_Specific_Data_Ptr);

      function Get_Next
        (T : Type_Specific_Data_Ptr)
        return Type_Specific_Data_Ptr;

      function Hash (F : Tag) return HTable_Headers;

      function Equal (A, B : Tag) return Boolean;
   end HTable_Subprograms;

   package External_Tag_HTable is new System.HTable.Static_HTable (
     Header_Num => HTable_Headers,
     Element    => Type_Specific_Data,
     Elmt_Ptr   => Type_Specific_Data_Ptr,
     Null_Ptr   => null,
     Set_Next   => HTable_Subprograms.Set_Next,
     Next       => HTable_Subprograms.Get_Next,
     Key        => Tag,
     Get_Key    => Get_Key,
     Hash       => HTable_Subprograms.Hash,
     Equal      => HTable_Subprograms.Equal);

end Ada.Tags;
