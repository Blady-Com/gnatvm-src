------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M . P O O L                              --
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

with JVM.Info;   use JVM.Info;
with GNAT.Table;

package body JVM.Pool is

   --  A Pool_Item is used to represent a constant pool item associated
   --  with a JVM class.

   type Pool_Item (Kind : CP_Tag := CONSTANT_Empty) is
      record
         Next_Item : Pool_Id  := Null_Pool_Item;
         --  Link to the next pool item in a class's constant pool list

         Parent    : Class_Id := Null_Class;
         --  The class to which this pool item belongs

         Ref_Link  : Pool_Id  := Null_Pool_Item;
         --  A link to another pool item associated with the same JVM entity
         --  but belonging to a different class (used for classes, fields,
         --  methods, and types).

         Pool_Index : CP_Index := CP_Empty;
         --  The physical index of the corresponding CP_Info record in the
         --  class file's constant pool.

         case Kind is
            when CONSTANT_Empty =>
               null;

            when CONSTANT_Utf8 =>
               Utf8_Value : String_Id := No_String;

            when CONSTANT_Integer =>
               I_Value : Uint := Uint_0;

            when CONSTANT_Float =>
               F_Value : Ureal := Ureal_0;

            when CONSTANT_Long =>
               L_Value : Uint := Uint_0;

            when CONSTANT_Double =>
               D_Value : Ureal := Ureal_0;

            when CONSTANT_Class =>
               C_Type : Type_Id := Null_Type;

            when CONSTANT_String =>
               S_Value : String_Id := No_String;

            when CONSTANT_Fieldref =>
               Field : Field_Id := Null_Field;

            when CONSTANT_Methodref | CONSTANT_Interface_Methodref =>
               Method : Method_Id := Null_Method;

            when CONSTANT_Name_And_Type =>
               JVM_Type : Type_Id := Null_Type;

         end case;
      end record;

   type Pool_Item_Access is access all Pool_Item;

   --  The table where all Pool_Items reside

   package Pool_Table is new GNAT.Table (
     Table_Component_Type => Pool_Item,
     Table_Index_Type     => Pool_Id'Base,
     Table_Low_Bound      => Pool_Id'First,
     Table_Initial        => 1000,
     Table_Increment      => 100);

   First_Pool_Id : constant Pool_Id := Pool_Id'First + 1;
   --  First available id for Pool_Item allocation

   Next_Pool_Id : Pool_Id := First_Pool_Id;
   --  Next available table index for Pool_Item allocation

   ----------------------
   -- Utility Routines --
   ----------------------

   function CP_Item_Id
     (Parent  : Class_Id;
      CP_List : Pool_Id;
      CP_Kind : CP_Tag := CONSTANT_Empty) return Pool_Id;
   --  Returns a Pool_Id for a constant pool item belonging to class Parent and
   --  matching the CP_Tag indicated by CP_Kind (if CP_Kind /= CONSTANT_Empty)
   --  that exists in the list CP_List (there should be at most one); returns
   --  Null_Pool_Item if none exists.

   function Ptable (Id : Pool_Id) return Pool_Item_Access;
   pragma Inline (Ptable);
   --  This function returns an access value denoting the actual entry in
   --  Pool_Table that is associated with Id. Note that this access value is
   --  unsafe to use in conjunction with any operation that might involve
   --  adding entries to the table during the life of the access value. An
   --  exception is raised if Id is not in the current range of the table (but
   --  the range excludes the zeroth table index, which is reserved to denote
   --  the null value for Pool_Ids).

   ------------
   -- Ptable --
   ------------

   function Ptable (Id : Pool_Id) return Pool_Item_Access is
   begin
      pragma Assert (Id in First_Pool_Id .. Pool_Table.Last);
      return Pool_Table.Table (Id)'Unrestricted_Access;
   end Ptable;

   ----------------
   -- CP_Item_Id --
   ----------------

   function CP_Item_Id
     (Parent  : Class_Id;
      CP_List : Pool_Id;
      CP_Kind : CP_Tag := CONSTANT_Empty) return Pool_Id
   is
      CP_Id : Pool_Id := CP_List;

   begin
      while CP_Id /= Null_Pool_Item loop
         if Parent_Class (CP_Id) = Parent
           and then
             (CP_Kind = CONSTANT_Empty or else Pool_Item_Tag (CP_Id) = CP_Kind)
         then
            return CP_Id;
         end if;
         CP_Id := Next_Pool_Ref (CP_Id);
      end loop;

      return Null_Pool_Item;
   end CP_Item_Id;

   --------------------------------------------
   -- Constant Pool Item Creation Operations --
   ---------------------------------------------------------------------------

   function New_Pool_Item (Tag : CP_Tag) return Pool_Id;
   --  Creates a constant pool item with the given tag and returns a reference
   --  to it.

   -------------------
   -- New_Pool_Item --
   -------------------

   function New_Pool_Item (Tag : CP_Tag) return Pool_Id is
      New_Item : Pool_Item (Tag);

   begin
      Pool_Table.Table (Next_Pool_Id) := New_Item;
      Next_Pool_Id := Next_Pool_Id + 1;
      Pool_Table.Set_Last (Next_Pool_Id);

      return Next_Pool_Id - 1;
   end New_Pool_Item;

   function New_Pool_Item (Class : Class_Id; Kind : CP_Tag) return Pool_Id is
      CP_Id : constant Pool_Id := New_Pool_Item (Kind);

   begin
      Set_Parent_Class (CP_Id, Class);
      Set_Pool_Index (CP_Id, CP_Empty);
      Add_Pool_Item (Class, CP_Id);

      return CP_Id;
   end New_Pool_Item;

   --  ------------------------------------------------------------------------
   --  --------------------  Constant Pool Attributes  ------------------------
   --  ------------------------------------------------------------------------

   ----------------
   -- Array_Item --
   ----------------

   function Array_Item (Parent : Class_Id; A_Type : Type_Id) return Pool_Id is
      CP_Id : Pool_Id;

   begin
      pragma Assert (Type_Kind (A_Type) = Array_Kind);

      CP_Id := CP_Item_Id (Parent, First_Pool_Ref (A_Type), CONSTANT_Class);

      if CP_Id = Null_Pool_Item then
         CP_Id := New_Pool_Item (Parent, CONSTANT_Class);
         Set_Ref_Class_Type (CP_Id, A_Type);
         Add_Pool_Ref (A_Type, CP_Id);
      end if;

      return CP_Id;
   end Array_Item;

   ----------------
   -- Class_Item --
   ----------------

   function Class_Item (Parent : Class_Id; Class : Class_Id) return Pool_Id is
      CP_Id : Pool_Id := CP_Item_Id (Parent, First_Pool_Ref (Class));

   begin
      if CP_Id = Null_Pool_Item then
         CP_Id := New_Pool_Item (Parent, CONSTANT_Class);
         Set_Ref_Class_Type (CP_Id, Class_Type (Class));
         Add_Pool_Ref (Class, CP_Id);
      end if;

      return CP_Id;
   end Class_Item;

   -----------------
   -- Double_Item --
   -----------------

   function Double_Item (Parent : Class_Id; Value : Ureal) return Pool_Id is
      CP_Id : constant Pool_Id := New_Pool_Item (Parent, CONSTANT_Double);

   begin
      Ptable (CP_Id).D_Value := Value;
      return CP_Id;
   end Double_Item;

   ----------------
   -- Field_Item --
   ----------------

   function Field_Item (Parent : Class_Id; Field : Field_Id) return Pool_Id is
      CP_Id : Pool_Id := CP_Item_Id (Parent, First_Pool_Ref (Field));

   begin
      if CP_Id = Null_Pool_Item then
         CP_Id := New_Pool_Item (Parent, CONSTANT_Fieldref);
         Set_Ref_Field (CP_Id, Field);
         Add_Pool_Ref (Field, CP_Id);
      end if;

      return CP_Id;
   end Field_Item;

   ----------------
   -- Float_Item --
   ----------------

   function Float_Item (Parent : Class_Id; Value : Ureal) return Pool_Id is
      CP_Id : constant Pool_Id := New_Pool_Item (Parent, CONSTANT_Float);

   begin
      Ptable (CP_Id).F_Value := Value;
      return CP_Id;
   end Float_Item;

   ------------------
   -- Integer_Item --
   ------------------

   function Integer_Item (Parent : Class_Id; Value : Uint) return Pool_Id is
      CP_Id : constant Pool_Id := New_Pool_Item (Parent, CONSTANT_Integer);

   begin
      Ptable (CP_Id).I_Value := Value;
      return CP_Id;
   end Integer_Item;

   -------------------
   -- Is_Array_Type --
   -------------------

   function Is_Array_Type (CP_Item : Pool_Id) return Boolean is
      Class_Type : constant Type_Id := Ptable (CP_Item).C_Type;

   begin
      return (Type_Kind (Class_Type) = Array_Kind);
   end Is_Array_Type;

   ---------------
   -- Long_Item --
   ---------------

   function Long_Item (Parent : Class_Id; Value : Uint) return Pool_Id is
      CP_Id : constant Pool_Id := New_Pool_Item (Parent, CONSTANT_Long);

   begin
      Ptable (CP_Id).L_Value := Value;
      return CP_Id;
   end Long_Item;

   -----------------
   -- Method_Item --
   -----------------

   function Method_Item (Parent : Class_Id; Method : Method_Id) return Pool_Id
   is
      CP_Id : Pool_Id := CP_Item_Id (Parent, First_Pool_Ref (Method));

   begin
      if CP_Id = Null_Pool_Item then
         if Is_Interface (Class_Of (Method)) then
            CP_Id := New_Pool_Item (Parent, CONSTANT_Interface_Methodref);
         else
            CP_Id := New_Pool_Item (Parent, CONSTANT_Methodref);
         end if;

         Set_Ref_Method (CP_Id, Method);
         Add_Pool_Ref (Method, CP_Id);
      end if;

      return CP_Id;
   end Method_Item;

   --------------------
   -- Next_Pool_Item --
   --------------------

   function Next_Pool_Item (CP_Item : Pool_Id) return Pool_Id is
   begin
      return Ptable (CP_Item).Next_Item;
   end Next_Pool_Item;

   -------------------
   -- Next_Pool_Ref --
   -------------------

   function Next_Pool_Ref (CP_Item : Pool_Id) return Pool_Id is
   begin
      return Ptable (CP_Item).Ref_Link;
   end Next_Pool_Ref;

   ------------------
   -- Parent_Class --
   ------------------

   function Parent_Class (CP_Item : Pool_Id) return Class_Id is
   begin
      return Ptable (CP_Item).Parent;
   end Parent_Class;

   -----------------
   -- Pool_Double --
   -----------------

   function Pool_Double (CP_Item : Pool_Id) return Ureal is
   begin
      return Ptable (CP_Item).D_Value;
   end Pool_Double;

   ----------------
   -- Pool_Float --
   ----------------

   function Pool_Float (CP_Item : Pool_Id) return Ureal is
   begin
      return Ptable (CP_Item).F_Value;
   end Pool_Float;

   ----------------
   -- Pool_Index --
   ----------------

   function Pool_Index (CP_Item : Pool_Id) return CP_Index is
   begin
      return Ptable (CP_Item).Pool_Index;
   end Pool_Index;

   ------------------
   -- Pool_Integer --
   ------------------

   function Pool_Integer (CP_Item : Pool_Id) return Uint is
   begin
      return Ptable (CP_Item).I_Value;
   end Pool_Integer;

   -------------------
   -- Pool_Item_Tag --
   -------------------

   function Pool_Item_Tag (CP_Item : Pool_Id) return CP_Tag is
   begin
      return Ptable (CP_Item).Kind;
   end Pool_Item_Tag;

   ---------------
   -- Pool_Long --
   ---------------

   function Pool_Long (CP_Item : Pool_Id) return Uint is
   begin
      return Ptable (CP_Item).L_Value;
   end Pool_Long;

   -----------------
   -- Pool_String --
   -----------------

   function Pool_String (CP_Item : Pool_Id) return String_Id is
   begin
      return Ptable (CP_Item).S_Value;
   end Pool_String;

   ---------------
   -- Ref_Class --
   ---------------

   function Ref_Class (CP_Item : Pool_Id) return Class_Id is
      Class_Type : constant Type_Id := Ptable (CP_Item).C_Type;
   begin
      pragma Assert (Type_Kind (Class_Type) /= Array_Kind);

      return Class (Class_Type);
   end Ref_Class;

   --------------------
   -- Ref_Class_Type --
   --------------------

   function Ref_Class_Type (CP_Item : Pool_Id) return Type_Id is
   begin
      return Ptable (CP_Item).C_Type;
   end Ref_Class_Type;

   ---------------
   -- Ref_Field --
   ---------------

   function Ref_Field (CP_Item : Pool_Id) return Field_Id is
   begin
      return Ptable (CP_Item).Field;
   end Ref_Field;

   ----------------
   -- Ref_Method --
   ----------------

   function Ref_Method (CP_Item : Pool_Id) return Method_Id is
   begin
      return Ptable (CP_Item).Method;
   end Ref_Method;

   --------------
   -- Ref_Type --
   --------------

   function Ref_Type (CP_Item : Pool_Id) return Type_Id is
   begin
      return Ptable (CP_Item).JVM_Type;
   end Ref_Type;

   ----------------------
   -- Set_Next_CP_Item --
   ----------------------

   procedure Set_Next_CP_Item (CP_Item : Pool_Id; Next_Item : Pool_Id) is
   begin
      Ptable (CP_Item).Next_Item := Next_Item;
   end Set_Next_CP_Item;

   -----------------------
   -- Set_Next_Pool_Ref --
   -----------------------

   procedure Set_Next_Pool_Ref (CP_Item : Pool_Id; Next_Ref : Pool_Id) is
   begin
      Ptable (CP_Item).Ref_Link := Next_Ref;
   end Set_Next_Pool_Ref;

   ----------------------
   -- Set_Parent_Class --
   ----------------------

   procedure Set_Parent_Class (CP_Item : Pool_Id; Class : Class_Id) is
   begin
      Ptable (CP_Item).Parent := Class;
   end Set_Parent_Class;

   --------------------
   -- Set_Pool_Index --
   --------------------

   procedure Set_Pool_Index (CP_Item : Pool_Id; Index : CP_Index) is
   begin
      Ptable (CP_Item).Pool_Index := Index;
   end Set_Pool_Index;

   ------------------------
   -- Set_Ref_Class_Type --
   ------------------------

   procedure Set_Ref_Class_Type (CP_Item : Pool_Id; JVM_Type : Type_Id) is
   begin
      Ptable (CP_Item).C_Type := JVM_Type;
   end Set_Ref_Class_Type;

   -------------------
   -- Set_Ref_Field --
   -------------------

   procedure Set_Ref_Field (CP_Item : Pool_Id; Field : Field_Id) is
   begin
      Ptable (CP_Item).Field := Field;
   end Set_Ref_Field;

   --------------------
   -- Set_Ref_Method --
   --------------------

   procedure Set_Ref_Method (CP_Item : Pool_Id; Method : Method_Id) is
   begin
      Ptable (CP_Item).Method := Method;
   end Set_Ref_Method;

   ------------------
   -- Set_Ref_Type --
   ------------------

   procedure Set_Ref_Type (CP_Item : Pool_Id; JVM_Type : Type_Id) is
   begin
      Ptable (CP_Item).JVM_Type := JVM_Type;
   end Set_Ref_Type;

   -----------------
   -- String_Item --
   -----------------

   function String_Item (Parent : Class_Id; Str : String_Id) return Pool_Id is
      CP_Id : constant Pool_Id := New_Pool_Item (Parent, CONSTANT_String);

   begin
      Ptable (CP_Id).S_Value := Str;
      return CP_Id;
   end String_Item;

   ---------------
   -- Type_Item --
   ---------------

   function Type_Item (Parent : Class_Id; Typ : Type_Id) return Pool_Id is
      CP_Id : Pool_Id;

   begin
      CP_Id :=
        CP_Item_Id (Parent, First_Pool_Ref (Typ), CONSTANT_Name_And_Type);

      if CP_Id = Null_Pool_Item then
         CP_Id := New_Pool_Item (Parent, CONSTANT_Name_And_Type);
         Set_Ref_Type (CP_Id, Typ);
         Add_Pool_Ref (Typ, CP_Id);
      end if;

      return CP_Id;
   end Type_Item;

begin
   Pool_Table.Set_Last (Next_Pool_Id);
end JVM.Pool;
