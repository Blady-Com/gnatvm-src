------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M . I N F O                              --
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

with JVM.Dbg;    use JVM.Dbg;
with JVM.Pool;   use JVM.Pool;
with GNAT.Table;
with Stringt;    use Stringt;
with Targparm;   use Targparm;

package body JVM.Info is

   type Entity_List is record
      First : JVM_Id := Null_JVM_Id;
      Last  : JVM_Id := Null_JVM_Id;
   end record;

   Empty_List : constant Entity_List :=
                  (First => Null_JVM_Id, Last => Null_JVM_Id);

   --  The properties of JVM type entities are encapsulated in type
   --  JVM_Type_Info

   type JVM_Type_Info (Kind : JVM_Type_Kind := Void_Kind) is record
      case Kind is
         when Void_Kind =>
            null;

         when Boolean_Kind | Byte_Kind | Char_Kind  | Short_Kind |
              Int_Kind     | Long_Kind | Float_Kind | Double_Kind =>
            null;

         when Array_Kind =>
            Element_Type : Type_Id := Null_Type;
            Dimensions : Pos_8     := 1;

         when Class_Kind =>
            Class : Class_Id := Null_Class;

         when Return_Addr_Kind =>
            null;
      end case;
   end record;

   --  JVM_Entity records contain information about each class, field, method,
   --  type, variable, label, and subroutine created by JVM.

   type JVM_Entity (Kind : JVM_Entity_Kind := No_Entity) is record
      Name : Name_Id := No_Name;

      case Kind is
         when No_Entity =>
            null;

         when Class_Entity =>
            Next_Class  : Class_Id  := Null_Class;
            Pkg_Name    : String_Id := No_String;
            Src_Name    : Name_Id   := No_Name;

            Class_CP    : Pool_Id  := Null_Pool_Item;
            --  The head of a list of pool items referencing this class

            Superclass  : Class_Id := Null_Class;
            Outer_Class : Class_Id := Null_Class;
            Class_Type  : Type_Id  := Null_Type;
            --  Each class has an associated Class_Type

            Interfaces  : Entity_List;
            --  The list of interfaces that the class implements

            Fields      : Entity_List;
            Classes     : Entity_List;
            Methods     : Entity_List;
            CP_Items    : Pool_Id;
            --  The list of constant pool items associated with this class

            Is_Interface : Boolean := False;
            Is_Public    : Boolean;
            Is_Abstract  : Boolean;
            Is_Final     : Boolean;
            Class_Open   : Boolean;
            Class_Built  : Boolean := False;
            Delay_Class_File : Boolean := False;

         when Field_Entity =>
            Next_Field  : Field_Id := Null_Field;
            Field_Class : Class_Id := Null_Class;
            --  The class to which this field belongs
            Field_Type  : Type_Id  := Null_Type;
            Field_CP    : Pool_Id  := Null_Pool_Item;
            --  The head of a list of pool items referencing this field

            F_Is_Static   : Boolean;
            F_Is_Final    : Boolean;
            F_Is_Volatile : Boolean;
            F_Access_Mode : Member_Access;

         when Method_Entity =>
            Next_Method   : Method_Id       := Null_Method;
            Method_Class  : Class_Id        := Null_Class;
            --  The class to which this method belongs
            Result_Type   : Type_Id         := Null_Type;
            Parent_Method : Method_Id       := Null_Method;
            Method_CP     : Pool_Id         := Null_Pool_Item;
            --  The head of a list of pool items referencing this method

            Local_Vars   : Entity_List;
            Next_Index   : Local_Variable_Index := Local_Variable_Index'First;
            --  The next available index for local variables of this method

            M_Exported_Stdcall         : String_Id := No_String;
            Is_Interface_Wrapper       : Boolean := False;
            Class_Of_Wrapped_Interface : Class_Id := Null_Class;

            M_Has_AR_SL     : Boolean;
            M_Is_Abstract   : Boolean;
            M_Is_Delegate   : Boolean;
            M_Is_AR         : Boolean;
            M_Is_Static     : Boolean;
            M_Is_Final      : Boolean;
            Is_Synchronized : Boolean;
            M_Access_Mode   : Member_Access;
            Method_Open     : Boolean;

            Method_Code     : Code_Sequence;
            Method_Handlers : Handler_Sequence;
            Method_Stack    : Op_Stack_Id;
            Max_Stk_Depth   : Depth_Range := 0;
            Stack_Checking  : Boolean := True;
            Labels          : Entity_List;
            Subroutines     : Entity_List;
            Active_Subr     : Subroutine_Id := Null_Subroutine;
            Exception_Block : Boolean := False;  --  inside handled code?
            --  The currently active subroutine associated with this method

         when Type_Entity =>
            Type_Info : JVM_Type_Info;
            Type_CP   : Pool_Id := Null_Pool_Item;
            --  The head of a list of pool items referencing this type
            Is_Array_Descriptor : Boolean := False;
            Is_Descriptor       : Boolean := False;
            Descriptor_Type     : Type_Id := Null_Type;

         when Local_Var_Entity =>
            Next_Local    : Local_Var_Id := Null_Local_Var;
            Local_Index   : Local_Variable_Index;
            Local_Method  : Method_Id    := Null_Method;
            --  The method to which this local variable belongs
            Variable_Type : Type_Id      := Null_Type;
            Is_Param      : Boolean      := False;

         when Label_Entity =>
            Next_Label   : Label_Id  := Null_Label;
            Label_Method : Method_Id := Null_Method;
            --  The method to which this label belongs
            Location     : Instr_Id  := Null_Instr;
            --  The Nop instruction associated with this label
            Is_Targeted  : Boolean   := False;
            --  Indicates whether any branch instructions target this label
            Label_Number : Natural   := 0;
            --  A unique number (per method) associated with this label
            Code_Index   : Instruction_Index;
            --  The resolved byte code offset for this label within the
            --  code for the label's associated method

         when Subroutine_Entity =>
            Next_Subr    : Subroutine_Id := Null_Subroutine;
            Subr_Method  : Method_Id     := Null_Method;
            --  The method to which this subroutine belongs
            Subr_Label   : Label_Id;
            --  The label that marks the entry point of the subroutine
            Subr_Code    : Code_Sequence;
            Subr_Stack   : Op_Stack_Id;
            Subr_Max_Stk : Depth_Range := 0;
            Subr_Open    : Boolean;

         when Entity_Ref =>
            Referenced_Entity : JVM_Id;
            Next_Entity_Ref   : JVM_Entity_Ref;
      end case;
   end record;

   --  Default-initialized entities

   --  (Currently unused) New_No_Entity         : JVM_Entity (No_Entity);

   pragma Warnings (Off);
   New_Class_Entity      : JVM_Entity (Class_Entity);
   New_Field_Entity      : JVM_Entity (Field_Entity);
   New_Method_Entity     : JVM_Entity (Method_Entity);
   New_Type_Entity       : JVM_Entity (Type_Entity);
   New_Local_Var_Entity  : JVM_Entity (Local_Var_Entity);
   New_Label_Entity      : JVM_Entity (Label_Entity);
   New_Subroutine_Entity : JVM_Entity (Subroutine_Entity);
   New_Entity_Reference  : JVM_Entity (Entity_Ref);
   pragma Warnings (On);

   --  The table of all JVM entities

   package JVM_Table is new GNAT.Table (
     Table_Component_Type => JVM_Entity,
     Table_Index_Type     => JVM_Id'Base,
     Table_Low_Bound      => JVM_Id'First,
     Table_Initial        => 5000,
     Table_Increment      => 100);

   Next_Entity_Index : JVM_Id := First_JVM_Id;
   --  The table index of the next available JVM entity slot

   type JVM_Entity_Access is access all JVM_Entity;

   Label_Counter : Natural := 0;
   --  Each generated label will get a unique number.

   ------------
   -- Jtable --
   ------------

   --  Convenience functions for accessing and updating JVM_Table
   --  entities; these functions raise an exception if their
   --  parameter is out of the allocated range of entities or
   --  if the designated entity doesn't match the expected type.
   --  Note that these functions return access to the actual
   --  table elements, and thus they must only be used in
   --  restricted contexts where JVM_Table cannot be extended.

   function Jtable (Id : JVM_Id) return JVM_Entity_Access;

   function Jtable (Class : Class_Id) return JVM_Entity_Access;

   function Jtable (Field : Field_Id) return JVM_Entity_Access;

   function Jtable (Method : Method_Id) return JVM_Entity_Access;

   function Jtable (Typ : Type_Id) return JVM_Entity_Access;

   function Jtable (Local : Local_Var_Id) return JVM_Entity_Access;

   function Jtable (Label : Label_Id) return JVM_Entity_Access;

   function Jtable (Subr : Subroutine_Id) return JVM_Entity_Access;

   function Jtable (E_Ref : JVM_Entity_Ref) return JVM_Entity_Access;

   pragma Inline (Jtable);

   function Jtable (Id : JVM_Id) return JVM_Entity_Access is
   begin
      pragma Assert (Id in First_JVM_Id .. JVM_Table.Last);
      return JVM_Table.Table (Id)'Unrestricted_Access;
   end Jtable;

   function Jtable (Class : Class_Id) return JVM_Entity_Access is
   begin
      pragma Assert (JVM_Id (Class) in First_JVM_Id .. JVM_Table.Last);
      pragma Assert
        (JVM_Table.Table (JVM_Id (Class)).Kind = Class_Entity);
      return JVM_Table.Table (JVM_Id (Class))'Unrestricted_Access;
   end Jtable;

   function Jtable (Field : Field_Id) return JVM_Entity_Access is
   begin
      pragma Assert (JVM_Id (Field) in First_JVM_Id .. JVM_Table.Last);
      pragma Assert (JVM_Table.Table (JVM_Id (Field)).Kind = Field_Entity);
      return JVM_Table.Table (JVM_Id (Field))'Unrestricted_Access;
   end Jtable;

   function Jtable (Method : Method_Id) return JVM_Entity_Access is
   begin
      pragma Assert (JVM_Id (Method) in First_JVM_Id .. JVM_Table.Last);
      pragma Assert (JVM_Table.Table (JVM_Id (Method)).Kind = Method_Entity);
      return JVM_Table.Table (JVM_Id (Method))'Unrestricted_Access;
   end Jtable;

   function Jtable (Typ : Type_Id) return JVM_Entity_Access is
   begin
      pragma Assert (JVM_Id (Typ) in First_JVM_Id .. JVM_Table.Last);
      pragma Assert (JVM_Table.Table (JVM_Id (Typ)).Kind = Type_Entity);
      return JVM_Table.Table (JVM_Id (Typ))'Unrestricted_Access;
   end Jtable;

   function Jtable (Local : Local_Var_Id) return JVM_Entity_Access is
   begin
      pragma Assert (JVM_Id (Local) in First_JVM_Id .. JVM_Table.Last);
      pragma Assert (JVM_Table.Table (JVM_Id (Local)).Kind = Local_Var_Entity);
      return JVM_Table.Table (JVM_Id (Local))'Unrestricted_Access;
   end Jtable;

   function Jtable (Label : Label_Id) return JVM_Entity_Access is
   begin
      pragma Assert (JVM_Id (Label) in First_JVM_Id .. JVM_Table.Last);
      pragma Assert (JVM_Table.Table (JVM_Id (Label)).Kind = Label_Entity);
      return JVM_Table.Table (JVM_Id (Label))'Unrestricted_Access;
   end Jtable;

   function Jtable (Subr : Subroutine_Id) return JVM_Entity_Access is
   begin
      pragma Assert (JVM_Id (Subr) in First_JVM_Id .. JVM_Table.Last);
      pragma Assert (JVM_Table.Table (JVM_Id (Subr)).Kind = Subroutine_Entity);
      return JVM_Table.Table (JVM_Id (Subr))'Unrestricted_Access;
   end Jtable;

   function Jtable (E_Ref : JVM_Entity_Ref) return JVM_Entity_Access is
   begin
      pragma Assert (JVM_Id (E_Ref) in First_JVM_Id .. JVM_Table.Last);
      pragma Assert (JVM_Table.Table (JVM_Id (E_Ref)).Kind = Entity_Ref);
      return JVM_Table.Table (JVM_Id (E_Ref))'Unrestricted_Access;
   end Jtable;

   -------------------------------------
   -- Java Entity Creation Operations --
   -------------------------------------

   function New_JVM_Entity return JVM_Id;
   --  Returns the JVM identifier of a new JVM entity. Used to factorize
   --  code of New_Class, New_Interface, New_Field, New_Method, New_Type
   --  and thus provide a single point of breakpoint in the debugger.

   function New_JVM_Entity return JVM_Id is
   begin
      Next_Entity_Index := Next_Entity_Index + 1;
      JVM_Table.Set_Last (Next_Entity_Index);
      return Next_Entity_Index;
   end New_JVM_Entity;

   ---------------
   -- New_Class --
   ---------------

   function New_Class return Class_Id is
      E : constant JVM_Id := New_JVM_Entity;
   begin
      JVM_Table.Table (E) := New_Class_Entity;
      return Class_Id (E);
   end New_Class;

   -------------------
   -- New_Interface --
   -------------------

   function New_Interface return Class_Id is
      E : constant JVM_Id := New_JVM_Entity;
   begin
      JVM_Table.Table (E) := New_Class_Entity;
      JVM_Table.Table (E).Is_Interface := True;
      return Class_Id (E);
   end New_Interface;

   ---------------
   -- New_Field --
   ---------------

   function New_Field return Field_Id is
      E : constant JVM_Id := New_JVM_Entity;
   begin
      JVM_Table.Table (E) := New_Field_Entity;
      return Field_Id (E);
   end New_Field;

   ----------------
   -- New_Method --
   ----------------

   function New_Method return Method_Id is
      E : constant JVM_Id := New_JVM_Entity;
   begin
      JVM_Table.Table (E) := New_Method_Entity;
      return Method_Id (E);
   end New_Method;

   --------------
   -- New_Type --
   --------------

   function New_Type (Kind : JVM_Type_Kind) return Type_Id is
      E         : constant JVM_Id := New_JVM_Entity;
      Init_Type : JVM_Type_Info (Kind);
   begin
      JVM_Table.Table (E) := New_Type_Entity;
      Jtable (E).Type_Info := Init_Type;
      return Type_Id (E);
   end New_Type;

   -------------------
   -- New_Local_Var --
   -------------------

   function New_Local_Var return Local_Var_Id is
      E : constant JVM_Id := New_JVM_Entity;
   begin
      JVM_Table.Table (E) := New_Local_Var_Entity;
      return Local_Var_Id (E);
   end New_Local_Var;

   ---------------
   -- New_Label --
   ---------------

   function New_Label return Label_Id is
      E : constant JVM_Id := New_JVM_Entity;
   begin
      JVM_Table.Table (E) := New_Label_Entity;

      Label_Counter := Label_Counter + 1;
      JVM_Table.Table (E).Label_Number := Label_Counter;

      return Label_Id (E);
   end New_Label;

   --------------------
   -- New_Subroutine --
   --------------------

   function New_Subroutine return Subroutine_Id is
      E : constant JVM_Id := New_JVM_Entity;
   begin
      JVM_Table.Table (E) := New_Subroutine_Entity;
      return Subroutine_Id (E);
   end New_Subroutine;

   --------------------
   -- New_Entity_Ref --
   --------------------

   function New_Entity_Ref return JVM_Entity_Ref is
   begin
      Next_Entity_Index := Next_Entity_Index + 1;
      JVM_Table.Set_Last (Next_Entity_Index);
      JVM_Table.Table (Next_Entity_Index) := New_Entity_Reference;
      return JVM_Entity_Ref (Next_Entity_Index);
   end New_Entity_Ref;

   ------------------------------------
   -- Class and Interface Attributes --
   ------------------------------------

   procedure Add_Field (Class : Class_Id; Field : Field_Id) is
   begin
      if Jtable (Class).Fields = Empty_List then
         Jtable (Class).Fields.First := JVM_Id (Field);
      else
         Jtable (Jtable (Class).Fields.Last).Next_Field := Field;
      end if;
      Jtable (Class).Fields.Last := JVM_Id (Field);
   end Add_Field;

   procedure Add_Interface (Class : Class_Id; Intrface : Class_Id) is
      New_I_Ref    : constant JVM_Entity_Ref := New_Entity_Ref;
      Intrface_Ref : JVM_Entity_Ref;

   begin
      pragma Assert (Jtable (Intrface).Kind = Class_Entity);
      pragma Assert (Jtable (Intrface).Is_Interface);

      --  Check whether the class already implements the interface,
      --  in which case simply exit (duplicates are not allowed by
      --  the class verifier). We compare the interface names and
      --  package names because direct comparison of Class_Ids is
      --  not guaranteed to work (e.g., the API interface for
      --  java.io.Serializable gets its own entity, which will
      --  not compare with a user-imported version of that interface).

      Intrface_Ref := First_Interface_Ref (Class);

      while Intrface_Ref /= Null_Entity_Ref loop
         if Name (Info.Get_Interface (Intrface_Ref)) = Name (Intrface)
           and then
             String_Equal
               (Pkg_Name (Info.Get_Interface (Intrface_Ref)),
                Pkg_Name (Intrface))
         then
            return;
         end if;

         Intrface_Ref := Next_Interface_Ref (Intrface_Ref);
      end loop;

      Jtable (New_I_Ref).Referenced_Entity := JVM_Id (Intrface);
      Jtable (New_I_Ref).Next_Entity_Ref   := Null_Entity_Ref;

      if Jtable (Class).Interfaces = Empty_List then
         Jtable (Class).Interfaces.First := JVM_Id (New_I_Ref);
      else
         Jtable (Jtable (Class).Interfaces.Last).Next_Entity_Ref := New_I_Ref;
      end if;
      Jtable (Class).Interfaces.Last := JVM_Id (New_I_Ref);
   end Add_Interface;

   procedure Add_Method (Class : Class_Id; Method : Method_Id) is
   begin
      if Jtable (Class).Methods = Empty_List then
         Jtable (Class).Methods.First := JVM_Id (Method);
      else
         Jtable (Jtable (Class).Methods.Last).Next_Method := Method;
      end if;
      Jtable (Class).Methods.Last := JVM_Id (Method);
   end Add_Method;

   procedure Add_Nested_Class (Class : Class_Id; Nested_Class : Class_Id) is
   begin
      if Jtable (Class).Classes = Empty_List then
         Jtable (Class).Classes.First := JVM_Id (Nested_Class);
      else
         Jtable (Jtable (Class).Classes.Last).Next_Class := Nested_Class;
      end if;
      Jtable (Class).Classes.Last := JVM_Id (Nested_Class);
   end Add_Nested_Class;

   procedure Add_Pool_Item (Class : Class_Id; CP_Item : Pool_Id) is
   begin
      Set_Next_CP_Item (CP_Item, Jtable (Class).CP_Items);
      Jtable (Class).CP_Items := CP_Item;
   end Add_Pool_Item;

   procedure Add_Pool_Ref (Class : Class_Id; CP_Item : Pool_Id) is
   begin
      Set_Next_Pool_Ref (CP_Item, Jtable (Class).Class_CP);
      Jtable (Class).Class_CP := CP_Item;
   end Add_Pool_Ref;

   function Class_Type (Class : Class_Id) return Type_Id is
   begin
      return Jtable (Class).Class_Type;
   end Class_Type;

   function First_Field (Class : Class_Id) return Field_Id is
   begin
      return Field_Id (Jtable (Class).Fields.First);
   end First_Field;

   function First_Interface_Ref (Class : Class_Id) return JVM_Entity_Ref is
   begin
      return JVM_Entity_Ref (Jtable (Class).Interfaces.First);
   end First_Interface_Ref;

   function First_Method (Class : Class_Id) return Method_Id is
   begin
      return Method_Id (Jtable (Class).Methods.First);
   end First_Method;

   function First_Nested_Class (Class : Class_Id) return Class_Id is
   begin
      return Class_Id (Jtable (Class).Classes.First);
   end First_Nested_Class;

   function First_Pool_Item (Class : Class_Id) return Pool_Id is
   begin
      return Jtable (Class).CP_Items;
   end First_Pool_Item;

   function First_Pool_Ref (Class : Class_Id) return Pool_Id is
   begin
      return Jtable (Class).Class_CP;
   end First_Pool_Ref;

   function Get_Interface (I_Ref : JVM_Entity_Ref) return Class_Id is
   begin
      return Class_Id (Jtable (I_Ref).Referenced_Entity);
   end Get_Interface;

   function Is_Abstract (Class : Class_Id) return Boolean is
   begin
      return Jtable (Class).Is_Abstract;
   end Is_Abstract;

   function Is_Built (Class : Class_Id) return Boolean is
   begin
      return Jtable (Class).Class_Built;
   end Is_Built;

   function Is_Final (Class : Class_Id) return Boolean is
   begin
      return Jtable (Class).Is_Final;
   end Is_Final;

   function Is_Interface (Class : Class_Id) return Boolean is
   begin
      return (Jtable (Class).Is_Interface);
   end Is_Interface;

   function Is_Open (Class : Class_Id) return Boolean is
   begin
      return Jtable (Class).Class_Open;
   end Is_Open;

   function Is_Public (Class : Class_Id) return Boolean is
   begin
      return Jtable (Class).Is_Public;
   end Is_Public;

   function Name (Class : Class_Id) return Name_Id is
   begin
      return Jtable (Class).Name;
   end Name;

   function Next_Interface_Ref (I_Ref : JVM_Entity_Ref) return JVM_Entity_Ref
   is
   begin
      return Jtable (I_Ref).Next_Entity_Ref;
   end Next_Interface_Ref;

   function Outer_Class (Class : Class_Id) return Class_Id is
   begin
      return Jtable (Class).Outer_Class;
   end Outer_Class;

   function Pkg_Name (Class : Class_Id) return String_Id is
   begin
      return Jtable (Class).Pkg_Name;
   end Pkg_Name;

   procedure Set_Class_Type (Class : Class_Id; Class_Type : Type_Id) is
   begin
      Jtable (Class).Class_Type := Class_Type;
   end Set_Class_Type;

   procedure Set_Is_Abstract (Class : Class_Id; Value : Boolean := True) is
   begin
      Jtable (Class).Is_Abstract := Value;
   end Set_Is_Abstract;

   procedure Set_Is_Built (Class : Class_Id; Value : Boolean := True) is
   begin
      Jtable (Class).Class_Built := Value;
   end Set_Is_Built;

   procedure Set_Is_Final (Class : Class_Id; Value : Boolean := True) is
   begin
      Jtable (Class).Is_Final := Value;
   end Set_Is_Final;

   procedure Set_Is_Interface (Class : Class_Id; Value : Boolean := True) is
   begin
      Jtable (Class).Is_Interface := Value;
   end Set_Is_Interface;

   procedure Set_Is_Public (Class : Class_Id; Value : Boolean := True) is
   begin
      Jtable (Class).Is_Public := Value;
   end Set_Is_Public;

   procedure Set_Is_Open (Class : Class_Id; Value : Boolean := True) is
   begin
      Jtable (Class).Class_Open := Value;
   end Set_Is_Open;

   procedure Set_Name (Class : Class_Id; Name : Name_Id) is
   begin
      Jtable (Class).Name := Name;
   end Set_Name;

   procedure Set_Outer_Class (Class : Class_Id; Parent : Class_Id) is
   begin
      Jtable (Class).Outer_Class := Parent;
   end Set_Outer_Class;

   procedure Set_Pkg_Name (Class : Class_Id; Name : String_Id) is
   begin
      Jtable (Class).Pkg_Name := Name;
   end Set_Pkg_Name;

   procedure Set_Src_Name (Class : Class_Id; Name : Name_Id) is
   begin
      Jtable (Class).Src_Name := Name;
   end Set_Src_Name;

   procedure Set_Superclass (Class : Class_Id; Superclass : Class_Id) is
   begin
      Jtable (Class).Superclass := Superclass;
   end Set_Superclass;

   function Src_Name (Class : Class_Id) return Name_Id is
   begin
      return Jtable (Class).Src_Name;
   end Src_Name;

   function Superclass (Class : Class_Id) return Class_Id is
   begin
      return Jtable (Class).Superclass;
   end Superclass;

   -------------------------
   -- JVM Type Attributes --
   -------------------------

   procedure Add_Pool_Ref (Typ : Type_Id; CP_Item : Pool_Id) is
   begin
      Set_Next_Pool_Ref (CP_Item, Jtable (Typ).Type_CP);
      Jtable (Typ).Type_CP := CP_Item;
   end Add_Pool_Ref;

   function Class (Class_Type : Type_Id) return Class_Id is
   begin
      pragma Assert (Type_Kind (Class_Type) = Class_Kind);
      return Jtable (Class_Type).Type_Info.Class;
   end Class;

   function Descriptor_Type (Typ : Type_Id) return Type_Id is
   begin
      pragma Assert (Jtable (Typ).Kind = Type_Entity);
      return Jtable (Typ).Descriptor_Type;
   end Descriptor_Type;

   function Dimensions (Array_Type : Type_Id) return Pos_8 is
   begin
      pragma Assert (Type_Kind (Array_Type) = Array_Kind);
      return Jtable (Array_Type).Type_Info.Dimensions;
   end Dimensions;

   function Element_Type (Array_Type : Type_Id) return Type_Id is
   begin
      pragma Assert (Type_Kind (Array_Type) = Array_Kind);
      return Jtable (Array_Type).Type_Info.Element_Type;
   end Element_Type;

   function First_Pool_Ref (Typ : Type_Id) return Pool_Id is
   begin
      return Jtable (Typ).Type_CP;
   end First_Pool_Ref;

   function Get_Entity_Kind (Id : JVM_Id) return JVM_Entity_Kind is
   begin
      if Id = Null_JVM_Id then
         return No_Entity;
      else
         return Jtable (Id).Kind;
      end if;
   end Get_Entity_Kind;

   function Is_Array_Descriptor (Typ : Type_Id) return Boolean is
   begin
      return Jtable (Typ).Is_Array_Descriptor;
   end Is_Array_Descriptor;

   function Is_Descriptor (Typ : Type_Id) return Boolean is
   begin
      return Jtable (Typ).Is_Descriptor;
   end Is_Descriptor;

   function Name (Typ : Type_Id) return Name_Id is
   begin
      return Jtable (Typ).Name;
   end Name;

   procedure Set_Class (Class_Type : Type_Id; Class : Class_Id) is
   begin
      pragma Assert (Type_Kind (Class_Type) = Class_Kind);
      Jtable (Class_Type).Type_Info.Class := Class;
      Jtable (Class).Class_Type := Class_Type;
   end Set_Class;

   procedure Set_Descriptor_Type
     (Typ : Type_Id; Value : Type_Id) is
   begin
      pragma Assert (Jtable (Typ).Kind = Type_Entity);
      pragma Assert (Jtable (Typ).Descriptor_Type = Null_Type);
      Jtable (Typ).Descriptor_Type := Value;
   end Set_Descriptor_Type;

   procedure Set_Dimensions (Array_Type : Type_Id; Dimensions : Pos_8) is
   begin
      pragma Assert (Type_Kind (Array_Type) = Array_Kind);
      Jtable (Array_Type).Type_Info.Dimensions := Dimensions;
   end Set_Dimensions;

   procedure Set_Element_Type (Array_Type : Type_Id; Elmt_Type : Type_Id) is
   begin
      pragma Assert (Type_Kind (Array_Type) = Array_Kind);
      Jtable (Array_Type).Type_Info.Element_Type := Elmt_Type;
   end Set_Element_Type;

   procedure Set_Is_Array_Descriptor
     (Typ : Type_Id; Value : Boolean := True) is
   begin
      pragma Assert (Type_Kind (Typ) = Class_Kind);
      Jtable (Typ).Is_Array_Descriptor := Value;
      Jtable (Typ).Is_Descriptor := Value;
   end Set_Is_Array_Descriptor;

   procedure Set_Is_Descriptor
     (Typ : Type_Id; Value : Boolean := True) is
   begin
      pragma Assert (Type_Kind (Typ) = Class_Kind);
      Jtable (Typ).Is_Descriptor := Value;
   end Set_Is_Descriptor;

   procedure Set_Name (Typ : Type_Id; Name : Name_Id) is
   begin
      Jtable (Typ).Name := Name;
   end Set_Name;

   function Type_Kind (Typ : Type_Id) return JVM_Type_Kind is
   begin
      return Jtable (Typ).Type_Info.Kind;
   end Type_Kind;

   function Word_Size (Typ : Type_Id) return JVM_Type_Size is
   begin
      case Type_Kind (Typ) is
         when Void_Kind =>

            --  Used by anonymous subprogram access???

            return 1;

            --  pragma Assert (False);
            --  raise Program_Error;

         when Boolean_Kind | Byte_Kind | Char_Kind |
              Short_Kind   | Int_Kind  | Float_Kind =>
            return 1;

         when Long_Kind | Double_Kind =>
            return 2;

         when Array_Kind | Class_Kind | Return_Addr_Kind =>
            return 1;
      end case;
   end Word_Size;

   ----------------------
   -- Field Attributes --
   ----------------------

   function Name (Field : Field_Id) return Name_Id is
   begin
      return Jtable (Field).Name;
   end Name;

   function Next_Field (Field : Field_Id) return Field_Id is
   begin
      return Jtable (Field).Next_Field;
   end Next_Field;

   function Next_Nested_Class (Class : Class_Id) return Class_Id is
   begin
      return Jtable (Class).Next_Class;
   end Next_Nested_Class;

   function Class (Field : Field_Id) return Class_Id is
   begin
      return Jtable (Field).Field_Class;
   end Class;

   function Field_Type (Field : Field_Id) return Type_Id is
   begin
      return Jtable (Field).Field_Type;
   end Field_Type;

   function First_Pool_Ref (Field : Field_Id) return Pool_Id is
   begin
      return Jtable (Field).Field_CP;
   end First_Pool_Ref;

   function Is_Static (Field : Field_Id) return Boolean is
   begin
      return Jtable (Field).F_Is_Static;
   end Is_Static;

   function Is_Final (Field : Field_Id) return Boolean is
   begin
      return Jtable (Field).F_Is_Final;
   end Is_Final;

   function Is_Volatile (Field : Field_Id) return Boolean is
   begin
      return Jtable (Field).F_Is_Volatile;
   end Is_Volatile;

   function Access_Mode (Field : Field_Id) return Member_Access is
   begin
      return Jtable (Field).F_Access_Mode;
   end Access_Mode;

   procedure Set_Name (Field : Field_Id; Name : Name_Id) is
   begin
      Jtable (Field).Name := Name;
   end Set_Name;

   procedure Set_Class (Field : Field_Id; Class : Class_Id) is
   begin
      Jtable (Field).Field_Class := Class;
   end Set_Class;

   procedure Set_Field_Type (Field : Field_Id; Typ : Type_Id) is
   begin
      Jtable (Field).Field_Type := Typ;
   end Set_Field_Type;

   procedure Add_Pool_Ref (Field : Field_Id; CP_Item : Pool_Id) is
   begin
      Set_Next_Pool_Ref (CP_Item, Jtable (Field).Field_CP);
      Jtable (Field).Field_CP := CP_Item;
   end Add_Pool_Ref;

   procedure Set_Is_Static (Field : Field_Id; Value : Boolean := True) is
   begin
      Jtable (Field).F_Is_Static := Value;
   end Set_Is_Static;

   procedure Set_Is_Final (Field : Field_Id; Value : Boolean := True) is
   begin
      Jtable (Field).F_Is_Final := Value;
   end Set_Is_Final;

   procedure Set_Is_Volatile (Field : Field_Id; Value : Boolean := True) is
   begin
      Jtable (Field).F_Is_Volatile := Value;
   end Set_Is_Volatile;

   procedure Set_Access_Mode (Field : Field_Id; Acc_Mode : Member_Access) is
   begin
      Jtable (Field).F_Access_Mode := Acc_Mode;
   end Set_Access_Mode;

   -----------------------
   -- Method Attributes --
   -----------------------

   function Name (Method : Method_Id) return Name_Id is
   begin
      return Jtable (Method).Name;
   end Name;

   function Next_Method (Method : Method_Id) return Method_Id is
   begin
      return Jtable (Method).Next_Method;
   end Next_Method;

   function Class (Method : Method_Id) return Class_Id is
   begin
      return Jtable (Method).Method_Class;
   end Class;

   function Class_Of_Wrapped_Interface (Method : Method_Id) return Class_Id is
   begin
      pragma Assert (Jtable (Method).Is_Interface_Wrapper);
      return Jtable (Method).Class_Of_Wrapped_Interface;
   end Class_Of_Wrapped_Interface;

   function Result_Type (Method : Method_Id) return Type_Id is
   begin
      return Jtable (Method).Result_Type;
   end Result_Type;

   function Exported_Stdcall (Method : Method_Id) return String_Id is
   begin
      return Jtable (Method).M_Exported_Stdcall;
   end Exported_Stdcall;

   function Parent_Method (Method : Method_Id) return Method_Id is
   begin
      return Jtable (Method).Parent_Method;
   end Parent_Method;

   function First_Local_Var (Method : Method_Id) return Local_Var_Id is
   begin
      return Local_Var_Id (Jtable (Method).Local_Vars.First);
   end First_Local_Var;

   function Next_Local_Index
     (Method : Method_Id)
        return Local_Variable_Index
   is
   begin
      return Jtable (Method).Next_Index;
   end Next_Local_Index;

   function First_Pool_Ref (Method : Method_Id) return Pool_Id is
   begin
      return Jtable (Method).Method_CP;
   end First_Pool_Ref;

   function Has_AR_SL_Formal (Method : Method_Id) return Boolean is
   begin
      return Jtable (Method).M_Has_AR_SL;
   end Has_AR_SL_Formal;

   function Is_Abstract (Method : Method_Id) return Boolean is
   begin
      return Jtable (Method).M_Is_Abstract;
   end Is_Abstract;

   function Is_AR_Method (Method : Method_Id) return Boolean is
   begin
      return Jtable (Method).M_Is_AR;
   end Is_AR_Method;

   function Is_Delegate (Method : Method_Id) return Boolean is
   begin
      return Jtable (Method).M_Is_Delegate;
   end Is_Delegate;

   function Is_Interface_Wrapper (Method : Method_Id) return Boolean is
   begin
      return Jtable (Method).Is_Interface_Wrapper;
   end Is_Interface_Wrapper;

   function Is_Static (Method : Method_Id) return Boolean is
   begin
      return Jtable (Method).M_Is_Static;
   end Is_Static;

   function Is_Final (Method : Method_Id) return Boolean is
   begin
      return Jtable (Method).M_Is_Final;
   end Is_Final;

   function Is_Synchronized (Method : Method_Id) return Boolean is
   begin
      return Jtable (Method).Is_Synchronized;
   end Is_Synchronized;

   function Access_Mode (Method : Method_Id) return Member_Access is
   begin
      return Jtable (Method).M_Access_Mode;
   end Access_Mode;

   function Is_Open (Method : Method_Id) return Boolean is
   begin
      return Jtable (Method).Method_Open;
   end Is_Open;

   function Is_Exception_Block (Method : Method_Id) return Boolean is
   begin
      return Jtable (Method).Exception_Block;
   end Is_Exception_Block;

   function Is_Stack_Checking (Method : Method_Id) return Boolean is
   begin
      return Jtable (Method).Stack_Checking;
   end Is_Stack_Checking;

   function Method_Code (Method : Method_Id) return Code_Sequence is
   begin
      return Jtable (Method).Method_Code;
   end Method_Code;

   function Method_Handlers (Method : Method_Id) return Handler_Sequence is
   begin
      return Jtable (Method).Method_Handlers;
   end Method_Handlers;

   function First_Label (Method : Method_Id) return Label_Id is
   begin
      return Label_Id (Jtable (Method).Labels.First);
   end First_Label;

   function First_Subroutine (Method : Method_Id) return Subroutine_Id is
   begin
      return Subroutine_Id (Jtable (Method).Subroutines.First);
   end First_Subroutine;

   function Active_Subroutine (Method : Method_Id) return Subroutine_Id is
   begin
      return Jtable (Method).Active_Subr;
   end Active_Subroutine;

   function Op_Stack (Method : Method_Id) return Op_Stack_Id is
   begin
      return Jtable (Method).Method_Stack;
   end Op_Stack;

   function Max_Stack_Depth (Method : Method_Id) return Depth_Range is
   begin
      pragma Assert (not Is_Open (Method)
                     or else Is_Delegate (Method)
                     or else Is_Abstract (Method));

      return Jtable (Method).Max_Stk_Depth;
   end Max_Stack_Depth;

   procedure Set_Name (Method : Method_Id; Name : Name_Id) is
   begin
      Jtable (Method).Name := Name;
   end Set_Name;

   procedure Set_Class (Method : Method_Id; Class : Class_Id) is
   begin
      Jtable (Method).Method_Class := Class;
   end Set_Class;

   procedure Set_Result_Type (Method : Method_Id; Typ : Type_Id) is
   begin
      Jtable (Method).Result_Type := Typ;
   end Set_Result_Type;

   procedure Set_Exported_Stdcall (Method : Method_Id; Val : String_Id) is
   begin
      Jtable (Method).M_Exported_Stdcall := Val;
   end Set_Exported_Stdcall;

   procedure Set_Parent_Method (Method : Method_Id; Parent : Method_Id) is
   begin
      Jtable (Method).Parent_Method := Parent;
   end Set_Parent_Method;

   procedure Add_Local_Var (Method : Method_Id; Local : Local_Var_Id) is
   begin
      if Jtable (Method).Local_Vars = Empty_List then
         Jtable (Method).Local_Vars.First := JVM_Id (Local);
      else
         Jtable (Jtable (Method).Local_Vars.Last).Next_Local := Local;
      end if;

      Jtable (Method).Local_Vars.Last := JVM_Id (Local);

      case VM_Target is
         when JVM_Target =>
            Jtable (Method).Next_Index := Jtable (Method).Next_Index +
              Local_Variable_Index (Word_Size (Jtable (Local).Variable_Type));

         when CLI_Target =>
            Jtable (Method).Next_Index := Jtable (Method).Next_Index + 1;

         when No_VM =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Add_Local_Var;

   procedure Add_Pool_Ref (Method : Method_Id; CP_Item : Pool_Id) is
   begin
      Set_Next_Pool_Ref (CP_Item, Jtable (Method).Method_CP);
      Jtable (Method).Method_CP := CP_Item;
   end Add_Pool_Ref;

   procedure Set_Class_Of_Wrapped_Interface
     (Method : Method_Id; Class : Class_Id) is
   begin
      pragma Assert (Jtable (Method).Is_Interface_Wrapper);
      pragma Assert (Is_Interface (Class));

      Jtable (Method).Class_Of_Wrapped_Interface := Class;
   end Set_Class_Of_Wrapped_Interface;

   procedure Set_Has_AR_SL_Formal
     (Method : Method_Id; Value : Boolean := True) is
   begin
      Jtable (Method).M_Has_AR_SL := Value;
   end Set_Has_AR_SL_Formal;

   procedure Set_Is_Abstract (Method : Method_Id; Value : Boolean := True) is
   begin
      Jtable (Method).M_Is_Abstract := Value;
   end Set_Is_Abstract;

   procedure Set_Is_AR_Method (Method : Method_Id; Value : Boolean := True) is
   begin
      Jtable (Method).M_Is_AR := Value;
   end Set_Is_AR_Method;

   procedure Set_Is_Delegate (Method : Method_Id; Value : Boolean := True) is
   begin
      Jtable (Method).M_Is_Delegate := Value;
   end Set_Is_Delegate;

   procedure Set_Is_Interface_Wrapper
     (Method : Method_Id; Value : Boolean := True) is
   begin
      Jtable (Method).Is_Interface_Wrapper := Value;
   end Set_Is_Interface_Wrapper;

   procedure Set_Is_Static (Method : Method_Id; Value : Boolean := True) is
   begin
      Jtable (Method).M_Is_Static := Value;
   end Set_Is_Static;

   procedure Set_Is_Final (Method : Method_Id; Value : Boolean := True) is
   begin
      Jtable (Method).M_Is_Final := Value;
   end Set_Is_Final;

   procedure Set_Is_Synchronized
     (Method : Method_Id; Value : Boolean := True)
   is
   begin
      Jtable (Method).Is_Synchronized := Value;
   end Set_Is_Synchronized;

   procedure Set_Access_Mode (Method : Method_Id; Acc_Mode : Member_Access) is
   begin
      Jtable (Method).M_Access_Mode := Acc_Mode;
   end Set_Access_Mode;

   procedure Set_Is_Open (Method : Method_Id; Value : Boolean := True) is
   begin
      Jtable (Method).Method_Open := Value;
   end Set_Is_Open;

   procedure Set_Exception_Block
     (Method : Method_Id; Value : Boolean := True) is
   begin
      Jtable (Method).Exception_Block := Value;
   end Set_Exception_Block;

   procedure Set_Code (Method : Method_Id; Code : Code_Sequence) is
   begin
      Jtable (Method).Method_Code := Code;
   end Set_Code;

   procedure Set_Handlers (Method : Method_Id; Handlers : Handler_Sequence) is
   begin
      Jtable (Method).Method_Handlers := Handlers;
   end Set_Handlers;

   procedure Set_Op_Stack (Method : Method_Id; Stack : Op_Stack_Id) is
   begin
      if Stack = Null_Op_Stack then
         Free_Stack (Jtable (Method).Method_Stack);
      else
         Jtable (Method).Method_Stack := Stack;
      end if;
   end Set_Op_Stack;

   procedure Set_Max_Stack_Depth (Method : Method_Id; Depth : Depth_Range) is
   begin
      Jtable (Method).Max_Stk_Depth := Depth;
   end Set_Max_Stack_Depth;

   procedure Set_Stack_Checking
     (Method : Method_Id;
      Value  : Boolean := True)
   is
   begin
      Jtable (Method).Stack_Checking := Value;
   end Set_Stack_Checking;

   procedure Add_Label (Method : Method_Id; Label : Label_Id) is
   begin
      if Jtable (Method).Labels = Empty_List then
         Jtable (Method).Labels.First := JVM_Id (Label);
      else
         Jtable (Jtable (Method).Labels.Last).Next_Label := Label;
      end if;
      Jtable (Method).Labels.Last := JVM_Id (Label);
   end Add_Label;

   procedure Add_Subroutine (Method : Method_Id; Subr : Subroutine_Id) is
   begin
      if Jtable (Method).Subroutines = Empty_List then
         Jtable (Method).Subroutines.First := JVM_Id (Subr);
      else
         Jtable (Jtable (Method).Subroutines.Last).Next_Subr := Subr;
      end if;
      Jtable (Method).Subroutines.Last := JVM_Id (Subr);
   end Add_Subroutine;

   procedure Set_Active_Subroutine
     (Method : Method_Id; Subr : Subroutine_Id)
   is
   begin
      Jtable (Method).Active_Subr := Subr;
   end Set_Active_Subroutine;

   -------------------------------
   -- Local Variable Attributes --
   -------------------------------

   function Name (Local : Local_Var_Id) return Name_Id is
   begin
      return Jtable (Local).Name;
   end Name;

   function Next_Local_Var (Local : Local_Var_Id) return Local_Var_Id is
   begin
      return Jtable (Local).Next_Local;
   end Next_Local_Var;

   function Local_Index (Local : Local_Var_Id) return Local_Variable_Index is
   begin
      return Jtable (Local).Local_Index;
   end Local_Index;

   function Method (Local : Local_Var_Id) return Method_Id is
   begin
      return Jtable (Local).Local_Method;
   end Method;

   function Variable_Type (Local : Local_Var_Id) return Type_Id is
   begin
      return Jtable (Local).Variable_Type;
   end Variable_Type;

   function Is_Param (Local : Local_Var_Id) return Boolean is
   begin
      return Jtable (Local).Is_Param;
   end Is_Param;

   procedure Set_Name (Local : Local_Var_Id; Name : Name_Id) is
   begin
      Jtable (Local).Name := Name;
   end Set_Name;

   procedure Set_Local_Index
     (Local : Local_Var_Id;
      Index : Local_Variable_Index)
   is
   begin
      Jtable (Local).Local_Index := Index;
   end Set_Local_Index;

   procedure Set_Method (Local : Local_Var_Id; Method : Method_Id) is
   begin
      Jtable (Local).Local_Method := Method;
   end Set_Method;

   procedure Set_Variable_Type (Local : Local_Var_Id; Typ : Type_Id) is
   begin
      Jtable (Local).Variable_Type := Typ;
   end Set_Variable_Type;

   procedure Set_Is_Param (Local : Local_Var_Id; Value : Boolean := True) is
   begin
      Jtable (Local).Is_Param := Value;
   end Set_Is_Param;

   ----------------------
   -- Label Attributes --
   ----------------------

   function Name (Label : Label_Id) return Name_Id is
   begin
      return Jtable (Label).Name;
   end Name;

   function Next_Label (Label : Label_Id) return Label_Id is
   begin
      return Jtable (Label).Next_Label;
   end Next_Label;

   function Method (Label : Label_Id) return Method_Id is
   begin
      return Jtable (Label).Label_Method;
   end Method;

   function Location (Label : Label_Id) return Instr_Id is
   begin
      return Jtable (Label).Location;
   end Location;

   function Code_Index (Label : Label_Id) return Instruction_Index is
   begin
      return Jtable (Label).Code_Index;
   end Code_Index;

   function Is_Targeted (Label : Label_Id) return Boolean is
   begin
      return Jtable (Label).Is_Targeted;
   end Is_Targeted;

   function Label_Number (Label : Label_Id) return Natural is
   begin
      return Jtable (Label).Label_Number;
   end Label_Number;

   procedure Set_Name (Label : Label_Id; Name : Name_Id) is
   begin
      Jtable (Label).Name := Name;
   end Set_Name;

   procedure Set_Method (Label : Label_Id; Method : Method_Id) is
   begin
      Jtable (Label).Label_Method := Method;
   end Set_Method;

   procedure Set_Location (Label : Label_Id; Instr : Instr_Id) is
   begin
      Jtable (Label).Location := Instr;
   end Set_Location;

   procedure Set_Code_Index (Label : Label_Id; Index : Instruction_Index) is
   begin
      Jtable (Label).Code_Index := Index;
   end Set_Code_Index;

   procedure Set_Is_Targeted (Label : Label_Id; Value : Boolean := True) is
   begin
      Jtable (Label).Is_Targeted := Value;
   end Set_Is_Targeted;

   procedure Set_Label_Number (Label : Label_Id; N : Natural) is
   begin
      Jtable (Label).Label_Number := N;
   end Set_Label_Number;

   ---------------------------
   -- Subroutine Attributes --
   ---------------------------

   function Name (Subr : Subroutine_Id) return Name_Id is
   begin
      return Jtable (Subr).Name;
   end Name;

   function Next_Subroutine (Subr : Subroutine_Id) return Subroutine_Id is
   begin
      return Jtable (Subr).Next_Subr;
   end Next_Subroutine;

   function Method (Subr : Subroutine_Id) return Method_Id is
   begin
      return Jtable (Subr).Subr_Method;
   end Method;

   function Subroutine_Code (Subr : Subroutine_Id) return Code_Sequence is
   begin
      return Jtable (Subr).Subr_Code;
   end Subroutine_Code;

   function Subroutine_Label (Subr : Subroutine_Id) return Label_Id is
   begin
      return Jtable (Subr).Subr_Label;
   end Subroutine_Label;

   function Op_Stack (Subr : Subroutine_Id) return Op_Stack_Id is
   begin
      return Jtable (Subr).Subr_Stack;
   end Op_Stack;

   function Max_Stack_Depth (Subr : Subroutine_Id) return Depth_Range is
   begin
      return Jtable (Subr).Subr_Max_Stk;
   end Max_Stack_Depth;

   function Is_Open (Subr : Subroutine_Id) return Boolean is
   begin
      return Jtable (Subr).Subr_Open;
   end Is_Open;

   procedure Set_Name (Subr : Subroutine_Id; Name : Name_Id) is
   begin
      Jtable (Subr).Name := Name;
   end Set_Name;

   procedure Set_Method (Subr : Subroutine_Id; Method : Method_Id) is
   begin
      Jtable (Subr).Subr_Method := Method;
   end Set_Method;

   procedure Set_Code (Subr : Subroutine_Id; Code : Code_Sequence) is
   begin
      Jtable (Subr).Subr_Code := Code;
   end Set_Code;

   procedure Set_Subroutine_Label (Subr : Subroutine_Id; Label : Label_Id) is
   begin
      Jtable (Subr).Subr_Label := Label;
   end Set_Subroutine_Label;

   procedure Set_Op_Stack (Subr : Subroutine_Id; Stack : Op_Stack_Id) is
   begin
      if Stack = Null_Op_Stack then
         Free_Stack (Jtable (Subr).Subr_Stack);
      else
         Jtable (Subr).Subr_Stack := Stack;
      end if;
   end Set_Op_Stack;

   procedure Set_Max_Stack_Depth (Subr : Subroutine_Id; Depth : Depth_Range) is
   begin
      Jtable (Subr).Subr_Max_Stk := Depth;
   end Set_Max_Stack_Depth;

   procedure Set_Is_Open (Subr : Subroutine_Id; Value : Boolean := True) is
   begin
      Jtable (Subr).Subr_Open := Value;
   end Set_Is_Open;

   ---------------------------------
   -- Entity Reference Attributes --
   ---------------------------------

   function Denoted_Entity (E_Ref : JVM_Entity_Ref) return JVM_Id is
   begin
      return Jtable (E_Ref).Referenced_Entity;
   end Denoted_Entity;

   function Next_Entity_Ref (E_Ref : JVM_Entity_Ref) return JVM_Id is
   begin
      return JVM_Id (Jtable (E_Ref).Next_Entity_Ref);
   end Next_Entity_Ref;

   procedure Set_Denoted_Entity (E_Ref : JVM_Entity_Ref; Entity : JVM_Id) is
   begin
      Jtable (E_Ref).Referenced_Entity := Entity;
   end Set_Denoted_Entity;

   procedure Set_Next_Entity_Ref
     (E_Ref : JVM_Entity_Ref; Next_E_Ref : JVM_Entity_Ref)
   is
   begin
      Jtable (E_Ref).Next_Entity_Ref := Next_E_Ref;
   end Set_Next_Entity_Ref;

   -------------------------
   -- Debugging Utilities --
   -------------------------

   procedure Print_JVM_Entity (Id : JVM_Id) is
   begin
      if Id = Null_JVM_Id then
         Print_Line ("*** Null JVM id passed ***");
      end if;

      case Jtable (Id).Kind is
         when No_Entity =>
            Print_Line ("*** <No_Entity> ***");
         when Class_Entity =>
            Print_Class (Class_Id (Id));
         when Field_Entity =>
            Print_Field (Field_Id (Id));
         when Method_Entity =>
            Print_Method (Method_Id (Id));
         when Type_Entity =>
            Print_Type (Type_Id (Id));
         when Local_Var_Entity =>
            Print_Local_Var (Local_Var_Id (Id));
         when Label_Entity =>
            Print_Line ("Label: <no info>");
         when Subroutine_Entity =>
            Print_Line ("Subroutine: <no info>");
         when Entity_Ref =>
            Print_Line ("Entity_Ref: <no info>");
      end case;
   end Print_JVM_Entity;

   procedure PJ (Id : JVM_Id) is
   begin
      Print_JVM_Entity (Id);
   end PJ;

begin
   JVM_Table.Set_Last (Next_Entity_Index);
end JVM.Info;
