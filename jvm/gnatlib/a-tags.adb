------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                             A D A . T A G S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1998-2011, AdaCore                     --
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

--  This is the JGNAT specific version of Ada.Tags body

--  An Ada Tag is mapped onto an instance of class java.lang.Class which is
--  the exact equivalent of a Tag object in the Java world. As a matter of
--  fact java.lang.Class contains the equivalent of routines External_Tag
--  (method getName) and Internal_Tag (method forName).

with System.WCh_Con; use System.WCh_Con;
with System.WCh_StW; use System.WCh_StW;

package body Ada.Tags is

   ------------------------
   -- HTable_Subprograms --
   ------------------------

   --  Bodies of routines for hash table instantiation

   package body HTable_Subprograms is

      -----------
      -- Equal --
      -----------

      function Equal (A, B : Tag) return Boolean is
      begin
         return A = B;
      end Equal;

      --------------
      -- Get_Next --
      --------------

      function Get_Next
        (T : Type_Specific_Data_Ptr) return Type_Specific_Data_Ptr is
      begin
         return T.HT_Link;
      end Get_Next;

      ----------
      -- Hash --
      ----------

      function Hash (F : Tag) return HTable_Headers is
         L : constant Natural := Natural (HTable_Headers'Last);
         S : constant String  := External_Tag (F);
         R : Natural := 0;
      begin
         for J in S'Range loop
            R := (R + Character'Pos (S (J))) mod L;
         end loop;

         return HTable_Headers (R + 1);
      end Hash;

      --------------
      -- Set_Next --
      --------------

      procedure Set_Next
        (T    : Type_Specific_Data_Ptr;
         Next : Type_Specific_Data_Ptr) is
      begin
         T.HT_Link := Next;
      end Set_Next;

   end HTable_Subprograms;

   type String_Access is access all String;

   --------------------------------
   -- Check_Interface_Conversion --
   --------------------------------

   procedure Check_Interface_Conversion (Src_Tag : Tag; Trg_Tag : Tag) is
   begin
      if not IW_Membership (Src_Tag, Trg_Tag) then
         raise Constraint_Error with "invalid interface conversion";
      end if;
   end Check_Interface_Conversion;

   ---------------
   -- Check_TSD --
   ---------------

   procedure Check_TSD (TSD : Type_Specific_Data_Ptr) is
      T       : Tag;
      TSD_Ptr : Type_Specific_Data_Ptr;

   begin
      --  Verify that the external tag of this new TSD has not been registered
      --  in the runtime hash table.

      T       := TSD.Tags_Table (0);
      TSD_Ptr := External_Tag_HTable.Get (T);

      if TSD_Ptr /= null then
         raise Program_Error with "duplicated external tag";
      end if;
   end Check_TSD;

   -------------------
   -- CW_Membership --
   -------------------

   function CW_Membership (Obj_Tag : Tag; Typ_Tag : Tag) return Boolean is
      Obj_TSD : Type_Specific_Data_Ptr;
      Typ_TSD : Type_Specific_Data_Ptr;
      Pos     : Integer;

   begin
      Obj_TSD := External_Tag_HTable.Get (Obj_Tag);
      Typ_TSD := External_Tag_HTable.Get (Typ_Tag);
      Pos     := Obj_TSD.Idepth - Typ_TSD.Idepth;

      return Pos >= 0 and then Obj_TSD.Tags_Table (Pos) = Typ_Tag;
   end CW_Membership;

   --------------------
   -- Descendant_Tag --
   --------------------

   function Descendant_Tag (External : String; Ancestor : Tag) return Tag is
      Int_Tag : constant Tag := Internal_Tag (External);

   begin
      if not Is_Descendant_At_Same_Level (Int_Tag, Ancestor) then
         raise Tag_Error;
      end if;

      return Int_Tag;
   end Descendant_Tag;

   -------------------
   -- Expanded_Name --
   -------------------

   function Expanded_Name (T : Tag) return String is
      function Ada_Name (T : Tag) return String_Access;
      pragma Import (Java, Ada_Name, "jgnat.adalib.GNAT_libc.ada_name");
   begin
      return Ada_Name (T).all;
   end Expanded_Name;

   ------------------
   -- External_Tag --
   ------------------

   function External_Tag (T : Tag) return String is
      function Ext_Tag (T : Tag) return String_Access;
      pragma Import (Java, Ext_Tag, "jgnat.adalib.GNAT_libc.external_tag");
   begin
      return Ext_Tag (T).all;
   end External_Tag;

   ----------------------
   -- Get_Access_Level --
   ----------------------

   function Get_Access_Level (T : Tag) return Natural is
      TSD : Type_Specific_Data_Ptr;

   begin
      TSD := External_Tag_HTable.Get (T);
      return TSD.Access_Level;
   end Get_Access_Level;

   -------------------
   -- Get_Alignment --
   -------------------

   function Get_Alignment (T : Tag) return Natural is
      TSD : Type_Specific_Data_Ptr;

   begin
      TSD := External_Tag_HTable.Get (T);
      return TSD.Alignment;
   end Get_Alignment;

   ---------------------
   -- Get_Entry_Index --
   ---------------------

   function Get_Entry_Index (T : Tag; Position : Positive) return Positive is
      TSD : Type_Specific_Data_Ptr;

   begin
      TSD := External_Tag_HTable.Get (T);
      return TSD.SSD.SSD_Table (Position).Index;
   end Get_Entry_Index;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (T : Type_Specific_Data_Ptr) return Tag is
   begin
      --  The first element in the Ancestors_Tags table is the Tag of the
      --  type associated with this TSD

      return T.Tags_Table (0);
   end Get_Key;

   ----------------------
   -- Get_Offset_Index --
   ----------------------

   function Get_Offset_Index
     (Obj_Tag   : Tag;
      Iface_Tag : Tag;
      Position  : Positive) return Positive
   is
      Iface_Table : Interface_Data_Ptr;
      TSD         : Type_Specific_Data_Ptr;
      OSD         : Object_Specific_Data_Ptr;

   begin
      TSD := External_Tag_HTable.Get (Obj_Tag);
      Iface_Table := TSD.Interfaces_Table;

      if Iface_Table /= null then
         for J in 1 .. Iface_Table.Nb_Ifaces loop
            if Iface_Table.Ifaces_Table (J).Iface_Tag = Iface_Tag then
               OSD := Iface_Table.Ifaces_Table (J).OSD;
               return OSD.OSD_Table (Position);
            end if;
         end loop;
      end if;

      return Position;
   end Get_Offset_Index;

   ----------------------
   -- Get_Prim_Op_Kind --
   ----------------------

   function Get_Prim_Op_Kind
     (T        : Tag;
      Position : Positive) return Prim_Op_Kind
   is
      TSD : Type_Specific_Data_Ptr;

   begin
      TSD := External_Tag_HTable.Get (T);
      return TSD.SSD.SSD_Table (Position).Kind;
   end Get_Prim_Op_Kind;

   ---------------------
   -- Get_Tagged_Kind --
   ---------------------

   function Get_Tagged_Kind (T : Tag) return Tagged_Kind is
      TSD : Type_Specific_Data_Ptr;
   begin
      TSD := External_Tag_HTable.Get (T);
      return TSD.Tag_Kind;
   end Get_Tagged_Kind;

   -----------------------------
   -- Interface_Ancestor_Tags --
   -----------------------------

   function Interface_Ancestor_Tags (T : Tag) return Tag_Array is
      Iface_Table : Interface_Data_Ptr;
      TSD         : Type_Specific_Data_Ptr;

   begin
      TSD := External_Tag_HTable.Get (T);
      Iface_Table := TSD.Interfaces_Table;

      if Iface_Table = null then
         declare
            Table : Tag_Array (1 .. 0);
         begin
            return Table;
         end;
      else
         declare
            Table : Tag_Array (1 .. Iface_Table.Nb_Ifaces);
         begin
            for J in 1 .. Iface_Table.Nb_Ifaces loop
               Table (J) := Iface_Table.Ifaces_Table (J).Iface_Tag;
            end loop;

            return Table;
         end;
      end if;
   end Interface_Ancestor_Tags;

   ------------------
   -- Internal_Tag --
   ------------------

   function Internal_Tag (External : String) return Tag is
      function Int_Tag (External : String) return Tag;
      pragma Import (Java, Int_Tag, "jgnat.adalib.GNAT_libc.internal_tag");

      T : constant Tag := Int_Tag (External);
   begin
      if T = No_Tag then
         raise Tag_Error;
      end if;

      return T;
   end Internal_Tag;

   ---------------------------------
   -- Is_Descendant_At_Same_Level --
   ---------------------------------

   function Is_Descendant_At_Same_Level
     (Descendant : Tag;
      Ancestor   : Tag) return Boolean
   is
      D_TSD     : constant Type_Specific_Data_Ptr :=
                    External_Tag_HTable.Get (Descendant);
      A_TSD     : constant Type_Specific_Data_Ptr :=
                    External_Tag_HTable.Get (Ancestor);
   begin
      return CW_Membership (Descendant, Ancestor)
        and then D_TSD.Access_Level = A_TSD.Access_Level;
   end Is_Descendant_At_Same_Level;

   -------------------
   -- IW_Membership --
   -------------------

   --  Canonical implementation of Classwide Membership corresponding to:

   --     Obj in Iface'Class

   --  Obj is in Iface'Class if Iface'Tag is found in the table of interfaces
   --  that are contained in the TSD asociated with Obj'Tag.

   function IW_Membership (Obj_Tag : Tag; Typ_Tag : Tag) return Boolean is
      Iface_Table : Interface_Data_Ptr;
      Obj_TSD     : Type_Specific_Data_Ptr;

   begin
      Obj_TSD := External_Tag_HTable.Get (Obj_Tag);
      Iface_Table := Obj_TSD.Interfaces_Table;

      if Iface_Table /= null then
         for Id in 1 .. Iface_Table.Nb_Ifaces loop
            if Iface_Table.Ifaces_Table (Id).Iface_Tag = Typ_Tag then
               return True;
            end if;
         end loop;
      end if;

      --  Look for the tag in the ancestor tags table. This is required for:
      --     Iface_CW in Typ'Class

      for Id in 0 .. Obj_TSD.Idepth loop
         if Obj_TSD.Tags_Table (Id) = Typ_Tag then
            return True;
         end if;
      end loop;

      return False;
   end IW_Membership;

   ----------------
   -- Parent_Tag --
   ----------------

   function Parent_Tag (T : Tag) return Tag is
      TSD : Type_Specific_Data_Ptr;

   begin
      if T = No_Tag then
         raise Tag_Error;
      end if;

      TSD := External_Tag_HTable.Get (T);

      --  The Parent_Tag of a root-level tagged type is defined to be No_Tag.
      --  The first entry in the Ancestors_Tags array will be null for such
      --  a type, but it's better to be explicit about returning No_Tag in
      --  this case.

      if TSD.Idepth = 0 then
         return No_Tag;
      else
         return TSD.Tags_Table (1);
      end if;
   end Parent_Tag;

   ------------------
   -- Register_TSD --
   ------------------

   procedure Register_TSD (TSD : Type_Specific_Data_Ptr) is
   begin
      External_Tag_HTable.Set (TSD);
   end Register_TSD;

   ---------------------
   -- Set_Entry_Index --
   ---------------------

   procedure Set_Entry_Index
     (T        : Tag;
      Position : Positive;
      Value    : Positive)
   is
      TSD : Type_Specific_Data_Ptr;

   begin
      TSD := External_Tag_HTable.Get (T);
      TSD.SSD.SSD_Table (Position).Index := Value;
   end Set_Entry_Index;

   ----------------------
   -- Set_Prim_Op_Kind --
   ----------------------

   procedure Set_Prim_Op_Kind
     (T        : Tag;
      Position : Positive;
      Value    : Prim_Op_Kind)
   is
      TSD : Type_Specific_Data_Ptr;

   begin
      TSD := External_Tag_HTable.Get (T);
      TSD.SSD.SSD_Table (Position).Kind := Value;
   end Set_Prim_Op_Kind;

   ----------------------
   -- Type_Is_Abstract --
   ----------------------

   function Type_Is_Abstract (T : Tag) return Boolean is
      TSD : Type_Specific_Data_Ptr;

   begin
      TSD := External_Tag_HTable.Get (T);
      return TSD.Type_Is_Abstract;
   end Type_Is_Abstract;

   ------------------------
   -- Wide_Expanded_Name --
   ------------------------

   WC_Encoding : Character;
   pragma Import (C, WC_Encoding, "__gl_wc_encoding");
   --  Encoding method for source, as exported by binder

   function Wide_Expanded_Name (T : Tag) return Wide_String is
      S : constant String := Expanded_Name (T);
      W : Wide_String (1 .. S'Length);
      L : Natural;
   begin
      String_To_Wide_String
        (S, W, L, Get_WC_Encoding_Method (WC_Encoding));
      return W (1 .. L);
   end Wide_Expanded_Name;

   -----------------------------
   -- Wide_Wide_Expanded_Name --
   -----------------------------

   function Wide_Wide_Expanded_Name (T : Tag) return Wide_Wide_String is
      S : constant String := Expanded_Name (T);
      W : Wide_Wide_String (1 .. S'Length);
      L : Natural;
   begin
      String_To_Wide_Wide_String
        (S, W, L, Get_WC_Encoding_Method (WC_Encoding));
      return W (1 .. L);
   end Wide_Wide_Expanded_Name;

end Ada.Tags;
