------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J _ T A B L E                               --
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

with GNAT.Table;

package body J_Table is

   ----------------
   -- Local Data --
   ----------------

   package Global is new GNAT.Table
     (Table_Index_Type     => Nat_32,
      Table_Component_Type => Data,
      Table_Low_Bound      => 1,
      Table_Initial        => 128,
      Table_Increment      => 100);
   --  Global table where all the Data Tables will be stored one after the
   --  other. For a table T, T.Index gives the beginning of T in Global.Table.

   Current_Expandable_Table_Index : Nat_32 := No_Index;
   --  Index in Global.Table giving the beginning of the current expandable
   --  table. If its value is No_Index, then no current expandable table exists
   --  at this point in time.

   package Is_Allocated is new GNAT.Table
     (Table_Index_Type     => Allocation_Id,
      Table_Component_Type => Boolean,
      Table_Low_Bound      => 1,
      Table_Initial        => 128,
      Table_Increment      => 100);
   --  Used to keep track of the currently allocated Table objects. See the
   --  comment under field Allocation_Nb in the private part of this spec
   --  for more info.

   type Water_Mark_Info is record
      --  In the context of this implementation, a water mark is the value of
      --  Global.Last and Is_Allocated.Last at the time when routine
      --  Set_Water_Mark was called.

      Global_Last : Nat_32;
      --  Water mark of Global.Table

      Is_Allocated_Last : Allocation_Id;
      --  Water mark of Is_Allocated.Table
   end record;

   package Water_Mark is new GNAT.Table
     (Table_Index_Type     => Nat_32,
      Table_Component_Type => Water_Mark_Info,
      Table_Low_Bound      => 1,
      Table_Initial        => 32,
      Table_Increment      => 100);
   --  Table to keep track of all the water marks for Table deallocation.
   --  Needed by routines Set_Water_Mark/Free_To_Next_Water_Mark.

   ---------
   -- "=" --
   ---------

   function "=" (T1, T2 : Table) return Boolean is
      pragma Unreferenced (T1, T2);

   begin
      pragma Assert (False);
      raise Program_Error;
      return False;
   end "=";

   ---------
   -- Add --
   ---------

   procedure Add (T : in out Table; Val : Data) is
   begin
      pragma Assert (Allocated (T));
      pragma Assert (T.Index = Current_Expandable_Table_Index);

      Global.Increment_Last;
      Global.Table (Global.Last) := Val;

      T.Length := T.Length + 1;
   end Add;

   procedure Add (T : in out Table; A : Data_Array) is
      Start : Nat_32;

   begin
      pragma Assert (Allocated (T));
      pragma Assert (T.Index = Current_Expandable_Table_Index);

      Start := Global.Last + 1;
      Global.Set_Last (Global.Last + A'Length);

      Global.Table (Start .. Global.Last) := Global.Table_Type (A);

      T.Length := T.Length + A'Length;
   end Add;

   ---------------
   -- Allocated --
   ---------------

   function Allocated (T : Table) return Boolean is
   begin
      return T.Allocation_Nb in 1 .. Is_Allocated.Last
        and then Is_Allocated.Table (T.Allocation_Nb);
   end Allocated;

   -------------------------------
   -- Allocate_Expandable_Table --
   -------------------------------

   procedure Allocate_Expandable_Table (T : in out Table) is
   begin
      Allocate_Fixed_Table (T, 0);
      Current_Expandable_Table_Index := T.Index;
   end Allocate_Expandable_Table;

   --------------------------
   -- Allocate_Fixed_Table --
   --------------------------

   procedure Allocate_Fixed_Table (T : in out Table; L : Nat_32) is
   begin
      pragma Assert (not Allocated (T));
      pragma Assert (not Building_Expandable_Table);

      Is_Allocated.Increment_Last;
      Is_Allocated.Table (Is_Allocated.Last) := True;

      T := (Index         => Global.Last + 1,
            Length        => L,
            Allocation_Nb => Is_Allocated.Last);

      Global.Set_Last (Global.Last + L);

      --  Now perform the default initialization if requested

      if Default_Init then
         declare
            Init_Val : Data;
            pragma Warnings (Off, Init_Val);
         begin
            for K in T.Index .. T.Index + L - 1 loop
               Global.Table (K) := Init_Val;
            end loop;
         end;
      end if;
   end Allocate_Fixed_Table;

   -------------------------------
   -- Building_Expandable_Table --
   -------------------------------

   function Building_Expandable_Table return Boolean is
   begin
      return Current_Expandable_Table_Index /= No_Index;
   end Building_Expandable_Table;

   -----------------------------
   -- Freeze_Expandable_Table --
   -----------------------------

   procedure Freeze_Expandable_Table (T : Table) is
   begin
      pragma Assert (Allocated (T));
      pragma Assert (T.Index = Current_Expandable_Table_Index);

      Current_Expandable_Table_Index := No_Index;
   end Freeze_Expandable_Table;

   -----------------------------
   -- Free_To_Next_Water_Mark --
   -----------------------------

   procedure Free_To_Next_Water_Mark is
      Previous : Water_Mark_Info;

   begin
      pragma Assert (not Building_Expandable_Table);
      pragma Assert (Water_Mark.Last >= Water_Mark.First);

      Previous := Water_Mark.Table (Water_Mark.Last);
      Water_Mark.Decrement_Last;

      Global.Set_Last (Previous.Global_Last);

      for J in Previous.Is_Allocated_Last + 1 .. Is_Allocated.Last loop
         Is_Allocated.Table (J) := False;
      end loop;
   end Free_To_Next_Water_Mark;

   ---------
   -- Get --
   ---------

   function Get (T : Table; K : Index) return Data is
   begin
      return Get (T, Index'Pos (K));
   end Get;

   function Get (T : Table; K : Nat_32) return Data is
   begin
      pragma Assert (Allocated (T));

      if K >= T.Length then
         raise Constraint_Error;
      else
         return Global.Table (T.Index + K);
      end if;
   end Get;

   function Get (T : Table) return Data_Array is
   begin
      pragma Assert (Allocated (T));
      return Data_Array (Global.Table (T.Index .. T.Index + T.Length - 1));
   end Get;

   ----------
   -- Last --
   ----------

   function Last (T : Table) return Int_32 is
      L : constant Nat_32 := Length (T);
   begin
      return L - 1;
   end Last;

   ------------
   -- Length --
   ------------

   function Length (T : Table) return Nat_32 is
   begin
      if not Allocated (T) then
         return 0;
      else
         return T.Length;
      end if;
   end Length;

   -------------
   -- Process --
   -------------

   procedure Process (T : Table) is
   begin
      pragma Assert (Allocated (T));
      Visit (Data_Array (Global.Table (T.Index .. T.Index + T.Length - 1)));
   end Process;

   ---------
   -- Put --
   ---------

   procedure Put (T : Table; K : Index; Val : Data) is
   begin
      Put (T, Index'Pos (K), Val);
   end Put;

   procedure Put (T : Table; K : Nat_32; Val : Data) is
   begin
      pragma Assert (Allocated (T));

      if K >= T.Length then
         raise Constraint_Error;
      else
         Global.Table (T.Index + K) := Val;
      end if;
   end Put;

   procedure Put (T : Table; A : Data_Array) is
   begin
      pragma Assert (Allocated (T));

      if A'Length /= T.Length then
         raise Constraint_Error;
      else
         Global.Table (T.Index .. T.Index + T.Length - 1)
           := Global.Table_Type (A);
      end if;
   end Put;

   --------------------
   -- Set_Water_Mark --
   --------------------

   procedure Set_Water_Mark is
   begin
      pragma Assert (not Building_Expandable_Table);

      Water_Mark.Increment_Last;
      Water_Mark.Table (Water_Mark.Last) := (Global.Last, Is_Allocated.Last);
   end Set_Water_Mark;

end J_Table;
