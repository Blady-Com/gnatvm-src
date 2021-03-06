------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--              ADA.CONTAINERS.HASH_TABLES.GENERIC_BOUNDED_KEYS             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2010, Free Software Foundation, Inc.         --
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
-- This unit was originally developed by Matthew J Heaney.                  --
------------------------------------------------------------------------------

package body Ada.Containers.Hash_Tables.Generic_Bounded_Keys is

   --------------------------
   -- Delete_Key_Sans_Free --
   --------------------------

   procedure Delete_Key_Sans_Free
     (HT  : in out Hash_Table_Type'Class;
      Key : Key_Type;
      X   : out Count_Type)
   is
      Indx : Hash_Type;
      Prev : Count_Type;

   begin
      if HT.Length = 0 then
         X := 0;
         return;
      end if;

      Indx := Index (HT, Key);
      X := HT.Buckets (Indx);

      if X = 0 then
         return;
      end if;

      if Equivalent_Keys (Key, HT.Nodes (X)) then
         if HT.Busy > 0 then
            raise Program_Error with
              "attempt to tamper with cursors (container is busy)";
         end if;
         HT.Buckets (Indx) := Next (HT.Nodes (X));
         HT.Length := HT.Length - 1;
         return;
      end if;

      loop
         Prev := X;
         X := Next (HT.Nodes (Prev));

         if X = 0 then
            return;
         end if;

         if Equivalent_Keys (Key, HT.Nodes (X)) then
            if HT.Busy > 0 then
               raise Program_Error with
                 "attempt to tamper with cursors (container is busy)";
            end if;
            Set_Next (HT.Nodes (Prev), Next => Next (HT.Nodes (X)));
            HT.Length := HT.Length - 1;
            return;
         end if;
      end loop;
   end Delete_Key_Sans_Free;

   ----------
   -- Find --
   ----------

   function Find
     (HT  : Hash_Table_Type'Class;
      Key : Key_Type) return Count_Type
   is
      Indx : Hash_Type;
      Node : Count_Type;

   begin
      if HT.Length = 0 then
         return 0;
      end if;

      Indx := Index (HT, Key);

      Node := HT.Buckets (Indx);
      while Node /= 0 loop
         if Equivalent_Keys (Key, HT.Nodes (Node)) then
            return Node;
         end if;
         Node := Next (HT.Nodes (Node));
      end loop;

      return 0;
   end Find;

   --------------------------------
   -- Generic_Conditional_Insert --
   --------------------------------

   procedure Generic_Conditional_Insert
     (HT       : in out Hash_Table_Type'Class;
      Key      : Key_Type;
      Node     : out Count_Type;
      Inserted : out Boolean)
   is
      Indx : constant Hash_Type := Index (HT, Key);
      B    : Count_Type renames HT.Buckets (Indx);

   begin
      if B = 0 then
         if HT.Busy > 0 then
            raise Program_Error with
              "attempt to tamper with cursors (container is busy)";
         end if;

         if HT.Length = HT.Capacity then
            raise Capacity_Error with "no more capacity for insertion";
         end if;

         Node := New_Node;
         Set_Next (HT.Nodes (Node), Next => 0);

         Inserted := True;

         B := Node;
         HT.Length := HT.Length + 1;

         return;
      end if;

      Node := B;
      loop
         if Equivalent_Keys (Key, HT.Nodes (Node)) then
            Inserted := False;
            return;
         end if;

         Node := Next (HT.Nodes (Node));

         exit when Node = 0;
      end loop;

      if HT.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

      if HT.Length = HT.Capacity then
         raise Capacity_Error with "no more capacity for insertion";
      end if;

      Node := New_Node;
      Set_Next (HT.Nodes (Node), Next => B);

      Inserted := True;

      B := Node;
      HT.Length := HT.Length + 1;
   end Generic_Conditional_Insert;

   -----------
   -- Index --
   -----------

   function Index
     (HT  : Hash_Table_Type'Class;
      Key : Key_Type) return Hash_Type is
   begin
      return HT.Buckets'First + Hash (Key) mod HT.Buckets'Length;
   end Index;

   -----------------------------
   -- Generic_Replace_Element --
   -----------------------------

   procedure Generic_Replace_Element
     (HT   : in out Hash_Table_Type'Class;
      Node : Count_Type;
      Key  : Key_Type)
   is
      pragma Assert (HT.Length > 0);
      pragma Assert (Node /= 0);

      BB : Buckets_Type renames HT.Buckets;
      NN : Nodes_Type renames HT.Nodes;

      Old_Hash : constant Hash_Type := Hash (NN (Node));
      Old_Indx : constant Hash_Type := BB'First + Old_Hash mod BB'Length;

      New_Hash : constant Hash_Type := Hash (Key);
      New_Indx : constant Hash_Type := BB'First + New_Hash mod BB'Length;

      New_Bucket : Count_Type renames BB (New_Indx);
      N, M       : Count_Type;

   begin
      --  Replace_Element is allowed to change a node's key to Key
      --  (generic formal operation Assign provides the mechanism), but
      --  only if Key is not already in the hash table. (In a unique-key
      --  hash table as this one, a key is mapped to exactly one node.)

      if Equivalent_Keys (Key, NN (Node)) then
         pragma Assert (New_Hash = Old_Hash);

         if HT.Lock > 0 then
            raise Program_Error with
              "attempt to tamper with elements (container is locked)";
         end if;

         --  The new Key value is mapped to this same Node, so Node
         --  stays in the same bucket.

         Assign (NN (Node), Key);
         pragma Assert (Hash (NN (Node)) = New_Hash);
         pragma Assert (Equivalent_Keys (Key, NN (Node)));
         return;
      end if;

      --  Key is not equivalent to Node, so we now have to determine if it's
      --  equivalent to some other node in the hash table. This is the case
      --  irrespective of whether Key is in the same or a different bucket from
      --  Node.

      N := New_Bucket;
      while N /= 0 loop
         if Equivalent_Keys (Key, NN (N)) then
            pragma Assert (N /= Node);
            raise Program_Error with
              "attempt to replace existing element";
         end if;

         N := Next (NN (N));
      end loop;

      --  We have determined that Key is not already in the hash table, so
      --  the change is tentatively allowed. We now perform the standard
      --  checks to determine whether the hash table is locked (because you
      --  cannot change an element while it's in use by Query_Element or
      --  Update_Element), or if the container is busy (because moving a
      --  node to a different bucket would interfere with iteration).

      if Old_Indx = New_Indx then
         --  The node is already in the bucket implied by Key. In this case
         --  we merely change its value without moving it.

         if HT.Lock > 0 then
            raise Program_Error with
              "attempt to tamper with elements (container is locked)";
         end if;

         Assign (NN (Node), Key);
         pragma Assert (Hash (NN (Node)) = New_Hash);
         pragma Assert (Equivalent_Keys (Key, NN (Node)));
         return;
      end if;

      --  The node is a bucket different from the bucket implied by Key

      if HT.Busy > 0 then
         raise Program_Error with
           "attempt to tamper with cursors (container is busy)";
      end if;

      --  Do the assignment first, before moving the node, so that if Assign
      --  propagates an exception, then the hash table will not have been
      --  modified (except for any possible side-effect Assign had on Node).

      Assign (NN (Node), Key);
      pragma Assert (Hash (NN (Node)) = New_Hash);
      pragma Assert (Equivalent_Keys (Key, NN (Node)));

      --  Now we can safely remove the node from its current bucket

      N := BB (Old_Indx);  -- get value of first node in old bucket
      pragma Assert (N /= 0);

      if N = Node then  -- node is first node in its bucket
         BB (Old_Indx) := Next (NN (Node));

      else
         pragma Assert (HT.Length > 1);

         loop
            M := Next (NN (N));
            pragma Assert (M /= 0);

            if M = Node then
               Set_Next (NN (N), Next => Next (NN (Node)));
               exit;
            end if;

            N := M;
         end loop;
      end if;

      --  Now we link the node into its new bucket (corresponding to Key)

      Set_Next (NN (Node), Next => New_Bucket);
      New_Bucket := Node;
   end Generic_Replace_Element;

end Ada.Containers.Hash_Tables.Generic_Bounded_Keys;
