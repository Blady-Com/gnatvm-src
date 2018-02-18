------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J _ L I S T                                --
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

with Unchecked_Deallocation;

package body J_List is

   procedure Free is
      new Unchecked_Deallocation (Node, Node_Access);

   procedure Element_Free is
      new Unchecked_Deallocation (Element, Element_Access);

   -------------
   -- Prepend --
   -------------

   procedure Prepend (Item    : Element;
                      To_List : in out List)
   is
   begin
      To_List.Head := new Node'(Item => new Element'(Item),
                                Next => To_List.Head);
      if To_List.Head.Next = null then
         To_List.Last := To_List.Head;
      end if;
   end Prepend;

   ------------
   -- Append --
   ------------

   procedure Append (Item : Element; To_List : in out List) is
   begin
      if To_List.Last = null then
         To_List.Head := new Node'(Item => new Element'(Item),
                                   Next => null);
         To_List.Last := To_List.Head;
      else
         To_List.Last.Next := new Node'(Item => new Element'(Item),
                                        Next => null);
         To_List.Last := To_List.Last.Next;
      end if;
   end Append;

   -----------------
   -- Append_Uniq --
   -----------------

   procedure Append_If_Uniq (Item : Element; To_List : in out List) is
   begin
      if not Exists (Item, To_List) then
         Append (Item, To_List);
      end if;
   end Append_If_Uniq;

   ---------------
   -- Associate --
   ---------------

   procedure Associate (The_List      : List;
                        With_Iterator : in out List_Iterator)
   is
   begin
      With_Iterator.Content := The_List.Head;
   end Associate;

   -----------
   -- Clean --
   -----------

   procedure Clean (Alist : in out List)
   is
      Current  : Node_Access := Alist.Head;
      Next_One : Node_Access;
   begin
      while Current /= null loop
         Next_One := Current.Next;
         Element_Free (Current.Item);
         Free (Current);
         Current := Next_One;
      end loop;
      Alist.Head := null;
      Alist.Last := null;
   end Clean;

   ----------
   -- Copy --
   ----------

   procedure Copy (From_List : List; To_List : in out List) is
      Ptr : Node_Access := From_List.Head;
   begin
      Clean (To_List);
      while Ptr /= null loop
         if To_List.Last = null then
            To_List.Head := new Node'(Item => Ptr.Item, Next => null);
            To_List.Last := To_List.Head;
         else
            To_List.Last.Next := new Node'(Item => Ptr.Item, Next => null);
            To_List.Last := To_List.Last.Next;
         end if;
         Ptr := Ptr.Next;
      end loop;
   end Copy;

   ------------
   -- Exists --
   ------------

   function Exists (Item    : Element;
                    In_List : List)
                    return Boolean
   is
      Current  : Node_Access := In_List.Head;
   begin
      while Current /= null loop
         if Current.Item.all = Item then
            return True;
         end if;
         Current := Current.Next;
      end loop;

      return False;
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Iterator : List_Iterator) return Element is
   begin
      return Iterator.Content.Item.all;
   end Get;

   -------------
   -- Is_Last --
   -------------

   function Is_Last (Iterator : List_Iterator) return Boolean is
   begin
      return Iterator.Content = null;
   end Is_Last;

   ----------
   -- Next --
   ----------

   procedure Next (Iterator : in out List_Iterator) is
   begin
      Iterator.Content := Iterator.Content.Next;
   end Next;

   ---------
   -- Pop --
   ---------

   procedure Pop (In_List : in out List) is
      Current : Node_Access := In_List.Head;
   begin
      if In_List.Head = null then
         raise List_Is_Empty;
      end if;

      In_List.Head := In_List.Head.Next;
      Element_Free (Current.Item);
      Free (Current);
   end Pop;

   ----------
   -- Sort --
   ----------

   procedure Sort (The_List : List)
   is
      Current   : Node_Access;
      Bound     : Node_Access := The_List.Last; --  Last item to check
      Highest_Exchange : Node_Access;
      Tmp       : Element_Access;
   begin
      --  The chosen algorithm is a bubble sort, so that replacement is done
      --  in place, and we do not have to do any memory allocation

      --  We need at least two elements
      if The_List.Head = null or else The_List.Head.Next = null then
         return;
      end if;

      loop

         Current := The_List.Head;
         Highest_Exchange := null;

         while Current /= Bound loop
            if Less_Than (Current.Next.Item.all, Current.Item.all) then
               Tmp := Current.Item;
               Current.Item := Current.Next.Item;
               Current.Next.Item := Tmp;
               Highest_Exchange := Current;
            end if;

            Current := Current.Next;
         end loop;

         if Highest_Exchange = null or Highest_Exchange = The_List.Head then
            return;
         end if;

         Bound := Highest_Exchange;
      end loop;
   end Sort;

end J_List;
