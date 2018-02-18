------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J _ L I S T                                --
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

generic
   type Element (<>) is private;

package J_List is

   type List is private;
   type List_Iterator is private;

   Empty_List : constant List;

   procedure Prepend (Item : Element; To_List : in out List);
   --  Add a new element at the beginning of the list

   procedure Append (Item : Element; To_List : in out List);
   --  Add a new element at the end of the list

   procedure Append_If_Uniq (Item : Element; To_List : in out List);
   --  Add a new element at the end of the list, if the element is
   --  not already in the list

   procedure Copy (From_List : List; To_List : in out List);
   --  Duplicates all of From List elements in To_List. The list
   --  pointed by To_List is lost.

   procedure Clean (Alist : in out List);
   --  Delete all elements of the list

   function Exists (Item    : Element;
                    In_List : List)
                    return Boolean;
   --  Test if the item already exists in the list

   procedure Associate (The_List      : List;
                        With_Iterator : in out List_Iterator);
   --  Associate an iterator with a list

   function Get (Iterator : List_Iterator) return Element;
   --  Returns the current Element pointer to by the Iterator

   procedure Next (Iterator : in out List_Iterator);
   --  Move the Iterator to the next position

   function Is_Last (Iterator : List_Iterator) return Boolean;
   pragma Inline (Is_Last);
   --  Returns a pointer to the last element of the list + 1
   --  ie we should stop the loop to parse the list when the
   --  current iterator is Last (List)

   procedure Pop (In_List : in out List);
   --  Delete the last element inserted in the list
   --  raise List_Empty if the list does not contain at least one item

   generic
      with function Less_Than (A, B : Element) return Boolean;
   procedure Sort (The_List : List);
   --  Sort the list, based on the Sort_Func criterion
   --  Warning: This function is not efficient. It should be used for small
   --  lists only.
   --  The chosen algorithm is a bubble sort.

   List_Is_Empty : exception;

private

   type Element_Access is access all Element;
   type Node;
   type Node_Access is access Node;
   type Node is
      record
         Item  : Element_Access;
         Next  : Node_Access;
      end record;

   type List is
      record
         Head  : Node_Access := null;
         Last  : Node_Access := null;
      end record;

   type List_Iterator is
      record
         Content : Node_Access;
      end record;

   Empty_List : constant List := (Head => null, Last => null);

end J_List;
