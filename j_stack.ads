------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J _ S T A C K                               --
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

--  This generic package defines a general-purpose stack abstraction

generic
   type Element_Type is private;
   Max_Depth : Natural;
package J_Stack is
   pragma Elaborate_Body;

   function Element (Stack_Position : Natural) return Element_Type;
   --  Returns the stack element stored at Stack_Position; used to traverse
   --  all the elements stored in the stack.

   function Empty return Boolean;
   --  Returns True if and only if the stack is empty

   function Num_Elements return Natural;
   --  Returns the number of elements stored in the stack

   procedure Push (Elmt : Element_Type);
   --  Pushes the type Typ on the stack; raises an exception if the maximum
   --  stack depth is exceeded.

   procedure Pop;
   --  Pops the top type element of the stack; raises an exception if the stack
   --  is empty.

   function Pop return Element_Type;
   --  Pops and returns the top type element of the stack; raises an exception
   --  if the stack is empty.

   function Top return Element_Type;
   --  Returns the top type on the stack; raises an exception if the stack is
   --  empty.

end J_Stack;
