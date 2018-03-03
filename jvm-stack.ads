------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M . C O D E                              --
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

--  This package provides support for operand stack management. Used in code
--  generation and code verification.

package JVM.Stack is

   type Op_Stack_Id is private;
   --  Values of this type denote a simulated operand type stack
   --  associated with a method or subroutine.

   Null_Op_Stack : constant Op_Stack_Id;
   --  A constant denoting no stack; the initial value of an Op_Stack_Id

   type Stack_Range is range 0 .. 1000;
   --  The range of one and two-word items that can be allocated on an
   --  operand type stack.

   type Depth_Range is range 0 .. Stack_Range'Last * 2;
   --  The allowed range of depths of the operand stack in words

   procedure Copy (From_Stack : Op_Stack_Id; To_Stack : in out Op_Stack_Id);
   --  Duplicates the contents of From_Stack elements into To_Stack. The
   --  stack pointed by To_Stack is lost.

   procedure Free_Stack (Stack : in out Op_Stack_Id);
   --  Frees up any space associated with the given stack and sets Stack to
   --  Null_Op_Stack.

   function Is_Empty (Stack : Op_Stack_Id) return Boolean;
   --  Returns True if and only if the stack is empty

   procedure Mark (Stack : Op_Stack_Id);
   --  Record the current top-of-stack level of Stack to allow a later
   --  release to this level via a call to Release. Raises an exception
   --  if the stack is currently empty.

   function Marked (Stack : Op_Stack_Id) return Boolean;
   --  Returns Boolean result indicating whether the given stack is marked

   function Max_Depth (Stack : Op_Stack_Id) return Depth_Range;
   --  Returns the current maximum depth of the stack up to this time,
   --  given in terms of words.

   function Max_Elements (Stack : Op_Stack_Id) return Stack_Range;
   --  Returns the maximum number of elements that can be stored in the stack

   function New_Stack (Max_Elements : Stack_Range) return Op_Stack_Id;
   --  Allocates a new operand type stack with a maximum depth given
   --  by Max_Elements.

   function Next_To_Top (Stack : Op_Stack_Id) return Type_Id;
   --  Returns the type below the top stack element; raises an exception
   --  if the stack contains fewer than two elements.

   function Num_Elements (Stack : Op_Stack_Id) return Natural;
   --  Returns the number of elements stored in the stack

   procedure Pop (Stack : Op_Stack_Id; Count : Stack_Range := 1);
   --  Pops Count elements from the stack; raises an exception
   --  if Count exceeds the current stack depth.

   function Pop (Stack : Op_Stack_Id) return Type_Id;
   --  Pops and returns the top type element of the stack; raises an
   --  exception if the stack is empty.

   procedure Print_Stack
     (Stack     : Op_Stack_Id;
      Max_Elems : Stack_Range := Stack_Range'Last);
   --  Prints out the current contents of the operand type stack
   --  to standard output. Useful for debugging.

   procedure Push (Stack : Op_Stack_Id; Typ : Type_Id);
   --  Pushes the type Typ on the stack; raises an exception if the
   --  maximum stack depth is exceeded.

   procedure Release (Stack : Op_Stack_Id);
   --  Restores the current top-of-stack level of Stack to the level
   --  recorded by the most recent call to Mark. Any operand types that
   --  are above the mark level are discarded. Once a stack has been
   --  released another call to Mark is required before a Release
   --  can occur. Raises an exception if the stack does not have
   --  an active mark level or if the mark level is greater than
   --  the current top of stack. Also checks that each released
   --  operand type is mirrored by a stack element with that type
   --  at the same relative position below the mark point; raises
   --  an exception if this condition is violated.

   procedure Reset (Stack : Op_Stack_Id);
   --  Pops all contents of the given stack, leaving it empty.
   --  Releases any stack mark that has been set.

   procedure Set_Stack_Method (Stack : Op_Stack_Id; Method : Method_Id);
   --  Associate Stack with method M. Used for debugging.

   procedure Set_Max_Depth (Stack : Op_Stack_Id; Depth : Depth_Range);
   --  Forcibly sets the maximum depth of the stack to Depth. This
   --  is needed for updating method stack depths when generating
   --  subroutine calls to reflect an increment by the maximum
   --  depth of the called subroutine's stack.

   function Top
     (Stack : Op_Stack_Id;
      Disp  : Stack_Range := 0) return Type_Id;
   --  Returns the top type on the stack; raises an exception if the
   --  stack is empty.

private
   type Stack_Element is record
      Jtype      : Type_Id;
      Mark_Count : Natural := 0;
   end record;

   type Stack_Buffer is array (Stack_Range range <>) of Stack_Element;

   Empty_Stack_Index : constant Stack_Range := Stack_Range'First;

   type Operand_Stack (Max : Stack_Range) is record
      Top        : Stack_Range := Empty_Stack_Index;
      Curr_Depth : Depth_Range := 0;
      Max_Depth  : Depth_Range := 0;
      Mark_Level : Stack_Range := Empty_Stack_Index;
      Method     : Method_Id   := Null_Method;
      Stack      : Stack_Buffer (Empty_Stack_Index .. Max) :=
                     (others => (Null_Type, 0));
   end record;

   type Op_Stack_Id is access Operand_Stack;

   Null_Op_Stack : constant Op_Stack_Id := null;

end JVM.Stack;
