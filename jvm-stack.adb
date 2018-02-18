------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M . C O D E                              --
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

with JVM.Dbg;  use JVM.Dbg;
with JVM.Info; use JVM.Info;
with Output;   use Output;
with Unchecked_Deallocation;

package body JVM.Stack is

   ----------
   -- Copy --
   ----------

   procedure Copy (From_Stack : Op_Stack_Id; To_Stack : in out Op_Stack_Id) is
   begin
      Free_Stack (To_Stack);
      To_Stack := New_Stack (Max_Elements (From_Stack));
      To_Stack.all := From_Stack.all;
   end Copy;

   ----------------
   -- Free_Stack --
   ----------------

   procedure Free_Stack (Stack : in out Op_Stack_Id) is
      procedure Free is
        new Unchecked_Deallocation (Operand_Stack, Op_Stack_Id);
   begin
      Free (Stack);
   end Free_Stack;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Stack : Op_Stack_Id) return Boolean is
   begin
      return Stack = null
        or else Stack.Top = Empty_Stack_Index;
   end Is_Empty;

   ----------
   -- Mark --
   ----------

   procedure Mark (Stack : Op_Stack_Id) is
   begin
      pragma Assert (Stack.Top /= Empty_Stack_Index);

      Stack.Mark_Level := Stack.Top;
      Stack.Stack (Stack.Top).Mark_Count
        := Stack.Stack (Stack.Top).Mark_Count + 1;
   end Mark;

   ------------
   -- Marked --
   ------------

   function Marked (Stack : Op_Stack_Id) return Boolean is
   begin
      return Stack.Mark_Level > Empty_Stack_Index;
   end Marked;

   ---------------
   -- Max_Depth --
   ---------------

   function Max_Depth (Stack : Op_Stack_Id) return Depth_Range is
   begin
      return Stack.Max_Depth;
   end Max_Depth;

   ------------------
   -- Max_Elements --
   ------------------

   function Max_Elements (Stack : Op_Stack_Id) return Stack_Range is
   begin
      return Stack.Max;
   end Max_Elements;

   ---------------
   -- New_Stack --
   ---------------

   function New_Stack (Max_Elements : Stack_Range) return Op_Stack_Id is
   begin
      return new Operand_Stack (Max_Elements);
   end New_Stack;

   -----------------
   -- Next_To_Top --
   -----------------

   function Next_To_Top (Stack : Op_Stack_Id) return Type_Id is
   begin
      pragma Assert (Stack.Top >= 2);

      return Stack.Stack (Stack.Top - 1).Jtype;
   end Next_To_Top;

   ------------------
   -- Num_Elements --
   ------------------

   function Num_Elements (Stack : Op_Stack_Id) return Natural is
   begin
      return Natural (Stack.Top - Empty_Stack_Index);
   end Num_Elements;

   ---------
   -- Pop --
   ---------

   procedure Pop (Stack : Op_Stack_Id; Count : Stack_Range := 1) is
   begin
      pragma Assert (Stack.Top - Count >= Empty_Stack_Index);

      pragma Assert (Stack.Mark_Level = Empty_Stack_Index
             or else Stack.Top - Count >= Stack.Mark_Level);

      for T in 0 .. Count - 1 loop
         Stack.Curr_Depth := Stack.Curr_Depth
           - Depth_Range (Word_Size (Stack.Stack (Stack.Top - T).Jtype));
      end loop;

      Stack.Top := Stack.Top - Count;

      --  Invoke the debugging routine

      Stack_Watch (Stack.Method);
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop (Stack : Op_Stack_Id) return Type_Id is
   begin
      pragma Assert (Stack.Top > Empty_Stack_Index);

      pragma Assert (Stack.Mark_Level = Empty_Stack_Index
             or else Stack.Top - 1 >= Stack.Mark_Level);

      Stack.Curr_Depth := Stack.Curr_Depth
        - Depth_Range (Word_Size (Stack.Stack (Stack.Top).Jtype));

      Stack.Top := Stack.Top - 1;
      return Stack.Stack (Stack.Top + 1).Jtype;
   end Pop;

   -----------------
   -- Print_Stack --
   -----------------

   procedure Print_Stack
     (Stack     : Op_Stack_Id;
      Max_Elems : Stack_Range := Stack_Range'Last)
   is
      First_Elem : Stack_Range;
      First      : Integer;
      Disp       : Stack_Range;

   begin
      if Stack = null then
         return;
      end if;

      if Max_Elems = Stack_Range'Last then
         First_Elem := Empty_Stack_Index + 1;
      else
         First := Integer (Stack.Top) - Integer (Max_Elems) + 1;

         if First < Integer (Empty_Stack_Index + 1) then
            First_Elem := Empty_Stack_Index + 1;
         else
            First_Elem := Stack_Range (First);
         end if;
      end if;

      Write_Str ("======(Top of Stack)======");
      Write_Eol;

      if Is_Empty (Stack) then
         Write_Str (" *** <stack is empty> ***");
         Write_Eol;
      else
         for Index in reverse First_Elem .. Stack.Top loop
            Disp := Stack.Top - Index;

            Write_Str ("[" & Index'Img & " :" & Disp'Img & " ]: ");

            if Name (Stack.Stack (Index).Jtype) = No_Name then
               Write_Str ("<unknown type>");
            else
               Write_Name (Name (Stack.Stack (Index).Jtype));
               Write_Str (" (type_id =" & Stack.Stack (Index).Jtype'Img & ")");

               if Is_Array_Descriptor (Stack.Stack (Index).Jtype) then
                  Write_Str ("... array descriptor");

               elsif Is_Descriptor (Stack.Stack (Index).Jtype) then
                  Write_Str ("... descriptor");
               end if;
            end if;

            Write_Str (" " & Type_Kind (Stack.Stack (Index).Jtype)'Img);
            Write_Eol;
         end loop;
      end if;

      Write_Str ("======(Stack Bottom)======");
      Write_Eol;
      Write_Eol;
   end Print_Stack;

   ----------
   -- Push --
   ----------

   procedure Push (Stack : Op_Stack_Id; Typ : Type_Id) is
   begin
      pragma Assert (Stack.Top < Stack.Max);

      Stack.Top := Stack.Top + 1;
      Stack.Stack (Stack.Top).Jtype := Typ;

      Stack.Curr_Depth := Stack.Curr_Depth + Depth_Range (Word_Size (Typ));
      if Stack.Curr_Depth > Stack.Max_Depth then
         Stack.Max_Depth := Stack.Curr_Depth;
      end if;

      --  Invoke the debugging routine. Note that Stack_Watch is
      --  implicitly invoked.

      Stack_Watch      (Stack.Method);
      Stack_Breakpoint (Stack.Method, Natural (Stack.Top), Typ);
   end Push;

   -------------
   -- Release --
   -------------

   procedure Release (Stack : Op_Stack_Id) is
      Pop_Count   : constant Stack_Range := Stack.Top - Stack.Mark_Level;
      Stack_Index : Stack_Range;

   begin
      pragma Assert (Stack.Mark_Level <= Stack.Top);

      --  Check that the stack below the released stack elements is
      --  consistent with the released operand types (each of the
      --  popped types must be mirrored by another type in the same
      --  relative position below the mark point). Note that we
      --  only check type kinds here, which is adequate for most
      --  purposes and avoids problems with cases of N_Conditional_Ops
      --  where there might be compatible but differing scalar types
      --  on the stack (e.g., byte vs. int).

      --  ??? In some cases (e.g. some kinds of UC, the checks below are
      --  too strong, so disable them for now, but they can be useful for
      --  debugging purposes, so are left here for reference.
      --
      --  for Index in Stack.Mark_Level + 1 .. Stack.Top loop
      --     pragma Assert
      --       (Type_Kind (Stack.Stack (Index).Jtype)
      --          = Type_Kind (Stack.Stack (Index - Pop_Count).Jtype));
      --     null;
      --  end loop;

      Pop (Stack, Pop_Count);
      pragma Assert (Stack.Top = Stack.Mark_Level);

      Stack.Stack (Stack.Top).Mark_Count
        := Stack.Stack (Stack.Top).Mark_Count - 1;

      --  If there is no mark at the current Mark_Level, then
      --  look down the stack to see if any other stack entries
      --  have marks, and reset the Mark_Level to indicate the
      --  nearest mark (if none, then Mark_Level will be set to
      --  Empty_Stack_Index).

      if Stack.Stack (Stack.Top).Mark_Count = 0 then
         Stack.Mark_Level := Empty_Stack_Index;

         Stack_Index := Stack.Top - 1;
         while Stack_Index > Empty_Stack_Index loop
            if Stack.Stack (Stack_Index).Mark_Count > 0 then
               Stack.Mark_Level := Stack_Index;
               exit;
            end if;
            Stack_Index := Stack_Index - 1;
         end loop;
      end if;
   end Release;

   -----------
   -- Reset --
   -----------

   procedure Reset (Stack : Op_Stack_Id) is
   begin
      Stack.Top := Empty_Stack_Index;
      Stack.Curr_Depth := 0;
      Stack.Mark_Level := Empty_Stack_Index;
   end Reset;

   -------------------
   -- Set_Max_Depth --
   -------------------

   procedure Set_Max_Depth (Stack : Op_Stack_Id; Depth : Depth_Range) is
   begin
      pragma Assert (Depth >= Stack.Max_Depth);

      Stack.Max_Depth := Depth;
   end Set_Max_Depth;

   ----------------------
   -- Set_Stack_Method --
   ----------------------

   procedure Set_Stack_Method (Stack : Op_Stack_Id; Method : Method_Id) is
   begin
      Stack.Method := Method;
   end Set_Stack_Method;

   ---------
   -- Top --
   ---------

   function Top
     (Stack : Op_Stack_Id;
      Disp  : Stack_Range := 0) return Type_Id
   is
      Index : constant Integer := Integer (Stack.Top - Disp);

   begin
      pragma Assert (Index > Integer (Empty_Stack_Index));

      return Stack.Stack (Stack_Range (Index)).Jtype;
   end Top;

end JVM.Stack;
