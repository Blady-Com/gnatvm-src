------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J V M . D B G                               --
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

--  This package provides a set of debugging routines that print the
--  contents of the various forms of JVM entity.

package JVM.Dbg is

   procedure Print_Class (C : Class_Id);
   --  Prints the basic information associated with a JVM class entity,
   --  including its name, superclass, and list of fields and methods (but
   --  not the code associated with the methods).

   procedure Print_Class_Parents (Class : Class_Id);
   --  Prints all the parents of Class

   procedure Print_Current_Class;
   --  Prints the class stored in Current_Class

   procedure Print_Current_Method;
   --  Prints the class stored in Current_Method

   procedure Print_Field (F : Field_Id);
   --  Prints information associated with a JVM class field entity, including
   --  the field's associated class and the field's type.

   procedure Print_Jcode (M : Method_Id);
   --  Prints the symbolic J-code associated with the given method.

   procedure Print_Jcode (Method : Method_Id; From_Line, To_Line : Positive);
   --  Prints the symbolic J-code associated with the given method.

   procedure Print_Local_Var (L : Local_Var_Id);
   --  Prints information associated with a JVM local variable entity,
   --  including the variable's type and word offset (index) in its method.

   procedure Print_Method (M : Method_Id);
   --  Prints information associated with a JVM class method entity, including
   --  the method's associated class, its parameters, and the method's result
   --  type.

   procedure Print_Type (T : Type_Id);
   --  Prints information associated with a JVM type entity: the type's name,
   --  kind (int, float, array, class, etc.), and other relevant information
   --  (e.g., element type and dimensions for an array type).

   procedure DPC (C : Class_Id);
   --  The same as Print_Class (shortened name for debugging convenience)

   procedure DPF (F : Field_Id);
   --  The same as Print_Field (shortened name for debugging convenience)

   procedure DPJ (M : Method_Id);
   --  The same as Print_Jcode (shortened name for debugging convenience)

   procedure DPL (L : Local_Var_Id);
   --  The same as Print_Local_Var (shortened name for debugging convenience)

   procedure DPM (M : Method_Id);
   --  The same as Print_Method (shortened name for debugging convenience)

   procedure DPT (T : Type_Id);
   --  The same as Print_Type (shortened name for debugging convenience)

   ------------------------------
   -- Routines for breakpoints --
   ------------------------------

   --  Some examples of use:
   --
   --  1. Instruction breakpoint: Register a breakpoint that is invoked when
   --     the backend is processing the method with identifier 300 and
   --     generates the 10th CIL instruction of this method.
   --
   --       (gdb) b dib if m=300 and l=10
   --
   --  2. Stack breakpoint: Register a breakpoint that is invoked when the
   --     backend is processing the method with identifier 300 and pushes in
   --     the third stack element an integer type.
   --
   --       (gdb) b dsb if m=300 and e=3 and t=int_type
   --
   --  3. Stack breakpoint: Register a breakpoint that is invoked when the
   --     backend is processing the method with identifier 300 and pushes any
   --     type in the fifth stack element.
   --
   --       (gdb) b dsb if m=300 and e=5
   --
   --  4. Stack watch: Register a breakpoint that is invoked whenever the stack
   --     associated with method 301 changes.
   --
   --       (gdb) b dsw if m=301

   procedure DIB (M : Method_Id; L : Natural);
   --  Routine designed to set breakpoints when the instruction L is generated
   --  for method M.

   procedure Instruction_Breakpoint (M : Method_Id; L : Natural)
     renames DIB;

   --  Routine designed for placing breakpoints whenever the instruction
   --  associated with line L is generated.

   procedure DSB (M : Method_Id; E : Natural; T : Type_Id);
   --  Routine designed for placing breakpoints on the stack associated
   --  with method M when the type T is added to the stack at position E.

   procedure DSW (M : Method_Id);
   --  This routine is invoked when an element is added or removed from the
   --  stack of method M. Used for placing breakpoints with gdb to trace the
   --  stack contents.

   procedure Stack_Breakpoint (M : Method_Id; E : Natural; T : Type_Id)
      renames DSB;
   --  The same as DSB. This is the routine that is invoked by the backend to
   --  leave the compiler sources clear.

   procedure Stack_Watch (M : Method_Id) renames DSW;
   --  The same as DSW. This is the routine that is invoked by the backend to
   --  leave the compiler sources clear.

   ----------------------------------------
   -- General Utility Debugging Routines --
   ----------------------------------------

   procedure Init_Source_Line_Output (Node : Node_Id);
   --  If debugging is enabled, then allows output of source line information
   --  from the source file associated with Node via calls to
   --  Print_Source_Line. Must be called before any call to Print_Source_Line.

   procedure Print (N : Name_Id);
   --  Writes N's associated string as debugging output

   procedure Print (S : String);
   --  Writes S as debugging output

   procedure Print_Class_Stack;
   --  Print the contents of Class_Stack

   procedure Print_Line (S : String := "");
   --  Writes S following by a new line as debugging output

   procedure Print_Scopes;
   --  Print the contents of JVM_Scope_Stack

   procedure Print_Source_Line (Node : Node_Id);
   --  If debugging is enabled, then prints the source line corresponding to
   --  Node preceded by the associated source file name and Sloc. If there was
   --  a preceding call to this procedure with a node having the same source
   --  line as Node then nothing is output.

end JVM.Dbg;
