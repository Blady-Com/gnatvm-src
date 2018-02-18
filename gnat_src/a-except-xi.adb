------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This body is part of the bare board Ravenscar run time. It implements
--  Ada 83 exception handling, plus a subset of the operations available
--  in Ada 95 for Exception_Occurrences and Exception_Ids (Exception_Name,
--  Exception_Identity ...).

with System;                  use System;
with System.Standard_Library; use System.Standard_Library;
with System.Soft_Links;       use System.Soft_Links;
with System.Exceptions_Debug; use System.Exceptions_Debug;

package body Ada.Exceptions is

   procedure Last_Chance_Handler (Except :  Exception_Occurrence);
   pragma Import (C, Last_Chance_Handler, "__gnat_last_chance_handler");
   pragma No_Return (Last_Chance_Handler);

   pragma Suppress (All_Checks);
   --  We definitely do not want exceptions occurring within this unit, or
   --  we are in big trouble. If an exceptional situation does occur, better
   --  that it not be raised, since raising it can cause confusing chaos.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Code_Address_For_AAA return System.Address;
   function Code_Address_For_ZZZ return System.Address;
   --  Return start and end of procedures in this package
   --
   --  These procedures are used to provide exclusion bounds in
   --  calls to Call_Chain at exception raise points from this unit. The
   --  purpose is to arrange for the exception tracebacks not to include
   --  frames from routines involved in the raise process, as these are
   --  meaningless from the user's standpoint.
   --
   --  For these bounds to be meaningful, we need to ensure that the object
   --  code for the routines involved in processing a raise is located after
   --  the object code Code_Address_For_AAA and before the object code
   --  Code_Address_For_ZZZ. This will indeed be the case as long as the
   --  following rules are respected:
   --
   --  1) The bodies of the subprograms involved in processing a raise
   --     are located after the body of Code_Address_For_AAA and before the
   --     body of Code_Address_For_ZZZ.
   --
   --  2) No pragma Inline applies to any of these subprograms, as this
   --     could delay the corresponding assembly output until the end of
   --     the unit.

   procedure Call_Chain (Excep : EOA);
   --  Generate traceback if enabled

   procedure Process_Exception
     (E          : Exception_Id;
      Is_Reraise : Boolean := False);
   --  Shared exception processing for raise / reraise
   pragma No_Return (Process_Exception);
   pragma Export (Ada, Process_Exception, "__gnat_raise_nodefer_with_msg");

   procedure Raise_Constraint_Error
     (File : System.Address;
      Line : Integer);
   pragma No_Return (Raise_Constraint_Error);
   pragma Export
     (C, Raise_Constraint_Error, "__gnat_raise_constraint_error");
   --  Raise constraint error

   procedure Raise_Program_Error
     (File : System.Address;
      Line : Integer);
   pragma No_Return (Raise_Program_Error);
   pragma Export
     (C, Raise_Program_Error, "__gnat_raise_program_error");
   --  Raise program error

   procedure Raise_Storage_Error
     (File : System.Address;
      Line : Integer);
   pragma No_Return (Raise_Storage_Error);
   pragma Export
     (C, Raise_Storage_Error, "__gnat_raise_storage_error");
   --  Raise storage error

   -----------------------------
   -- Run-Time Check Routines --
   -----------------------------

   --  These routines raise a specific exception with a reason message
   --  attached. The parameters are the file name and line number in each
   --  case. The names are defined by Exp_Ch11.Get_RT_Exception_Name.

   procedure Rcheck_CE_Access_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Null_Access_Parameter
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Discriminant_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Divide_By_Zero
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Explicit_Raise
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Index_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Invalid_Data
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Length_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Null_Exception_Id
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Null_Not_Allowed
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Overflow_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Partition_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Range_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_CE_Tag_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Access_Before_Elaboration
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Accessibility_Check
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Address_Of_Intrinsic
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Aliased_Parameters
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_All_Guards_Closed
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Bad_Predicated_Generic_Type
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Current_Task_In_Entry_Body
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Duplicated_Entry_Address
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Explicit_Raise
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Implicit_Return
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Misaligned_Address_Value
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Missing_Return
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Overlaid_Controlled_Object
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Potentially_Blocking_Operation
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Stubbed_Subprogram_Called
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Unchecked_Union_Restriction
     (File : System.Address; Line : Integer);
   procedure Rcheck_PE_Non_Transportable_Actual
     (File : System.Address; Line : Integer);
   procedure Rcheck_SE_Empty_Storage_Pool
     (File : System.Address; Line : Integer);
   procedure Rcheck_SE_Explicit_Raise
     (File : System.Address; Line : Integer);
   procedure Rcheck_SE_Infinite_Recursion
     (File : System.Address; Line : Integer);
   procedure Rcheck_SE_Object_Too_Large
     (File : System.Address; Line : Integer);

   procedure Rcheck_PE_Finalize_Raised_Exception
     (File : System.Address; Line : Integer);

   pragma Export (C, Rcheck_CE_Access_Check,
                  "__gnat_rcheck_CE_Access_Check");
   pragma Export (C, Rcheck_CE_Null_Access_Parameter,
                  "__gnat_rcheck_CE_Null_Access_Parameter");
   pragma Export (C, Rcheck_CE_Discriminant_Check,
                  "__gnat_rcheck_CE_Discriminant_Check");
   pragma Export (C, Rcheck_CE_Divide_By_Zero,
                  "__gnat_rcheck_CE_Divide_By_Zero");
   pragma Export (C, Rcheck_CE_Explicit_Raise,
                  "__gnat_rcheck_CE_Explicit_Raise");
   pragma Export (C, Rcheck_CE_Index_Check,
                  "__gnat_rcheck_CE_Index_Check");
   pragma Export (C, Rcheck_CE_Invalid_Data,
                  "__gnat_rcheck_CE_Invalid_Data");
   pragma Export (C, Rcheck_CE_Length_Check,
                  "__gnat_rcheck_CE_Length_Check");
   pragma Export (C, Rcheck_CE_Null_Exception_Id,
                  "__gnat_rcheck_CE_Null_Exception_Id");
   pragma Export (C, Rcheck_CE_Null_Not_Allowed,
                  "__gnat_rcheck_CE_Null_Not_Allowed");
   pragma Export (C, Rcheck_CE_Overflow_Check,
                  "__gnat_rcheck_CE_Overflow_Check");
   pragma Export (C, Rcheck_CE_Partition_Check,
                  "__gnat_rcheck_CE_Partition_Check");
   pragma Export (C, Rcheck_CE_Range_Check,
                  "__gnat_rcheck_CE_Range_Check");
   pragma Export (C, Rcheck_CE_Tag_Check,
                  "__gnat_rcheck_CE_Tag_Check");
   pragma Export (C, Rcheck_PE_Access_Before_Elaboration,
                  "__gnat_rcheck_PE_Access_Before_Elaboration");
   pragma Export (C, Rcheck_PE_Accessibility_Check,
                  "__gnat_rcheck_PE_Accessibility_Check");
   pragma Export (C, Rcheck_PE_Address_Of_Intrinsic,
                  "__gnat_rcheck_PE_Address_Of_Intrinsic");
   pragma Export (C, Rcheck_PE_Aliased_Parameters,
                  "__gnat_rcheck_PE_Aliased_Parameters");
   pragma Export (C, Rcheck_PE_All_Guards_Closed,
                  "__gnat_rcheck_PE_All_Guards_Closed");
   pragma Export (C, Rcheck_PE_Bad_Predicated_Generic_Type,
                  "__gnat_rcheck_PE_Bad_Predicated_Generic_Type");
   pragma Export (C, Rcheck_PE_Current_Task_In_Entry_Body,
                  "__gnat_rcheck_PE_Current_Task_In_Entry_Body");
   pragma Export (C, Rcheck_PE_Duplicated_Entry_Address,
                  "__gnat_rcheck_PE_Duplicated_Entry_Address");
   pragma Export (C, Rcheck_PE_Explicit_Raise,
                  "__gnat_rcheck_PE_Explicit_Raise");
   pragma Export (C, Rcheck_PE_Finalize_Raised_Exception,
                  "__gnat_rcheck_PE_Finalize_Raised_Exception");
   pragma Export (C, Rcheck_PE_Implicit_Return,
                  "__gnat_rcheck_PE_Implicit_Return");
   pragma Export (C, Rcheck_PE_Misaligned_Address_Value,
                  "__gnat_rcheck_PE_Misaligned_Address_Value");
   pragma Export (C, Rcheck_PE_Missing_Return,
                  "__gnat_rcheck_PE_Missing_Return");
   pragma Export (C, Rcheck_PE_Overlaid_Controlled_Object,
                  "__gnat_rcheck_PE_Overlaid_Controlled_Object");
   pragma Export (C, Rcheck_PE_Potentially_Blocking_Operation,
                  "__gnat_rcheck_PE_Potentially_Blocking_Operation");
   pragma Export (C, Rcheck_PE_Stubbed_Subprogram_Called,
                  "__gnat_rcheck_PE_Stubbed_Subprogram_Called");
   pragma Export (C, Rcheck_PE_Unchecked_Union_Restriction,
                  "__gnat_rcheck_PE_Unchecked_Union_Restriction");
   pragma Export (C, Rcheck_PE_Non_Transportable_Actual,
                  "__gnat_rcheck_PE_Non_Transportable_Actual");
   pragma Export (C, Rcheck_SE_Empty_Storage_Pool,
                  "__gnat_rcheck_SE_Empty_Storage_Pool");
   pragma Export (C, Rcheck_SE_Explicit_Raise,
                  "__gnat_rcheck_SE_Explicit_Raise");
   pragma Export (C, Rcheck_SE_Infinite_Recursion,
                  "__gnat_rcheck_SE_Infinite_Recursion");
   pragma Export (C, Rcheck_SE_Object_Too_Large,
                  "__gnat_rcheck_SE_Object_Too_Large");

   --  None of these procedures ever returns (they raise an exception!). By
   --  using pragma No_Return, we ensure that any junk code after the call,
   --  such as normal return epilog stuff, can be eliminated).

   pragma No_Return (Rcheck_CE_Access_Check);
   pragma No_Return (Rcheck_CE_Null_Access_Parameter);
   pragma No_Return (Rcheck_CE_Discriminant_Check);
   pragma No_Return (Rcheck_CE_Divide_By_Zero);
   pragma No_Return (Rcheck_CE_Explicit_Raise);
   pragma No_Return (Rcheck_CE_Index_Check);
   pragma No_Return (Rcheck_CE_Invalid_Data);
   pragma No_Return (Rcheck_CE_Length_Check);
   pragma No_Return (Rcheck_CE_Null_Exception_Id);
   pragma No_Return (Rcheck_CE_Null_Not_Allowed);
   pragma No_Return (Rcheck_CE_Overflow_Check);
   pragma No_Return (Rcheck_CE_Partition_Check);
   pragma No_Return (Rcheck_CE_Range_Check);
   pragma No_Return (Rcheck_CE_Tag_Check);
   pragma No_Return (Rcheck_PE_Access_Before_Elaboration);
   pragma No_Return (Rcheck_PE_Accessibility_Check);
   pragma No_Return (Rcheck_PE_Address_Of_Intrinsic);
   pragma No_Return (Rcheck_PE_Aliased_Parameters);
   pragma No_Return (Rcheck_PE_All_Guards_Closed);
   pragma No_Return (Rcheck_PE_Bad_Predicated_Generic_Type);
   pragma No_Return (Rcheck_PE_Current_Task_In_Entry_Body);
   pragma No_Return (Rcheck_PE_Duplicated_Entry_Address);
   pragma No_Return (Rcheck_PE_Explicit_Raise);
   pragma No_Return (Rcheck_PE_Implicit_Return);
   pragma No_Return (Rcheck_PE_Misaligned_Address_Value);
   pragma No_Return (Rcheck_PE_Missing_Return);
   pragma No_Return (Rcheck_PE_Overlaid_Controlled_Object);
   pragma No_Return (Rcheck_PE_Potentially_Blocking_Operation);
   pragma No_Return (Rcheck_PE_Stubbed_Subprogram_Called);
   pragma No_Return (Rcheck_PE_Unchecked_Union_Restriction);
   pragma No_Return (Rcheck_PE_Non_Transportable_Actual);
   pragma No_Return (Rcheck_PE_Finalize_Raised_Exception);
   pragma No_Return (Rcheck_SE_Empty_Storage_Pool);
   pragma No_Return (Rcheck_SE_Explicit_Raise);
   pragma No_Return (Rcheck_SE_Infinite_Recursion);
   pragma No_Return (Rcheck_SE_Object_Too_Large);

   --------------------------
   -- Code_Address_For_AAA --
   --------------------------

   --  This function gives us the start of the PC range for addresses
   --  within the exception unit itself. We hope that gigi/gcc keep all the
   --  procedures in their original order!

   function Code_Address_For_AAA return System.Address is
   begin
      --  We are using a label instead of merely using
      --  Code_Address_For_AAA'Address because on some platforms the latter
      --  does not yield the address we want, but the address of a stub or of
      --  a descriptor instead. This is the case at least on Alpha-VMS and
      --  PA-HPUX.

      <<Start_Of_AAA>>
      return Start_Of_AAA'Address;
   end Code_Address_For_AAA;

   ----------------
   -- Call_Chain --
   ----------------

   procedure Call_Chain (Excep : EOA) is separate;

   ------------------------
   -- Exception_Identity --
   ------------------------

   function Exception_Identity
     (X : Exception_Occurrence) return Exception_Id
   is
   begin
      return X.Id;
   end Exception_Identity;

   --------------------
   -- Exception_Name --
   --------------------

   function Exception_Name (X : Exception_Occurrence) return String is
   begin
      return Exception_Name (X.Id);
   end Exception_Name;

   function Exception_Name (Id : Exception_Id) return String is
   begin
      return To_Ptr (Id.Full_Name).all (1 .. Id.Name_Length - 1);
   end Exception_Name;

   --------------------------------------
   -- Calls to Run-Time Check Routines --
   --------------------------------------

   procedure Rcheck_CE_Access_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_CE_Access_Check;

   procedure Rcheck_CE_Null_Access_Parameter
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_CE_Null_Access_Parameter;

   procedure Rcheck_CE_Discriminant_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_CE_Discriminant_Check;

   procedure Rcheck_CE_Divide_By_Zero
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_CE_Divide_By_Zero;

   procedure Rcheck_CE_Explicit_Raise
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_CE_Explicit_Raise;

   procedure Rcheck_CE_Index_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_CE_Index_Check;

   procedure Rcheck_CE_Invalid_Data
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_CE_Invalid_Data;

   procedure Rcheck_CE_Length_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_CE_Length_Check;

   procedure Rcheck_CE_Null_Exception_Id
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_CE_Null_Exception_Id;

   procedure Rcheck_CE_Null_Not_Allowed
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_CE_Null_Not_Allowed;

   procedure Rcheck_CE_Overflow_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_CE_Overflow_Check;

   procedure Rcheck_CE_Partition_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_CE_Partition_Check;

   procedure Rcheck_CE_Range_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_CE_Range_Check;

   procedure Rcheck_CE_Tag_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Constraint_Error (File, Line);
   end Rcheck_CE_Tag_Check;

   procedure Rcheck_PE_Access_Before_Elaboration
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Access_Before_Elaboration;

   procedure Rcheck_PE_Accessibility_Check
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Accessibility_Check;

   procedure Rcheck_PE_Address_Of_Intrinsic
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Address_Of_Intrinsic;

   procedure Rcheck_PE_Aliased_Parameters
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Aliased_Parameters;

   procedure Rcheck_PE_All_Guards_Closed
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_All_Guards_Closed;

   procedure Rcheck_PE_Bad_Predicated_Generic_Type
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Bad_Predicated_Generic_Type;

   procedure Rcheck_PE_Current_Task_In_Entry_Body
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Current_Task_In_Entry_Body;

   procedure Rcheck_PE_Duplicated_Entry_Address
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Duplicated_Entry_Address;

   procedure Rcheck_PE_Explicit_Raise
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Explicit_Raise;

   procedure Rcheck_PE_Implicit_Return
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Implicit_Return;

   procedure Rcheck_PE_Misaligned_Address_Value
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Misaligned_Address_Value;

   procedure Rcheck_PE_Missing_Return
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Missing_Return;

   procedure Rcheck_PE_Overlaid_Controlled_Object
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Overlaid_Controlled_Object;

   procedure Rcheck_PE_Potentially_Blocking_Operation
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Potentially_Blocking_Operation;

   procedure Rcheck_PE_Stubbed_Subprogram_Called
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Stubbed_Subprogram_Called;

   procedure Rcheck_PE_Unchecked_Union_Restriction
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Unchecked_Union_Restriction;

   procedure Rcheck_PE_Non_Transportable_Actual
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Non_Transportable_Actual;

   procedure Rcheck_SE_Empty_Storage_Pool
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Storage_Error (File, Line);
   end Rcheck_SE_Empty_Storage_Pool;

   procedure Rcheck_SE_Explicit_Raise
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Storage_Error (File, Line);
   end Rcheck_SE_Explicit_Raise;

   procedure Rcheck_SE_Infinite_Recursion
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Storage_Error (File, Line);
   end Rcheck_SE_Infinite_Recursion;

   procedure Rcheck_SE_Object_Too_Large
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Storage_Error (File, Line);
   end Rcheck_SE_Object_Too_Large;

   procedure Rcheck_PE_Finalize_Raised_Exception
     (File : System.Address; Line : Integer)
   is
   begin
      Raise_Program_Error (File, Line);
   end Rcheck_PE_Finalize_Raised_Exception;

   ----------------------------
   -- Raise_Constraint_Error --
   ----------------------------

   procedure Raise_Constraint_Error (File : System.Address; Line : Integer) is
      pragma Unreferenced (File, Line);
   begin
      Raise_Exception (Constraint_Error_Def'Access);
   end Raise_Constraint_Error;

   -----------------------
   -- Process_Exception --
   -----------------------

   procedure Process_Exception
     (E          : Exception_Id;
      Is_Reraise : Boolean := False)
   is
      Jumpbuf_Ptr : constant Address := Get_Jmpbuf_Address.all;
      Excep       : constant EOA     := Get_Current_Excep.all;
   begin
      Debug_Raise_Exception (E => SSL.Exception_Data_Ptr (E));

      --  Store the identifier for this exception because it may be
      --  needed by a reraise.

      Excep.Id := E;

      --  Generate traceback if enabled

      if not Is_Reraise then
         Excep.Num_Tracebacks := 0;
         Call_Chain (Excep);
      end if;

      --  WARNING : There should be no exception handler for this body
      --  because this would cause gigi to prepend a setup for a new
      --  jmpbuf to the sequence of statements. We would then always get
      --  this new buf in Jumpbuf_Ptr instead of the one for the exception
      --  we are handling, which would completely break the whole design
      --  of this procedure.

      --  If the jump buffer pointer is non-null, transfer control using it

      if Jumpbuf_Ptr /= Null_Address then
         builtin_longjmp (To_Jmpbuf_Address (Jumpbuf_Ptr), 1);

      --  Otherwise this is an unhandled exception

      else
         --  Call this hook so that GDB can insert a breakpoint on unhandled
         --  exceptions. This procedure has no other effect.

         Debug_Unhandled_Exception (E => SSL.Exception_Data_Ptr (E));

         --  Check whether there is any termination handler to be executed for
         --  the environment task, and execute it if needed.

         Task_Termination_Handler.all;

         --  Code to be executed for unhandled exceptions

         Last_Chance_Handler (Excep.all);
      end if;
   end Process_Exception;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception (E : Exception_Id; Message : String := "") is
      pragma Unreferenced (Message);
      --  This appears to be as early as we can start ignoring the "Message"
      --  parameter, since "Raise_Exception" is externally callable.
   begin
      Process_Exception (E);
   end Raise_Exception;

   ----------------------------
   -- Raise_Exception_Always --
   ----------------------------

   procedure Raise_Exception_Always
     (E       : Exception_Id;
      Message : String := "") renames Raise_Exception;

   -------------------------
   -- Raise_Program_Error --
   -------------------------

   procedure Raise_Program_Error (File : System.Address; Line : Integer) is
      pragma Unreferenced (File, Line);
   begin
      Process_Exception (Program_Error_Def'Access);
   end Raise_Program_Error;

   -------------------------
   -- Raise_Storage_Error --
   -------------------------

   procedure Raise_Storage_Error (File : System.Address; Line : Integer) is
      pragma Unreferenced (File, Line);
   begin
      Process_Exception (Storage_Error_Def'Access);
   end Raise_Storage_Error;

   ------------------------
   -- Reraise_Occurrence --
   ------------------------

   procedure Reraise_Occurrence (X : Exception_Occurrence) is
   begin
      Process_Exception (X.Id, Is_Reraise => True);
   end Reraise_Occurrence;

   -------------------------------
   -- Reraise_Occurrence_Always --
   -------------------------------

   procedure Reraise_Occurrence_Always (X : Exception_Occurrence)
     renames Reraise_Occurrence;

   ---------------------------------
   -- Reraise_Occurrence_No_Defer --
   ---------------------------------

   procedure Reraise_Occurrence_No_Defer (X : Exception_Occurrence)
     renames Reraise_Occurrence;

   ---------------------
   -- Save_Occurrence --
   ---------------------

   procedure Save_Occurrence
     (Target : out Exception_Occurrence;
      Source : Exception_Occurrence)
   is
   begin
      Target.Id             := Source.Id;
      Target.Num_Tracebacks := Source.Num_Tracebacks;

      Target.Tracebacks (1 .. Target.Num_Tracebacks) :=
        Source.Tracebacks (1 .. Target.Num_Tracebacks);
   end Save_Occurrence;

   --------------------------
   -- Code_Address_For_ZZZ --
   --------------------------

   --  This function gives us the end of the PC range for addresses
   --  within the exception unit itself. We hope that gigi/gcc keeps all the
   --  procedures in their original order!

   function Code_Address_For_ZZZ return System.Address is
   begin
      <<Start_Of_ZZZ>>
      return Start_Of_ZZZ'Address;
   end Code_Address_For_ZZZ;

end Ada.Exceptions;
