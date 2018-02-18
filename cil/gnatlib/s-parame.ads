------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . P A R A M E T E R S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1999-2011, AdaCore                     --
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

--  This is the .NET/JVM specific version of System.Parameters

package System.Parameters is
   pragma Pure;
   pragma Elaborate_Body;

   ---------------------------------------
   -- Task And Stack Allocation Control --
   ---------------------------------------

   type Task_Storage_Size is new Integer;
   --  Type used in tasking units for task storage size

   type Size_Type is new Task_Storage_Size;
   --  Type used to provide task storage size to runtime

   Unspecified_Size : constant Size_Type := Size_Type'First;
   --  Value used to indicate that no size type is set

   subtype Percentage is Size_Type range -1 .. 100;
   Dynamic : constant Size_Type := -1;
   --  Secondary_Stack_Ratio is a constant between 0 and 100 wich
   --  determines the percentage of the allocate task stack that is
   --  used by the secondary stack (the rest being the primary stack).
   --  The special value of minus one indicates that the secondary
   --  stack is to be allocated from the heap instead.

   Sec_Stack_Percentage : constant Percentage := -1;
   --  This constant defines the handling of the secondary stack

   Sec_Stack_Dynamic : constant Boolean := Sec_Stack_Percentage = Dynamic;
   --  Convenient Boolean for testing for dynamic secondary stack

   function Default_Stack_Size return Size_Type;
   --  Default task stack size used if none is specified

   function Minimum_Stack_Size return Size_Type;
   --  Minimum task stack size permitted

   function Adjust_Storage_Size (Size : Size_Type) return Size_Type;
   --  Given the storage size stored in the TCB, return the Storage_Size
   --  value required by the RM for the Storage_Size attribute. The
   --  required adjustment is as follows:
   --
   --    when Size = Unspecified_Size, return Default_Stack_Size
   --    when Size < Minimum_Stack_Size, return Minimum_Stack_Size
   --    otherwise return given Size

   Default_Env_Stack_Size : constant Size_Type := 8_192_000;
   --  Assumed size of the environment task, if no other information
   --  is available. This value is used when stack checking is
   --  enabled and no GNAT_STACK_LIMIT environment variable is set.

   ----------------------------------------------
   -- Characteristics of types in Interfaces.C --
   ----------------------------------------------

   long_bits : constant := 64;
   --  Number of bits in type long and unsigned_long

   ptr_bits  : constant := Standard'Address_Size;
   subtype C_Address is System.Address;
   --  Number of bits in Interaces.C pointers, normally a standard address,
   --  except on 64-bit VMS where they are 32-bit addresses, for compatibility
   --  with legacy code.

   C_Malloc_Linkname : constant String := "__gnat_malloc";
   --  Name of runtime function used to allocate such a pointer

   ----------------------------------------------
   -- Behavior of Pragma Finalize_Storage_Only --
   ----------------------------------------------

   --  Garbage_Collected is a Boolean constant whose value indicates the
   --  effect of the pragma Finalize_Storage_Entry on a controlled type.

   --    Garbage_Collected = False

   --      The system releases all storage on program termination only,
   --      but not other garbage collection occurs, so finalization calls
   --      are ommitted only for outer level onjects can be omitted if
   --      pragma Finalize_Storage_Only is used.

   --    Garbage_Collected = True

   --      The system provides full garbage collection, so it is never
   --      necessary to release storage for controlled objects for which
   --      a pragma Finalize_Storage_Only is used.

   Garbage_Collected : constant Boolean := True;
   --  The storage mode for this system (release on program exit)

   ---------------------
   -- Tasking Profile --
   ---------------------

   --  In the following sections, constant parameters are defined to
   --  allow some optimizations within the tasking run time based on
   --  restrictions on the tasking features.

   ----------------------
   -- Locking Strategy --
   ----------------------

   Single_Lock : constant Boolean := False;
   --  Indicates whether a single lock should be used within the tasking
   --  run-time to protect internal structures. If True, a single lock
   --  will be used, meaning less locking/unlocking operations, but also
   --  more global contention. In general, Single_Lock should be set to
   --  True on single processor machines, and to False to multi-processor
   --  systems, but this can vary from application to application and also
   --  depends on the scheduling policy.

   ----------------
   -- Task Abort --
   ----------------

   No_Abort : constant Boolean := False;
   --  This constant indicates whether abort statements and asynchronous
   --  transfer of control (ATC) are disallowed. If set to True, it is
   --  assumed that neither construct is used, and the run time does not
   --  need to defer/undefer abort and check for pending actions at
   --  completion points. A value of True for No_Abort corresponds to:
   --  pragma Restrictions (No_Abort_Statements);
   --  pragma Restrictions (Max_Asynchronous_Select_Nesting => 0);

   ---------------------
   -- Task Attributes --
   ---------------------

   Default_Attribute_Count : constant := 4;
   --  Number of pre-allocated Address-sized task attributes stored in the
   --  task control block.

   --------------------
   -- Runtime Traces --
   --------------------

   Runtime_Traces : constant Boolean := False;
   --  This constant indicates whether the runtime outputs traces to a
   --  predefined output or not (True means that traces are output).
   --  See System.Traces for more details.

   -----------------------
   -- Task Image Length --
   -----------------------

   Max_Task_Image_Length : constant := 256;
   --  This constant specifies the maximum length of a task's image.

   ------------------------------
   -- Exception Message Length --
   ------------------------------

   Default_Exception_Msg_Max_Length : constant := 200;
   --  This constant specifies the default number of characters to allow
   --  in an exception message (200 is minimum required by RM 11.4.1(18)).

end System.Parameters;
