------------------------------------------------------------------------------
--                                                                          --
--                   GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 S Y S T E M . T A S K _ P R I M I T I V E S              --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                     Copyright (C) 1998-2010, AdaCore                     --
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

--  This is a Java version of this package

--  This package provides low-level support for most tasking features

pragma Warnings (Off, "*does not correspond to*");

with Interfaces.Java.Lang.Object;

package System.Task_Primitives is
   pragma Preelaborate;

   type Lock is limited private;
   type Lock_Ptr is access all Lock;
   --  Should be used for implementation of protected objects

   type RTS_Lock is limited private;
   type RTS_Lock_Ptr is access all RTS_Lock;
   --  Should be used inside the runtime system. The difference between Lock
   --  and the RTS_Lock is that the later one serves only as a semaphore so
   --  that do not check for ceiling violations.

   type Suspension_Object is limited private;
   --  Should be used for the implementation of Ada.Synchronous_Task_Control

   type Task_Body_Access is access procedure;
   --  Pointer to the task body's entry point (or possibly a wrapper declared
   --  local to the GNARL).

   type Private_Data is limited private;
   --  Any information that the GNULLI needs maintained on a per-task basis.
   --  A component of this type is guaranteed to be included in the
   --  Ada_Task_Control_Block.

   subtype Task_Address is System.Address;
   --  In some versions of Task_Primitives, notably for VMS, Task_Address is
   --  the short version of address defined in System.Aux_DEC. To avoid
   --  dragging Aux_DEC into tasking packages a tasking specific subtype is
   --  defined here.

   Task_Address_Size : constant := Standard'Address_Size;
   --  The size of Task_Address

   Alternate_Stack_Size : constant := 0;
   --  No alternate signal stack is used on this platform

private

   type Private_Task_Serial_Number is mod 2 ** 64;
   --  Used to give each task a unique serial number

   type RTS_Lock is record
      Lock : Interfaces.Java.Lang.Object.Ref;
      Cond : Interfaces.Java.Lang.Object.Ref;
   end record;
   type Owner_ID is access all Integer;

   function New_Lock return Interfaces.Java.Lang.Object.Ref;
   pragma Import (C, New_Lock, "__gnat_new_lock");

   function New_Condition
     (Lock : Interfaces.Java.Lang.Object.Ref)
      return Interfaces.Java.Lang.Object.Ref;
   pragma Import (C, New_Condition, "__gnat_new_condition");

   procedure Get_Lock (Lock : Interfaces.Java.Lang.Object.Ref);
   pragma Import (C, Get_Lock, "__gnat_get_lock");

   procedure Release_Lock (Lock : Interfaces.Java.Lang.Object.Ref);
   pragma Import (C, Release_Lock, "__gnat_release_lock");

   procedure Await (Cond : Interfaces.Java.Lang.Object.Ref);
   pragma Import (C, Await, "__gnat_await");

   procedure Await_Timeout
     (Cond    : Interfaces.Java.Lang.Object.Ref;
      Timeout : Interfaces.Java.long;
      Nanos   : Interfaces.Java.int);
   pragma Import (C, Await_Timeout, "__gnat_await_timeout");

   procedure Signal (Cond : Interfaces.Java.Lang.Object.Ref);
   pragma Import (C, Signal, "__gnat_signal");

   type Lock is record
      L : aliased RTS_Lock;
      Ceiling : System.Any_Priority := System.Any_Priority'First;
      Saved_Priority : System.Any_Priority := System.Any_Priority'First;
      Owner : Owner_ID;
      Next  : Lock_Ptr;
      Level : Private_Task_Serial_Number := 0;
      Buddy : Owner_ID;
      Frozen : Boolean := False;
   end record;

   type Suspension_Object is record
      State : Boolean;
      pragma Atomic (State);
      --  Boolean that indicates whether the object is open. This field is
      --  marked Atomic to ensure that we can read its value without locking
      --  the access to the Suspension_Object.

      Waiting : Boolean;
      --  Flag indicating if there is a task already suspended on this object

      L : aliased RTS_Lock;
      --  Protection/Condition variable for ensuring mutual exclusion on the
      --  Suspension_Object and used to queue threads until condition is
      --  signalled
   end record;

   type Private_Data is record
      Thread : Interfaces.Java.Lang.Object.Ref;
      pragma Atomic (Thread);

      L : aliased RTS_Lock;

      Active_Priority : System.Any_Priority := System.Any_Priority'First;
      --  Simulated active priority, used iff Priority_Ceiling_Support is True

      Locking : Lock_Ptr;
      Locks   : Lock_Ptr;
      Wakeups : Natural := 0;
   end record;

end System.Task_Primitives;
