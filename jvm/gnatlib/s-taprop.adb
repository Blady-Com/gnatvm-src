------------------------------------------------------------------------------
--                                                                          --
--                   GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                     Copyright (C) 1998-2011, AdaCore                     --
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

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with System.Tasking.Debug;
--  used for Known_Tasks

with Interfaces.Java.Lang.Object;
with Interfaces.Java.Lang.Thread;

with System.OS_Primitives;
--  used for Delay_Modes

with System.Soft_Links;
--  used for Abort_Defer/Undefer

--  We use System.Soft_Links instead of System.Tasking.Initialization
--  because the later is a higher level package that we shouldn't depend on.
--  For example when using the restricted run time, it is replaced by
--  System.Tasking.Restricted.Stages.

with Ada.Unchecked_Conversion;

package body System.Task_Primitives.Operations is

   use Tasking;
   use OS_Interface;
   use Parameters;
   use Tasking.Debug;
   use OS_Primitives;

   use Interfaces.Java;
   use Interfaces.Java.Lang.Object;
   use Interfaces.Java.Lang.Thread;

   package IJL_Thread renames Interfaces.Java.Lang.Thread;
   package SSL renames System.Soft_Links;

   ----------------
   -- Local Data --
   ----------------

   Environment_Task_Id : Task_Id;
   --  A variable to hold Task_Id for the environment task.
   --  If we use this variable to get the Task_Id, we need the following
   --  ATCB_Key only for non-Ada threads.

   Single_RTS_Lock : aliased RTS_Lock;
   --  This is a lock to allow only one thread of control in the RTS at
   --  a time; it is used to execute in mutual exclusion from all other tasks.
   --  Used mainly in Single_Lock mode, but also to protect All_Tasks_List

   Priority_Ceiling_Emulation : constant Boolean := True;
   --  controls whether we emulate priority ceiling locking

   ----------------------------------
   -- ATCB allocation/deallocation --
   ----------------------------------

   package body ATCB_Allocation is

      ---------------
      -- Free_ATCB --
      ---------------

      procedure Free_ATCB (T : Task_Id) is
         pragma Unreferenced (T);
      begin
         null;
      end Free_ATCB;

      --------------
      -- New_ATCB --
      --------------

      function New_ATCB (Entry_Num : Task_Entry_Index) return Task_Id is
      begin
         return new Ada_Task_Control_Block (Entry_Num);
      end New_ATCB;

   end ATCB_Allocation;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function To_Task_Id is new
     Ada.Unchecked_Conversion (System.Address, Task_Id);

   function To_Address is new
     Ada.Unchecked_Conversion (Task_Id, System.Address);

   function "+" is new Ada.Unchecked_Conversion (Thread_Id, Lang.Thread.Ref);
   function "+" is new Ada.Unchecked_Conversion (Lang.Object.Ref, Thread_Id);
   function "+" is new Ada.Unchecked_Conversion
     (Lang.Thread.Ref_Class, Lang.Object.Ref);

   subtype int is Interfaces.Java.int;
   subtype long is Interfaces.Java.long;

   procedure Split_MS_NS
     (Rel_Time : Duration;
      MS       : out long;
      NS       : out int);
   --  Split up Rel_Time into pair of values suitable for
   --  use with Java.Lang.Threads.sleep
   --  Rel_Time must be null or positive.

   procedure Split_MS_NS
     (Rel_Time : Duration;
      MS       : out long;
      NS       : out int)
   is
   begin
      pragma Assert (Rel_Time >= 0.0);

      --  The fractional part of Java_Time, Java_Time - IP, is the number of
      --  nanoseconds in Rel_Time, divided by one million. So, NS is equal
      --  to that number, multiplied by 1E6. As we want an integer number
      --  of nanoseconds, we then calculate its integer part.

      MS := long (Rel_Time * 1.0E3);
      NS := int ((Rel_Time - Duration (Long_Float (MS) / 1.0E3)) * 1.0E9);
   end Split_MS_NS;

   ----------
   -- Self --
   ----------

   function Self return Task_Id is
      T : Task_Id;
   begin
      T := To_Task_Id (Get_Self_Id);
      pragma Assert (T /= Null_Task);
      return T;
   end Self;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   --  Note: mutexes and cond_variables needed per-task basis are
   --        initialized in Intialize_TCB and the Storage_Error is
   --        handled. Other mutexes (such as RTS_Lock, Memory_Lock...)
   --        used in RTS is initialized before any status change of RTS.
   --        Therefore rasing Storage_Error in the following routines
   --        should be able to be handled safely.

   procedure Initialize_Lock
     (Prio : System.Any_Priority;
      L    : not null access Lock)
   is
   begin
      if Priority_Ceiling_Emulation then
         L.Ceiling := Prio;
      end if;

      L.L.Lock := New_Lock;
      L.L.Cond := New_Condition (L.L.Lock);
   end Initialize_Lock;

   procedure Initialize_Lock
     (L : not null access RTS_Lock; Level : Lock_Level)
   is
      pragma Unreferenced (Level);
   begin
      L.Lock := New_Lock;
      L.Cond := New_Condition (L.Lock);
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : not null access Lock) is
   begin
      L.L.Lock := null;
      L.L.Cond := null;
   end Finalize_Lock;

   procedure Finalize_Lock (L : not null access RTS_Lock) is
   begin
      L.Lock := null;
      L.Cond := null;
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   procedure Write_Lock
     (L : not null access Lock; Ceiling_Violation : out Boolean)
   is
      Self_ID : constant Task_Id := Self;

   begin
      if Priority_Ceiling_Emulation then
         if Self_ID.Common.LL.Active_Priority > L.Ceiling then
            Ceiling_Violation := True;
            return;
         end if;

         L.Saved_Priority := Self_ID.Common.LL.Active_Priority;

         if Self_ID.Common.LL.Active_Priority < L.Ceiling then
            Self_ID.Common.LL.Active_Priority := L.Ceiling;
         end if;

         Get_Lock (L.L.Lock);
         Ceiling_Violation := False;

      else
         Get_Lock (L.L.Lock);
         Ceiling_Violation := False;
      end if;
   end Write_Lock;

   procedure Write_Lock
     (L           : not null access RTS_Lock;
      Global_Lock : Boolean := False)
   is
   begin
      if not Single_Lock or else Global_Lock then
         Get_Lock (L.Lock);
      end if;
   end Write_Lock;

   procedure Write_Lock (T : Task_Id) is
   begin
      if not Single_Lock then
         Get_Lock (T.Common.LL.L.Lock);
      end if;
   end Write_Lock;

   ---------------
   -- Read_Lock --
   ---------------

   procedure Read_Lock
     (L : not null access Lock; Ceiling_Violation : out Boolean) is
   begin
      Write_Lock (L, Ceiling_Violation);
   end Read_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : not null access Lock) is
      Self_ID : constant Task_Id := Self;

   begin
      if Priority_Ceiling_Emulation then
         Release_Lock (L.L.Lock);

         if Self_ID.Common.LL.Active_Priority > L.Saved_Priority then
            Self_ID.Common.LL.Active_Priority := L.Saved_Priority;
         end if;

      else
         Release_Lock (L.L.Lock);
      end if;
   end Unlock;

   procedure Unlock
     (L : not null access RTS_Lock; Global_Lock : Boolean := False) is
   begin
      if not Single_Lock or else Global_Lock then
         Release_Lock (L.Lock);
      end if;
   end Unlock;

   procedure Unlock (T : Task_Id) is
   begin
      if not Single_Lock then
         Release_Lock (T.Common.LL.L.Lock);
      end if;
   end Unlock;

   ---------------------
   -- Monotonic_Clock --
   ---------------------

   --  For the time delay implementation, we need to make sure we
   --  achieve following criteria:

   --  1) We have to delay at least for the amount requested.
   --  2) We have to give up CPU even though the actual delay does not
   --     result in blocking.
   --  3) Except for restricted run-time systems that do not support
   --     ATC or task abort, the delay must be interrupted by the
   --     abort_task operation.
   --  4) The implementation has to be efficient so that the delay overhead
   --     is relatively cheap.
   --  (1)-(3) are Ada requirements. Even though (2) is an Annex-D
   --     requirement we still want to provide the effect in all cases.
   --     The reason is that users may want to use short delays to implement
   --     their own scheduling effect in the absence of language provided
   --     scheduling policies.

   function Monotonic_Clock return Duration
     renames System.OS_Primitives.Monotonic_Clock;

   -------------------
   -- RT_Resolution --
   -------------------

   function RT_Resolution return Duration is
   begin
      return 10#1.0#E-6;
   end RT_Resolution;

   -----------
   -- Yield --
   -----------

   procedure Yield (Do_Yield : Boolean := True) is
   begin
      if Do_Yield then
         Lang.Thread.Yield;
      end if;
   end Yield;

   -----------------
   -- Set_Ceiling --
   -----------------

   procedure Set_Ceiling
     (L : not null access Lock; Prio : System.Any_Priority)
   is
      pragma Unreferenced (L, Prio);
   begin
      null;
   end Set_Ceiling;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (T    : Task_Id;
      Prio : System.Any_Priority;
      Loss_Of_Inheritance : Boolean := False)
   is
      pragma Unreferenced (Loss_Of_Inheritance);

      --  First : constant System.Priority := System.Priority'First;
      --  Last  : constant System.Priority := System.Priority'Last;

   begin
      T.Common.Current_Priority := Prio;

      if Priority_Ceiling_Emulation then
         if T.Common.LL.Active_Priority < Prio then
            T.Common.LL.Active_Priority := Prio;
         end if;
      end if;

      --  if Prio <= Last then
      --     Set_Priority
      --       (+T.Common.LL.Thread,
      --        int (MIN_PRIORITY + (MAX_PRIORITY - 1 - MIN_PRIORITY) *
      --             (Prio - First) / (Last - First)));
      --  else
      --     --  Interrupt_Priority
      --     Set_Priority (+T.Common.LL.Thread, int (MAX_PRIORITY));
      --  end if;
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : Task_Id) return System.Any_Priority is
   begin
      return T.Common.Current_Priority;
   end Get_Priority;

   ----------------
   -- Enter_Task --
   ----------------

   procedure Enter_Task (Self_ID : Task_Id) is
   begin
      Self_ID.Common.LL.Thread := +Current_Thread;

      Lock_RTS;

      for J in Known_Tasks'Range loop
         if Known_Tasks (J) = null then
            Known_Tasks (J) := Self_ID;
            Self_ID.Known_Tasks_Index := J;
            exit;
         end if;
      end loop;

      Unlock_RTS;
   end Enter_Task;

   -------------------
   -- Is_Valid_Task --
   -------------------

   function Is_Valid_Task return Boolean is
   begin
      return False;
   end Is_Valid_Task;

   -----------------------------
   -- Register_Foreign_Thread --
   -----------------------------

   function Register_Foreign_Thread return Task_Id is
   begin
      return null;
   end Register_Foreign_Thread;

   --------------------
   -- Initialize_TCB --
   --------------------

   procedure Initialize_TCB (Self_ID : Task_Id; Succeeded : out Boolean) is
   begin
      Self_ID.Common.LL.L.Lock := New_Lock;
      Self_ID.Common.LL.L.Cond := New_Condition (Self_ID.Common.LL.L.Lock);
      Succeeded := True;
   end Initialize_TCB;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (T          : Task_Id;
      Wrapper    : System.Address;
      Stack_Size : System.Parameters.Size_Type;
      Priority   : System.Any_Priority;
      Succeeded  : out Boolean)
   is
      pragma Unreferenced (Wrapper, Stack_Size);

      Thread : Thread_Id;

      First  : constant System.Priority := System.Priority'First;
      Last   : constant System.Priority := System.Priority'Last;

   begin
      --  We are forced to ignore Stack_Size

      pragma Assert (T.Common.Activator /= null);

      Thread := System.OS_Interface.new_Thread (To_Address (T));

      --  ????
      --  Consider modifying GNULL to allow passing in the
      --  task name as the thread name.
      --  This is not essential, but might be of some value if there is a
      --  thread-based debugger.

      --  All Ada tasks, except for the environment task, should be
      --  daemons, since Ada enforces its own program termination rules.
      --  Likewise, we cannot use Thread.Join, or Thread_Group.

      Set_Daemon (+Thread, True);

      if Priority <= Last then
         Set_Priority
           (+Thread,
            int (MIN_PRIORITY + (IJL_Thread.MAX_PRIORITY - 1 - MIN_PRIORITY) *
                 (Priority - First) / (Last - First)));
      else
         --  Interrupt_Priority
         Set_Priority (+Thread, int (IJL_Thread.MAX_PRIORITY));
      end if;

      --  Start the newly created thread

      Start (+Thread);

      Succeeded := True;

      --  This presumes that notification of failure of thread creation
      --  raises a Java exception and that is automatically propagated
      --  as an Ada exception.

   exception
      when others =>
         --  We generally do not allow for exception propagation with
         --  the runtime system.

         Succeeded := False;
   end Create_Task;

   ------------------
   -- Finalize_TCB --
   ------------------

   procedure Finalize_TCB (T : Task_Id) is
   begin
      T.Common.LL.Thread := null;

      if T.Known_Tasks_Index /= -1 then
         Known_Tasks (T.Known_Tasks_Index) := null;
      end if;
   end Finalize_TCB;

   ---------------
   -- Exit_Task --
   ---------------

   --  This procedure must be called with abort deferred.
   --  It can no longer call Self or access
   --  the current task's ATCB, since the ATCB has been deallocated.

   procedure Exit_Task is
   begin
      null;
      --  Trust the JVM to stop the thread when it reaches the
      --  end of its body.
   end Exit_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_Id) is
   begin
      pragma Assert (T /= Self);

      --  ????
      --  It is tempting to call Java.Lang.Thread.stop, to raise an
      --  asynchronous exception, here.  However, we don't (yet?) know
      --  of a way to defer the effect of that.  Therefore, we assume
      --  for now that abort will need to be synchronous only.

      null;
   end Abort_Task;

   -----------
   -- Sleep --
   -----------

   procedure Sleep (Self_ID : Task_Id; Reason  : Task_States) is
      pragma Unreferenced (Reason);

   begin
      pragma Assert (Self_ID = Self);

      if Single_Lock then
         Await (Single_RTS_Lock.Cond);
      else
         Await (Self_ID.Common.LL.L.Cond);
      end if;
   end Sleep;

   -----------------
   -- Timed_Sleep --
   -----------------

   procedure Timed_Sleep
     (Self_ID  : Task_Id;
      Time     : Duration;
      Mode     : Delay_Modes;
      Reason   : System.Tasking.Task_States;
      Timedout : out Boolean;
      Yielded  : out Boolean)
   is
      pragma Unreferenced (Reason);

      Check_Time  : constant Duration := Monotonic_Clock;
      Abs_Time    : Duration;
      Request_MS  : long;
      Request_NS  : int;

   begin
      Timedout := True;
      Yielded  := False;

      if Mode = Relative then
         Abs_Time := Time + Check_Time;
      else
         Abs_Time := Time;
      end if;

      if Abs_Time > Check_Time then
         if Self_ID.Pending_ATC_Level >= Self_ID.ATC_Nesting_Level then
            Split_MS_NS (Abs_Time - Check_Time, Request_MS, Request_NS);

            if Single_Lock then
               Await_Timeout (Single_RTS_Lock.Cond, Request_MS, Request_NS);
            else
               Await_Timeout
                 (Self_ID.Common.LL.L.Cond, Request_MS, Request_NS);
            end if;

            if Abs_Time > Monotonic_Clock then
               Timedout := False;
            end if;
         end if;
      end if;
   end Timed_Sleep;

   -----------------
   -- Timed_Delay --
   -----------------

   procedure Timed_Delay
     (Self_ID  : Task_Id;
      Time     : Duration;
      Mode     : Delay_Modes)
   is
      Check_Time  : Duration := Monotonic_Clock;
      Abs_Time    : Duration;
      Request_MS  : long;
      Request_NS  : int;

   begin
      pragma Assert (Self_ID = Self);

      if Single_Lock then
         Lock_RTS;
      end if;

      Write_Lock (Self_ID);

      if Mode = Relative then
         Abs_Time := Time + Check_Time;
      else
         Abs_Time := Time;
      end if;

      if Abs_Time > Check_Time then
         Self_ID.Common.State := Delay_Sleep;

         loop
            exit when Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level;

            Split_MS_NS (Abs_Time - Check_Time, Request_MS, Request_NS);

            if Single_Lock then
               Await_Timeout (Single_RTS_Lock.Cond, Request_MS, Request_NS);
            else
               Await_Timeout
                 (Self_ID.Common.LL.L.Cond, Request_MS, Request_NS);
            end if;

            Check_Time := Monotonic_Clock;

            exit when Abs_Time <= Check_Time;
         end loop;

         Self_ID.Common.State := Runnable;
      end if;

      Unlock (Self_ID);

      if Single_Lock then
         Unlock_RTS;
      end if;

      Lang.Thread.Yield;
   end Timed_Delay;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : Task_Id; Reason : Task_States) is
      pragma Unreferenced (Reason);
   begin
      Signal (T.Common.LL.L.Cond);
   end Wakeup;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (S : in out Suspension_Object) is
   begin
      --  Initialize internal state. It is always initialized to False (ARM
      --  D.10 par. 6).

      S.State := False;
      S.Waiting := False;

      --  Initialize internal monitor

      S.L.Lock := New_Lock;
      S.L.Cond := New_Condition (S.L.Lock);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (S : in out Suspension_Object) is
   begin
      --  Destroy internal mutex

      S.L.Lock := null;
      S.L.Cond := null;
   end Finalize;

   -------------------
   -- Current_State --
   -------------------

   function Current_State (S : Suspension_Object) return Boolean is
   begin
      --  We do not want to use lock on this read operation. State is marked
      --  as Atomic so that we ensure that the value retrieved is correct.

      return S.State;
   end Current_State;

   ---------------
   -- Set_False --
   ---------------

   procedure Set_False (S : in out Suspension_Object) is
   begin
      SSL.Abort_Defer.all;

      Get_Lock (S.L.Lock);
      S.State := False;
      Release_Lock (S.L.Lock);

      SSL.Abort_Undefer.all;
   end Set_False;

   --------------
   -- Set_True --
   --------------

   procedure Set_True (S : in out Suspension_Object) is
   begin
      SSL.Abort_Defer.all;

      Get_Lock (S.L.Lock);

      --  If there is already a task waiting on this suspension object then
      --  we resume it, leaving the state of the suspension object to False,
      --  as it is specified in ARM D.10 par. 9. Otherwise, it just leaves
      --  the state to True.

      if S.Waiting then
         S.Waiting := False;
         S.State := False;

         Signal (S.L.Cond);
      else
         S.State := True;
      end if;

      Release_Lock (S.L.Lock);

      SSL.Abort_Undefer.all;
   end Set_True;

   ------------------------
   -- Suspend_Until_True --
   ------------------------

   procedure Suspend_Until_True (S : in out Suspension_Object) is
   begin
      SSL.Abort_Defer.all;

      Get_Lock (S.L.Lock);

      --  Program_Error must be raised upon calling Suspend_Until_True if
      --  another task is already waiting on that suspension object
      --  (RM D.10(10)).

      if S.Waiting then
         Release_Lock (S.L.Lock);

         SSL.Abort_Undefer.all;

         raise Program_Error;
      end if;

      --  Suspend the task if the state is False. Otherwise, the task continues
      --  its execution, and the state of the suspension object is set to False
      --  (RM D.10(9)).

      if S.State then
         S.State := False;

         Release_Lock (S.L.Lock);

         SSL.Abort_Undefer.all;

      else
         S.Waiting := True;

         --  Release the mutex before sleeping

         Release_Lock (S.L.Lock);

         SSL.Abort_Undefer.all;

         Await (S.L.Cond);
      end if;
   end Suspend_Until_True;

   ------------
   -- Checks --
   ------------

   --  Dummy versions

   function Check_Exit (Self_ID : Task_Id) return Boolean is
      pragma Unreferenced (Self_ID);
   begin
      return True;
   end Check_Exit;

   function Check_No_Locks (Self_ID : Task_Id) return Boolean is
      pragma Unreferenced (Self_ID);
   begin
      return True;
   end Check_No_Locks;

   ----------------------
   -- Environment_Task --
   ----------------------

   function Environment_Task return Task_Id is
   begin
      return Environment_Task_Id;
   end Environment_Task;

   --------------
   -- Lock_RTS --
   --------------

   procedure Lock_RTS is
   begin
      Write_Lock (Single_RTS_Lock'Access, Global_Lock => True);
   end Lock_RTS;

   ----------------
   -- Unlock_RTS --
   ----------------

   procedure Unlock_RTS is
   begin
      Unlock (Single_RTS_Lock'Access, Global_Lock => True);
   end Unlock_RTS;

   -----------------
   -- Stack_Guard --
   -----------------

   --  Trust the JVM for stack checking

   procedure Stack_Guard (T : ST.Task_Id; On : Boolean) is
      pragma Unreferenced (T, On);
   begin
      null;
   end Stack_Guard;

   -------------------
   -- Get_Thread_Id --
   -------------------

   function Get_Thread_Id (T : System.Tasking.Task_Id) return Thread_Id is
   begin
      return +T.Common.LL.Thread;
   end Get_Thread_Id;

   ------------------
   -- Suspend_Task --
   ------------------

   function Suspend_Task
     (T           : ST.Task_Id;
      Thread_Self : Thread_Id) return Boolean
   is
      pragma Unreferenced (T, Thread_Self);
   begin
      return False;
   end Suspend_Task;

   -----------------
   -- Resume_Task --
   -----------------

   function Resume_Task
     (T           : ST.Task_Id;
      Thread_Self : Thread_Id) return Boolean
   is
      pragma Unreferenced (T, Thread_Self);
   begin
      return False;
   end Resume_Task;

   --------------------
   -- Stop_All_Tasks --
   --------------------

   procedure Stop_All_Tasks is
   begin
      null;
   end Stop_All_Tasks;

   ---------------
   -- Stop_Task --
   ---------------

   function Stop_Task (T : ST.Task_Id) return Boolean is
      pragma Unreferenced (T);
   begin
      return False;
   end Stop_Task;

   -------------------
   -- Continue_Task --
   -------------------

   function Continue_Task (T : ST.Task_Id) return Boolean is
      pragma Unreferenced (T);
   begin
      return False;
   end Continue_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : ST.Task_Id) is
   begin
      Environment_Task_Id := Environment_Task;

      --  Initialize the lock used to synchronize chain of all ATCBs

      Initialize_Lock (Single_RTS_Lock'Access, RTS_Lock_Level);

      Enter_Task (Environment_Task_Id);
   end Initialize;

   -----------------------
   -- Set_Task_Affinity --
   -----------------------

   procedure Set_Task_Affinity (T : ST.Task_Id) is
      pragma Unreferenced (T);

   begin
      --  Setting task affinity is not supported by the underlying system

      null;
   end Set_Task_Affinity;

end System.Task_Primitives.Operations;
