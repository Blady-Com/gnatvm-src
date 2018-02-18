------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--                     Copyright (C) 2009-2010, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Ravenscar version of this package for PikeOS

--  This package encapsulates all direct interfaces to OS services that are
--  needed by the tasking run-time (libgnarl).

package body System.OS_Interface is

   --  Sycalls defined in p4.h

   procedure p4_get_time_syscall (Res : Address);
   pragma Import (C, p4_get_time_syscall);

   function p4_sleep (Timeout : P4_timeout_t) return P4_e_t;
   pragma Import (C, p4_sleep);

   procedure p4_thread_yield;
   pragma Import (C, p4_thread_yield);

   function p4_my_uid return P4_uid_t;
   pragma Import (C, p4_my_uid);

   function p4_thread_stop (Tnum : P4_thr_t) return P4_e_t;
   pragma Import (C, p4_thread_stop);

   function p4_thread_resume (Tnum : P4_thr_t) return P4_e_t;
   pragma Import (C, p4_thread_resume);

   function p4_thread_ex_sched
     (Tnum     : P4_thr_t;
      Old_Prio : Address;
      Old_Tp   : Address;
      New_Prio : P4_prio_t;
      New_Tp   : P4_uint32_t)
     return P4_e_t;
   pragma Import (C, p4_thread_ex_sched);

   function p4_fast_set_prio (New_Prio : P4_prio_t) return P4_prio_t;
   pragma Import (C, p4_fast_set_prio);

   function p4_thread_get_attr (Tnum : P4_thr_t; Attr : Address) return P4_e_t;
   pragma Import (C, p4_thread_get_attr);

   function p4_my_thread return P4_thr_t;
   --  This function is documented as pseudo-syscall. The implementation is
   --  only provided by a C static inline function.

   function p4_thread_get_priority (Tnum : P4_thr_t; Prio : Address)
                                   return P4_e_t;
   --  This function is documented as pseudo-syscall. The implementation is
   --  only provided by a C static inline function.

   Max_Thread_Num : constant P4_thr_t := 512;
   --  Maximum number of threads that can be created

   type Thread_Id_Array is array (P4_thr_t range 0 .. Max_Thread_Num) of
     Thread_Id;
   All_Threads : Thread_Id_Array := (others => null);
   --  Array of tasks.  Used to implement Get_ATCB.
   --  ??? This duplicates system.tasking.debug.known_tasks.

   Next_Thread : P4_thr_t := 0;
   --  Number of the next task to be created

   ------------------------------
   --  p4_thread_get_priority  --
   ------------------------------

   function p4_thread_get_priority (Tnum : P4_thr_t; Prio : Address)
                                   return P4_e_t is
   begin
      return p4_thread_ex_sched (Tnum, Prio, Null_Address,
                                 P4_PRIO_KEEP, P4_TIMEPART_KEEP);
   end p4_thread_get_priority;

   -------------------
   --  p4_my_thread --
   -------------------

   function p4_my_thread return P4_thr_t is
      Uid : constant P4_uid_t := p4_my_uid;

   begin
      --  The lower 9 bits

      return Uid mod 512;
   end p4_my_thread;

   ----------------------
   --  Attach_Handler  --
   ----------------------

   procedure Attach_Handler
     (Handler : Interrupt_Handler;
      Id      : Interrupt_ID)
   is
   begin
      --  Not yet supported

      raise Program_Error;
   end Attach_Handler;

   ------------------
   --  Current_CPU --
   ------------------

   function Current_CPU return Multiprocessors.CPU is
   begin
      --  No multiprocessor support, always return the first CPU Id

      return Multiprocessors.CPU'First;
   end Current_CPU;

   -------------------------
   --  Current_Interrupt  --
   -------------------------

   function Current_Interrupt return Interrupt_ID is
   begin
      --  Not yet supported

      return No_Interrupt;
   end Current_Interrupt;

   -------------
   --  Clock  --
   -------------

   function Clock return Time is
      Res : P4_time_t;
   begin
      p4_get_time_syscall (Res'Address);
      return Res;
   end Clock;

   -------------------
   --  Delay_Until  --
   -------------------

   procedure Delay_Until (T : Time) is
      Res : P4_e_t;

   begin
      Res := p4_sleep (T + P4_TIMEOUT_ABSOLUTE);

      if Res = P4_E_BADTIMEOUT then

         --  ARM D.2.3 7/2 requires a yield, even if delay is in the past or 0

         p4_thread_yield;
      end if;
   end Delay_Until;

   -----------------
   --  Initialize --
   -----------------

   procedure Initialize
     (Environment_Thread : Thread_Id;
      Main_Priority      : System.Any_Priority) is
      Prev : P4_prio_t;
      pragma Unreferenced (Prev);

   begin
      --  The environment thread is the first thread

      pragma Assert (p4_my_thread = 0);
      pragma Assert (Next_Thread = 0);

      Environment_Thread.Num := 0;
      Environment_Thread.Base_Priority := Main_Priority;
      All_Threads (0) := Environment_Thread;

      Prev := p4_fast_set_prio (P4_prio_t (Main_Priority));

      --  Find the next available thread number. It should be 1 but the
      --  debugger stub (if enabled) creates two threads.

      loop
         Next_Thread := Next_Thread + 1;
         exit when p4_thread_get_attr (Next_Thread, Null_Address) = P4_E_STATE;
      end loop;
   end Initialize;

   ----------------------------------
   -- Initialize_Slave_Environment --
   ----------------------------------

   procedure Initialize_Slave_Environment (Environment_Thread : Thread_Id) is
      pragma Unreferenced (Environment_Thread);

   begin
      --  Multiprocessors are not supported on this target

      null;
   end Initialize_Slave_Environment;

   ---------------------
   --  Thread_Create  --
   ---------------------

   procedure Thread_Create
     (Id            : Thread_Id;
      Code          : System.Address;
      Arg           : System.Address;
      Priority      : System.Any_Priority;
      Base_CPU      : System.Multiprocessors.CPU_Range;
      Stack_Address : System.Address;
      Stack_Size    : System.Parameters.Size_Type)
   is
      pragma Unreferenced (Base_CPU);

      function Gnat_p4_thread_create
        (Num        : P4_thr_t;
         Prio       : P4_prio_t;
         Code       : Address;
         Arg        : Address;
         Stack      : Address;
         Stack_Size : System.Parameters.Size_Type) return P4_e_t;
      pragma Import (C, Gnat_p4_thread_create, "__gnat_p4_thread_create");
      --  Wrapper in C to make the implementation easier

      Status : P4_e_t;
   begin
      --  Be sure there is enough room in the task array
      pragma Assert (Next_Thread <= Max_Thread_Num);
      pragma Assert (Stack_Address /= Null_Address);

      All_Threads (Next_Thread) := Id;
      Id.Num := Next_Thread;

      --  No need to atomically increment Next_Thread as only the environmental
      --  task creates tasks, assuming a ravenscar implementation.

      Next_Thread := Next_Thread + 1;

      Status := Gnat_p4_thread_create
        (Id.Num, P4_prio_t (Priority), Code, Arg,
         Stack_Address, Stack_Size);
      pragma Assert (Status = P4_E_OK);
   end Thread_Create;

   -------------------
   --  Thread_Self  --
   -------------------

   function Thread_Self return Thread_Id is
      Id : constant P4_thr_t := p4_my_thread;
   begin
      return All_Threads (Id);
   end Thread_Self;

   ----------------
   --  Lwp_Self  --
   ----------------

   function Lwp_Self return System.Address is
   begin
      --  This magic value matches the tid returned by gdbstub (as tid 0 is
      --  reserved, the tids are shifted).

      return Address (p4_my_thread + 1);
   end Lwp_Self;

   ----------------
   --  Set_ATCB  --
   ----------------

   procedure Set_ATCB (ATCB : System.Address) is
      Id : constant P4_thr_t := p4_my_thread;
   begin
      All_Threads (Id).ATCB := ATCB;
   end Set_ATCB;

   ----------------
   --  Get_ATCB  --
   ----------------

   function Get_ATCB return System.Address is
      Id : constant P4_thr_t := p4_my_thread;
   begin
      return All_Threads (Id).ATCB;
   end Get_ATCB;

   --------------------
   --  Set_Priority  --
   --------------------

   procedure Set_Priority (Priority : System.Any_Priority) is
      Prev : P4_prio_t;
      pragma Unreferenced (Prev);
   begin
      Prev := p4_fast_set_prio (P4_prio_t (Priority));
   end Set_Priority;

   --------------------
   --  Get_Priority  --
   --------------------

   function Get_Priority  (Id : Thread_Id) return System.Any_Priority is
      Status : P4_e_t;
      Prio : P4_prio_t;
   begin
      Status := p4_thread_get_priority (Id.Num, Prio'Address);
      pragma Assert (Status = P4_E_OK);
      return Any_Priority (Prio);
   end Get_Priority;

   -------------
   --  Sleep  --
   -------------

   procedure Sleep is
      Status : P4_e_t;
   begin
      Status := p4_thread_stop (p4_my_thread);
      pragma Assert (Status = P4_E_OK);
   end Sleep;

   --------------
   --  Wakeup  --
   --------------

   procedure Wakeup (Id : Thread_Id) is
      Status : P4_e_t;
   begin
      Status := p4_thread_resume (Id.Num);
      pragma Assert (Status = P4_E_OK);
   end Wakeup;

   --------------------
   --  Get_Affinity  --
   --------------------

   function Get_Affinity (Id : Thread_Id) return Multiprocessors.CPU_Range is
      pragma Unreferenced (Id);

   begin
      --  No multiprocessor support, always return Not_A_Specific_CPU

      return Multiprocessors.Not_A_Specific_CPU;
   end Get_Affinity;

   ---------------
   --  Get_CPU  --
   ---------------

   function Get_CPU  (Id : Thread_Id) return Multiprocessors.CPU is
      pragma Unreferenced (Id);

   begin
      --  No multiprocessor support, always return the first CPU Id

      return Multiprocessors.CPU'First;
   end Get_CPU;

end System.OS_Interface;
