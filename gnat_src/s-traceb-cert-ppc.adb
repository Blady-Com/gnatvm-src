------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2012, Free Software Foundation, Inc.         --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with System.Address_To_Access_Conversions;

--  The unit is a replacement of tracebak.c for PowerPC targets with
--  certification needs, providing an all Ada implementation with a very
--  simple parameterization scheme (see System.Traceback_Control below).

--  The general principle of operation is similar: assuming a simple memory
--  organization of the call stack frames with both a return address and a
--  back-link expected to be stored at a fixed offset from a given frame
--  pointer. For "main" calling "A" calling "B", something like:
--
--  SP        <------ stack memory, frames push this way                 TOP
--  |         <------ (decreasing addresses) on calls                      |
--  v                                                                      v
--  |<----- frame for B ----->|<----- frame for A ----->|< frame for main >|
--  .                         .                         .                  .
--  .    ------------------------  ------------------------                .
--  .    |                    . v  |                    . v                .
--  #====|====================#====|====================#==================#
--  # link | ... | ret@ | ... # link | ... | ret@ | ... # link ...         #
--  #===============|=========#===============|=========#==================#
--  .               v         .               v         .                  .
--  .      1 insn past the    .    1 insn past the      .                  .
--  .      call to B in A     .    call to A in main    .                  .

--  Computing a backtrace consists in walking up the link chains starting from
--  the current frame, latching the address of calls at each step and stopping
--  when the top frame is reached.

--  This is pretty much generic except for two ABI details that vary across
--  OS implementations: the frame pointer to return address offset, and
--  the characterization of the top-of-stack frame. We rely on a separate
--  Traceback_Control package to expose a consistent abstraction of those
--  differences.

with System.Traceback_Control; use System.Traceback_Control;

--  We expect Traceback_Control to expose:
--
--     function Return_Address_Offset return System.Address;
--     --  Relative to a given frame pointer, the constant offset at which the
--     --  return address for this frame is stored.
--
--     FP  RA_Offset
--      |----------->|
--      v            v
--     #=====================...
--     # link | ... | ret@ | ...
--     #=====================...
--
--  Then:
--
--     function Is_Topframe_Retaddr (Retaddr : System.Address) return boolean;
--     --  Whether the provided Retaddr signals a top-of-stack frame.

package body System.Traceback is

   package Addr is new System.Address_To_Access_Conversions (System.Address);

   package CTL renames System.Traceback_Control;

   -----------------
   -- Frame links --
   -----------------

   function Link_From (Frame : System.Address) return System.Address;
   --  For a given subprogram frame, return the address of the next
   --  frame in the call chain.

   pragma Inline (Link_From);

   function Link_From (Frame : System.Address) return System.Address is

      Frame_Link_Offset : constant := 0;
      --  Relative to a given frame pointer, this is the offset in bytes of
      --  the memory location where we always expect the address of the caller
      --  frame to be stored. This defines our notion of a frame pointer.

   begin
      return Addr.To_Pointer (Frame + Frame_Link_Offset).all;
   end Link_From;

   ---------------
   -- Frame PCs --
   ---------------

   function Retaddr_From (Frame : System.Address) return System.Address;
   --  Return the return address for a given frame

   pragma Inline (Retaddr_From);

   function Retaddr_From (Frame : System.Address) return System.Address is
   begin
      --  Here, the offset at which we expect to find the return address
      --  in the frame is part of control parameters.

      return Addr.To_Pointer (Frame + CTL.Return_Address_Offset).all;
   end Retaddr_From;

   ----------------------------------
   -- Identifying the Top of stack --
   ----------------------------------

   function Top_Of_Stack_At (Frame : System.Address) return Boolean;
   --  Whether the provided Frame pointer designates a call chain entry point.

   pragma Inline (Top_Of_Stack_At);

   function Top_Of_Stack_At (Frame : System.Address) return Boolean is
   begin
      --  Two indicators we expect are a null frame pointer (back-link from
      --  the previous frame) or a particular return address with variations
      --  across target ABIs. We add an extra alignment check only to prevent
      --  erroneous memory accesses in case something unexpected happens.

      return Frame = System.Null_Address
        or else Frame mod System.Address'Alignment /= 0
        or else CTL.Is_Topframe_Retaddr (Retaddr_From (Frame));
   end Top_Of_Stack_At;

   ------------------
   --  Call_Chain  --
   ------------------

   procedure Call_Chain
     (Traceback   : in out Ada.Exceptions.Traceback.Tracebacks_Array;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1) is

      pragma Assert (Return_Address_Offset mod System.Address'Alignment = 0);

      Retaddr_To_Call_Offset : constant := 4;
      --  Size of call instruction to subtract from return address to get the
      --  PC of the corresponding call instruction.

      Frame : System.Address;
      --  Frame being processed

      Last : Integer := Traceback'First - 1;
      --  Index of last traceback written to the buffer

      procedure Forced_Callee;
      --  Force save of return address of Call_Chain on PPC

      -------------------
      -- Forced_Callee --
      -------------------

      --  The PPC ABI has an unusual characteristic: the return address saved
      --  by a subprogram is located in its caller's frame, and the save
      --  operation only occurs if the function performs a call.

      --  To make Call_Chain able to consistently retrieve its own return
      --  address, we define Forced_Callee and call it.  This routine should
      --  never be inlined.

      procedure Forced_Callee is
         Dummy : aliased Integer := 0;
         pragma Volatile (Dummy);
         pragma Warnings (Off, Dummy);
         --  Force allocation of a frame. Dummy must be both volatile and
         --  referenced (achieved by declaring it aliased). Suppress warning
         --  that it could be declared a constant, and that pragma Volatile
         --  has no effect (it forces creation of the frame).
      begin
         null;
      end Forced_Callee;

      function builtin_frame_address (Level : Integer) return System.Address;
      pragma Import
        (Intrinsic, builtin_frame_address, "__builtin_frame_address");

   --  Start of processing for Call_Chain

   begin
      Forced_Callee;
      Len := 0;

      --  Start with the frame pointer for the current frame.

      Frame := builtin_frame_address (0);

      --  Skip the first Skip_Frames frames, as required by our spec. If we
      --  happen to hit the top of stack while doing so, just yield an empty
      --  trace. We expect this never to happen in regular circumstances of
      --  calls issued by the other parts of the runtime library.

      for J in 1 .. Skip_Frames + 1 loop
         if Top_Of_Stack_At (Frame) then
            return;
         end if;

         Frame := Link_From (Frame);
      end loop;

      pragma Assert (Frame /= System.Null_Address);

      --  Now walk up recording call addresses as we go. Stop when the top
      --  of stack is reached or when the provided trace buffer is full.
      --  Don't record addresses in the provided exclusion range, typically
      --  used to leave aside frames from our exception propagation internals.

      while not Top_Of_Stack_At (Frame)
        and then Last < Traceback'Last
        and then Len < Max_Len
      loop

         declare
            Call_Addr : constant System.Address
              := Retaddr_From (Frame) - Retaddr_To_Call_Offset;

         begin
            if Call_Addr not in Exclude_Min .. Exclude_Max then
               Last := Last + 1;
               Len := Len + 1;
               Traceback (Last) := Call_Addr;
            end if;

            Frame := Link_From (Frame);
         end;

         pragma Assert (Frame /= System.Null_Address);
      end loop;
   end Call_Chain;

end System.Traceback;
