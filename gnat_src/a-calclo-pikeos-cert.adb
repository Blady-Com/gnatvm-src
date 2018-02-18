------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    A D A . C A L E N D A R . C L O C K                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

--  This is the PikeOS cert version of this package

separate (Ada.Calendar)

-----------
-- Clock --
-----------

function Clock return Time is
   --  VxWorks Time Definitions

   type P4_time_t is new Long_Long_Integer;

   type P4_e_t is new Integer;

   function p4_get_time_syscall (timep : not null access P4_time_t)
                                return P4_e_t;
   pragma Import (C, p4_get_time_syscall, "p4_get_time_syscall");
   --  Get the system time in nanosecond

   Timep  : aliased P4_time_t;
   Result : P4_e_t;

   Elapsed_Seconds : Duration;
   Elapsed_Days    : Time;

begin
   Result := p4_get_time_syscall (Timep'Unchecked_Access);
   pragma Assert (Result = 0);

   Elapsed_Seconds := Duration (Timep) / 10#1#E9;
   Elapsed_Days := Elapsed_Seconds / Secs_Per_Day;

   return Radix_Time + Elapsed_Days;
end Clock;
