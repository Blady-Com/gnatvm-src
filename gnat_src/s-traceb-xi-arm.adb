------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2013, Free Software Foundation, Inc.         --
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

--  This is the bare board version of this package for ARM targets
with System.Machine_Code; use System.Machine_Code;

package body System.Traceback is

   function Return_Address (Level : Natural)  return System.Address;
   pragma Import (Intrinsic, Return_Address, "__builtin_return_address");

   ----------------
   -- Call_Chain --
   ----------------

   procedure Call_Chain
     (Traceback   : in out Ada.Exceptions.Traceback.Tracebacks_Array;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1)
   is
   begin
      Len := 0;

      --  On ARM, no traceback is possible without extra unwind information,
      --  so we can at most return our own address and that of our caller.

      for J in 1 .. Max_Len loop
         case J + Skip_Frames - 1 is
            when 0 => Asm ("mov  %0,pc", Outputs => Address'Asm_Output
                           ("=r", Traceback (Traceback'First + Len)),
                            Volatile => True);
            when 1 => Traceback (Traceback'First + Len) := Return_Address (0);
            when others => exit;
         end case;

         if Traceback (Len + 1) not in Exclude_Min .. Exclude_Max then
            Len := Len + 1;
         end if;
      end loop;
   end Call_Chain;

end System.Traceback;
