------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--  G N A T . T R A C E B A C K . S Y M B O L I C . M O D U L E _ N A M E   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

--  This is the Windows specific version of this package

with Interfaces.C; use Interfaces.C;

with System.Win32; use System.Win32;

separate (GNAT.Traceback.Symbolic)

package body Module_Name is

   use System;

   function Executable_Name return String;
   --  Returns the executable name as reported by argv[0]

   ---------------------
   -- Executable_Name --
   ---------------------

   function Executable_Name return String is
      type Argv_Array is array (0 .. 0) of chars_ptr;
      gnat_argv : access Argv_Array;
      pragma Import (C, gnat_argv, "gnat_argv");

      function locate_exec_on_path
        (Name : chars_ptr) return chars_ptr;
      pragma Import (C, locate_exec_on_path, "__gnat_locate_exec_on_path");

      chars  : chars_ptr := locate_exec_on_path (gnat_argv (0));
      Result : constant String := Value (chars);

   begin
      Free (chars);
      return Result;
   end Executable_Name;

   ---------
   -- Get --
   ---------

   function Get (Addr : access System.Address) return String is
      Res     : DWORD;
      hModule : aliased HANDLE;
      Path    : String (1 .. 1_024);
   begin
      if GetModuleHandleEx
        (GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS,
         Addr.all,
         hModule'Access) = Win32.TRUE
      then
         Res := GetModuleFileName (hModule, Path'Address, Path'Length);

         if CloseHandle (hModule) = Win32.FALSE then
            null;
         end if;

         if Res > 0 then
            return Path (1 .. Positive (Res));
         end if;
      end if;

      return Executable_Name;
   exception
      when others =>
         return Executable_Name;
   end Get;

   ------------------
   -- Is_Supported --
   ------------------

   function Is_Supported return Boolean is
   begin
      return True;
   end Is_Supported;

end Module_Name;
