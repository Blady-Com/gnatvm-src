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

--  This is the GNU/Linux specific version of this package

with Interfaces.C;              use Interfaces.C;

with System.Address_Operations; use System.Address_Operations;

separate (GNAT.Traceback.Symbolic)

package body Module_Name is

   use System;

   pragma Linker_Options ("-ldl");

   --  The principe is:
   --
   --  1. we get information about the module containing the address
   --
   --  2. we check that the full pathname is pointing to a shared library
   --
   --  3. for shared libraries, we return the non relocated address (so
   --     the absolute address in the shared library).
   --
   --  4. we also return the full pathname of the module containing this
   --     address.

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

      --  Dl_info record for Linux, used to get sym reloc offset

      type Dl_info is record
         dli_fname : chars_ptr;
         dli_fbase : Address;
         dli_sname : chars_ptr;
         dli_saddr : Address;
      end record;

      function dladdr
        (addr : Address; info : not null access Dl_info) return int;
      pragma Import (C, dladdr, "dladdr");
      --  This is a Linux extension and not POSIX

      function Is_Shared_Lib (info : not null access Dl_info) return Boolean;
      --  Returns True if a shared library

      -------------------
      -- Is_Shared_Lib --
      -------------------

      function Is_Shared_Lib (info : not null access Dl_info) return Boolean is

         EI_NIDENT : constant := 16;
         type u16 is mod 2 ** 16;

         --  Just declare the needed header information, we just need to
         --  read the type encoded into the second field.

         type Elf32_Ehdr is record
            e_ident : char_array (1 .. EI_NIDENT);
            e_type  : u16;
         end record;

         ET_DYN : constant := 3; -- A shared lib if e_type = ET_DYN

         Header : Elf32_Ehdr;
         for Header'Address use info.dli_fbase;

      begin
         return Header.e_type = ET_DYN;
      exception
         when others =>
            return False;
      end Is_Shared_Lib;

      info : aliased Dl_info;

   begin
      if dladdr (Addr.all, info'Access) /= 0 then
         --  If we have a shared library we need to adjust the address to
         --  be relative to the base address of the library.

         if Is_Shared_Lib (info'Access) then
            Addr.all := SubA (Addr.all, info.dli_fbase);
         end if;

         return Value (info.dli_fname);

      --  Not found, fallback to executable name

      else
         return Executable_Name;
      end if;

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
