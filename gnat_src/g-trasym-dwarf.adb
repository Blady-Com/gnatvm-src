------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             G N A T . T R A C E B A C K . S Y M B O L I C                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2013, AdaCore                     --
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

--  Run-time symbolic traceback support for targets using DWARF debug data

with Ada.Exceptions.Traceback; use Ada.Exceptions.Traceback;

with Interfaces.C.Strings; use Interfaces.C.Strings;

with System.Dwarf_Lines; use System.Dwarf_Lines;

package body GNAT.Traceback.Symbolic is

   package Module_Name is

      function Get (Addr : access System.Address) return String;
      --  Returns the module name for the given address, Addr may be updated
      --  to be set relative to a shared library. This depends on the platform.

      function Is_Supported return Boolean;
      --  Returns True if Module_Name is supported, so if the traceback is
      --  supported for shared libraries.

   end Module_Name;

   package body Module_Name is separate;

   function Module_Symbolic_Traceback
     (Module_Name : String;
      Traceback   : Tracebacks_Array) return String;
   --  Returns the Traceback for a given module or an empty string if problems
   --  occurred (like not finding the module containing the DWARF information).

   -------------------------------
   -- Module_Symbolic_Traceback --
   -------------------------------

   function Module_Symbolic_Traceback
     (Module_Name : String;
      Traceback   : Tracebacks_Array) return String
   is
      C : Dwarf_Context (In_Exception => True);

   --  Start of processing for Module_Symbolic_Traceback

   begin
      Open (Module_Name, C);

      --  If a module can't be opened just return an empty string, we
      --  just cannot give more information in this case.

      if not Is_Open (C) then
         return "";
      end if;

      declare
         Result : constant String :=
                    Symbolic_Traceback
                      (C, Traceback,
                       Suppress_Hex => Symbolic.Module_Name.Is_Supported);
      begin
         Close (C);

         if Symbolic.Module_Name.Is_Supported then
            return '[' & Module_Name & ']' & ASCII.LF & Result;
         else
            return Result;
         end if;
      end;

      --  We must not allow an unhandled exception here, since this function
      --  may be installed as a decorator for all automatic exceptions.

   exception
      when others =>
         return "";
   end Module_Symbolic_Traceback;

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   function Symbolic_Traceback (Traceback : Tracebacks_Array) return String is

      TB : Tracebacks_Array (Traceback'Range);
      --  A partial copy of the possibly relocated traceback addresses. These
      --  addresses gets relocated for GNU/Linux shared library for example.
      --  This gets done in the Get_Module_Name routine.

   --  Start of processing for Symbolic_Traceback

   begin
      if Traceback'Length = 0 then
         return "";
      end if;

      declare
         Addr   : aliased System.Address := Traceback (Traceback'First);
         M_Name : constant String := Module_Name.Get (Addr'Access);
         Pos    : Positive;

      begin
         TB (TB'First) := Addr;

         Pos := TB'First + 1;

         Same_Module : loop
            exit Same_Module when Pos > Traceback'Last;

            --  Get address to check for corresponding module name

            Addr := Traceback (Pos);

            exit Same_Module when Module_Name.Get (Addr'Access) /= M_Name;

            --  Copy the possibly relocated address into TB

            TB (Pos) := Addr;

            Pos := Pos + 1;
         end loop Same_Module;

         return Module_Symbolic_Traceback
           (M_Name, TB (TB'First .. Pos - 1))
            & Symbolic_Traceback (Traceback (Pos .. Traceback'Last));
      end;
   end Symbolic_Traceback;

   function Symbolic_Traceback (E : Exception_Occurrence) return String is
   begin
      return Symbolic_Traceback (Tracebacks (E));
   end Symbolic_Traceback;

end GNAT.Traceback.Symbolic;
