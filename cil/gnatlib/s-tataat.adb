------------------------------------------------------------------------------
--                                                                          --
--                   GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--         S Y S T E M . T A S K I N G . T A S K _ A T T R I B U T E S      --
--                                                                          --
--                                  B o d y                                 --
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

--  This is a dummy version of this package.
--  Currently it is used by the .NET/JVM targets

package body System.Tasking.Task_Attributes is

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Instance) is
      pragma Unreferenced (X);
   begin
      null;
   end Finalize;

   -------------------------
   -- Finalize Attributes --
   -------------------------

   procedure Finalize_Attributes (T : Task_Id) is
      pragma Unreferenced (T);
   begin
      null;
   end Finalize_Attributes;

   ---------------------------
   -- Initialize Attributes --
   ---------------------------

   procedure Initialize_Attributes (T : Task_Id) is
      pragma Unreferenced (T);
   begin
      null;
   end Initialize_Attributes;

end System.Tasking.Task_Attributes;
