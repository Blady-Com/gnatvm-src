------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--              S Y S T E M . S T A N D A R D _ L I B R A R Y               --
--                                                                          --
--                                 S p e c                                  --
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

--  This is the .NET/JVM specific version of package System.Standard_Library

--  It is much smaller than the corresponding package for other targets
--  since the Java Virtual Machine has exceptions built in and there is no
--  need to define type Exception_Data for JGNAT.

package System.Standard_Library is
   pragma Preelaborate;

   type Exception_Trace_Kind is
     (RM_Convention,
      --  No particular trace is requested, only unhandled exceptions
      --  in the environment task (following the RM) will be printed.
      --  This is the default behavior.

      Every_Raise,
      --  Denotes every possible raise event, either explicit or due to
      --  a specific language rule, within the context of a task or not.

      Unhandled_Raise
      --  Denotes the raise events corresponding to exceptions for which
      --  there is no user defined handler.
     );
   --  Provide a way to denote different kinds of automatic traces related
   --  to exceptions that can be requested.

   Exception_Trace : Exception_Trace_Kind := RM_Convention;
   pragma Atomic (Exception_Trace);
   --  By default, follow the RM convention

   procedure Abort_Undefer_Direct;
   pragma Inline (Abort_Undefer_Direct);
   --  A little procedure that just calls Abort_Undefer.all, for use in
   --  clean up procedures, which only permit a simple subprogram name.

   procedure Adafinal;
   --  Performs the Ada Runtime finalization the first time it is invoked.
   --  All subsequent calls are ignored.

end System.Standard_Library;
