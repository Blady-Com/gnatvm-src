------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               S Y S T E M . P U R E _ E X C E P T I O N S                --
--                                                                          --
--                                 S p e c                                  --
--                              (.NET Version)                              --
--                                                                          --
--                     Copyright (C) 2000-2010, AdaCore                     --
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

--  This package provides an interface for raising predefined exceptions
--  with an exception message. It can be used from Pure units.

--  Note: the reason that a separate version of this is required is that
--  Raise_Exception is defined in GNAT_libc and therefore needs to specify the
--  language as C not in Ada in Import.
--  In addition, Exception_Type definition needs to be changed to map a
--  System.Object.

with System;

package System.Pure_Exceptions is
   pragma Pure;

   type Exception_Type is private;
   --  Type used to specify which exception to raise

   CE : constant Exception_Type;  -- Constraint_Error
   PE : constant Exception_Type;  -- Program_Error
   SE : constant Exception_Type;  -- Storage_Error
   TE : constant Exception_Type;  -- Tasking_Error
   --  One of these constants is used in the call to specify the exception

   procedure Raise_Exception (E : Exception_Type; Message : String);
   pragma Import (C, Raise_Exception, "__gnat_raise_exception");
   pragma No_Return (Raise_Exception);
   --  Raise specified exception with specified message

private
   --  Really Exception_Type is Exception_Id, but Exception_Id can't be
   --  used directly since it is declared in the non-pure unit Ada.Exceptions,
   --  so redeclare it as a System.Address (maps to System.Object).

   type Exception_Type is new System.Address;

   pragma Import (C, CE, "constraint_error");
   pragma Import (C, PE, "program_error");
   pragma Import (C, SE, "storage_error");
   pragma Import (C, TE, "tasking_error");
   --  References to the exception structures in the standard library

end System.Pure_Exceptions;
