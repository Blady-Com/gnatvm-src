------------------------------------------------------------------------------
--                                                                          --
--                   GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--             Copyright (C) 1995-2010, Free Software Foundation, Inc.      --
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

--  This is a Java version of this package

--  This package includes all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package. It is designed to be
--  a bottom-level (leaf) package.

with Interfaces.Java.Lang.Thread;

package System.OS_Interface is
   pragma Preelaborate;

   type Thread    is new Interfaces.Java.Lang.Thread.Typ with null record;
   type Thread_Id is access all Thread;

   --  Note: System.Address is used below instead of Task_Id to avoid a
   --  circular dependency. The GNULLI is responsible for converting from and
   --  to a Task_Id this value.

   function new_Thread (Self_Id : System.Address) return Thread_Id;
   --  Create a new thread

   function Get_Self_Id return System.Address;
   --  Return the self id associated with the current thread.

private
   pragma Convention (Java, Thread);
   pragma Java_Constructor (new_Thread);
   pragma Import (Java, Get_Self_Id, "getSelfId");
end System.OS_Interface;

pragma Import (Java, System.OS_Interface, "jgnat.adalib.ada_wrapper");
