------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 7                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2013, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by --
-- AdaCore - http://www.adacore.com                                         --
--                                                                          --
------------------------------------------------------------------------------

--  This package implements translation of Ada packages into their equivalent
--  JVM class file.

with JVM;   use JVM;
with Types; use Types;

package Jx_Ch7 is

   procedure Generate_Null_Methods (Class : Class_Id);
   --  Generate null methods associated with non-abstract classes that have
   --  abstract primitives. Required in the VM target to provide support to
   --  the following Ada 2005 construct:
   --
   --       type Unit is new Float;
   --       function "*" (L, R : Unit) return Unit is abstract;

   function Generate_Package_Class (Pkg : Node_Id) return Boolean;
   --  Generates a class file for the given package. Returns True if and only
   --  if elaboration code has been generated for Pkg.

end Jx_Ch7;
