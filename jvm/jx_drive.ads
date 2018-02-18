------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J X _ D R I V E                              --
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

--  This package is the main driver for the JGNAT back end and is invoked
--  by the gnat1 driver.

with Snames; use Snames;
with Types;  use Types;

package Jx_Drive is

   Convention_VM : constant Convention_Id := Convention_Java;
   --  Calling convention corresponding to the current virtual machine
   --  (Convention_Java for JGNAT, Convention_CIL for .NET).

   procedure GNAT_To_JVM (GNAT_Root : Node_Id);
   --  Translates an entire GNAT tree for a compilation unit into
   --  a set of JVM class files. This is the main driver for the
   --  Ada-to-JVM back end and is invoked by Gnat1drv.

   procedure Translate (Node : Node_Id);
   --  This is the top-level translation routine which is applied to
   --  declarations. We export this so it can be called recursively
   --  from other JVM back-end packages.

   function Is_Back_End_Switch (Switch : String) return Boolean;
   --  Returns True if and only if Switch denotes a back-end switch

end Jx_Drive;
