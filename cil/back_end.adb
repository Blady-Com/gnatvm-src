------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             B A C K _ E N D                              --
--                                                                          --
--                                 B o d y                                  --
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

--  This is the GNAT for .NET target-dependent version of the Back_End package

with Jx_Drive;
with JVM.Emit.CIL;
with Adabkend;
with Elists;
with Stringt;
with Namet;

package body Back_End is

   package Dotnet_GNAT is new Adabkend
     (Product_Name       => "GNAT for .NET",
      Copyright_Years    => "1998-2010",
      Driver             => Jx_Drive.GNAT_To_JVM,
      Is_Back_End_Switch => Jx_Drive.Is_Back_End_Switch);

   procedure Scan_Compiler_Arguments renames
     Dotnet_GNAT.Scan_Compiler_Arguments;

   -------------------
   -- Call_Back_End --
   -------------------

   procedure Call_Back_End (Mode : Back_End_Mode_Type) is
      pragma Unreferenced (Mode); -- Mode not referenced

   begin
      --  Since the back end is called with all tables locked,
      --  first unlock any tables that we need to change.

      Stringt.Unlock;
      Namet.Unlock;
      Elists.Unlock;

      Dotnet_GNAT.Call_Back_End;

      --  Make sure to lock any unlocked tables again before returning

      Elists.Lock;
      Namet.Lock;
      Stringt.Lock;
   end Call_Back_End;

   -------------------------------
   -- Gen_Or_Update_Object_File --
   -------------------------------

   procedure Gen_Or_Update_Object_File is
   begin
      --  We need to update the .il file's timestamp as gnatmake asserts that
      --  this file is created after the .ali file, while it is actually
      --  created before.
      JVM.Emit.CIL.Update_Obj_File_Timestamp;
   end Gen_Or_Update_Object_File;

end Back_End;
