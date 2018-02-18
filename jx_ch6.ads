------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 6                                --
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

with Types; use Types;

package Jx_Ch6 is

   --  Processing for subprograms

   procedure Generate_Subprogram_Class (Comp_Unit : Node_Id);
   --  Generates a class file for a subprogram compilation unit. The class will
   --  have the name of the subprogram and the subprogram's method will be
   --  named '$'. For non CIL back-end, if the subprogram is a parameterless
   --  procedure then a 'main' method will also be created that has the
   --  signature of a standard Java main method:
   --    public static void main (String [] args).

   procedure Generate_Interface_Wrappers (Tag_Typ : Entity_Id);
   --  Generate interface wrappers associated with interfaces implemented by
   --  Tag_Typ

   procedure Generate_Method (Subp_Body : Node_Id);
   --  Generates the subprogram body's method and its J-code.

   procedure Translate_Subprogram_Call (Call : Node_Id);
   --  Generates the J-code for a subprogram call.

end Jx_Ch6;
