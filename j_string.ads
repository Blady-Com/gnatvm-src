------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J _ S T R I N G                              --
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

--  This package provides utilities convenient for converting between
--  strings and their corresponding GNAT Name_Ids and String_Ids.

with Namet; use Namet;
with Types; use Types;

package J_String is

   function Name (Name : String) return Name_Id;
   --  Returns a Name_Id corresponding to the given name string.

   function Name_String (Name : Name_Id) return String;
   --  Returns the string associated with Name.

   function Str_Id (S : String) return String_Id;
   --  Returns a String_Id corresponding to the given string.

   function Str (Str_Id : String_Id) return String;
   --  Returns the string associated with Str_Id.

   function Source_Name (Sloc : Source_Ptr) return Name_Id;
   --  Returns a Name_Id corresponding to the source file name
   --  associated with the given source position Sloc.

end J_String;
