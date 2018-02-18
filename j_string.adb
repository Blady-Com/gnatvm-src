------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J _ S T R I N G                              --
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

with Sinput;  use Sinput;
with Stringt; use Stringt;

package body J_String is

   ----------
   -- Name --
   ----------

   function Name (Name : String) return Name_Id is
   begin
      for J in 1 .. Name'Length loop
         Name_Buffer (J) := Name (Name'First + (J - 1));
      end loop;

      Name_Len := Name'Length;
      return Name_Find;
   end Name;

   -----------------
   -- Name_String --
   -----------------

   function Name_String (Name : Name_Id) return String is
   begin
      pragma Assert (Name /= No_Name);
      return Get_Name_String (Name);
   end Name_String;

   ------------
   -- Str_Id --
   ------------

   function Str_Id (S : String) return String_Id is
   begin
      for J in 1 .. S'Length loop
         Name_Buffer (J) := S (S'First + (J - 1));
      end loop;

      Name_Len := S'Length;
      return String_From_Name_Buffer;
   end Str_Id;

   ---------
   -- Str --
   ---------

   function Str (Str_Id : String_Id) return String is
   begin
      --  ??? pragma Assert (Str_Id /= No_String);
      if Str_Id = No_String then
         return "";
      end if;

      String_To_Name_Buffer (Str_Id);

      return Name_Buffer (1 .. Name_Len);
   end Str;

   -----------------
   -- Source_Name --
   -----------------

   function Source_Name (Sloc : Source_Ptr) return Name_Id is
   begin
      if Sloc = No_Location or Sloc = Standard_Location then
         return No_Name;
      else
         return Name_Id (Debug_Source_Name (Get_Source_File_Index (Sloc)));
      end if;
   end Source_Name;

end J_String;
