------------------------------------------------------------------------------
--                                                                          --
--                      GNAT VISUAL STUDIO INTEGRATION                      --
--                                                                          --
--                               FILE_HELPER                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2007, AdaCore                        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by --
-- AdaCore - http://www.adacore.com                                         --
--                                                                          --
-- This work is based on A# Visual Studio integration by  Prof. Martin C.   --
-- Carlisle of the United States Air Force Academy.                         --
--                                                                          --
------------------------------------------------------------------------------
---------------------------------------------------------------
--
--  FILE_HELPERS.ADS
--  Description : IO helpers
--
--  By: Martin Carlisle
--      United States Air Force Academy
--
---------------------------------------------------------------

with Ada.Text_IO;

package File_Helpers is

   procedure Get_String
     (File : in     Ada.Text_IO.File_Type;
      Item :    out String;
      Last :    out Natural);
   --  Reads from file the next string.  If first non-blank
   --  is ", then reads until ending ", o/w reads sequence
   --  of non-blank characters.  Stops at end of line.

   procedure Get_Number
     (File       : in     Ada.Text_IO.File_Type;
      Item       :    out Integer;
      First_Char : in out Character);
   --  given the first character, reads from file the
   --  next number.  First_Char ends with character following
   --  the number.

end File_Helpers;
