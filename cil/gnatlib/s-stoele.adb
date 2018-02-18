------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S T O R A G E _ E L E M E N T S              --
--                                                                          --
--                                 B o d y                                  --
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

package body System.Storage_Elements is

   pragma Suppress (All_Checks);

   --  Address arithmetic

   function "+" (Left : Address; Right : Storage_Offset) return Address is
      pragma Unreferenced (Left, Right);

   begin
      return Null_Address;
   end "+";

   function "+" (Left : Storage_Offset; Right : Address) return Address is
      pragma Unreferenced (Left, Right);

   begin
      return Null_Address;
   end "+";

   function "-" (Left : Address; Right : Storage_Offset) return Address is
      pragma Unreferenced (Left, Right);

   begin
      return Null_Address;
   end "-";

   function "-" (Left, Right : Address) return Storage_Offset is
      pragma Unreferenced (Left, Right);

   begin
      return 0;
   end "-";

   function "mod" (Left : Address; Right : Storage_Offset)
     return Storage_Offset
   is
      pragma Unreferenced (Left, Right);

   begin
      return 0;
   end "mod";

   --  Conversion to/from integers

   function To_Address (Value : Integer_Address) return Address is
      pragma Unreferenced (Value);

   begin
      return Null_Address;
   end To_Address;

   function To_Integer (Value : Address) return Integer_Address is
      function Hash_Code (A : Address) return Integer;
      pragma Import (C, Hash_Code, "hash_code");
   begin
      --  Hash_Code will blow up if passed a null address, so we check
      --  for null and simply return 0.

      if Value = Null_Address then
         return 0;
      else
         return Integer_Address (Hash_Code (Value));
      end if;
   end To_Integer;

end System.Storage_Elements;
