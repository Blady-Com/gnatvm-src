------------------------------------------------------------------------------
--                                                                          --
--                                 J N I                                    --
--                                                                          --
--                        Copyright (C) 2007-2013, AdaCore                  --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
--                                                                          --
------------------------------------------------------------------------------

package body Utils is

   ---------------
   -- Mangle_ID --
   ---------------

   function Mangle_ID (Str : Wide_String) return Wide_String is
      Size : Integer := Str'Length;
   begin
      for J in Str'Range loop
         case Str (J) is
            when '_' | ';' | '[' =>
               Size := Size + 1;

            when '$' =>
               Size := Size + 5;

            when others =>
               null;

         end case;
      end loop;

      declare
         Mangled       : Wide_String (1 .. Size);
         Mangled_Index : Natural := 1;
      begin
         for J in Str'Range loop
            if Str (J) = '.' or else Str (J) = '/' then
               Mangled (Mangled_Index) := '_';
               Mangled_Index := Mangled_Index + 1;
            elsif Str (J) = '_' then
               Mangled (Mangled_Index) := '_';
               Mangled (Mangled_Index + 1) := '1';
               Mangled_Index := Mangled_Index + 2;
            elsif Str (J) = ';' then
               Mangled (Mangled_Index) := '_';
               Mangled (Mangled_Index + 1) := '2';
               Mangled_Index := Mangled_Index + 2;
            elsif Str (J) = '[' then
               Mangled (Mangled_Index) := '_';
               Mangled (Mangled_Index + 1) := '3';
               Mangled_Index := Mangled_Index + 2;
            elsif Str (J) = '$' then
               Mangled (Mangled_Index .. Mangled_Index + 5) := "_00024";
               Mangled_Index := Mangled_Index + 6;
            else
               Mangled (Mangled_Index) := Str (J);
               Mangled_Index := Mangled_Index + 1;
            end if;
         end loop;

         return Mangled;
      end;
   end Mangle_ID;

end Utils;
