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

package Utils is

   function Mangle_ID (Str : Wide_String) return Wide_String;
   --  Mangle the identifier given in parameter, in the corresponding JNI
   --  symbol.

end Utils;
