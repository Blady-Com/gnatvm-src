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

with JVM_File;    use JVM_File;

package JVM_Utils is

   function Get_String (CF : Class_File; K : CP_Index_Class) return String;
   --  Given a constant pool entry K, which is the index of a class entry
   --  in constant pool T, return the name of that class as a string.

   JVM_Byte     : constant Character := 'B';
   JVM_Char     : constant Character := 'C';
   JVM_Double   : constant Character := 'D';
   JVM_Float    : constant Character := 'F';
   JVM_Int      : constant Character := 'I';
   JVM_Long     : constant Character := 'J';
   JVM_Short    : constant Character := 'S';
   JVM_Boolean  : constant Character := 'Z';
   JVM_Void     : constant Character := 'V';
   JVM_Class    : constant Character := 'L';
   JVM_Array    : constant Character := '[';

   function Get_JNI_Type (C : Character) return String;
   --  Return the string representation of the JNI_Type to use for
   --  the C Character representing a Java primitive type as used
   --  in field descriptor.

   function Parameter_Count (Descriptor : String) return Natural;
   --  Count the number of arguments.

   function Next_Declaration_Pos (Descriptor : String) return Natural;
   --  Descriptor is a method descriptor or at the tail of a method
   --  descriptor.  This routine returns the first position in Descriptor of
   --  the next valid type descriptor. If we have reached the end of the
   --  method descriptor 0 is returned.

end JVM_Utils;
