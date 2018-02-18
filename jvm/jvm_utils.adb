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

with J_Basics;

package body JVM_Utils is

   ----------------
   -- Get_String --
   ----------------

   function Get_String (CF : Class_File; K : CP_Index_Class) return String is
      T          : constant CP.Table   := CF.Constant_Pool;
      Class_Info : CP_Info;
      Class_Name : Utf8.Table;
   begin
      if K = CP_Empty then
         return "";
      end if;

      Class_Info := JVM_File.CP.Get (T, K);
      Class_Name := J_Basics.Get_Utf8 (T, Class_Info.Class_Name_Index);
      return To_String (Class_Name);
   end Get_String;

   ------------------
   -- Get_JNI_Type --
   ------------------

   function Get_JNI_Type (C : Character) return String is
   begin
      case C is
         when JVM_Byte    =>
            return "J_Byte";

         when JVM_Char    =>
            return "J_Char";

         when JVM_Double  =>
            return "J_Double";

         when JVM_Float   =>
            return "J_Float";

         when JVM_Int     =>
            return "J_Int";

         when JVM_Long    =>
            return "J_Long";

         when JVM_Short   =>
            return "J_Short";

         when JVM_Boolean =>
            return "J_Boolean";

         when others =>
            return "J_Object";
      end case;
   end Get_JNI_Type;

   ---------------------
   -- Parameter_Count --
   ---------------------

   function Parameter_Count (Descriptor : String) return Natural is
      Pos : Integer := Descriptor'First;
      C   : Integer := -1;
   begin
      Pos := Next_Declaration_Pos (Descriptor);
      while Pos /= 0 loop
         C := C + 1;
         Pos := Next_Declaration_Pos (Descriptor (Pos .. Descriptor'Last));
      end loop;
      return C;
   end Parameter_Count;

   --------------------------
   -- Next_Declaration_Pos --
   --------------------------

   function Next_Declaration_Pos (Descriptor : String) return Natural is
      Pos : Natural := Descriptor'First;
   begin
      case Descriptor (Pos) is
         when JVM_Class =>
            while Descriptor (Pos) /= ';' loop
               Pos := Pos + 1;
            end loop;
            return Pos + 1;

         when JVM_Array =>
            while Descriptor (Pos) = JVM_Array loop
               Pos := Pos + 1;
            end loop;
            return
              Next_Declaration_Pos (Descriptor (Pos .. Descriptor'Last));

         when ')' =>
            return 0;

         when others =>
            return Pos + 1;
      end case;
   end Next_Declaration_Pos;

end JVM_Utils;
