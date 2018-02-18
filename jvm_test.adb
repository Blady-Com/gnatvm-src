------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M _ T E S T                              --
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

with J_Basics; use J_Basics;
with J_Types;  use J_Types;

package body JVM_Test is

   use CP;
   use Utf8;

   ------------------------------
   -- Check_Class_Access_Flags --
   ------------------------------

   procedure Check_Class_Access_Flags (A : Access_Mask) is
   begin
      if        Is_Set (A, ACC_Private)
        or else Is_Set (A, ACC_Protected)
        or else Is_Set (A, ACC_Static)
        or else Is_Set (A, ACC_Volatile)
        or else Is_Set (A, ACC_Transient)
        or else Is_Set (A, ACC_Native)
      then
         Fail ("Bad class file: bad access flag for class");
      end if;
   end Check_Class_Access_Flags;

   ------------------------------
   -- Check_Field_Access_Flags --
   ------------------------------

   procedure Check_Field_Access_Flags (A : Access_Mask) is
   begin
      if        Is_Set (A, ACC_Synchronized)
        or else Is_Set (A, ACC_Native)
        or else Is_Set (A, ACC_Interface)
        or else Is_Set (A, ACC_Abstract)
      then
         Fail ("Bad class file: bad access flag for field");
      end if;
   end Check_Field_Access_Flags;

   -------------------------------
   -- Check_Method_Access_Flags --
   -------------------------------

   procedure Check_Method_Access_Flags (A : Access_Mask) is
   begin
      if        Is_Set (A, ACC_Volatile)
        or else Is_Set (A, ACC_Transient)
        or else Is_Set (A, ACC_Interface)
      then
         Fail ("Bad class file: bad access flag for method");
      end if;
   end Check_Method_Access_Flags;

   --------------
   -- Check_CP --
   --------------

   procedure Check_CP (T : CP.Table) is
      R   : CP_Info;
      Typ : Utf8.Table;

   begin
      if Last (T) < 1 then
         Fail ("Bad class file: constant pool is of length zero");

      elsif Get (T, CP_Index'(0)).Tag /= CONSTANT_Empty then
         Fail ("Bad class file: 0-th entry in constant pool not found");
      end if;

      for K in 0 .. Last (T) loop
         R := Get (T, K);

         case R.Tag is
            when CONSTANT_Class =>
               Check_CP_Entry (T, R.Class_Name_Index, CONSTANT_Utf8);

            when CONSTANT_Fieldref
              |  CONSTANT_Methodref
              |  CONSTANT_Interface_Methodref =>
               Check_CP_Entry (T, R.Class_Index, CONSTANT_Class);
               Check_CP_Entry
                 (T, R.Name_And_Type_Index, CONSTANT_Name_And_Type);

            when CONSTANT_Long
              |  CONSTANT_Double =>
               Check_CP_Entry (T, CP_Index (K + 1), CONSTANT_Empty);

            when CONSTANT_String =>
               Check_CP_Entry (T, R.String_Index, CONSTANT_Utf8);

            when CONSTANT_Name_And_Type =>
               Check_CP_Entry (T, R.Name_Index,       CONSTANT_Utf8);
               Check_CP_Entry (T, R.Descriptor_Index, CONSTANT_Utf8);

               Check_Simple_Name (T, R.Name_Index);
               Typ := Get_Utf8 (T, R.Descriptor_Index);
               if not Type_OK (Typ) then
                  if not Has_Wide_Chars (Typ) then
                     Fail
                       ("Bad class file: " &
                        "bad type signature in constant pool #" & Image (K) &
                        ", found : " & To_String (Typ));
                  else
                     Fail
                       ("Bad class file: " &
                        "bad type signature in constant pool #" & Image (K));
                  end if;
               end if;

            when others =>
               null;
         end case;
      end loop;
   end Check_CP;

   --------------------
   -- Check_CP_Entry --
   --------------------

   procedure Check_CP_Entry (T : CP.Table; K : CP_Index; The_Tag : CP_Tag) is
      Tag_Found : constant CP_Tag := Get (T, K).Tag;
   begin
      if Tag_Found /= The_Tag then
         Fail
           ("Bad class file: bad pool ref: #" & Strip (CP_Index'Image (K)) &
            ". Expecting " & CP_Tag'Image (The_Tag) &
            " found " & CP_Tag'Image (Tag_Found));
      end if;
   end Check_CP_Entry;

   -----------------------
   -- Check_Simple_Name --
   -----------------------

   procedure Check_Simple_Name (T : CP.Table; K : CP_Index) is
      Space         : constant U1 := Character'Pos (' ');
      Slash         : constant U1 := Character'Pos ('\');
      Backslash     : constant U1 := Character'Pos ('/');
      Open_Bracket  : constant U1 := Character'Pos ('[');
      Close_Bracket : constant U1 := Character'Pos (']');
      Open_Paren    : constant U1 := Character'Pos ('(');
      Close_Paren   : constant U1 := Character'Pos (')');
      Semicolon     : constant U1 := Character'Pos (';');

      UT : constant Utf8.Table := Get_Utf8 (T, K);

      U1_Val : U1;

   begin
      for J in 0 .. Last (UT) loop
         U1_Val := Get (UT, J);
         if        U1_Val <= Space
           or else U1_Val  = Slash
           or else U1_Val  = Backslash
           or else U1_Val  = Open_Bracket
           or else U1_Val  = Close_Bracket
           or else U1_Val  = Open_Paren
           or else U1_Val  = Close_Paren
           or else U1_Val  = Semicolon
           or else U1_Val  = 16#7F#
         then
            if not Has_Wide_Chars (UT) then
               Fail
                 ("Bad class file: bad simple name in constant pool #" &
                  Strip (CP_Index'Image (K)) &
                  " found: " & To_String (UT));
            else
               Fail
                 ("Bad class file: bad simple name in constant pool ref #" &
                  Strip (CP_Index'Image (K)));
            end if;
         end if;
      end loop;
   end Check_Simple_Name;

   -------------
   -- Type_OK --
   -------------

   function Type_OK (Typ : Utf8.Table) return Boolean is
      B : constant U1 := Character'Pos ('B');
      C : constant U1 := Character'Pos ('C');
      D : constant U1 := Character'Pos ('D');
      F : constant U1 := Character'Pos ('F');
      I : constant U1 := Character'Pos ('I');
      J : constant U1 := Character'Pos ('J');
      S : constant U1 := Character'Pos ('S');
      Z : constant U1 := Character'Pos ('Z');
      V : constant U1 := Character'Pos ('V');
      L : constant U1 := Character'Pos ('L');

      Open_Bracket : constant U1 := Character'Pos ('[');
      Open_Paren   : constant U1 := Character'Pos ('(');
      Close_Paren  : constant U1 := Character'Pos (')');
      Semicolon    : constant U1 := Character'Pos (';');

      Current_Pos : U2 := 0;
      --  Position of the character in Typ we are currently looking at

      function Field_Or_Return_Type_OK return Boolean;
      --  Checks that the signature of a field or return type starting at index
      --  Current_Pos in Typ is valid. Return True if it is, False if it is
      --  not.

      function Param_Types_OK return Boolean;
      --  Checks that the signature of a paramter list starting at index 0 in
      --  Typ is valid. Return True if it is, False if it is not.

      -----------------------------
      -- Field_Or_Return_Type_OK --
      -----------------------------

      function Field_Or_Return_Type_OK return Boolean is
      begin
         while Get (Typ, Current_Pos) = Open_Bracket loop
            Current_Pos := Current_Pos + 1;
         end loop;

         if Get (Typ, Current_Pos) = L then
            loop
               Current_Pos := Current_Pos + 1;
               if Int_32 (Current_Pos) > Last (Typ) then
                  return False;
               end if;
               exit when Get (Typ, Current_Pos) = Semicolon;
            end loop;

         else
            case Get (Typ, Current_Pos) is
               when B | C | D | F | I | J | S | Z | V =>
                  null;
               when others =>
                  return False;
            end case;
         end if;

         Current_Pos := Current_Pos + 1;
         return True;
      end Field_Or_Return_Type_OK;

      --------------------
      -- Param_Types_OK --
      --------------------

      function Param_Types_OK return Boolean is
      begin
         Current_Pos := 1;
         while Get (Typ, Current_Pos) /= Close_Paren loop
            if not Field_Or_Return_Type_OK then
               return False;
            end if;
         end loop;
         return True;
      end Param_Types_OK;

   --  Beginning of Type_OK

   begin
      --  If first character is a "(" then we are dealing with a method
      --  signature, so advance Current_Pos after you have found the matching
      --  ")".

      if Get (Typ, U2'(0)) = Open_Paren then
         loop
            Current_Pos := Current_Pos + 1;
            exit when Get (Typ, Current_Pos) = Close_Paren;
         end loop;
         Current_Pos := Current_Pos + 1;
      end if;

      --  Check the field type or the method return type

      if not Field_Or_Return_Type_OK then
         return False;
      end if;

      --  If at this stage we have not consumed all characters in Typ then
      --  there is something wrong.

      if Nat_32 (Current_Pos) < Length (Typ) then
         return False;
      end if;

      --  If we are dealing with a method then check the parameters

      if Get (Typ, U2'(0)) = Open_Paren and then not Param_Types_OK then
         return False;
      end if;

      return True;
   end Type_OK;

end JVM_Test;
