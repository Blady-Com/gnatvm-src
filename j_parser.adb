------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J _ P A R S E R                              --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with J_Basics;
with J_Types;                 use J_Types;
with Osint;                   use Osint;
with Unchecked_Deallocation;

package body J_Parser is

   File : Stream_Of_U1_Ptr := null;
   --  The last java file read (kept so that we don't have to duplicate
   --  function and parameter names) This variable is freed (and recreated)
   --  each time Parse_File is called

   File_Name : Unbounded_String;
   --  Name of the file stored in 'File'

   type Entity_Pos is
      record
         Start : Nat_32;
         Last  : Nat_32;
      end record;
   type Entity_Pos_Array is array (Natural range <>) of Entity_Pos;
   --  Defines a word in File (starting and ending index of the word)

   type Function_Definition;
   type Function_Definition_List is access all Function_Definition;
   type Function_Definition is
      record
         Name       : Entity_Pos;
         Class_Name : Entity_Pos;
         Descriptor : Ada.Strings.Unbounded.Unbounded_String;
         Num_Args   : Natural;
         Args       : Entity_Pos_Array (0 .. 15);
         --  Probably no more than 15 parameters for a single function
         Next       : Function_Definition_List;
      end record;
   --  This is a whole function definition, as will be stored in the list.

   Def_List : Function_Definition_List := null;
   --  We use a list instead of a htable, even if it is a little bit
   --  smaller (there probably won't be that many functions in a single
   --  java file anyway). The htable needs to allocate some memory, even if
   --  it is never used, wether the list does allocate only the memory
   --  needed. Since we don't necessary need to use Parse_File when using
   --  jvm2ada, it's probably better to use the list.

   procedure Free (List : in out Function_Definition_List);
   --  Free the list. This function is called every time Parse_File is called.

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Function_Definition_List) is
      procedure Free_Internal is new Unchecked_Deallocation
        (Function_Definition, Function_Definition_List);
      Tmp : Function_Definition_List;
   begin
      while List /= null loop
         Tmp := List.Next;
         Free_Internal (List);
         List := Tmp;
      end loop;
   end Free;

   --------------------
   -- Parameter_Name --
   --------------------

   function Parameter_Name (Func_Name  : String;
                            Descriptor : String;
                            Param      : U2)
                            return       String
   is
      Decl      : Function_Definition_List := Def_List;
      Is_Constructor : Boolean := False;
      Descr     : String := Descriptor;
      Index     : Natural := Descriptor'First;
      End_Descr : Natural := Descr'First - 1;
      Last_Slash : Natural;
   begin

      --  If the source file was not found then

      if File = null then
         return "P" & Image (Param);
      end if;

      Is_Constructor := Func_Name = "<init>";

      --  Modify the descriptor to use only class names without packages

      while Index <= Descriptor'Last loop
         case Descriptor (Index) is
            when 'L' =>
               End_Descr := End_Descr + 1;
               Descr (End_Descr) := 'L';
               Last_Slash := Index;
               while Descriptor (Index) /= ';' loop
                  if Is_Directory_Separator (Descriptor (Index)) then
                     Last_Slash := Index;
                  end if;
                  Index := Index + 1;
               end loop;
               Descr (End_Descr + 1 .. End_Descr + Index - Last_Slash)
                 := Descriptor (Last_Slash + 1 .. Index);
               End_Descr := End_Descr + Index - Last_Slash;

            when others =>
               End_Descr := End_Descr + 1;
               Descr (End_Descr) := Descriptor (Index);
         end case;
         Index := Index + 1;
      end loop;

      --  Search the descriptor in the list of the functions

      while Decl /= null loop
         if (Is_Constructor
             and then File (Decl.Name.Start .. Decl.Name.Last)
             = File (Decl.Class_Name.Start .. Decl.Class_Name.Last))
           or
           (not Is_Constructor
            and then
            Func_Name = J_Basics.To_String (File (Decl.Name.Start
                                                  .. Decl.Name.Last)))
         then
            if Descr (Descr'First .. End_Descr)
              = To_String (Decl.Descriptor)
            then
               return J_Basics.To_String
                 (File
                  (Decl.Args (Natural (Param)).Start
                   .. Decl.Args (Natural (Param)).Last));
            end if;
         end if;
         Decl := Decl.Next;
      end loop;

      return "P" & Image (Param);
   end Parameter_Name;

   ----------------
   -- Parse_File --
   ----------------

   procedure Parse_File (Filename : String;
                         Stream   : J_Types.Stream_Of_U1)
   is
      procedure Free is new Unchecked_Deallocation (Stream_Of_U1,
                                                    Stream_Of_U1_Ptr);
      P    : Nat_32;

      Open_Brace   : constant U1 := U1 (Character'Pos ('{'));
      Close_Brace  : constant U1 := U1 (Character'Pos ('}'));
      Open_Paren   : constant U1 := U1 (Character'Pos ('('));
      Close_Paren  : constant U1 := U1 (Character'Pos (')'));
      Open_Square  : constant U1 := U1 (Character'Pos ('['));
      Close_Square : constant U1 := U1 (Character'Pos (']'));
      New_Line     : constant U1 := U1 (Character'Pos (Ascii.LF));
      Semi_Colon   : constant U1 := U1 (Character'Pos (';'));
      Equal        : constant U1 := U1 (Character'Pos ('='));
      Slash        : constant U1 := U1 (Character'Pos ('/'));
      Star         : constant U1 := U1 (Character'Pos ('*'));
      Class_Id     : constant Stream_Of_U1
        := (U1 (Character'Pos ('c')),
            U1 (Character'Pos ('l')),
            U1 (Character'Pos ('a')),
            U1 (Character'Pos ('s')),
            U1 (Character'Pos ('s')));
      Interface_Id : constant Stream_Of_U1
        := (U1 (Character'Pos ('i')),
            U1 (Character'Pos ('n')),
            U1 (Character'Pos ('t')),
            U1 (Character'Pos ('e')),
            U1 (Character'Pos ('r')),
            U1 (Character'Pos ('f')),
            U1 (Character'Pos ('a')),
            U1 (Character'Pos ('c')),
            U1 (Character'Pos ('e')));

      function Next_Entity return Entity_Pos;
      --  Return the beginning of the next entity (which is thus between
      --  Next_entity and P - 1

      function Full_Java_Descriptor (Start : Nat_32; Last : Nat_32)
                                     return String;
      --  Create a java descriptor from a java type. As opposed to
      --  Java_Descriptor, in this function the descriptor can include
      --  spaces and square brackets

      function Java_Descriptor (Start : Nat_32; Last : Nat_32) return String;
      --  Return the java descriptor for the type described in file,
      --  between indexes Start and Last

      --------------------------
      -- Full_Java_Descriptor --
      --------------------------

      function Full_Java_Descriptor (Start : Nat_32; Last : Nat_32)
                                     return String
      is
         Num_Brace : Natural := 0;
         Final     : Nat_32 := Last;
      begin
         loop
            case File (Final) is
               when Open_Square =>
                  Num_Brace := Num_Brace + 1;
               when Close_Square =>
                  null;
               when U1 (Character'Pos (' '))
                 | U1 (Character'Pos (Ada.Characters.Latin_1.HT))
                 | U1 (Character'Pos (Ada.Characters.Latin_1.LF))
                 | U1 (Character'Pos (Ada.Characters.Latin_1.CR)) =>
                  null;
               when others =>
                  exit;
            end case;
            Final := Final - 1;
         end loop;

         return (Ada.Strings.Fixed."*" (Num_Brace, '['))
           & Java_Descriptor (Start, Final);
      end Full_Java_Descriptor;

      ---------------------
      -- Java_Descriptor --
      ---------------------

      function Java_Descriptor (Start : Nat_32; Last : Nat_32) return String is
         S     : String := J_Basics.To_String (File (Start .. Last));
         Index : Natural;
      begin
         if S = "byte" then
            return "B";
         elsif S = "char" then
            return "C";
         elsif S = "double" then
            return "D";
         elsif S = "float" then
            return "F";
         elsif S = "int" then
            return "I";
         elsif S = "long" then
            return "J";
         elsif S = "short" then
            return "S";
         elsif S = "boolean" then
            return "Z";
         elsif S = "void" then
            return "V";
         else
            Index := Ada.Strings.Fixed.Index (S, ".", Ada.Strings.Backward);
            --  Return the class name, without the package part
            if Index /= 0 then
               return "L" & S (Index + 1 .. S'Last) & ";";
            else
               return "L" & S & ";";
            end if;
         end if;
      end Java_Descriptor;

      -----------------
      -- Next_Entity --
      -----------------

      function Next_Entity return Entity_Pos is
         Start : Nat_32;
         Char  : Character := Character'Val (File (P));
      begin

         --  Skip spaces

         while Char = ' ' or Char = Ada.Characters.Latin_1.HT
           or Char = Ada.Characters.Latin_1.LF
           or Char = Ada.Characters.Latin_1.CR
         loop
            P := P + 1;
            if P > File'Last then
               return Entity_Pos'(P, P);
            end if;
            Char := Character'Val (File (P));
         end loop;

         --  Skip comments

         if File (P) = Slash then

            if File (P + 1) = Slash then
               loop
                  P := P + 1;
                  exit when File (P) = New_Line;
               end loop;
               P := P + 1;
               return Next_Entity;

            elsif File (P + 1) = Star then
               P := P + 1;
               loop
                  P := P + 1;
                  exit when File (P) = Star
                    and then File (P + 1) = Slash;
               end loop;
               P := P + 2;
               return Next_Entity;

            end if;
         end if;

         Start := P;
         Char  := Character'Val (File (P));

         if Is_Alphanumeric (Char) or Char = '_'
           or Char = '$' or Char = '.' then

            loop
               P := P + 1;
               Char := Character'Val (File (P));
               exit when not Is_Alphanumeric (Char)
                 and     Char /= '_'
                 and     Char /= '.'
                 and     Char /= '$';
            end loop;

         elsif Char = Ada.Characters.Latin_1.Apostrophe then
            P := P + 3;

         elsif Char = '"' then
            loop
               P := P + 1;
               Char := Character'Val (File (P));
               exit when Char = '"'
                 and then Character'Val (File (P - 1)) /= '\';
            end loop;
            P := P + 1;

         else
            P := P + 1;
         end if;

--          Ada.Text_IO.Put_Line
--            ("Entity = "
--             & J_Basics.To_String (File (Start .. P - 1)));
--             & "   Start=" & Nat_32'Image (Start)
--             & "   Last=" & Nat_32'Image (P - 1));
         return Entity_Pos'(Start, P - 1);
      end Next_Entity;

      Num_Brace        : Natural := 0;
      Last_Start       : Nat_32  := 0;
      Entity           : Entity_Pos := (0, 0);
      Class_Level      : Natural := 0;
      Prev_Entity      : Entity_Pos := (0, 0);
      Prev_Prev_Entity : Entity_Pos;
      Class_Name       : array (0 .. 20) of Entity_Pos := (others => (0, 0));
   begin

      --  Let's be lazy (if the work was already done, no need to start again

      if To_String (File_Name) = Filename then
         return;
      end if;

      Free (File);
      Free (Def_List);
      File_Name := To_Unbounded_String (Filename);

      if Stream'Length = 0 then
         File := J_Basics.Get_Stream_Of_U1 (Filename);
      else
         File := new Stream_Of_U1'(Stream);
      end if;

      P := File'First;

      loop
         Prev_Prev_Entity := Prev_Entity;
         Prev_Entity := Entity;
         Entity := Next_Entity;
         exit when Entity.Start > File'Last;

         case File (Entity.Start) is

            when Open_Brace =>
               Num_Brace  := Num_Brace + 1;
               Last_Start := P;

            when Close_Brace =>
               Num_Brace := Num_Brace - 1;
               Last_Start := P;

               if Num_Brace = Class_Level - 1 then
                  Class_Level := Class_Level - 1;
               end if;

            when Semi_Colon =>
               Last_Start := P;

            when Open_Square | Close_Square =>
               Prev_Entity.Last := Entity.Last;
               Entity := Prev_Entity;

            when Equal =>
               Last_Start := 0;

            when Open_Paren =>

               --  If we have found a new function

               if Num_Brace = Class_Level
                 and then Last_Start /= 0
               then

                  declare
                     Func_Def    : Function_Definition_List;
                     Type_Entity : Entity_Pos;
                  begin

                     Func_Def := new Function_Definition;
                     Func_Def.Next := Def_List;
                     Def_List := Func_Def;

                     Func_Def.Name       := (Prev_Entity.Start,
                                             Prev_Entity.Last);
                     Func_Def.Class_Name := (Class_Name (Class_Level).Start,
                                             Class_Name (Class_Level).Last);
                     Func_Def.Num_Args   := 0;
                     Func_Def.Descriptor := To_Unbounded_String ("(");

                     Last_Start := 0;

                     loop
                        --  Type name
                        Entity := Next_Entity;
                        exit when File (Entity.Start) = Close_Paren;
                        Type_Entity := Entity;

                        --  Parameter name or brackets for an array type
                        Entity := Next_Entity;

                        while File (Entity.Start) = Open_Square loop
                           Append (Func_Def.Descriptor, "[");
                           Entity := Next_Entity; --  Close_Square
                           Entity := Next_Entity;
                        end loop;

                        Func_Def.Args (Func_Def.Num_Args) := (Entity.Start,
                                                              Entity.Last);
                        Func_Def.Num_Args := Func_Def.Num_Args + 1;

                        --  Separator or array identificators
                        Entity := Next_Entity;
                        while File (Entity.Start) = Open_Square loop
                           Append (Func_Def.Descriptor, "[");
                           Entity := Next_Entity; --  Close_Square
                           Entity := Next_Entity;
                        end loop;

                        Append (Func_Def.Descriptor,
                                Java_Descriptor (Type_Entity.Start,
                                                 Type_Entity.Last));

                        exit when File (Entity.Start) = Close_Paren;
                     end loop;

                     --  If we do not have a constructor, print the return type
                     Append (Func_Def.Descriptor, ")");

                     Entity := Next_Entity;
                     while File (Entity.Start) = Open_Square loop
                        Append (Func_Def.Descriptor, "[");
                        Entity := Next_Entity; --  Close_Square
                        Entity := Next_Entity;
                     end loop;

                     if File (Entity.Start) = Open_Brace then
                        Num_Brace  := Num_Brace + 1;
                     end if;

                     if File (Prev_Entity.Start .. Prev_Entity.Last)
                       /= File (Class_Name (Class_Level).Start
                                .. Class_Name (Class_Level).Last)
                     then
                        Append (Func_Def.Descriptor,
                                Full_Java_Descriptor (Prev_Prev_Entity.Start,
                                                      Prev_Prev_Entity.Last));
                     else
                        Append (Func_Def.Descriptor, "V");
                     end if;
                  end;

               end if;

            when U1 (Character'Pos ('c')) =>

               if File (Entity.Start .. Entity.Last) = Class_Id then
                  Class_Level := Class_Level + 1;
                  Entity := Next_Entity;
                  Class_Name (Class_Level) := Entity;
               end if;

            when U1 (Character'Pos ('i')) =>
               if File (Entity.Start .. Entity.Last) = Interface_Id then
                  Class_Level := Class_Level + 1;
                  Entity := Next_Entity;
                  Class_Name (Class_Level) := Entity;
               end if;

            when others =>
               null;
         end case;
      end loop;

      --  Print the list (Debug)
--       declare
--          Func_Def : Function_Definition_List := Def_List;
--       begin
--          Ada.Text_IO.Put_Line ("Parsing " & Filename);
--          while Func_Def /= null loop
--             Ada.Text_IO.Put
--               (J_Basics.To_String
--               (File (Func_Def.Class_Name.Start .. Func_Def.Class_Name.Last))
--                & "."
--                & J_Basics.To_String
--                (File (Func_Def.Name.Start .. Func_Def.Name.Last))
--                & " "
--                & Ada.Strings.Unbounded.To_String (Func_Def.Descriptor)
--                & " ");
--             for J in 0 .. Func_Def.Num_Args - 1 loop
--                Ada.Text_IO.Put
--                  (J_Basics.To_String
--                   (File (Func_Def.Args (J).Start .. Func_Def.Args (J).Last))
--                   & " ");
--             end loop;
--             Ada.Text_IO.New_Line;
--             Func_Def := Func_Def.Next;
--          end loop;
--       end;

   end Parse_File;

end J_Parser;
