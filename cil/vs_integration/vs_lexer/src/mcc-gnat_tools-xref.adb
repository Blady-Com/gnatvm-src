------------------------------------------------------------------------------
--                                                                          --
--                      GNAT VISUAL STUDIO INTEGRATION                      --
--                                                                          --
--                           MCC.GNAT_TOOLS.XREF                            --
--                                                                          --
--                                 B o d y                                  --
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
--  MCC-GNAT_TOOLS-XREF.ADB
--  Description : implements GNAT XREF functionality
--
--  By: Dr. Martin C. Carlisle
--      US Air Force Academy
--
---------------------------------------------------------------
with Ada.Unchecked_Deallocation;
with Ada.Text_IO, Ada.Integer_Text_IO;
use type Ada.Text_IO.Count;
with File_Helpers;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Exceptions;
use Ada.Exceptions;

package body MCC.Gnat_Tools.Xref is

   use type Entity_List_Package.Listptr;
   use type Reference_List_Package.Listptr;
   use type Xref_File_List_Package.Listptr;

   function String_Equal (L, R : in String) return Boolean;

   procedure Free (References : in out Reference_List);
   procedure Free (Entities : in out Entity_List);

   ------------------
   -- String_Equal --
   ------------------

   function String_Equal (L, R : in String) return Boolean is
   begin
      if L'Length /= R'Length then
         return False;
      end if;

      for I in L'Range loop
         if Ada.Characters.Handling.To_Lower (L (I)) /=
           Ada.Characters.Handling.To_Lower (R (I - L'First + R'First))
         then
            return False;
         end if;
      end loop;

      return True;
   end String_Equal;

   ------------------------
   -- Enumerate_Entities --
   ------------------------

   procedure Enumerate_Entities
     (Xref     : in Xrefs;
      Callback : in Callback_Type)
   is
      Files          : Xref_File_List := Xref_File_List (Xref);
      File_Walk      : Xref_File_List := Files;
      File_Car       : Xref_File_Pointer;
      Entities       : Entity_List;
      Entity_Car     : Entity_Pointer;

   begin
      --  First determine our File_Number
      while File_Walk /= null loop
         File_Car := Xref_File_List_Package.Car (File_Walk);
         Entities := File_Car.Entities;

         while Entities /= null loop
            Entity_Car := Entity_List_Package.Car (Entities);
            Callback.all (Entity_Car.Name (1 .. Entity_Car.Name_Length));
            Entities := Entity_List_Package.Cdr (Entities);
         end loop;

         File_Walk := Xref_File_List_Package.Cdr (File_Walk);
      end loop;
   end Enumerate_Entities;

   -------------------------------
   -- Enumerate_Public_Entities --
   -------------------------------

   procedure Enumerate_Public_Entities
     (Filename  : in String;
      Xref      : in Xrefs;
      Callback  : in Public_Callback_Type)
   is
      Files          : Xref_File_List := Xref_File_List (Xref);
      File_Walk      : Xref_File_List := Files;
      File_Car       : Xref_File_Pointer;
      Entities       : Entity_List;
      Entity_Car     : Entity_Pointer;
   begin
      --  First determine our File_Number
      while File_Walk /= null loop
         File_Car := Xref_File_List_Package.Car (File_Walk);

         if File_Car.Name (File_Car.Name'First ..
                             File_Car.Name'First + File_Car.Name_Length - 1) =
             Filename
         then
            Entities := File_Car.Entities;

            while Entities /= null loop
               Entity_Car := Entity_List_Package.Car (Entities);
                  Callback.all (Entity_Car.all);
               Entities := Entity_List_Package.Cdr (Entities);
            end loop;
         end if;

         File_Walk := Xref_File_List_Package.Cdr (File_Walk);
      end loop;
   end Enumerate_Public_Entities;

   ------------------------
   -- Enumerate_Entities --
   ------------------------

   procedure Enumerate_Entities
     (Xref      : in Xrefs;
      Callback  : in Extended_Callback_Type)
   is
      Files      : Xref_File_List := Xref_File_List (Xref);
      File_Walk  : Xref_File_List := Files;
      File_Car   : Xref_File_Pointer;
      Entities   : Entity_List;
      Entity_Car : Entity_Pointer;
   begin
      --  First determine our File_Number
      while File_Walk /= null loop
         File_Car := Xref_File_List_Package.Car (File_Walk);
         Entities := File_Car.Entities;

         while Entities /= null loop
            Entity_Car := Entity_List_Package.Car (Entities);
            Callback.all
              (File_Car.Name (1 .. File_Car.Name_Length),
               Entity_Car.all);
            Entities := Entity_List_Package.Cdr (Entities);
         end loop;

         File_Walk := Xref_File_List_Package.Cdr (File_Walk);
      end loop;
   end Enumerate_Entities;

   Temp_Entity : aliased Entity;

   ----------
   -- Find --
   ----------

   procedure Find
     (Xref        : in     Xrefs;
      File_Name   : in     String;
      Line        : in     Integer;
      Col         : in     Integer;
      X_File      :    out Xref_File_Pointer;
      X_Entity    :    out Entity_Pointer;
      X_Reference :    out Reference_Pointer;
      Found       :    out Boolean)
   is
      Files          : constant Xref_File_List := Xref_File_List (Xref);
      File_Walk      : Xref_File_List := Files;
      File_Car       : Xref_File_Pointer;
      Entities       : Entity_List;
      Entity_Car     : Entity_Pointer;
      References     : Reference_List;
      Reference_Car  : Reference_Pointer;
      File_Number    : Integer := 0;

      function Find_File (Number : in Integer) return Xref_File_Pointer;

      ---------------
      -- Find_File --
      ---------------

      function Find_File (Number : in Integer) return Xref_File_Pointer is
         File_Walk      : Xref_File_List := Files;
      begin
         while File_Walk /= null loop
            File_Car := Xref_File_List_Package.Car (File_Walk);

            if File_Car.Number = Number then
               return File_Car;
            end if;

            File_Walk := Xref_File_List_Package.Cdr (File_Walk);
         end loop;

         return null;
      end Find_File;

   begin -- Find
      Found := False;

      --  First determine our File_Number
      while File_Walk /= null loop
         File_Car := Xref_File_List_Package.Car (File_Walk);

         exit when String_Equal (File_Car.Name (1 .. File_Car.Name_Length),
                                 File_Name);

         File_Walk := Xref_File_List_Package.Cdr (File_Walk);
      end loop;

      File_Number := File_Car.Number;

      --  See if we have a reference on this line/col/file from
      --  something defined in any file
      File_Walk := Files;

      while not Found and then File_Walk /= null loop
         File_Car := Xref_File_List_Package.Car (File_Walk);
         Entities := File_Car.Entities;

         while not Found and then Entities /= null loop
            Entity_Car := Entity_List_Package.Car (Entities);
            References := Entity_Car.References;

            while not Found and then References /= null loop
               Reference_Car := Reference_List_Package.Car (References);

               if Reference_Car.File_Number = File_Number
                 and then Reference_Car.Line = Line
                 and then Reference_Car.Col = Col
               then
                  Found := True;
                  X_File := File_Car;
                  X_Reference := Reference_Car;
                  X_Entity := Entity_Car;
               end if;

               References := Reference_List_Package.Cdr (References);
            end loop;

            Entities := Entity_List_Package.Cdr (Entities);
         end loop;

         File_Walk := Xref_File_List_Package.Cdr (File_Walk);
      end loop;

      --  Ef not Found yet, try to switch from spec to body
      if not Found then
         --  First determine our File
         File_Car := Find_File (File_Number);

         if File_Car = null then
            return;
         end if;

         Entities := File_Car.Entities;

         while not Found and then Entities /= null loop
            Entity_Car := Entity_List_Package.Car (Entities);

            if Entity_Car.Line = Line and Entity_Car.Col = Col then
               References := Entity_Car.References;

               while not Found and then References /= null loop
                  Reference_Car := Reference_List_Package.Car (References);

                  --  Switch from spec to body
                  if Reference_Car.Ref_Type = 'b' then
                     Found            := True;
                     X_File           := Find_File (Reference_Car.File_Number);
                     Temp_Entity      := Entity_Car.all;
                     Temp_Entity.Line := Reference_Car.Line;
                     Temp_Entity.Col  := Reference_Car.Col;
                     X_Entity         := Temp_Entity'Access;
                     X_Reference      := Reference_Car;
                  end if;

                  References := Reference_List_Package.Cdr (References);
               end loop;
            end if;

            Entities := Entity_List_Package.Cdr (Entities);
         end loop;
      end if;
   end Find;

   --------------
   -- Read_Ali --
   --------------

   --  Create the Xrefs structure from an ALI file
   procedure Read_Ali (File_Name : in String;
                       Xref      : out Xrefs)
   is

      procedure Read_References
        (File        : in     Ada.Text_IO.File_Type;
         First_Char  : in out Character;
         Number      : in     Integer;
         References  : in out Reference_List;
         Param_Modes :    out Mode_String);
      --  Reads the reference list for a particular entity
      --  Alse reads parameters access mode if any

      procedure Read_Entities
        (File       : in     Ada.Text_IO.File_Type;
         First_Char : in out Character;
         Number     : in     Integer;
         Is_Spec    : in     Boolean;
         X_Files    : in     Xref_File_List;
         Entities   :    out Entity_List);
      --  Reads the entity list for a particular file

      procedure Read_Xref_File
        (File       : in     Ada.Text_IO.File_Type;
         First_Char : in out Character;
         X_Files    : in     Xref_File_List;
         X_File     :    out Xref_File_Pointer);
      --  First_Char will have first character on line
      --  after file on completion
      --  should have 'X' on start

      ---------------------
      -- Read_References --
      ---------------------

      procedure Read_References
        (File        : in     Ada.Text_IO.File_Type;
         First_Char  : in out Character;
         Number      : in     Integer;
         References  : in out Reference_List;
         Param_Modes :    out Mode_String)
      is
         X_Reference    : Reference_Pointer;
         Tbd_Number     : Integer;
         Current_Number : Integer := Number;
         Mode_Index     : Natural := Param_Modes'First;
         Dead           : Integer;

      begin
         --  Maybe there were no references?
         if Ada.Text_IO.Col (File => File) = 1
           or else Ada.Text_IO.End_Of_Line (File)
         then
            --  First_Char needs to be first character of next line
            Ada.Text_IO.Get
              (File => File,
               Item => First_Char);

            return;
         end if;

         loop
            --  We expect something of the format
            --  FF|LLrCC where FF is file number, LL line, CC column
            --  or else
            --  LLrCC (if FF is omitted, use previous file number)
            --
            --  there is also an optional [XXX] (undetermined) at end
            File_Helpers.Get_Number
              (File       => File,
               First_Char => First_Char,
               Item       => Tbd_Number);

            if First_Char = '>'
              or else First_Char = '<'
              or else First_Char = '='
              or else First_Char = '^'
            then
               if Mode_Index <= Param_Modes'Last then
                  Param_Modes (Mode_Index) := First_Char;
                  Mode_Index := Mode_Index + 1;
               end if;

               File_Helpers.Get_Number
                 (File       => File,
                  First_Char => First_Char,
                  Item       => Dead);
            else

               X_Reference := new Reference;

               --  If we omit | then number is same as previous
               --  If we have > < =, then these are not references but
               --  parameters
               if First_Char = '|' then
                  X_Reference.File_Number := Tbd_Number;
                  Current_Number := Tbd_Number;

                  File_Helpers.Get_Number
                    (File       => File,
                     First_Char => First_Char,
                     Item       => X_Reference.Line);

               else
                  X_Reference.File_Number := Current_Number;
                  X_Reference.Line        := Tbd_Number;
               end if;

               X_Reference.Ref_Type := First_Char;

               File_Helpers.Get_Number
                 (File       => File,
                  First_Char => First_Char,
                  Item       => X_Reference.Col);

               if First_Char = '[' then
                  while First_Char /= ']' loop
                     Ada.Text_IO.Get
                       (File => File,
                        Item => First_Char);
                  end loop;
               end if;

               Reference_List_Package.Append
                 (Val => X_Reference,
                  Ptr => References);
            end if;

            if Ada.Text_IO.End_Of_File (File => File) then
               return;
            end if;

            if Ada.Text_IO.End_Of_Line (File => File) then
               begin
                  Ada.Text_IO.Skip_Line (File => File);
                  --  This actually consumes first_char, but
                  --  our file_helpers expects that.
                  Ada.Text_IO.Get
                    (File => File,
                     Item => First_Char);

                  --  '.' is the continuation character
                  exit when First_Char /= '.';

               exception
                  --  Note that we might have blank lines at
                  --  the end, so we catch that exception
                  when others =>
                     return;
               end;
            end if;
         end loop;
      end Read_References;

      -------------------
      -- Read_Entities --
      -------------------

      procedure Read_Entities
        (File       : in     Ada.Text_IO.File_Type;
         First_Char : in out Character;
         Number     : in     Integer;
         Is_Spec    : in     Boolean;
         X_Files    : in     Xref_File_List;
         Entities   :    out Entity_List)
      is
         X_Entity        : Entity_Pointer;
         This_Entity     : Entity_Pointer;
         Class_Entity    : Entity_Pointer;
         Bracket_Index   : Natural;
         Is_Parameter    : Boolean;
         First_Parameter : Boolean := True;
         File_Entities   : Entity_List;
         Index           : Natural;
         From_Other_Pkg  : Boolean := False;
         FileObj         : Xref_File_Pointer;

      begin
         Ada.Text_IO.Get
           (File => File,
            Item => First_Char);

         while Ada.Characters.Handling.Is_Digit (First_Char) loop
            X_Entity := new Entity;

            File_Helpers.Get_Number
              (File       => File,
               Item       => X_Entity.Line,
               First_Char => First_Char);

            X_Entity.Description := First_Char;

            Ada.Text_IO.Get
              (File => File,
               Item => First_Char);
            File_Helpers.Get_Number
              (File       => File,
               Item       => X_Entity.Col,
               First_Char => First_Char);

            Is_Parameter := (First_Char /= '*')
              and then (X_Entity.Description /= 'V')
              and then (X_Entity.Description /= 'U')
              and then (X_Entity.Description /= 'N')
              and then (X_Entity.Description /= 'I');

            File_Helpers.Get_String
              (File => File,
               Item => X_Entity.Name,
               Last => X_Entity.Name_Length);

            Bracket_Index := Ada.Strings.Fixed.Index
              (X_Entity.Name (1 .. X_Entity.Name_Length), "=");

            if Bracket_Index > 1 then
               X_Entity.Name_Length := Bracket_Index - 1;
            end if;

            Bracket_Index := Ada.Strings.Fixed.Index
              (X_Entity.Name (1 .. X_Entity.Name_Length), "{");

            if Bracket_Index > 1 then
               X_Entity.Bracket_Start := Bracket_Index + 1;
               X_Entity.Bracket_End   := X_Entity.Name_Length - 1;
               X_Entity.Name_Length   := Bracket_Index - 1;
            end if;

            Bracket_Index := Ada.Strings.Fixed.Index
              (X_Entity.Name (1 .. X_Entity.Name_Length), "(");

            if Bracket_Index > 1 then
               X_Entity.Bracket_Start := Bracket_Index + 1;
               X_Entity.Bracket_End   := X_Entity.Name_Length - 1;
               X_Entity.Name_Length   := Bracket_Index - 1;
            end if;

            --  Check if we have a tagged type?
            if X_Entity.Description = 'R' then
               Class_Entity := X_Entity;

            else
               Class_Entity := null;
            end if;

            Bracket_Index := Ada.Strings.Fixed.Index
              (X_Entity.Name (1 .. X_Entity.Name_Length), "<");

            if Bracket_Index > 1 then
               X_Entity.Bracket_Start := Bracket_Index + 1;
               X_Entity.Bracket_End   := X_Entity.Name_Length - 1;
               X_Entity.Name_Length   := Bracket_Index - 1;
            end if;

            --  Find out the entity's type
            if X_Entity.Bracket_Start > 0 then
               X_Entity.Type_Length := X_Entity.Bracket_End -
                 X_Entity.Bracket_Start + 1;
               X_Entity.Typ (1 .. X_Entity.Type_Length) :=
                 X_Entity.Name
                   (X_Entity.Bracket_Start .. X_Entity.Bracket_End);

               if Ada.Characters.Handling.Is_Digit (X_Entity.Typ (1)) then
                  --  References another entity, that should already be defined

                  Index := 1;
                  From_Other_Pkg := False;

                  if Ada.Strings.Fixed.Index
                    (X_Entity.Typ (1 .. X_Entity.Type_Length), "|") >= 1
                  then
                     --  This type is defined in another file
                     declare
                        FilePtr     : Xref_File_List;
                        File_Number : Natural;

                     begin
                        File_Number := 0;

                        while Ada.Characters.Handling.Is_Digit
                          (X_Entity.Typ (Index))
                        loop
                           File_Number := File_Number * 10;
                           File_Number := File_Number +
                             Character'Pos (X_Entity.Typ (Index)) -
                             Character'Pos ('0');
                           Index := Index + 1;
                        end loop;

                        Index := Index + 1; --  skip the '|' character

                        FilePtr := X_Files;
                        File_Entities := null;

                        while FilePtr /= null loop
                           FileObj := Xref_File_List_Package.Car (FilePtr);

                           if FileObj.Number = File_Number then
                              File_Entities  := FileObj.Entities;
                              From_Other_Pkg := True;
                              exit;
                           end if;

                           FilePtr :=  Xref_File_List_Package.Cdr (FilePtr);
                        end loop;
                     end;

                  else
                     File_Entities := Entities;
                  end if;

                  --  Let's find the type name in the File_Entities list
                  declare
                     The_Entity : Entity_Pointer;
                     The_Node   : Entity_List := File_Entities;
                     Line       : Natural;
                     Col        : Natural;

                  begin
                     Line  := 0;

                     while Ada.Characters.Handling.Is_Digit
                       (X_Entity.Typ (Index))
                     loop
                        Line := Line * 10;
                        Line := Line +
                          Character'Pos (X_Entity.Typ (Index)) -
                          Character'Pos ('0');
                        Index := Index + 1;
                     end loop;

                     Col   := 0;
                     Index := Index + 1;

                     while Ada.Characters.Handling.Is_Digit
                       (X_Entity.Typ (Index))
                     loop
                        Col := Col * 10;
                        Col := Col + Character'Pos (X_Entity.Typ (Index)) -
                          Character'Pos ('0');
                        Index := Index + 1;
                     end loop;

                     while The_Node /= null loop
                        The_Entity := Entity_List_Package.Car (The_Node);

                        if The_Entity.Line = Line
                          and then The_Entity.Col = Col
                        then
                           if From_Other_Pkg then
                              --  Package entity is first in the list of
                              --  entities.
                              declare
                                 Pkg_Entity : constant Entity_Pointer :=
                                    Entity_List_Package.Car (FileObj.Entities);
                              begin
                                 X_Entity.Type_Length :=
                                   Pkg_Entity.Name_Length +
                                   The_Entity.Name_Length + 1;
                                 X_Entity.Typ (1 .. X_Entity.Type_Length) :=
                                   Pkg_Entity.Name
                                     (1 .. Pkg_Entity.Name_Length) & '.' &
                                   The_Entity.Name
                                     (1 .. The_Entity.Name_Length);
                              end;

                           else
                              X_Entity.Type_Length := The_Entity.Name_Length;
                              X_Entity.Typ (1 .. X_Entity.Type_Length) :=
                                The_Entity.Name (1 .. The_Entity.Name_Length);
                           end if;

                           exit;
                        end if;

                        The_Node := Entity_List_Package.Cdr (The_Node);
                     end loop;
                  end;
               end if;
            else
               X_Entity.Type_Length := 0;
            end if;

            if not Is_Parameter then
               This_Entity := X_Entity;
               First_Parameter := True;

            elsif First_Parameter then
               First_Parameter := False;

               --  If we have a current class, and this is bracketed,
               --  and it doesn't have a | (is in current file)
               --  check to see if the first param matches
               if Class_Entity /= null
                 and then X_Entity.Bracket_Start > 0
                 and then Ada.Strings.Fixed.Index
                   (X_Entity.Name
                        (X_Entity.Bracket_Start .. X_Entity.Bracket_End),
                    "|") < X_Entity.Bracket_Start
               then

                  declare
                     Type_Line, Type_Col : Integer;
                     Type_Descr          : Character;
                     Last                : Natural;
                  begin
                     Ada.Integer_Text_IO.Get
                       (From => X_Entity.Name
                          (X_Entity.Bracket_Start .. X_Entity.Bracket_End),
                        Item => Type_Line,
                        Last => Last);
                     Type_Descr := X_Entity.Name (Last + 1);
                     Ada.Integer_Text_IO.Get
                       (From => X_Entity.Name
                          (Last + 2 .. X_Entity.Bracket_End),
                        Item => Type_Col,
                        Last => Last);

                     if Type_Col = Class_Entity.Col
                       and then Type_Line = Class_Entity.Line
                       and then Type_Descr = Class_Entity.Description
                     then
                        Entity_List_Package.Append
                          (Val => This_Entity,
                           Ptr => Class_Entity.Parameters);
                     end if;

                  exception
                     when others =>
                        null;
                  end;
               end if;
            end if;

            X_Entity.References := null;
            Read_References
              (File        => File,
               First_Char  => First_Char,
               Number      => Number,
               References  => X_Entity.References,
               Param_Modes => X_Entity.Param_Modes);

            if not Is_Parameter
              or else This_Entity = null
            then
               Entity_List_Package.Append
                 (Val => X_Entity,
                  Ptr => Entities);
            else
               Entity_List_Package.Append
                 (Val => X_Entity,
                  Ptr => This_Entity.Parameters);
            end if;
         end loop;

      exception
         when Ada.Text_IO.End_Error =>
            null;
      end Read_Entities;

      --------------------
      -- Read_Xref_File --
      --------------------

      procedure Read_Xref_File
        (File       : in     Ada.Text_IO.File_Type;
         First_Char : in out Character;
         X_Files    : in     Xref_File_List;
         X_File     :    out Xref_File_Pointer)
      is
         Is_Spec : Boolean;
      begin
         X_File := new Xref_File;
         Ada.Integer_Text_IO.Get
           (File => File,
            Item => X_File.Number);
         File_Helpers.Get_String
           (File => File,
            Item => X_File.Name,
            Last => X_File.Name_Length);

         if X_File.Name_Length > 4
           and then X_File.Name (X_File.Name_Length - 3 .. X_File.Name_Length)
            = ".ads"
         then
            Is_Spec := True;
         else
            Is_Spec := False;
         end if;

         X_File.Entities := null;

         Read_Entities
           (File       => File,
            First_Char => First_Char,
            Number     => X_File.Number,
            Is_Spec    => Is_Spec,
            X_Files    => X_Files,
            Entities   => X_File.Entities);
      end Read_Xref_File;

      File       : Ada.Text_IO.File_Type;
      First_Char : Character;
      X_Files    : Xref_File_List := null;
      X_File     : Xref_File_Pointer;

   begin -- Read_Ali
      Xref := null;

      Ada.Text_IO.Open
        (File => File,
         Name => File_Name,
         Mode => Ada.Text_IO.In_File);

      --  Skip until the Xref section (marked by having an X
      --  as the first character)
      loop
         Ada.Text_IO.Get
           (File => File,
            Item => First_Char);

         exit when First_Char = 'X';
         Ada.Text_IO.Skip_Line (File => File);
      end loop;

      while First_Char = 'X' loop
         Read_Xref_File
           (File       => File,
            First_Char => First_Char,
            X_Files    => X_Files,
            X_File     => X_File);
         Xref_File_List_Package.Append
           (Val => X_File, Ptr => X_Files);
      end loop;

      Ada.Text_IO.Close (File => File);

      Xref := Xrefs (X_Files);

   exception
      when others =>
         Xref := Xrefs (X_Files);
   end Read_Ali;

   ----------
   -- Free --
   ----------

   procedure Free (References : in out Reference_List)
   is

      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Reference, Reference_Pointer);

      Walk      : Reference_List := References;
      The_Car   : Reference_Pointer;

   begin -- Free
      while Walk /= null loop
         The_Car := Reference_List_Package.Car (Walk);
         Deallocate (The_Car);
         Walk := Reference_List_Package.Cdr (Walk);
      end loop;

      Reference_List_Package.Deallocate (References);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Entities : in out Entity_List) is
      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Entity, Entity_Pointer);

      Walk      : Entity_List := Entities;
      The_Car   : Entity_Pointer;

   begin -- Free
      while Walk /= null loop
         The_Car := Entity_List_Package.Car (Walk);
         Free (Entity_List_Package.Car (Walk).References);
         Deallocate (The_Car);
         Walk := Entity_List_Package.Cdr (Walk);
      end loop;

      Entity_List_Package.Deallocate (Entities);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Xref : in out Xrefs) is

      procedure Deallocate is new Ada.Unchecked_Deallocation
        (Xref_File, Xref_File_Pointer);

      File_List : Xref_File_List := Xref_File_List (Xref);
      Walk      : Xref_File_List := File_List;
      The_Car   : Xref_File_Pointer;

   begin -- Free
      while Walk /= null loop
         The_Car := Xref_File_List_Package.Car (Walk);
         Free (Xref_File_List_Package.Car (Walk).Entities);
         Deallocate (The_Car);
         Walk := Xref_File_List_Package.Cdr (Walk);
      end loop;

      Xref_File_List_Package.Deallocate (File_List);
   end Free;

end MCC.Gnat_Tools.Xref;
