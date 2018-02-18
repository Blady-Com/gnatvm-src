------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    A D A . S E Q U E N T I A L _ I O                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2000-2010, AdaCore                     --
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

--  This is the JGNAT-specific version of the Ada.Sequential_IO body

--  This is the generic template for Sequential_IO, i.e. the code that gets
--  duplicated. We absolutely minimize this code by either calling routines
--  in System.File_IO (for common file functions), in System.Sequential_IO
--  (for specialized Sequential_IO functions), or in the Ada-specific Java
--  class Object_File.

with Interfaces.C_Streams; use Interfaces.C_Streams;
with System;
with System.File_Control_Block;
with System.File_IO;
with Ada.Unchecked_Conversion;

package body Ada.Sequential_IO is

   package FIO renames System.File_IO;
   package FCB renames System.File_Control_Block;
   package SIO renames System.Sequential_IO;

   subtype AP is FCB.AFCB_Ptr;
   subtype FP is SIO.File_Type;
   subtype long is Long_Integer;

   function To_FCB is new Ada.Unchecked_Conversion (File_Mode, FCB.File_Mode);
   function To_SIO is new Ada.Unchecked_Conversion (FCB.File_Mode, File_Mode);

   procedure Set_Index (File : File_Type; To : long);
   --  Sets the file's corresponding JGNAT Object_File index

   function Index (File : File_Type) return long;
   --  Returns the file's corresponding JGNAT Object_File index

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      FIO.Close (AP (File)'Unrestricted_Access);
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "")
   is
   begin
      SIO.Create (FP (File), To_FCB (Mode), Name, Form);
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete (File : in out File_Type) is
   begin
      FIO.Delete (AP (File)'Unrestricted_Access);
   end Delete;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : File_Type) return Boolean is
      function Object_End_Of_File
        (Stream : Interfaces.C_Streams.FILEs)
           return Boolean;
      pragma Import
        (Java, Object_End_Of_File, "jgnat.adalib.Object_File.object_eof");

   begin
      FIO.Check_Read_Status (AP (File));

      return Object_End_Of_File (File.Stream);
   end End_Of_File;

   ----------
   -- Form --
   ----------

   function Form (File : File_Type) return String is
   begin
      return FIO.Form (AP (File));
   end Form;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (File : File_Type) return Boolean is
   begin
      return FIO.Is_Open (AP (File));
   end Is_Open;

   ----------
   -- Mode --
   ----------

   function Mode (File : File_Type) return File_Mode is
   begin
      return To_SIO (FIO.Mode (AP (File)));
   end Mode;

   ----------
   -- Name --
   ----------

   function Name (File : File_Type) return String is
   begin
      return FIO.Name (AP (File));
   end Name;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "")
   is
   begin
      SIO.Open (FP (File), To_FCB (Mode), Name, Form);
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read (File : File_Type; Item : out Element_Type) is

      function Read_Element (Stream : Interfaces.C_Streams.FILEs)
        return System.Address;
      pragma Import
        (Java, Read_Element, "jgnat.adalib.Object_File.read_element");

      type Elt_Access is access Element_Type;

      function Addr_To_Elt is
        new Ada.Unchecked_Conversion (System.Address, Elt_Access);

   begin
      FIO.Check_Read_Status (AP (File));

      Set_Index (File, Index (File));
      Item := Addr_To_Elt (Read_Element (File.Stream)).all;
      Set_Index (File, Index (File) + 1);
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : in out File_Type; Mode : File_Mode) is
   begin
      FIO.Reset (AP (File)'Unrestricted_Access, To_FCB (Mode));
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      FIO.Reset (AP (File)'Unrestricted_Access);
   end Reset;

   -----------
   -- Write --
   -----------

   procedure Write (File : File_Type; Item : Element_Type) is

      procedure Write_Element
        (Stream : Interfaces.C_Streams.FILEs; Item : System.Address);
      pragma Import
        (Java, Write_Element, "jgnat.adalib.Object_File.write_element");

      type Elt_Access is access Element_Type;

      function Elt_To_Addr is
        new Ada.Unchecked_Conversion (Elt_Access, System.Address);

   begin
      FIO.Check_Write_Status (AP (File));

      Set_Index (File, Index (File));
      Write_Element (File.Stream, Elt_To_Addr (new Element_Type'(Item)));
      Set_Index (File, Index (File) + 1);
   end Write;

   ---------------
   -- Set_Index --
   ---------------

   procedure Set_Index (File : File_Type; To : long) is
      procedure Set_File_Index
        (Stream : Interfaces.C_Streams.FILEs;
         Index  : long);
      pragma Import
        (Java, Set_File_Index, "jgnat.adalib.Object_File.set_file_index");

   begin
      Set_File_Index (File.Stream, To);
   end Set_Index;

   -----------
   -- Index --
   -----------

   function Index (File : File_Type) return long is
      function File_Index (Stream : Interfaces.C_Streams.FILEs) return long;
      pragma Import
        (Java, File_Index, "jgnat.adalib.Object_File.file_index");

   begin
      return File_Index (File.Stream);
   end Index;

end Ada.Sequential_IO;
