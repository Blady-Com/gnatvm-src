------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M _ W A L K                              --
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

with Ada.Unchecked_Conversion; use Ada;
with J_Basics;                 use J_Basics;
with J_Table;
with J_Utils;                  use J_Utils;
with JVM_Test;                 use JVM_Test;

package body JVM_Walk is

   --------------------
   -- Local Routines --
   --------------------

   generic
      type Orig_Type    is    (<>);
      type Process_Type is mod <>;
      with procedure Process (D : in out Process_Type) is <>;
   procedure Process_Scalar  (D : in out Orig_Type);
   pragma Inline (Process_Scalar);
   --  Uses unchecked conversion and the Process routine of Process_Type to
   --  process an object of Orig_Type. Orig_Type and Process_Type must have the
   --  same size.

   --------------------
   -- Process_Scalar --
   --------------------

   procedure Process_Scalar (D : in out Orig_Type) is
      function Convert is new Unchecked_Conversion (Orig_Type, Process_Type);
      function Convert is new Unchecked_Conversion (Process_Type, Orig_Type);

      Tmp : Process_Type := Convert (D);

   begin
      Process (Tmp);
      D := Convert (Tmp);
   end Process_Scalar;

   -------------------------
   -- Process_Instruction --
   -------------------------

   procedure Process_Instruction (R : in out Instruction) is

      procedure Process is new Process_Scalar (Int_8,                U1);
      procedure Process is new Process_Scalar (Array_Type,           U1);
      procedure Process is new Process_Scalar (Int_16,               U2);
      procedure Process is new Process_Scalar (CP_Index,             U2);
      procedure Process is new Process_Scalar (Local_Variable_Index, U2);
      procedure Process is new Process_Scalar (Jump_Offset_U2,       U2);
      procedure Process is new Process_Scalar (Int_32,               U4);
      procedure Process is new Process_Scalar (Jump_Offset_U4,       U4);
      --  Needed to process the scalar types occurring in type Instruction

   begin
      Process (R.Op);

      case R.Op is
         when Bipush =>
            Process (R.Byte);

         when Sipush =>
            Process (R.Short);

         when Iinc =>
            --  If Wide_Iinc is not set we must process the following 2 fields
            --  as 8 bit entities.

            if not R.Wide_Iinc then
               Process (U1 (R.Var_Iinc));
               Process (U1 (R.Const_Val));
            else
               Process (R.Var_Iinc);
               Process (R.Const_Val);
            end if;

         when Ret
           |  Iload  | Lload  | Fload  | Dload  | Aload
           |  Istore | Lstore | Fstore | Dstore | Astore =>
            --  If Wide is not set we must process the following fields
            --  as an 8 bit entity.

            if not R.Wide then
               Process (U1 (R.Local_Var));
            else
               Process (R.Local_Var);
            end if;

         when Invokeinterface =>
            Process (R.I_Method);
            Process (R.Nargs);
            Process (R.Unused);

         when Newarray =>
            Process (R.Atype);

         when Multianewarray =>
            Process (R.Array_Class);
            Process (R.Dimensions);

         when Lookupswitch =>
            Process (R.Lookup_Default);
            Process (R.Npairs);

            for J in R.Lookup_Table'Range loop
               Process (R.Lookup_Table (J).Match);
               Process (R.Lookup_Table (J).Offset);
            end loop;

         when Tableswitch =>
            Process (R.Table_Default);
            Process (R.Low);
            Process (R.High);
            for J in R.Jump_Table'Range loop
               Process (R.Jump_Table (J));
            end loop;

         when Ldc =>
            Process (U1 (R.CP_Const_U1));

         when Ldc_W =>
            Process (R.CP_Const_U2);

         when Ldc2_W =>
            Process (R.CP_Const);

         when Jump
           |  Jsr
           |  Ifnull    | Ifnonnull
           |  If_Acmpeq | If_Acmpne
           |  Ifeq      | Ifne
           |  Ifle      | Iflt      | Ifge      | Ifgt
           |  If_Icmpeq | If_Icmpne
           |  If_Icmple | If_Icmplt | If_Icmpge | If_Icmpgt =>
            Process (R.Offset);

         when Jsr_W
           |  Goto_W =>
            Process (R.Offset_U4);

         when Getstatic | Putstatic
           |  Getfield  | Putfield =>
            Process (R.Field);

         when Invokestatic
           |  Invokespecial
           |  Invokevirtual =>
            Process (R.Method);

         when Newobject
           |  Anewarray
           |  Checkcast
           |  Instanceof =>
            Process (R.Class);

         when others =>
            null;
      end case;
   end Process_Instruction;

   --------------------
   -- Process_Class --
   --------------------

   procedure Process_Class (R : in out Class_File; Check : Boolean := False) is

      generic
         with package   P is new J_Table (<>);
         with procedure Process_Elmt (R : in out P.Data);
         Has_U2_Length : Boolean := True;
      procedure Process_Table (T : in out P.Table);
      --  Generic routine to process a Table. The table's length is processed
      --  as a U2 unless Has_U2_Length is False, in which case it is processed
      --  as a U4. After processingthe T's length each of T's elements is
      --  processed in turn by using Process_Elmt.

      procedure Process is new Process_Scalar (CP_Index,             U2);
      procedure Process is new Process_Scalar (Access_Mask,          U2);
      procedure Process is new Process_Scalar (Instruction_Index,    U2);
      procedure Process is new Process_Scalar (Local_Variable_Index, U2);
      --  Instantiations needed to process all scalar types occurring below

      procedure Process_CP_Info          (R : in out CP_Info);
      procedure Process_Handler_Info     (R : in out Handler_Info);
      procedure Process_Line_Info        (R : in out Line_Info);
      procedure Process_Variable_Info    (R : in out Variable_Info);
      procedure Process_Inner_Class_Info (R : in out Inner_Class_Info);
      procedure Process_Code_Attr_Info   (R : in out Code_Attribute_Info);
      procedure Process_Member_Attr_Info (R : in out Member_Attribute_Info);
      procedure Process_Class_Attr_Info  (R : in out Class_Attribute_Info);
      procedure Process_Member_Info      (R : in out Member_Info);
      --  Traversal routines for each of the record types directly or
      --  indirectly included in a Class_File record.

      procedure Skip_Bytes (Nb : U4);
      --  Skip Nb bytes from the current position in the class file

      -------------------
      -- Process_Table --
      -------------------

      procedure Process_Table (T : in out P.Table) is
         procedure Visit (A : in out P.Data_Array);
         --  Does the actual processing for T

         procedure Visit (A : in out P.Data_Array) is
         begin
            for J in A'Range loop
               Process_Elmt (A (J));
            end loop;
         end Visit;

         procedure Process_The_Table is new P.Process;

         L : Nat_32 := 0;

      --  Beginning of Process_Table

      begin
         --  If tables are allocated initialize the length from T

         if P.Allocated (T) then
            L := P.Length (T);
         end if;

         --  Process the table's length before processing T's elements

         if Has_U2_Length then
            Process (U2 (L));
         else
            Process (U4 (L));
         end if;

         --  After having processed T's length see if we need to allocate T

         if not P.Allocated (T) then
            P.Allocate_Fixed_Table (T, L);
         end if;

         --  Now process T's elements

         Process_The_Table (T);
      end Process_Table;

      ---------------------
      -- Process_CP_Info --
      ---------------------

      procedure Process_CP_Info (R : in out CP_Info) is
         procedure Process_Tab is new Process_Table (Utf8, Process);
         --  Needed to process R.Str_Bytes

         New_Tag : CP_Tag := R.Tag;

      begin
         Process (New_Tag);

         if R.Tag /= New_Tag  then
            declare
               Tmp : CP_Info (New_Tag);
            begin
               R := Tmp;
            end;
         end if;

         case R.Tag is
            when CONSTANT_Empty =>
               null;

            when CONSTANT_Class =>
               Process (R.Class_Name_Index);

            when CONSTANT_Fieldref
              |  CONSTANT_Methodref
              |  CONSTANT_Interface_Methodref =>
               Process (R.Class_Index);
               Process (R.Name_And_Type_Index);

            when CONSTANT_String =>
               Process (R.String_Index);

            when CONSTANT_Integer
              |  CONSTANT_Float  =>
               Process (R.Bytes);

            when CONSTANT_Long
              |  CONSTANT_Double =>
               Process (R.High_Bytes);
               Process (R.Low_Bytes);

            when CONSTANT_Name_And_Type =>
               Process (R.Name_Index);
               Process (R.Descriptor_Index);

            when CONSTANT_Utf8 =>
               Process_Tab (R.Str_Bytes);
         end case;
      end Process_CP_Info;

      ----------------------------------
      -- Process_Handler_Handler_Info --
      ----------------------------------

      procedure Process_Handler_Info (R : in out Handler_Info) is
      begin
         Process (R.Start_PC);
         Process (R.End_PC);
         Process (R.Handler_PC);
         Process (R.Catch_Type);
      end Process_Handler_Info;

      -----------------------
      -- Process_Line_Info --
      -----------------------

      procedure Process_Line_Info (R : in out Line_Info) is
      begin
         Process (R.Start_PC);
         Process (R.Line_Number);
      end Process_Line_Info;

      ---------------------------
      -- Process_Variable_Info --
      ---------------------------

      procedure Process_Variable_Info (R : in out Variable_Info) is
      begin
         Process (R.Start_PC);
         Process (R.Length);
         Process (R.Name_Index);
         Process (R.Descriptor_Index);
         Process (R.Index);
      end Process_Variable_Info;

      ------------------------------
      -- Process_Inner_Class_Info --
      ------------------------------

      procedure Process_Inner_Class_Info (R : in out Inner_Class_Info) is
      begin
         Process (R.Inner_Class_Info_Index);
         Process (R.Outer_Class_Info_Index);
         Process (R.Inner_Name_Index);
         Process (R.Inner_Class_Access_Flags);
      end Process_Inner_Class_Info;

      ----------------------------
      -- Process_Code_Attr_Info --
      ----------------------------

      procedure Process_Code_Attr_Info (R : in out Code_Attribute_Info) is
         procedure Process_Tab is new
           Process_Table (Line, Process_Line_Info);
         procedure Process_Tab is new
           Process_Table (Variable, Process_Variable_Info);
         --  Needed to process R.Line_Number_Table and R.Local_Variable_Table

         New_Kind : Attribute_Kind := R.Kind;

      begin
         Process (New_Kind);

         if R.Kind /= New_Kind then
            declare
               Tmp : Code_Attribute_Info (New_Kind);
            begin
               R := Tmp;
            end;
         end if;

         Process (R.Attribute_Name_Index);
         Process (R.Attribute_Length);

         case R.Kind is
            when Attr_Line_Number_Table =>
               Process_Tab (R.Line_Number_Table);

            when Attr_Local_Variable_Table =>
               Process_Tab (R.Local_Variable_Table);

            when others =>
               Skip_Bytes (R.Attribute_Length);
         end case;
      end Process_Code_Attr_Info;

      ------------------------------
      -- Process_Member_Attr_Info --
      ------------------------------

      procedure Process_Member_Attr_Info (R : in out Member_Attribute_Info) is
         procedure Process_Tab is new
           Process_Table (Code_Array, Process, Has_U2_Length => False);
         procedure Process_Tab is new
           Process_Table (Handler, Process_Handler_Info);
         procedure Process_Tab is new
           Process_Table (Code_Attribute, Process_Code_Attr_Info);
         procedure Process_Tab is new
           Process_Table (Class_Index, Process);
         --  Needed to process R.Code, R.Exception_Table, R.Attributes and
         --  R.Exception_Index_Table.

         New_Kind : Attribute_Kind := R.Kind;

      begin
         Process (New_Kind);

         if R.Kind /= New_Kind then
            declare
               Tmp : Member_Attribute_Info (New_Kind);
            begin
               R := Tmp;
            end;
         end if;

         Process (R.Attribute_Name_Index);
         Process (R.Attribute_Length);

         case R.Kind is
            when Attr_Constant_Value =>
               Process (R.Constant_Value_Index);

            when Attr_Synthetic
              |  Attr_Deprecated =>
               null;

            when Attr_Code =>
               Process (R.Max_Stack);
               Process (R.Max_Locals);

               Process_Tab (R.Code);
               Process_Tab (R.Exception_Table);
               Process_Tab (R.Attributes);

            when Attr_Exceptions =>
               Process_Tab (R.Exception_Index_Table);

            when others =>
               Skip_Bytes (R.Attribute_Length);
         end case;
      end Process_Member_Attr_Info;

      -----------------------------
      -- Process_Class_Attr_Info --
      -----------------------------

      procedure Process_Class_Attr_Info (R : in out Class_Attribute_Info) is
         procedure Process_Tab is new
           Process_Table (Inner_Class, Process_Inner_Class_Info);
         --  Needed to process R.Classes

         New_Kind : Attribute_Kind := R.Kind;

      begin
         Process (New_Kind);

         if R.Kind /= New_Kind then
            declare
               Tmp : Class_Attribute_Info (New_Kind);
            begin
               R := Tmp;
            end;
         end if;

         Process (R.Attribute_Name_Index);
         Process (R.Attribute_Length);

         case R.Kind is
            when Attr_Deprecated =>
               null;

            when Attr_Source_File =>
               Process (R.Source_File_Index);

            when Attr_Inner_Classes =>
               Process_Tab (R.Classes);

            when others =>
               Skip_Bytes (R.Attribute_Length);
         end case;
      end Process_Class_Attr_Info;

      -------------------------
      -- Process_Member_Info --
      -------------------------

      procedure Process_Member_Info (R : in out Member_Info) is
         procedure Process_Tab is new
           Process_Table (Member_Attribute, Process_Member_Attr_Info);
         --  Needed to process R.Attributes

      begin
         Process (R.Access_Flags);
         Process (R.Name_Index);
         Process (R.Descriptor_Index);

         Process_Tab (R.Attributes);
      end Process_Member_Info;

      procedure Process_Tab is new
        Process_Table (CP, Process_CP_Info);
      procedure Process_Tab is new
        Process_Table (Class_Index, Process);
      procedure Process_Tab is new
        Process_Table (Member, Process_Member_Info);
      procedure Process_Tab is new
        Process_Table (Class_Attribute, Process_Class_Attr_Info);
      --  Needed to process R.Constant_Pool, R.Interfaces, R.Fields,
      --  R.Methods and R.Attributes.

      ----------------
      -- Skip_Bytes --
      ----------------

      procedure Skip_Bytes (Nb : U4) is
         Ignore : U1 := 0;
      begin
         for J in 1 .. Nb loop
            Process (Ignore);
         end loop;
      end Skip_Bytes;

   --  Beginning of Process_Class

   begin
      Process (R.Magic);
      Process (R.Minor_Version);
      Process (R.Major_Version);

      --  Check that we are indeed dealing with a JVM class file

      if Check then
         if R.Magic /= SUN_Magic_Number then
            Fail ("Bad class file: bad magic number");
         elsif not Supported_Version (R) then
            Fail ("Bad class file: version " &
                        Safe_Image (R.Major_Version) & '.' &
                        Safe_Image (R.Minor_Version) & " not supported");
         end if;
      end if;

      Process_Tab (R.Constant_Pool);

      if Check then
         Check_CP (R.Constant_Pool);
      end if;

      Process (R.Access_Flags);
      Process (R.This_Class);
      Process (R.Super_Class);

      if Check then
         Check_Class_Access_Flags (R.Access_Flags);
         Check_CP_Entry (R.Constant_Pool, R.This_Class, CONSTANT_Class);

         --  class java.lang.Object has no super class

         if Is_Java_Lang_Object (R.Constant_Pool, R.This_Class) then
            Check_CP_Entry (R.Constant_Pool, R.Super_Class, CONSTANT_Empty);
         else
            Check_CP_Entry (R.Constant_Pool, R.Super_Class, CONSTANT_Class);
         end if;
      end if;

      Process_Tab (R.Interfaces);
      Process_Tab (R.Fields);
      Process_Tab (R.Methods);
      Process_Tab (R.Attributes);
   end Process_Class;

end JVM_Walk;
