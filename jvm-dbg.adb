------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J V M . D B G                               --
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

with Atree;    use Atree;
with Debug;    use Debug;
with JVM.Code; use JVM.Code;
with JVM.Info; use JVM.Info;
with J_Basics; use J_Basics;
with J_String; use J_String;
with Output;   use Output;
with Sinput;   use Sinput;

with Jx_Decl;  use Jx_Decl;
with Jx_Ch8;   use Jx_Ch8;

package body JVM.Dbg is

   --  State variables used by Print_Source_Line for source line output.

   Source_Name_Id : Name_Id;
   Src_Stream_Ptr : Stream_Of_U1_Ptr;

   type Line_Table_Ptr is access Line_Table;

   Src_Lines      : Line_Table_Ptr;
   Last_Line_Num  : Nat_32 := 0;

   Previous_File_Index : Source_File_Index := No_Source_File;
   --  The most recent source file index established by Init_Source_Line

   -----------------------------
   -- Init_Source_Line_Output --
   -----------------------------

   procedure Init_Source_Line_Output (Node : Node_Id) is
      File_Index : constant Source_File_Index :=
                     Get_Source_File_Index (Sloc (Node));

   begin
      if Debug_Flag_JJ and then File_Index /= Previous_File_Index then

         --  Set up the source file stream and line table for use by
         --  Print_Source_Line when debugging.

         Source_Name_Id := Name_Id (Full_Debug_Name (File_Index));
         Src_Stream_Ptr := Get_Stream_Of_U1 (Name_String (Source_Name_Id));
         Src_Lines := new Line_Table'(Get_Line_Table (Src_Stream_Ptr.all));

         Previous_File_Index := File_Index;
      end if;
   end Init_Source_Line_Output;

   -----------
   -- Print --
   -----------

   procedure Print (N : Name_Id) is
   begin
      if N = No_Name then
         Write_Str ("<no name>");
      else
         Write_Name (N);
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (S : String) is
   begin
      Write_Str (S);
   end Print;

   -----------------
   -- Print_Class --
   -----------------

   procedure Print_Class (C : Class_Id) is
   begin
      Print (">>> Class: ");
      Print (Name (C));

      Print ("[class_id =");
      Print (C'Img);

      Print ("], superclass = ");

      if Superclass (C) /= Null_Class then
         Print (Name (Superclass (C)));
         Print ("[class_id =");
         Print (Superclass (C)'Img);
      else
         Print ("<<No superclass>>");
      end if;

      Print ("]");

      if Is_Open (C) then
         Print (" (Is_Open)");
      else
         Print (" (not Is_Open)");
      end if;

      Print_Line;

      null;  --  other stuff TBD ???...
   end Print_Class;

   -------------------------
   -- Print_Class_Parents --
   -------------------------

   procedure Print_Class_Parents (Class : Class_Id) is
   begin
      if Class = Null_Class
        or else Class = Java_Lang_Object
      then
         return;
      end if;

      Print_Class_Parents (Superclass (Class));
      Print_Class (Class);
   end Print_Class_Parents;

   -----------------------
   -- Print_Class_Stack --
   -----------------------

   procedure Print_Class_Stack is
   begin
      Print_Line ("****** Print_Class_Stack");

      for J in 1 .. Class_Stack.Num_Elements loop
         Print_Class (Class_Stack.Element (J));
      end loop;
   end Print_Class_Stack;

   -------------------------
   -- Print_Current_Class --
   -------------------------

   procedure Print_Current_Class is
   begin
      Print_Class (Current_Class);
   end Print_Current_Class;

   --------------------------
   -- Print_Current_Method --
   --------------------------

   procedure Print_Current_Method is
   begin
      Print_Method (Current_Method);
   end Print_Current_Method;

   -----------------
   -- Print_Field --
   -----------------

   procedure Print_Field (F : Field_Id) is
   begin
      Print (">>> Field: ");  Print (Name (F));
      Print ("[field_id =");  Print (F'Img);
      Print ("], class = ");  Print (Name (Class (F)));
      Print ("[class_id =");  Print (Class (F)'Img);
      Print ("], type = ");   Print (Name (Type_Of (F)));
      Print (" [type_id = "); Print (Name (Type_Of (F))); Print ("]");
      Print_Line;
   end Print_Field;

   -----------------
   -- Print_Jcode --
   -----------------

   procedure Print_Jcode (M : Method_Id) is
   begin
      --  List all the code of this method

      Print_Jcode (M, 1, Positive'Last);
   end Print_Jcode;

   -----------------
   -- Print_Jcode --
   -----------------

   procedure Print_Jcode (Method : Method_Id; From_Line, To_Line : Positive) is
      Method_Seq  : constant Code_Sequence := Method_Code (Method);
      Last_Line   : constant Natural := Count_Sequence (Method_Seq);
      Last_Line_L : constant Natural := Last_Line'Img'Length;
      Spaces      : constant String := "      ";
      Instr       : Instr_Id;
      Line_Number : Natural := 0;

   begin
      --  Print a header with the name of the method, its formals and local
      --  variables

      declare
         Class : constant Class_Id := Class_Of (Method);
         LV    : Local_Var_Id;

      begin
         Print (">>> ");
         Print (Name (Class));
         Print ("::");
         Print (Name (Method));
         Print (" [class_id =");
         Print (Class'Img);
         Print ("; method_id =");
         Print (Method'Img);
         Print ("]");
         Print_Line;

         LV := First_Local_Var (Method);
         while LV /= Null_Local_Var
           and then Is_Param (LV)
         loop
            Print_Local_Var (LV);
            LV := Next_Local_Var (LV);
         end loop;

         Print_Line ("Local variables:");

         LV := First_Local_Var (Method);
         while LV /= Null_Local_Var
           and then not Is_Param (LV)
         loop
            Print_Local_Var (LV);
            LV := Next_Local_Var (LV);
         end loop;
      end;

      --  List the bytecode of the lines in the specified range

      Instr := First (Method_Seq);
      while Instr /= Null_Instr loop
         Line_Number := Line_Number + 1;

         if Line_Number >= From_Line and Line_Number <= To_Line then
            declare
               Str : constant String := Line_Number'Img;

            begin
               Print (Method'Img & ":");
               Print (Str & ":" & Spaces (1 .. Last_Line_L - Str'Length));
               Print_Instruction (Get (Instr));
            end;
         end if;

         Instr := Get (Instr).Next;
      end loop;

      Print_Line;
   end Print_Jcode;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (S : String := "") is
   begin
      Write_Str (S);
      Write_Eol;
   end Print_Line;

   ---------------------
   -- Print_Local_Var --
   ---------------------

   procedure Print_Local_Var (L : Local_Var_Id) is
   begin
      Print (">>> Local: ");   Print (Name (L));
      Print (" [local_id = "); Print (L'Img);
      Print ("], type = ");    Print (Name (Type_Of (L)));
      Print (" [type_id = ");  Print (Type_Of (L)'Img);  Print ("]");
      Print_Line;

      null;  --  other stuff TBD ???...
   end Print_Local_Var;

   ------------------
   -- Print_Method --
   ------------------

   procedure Print_Method (M : Method_Id) is
   begin
      Print (">>> Method: ");   Print (Name (M));
      Print ("; method_id ="); Print (M'Img);

      if Is_Open (M) then
         Print (" (Is_Open)");
      else
         Print (" (not Is_Open)");
      end if;

      Print (" [class_id =");   Print (Class (M)'Img);   Print ("]");
      Print_Line;

      Print ("Result: ");
      Print_Type (Result_Type (M));

      declare
         LV : Local_Var_Id;
      begin
         LV := First_Local_Var (M);
         while LV /= Null_Local_Var
           and then Is_Param (LV)
         loop
            Print (" Param: ");
            Print_Local_Var (LV);
            LV := Next_Local_Var (LV);
         end loop;
      end;

      null;  --  other stuff TBD ???...
   end Print_Method;

   ------------------
   -- Print_Scopes --
   ------------------

   procedure Print_Scopes is
      S : Entity_Id;

   begin
      Print_Line ("****** Print_VM_Scopes");

      for J in 1 .. JVM_Scope_Stack.Num_Elements loop
         S := JVM_Scope_Stack.Element (J);

         Write_Str ("(");
         Write_Int (Int (S));
         Write_Str (")");

         if Sloc (S) /= No_Location then
            Write_Str (" ");
            Write_Location (Sloc (S));
         end if;

         Write_Eol;
      end loop;
   end Print_Scopes;

   -----------------------
   -- Print_Source_Line --
   -----------------------

   procedure Print_Source_Line (Node : Node_Id) is
      Line_Num : Nat_32;
      Src_Ptr  : constant Source_Ptr := Sloc (Node);

   begin
      if not Debug_Flag_JJ or else Src_Ptr = No_Location then
         return;
      end if;

      Init_Source_Line_Output (Node);

      Line_Num := Nat_32 (Get_Logical_Line_Number (Src_Ptr));

      if Line_Num > 0
        and then Line_Num /= Last_Line_Num
      then
         Last_Line_Num := Line_Num;

         Write_Location (Src_Ptr);
         Write_Str (":  ");
         Write_Str
           (To_String
             (Src_Stream_Ptr
               (Src_Lines (Line_Num).First .. Src_Lines (Line_Num).Last)));
         Write_Eol;
      end if;
   end Print_Source_Line;

   ----------------
   -- Print_Type --
   ----------------

   procedure Print_Type (T : Type_Id) is
   begin
      Print (">>> Type: ");  Print (Name (T));
      Print ("; type_id ="); Print (T'Img);
      Print ("; kind =");    Print (Type_Kind (T)'Img);

      if Is_Array_Descriptor (T) then
         Print ("; Array descriptor");

      elsif Is_Descriptor (T) then
         Print ("; Descriptor");
      end if;

      Print_Line;

      null;  --  other stuff TBD... ???
   end Print_Type;

   ---------
   -- DPC --
   ---------

   procedure DPC (C : Class_Id) is
   begin
      Print_Class (C);
   end DPC;

   ---------
   -- DPF --
   ---------

   procedure DPF (F : Field_Id) is
   begin
      Print_Field (F);
   end DPF;

   ---------
   -- DPJ --
   ---------

   procedure DPJ (M : Method_Id) is
   begin
      Print_Jcode (M);
   end DPJ;

   ---------
   -- DPL --
   ---------

   procedure DPL (L : Local_Var_Id) is
   begin
      Print_Local_Var (L);
   end DPL;

   ---------
   -- DPM --
   ---------

   procedure DPM (M : Method_Id) is
   begin
      Print_Method (M);
   end DPM;

   ---------
   -- DPT --
   ---------

   procedure DPT (T : Type_Id) is
   begin
      Print_Type (T);
   end DPT;

   --  ****************************************************************
   --  **************** Routines for breakpoints **********************
   --  ****************************************************************

   ---------
   -- DIB --
   ---------

   procedure DIB (M : Method_Id; L : Natural) is
      pragma Unreferenced (L);
   begin
      pragma Assert (M /= Null_Method);
      null;
   end DIB;

   ---------
   -- DSB --
   ---------

   procedure DSB (M : Method_Id; E : Natural; T : Type_Id) is
      pragma Unreferenced (M);
      pragma Unreferenced (E);
      pragma Unreferenced (T);
   begin
      null;
   end DSB;

   ---------
   -- DSW --
   ---------

   procedure DSW (M : Method_Id) is
      pragma Unreferenced (M);
   begin
      null;
   end DSW;

end JVM.Dbg;
