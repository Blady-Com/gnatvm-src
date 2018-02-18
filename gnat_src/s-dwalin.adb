------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . D W A R F _ L I N E S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2009-2013, Free Software Foundation, Inc.         --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions.Traceback; use Ada.Exceptions.Traceback;
with Ada.Strings.Bounded;      use Ada.Strings.Bounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Interfaces; use Interfaces;

with System;                   use System;
with System.Address_Image;
with System.IO;                use System.IO;
with System.Object_Reader;     use System.Object_Reader;
with System.Traceback_Entries; use System.Traceback_Entries;

package body System.Dwarf_Lines is

   MAX_STRING_LENGTH : constant := 4096;
   --  This is the maximum size of a traceback string before the output of
   --  Symbolic_Traceback is truncated. This provides for about 50 lines of
   --  80 characters, which is plenty for all but the most pathological cases.
   --  cases.

   package BStrings is new Generic_Bounded_Length (MAX_STRING_LENGTH);
   use BStrings;

   type Word is mod 2 ** Standard'Address_Size;
   --  Modular type wide enough to hold an address

   function To_Addr is
      new Ada.Unchecked_Conversion (Word, System.Address);

   function To_Word is
      new Ada.Unchecked_Conversion (System.Address, Word);

   ---------------------------------
   -- DWARF Parser Implementation --
   ---------------------------------

   procedure Initialize_Pass (C : in out Dwarf_Context);
   --  Seek to the first byte of the first prologue and prepare to make a pass
   --  over the line number entries.

   procedure Initialize_State_Machine (C : in out Dwarf_Context);
   --  Set all state machine registers to their specified initial values

   procedure Parse_Prologue (C : in out Dwarf_Context);
   --  Decode a DWARF statement program prologue

   procedure Read_And_Execute_Isn
     (C    : in out Dwarf_Context;
      Done : out Boolean);
   --  Read an execute a statement program instruction

   function Dir_Code_To_Offset
     (C    : Dwarf_Context;
      Code : uint32) return Offset;
   --  Convert a directory reference to the offset of a null terminated string.
   --  Returns zero on failure.

   function To_File_Name
     (C    : Dwarf_Context;
      Code : uint32) return String;
   --  Extract a file name from the prologue

   function To_Dir_Name
     (C    : Dwarf_Context;
      Code : uint32) return String;
   --  Extract a directory name from the prologue

   function File_Code_To_Offset
     (C    : Dwarf_Context;
      Code : uint32) return Offset;
   --  Convert a file reference to the offset of a null terminated string.
   --  Returns zero on failure.

   type Callback is access procedure (C : Dwarf_Context);
   procedure For_Each_Row (C : out Dwarf_Context; F : Callback);
   --  Traverse each .debug_line entry with a callback

   procedure Dump_Row (C : Dwarf_Context);
   --  Dump a single row

   -----------------------
   --  DWARF constants  --
   -----------------------

   --  6.2.5.2 Standard Opcodes

   DW_LNS_copy               : constant := 1;
   DW_LNS_advance_pc         : constant := 2;
   DW_LNS_advance_line       : constant := 3;
   DW_LNS_set_file           : constant := 4;
   DW_LNS_set_column         : constant := 5;
   DW_LNS_negate_stmt        : constant := 6;
   DW_LNS_set_basic_block    : constant := 7;
   DW_LNS_const_add_pc       : constant := 8;
   DW_LNS_fixed_advance_pc   : constant := 9;
   DW_LNS_set_prologue_end   : constant := 10;
   DW_LNS_set_epilogue_begin : constant := 11;
   DW_LNS_set_isa            : constant := 12;

   --  6.2.5.3 Extended Opcodes

   DW_LNE_end_sequence       : constant := 1;
   DW_LNE_set_address        : constant := 2;
   DW_LNE_define_file        : constant := 3;

   --  From the DWARF version 4 public review draft

   DW_LNE_set_discriminator  : constant := 4;

   -----------
   -- Close --
   -----------

   procedure Close (C : in out Dwarf_Context) is
      procedure Unchecked_Deallocation is new
        Ada.Unchecked_Deallocation (Object_File'Class, Object_File_Access);
   begin
      Close (C.Obj.all);
      Unchecked_Deallocation (C.Obj);
   end Close;

   ------------------------
   -- Dir_Code_To_Offset --
   ------------------------

   function Dir_Code_To_Offset
     (C    : Dwarf_Context;
      Code : uint32) return Offset
   is
      Saved_Off : Offset;
      Off       : Offset;
      Buf       : Buffer;
      J         : uint32;

      Dummy : uint32;
      pragma Unreferenced (Dummy);

   begin
      Tell (C.Obj.all, Saved_Off);
      Seek (C.Obj.all, C.Prologue.Includes_Offset);

      J := 0;
      loop
         J := J + 1;
         Tell (C.Obj.all, Off);
         Read_C_String (C.Obj.all, Buf);

         if Strlen (Buf) = 0 then
            Seek (C.Obj.all, Saved_Off);
            return 0;
         end if;

         exit when J = Code;
      end loop;

      Seek (C.Obj.all, Saved_Off);
      return Off;
   end Dir_Code_To_Offset;

   ----------
   -- Dump --
   ----------

   procedure Dump (C : in out Dwarf_Context) is
   begin
      For_Each_Row (C, Dump_Row'Access);
   end Dump;

   --------------
   -- Dump_Row --
   --------------

   procedure Dump_Row (C : Dwarf_Context) is
      PC : constant Word := Word (C.Registers.Address);

   begin
      Put (System.Address_Image (To_Addr (PC)));
      Put (" ");
      Put (To_File_Name (C, C.Registers.File));
      Put (":");

      declare
         Image : constant String := uint32'Image (C.Registers.Line);
      begin
         Put_Line (Image (2 .. Image'Last));
      end;

   end Dump_Row;

   -------------------------
   -- File_Code_To_Offset --
   -------------------------

   function File_Code_To_Offset
     (C    : Dwarf_Context;
      Code : uint32) return Offset
   is
      Off       : Offset;
      Saved_Off : Offset;
      Buf       : Buffer;
      J         : uint32;

      Dummy : uint32;
      pragma Unreferenced (Dummy);

   begin
      Tell (C.Obj.all, Saved_Off);
      Seek (C.Obj.all, C.Prologue.File_Names_Offset);

      J := 0;
      loop
         J := J + 1;
         Tell (C.Obj.all, Off);
         Read_C_String (C.Obj.all, Buf);

         if Strlen (Buf) = 0 then
            Seek (C.Obj.all, Saved_Off);
            return 0;
         end if;

         Dummy := Read_LEB128 (C.Obj.all);
         Dummy := Read_LEB128 (C.Obj.all);
         Dummy := Read_LEB128 (C.Obj.all);
         exit when J = Code;
      end loop;

      Seek (C.Obj.all, Saved_Off);
      return Off;
   end File_Code_To_Offset;

   ------------------
   -- For_Each_Row --
   ------------------

   procedure For_Each_Row (C : out Dwarf_Context; F : Callback) is
      Done : Boolean;

   begin
      Initialize_Pass (C);

      loop
         Read_And_Execute_Isn (C, Done);

         if C.Registers.Is_Row then
            F.all (C);
         end if;

         exit when Done;
      end loop;
   end For_Each_Row;

   ---------------------
   -- Initialize_Pass --
   ---------------------

   procedure Initialize_Pass (C : in out Dwarf_Context) is
      Sec : Object_Section;

   begin
      if Format (C.Obj.all) = XCOFF32 then
         Sec := Get_Section (C.Obj.all, ".dwline");
      else
         Sec := Get_Section (C.Obj.all, ".debug_line");
      end if;

      if Sec = Null_Section and then C.In_Exception then
         C.Valid := False;

      else
         C.Valid := True;

         C.Next_Prologue := Off (Sec);
         C.End_Of_Section := Off (Sec) + Offset (Size (Sec)) - 1;
         Seek (C.Obj.all, C.Next_Prologue);
         Initialize_State_Machine (C);
      end if;
   end Initialize_Pass;

   ------------------------------
   -- Initialize_State_Machine --
   ------------------------------

   procedure Initialize_State_Machine (C : in out Dwarf_Context) is
      Registers : Line_Info_Registers renames C.Registers;
   begin
      Registers.Address := 0;
      Registers.File := 1;
      Registers.Line := 1;
      Registers.Column := 0;
      Registers.Is_Stmt := C.Prologue.Default_Is_Stmt = 0;
      Registers.Basic_Block := False;
      Registers.End_Sequence := False;
      Registers.Prologue_End := False;
      Registers.Epilouge_Begin := False;
      Registers.ISA := 0;
      Registers.Is_Row := False;
   end Initialize_State_Machine;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (C : Dwarf_Context) return Boolean is
   begin
      return C.Obj /= null;
   end Is_Open;

   ----------
   -- Open --
   ----------

   procedure Open (File_Name : String; C : in out Dwarf_Context) is
   begin
      C.Obj := Open (File_Name, C.In_Exception);
   end Open;

   --------------------
   -- Parse_Prologue --
   --------------------

   procedure Parse_Prologue (C : in out Dwarf_Context) is
      Char : uint8;
      Prev : uint8;
      --  The most recently read character and the one preceding it

      Dummy : uint32;
      pragma Unreferenced (Dummy);
      --  Destination for reads we don't care about

      Buf : Buffer;
      Off : Offset;

      First_Byte_Of_Prologue : Offset;
      Last_Byte_Of_Prologue  : Offset;

      Obj      : Object_File'Class renames C.Obj.all;
      Prologue : Line_Info_Prologue renames C.Prologue;

   begin
      Tell (Obj, First_Byte_Of_Prologue);
      Prologue.Unit_Length := Read (Obj);
      Tell (Obj, Off);
      C.Next_Prologue := Off + Offset (Prologue.Unit_Length);

      Prologue.Version := Read (Obj);
      Prologue.Prologue_Length := Read (Obj);
      Tell (Obj, Last_Byte_Of_Prologue);
      Last_Byte_Of_Prologue :=
        Last_Byte_Of_Prologue + Offset (Prologue.Prologue_Length) - 1;

      Prologue.Min_Isn_Length  := Read (Obj);
      Prologue.Default_Is_Stmt := Read (Obj);
      Prologue.Line_Base       := Read (Obj);
      Prologue.Line_Range      := Read (Obj);
      Prologue.Opcode_Base     := Read (Obj);

      --  Opcode_Lengths is an array of Opcode_Base bytes specifying the
      --  number of LEB128 operands for each of the standard opcodes.

      for J in 1 .. uint32 (Prologue.Opcode_Base - 1) loop
         Prologue.Opcode_Lengths (J) := Read (Obj);
      end loop;

      --  The include directories table follows. This is a list of null
      --  terminated strings terminated by a double null. We only store
      --  its offset for later decoding.

      Tell (Obj, Prologue.Includes_Offset);
      Char := Read (Obj);

      if Char /= 0 then
         loop
            Prev := Char;
            Char := Read (Obj);
            exit when Char = 0 and Prev = 0;
         end loop;
      end if;

      --  The file_names table is next. Each record is a null terminated string
      --  for the file name, an unsigned LEB128 directory index, an unsigned
      --  LEB128 modification time, and an LEB128 file length. The table is
      --  terminated by a null byte.

      Tell (Obj, Prologue.File_Names_Offset);

      loop
         --  Read the filename

         Read_C_String (Obj, Buf);
         exit when Buf (0) = 0;
         Dummy := Read_LEB128 (Obj); --  Skip the directory index.
         Dummy := Read_LEB128 (Obj); --  Skip the modification time.
         Dummy := Read_LEB128 (Obj); --  Skip the file length.
      end loop;

      --  Check we're where we think we are. This sanity check ensures we think
      --  the prologue ends where the prologue says it does. It we aren't then
      --  we've probably gotten out of sync somewhere.

      Tell (Obj, Off);

      if Prologue.Unit_Length /= 0
        and then Off /= Last_Byte_Of_Prologue + 1
      then
         raise Dwarf_Error with "Parse error reading DWARF information";
      end if;
   end Parse_Prologue;

   --------------------------
   -- Read_And_Execute_Isn --
   --------------------------

   procedure Read_And_Execute_Isn
     (C    : in out Dwarf_Context;
      Done : out Boolean)
   is
      Opcode          : uint8;
      Extended_Opcode : uint8;
      uint32_Operand  : uint32;
      int32_Operand   : int32;
      uint16_Operand  : uint16;
      Off             : Offset;

      Extended_Length : uint32;
      pragma Unreferenced (Extended_Length);

      Obj       : Object_File'Class renames C.Obj.all;
      Registers : Line_Info_Registers renames C.Registers;
      Prologue  : Line_Info_Prologue renames C.Prologue;

   begin
      Done := False;
      Registers.Is_Row := False;

      if Registers.End_Sequence then
         Initialize_State_Machine (C);
      end if;

      --  Read the next prologue

      Tell (Obj, Off);
      while Off = C.Next_Prologue loop
         Initialize_State_Machine (C);
         Parse_Prologue (C);
         Tell (Obj, Off);
         exit when Off + 3 >= C.End_Of_Section;
      end loop;

      --  Test whether we're done

      Tell (Obj, Off);

      --  We are finished when we either reach the end of the section,
      --  or we have reached zero padding at the end of the section.

      if Prologue.Unit_Length = 0 or else Off + 3 >= C.End_Of_Section then
         Done := True;
         return;
      end if;

      --  Read and interpret an instruction

      Opcode := Read (Obj);

      --  Extended opcodes

      if Opcode = 0 then
         Extended_Length := Read_LEB128 (Obj);
         Extended_Opcode := Read (Obj);

         case Extended_Opcode is
            when DW_LNE_end_sequence =>

               --  Mark the end of a sequence of source locations

               Registers.End_Sequence := True;
               Registers.Is_Row := True;

            when DW_LNE_set_address =>

               --  Set the program counter to a word

               Registers.Address := Read_Address (Obj);

            when DW_LNE_define_file =>

               --  Not implemented

               raise Dwarf_Error with "DWARF operator not implemented";

            when DW_LNE_set_discriminator =>

               --  Ignored

               int32_Operand := Read_LEB128 (Obj);

            when others =>

               --  Fail on an unrecognized opcode

               raise Dwarf_Error with "DWARF operator not implemented";
         end case;

      --  Standard opcodes

      elsif Opcode < Prologue.Opcode_Base then
         case Opcode is

            --  Append a row to the line info matrix

            when DW_LNS_copy =>
               Registers.Basic_Block := False;
               Registers.Is_Row := True;

            --  Add an unsigned word to the program counter

            when DW_LNS_advance_pc =>
               uint32_Operand := Read_LEB128 (Obj);
               Registers.Address :=
                 Registers.Address +
                 uint64 (uint32_Operand *
                           uint32 (Prologue.Min_Isn_Length));

            --  Add a signed word to the current source line

            when DW_LNS_advance_line =>
               int32_Operand := Read_LEB128 (Obj);
               Registers.Line :=
                 uint32 (int32 (Registers.Line) + int32_Operand);

            --  Set the current source file

            when DW_LNS_set_file =>
               uint32_Operand := Read_LEB128 (Obj);
               Registers.File := uint32_Operand;

            --  Set the current source column

            when DW_LNS_set_column =>
               uint32_Operand := Read_LEB128 (Obj);
               Registers.Column := uint32_Operand;

            --  Toggle the "is statement" flag. GCC doesn't seem to set this???

            when DW_LNS_negate_stmt =>
               Registers.Is_Stmt := not Registers.Is_Stmt;

            --  Mark the beginning of a basic block

            when DW_LNS_set_basic_block =>
               Registers.Basic_Block := True;

            --  Advance the program counter as by the special opcode 255

            when DW_LNS_const_add_pc =>
               Registers.Address :=
                 Registers.Address +
                 uint64
                   (((255 - Prologue.Opcode_Base) / Prologue.Line_Range) *
                      Prologue.Min_Isn_Length);

            --  Advance the program counter by a constant

            when DW_LNS_fixed_advance_pc =>
               uint16_Operand := Read (Obj);
               Registers.Address :=
                 Registers.Address + uint64 (uint16_Operand);

            --  The following are not implemented and ignored

            when DW_LNS_set_prologue_end =>
               null;

            when DW_LNS_set_epilogue_begin =>
               null;

            when DW_LNS_set_isa =>
               null;

            --  Anything else is an error

            when others =>
               raise Dwarf_Error with "DWARF operator not implemented";
         end case;

      --  Decode a special opcode. This is a line and address increment encoded
      --  in a single byte 'special opcode' as described in 6.2.5.1.

      else
         declare
            Address_Increment : int32;
            Line_Increment    : int32;

         begin
            Opcode := Opcode - Prologue.Opcode_Base;

            Address_Increment := int32 (Opcode / Prologue.Line_Range) *
              int32 (Prologue.Min_Isn_Length);
            Line_Increment :=
              int32 (Prologue.Line_Base +
                       int8 (Opcode mod Prologue.Line_Range));

            Registers.Address :=
              Registers.Address + uint64 (Address_Increment);
            Registers.Line := uint32 (int32 (Registers.Line) + Line_Increment);
            Registers.Basic_Block := False;
            Registers.Prologue_End := False;
            Registers.Epilouge_Begin := False;
            Registers.Is_Row := True;
         end;
      end if;

   exception
      when Dwarf_Error =>

         --  In case of errors during parse, just stop reading

         Registers.Is_Row := False;
         Done := True;
   end Read_And_Execute_Isn;

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   function Symbolic_Traceback
     (Cin          : Dwarf_Context;
      Traceback    : Tracebacks_Array;
      Suppress_Hex : Boolean := False) return String
   is
      Done         : Boolean;
      Previous_Row : Line_Info_Registers;
      C            : Dwarf_Context := Cin;
      Result       : Bounded_String;

      --  Tables of matches for the passed array of addresses

      File_Names : array (Traceback'Range) of Offset  := (others => 0);
      Lines      : array (Traceback'Range) of uint32  := (others => 0);
      Matched    : array (Traceback'Range) of Boolean := (others => False);
      Symbols    : array (Traceback'Range) of Object_Symbol;

      procedure Append (Match : Line_Info_Registers; Idx : Integer);
      --  Add an entry to the matched address list

      procedure Build_Return_String;
      --  Construct a human readable string to return to the caller

      procedure Match_And_Collect;
      --  Check whether the current address is one the caller is interested in
      --  and if so collect it for output

      procedure Find_Corresponding_Symbols;
      --  Iterate over each symbol in the symbol table, and for each address in
      --  the traceback try to populate Symbols

      ------------
      -- Append --
      ------------

      procedure Append (Match : Line_Info_Registers; Idx : Integer) is
      begin
         Matched (Idx) := True;
         File_Names (Idx) := File_Code_To_Offset (C, Match.File);
         Lines (Idx) := Match.Line;
      end Append;

      -------------------------
      -- Build_Return_String --
      -------------------------

      procedure Build_Return_String is
      begin

         --  Append a line for each traceback entry

         for J in Traceback'Range loop
            declare
               File_Image : constant String :=
                 Offset_To_String (C.Obj.all, File_Names (J));

               Symbol_Image : constant String :=
                 System.Object_Reader.Decoded_Ada_Name
                   (C.Obj.all, Symbols (J));

               Line_Image : constant String :=
                              uint32'Image (Lines (J));

               Address_Image : constant String :=
                                 "0x" &
                                 System.Address_Image (PC_For (Traceback (J)));

            begin
               BStrings.Append (Result, Address_Image);
               BStrings.Append (Result, " ");

               if Matched (J) then
                  if Symbols (J) /= Null_Symbol then
                     BStrings.Append (Result, Symbol_Image);
                     BStrings.Append (Result, " ");
                  end if;

                  BStrings.Append (Result, "at ");
                  BStrings.Append (Result, File_Image);
                  BStrings.Append (Result, ":");
                  BStrings.Append
                    (Result,
                     Line_Image (2 .. Line_Image'Last));

               else
                  BStrings.Append (Result, " at ???");
               end if;

            end;

            BStrings.Append (Result, ASCII.LF);
         end loop;

         --  Add the raw list of addresses following a blank line

         if not Suppress_Hex then
            BStrings.Append (Result, ASCII.LF);

            for J in Traceback'Range loop
               declare
                  Address_Image : constant String :=
                                    "0x" &
                                    System.Address_Image
                                      (PC_For (Traceback (J)));

               begin
                  BStrings.Append (Result, Address_Image);

                  if J /= Traceback'Last then
                     BStrings.Append (Result, " ");
                  end if;
               end;
            end loop;
         end if;

         BStrings.Append (Result, ASCII.LF);
      end Build_Return_String;

      --------------------------------
      -- Find_Corresponding_Symbols --
      --------------------------------

      procedure Find_Corresponding_Symbols is
         S : Object_Symbol;

      begin
         S := First_Symbol (C.Obj.all);
         while S /= Null_Symbol loop
            for J in Traceback'Range loop
               if Spans (S, uint64 (PC_For (Traceback (J)))) then
                  Symbols (J) := S;
               end if;
            end loop;

            S := Next_Symbol (C.Obj.all, S);
         end loop;
      end Find_Corresponding_Symbols;

      -----------------------
      -- Match_And_Collect --
      -----------------------

      procedure Match_And_Collect is
         Addr : System.Address;

      begin
         for J in Traceback'Range loop
            Addr := PC_For (Traceback (J));

            if not Previous_Row.End_Sequence
              and then To_Word (Addr) >= Word (Previous_Row.Address)
              and then To_Word (Addr) < Word (C.Registers.Address)
            then
               Append (Previous_Row, J);

            elsif To_Word (Addr) = Word (C.Registers.Address) then
               Append (C.Registers, J);
            end if;
         end loop;
      end Match_And_Collect;

   --  Start of processing for Symbolic_Traceback

   begin
      Initialize_Pass (C);

      if not C.Valid then
         --  In this case just return an empty information. The module we
         --  have opened is either in a non supported format or the debug
         --  information is missing.
         return "";
      end if;

      --  Advance to the first entry

      loop
         Read_And_Execute_Isn (C, Done);

         if C.Registers.Is_Row then
            Previous_Row := C.Registers;
            exit;
         end if;

         exit when Done;
      end loop;

      --  Read the rest of the entries

      loop
         Read_And_Execute_Isn (C, Done);

         if C.Registers.Is_Row then
            Match_And_Collect;
            Previous_Row := C.Registers;
         end if;

         exit when Done;
      end loop;

      --  Find the symbols covering the addresses in the traceback

      Find_Corresponding_Symbols;

      Build_Return_String;

      return BStrings.To_String (Result);
   end Symbolic_Traceback;

   -----------------
   -- To_Dir_Name --
   -----------------

   function To_Dir_Name
     (C    : Dwarf_Context;
      Code : uint32) return String
   is
      Old_Off : Offset;
      Off     : Offset;

   begin
      if Code = 0 then
         return "";
      end if;

      Tell (C.Obj.all, Old_Off);
      Off := Dir_Code_To_Offset (C, Code);
      Seek (C.Obj.all, Old_Off);
      return Offset_To_String (C.Obj.all, Off);
   end To_Dir_Name;

   ------------------
   -- To_File_Name --
   ------------------

   function To_File_Name
     (C    : Dwarf_Context;
      Code : uint32) return String
   is
      Old_Off : Offset;
      Off     : Offset;
      Buf     : Buffer;
      Dir_Idx : uint32;
      J       : uint32;

      Mod_Time : uint32;
      pragma Unreferenced (Mod_Time);

      Length : uint32;
      pragma Unreferenced (Length);

   begin
      Tell (C.Obj.all, Old_Off);
      Seek (C.Obj.all, C.Prologue.File_Names_Offset);

      --  Find the entry

      J := 0;
      loop
         J := J + 1;
         Tell (C.Obj.all, Off);
         Read_C_String (C.Obj.all, Buf);

         if Strlen (Buf) = 0 then
            return "???";
         end if;

         Dir_Idx := Read_LEB128 (C.Obj.all);
         Mod_Time := Read_LEB128 (C.Obj.all);
         Length := Read_LEB128 (C.Obj.all);
         exit when J = Code;
      end loop;

      Seek (C.Obj.all, Old_Off);

      declare
         Path : constant String := To_Dir_Name (C, Dir_Idx);
      begin
         if Path'Length > 0 then
            return Path & "/" & To_String (Buf);
         else
            return To_String (Buf);
         end if;
      end;
   end To_File_Name;

end System.Dwarf_Lines;
