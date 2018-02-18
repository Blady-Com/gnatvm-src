------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . O B J E C T _ R E A D E R                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2009-2013, Free Software Foundation, Inc.          --
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

with Ada.Unchecked_Conversion;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C_Streams; use Interfaces.C_Streams;

package body System.Object_Reader is

   SSU : constant := System.Storage_Unit;

   function To_int32 is new Ada.Unchecked_Conversion (uint32, int32);

   function Trim_Trailing_Nuls (Str : String) return String;
   --  Return a copy of a string with any trailing NUL characters truncated

   procedure Read
     (F     : ICS.FILEs;
      Addr  : Address;
      Size  : uint32);
   --  Low-level read procedure

   procedure Seek (F : ICS.FILEs; Off : Offset);
   --  Low-level seek procedure

   function Read (F : ICS.FILEs) return int32;
   --  Low-level read procedure

   -------------------------------------
   -- ELF object file format handling --
   -------------------------------------

   generic
      type uword is mod <>;

   package ELF_Ops is

      --  ELF version codes

      ELFCLASS32 : constant :=  1;  --  32 bit ELF
      ELFCLASS64 : constant :=  2;  --  64 bit ELF

      --  ELF machine codes

      EM_NONE        : constant :=  0; --  No machine
      EM_SPARC       : constant :=  2; --  SUN SPARC
      EM_386         : constant :=  3; --  Intel 80386
      EM_MIPS        : constant :=  8; --  MIPS RS3000 Big-Endian
      EM_MIPS_RS3_LE : constant := 10; --  MIPS RS3000 Little-Endian
      EM_SPARC32PLUS : constant := 18; --  Sun SPARC 32+
      EM_PPC         : constant := 20; --  PowerPC
      EM_PPC64       : constant := 21; --  PowerPC 64-bit
      EM_ARM         : constant := 40; --  ARM
      EM_SPARCV9     : constant := 43; --  SPARC v9 64-bit
      EM_IA_64       : constant := 50; --  Intel Merced
      EM_X86_64      : constant := 62; --  AMD x86-64 architecture

      EN_NIDENT  : constant := 16;

      type E_Ident_Type is array (0 .. EN_NIDENT - 1) of uint8;

      type Header is record
         E_Ident     : E_Ident_Type; -- Magic number and other info
         E_Type      : uint16;       -- Object file type
         E_Machine   : uint16;       -- Architecture
         E_Version   : uint32;       -- Object file version
         E_Entry     : uword;        -- Entry point virtual address
         E_Phoff     : uword;        -- Program header table file offset
         E_Shoff     : uword;        -- Section header table file offset
         E_Flags     : uint32;       -- Processor-specific flags
         E_Ehsize    : uint16;       -- ELF header size in bytes
         E_Phentsize : uint16;       -- Program header table entry size
         E_Phnum     : uint16;       -- Program header table entry count
         E_Shentsize : uint16;       -- Section header table entry size
         E_Shnum     : uint16;       -- Section header table entry count
         E_Shstrndx  : uint16;       -- Section header string table index
      end record;

      type Section_Header is record
         Sh_Name      : uint32; -- Section name string table index
         Sh_Type      : uint32; -- Section type
         Sh_Flags     : uword;  -- Section flags
         Sh_Addr      : uword;  -- Section virtual addr at execution
         Sh_Offset    : uword;  -- Section file offset
         Sh_Size      : uword;  -- Section size in bytes
         Sh_Link      : uint32; -- Link to another section
         Sh_Info      : uint32; -- Additional section information
         Sh_Addralign : uword;  -- Section alignment
         Sh_Entsize   : uword;  -- Entry size if section holds table
      end record;

      type Symtab_Entry32 is record
         St_Name  : uint32;  --  Name (string table index)
         St_Value : uint32;  --  Value
         St_Size  : uint32;  --  Size in bytes
         St_Info  : uint8;   --  Type and binding attributes
         St_Other : uint8;   --  Undefined
         St_Shndx : uint16;  --  Defining section
      end record;

      type Symtab_Entry64 is record
         St_Name  : uint32;  --  Name (string table index)
         St_Info  : uint8;   --  Type and binding attributes
         St_Other : uint8;   --  Undefined
         St_Shndx : uint16;  --  Defining section
         St_Value : uint64;  --  Value
         St_Size  : uint64;  --  Size in bytes
      end record;

      function Read_Header (F : ICS.FILEs) return Header;
      --  Read a header from an ELF format object

      type ELF_Object_File is new Object_File with private;

      function First_Symbol
        (Obj : in out ELF_Object_File) return Object_Symbol;
      --  Return the first element in the symbol table, or Null_Symbol if the
      --  symbol table is empty.

      function Next_Symbol
        (Obj  : in out ELF_Object_File;
         Prev : Object_Symbol) return Object_Symbol;
      --  Return the element following Prev in the symbol table, or Null_Symbol
      --  if Prev is the last symbol in the table.

      function Name
        (Obj : ELF_Object_File;
         Sym : Object_Symbol) return String;
      --  Return the name of the symbol

      function Name
        (Obj : ELF_Object_File;
         Sec : Object_Section) return String;
      --  Return the name of a section

      function Get_Section
        (Obj   : ELF_Object_File;
         Shnum : uint32) return Object_Section;
      --  Fetch a section by index from zero

      function Initialize
        (F            : ICS.FILEs;
         Hdr          : Header;
         In_Exception : Boolean) return ELF_Object_File;
      --  Initialize an object file

   private
      type ELF_Object_File is new Object_File with record
         Sectab : Offset := 0;  --  Offset of the sections table
         Strtab : Offset := 0;  --  Offset of the string table
         Symtab : Offset := 0;  --  Offset of the symbol table
      end record;
   end ELF_Ops;

   -----------------------------------
   -- PECOFF object format handling --
   -----------------------------------

   package PECOFF_Ops is

      --  Constants and data layout are taken from the document "Microsoft
      --  Portable Executable and Common Object File Format Specification"
      --  Revision 8.1.

      Signature_Loc_Offset : constant := 16#3C#;
      --  Offset of pointer to the file signature

      Size_Of_Standard_Header_Fields : constant := 16#18#;
      --  Length in bytes of the standard header record

      Function_Symbol_Type : constant := 16#20#;
      --  Type field value indicating a symbol refers to a function

      Not_Function_Symbol_Type : constant := 16#00#;
      --  Type field value indicating a symbol does not refer to a function

      Image_Load_Address : constant := 16#400000#;
      --  Base image load address for Windows 95 through Vista

      type Magic_Array is array (0 .. 3) of uint8;
      --  Array of magic numbers from the header

      --  Magic numbers for PECOFF variants

      VARIANT_PE32      : constant := 16#010B#;
      VARIANT_PE32_PLUS : constant := 16#020B#;

      --  PECOFF machine codes

      IMAGE_FILE_MACHINE_I386  : constant := 16#014C#;
      IMAGE_FILE_MACHINE_IA64  : constant := 16#0200#;
      IMAGE_FILE_MACHINE_AMD64 : constant := 16#8664#;

      --  PECOFF Data layout

      type Header is record
         Magics               : Magic_Array;
         Machine              : uint16;
         NumberOfSections     : uint16;
         TimeDateStamp        : uint32;
         PointerToSymbolTable : uint32;
         NumberOfSymbols      : uint32;
         SizeOfOptionalHeader : uint16;
         Characteristics      : uint16;
         Variant              : uint16;
      end record;

      pragma Pack (Header);

      subtype Name_Str is String (1 .. 8);

      type Section_Header is record
         Name                 : Name_Str;
         VirtualSize          : uint32;
         VirtualAddress       : uint32;
         SizeOfRawData        : uint32;
         PointerToRawData     : uint32;
         PointerToRelocations : uint32;
         PointerToLinenumbers : uint32;
         NumberOfRelocations  : uint16;
         NumberOfLinenumbers  : uint16;
         Characteristics      : uint32;
      end record;

      pragma Pack (Section_Header);

      type Symtab_Entry is record
         Name                  : Name_Str;
         Value                 : uint32;
         SectionNumber         : int16;
         TypeField             : uint16;
         StorageClass          : uint8;
         NumberOfAuxSymbols    : uint8;
      end record;

      pragma Pack (Symtab_Entry);

      type Auxent_Section is record
         Length              : uint32;
         NumberOfRelocations : uint16;
         NumberOfLinenumbers : uint16;
         CheckSum            : uint32;
         Number              : uint16;
         Selection           : uint8;
         Unused1             : uint8;
         Unused2             : uint8;
         Unused3             : uint8;
      end record;

      for Auxent_Section'Size use 18 * 8;

      function Read_Header (F : ICS.FILEs) return Header;
      --  Read the object file header

      type PECOFF_Object_File is new Object_File with private;

      function First_Symbol
        (Obj : in out PECOFF_Object_File) return Object_Symbol;
      --  Return the first element in the symbol table, or Null_Symbol if the
      --  symbol table is empty.

      function Next_Symbol
        (Obj  : in out PECOFF_Object_File;
         Prev : Object_Symbol) return Object_Symbol;
      --  Return the element following Prev in the symbol table or Null_Symbol
      --  if Prev is the last symbol in the table.

      function Name
        (Obj : PECOFF_Object_File;
         Sym : Object_Symbol) return String;
      --  Return the name of the symbol

      function Name
        (Obj : PECOFF_Object_File;
         Sec : Object_Section) return String;
      --  Return the name of a section

      function Get_Section
        (Obj   : PECOFF_Object_File;
         Index : uint32) return Object_Section;
      --  Fetch a section by index from zero

      function Initialize
        (F            : ICS.FILEs;
         Hdr          : Header;
         In_Exception : Boolean) return PECOFF_Object_File;
      --  Initialize an object file

   private
      type PECOFF_Object_File is new Object_File with record
         Symtab      : Offset := 0;  --  Offset of the symbol table
         Symtab_Last : Offset := 0;  --  Offset past the symbol table
         Sectab      : Offset := 0;  --  Offset of the section table

         --  Cache for latest result of Get_Section_Virtual_Address

         GSVA_Sec  : uint32 := uint32'Last;
         GSVA_Addr : uint64;
      end record;
   end PECOFF_Ops;

   -------------------------------------
   -- XCOFF-32 object format handling --
   -------------------------------------

   package XCOFF32_Ops is

      --  XCOFF Data layout

      type Header is record
         f_magic  : uint16;
         f_nscns  : uint16;
         f_timdat : uint32;
         f_symptr : uint32;
         f_nsyms  : uint32;
         f_opthdr : uint16;
         f_flags  : uint16;
      end record;

      subtype Name_Str is String (1 .. 8);

      type Section_Header is record
         s_name    : Name_Str;
         s_paddr   : uint32;
         s_vaddr   : uint32;
         s_size    : uint32;
         s_scnptr  : uint32;
         s_relptr  : uint32;
         s_lnnoptr : uint32;
         s_nreloc  : uint16;
         s_nlnno   : uint16;
         s_flags   : uint32;
      end record;

      pragma Pack (Section_Header);

      type Symbol_Entry is record
         n_name   : Name_Str;
         n_value  : uint32;
         n_scnum  : uint16;
         n_type   : uint16;
         n_sclass : uint8;
         n_numaux : uint8;
      end record;
      for Symbol_Entry'Size use 18 * 8;

      type Aux_Entry is record
         x_scnlen   : uint32;
         x_parmhash : uint32;
         x_snhash   : uint16;
         x_smtyp    : uint8;
         x_smclass  : uint8;
         x_stab     : uint32;
         x_snstab   : uint16;
      end record;
      for Aux_Entry'Size use 18 * 8;

      pragma Pack (Aux_Entry);

      C_EXT     : constant := 2;
      C_HIDEXT  : constant := 107;
      C_WEAKEXT : constant := 111;

      XTY_LD : constant := 2;
      --  Magic constant should be documented, especially since it's changed???

      function Read_Header (F : ICS.FILEs) return Header;
      --  Read the object file header

      type XCOFF32_Object_File is new Object_File with private;

      function First_Symbol (Obj : in out XCOFF32_Object_File)
                            return Object_Symbol;
      --  Return the first element in the symbol table, or Null_Symbol if the
      --  symbol table is empty.

      function Next_Symbol
        (Obj  : in out XCOFF32_Object_File;
         Prev : Object_Symbol) return Object_Symbol;
      --  Return the element following Prev in the symbol table or Null_Symbol
      --  if Prev is the last symbol in the table.

      function Name
        (Obj : XCOFF32_Object_File;
         Sym : Object_Symbol) return String;
      --  Return the name of the symbol

      function Name
        (Obj : XCOFF32_Object_File;
         Sec : Object_Section) return String;
      --  Return the name of a section

      function Initialize
        (F            : ICS.FILEs;
         Hdr          : Header;
         In_Exception : Boolean) return XCOFF32_Object_File;
      --  Initialize an object file

      function Get_Section
          (Obj   : XCOFF32_Object_File;
           Index : uint32) return Object_Section;
      --  Fetch a section by index from zero

   private
      type XCOFF32_Object_File is new Object_File with record
         Sectab : Offset := 0;  --  Offset of the section table XCOFF
         Symtab : Offset := 0;  --  Offset of the symbol table All
      end record;
   end XCOFF32_Ops;

   -------------
   -- ELF_Ops --
   -------------

   package body ELF_Ops is

      function Get_String_Table (Obj : ELF_Object_File) return Object_Section;
      --  Fetch the the section containing the string table

      function Get_Symbol_Table (Obj : ELF_Object_File) return Object_Section;
      --  Fetch the the section containing the symbol table

      function Read_Section_Header
        (Obj   : ELF_Object_File;
         Shnum : uint32) return Section_Header;
      --  Read the header for an ELF format object section indexed from zero

      function Read_Symbol
        (Obj : ELF_Object_File;
         Off : Offset;
         Num : uint64) return Object_Symbol;
      --  Read a symbol at offset Off

      ------------------
      -- First_Symbol --
      ------------------

      function First_Symbol
        (Obj : in out ELF_Object_File) return Object_Symbol
      is
      begin
         if Obj.Num_Symbols = 0 then
            return Null_Symbol;
         end if;

         return Read_Symbol (Obj, Obj.Symtab, 0);
      end First_Symbol;

      -----------------
      -- Get_Section --
      -----------------

      function Get_Section
        (Obj   : ELF_Object_File;
         Shnum : uint32) return Object_Section
      is
         SHdr : constant Section_Header := Read_Section_Header (Obj, Shnum);
      begin
         return (Shnum, Offset (SHdr.Sh_Offset), uint64 (SHdr.Sh_Size));
      end Get_Section;

      ------------------------
      --  Get_String_Table  --
      ------------------------

      function Get_String_Table
        (Obj : ELF_Object_File) return Object_Section
      is
      begin
         --  All cases except MIPS IRIX, string table located in .strtab

         if Obj.Arch /= MIPS then
            return Get_Section (Obj, ".strtab");

         --  On IRIX only .dynstr is available

         else
            return Get_Section (Obj, ".dynstr");
         end if;
      end Get_String_Table;

      ------------------------
      --  Get_Symbol_Table  --
      ------------------------

      function Get_Symbol_Table
        (Obj : ELF_Object_File) return Object_Section
      is
      begin
         --  All cases except MIPS IRIX, symbol table located in .symtab

         if Obj.Arch /= MIPS then
            return Get_Section (Obj, ".symtab");

         --  On IRIX, symbol table located somewhere other than .symtab

         else
            return Get_Section (Obj, ".dynsym");
         end if;
      end Get_Symbol_Table;

      ----------------
      -- Initialize --
      ----------------

      function Initialize
        (F            : ICS.FILEs;
         Hdr          : Header;
         In_Exception : Boolean) return ELF_Object_File
      is
         Res : ELF_Object_File;
         Sec : Object_Section;

      begin
         Res.fp := F;
         Res.In_Exception := In_Exception;

         Res.Num_Sections := uint32 (Hdr.E_Shnum);
         Res.Sectab := Offset (Hdr.E_Shoff);
         Res.Strtab := Get_String_Table (Res).Off;
         Sec := Get_Symbol_Table (Res);
         Res.Symtab := Sec.Off;

         case Hdr.E_Machine is
            when EM_SPARC        |
              EM_SPARC32PLUS     =>
               Res.Arch := SPARC;
            when EM_386          =>
               Res.Arch := i386;
            when EM_MIPS         |
              EM_MIPS_RS3_LE     =>
               Res.Arch := MIPS;
            when EM_PPC          =>
               Res.Arch := PPC;
            when EM_PPC64        =>
               Res.Arch := PPC64;
            when EM_SPARCV9      =>
               Res.Arch := SPARC64;
            when EM_IA_64        =>
               Res.Arch := IA64;
            when EM_X86_64       =>
               Res.Arch := x86_64;
            when others          =>
               raise Format_Error with "unrecognized architecture";
         end case;

         case uword'Size is
            when 64 =>
               Res.Format := ELF64;
               Res.Num_Symbols := Sec.Size / (Symtab_Entry64'Size / SSU);
            when 32 =>
               Res.Format := ELF32;
               Res.Num_Symbols := Sec.Size / (Symtab_Entry32'Size / SSU);
            when others =>
               raise Program_Error;
         end case;

         return Res;
      end Initialize;

      ------------------
      -- Next_Symbol --
      ------------------

      function Next_Symbol
        (Obj  : in out ELF_Object_File;
         Prev : Object_Symbol) return Object_Symbol
      is
      begin
         if Prev.Num = Obj.Num_Symbols - 1 then

            --  Return Null_Symbol if Prev is the last entry in the table

            return Null_Symbol;

         else
            --  Otherwise read the next symbol in the table and return it

            return Read_Symbol (Obj, Prev.Next, Prev.Num + 1);
         end if;
      end Next_Symbol;

      -----------------
      -- Read_Header --
      -----------------

      function Read_Header (F : ICS.FILEs) return Header is
         Hdr : Header;
      begin
         Seek (F, 0);
         Read (F, Hdr'Address, uint32 (Hdr'Size / SSU));
         return Hdr;
      end Read_Header;

      -------------------------
      -- Read_Section_Header --
      -------------------------

      function Read_Section_Header
        (Obj   : ELF_Object_File;
         Shnum : uint32) return Section_Header
      is
         Shdr : Section_Header;
      begin
         Seek (Obj, Obj.Sectab + Offset (Shnum * Section_Header'Size / SSU));
         Read (Obj, Shdr'Address, Section_Header'Size / SSU);
         return Shdr;
      end Read_Section_Header;

      -----------------
      -- Read_Symbol --
      -----------------

      function Read_Symbol
        (Obj : ELF_Object_File;
         Off : Offset;
         Num : uint64) return Object_Symbol
      is
         Old_Off    : Offset;
         ST_Entry32 : Symtab_Entry32;
         ST_Entry64 : Symtab_Entry64;
         Res        : Object_Symbol;

      begin
         Tell (Obj, Old_Off);
         Seek (Obj, Off);

         case uword'Size is
            when 32 =>
               Read (Obj, ST_Entry32'Address,
                     uint32 (ST_Entry32'Size / SSU));
               Res := (Num,
                       Off,
                       Off + ST_Entry32'Size / SSU,
                       uint64 (ST_Entry32.St_Value),
                       uint64 (ST_Entry32.St_Size));
            when 64 =>
               Read (Obj, ST_Entry64'Address,
                     uint32 (ST_Entry64'Size / SSU));
               Res := (Num,
                       Off,
                       Off + ST_Entry64'Size / SSU,
                       ST_Entry64.St_Value,
                       ST_Entry64.St_Size);
            when others =>
               raise Program_Error;
         end case;

         Seek (Obj, Old_Off);
         return Res;
      end Read_Symbol;

      ----------
      -- Name --
      ----------

      function Name
        (Obj : ELF_Object_File;
         Sec : Object_Section) return String
      is
         Old_Off        : Offset;
         Name_Offset    : Offset;
         Hdr            : Header;
         SHdr           : Section_Header;
         String_Tbl_Hdr : Section_Header;

      begin
         Tell (Obj, Old_Off);
         Hdr := Read_Header (Obj.fp);
         SHdr := Read_Section_Header (Obj, Sec.Num);
         String_Tbl_Hdr := Read_Section_Header (Obj, uint32 (Hdr.E_Shstrndx));
         Name_Offset :=
           Offset (String_Tbl_Hdr.Sh_Offset + uword (SHdr.Sh_Name));
         Seek (Obj, Old_Off);
         return Offset_To_String (Obj, Name_Offset);
      end Name;

      function Name
        (Obj : ELF_Object_File;
         Sym : Object_Symbol) return String
      is
         Old_Off    : Offset;
         ST_Entry32 : Symtab_Entry32;
         ST_Entry64 : Symtab_Entry64;
         Name_Off   : Offset;

      begin
         --  Test that this symbol is not null

         if Sym = Null_Symbol then
            return "";
         end if;

         --  Read the symbol table entry

         Tell (Obj, Old_Off);
         Seek (Obj, Sym.Off);

         case uword'Size is
            when 32 =>
               Read (Obj, ST_Entry32'Address,
                     uint32 (ST_Entry32'Size / SSU));
               Name_Off := Offset (ST_Entry32.St_Name);

            when 64 =>
               Read (Obj, ST_Entry64'Address,
                     uint32 (ST_Entry64'Size / SSU));
               Name_Off := Offset (ST_Entry64.St_Name);

            when others =>
               raise Program_Error;
         end case;

         Seek (Obj, Old_Off);

         --  Fetch the name from the string table

         return Offset_To_String (Obj, Obj.Strtab + Name_Off);

      end Name;

   end ELF_Ops;

   package ELF32_Ops is new ELF_Ops (uint32);
   package ELF64_Ops is new ELF_Ops (uint64);

   ----------------
   -- PECOFF_Ops --
   ----------------

   package body PECOFF_Ops is

      function Decode_Name
        (Obj      : PECOFF_Object_File;
         Raw_Name : String) return String;
      --  A section name is an 8 byte field padded on the right with null
      --  characters, or a '\' followed by an ASCII decimal string indicating
      --  an offset in to the string table. This routine decodes this

      function Get_Section_Virtual_Address
        (Obj   : in out PECOFF_Object_File;
         Index : uint32) return uint64;
      --  Fetch the address at which a section is loaded

      function Read_Section_Header
        (Obj   : PECOFF_Object_File;
         Index : uint32) return Section_Header;
      --  Read a header from section table

      function Read_Symbol
        (Obj  : in out PECOFF_Object_File;
         Off : Offset; Num : uint64) return Object_Symbol;
      --  Read a symbol at offset Off.

      function String_Table
        (Obj   : PECOFF_Object_File;
         Index : Offset) return String;
      --  Return an entry from the string table

      -----------------
      -- Decode_Name --
      -----------------

      function Decode_Name
        (Obj      : PECOFF_Object_File;
         Raw_Name : String) return String
      is
         Name_Or_Ref : constant String := Trim_Trailing_Nuls (Raw_Name);
         Off         : Offset;

      begin
         --  We should never find a symbol with a zero length name. If we do it
         --  probably means we are not parsing the symbol table correctly. If
         --  this happens we raise a fatal error.

         if Name_Or_Ref'Length = 0 then
            raise Format_Error with
              "found zero length symbol in symbol table";
         end if;

         if Name_Or_Ref (1) /= '/' then
            return Name_Or_Ref;
         else
            Off := Offset'Value (Name_Or_Ref (2 .. Name_Or_Ref'Last));
            return String_Table (Obj, Off);
         end if;
      end Decode_Name;

      ------------------
      -- First_Symbol --
      ------------------

      function First_Symbol
        (Obj : in out PECOFF_Object_File) return Object_Symbol is
      begin
         --  Return Null_Symbol in the case that the symbol table is empty

         if Obj.Symtab >= Obj.Symtab_Last then
            return Null_Symbol;
         end if;

         return Read_Symbol (Obj, Obj.Symtab, 0);
      end First_Symbol;

      -----------------
      -- Get_Section --
      -----------------

      function Get_Section
        (Obj   : PECOFF_Object_File;
         Index : uint32) return Object_Section
      is
         Sec : constant Section_Header := Read_Section_Header (Obj, Index);
      begin
         return (Index,
                 Offset (Sec.PointerToRawData),
                 uint64 (Sec.SizeOfRawData));
      end Get_Section;

      ---------------------------------
      -- Get_Section_Virtual_Address --
      ---------------------------------

      function Get_Section_Virtual_Address
        (Obj   : in out PECOFF_Object_File;
         Index : uint32) return uint64
      is
         Sec : Section_Header;

      begin
         --  Try cache

         if Index = Obj.GSVA_Sec then
            return Obj.GSVA_Addr;
         end if;

         Obj.GSVA_Sec := Index;
         Sec := Read_Section_Header (Obj, Index);
         Obj.GSVA_Addr := Image_Load_Address + uint64 (Sec.VirtualAddress);
         return Obj.GSVA_Addr;
      end Get_Section_Virtual_Address;

      ----------------
      -- Initialize --
      ----------------

      function Initialize
        (F            : ICS.FILEs;
         Hdr          : Header;
         In_Exception : Boolean) return PECOFF_Object_File
      is
         Res : PECOFF_Object_File;

      begin
         Res.fp := F;
         Res.In_Exception := In_Exception;

         case Hdr.Variant is
            when PECOFF_Ops.VARIANT_PE32 =>
               Res.Format := PECOFF;
            when PECOFF_Ops.VARIANT_PE32_PLUS =>
               Res.Format := PECOFF_PLUS;
            when others =>
               raise Format_Error with "unrecognized PECOFF variant";
         end case;

         case Hdr.Machine is
            when PECOFF_Ops.IMAGE_FILE_MACHINE_I386  =>
               Res.Arch := i386;
            when PECOFF_Ops.IMAGE_FILE_MACHINE_IA64  =>
               Res.Arch := IA64;
            when PECOFF_Ops.IMAGE_FILE_MACHINE_AMD64 =>
               Res.Arch := x86_64;
            when others =>
               raise Format_Error with "unrecognized architecture";
         end case;

         Res.Num_Symbols  := uint64 (Hdr.NumberOfSymbols);
         Res.Num_Sections := uint32 (Hdr.NumberOfSections);
         Res.Symtab       := Offset (Hdr.PointerToSymbolTable);
         Res.Symtab_Last  := Res.Symtab
           + Offset (Hdr.NumberOfSymbols) * (Symtab_Entry'Size / SSU);

         --  Save some offsets

         Seek (Res, Signature_Loc_Offset);
         Res.Sectab := Offset (uint32'(Read (Res)))
           + Size_Of_Standard_Header_Fields
           + Offset (Hdr.SizeOfOptionalHeader);

         return Res;
      end Initialize;

      ------------------
      -- Next_Symbol --
      ------------------

      function Next_Symbol
        (Obj  : in out PECOFF_Object_File;
         Prev : Object_Symbol) return Object_Symbol is
      begin
         --  Test whether we've reached the end of the symbol table

         if Prev.Next >= Obj.Symtab_Last then
            return Null_Symbol;
         end if;

         return Read_Symbol (Obj, Prev.Next, Prev.Num);
      end Next_Symbol;

      -----------------
      -- Read_Symbol --
      -----------------

      function Read_Symbol
        (Obj : in out PECOFF_Object_File;
         Off : Offset;
         Num : uint64) return Object_Symbol
      is
         ST_Entry  : Symtab_Entry;
         ST_Last   : Symtab_Entry;
         Aux_Entry : Auxent_Section;
         Sz        : constant Offset := ST_Entry'Size / SSU;
         Result    : Object_Symbol;
         Noff      : Offset;
         Sym_Off   : Offset;

      begin
         --  Seek to the successor of Prev

         Seek (Obj, Off);

         Noff := Off;

         loop
            Sym_Off := Noff;

            Read (Obj, ST_Entry'Address, uint32 (Sz));

            --  Read AUX entries

            for J in 1 .. ST_Entry.NumberOfAuxSymbols loop
               Read (Obj, Aux_Entry'Address, uint32 (Sz));
            end loop;

            Noff := Noff + Offset (1 + ST_Entry.NumberOfAuxSymbols) * Sz;

            exit when ST_Entry.TypeField = Function_Symbol_Type
              and then ST_Entry.SectionNumber > 0;

            if Noff >= Obj.Symtab_Last then
               return Null_Symbol;
            end if;
         end loop;

         --  Construct the symbol

         Result :=
           (Num   => Num + 1,
            Off   => Sym_Off,
            Next  => Noff,
            Value => uint64 (ST_Entry.Value),
            Size  => 0);

         --  Set the size as accurately as possible

         --  The size of a symbol is not directly available so we try scanning
         --  to the next function and assuming the code ends there.

         loop
            --  Read symbol and AUX entries

            Sym_Off := Noff;
            Read (Obj, ST_Last'Address, uint32 (Sz));

            for I in 1 .. ST_Last.NumberOfAuxSymbols loop
               Read (Obj, Aux_Entry'Address, uint32 (Sz));
            end loop;

            Noff := Noff + Offset (1 + ST_Last.NumberOfAuxSymbols) * Sz;

            if ST_Last.TypeField = Function_Symbol_Type then
               if ST_Last.SectionNumber = ST_Entry.SectionNumber
                 and then ST_Last.Value >= ST_Entry.Value
               then
                  --  Symbol is a function past ST_Entry

                  Result.Size := uint64 (ST_Last.Value - ST_Entry.Value);

               else
                  --  Not correlated function

                  Result.Next := Sym_Off;
               end if;

               exit;

            elsif ST_Last.SectionNumber = ST_Entry.SectionNumber
              and then ST_Last.TypeField = Not_Function_Symbol_Type
              and then ST_Last.StorageClass = 3
              and then ST_Last.NumberOfAuxSymbols = 1
            then
               --  Symbol is a section

               Result.Size := uint64 (ST_Last.Value + Aux_Entry.Length
                                        - ST_Entry.Value);
               Result.Next := Noff;
               exit;
            end if;

            exit when Noff > Obj.Symtab_Last;
         end loop;

         --  Relocate the address

         Result.Value :=
           Result.Value + Get_Section_Virtual_Address
              (Obj, uint32 (ST_Entry.SectionNumber - 1));

         return Result;
      end Read_Symbol;

      ------------------
      -- Read_Header  --
      ------------------

      function Read_Header (F : ICS.FILEs) return Header is
         Hdr : Header;
         Off : int32;
      begin
         Seek (F, Signature_Loc_Offset);
         Off := Read (F);
         Seek (F, Offset (Off));
         Read (F, Hdr'Address, uint32 (Hdr'Size / SSU));
         return Hdr;
      end Read_Header;

      -------------------------
      -- Read_Section_Header --
      -------------------------

      function Read_Section_Header
        (Obj   : PECOFF_Object_File;
         Index : uint32) return Section_Header
      is
         Sec : Section_Header;
      begin
         Seek (Obj, Obj.Sectab + Offset (Index * Section_Header'Size / SSU));
         Read (Obj, Sec'Address, Section_Header'Size / SSU);
         return Sec;
      end Read_Section_Header;

      ----------
      -- Name --
      ----------

      function Name
        (Obj : PECOFF_Object_File;
         Sec : Object_Section) return String
      is
         Shdr : constant Section_Header := Read_Section_Header (Obj, Sec.Num);
      begin
         return Decode_Name (Obj, Shdr.Name);
      end Name;

      -------------------
      -- String_Table  --
      -------------------

      function String_Table
        (Obj   : PECOFF_Object_File;
         Index : Offset) return String
      is
         Hdr : constant Header := Read_Header (Obj.fp);
         Off : Offset;

      begin
         --  An index of zero is used to represent an empty string, as the
         --  first word of the string table is specified to contain the length
         --  of the table rather than its contents.

         if Index = 0 then
            return "";

         else
            Off :=
              Offset (Hdr.PointerToSymbolTable) +
              Offset (Hdr.NumberOfSymbols * 18) +
              Index;
            return Offset_To_String (Obj, Off);
         end if;
      end String_Table;

      ----------
      -- Name --
      ----------

      function Name
        (Obj : PECOFF_Object_File;
         Sym : Object_Symbol) return String
      is
         ST_Entry : Symtab_Entry;
         Old_Off  : Offset;

      begin
         Tell (Obj, Old_Off);

         Seek (Obj, Sym.Off);
         Read (Obj, ST_Entry'Address, ST_Entry'Size / SSU);
         Seek (Obj, Old_Off);

         declare
            --  Symbol table entries are packed and Table_Entry.Name may not be
            --  sufficiently aligned to interpret as a 32 bit word, so it is
            --  copied to a temporary

            Aligned_Name : Name_Str := ST_Entry.Name;
            for Aligned_Name'Alignment use 4;

            First_Word : uint32;
            for First_Word'Address use Aligned_Name (1)'Address;

            Second_Word : uint32;
            for Second_Word'Address use Aligned_Name (5)'Address;

         begin
            if First_Word = 0 then
               return String_Table (Obj, int64 (Second_Word));
            else
               return Trim_Trailing_Nuls (ST_Entry.Name);
            end if;
         end;
      end Name;

   end PECOFF_Ops;

   -----------------
   -- XCOFF32_Ops --
   -----------------

   package body XCOFF32_Ops is

      function Read_Section_Header
        (Obj   : XCOFF32_Object_File;
         Index : uint32) return Section_Header;
      --  Read a header from section table

      function Read_Symbol
        (Obj : XCOFF32_Object_File;
         Off : Offset) return Object_Symbol;
      --  Read a symbol at offset Off

      function String_Table
        (Obj   : XCOFF32_Object_File;
         Index : Offset) return String;
      --  Return an entry from the string table

      -----------------
      -- Read_Symbol --
      -----------------

      function Read_Symbol
        (Obj : XCOFF32_Object_File;
         Off : Offset) return Object_Symbol
      is
         Sym     : Symbol_Entry;
         Sz      : constant Offset := Symbol_Entry'Size / SSU;
         Last    : constant Offset := Obj.Symtab +
                                            Offset (Obj.Num_Symbols - 1) * Sz;
         Aux     : Aux_Entry;
         Result  : Object_Symbol;
         Noff    : Offset;
         Sym_Off : Offset;

         procedure Read_LD_Symbol;
         --  Read the next LD symbol

         --------------------
         -- Read_LD_Symbol --
         --------------------

         procedure Read_LD_Symbol is
         begin
            loop
               Sym_Off := Noff;

               Read (Obj, Sym'Address, uint32 (Sz));

               Noff := Noff + Offset (1 + Sym.n_numaux) * Sz;

               for J in 1 .. Sym.n_numaux loop
                  Read (Obj, Aux'Address, uint32 (Sz));
               end loop;

               exit when Noff >= Last;

               exit when Sym.n_numaux = 1
                 and then Sym.n_scnum /= 0
                 and then (Sym.n_sclass = C_EXT
                           or else Sym.n_sclass = C_HIDEXT
                           or else Sym.n_sclass = C_WEAKEXT)
                 and then Aux.x_smtyp = XTY_LD;
            end loop;
         end Read_LD_Symbol;

      --  Start of processing for Read_Symbol

      begin
         Seek (Obj, Off);
         Noff := Off;
         Read_LD_Symbol;

         if Noff >= Last then
            return Null_Symbol;
         end if;

         --  Construct the symbol

         Result := (Num   => 0,
                    Off   => Sym_Off,
                    Next  => Noff,
                    Value => uint64 (Sym.n_value),
                    Size  => 0);

         --  Look for the next symbol to compute the size

         Read_LD_Symbol;

         if Noff >= Last then
            return Null_Symbol;
         end if;

         Result.Size := uint64 (Sym.n_value) - Result.Value;
         Result.Next := Sym_Off;
         return Result;
      end Read_Symbol;

      ------------------
      -- First_Symbol --
      ------------------

      function First_Symbol
        (Obj : in out XCOFF32_Object_File) return Object_Symbol is
      begin
         --  Return Null_Symbol in the case that the symbol table is empty

         if Obj.Num_Symbols = 0 then
            return Null_Symbol;
         end if;

         return Read_Symbol (Obj, Obj.Symtab);
      end First_Symbol;

      ----------------
      -- Initialize --
      ----------------

      function Initialize
        (F            : ICS.FILEs;
         Hdr          : Header;
         In_Exception : Boolean) return XCOFF32_Object_File
      is
         Res : XCOFF32_Object_File;

      begin
         Res.fp := F;
         Res.In_Exception := In_Exception;

         Res.Format := XCOFF32;
         Res.Arch := PPC;

         Res.Sectab := Offset (Header'Size / SSU) + Offset (Hdr.f_opthdr);
         Res.Num_Symbols := uint64 (Hdr.f_nsyms);
         Res.Num_Sections := uint32 (Hdr.f_nscns);
         Res.Symtab := Offset (Hdr.f_symptr);

         return Res;
      end Initialize;

      -----------------
      -- Get_Section --
      -----------------

      function Get_Section
        (Obj   : XCOFF32_Object_File;
         Index : uint32) return Object_Section
      is
         Sec : constant Section_Header := Read_Section_Header (Obj, Index);
      begin
         return (Index, Offset (Sec.s_scnptr), uint64 (Sec.s_size));
      end Get_Section;

      -----------------
      -- Next_Symbol --
      -----------------

      function Next_Symbol
        (Obj  : in out XCOFF32_Object_File;
         Prev : Object_Symbol) return Object_Symbol
      is
         Sz   : constant Offset := Symbol_Entry'Size / SSU;
         Last : constant Offset := Obj.Symtab +
                                         Offset (Obj.Num_Symbols - 1) * Sz;

      begin
         --  Test whether we've reached the end of the symbol table

         if Prev.Next > Last then
            return Null_Symbol;
         end if;

         return Read_Symbol (Obj, Prev.Next);
      end Next_Symbol;

      -----------------
      -- Read_Header --
      -----------------

      function Read_Header (F : ICS.FILEs) return Header is
         Hdr : Header;
      begin
         Seek (F, 0);
         Read (F, Hdr'Address, uint32 (Hdr'Size / SSU));
         return Hdr;
      end Read_Header;

      -------------------------
      -- Read_Section_Header --
      -------------------------

      function Read_Section_Header
        (Obj   : XCOFF32_Object_File;
         Index : uint32) return Section_Header
      is
         Old_Off : Offset;
         Sec     : Section_Header;

      begin
         Tell (Obj, Old_Off);

         --  Seek to the end of the object header

         Seek (Obj, Obj.Sectab + Offset (Index * Section_Header'Size / SSU));

         --  Read the section

         Read (Obj, Sec'Address, Section_Header'Size / SSU);

         --  Restore offset and return

         Seek (Obj, Old_Off);
         return Sec;
      end Read_Section_Header;

      ----------
      -- Name --
      ----------

      function Name
        (Obj : XCOFF32_Object_File;
         Sec : Object_Section) return String
      is
         Hdr : Section_Header;
      begin
         Hdr := Read_Section_Header (Obj, Sec.Num);
         return Trim_Trailing_Nuls (Hdr.s_name);
      end Name;

      -------------------
      -- String_Table  --
      -------------------

      function String_Table
        (Obj   : XCOFF32_Object_File;
         Index : Offset) return String
      is
         Hdr : constant Header := Read_Header (Obj.fp);
         Off : Offset;

      begin
         --  An index of zero is used to represent an empty string, as the
         --  first word of the string table is specified to contain the length
         --  of the table rather than its contents.

         if Index = 0 then
            return "";

         else
            Off := Offset (Hdr.f_symptr) + Offset (Hdr.f_nsyms * 18) + Index;
            return Offset_To_String (Obj, Off);
         end if;
      end String_Table;

      ----------
      -- Name --
      ----------

      function Name
        (Obj : XCOFF32_Object_File;
         Sym : Object_Symbol) return String
      is
         Symbol  : Symbol_Entry;
         Old_Off : Offset;

      begin
         Tell (Obj, Old_Off);

         Seek (Obj, Sym.Off);
         Read (Obj, Symbol'Address, Sym'Size / SSU);
         Seek (Obj, Old_Off);

         declare
            First_Word : uint32;
            for First_Word'Address use Symbol.n_name (1)'Address;

            Second_Word : uint32;
            for Second_Word'Address use Symbol.n_name (5)'Address;

         begin
            if First_Word = 0 then
               return String_Table (Obj, int64 (Second_Word));
            else
               return Trim_Trailing_Nuls (Symbol.n_name);
            end if;
         end;
      end Name;
   end XCOFF32_Ops;

   ----------
   -- Arch --
   ----------

   function Arch (Obj : Object_File'Class) return Object_Arch is
   begin
      return Obj.Arch;
   end Arch;

   -----------
   -- Close --
   -----------

   procedure Close (Obj : in out Object_File) is
   begin
      if fclose (Obj.fp) /= 0 then
         raise IO_Error with "could not close object file";
      end if;

      Obj.fp := NULL_Stream;
   end Close;

   ----------------------
   -- Decoded_Ada_Name --
   ----------------------

   function Decoded_Ada_Name
     (Obj : Object_File'Class;
      Sym : Object_Symbol) return String
   is
      procedure gnat_decode
        (Coded_Name_Addr : Address;
         Ada_Name_Addr   : Address;
         Verbose         : int);
      pragma Import (C, gnat_decode, "__gnat_decode");

      subtype size_t is Interfaces.C.size_t;
      function strlen (Str_Addr : Address) return size_t;
      pragma Import (C, strlen, "strlen");

      Raw     : char_array := To_C (Name (Obj, Sym));
      Raw_Len : constant size_t := strlen (Raw'Address);
      Decoded : char_array (0 .. Raw_Len * 2 + 60);

   begin
      --  In the PECOFF case most but not all symbol table entries have an
      --  extra leading underscore. In this case we trim it.

      if (Obj.Format = PECOFF  and then Raw (0) = '_')
           or else
         (Obj.Format = XCOFF32 and then Raw (0) = '.')
      then
         gnat_decode (Raw (1)'Address, Decoded'Address, 0);
      else
         gnat_decode (Raw'Address, Decoded'Address, 0);
      end if;

      return To_Ada (Decoded);
   end Decoded_Ada_Name;

   ------------
   -- Format --
   ------------

   function Format (Obj : Object_File'Class) return Object_Format is
   begin
      return Obj.Format;
   end Format;

   -----------------
   -- Get_Section --
   -----------------

   function Get_Section
     (Obj      : Object_File'Class;
      Sec_Name : String) return Object_Section
   is
      Sec : Object_Section;

   begin
      for J in 0 .. Obj.Num_Sections - 1 loop
         Sec := Get_Section (Obj, J);

         if Name (Obj, Sec) = Sec_Name then
            return Sec;
         end if;
      end loop;

      if Obj.In_Exception then
         return Null_Section;
      else
         raise Format_Error with "could not find section in object file";
      end if;
   end Get_Section;

   ---------
   -- Num --
   ---------

   function Num (Sec : Object_Section) return uint32 is
   begin
      return Sec.Num;
   end Num;

   ------------------
   -- Num_Sections --
   ------------------

   function Num_Sections (Obj : Object_File'Class) return uint32 is
   begin
      return Obj.Num_Sections;
   end Num_Sections;

   -----------------
   -- Num_Symbols --
   -----------------

   function Num_Symbols (Obj : Object_File'Class) return uint64 is
   begin
      return Obj.Num_Symbols;
   end Num_Symbols;

   ---------
   -- Off --
   ---------

   function Off (Sec : Object_Section) return Offset is
   begin
      return Sec.Off;
   end Off;

   ----------------------
   -- Offset_To_String --
   ----------------------

   function Offset_To_String
     (Obj : Object_File'Class;
      Off : Offset) return String
   is
      Old_Off : Offset;
      Buf     : Buffer;
   begin
      Tell (Obj, Old_Off);
      Seek (Obj, Off);
      Read_C_String (Obj, Buf);
      Seek (Obj, Old_Off);
      return To_String (Buf);
   end Offset_To_String;

   ----------
   -- Open --
   ----------

   function Open
     (File_Name    : String;
      In_Exception : Boolean := False) return Object_File_Access
   is
      F      : ICS.FILEs;
      C_Name : char_array := To_C (File_Name);
      C_Mode : char_array := To_C ("rb");

   begin
      --  Open the file

      F := fopen (C_Name'Address, C_Mode'Address);

      if F = NULL_Stream then
         if In_Exception then
            return null;
         else
            raise IO_Error with "could not open object file";
         end if;
      end if;

      declare
         Hdr : constant ELF32_Ops.Header := ELF32_Ops.Read_Header (F);

      begin
         --  Look for the magic numbers for the ELF case

         if Hdr.E_Ident (0) = 16#7F#              and then
            Hdr.E_Ident (1) = Character'Pos ('E') and then
            Hdr.E_Ident (2) = Character'Pos ('L') and then
            Hdr.E_Ident (3) = Character'Pos ('F') and then
            Hdr.E_Ident (4) = ELF32_Ops.ELFCLASS32
         then
            return new Object_File'Class'
              (Object_File'Class
                 (ELF32_Ops.Initialize (F, Hdr, In_Exception)));
         end if;
      end;

      declare
         Hdr : constant ELF64_Ops.Header := ELF64_Ops.Read_Header (F);

      begin
         --  Look for the magic numbers for the ELF case

         if Hdr.E_Ident (0) = 16#7F#              and then
            Hdr.E_Ident (1) = Character'Pos ('E') and then
            Hdr.E_Ident (2) = Character'Pos ('L') and then
            Hdr.E_Ident (3) = Character'Pos ('F') and then
            Hdr.E_Ident (4) = ELF32_Ops.ELFCLASS64
         then
            return new Object_File'Class'
              (Object_File'Class
                 (ELF64_Ops.Initialize (F, Hdr, In_Exception)));
         end if;
      end;

      declare
         Hdr : constant PECOFF_Ops.Header := PECOFF_Ops.Read_Header (F);

      begin
         --  Test the magic numbers

         if Hdr.Magics (0) = Character'Pos ('P') and then
            Hdr.Magics (1) = Character'Pos ('E') and then
            Hdr.Magics (2) = 0                   and then
            Hdr.Magics (3) = 0
         then
            return new Object_File'Class'
              (Object_File'Class
                 (PECOFF_Ops.Initialize (F, Hdr, In_Exception)));
         end if;

      exception
         --  If this is not a PECOFF file then we've done a seek and read to a
         --  random address, possibly raising IO_Error

         when IO_Error =>
            null;
      end;

      declare
         Hdr : constant XCOFF32_Ops.Header := XCOFF32_Ops.Read_Header (F);

      begin
         --  Test the magic numbers

         if Hdr.f_magic = 8#0737# then
            return new Object_File'Class'
              (Object_File'Class
                 (XCOFF32_Ops.Initialize (F, Hdr, In_Exception)));
         end if;
      end;

      if In_Exception then
         return null;
      else
         raise Format_Error with "unrecognized object format";
      end if;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (F    : ICS.FILEs;
      Addr : Address;
      Size : uint32)
   is
      subtype size_t is Interfaces.C_Streams.size_t;
      Num_Read : uint32;

   begin
      Num_Read := uint32 (fread (Addr, size_t (Size), 1, F));

      if Num_Read /= 1 then
         raise IO_Error with "could not read from object file";
      end if;
   end Read;

   procedure Read
     (Obj  : Object_File'Class;
      Addr : Address;
      Size : uint32) is
   begin
      Read (Obj.fp, Addr, Size);
   end Read;

   function Read (Obj : Object_File'Class) return uint8 is
      Data : uint8;
   begin
      Read (Obj, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   function Read (Obj : Object_File'Class) return uint16 is
      Data : uint16;
   begin
      Read (Obj, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   function Read (Obj : Object_File'Class) return uint32 is
      Data : uint32;
   begin
      Read (Obj, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   function Read (Obj : Object_File'Class) return uint64 is
      Data : uint64;
   begin
      Read (Obj, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   function Read (Obj : Object_File'Class) return int8 is
      Data : int8;
   begin
      Read (Obj, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   function Read (Obj : Object_File'Class) return int16 is
      Data : int16;
   begin
      Read (Obj, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   function Read (F : ICS.FILEs) return int32 is
      Data : int32;
   begin
      Read (F, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   function Read (Obj : Object_File'Class) return int32 is
   begin
      return Read (Obj.fp);
   end Read;

   function Read (Obj : Object_File'Class) return int64 is
      Data : int64;
   begin
      Read (Obj, Data'Address, Data'Size / SSU);
      return Data;
   end Read;

   ------------------
   -- Read_Address --
   ------------------

   function Read_Address (Obj : Object_File'Class) return uint64 is
      Address_32 : uint32;
      Address_64 : uint64;

   begin
      case Obj.Arch is
         when SPARC | i386 | PPC | MIPS =>
            Address_32 := Read (Obj);
            return uint64 (Address_32);

         when SPARC64 | x86_64 | IA64 | PPC64 =>
            Address_64 := Read (Obj);
            return Address_64;

         when others =>
            raise Format_Error with "unrecognized machine architecture";
      end case;
   end Read_Address;

   -------------------
   -- Read_C_String --
   -------------------

   procedure Read_C_String (Obj : Object_File'Class; B : out Buffer) is
      J : Integer := 0;

   begin
      loop
         --  Handle overflow case

         if J = B'Last then
            B (J) := 0;
            exit;
         end if;

         B (J) := Read (Obj);
         exit when B (J) = 0;
         J := J + 1;
      end loop;
   end Read_C_String;

   -----------------
   -- Read_LEB128 --
   -----------------

   function Read_LEB128 (Obj : Object_File'Class) return uint32 is
      B     : uint8;
      Shift : Integer := 0;
      Res   : uint32 := 0;

   begin
      loop
         B := Read (Obj);
         Res := Res or Shift_Left (uint32 (B and 16#7f#), Shift);
         exit when (B and 16#80#) = 0;
         Shift := Shift + 7;
      end loop;

      return Res;
   end Read_LEB128;

   function Read_LEB128 (Obj : Object_File'Class) return int32 is
      B     : uint8;
      Shift : Integer := 0;
      Res   : uint32 := 0;

   begin
      loop
         B := Read (Obj);
         Res := Res or Shift_Left (uint32 (B and 16#7f#), Shift);
         Shift := Shift + 7;
         exit when (B and 16#80#) = 0;
      end loop;

      if Shift < 32 and then (Res and Shift_Left (1, Shift - 1)) /= 0 then
         Res := Res or Shift_Left (-1, Shift);
      end if;

      return To_int32 (Res);
   end Read_LEB128;

   ----------
   -- Seek --
   ----------

   procedure Seek (F : ICS.FILEs; Off : Offset) is
      rv : Interfaces.C_Streams.int;

      subtype long is Interfaces.C_Streams.long;

   begin
      rv := fseek (F, long (Off), SEEK_SET);

      if rv /= 0 then
         raise IO_Error with "could not seek to offset in object file";
      end if;
   end Seek;

   procedure Seek (Obj : Object_File'Class; Off : Offset) is
   begin
      Seek (Obj.fp, Off);
   end Seek;

   procedure Seek (Obj : Object_File'Class; Sec : Object_Section) is
   begin
      Seek (Obj, Sec.Off);
   end Seek;

   ----------
   -- Size --
   ----------

   function Size (Sec : Object_Section) return uint64 is
   begin
      return Sec.Size;
   end Size;

   function Size (Sym : Object_Symbol) return uint64 is
   begin
      return Sym.Size;
   end Size;

   ------------
   -- Spans  --
   ------------

   function Spans (Sym : Object_Symbol; Addr : uint64) return Boolean is
   begin
      return Addr >= Sym.Value and then Addr < Sym.Value + Sym.Size;
   end Spans;

   ------------
   -- Strlen --
   ------------

   function Strlen (Buf : Buffer) return int32 is
   begin
      for J in Buf'Range loop
         if Buf (J) = 0 then
            return int32 (J);
         end if;
      end loop;

      return Buf'Length;
   end Strlen;

   ----------
   -- Tell --
   ----------

   procedure Tell (Obj : Object_File'Class; Off : out Offset) is
   begin
      Off := Offset (ftell (Obj.fp));
   end Tell;

   ---------------
   -- To_String --
   ---------------

   function To_String (Buf : Buffer) return String is
      Count  : constant Integer := Integer (Strlen (Buf));
      Result : String (1 .. Count);

   begin
      if Count = 0 then
         return "";
      end if;

      for J in 0 .. Count - 1 loop
         Result (J + 1) := Character'Val (Buf (J));
      end loop;

      return Result;
   end To_String;

   ------------------------
   -- Trim_Trailing_Nuls --
   ------------------------

   function Trim_Trailing_Nuls (Str : String) return String is
   begin
      for J in Str'Range loop
         if Str (J) = ASCII.NUL then
            return Str (Str'First .. J - 1);
         end if;
      end loop;

      return Str;
   end Trim_Trailing_Nuls;

   -----------
   -- Value --
   -----------

   function Value (Sym : Object_Symbol) return uint64 is
   begin
      return Sym.Value;
   end Value;

end System.Object_Reader;
