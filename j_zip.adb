------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                J _ Z I P                                 --
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

package body J_Zip is

   ----------------
   -- Local Data --
   ----------------

   --  Zip archive format (see http://www.wotsit.demon.co.uk/archives.htm for
   --  more info):
   --
   --     [local file header + file data + data_descriptor] . . .
   --     [central directory] end of central directory record
   --
   --  All 2 and 4 byte quantities are stored little-endian (low byte first).
   --
   --  A. Local file header:
   --
   --      local file header signature     4 bytes  (0x04034b50)
   --      version needed to extract       2 bytes
   --      general purpose bit flag        2 bytes
   --      compression method              2 bytes
   --      last mod file time              2 bytes
   --      last mod file date              2 bytes
   --      crc-32                          4 bytes
   --      compressed size                 4 bytes
   --      uncompressed size               4 bytes
   --      filename length                 2 bytes
   --      extra field length              2 bytes
   --
   --      filename (variable size)
   --      extra field (variable size)
   --
   --  B. Data descriptor:
   --
   --      crc-32                          4 bytes
   --      compressed size                 4 bytes
   --      uncompressed size               4 bytes
   --
   --  This descriptor exists only if bit 3 of the general purpose bit flag is
   --  set (see below). It is byte aligned and immediately follows the last
   --  byte of compressed data. This descriptor is used only when it was not
   --  possible to seek in the output zip file, e.g., when the output zip file
   --  was standard output or a non seekable device.
   --
   --  C. Central directory structure:
   --
   --       [central file header] . . .  end of central dir record
   --
   --  Central File header:
   --
   --      central file header signature   4 bytes  (0x02014b50)
   --      other fields ...
   --
   --  End of central dir record:
   --
   --      end of central dir signature    4 bytes  (0x06054b50)
   --      other ields ...
   --
   --  D. Explanation of fields relevants to this package:
   --
   --  general purpose bit flag: (2 bytes)
   --
   --        bit 0: If set, indicates that the file is encrypted.
   --
   --        bit 3: If this bit is set, the fields crc-32, compressed size
   --               and uncompressed size are set to zero in the local
   --               header. The correct values are put in the data descriptor
   --               immediately following the compressed data.
   --
   --
   --  compressed size: (4 bytes)
   --  uncompressed size: (4 bytes)
   --
   --        The size of the file compressed and uncompressed,
   --        respectively.  If bit 3 of the general purpose bit flag
   --        is set, these fields are set to zero in the local header
   --        and the correct values are put in the data descriptor and
   --        in the central directory.
   --
   --  filename length: (2 bytes)
   --  extra field length: (2 bytes)
   --
   --        The length of the filename and extra field respectively.
   --        The combined length of any directory record and these three fields
   --        should not generally exceed 65,535 bytes. If input came from
   --        standard input, the filename length is set to zero.
   --
   --  filename: (Variable)
   --
   --        The name of the file, with optional relative path.
   --        The path stored should not contain a drive or
   --        device letter, or a leading slash.  All slashes
   --        should be forward slashes '/' as opposed to
   --        backwards slashes '\' for compatibility with Amiga
   --        and Unix file systems etc.  If input came from standard
   --        input, there is no filename field.
   --
   --  extra field: (Variable)
   --
   --        This is for future expansion.
   --
   --  E. General notes:
   --
   --  (a) All fields unless otherwise noted are unsigned and stored
   --      in Intel low-byte:high-byte, low-word:high-word order.
   --
   --  (b) String fields are not null terminated, since the length is
   --      given explicitly.

   Local_File_Header_Signature   : constant U4 := 16#04_03_4B_50#;
   Central_File_Header_Signature : constant U4 := 16#02_01_4B_50#;
   End_Of_Central_Dir_Signature  : constant U4 := 16#06_05_4B_50#;

   MS_DOS_Archive : constant U1 := 0;
   --  The host system for the file. By default most versions of zip
   --  pick the number for MS DOS, so we do the same.

   Version_Number : constant U1 := 10;
   --  Version 1.0. The value/10 indicates the major version number,
   --  and the value mod 10 is the minor version number.

   Local_File_Header_Size   : constant := 30;
   Central_File_Header_Size : constant := 46;
   End_Of_Central_Dir_Size  : constant := 22;

   type Local_File_Header is record
      Signature          : U4;
      Version_To_Extract : U2;
      General_Bit_Flag   : U2;
      Compression_Method : U2;
      Last_Mod_Time      : U2;
      Last_Mod_Date      : U2;
      Crc_32             : U4;
      Compressed_Size    : U4;
      Uncompressed_Size  : U4;
      Filename_Length    : U2;
      Extra_Field_Length : U2;
   end record;

   --------------------
   -- Local Routines --
   --------------------

   procedure Read_U2
     (A : Stream_Of_U1;
      P : in out Archive_Byte_Offset;
      V :    out U2);
   pragma Inline (Read_U2);
   --  Read a U2 into V from archive A starting at offset position P and
   --  advance P. The U2 in A is stored in little-endian order.

   procedure Read_U4
     (A : Stream_Of_U1;
      P : in out Archive_Byte_Offset;
      V :    out U4);
   pragma Inline (Read_U4);
   --  Same as above but for a U4

   procedure Read_Local_File_Header
     (A : Stream_Of_U1;
      P : in out Archive_Byte_Offset;
      H :    out Local_File_Header);
   --  Same as above but for a Local_File_Header_File

   type Header_Type is (Unknown, Local_File, Central_File, End_Of_Central_Dir);
   function Header_Kind
     (A    : Stream_Of_U1;
      P    : Archive_Byte_Offset)
      return Header_Type;
   --  Returns the type of header starting at offset P in archive A

   procedure Write_U1 (File : GNAT.OS_Lib.File_Descriptor;
                       D    : U1);
   pragma Inline (Write_U1);
   --  Write a U1 to a file

   procedure Write_U2 (File : GNAT.OS_Lib.File_Descriptor;
                       D    : U2);
   pragma Inline (Write_U2);
   --  Same with U2

   procedure Write_U4 (File : GNAT.OS_Lib.File_Descriptor;
                       D    : U4);
   pragma Inline (Write_U4);
   --  Same with U4

   procedure Write_Stream (File   : GNAT.OS_Lib.File_Descriptor;
                           Stream : Stream_Of_U1);
   pragma Inline (Write_Stream);
   --  Write a stream of U1 to a file

   function CRC_32 (Bytes : Stream_Of_U1) return U4;
   --  Calculate Crc for a stream of u1

   --------------------
   -- Has_Zip_Format --
   --------------------

   function Has_Zip_Format (S : Stream_Of_U1) return Boolean is
   begin
      return Header_Kind (S, S'First) /= Unknown;
   end Has_Zip_Format;

   -----------------
   -- Header_Kind --
   -----------------

   function Header_Kind
     (A    : Stream_Of_U1;
      P    : Archive_Byte_Offset)
      return Header_Type
   is
      Signature : U4;
      Pos : Archive_Byte_Offset := P;

   begin
      if P + 3 > A'Last then
         return Unknown;
      end if;

      Read_U4 (A, Pos, Signature);

      if Signature = Local_File_Header_Signature
        and then P + Local_File_Header_Size - 1 <= A'Last
      then
         return Local_File;

      elsif Signature = Central_File_Header_Signature
        and then P + Central_File_Header_Size - 1 <= A'Last
      then
         return Central_File;

      elsif Signature = End_Of_Central_Dir_Signature
        and then P + End_Of_Central_Dir_Size - 1 <= A'Last
      then
         return End_Of_Central_Dir;

      else
         return Unknown;
      end if;
   end Header_Kind;

   ---------------------
   -- Get_Archive_Dir --
   ---------------------

   function Get_Archive_Dir (A : Stream_Of_U1) return Archive_Directory is
      H_Type : Header_Type;

      H : Local_File_Header;
      --  The header of the current file we are reading from the archive

      Cur_Pos : Archive_Byte_Offset := A'First;
      --  The current byte offset position in the archive A

      Nb_Files : Natural := 0;
      --  Number of files in the archive

   begin
      --  Loop through all the files in the archive counting how many we have

      loop
         H_Type := Header_Kind (A, Cur_Pos);

         if H_Type = Unknown then
            raise Bad_Zip_Archive;
         end if;

         exit when H_Type /= Local_File;

         Read_Local_File_Header (A, Cur_Pos, H);

         if H.Compression_Method /= 0 then
            raise Compressed_Zip_Archive;
         end if;

         if (H.General_Bit_Flag and 16#00_08#) /= 0 then
            raise Bad_Zip_Archive;
         end if;

         Nb_Files := Nb_Files + 1;

         --  Skip the file name and the extra field

         Cur_Pos := Cur_Pos
           + Archive_Byte_Offset (H.Filename_Length)
           + Archive_Byte_Offset (H.Extra_Field_Length)
           + Archive_Byte_Offset (H.Compressed_Size);
      end loop;

      declare
         File : Archive_Directory (1 .. Nb_Files);

      begin
         Cur_Pos := A'First;

         for K in 1 .. Nb_Files loop
            Read_Local_File_Header (A, Cur_Pos, H);

            File (K).Name_First := Cur_Pos;

            Cur_Pos := Cur_Pos + Archive_Byte_Offset (H.Filename_Length);
            File (K).Name_Last := Cur_Pos - 1;

            Cur_Pos := Cur_Pos + Archive_Byte_Offset (H.Extra_Field_Length);
            File (K).First := Cur_Pos;

            Cur_Pos := Cur_Pos + Archive_Byte_Offset (H.Compressed_Size);
            File (K).Last := Cur_Pos - 1;

            File (K).Compressed := H.Compression_Method /= 0;
            File (K).Encrypted  := (H.General_Bit_Flag and 16#00_01#) /= 0;
         end loop;

         return File;
      end;
   end Get_Archive_Dir;

   ----------------------------
   -- Read_Local_File_Header --
   ----------------------------

   procedure Read_Local_File_Header
     (A : Stream_Of_U1;
      P : in out Archive_Byte_Offset;
      H :    out Local_File_Header)
   is
   begin
      Read_U4 (A, P, H.Signature);
      Read_U2 (A, P, H.Version_To_Extract);
      Read_U2 (A, P, H.General_Bit_Flag);
      Read_U2 (A, P, H.Compression_Method);
      Read_U2 (A, P, H.Last_Mod_Time);
      Read_U2 (A, P, H.Last_Mod_Date);
      Read_U4 (A, P, H.Crc_32);
      Read_U4 (A, P, H.Compressed_Size);
      Read_U4 (A, P, H.Uncompressed_Size);
      Read_U2 (A, P, H.Filename_Length);
      Read_U2 (A, P, H.Extra_Field_Length);
   end Read_Local_File_Header;

   -------------
   -- Read_U2 --
   -------------

   procedure Read_U2
     (A : Stream_Of_U1;
      P : in out Archive_Byte_Offset;
      V : out U2)
   is
   begin
      V := To_U2 (A (P + 1), A (P));
      P := P + 2;
   end Read_U2;

   -------------
   -- Read_U4 --
   -------------

   procedure Read_U4
     (A : Stream_Of_U1;
      P : in out Archive_Byte_Offset;
      V : out U4)
   is
   begin
      V := To_U4 (A (P + 3), A (P + 2), A (P + 1), A (P));
      P := P + 4;
   end Read_U4;

   ------------------------
   -- Create_New_Archive --
   ------------------------

   procedure Create_New_Archive
     (File_Name : String;
      Arc       : out Zip_Archive)
   is
      use GNAT.OS_Lib;

      File_Name_0 : constant String := File_Name & ASCII.NUL;
      Output_FD   : File_Descriptor;

   begin
      Output_FD := GNAT.OS_Lib.Create_File (File_Name_0'Address, Binary);

      if Output_FD = Invalid_FD then
         Fail ("Cannot create: " & File_Name);
      end if;

      Arc.File := Output_FD;
      Arc.Is_Closed := False;
   end Create_New_Archive;

   ------------
   -- CRC_32 --
   ------------

   function CRC_32 (Bytes : Stream_Of_U1) return U4 is
      type Crc_Table is array (U1 range <>) of U4;

      Table : constant Crc_Table :=
        (16#00000000#, 16#77073096#, 16#ee0e612c#, 16#990951ba#, 16#076dc419#,
         16#706af48f#, 16#e963a535#, 16#9e6495a3#, 16#0edb8832#, 16#79dcb8a4#,
         16#e0d5e91e#, 16#97d2d988#, 16#09b64c2b#, 16#7eb17cbd#, 16#e7b82d07#,
         16#90bf1d91#, 16#1db71064#, 16#6ab020f2#, 16#f3b97148#, 16#84be41de#,
         16#1adad47d#, 16#6ddde4eb#, 16#f4d4b551#, 16#83d385c7#, 16#136c9856#,
         16#646ba8c0#, 16#fd62f97a#, 16#8a65c9ec#, 16#14015c4f#, 16#63066cd9#,
         16#fa0f3d63#, 16#8d080df5#, 16#3b6e20c8#, 16#4c69105e#, 16#d56041e4#,
         16#a2677172#, 16#3c03e4d1#, 16#4b04d447#, 16#d20d85fd#, 16#a50ab56b#,
         16#35b5a8fa#, 16#42b2986c#, 16#dbbbc9d6#, 16#acbcf940#, 16#32d86ce3#,
         16#45df5c75#, 16#dcd60dcf#, 16#abd13d59#, 16#26d930ac#, 16#51de003a#,
         16#c8d75180#, 16#bfd06116#, 16#21b4f4b5#, 16#56b3c423#, 16#cfba9599#,
         16#b8bda50f#, 16#2802b89e#, 16#5f058808#, 16#c60cd9b2#, 16#b10be924#,
         16#2f6f7c87#, 16#58684c11#, 16#c1611dab#, 16#b6662d3d#, 16#76dc4190#,
         16#01db7106#, 16#98d220bc#, 16#efd5102a#, 16#71b18589#, 16#06b6b51f#,
         16#9fbfe4a5#, 16#e8b8d433#, 16#7807c9a2#, 16#0f00f934#, 16#9609a88e#,
         16#e10e9818#, 16#7f6a0dbb#, 16#086d3d2d#, 16#91646c97#, 16#e6635c01#,
         16#6b6b51f4#, 16#1c6c6162#, 16#856530d8#, 16#f262004e#, 16#6c0695ed#,
         16#1b01a57b#, 16#8208f4c1#, 16#f50fc457#, 16#65b0d9c6#, 16#12b7e950#,
         16#8bbeb8ea#, 16#fcb9887c#, 16#62dd1ddf#, 16#15da2d49#, 16#8cd37cf3#,
         16#fbd44c65#, 16#4db26158#, 16#3ab551ce#, 16#a3bc0074#, 16#d4bb30e2#,
         16#4adfa541#, 16#3dd895d7#, 16#a4d1c46d#, 16#d3d6f4fb#, 16#4369e96a#,
         16#346ed9fc#, 16#ad678846#, 16#da60b8d0#, 16#44042d73#, 16#33031de5#,
         16#aa0a4c5f#, 16#dd0d7cc9#, 16#5005713c#, 16#270241aa#, 16#be0b1010#,
         16#c90c2086#, 16#5768b525#, 16#206f85b3#, 16#b966d409#, 16#ce61e49f#,
         16#5edef90e#, 16#29d9c998#, 16#b0d09822#, 16#c7d7a8b4#, 16#59b33d17#,
         16#2eb40d81#, 16#b7bd5c3b#, 16#c0ba6cad#, 16#edb88320#, 16#9abfb3b6#,
         16#03b6e20c#, 16#74b1d29a#, 16#ead54739#, 16#9dd277af#, 16#04db2615#,
         16#73dc1683#, 16#e3630b12#, 16#94643b84#, 16#0d6d6a3e#, 16#7a6a5aa8#,
         16#e40ecf0b#, 16#9309ff9d#, 16#0a00ae27#, 16#7d079eb1#, 16#f00f9344#,
         16#8708a3d2#, 16#1e01f268#, 16#6906c2fe#, 16#f762575d#, 16#806567cb#,
         16#196c3671#, 16#6e6b06e7#, 16#fed41b76#, 16#89d32be0#, 16#10da7a5a#,
         16#67dd4acc#, 16#f9b9df6f#, 16#8ebeeff9#, 16#17b7be43#, 16#60b08ed5#,
         16#d6d6a3e8#, 16#a1d1937e#, 16#38d8c2c4#, 16#4fdff252#, 16#d1bb67f1#,
         16#a6bc5767#, 16#3fb506dd#, 16#48b2364b#, 16#d80d2bda#, 16#af0a1b4c#,
         16#36034af6#, 16#41047a60#, 16#df60efc3#, 16#a867df55#, 16#316e8eef#,
         16#4669be79#, 16#cb61b38c#, 16#bc66831a#, 16#256fd2a0#, 16#5268e236#,
         16#cc0c7795#, 16#bb0b4703#, 16#220216b9#, 16#5505262f#, 16#c5ba3bbe#,
         16#b2bd0b28#, 16#2bb45a92#, 16#5cb36a04#, 16#c2d7ffa7#, 16#b5d0cf31#,
         16#2cd99e8b#, 16#5bdeae1d#, 16#9b64c2b0#, 16#ec63f226#, 16#756aa39c#,
         16#026d930a#, 16#9c0906a9#, 16#eb0e363f#, 16#72076785#, 16#05005713#,
         16#95bf4a82#, 16#e2b87a14#, 16#7bb12bae#, 16#0cb61b38#, 16#92d28e9b#,
         16#e5d5be0d#, 16#7cdcefb7#, 16#0bdbdf21#, 16#86d3d2d4#, 16#f1d4e242#,
         16#68ddb3f8#, 16#1fda836e#, 16#81be16cd#, 16#f6b9265b#, 16#6fb077e1#,
         16#18b74777#, 16#88085ae6#, 16#ff0f6a70#, 16#66063bca#, 16#11010b5c#,
         16#8f659eff#, 16#f862ae69#, 16#616bffd3#, 16#166ccf45#, 16#a00ae278#,
         16#d70dd2ee#, 16#4e048354#, 16#3903b3c2#, 16#a7672661#, 16#d06016f7#,
         16#4969474d#, 16#3e6e77db#, 16#aed16a4a#, 16#d9d65adc#, 16#40df0b66#,
         16#37d83bf0#, 16#a9bcae53#, 16#debb9ec5#, 16#47b2cf7f#, 16#30b5ffe9#,
         16#bdbdf21c#, 16#cabac28a#, 16#53b39330#, 16#24b4a3a6#, 16#bad03605#,
         16#cdd70693#, 16#54de5729#, 16#23d967bf#, 16#b3667a2e#, 16#c4614ab8#,
         16#5d681b02#, 16#2a6f2b94#, 16#b40bbe37#, 16#c30c8ea1#, 16#5a05df1b#,
         16#2d02ef8d#);
      Crc : U4 := 16#ffffffff#;
      Tmp : U4;
   begin
      for K in Bytes'Range loop
         Tmp := Crc xor U4 (Bytes (K));
         Crc := Table (U1 (Tmp and 255)) xor Shift_Right (Crc, 8);
      end loop;
      return Crc xor 16#Ffffffff#;
   end CRC_32;

   ---------------------------
   -- Add_Stream_To_Archive --
   ---------------------------

   procedure Add_Stream_To_Archive
     (Stream    : Stream_Of_U1;
      File_Name : Stream_Of_U1;
      Archive   : in out Zip_Archive)
   is
      Stored_Method : constant U2 := 0;
      Info          : Zip_List_Info (File_Name'Length);

   begin
      if Archive.Is_Closed then
         raise Bad_Zip_Archive;
      end if;

      --  Fill the info structure

      Info.Name          := File_Name;
      Info.Stream_Length := Stream'Length;
      Info.Crc_32        := CRC_32 (Stream);

      if Archive.Num_Files = 0 then
         Info.Offset_Start := 0;
      else
         Info.Offset_Start
           := Archive.File_List (Archive.Num_Files).Offset_End;
      end if;

      Info.Offset_End :=
        Local_File_Header_Size  +
          U4 (Info.Name_Length) +
          Info.Stream_Length    +
          Info.Offset_Start;

      Archive.Num_Files := Archive.Num_Files + 1;
      Archive.File_List (Archive.Num_Files) := Info;

      --  Write file Header

      Write_U4 (Archive.File, Local_File_Header_Signature);
      Write_U2 (Archive.File, To_U2 (MS_DOS_Archive, Version_Number));
      Write_U2 (Archive.File, 0);
      Write_U2 (Archive.File, Stored_Method);
      Write_U2 (Archive.File, 20000);  -- Last_Mod_Time
      Write_U2 (Archive.File, 20000);  -- Last_Mod_Date
      Write_U4 (Archive.File, Info.Crc_32);
      Write_U4 (Archive.File, U4 (Stream'Length)); -- Compressed_Size
      Write_U4 (Archive.File, U4 (Stream'Length)); -- Uncompressed_Size
      Write_U2 (Archive.File, U2 (File_Name'Length));
      Write_U2 (Archive.File, 0);

      --  Write file name

      Write_Stream (Archive.File, File_Name);

      --  Nothing to write for extra field

      --  Write file

      Write_Stream (Archive.File, Stream);

   end Add_Stream_To_Archive;

   -------------------
   -- Close_Archive --
   -------------------

   procedure Close_Archive (Archive : in out Zip_Archive) is
      Stored_Method : constant U2 := 0;
      Size          : U4 := 0;

   begin
      if Archive.Is_Closed then
         raise Bad_Zip_Archive;
      end if;

      --  Write Central Directory

      for K in 1 .. Archive.Num_Files loop
         Write_U4 (Archive.File, Central_File_Header_Signature);
         Write_U2 (Archive.File, To_U2 (MS_DOS_Archive, Version_Number));
         Write_U2 (Archive.File, To_U2 (MS_DOS_Archive, Version_Number));
         Write_U2 (Archive.File, 0);  --  Bit flag
         Write_U2 (Archive.File, Stored_Method);
         Write_U2 (Archive.File, 20000);  -- Last_Mod_Time
         Write_U2 (Archive.File, 20000);  -- Last_Mod_Date
         Write_U4 (Archive.File, Archive.File_List (K).Crc_32);
         Write_U4 (Archive.File, Archive.File_List (K).Stream_Length);
         Write_U4 (Archive.File, Archive.File_List (K).Stream_Length);
         Write_U2 (Archive.File,
                   U2 (Archive.File_List (K).Name_Length));
         Write_U2 (Archive.File, 0); --  Extra field length
         Write_U2 (Archive.File, 0); --  File comment length
         Write_U2 (Archive.File, 1); --  Disk number start
         Write_U2 (Archive.File, 0); --  Internal file attribute
         Write_U4 (Archive.File, 0); --  External file attribute
         Write_U4 (Archive.File, Archive.File_List (K).Offset_Start);

         Write_Stream (Archive.File, Archive.File_List (K).Name);

         Size := Size + Central_File_Header_Size
           + U4 (Archive.File_List (K).Name_Length);
      end loop;

      --  Write end of Central Dir

      Write_U4 (Archive.File, End_Of_Central_Dir_Signature);
      Write_U2 (Archive.File, 0); --  Number of this disk
      Write_U2 (Archive.File, 0); --  Disk of start for central directory

      Write_U2 (Archive.File, U2 (Archive.Num_Files));
      Write_U2 (Archive.File, U2 (Archive.Num_Files));
      Write_U4 (Archive.File, Size); --  Size of central directory
      Write_U4 (Archive.File, Archive.File_List
                (Archive.Num_Files).Offset_End);

      Write_U2 (Archive.File, 0); --  Zip file comment length

      GNAT.OS_Lib.Close (Archive.File);
      Archive.Is_Closed := True;
   end Close_Archive;

   -----------
   -- Write --
   -----------

   procedure Write_U1
     (File : GNAT.OS_Lib.File_Descriptor;
      D    : U1)
   is
   begin
      if GNAT.OS_Lib.Write (File, D'Address, 1) /= 1 then
         Fail ("error writing to Zip file (disk full ?)");
      end if;
   end Write_U1;

   procedure Write_U2
     (File : GNAT.OS_Lib.File_Descriptor;
      D    : U2)
   is
      A, B : U1;
   begin
      To_Half_Word (D, A, B);
      Write_U1 (File, B);
      Write_U1 (File, A);
   end Write_U2;

   procedure Write_U4
     (File : GNAT.OS_Lib.File_Descriptor;
      D    : U4)
   is
      A, B, C, E : U1;
   begin
      To_Word (D, A, B, C, E);
      Write_U1 (File, E);
      Write_U1 (File, C);
      Write_U1 (File, B);
      Write_U1 (File, A);
   end Write_U4;

   procedure Write_Stream
     (File   : GNAT.OS_Lib.File_Descriptor;
      Stream : Stream_Of_U1)
   is
   begin
      if Integer (Stream'Length) /=
        GNAT.OS_Lib.Write (File, Stream'Address, Integer (Stream'Length))
      then
         Fail ("error writing to Zip file (disk full ?)");
      end if;
   end Write_Stream;

end J_Zip;
