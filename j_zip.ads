------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                J _ Z I P                                 --
--                                                                          --
--                                 S p e c                                  --
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

--  This package contains general utilities to read uncompressed zip files.
--  Compressed zip files are currently not handled.

with GNAT.OS_Lib;
with J_Types;     use J_Types;

package J_Zip is

   function Has_Zip_Format (S : Stream_Of_U1) return Boolean;
   --  Test if S has the format of a zip archive, return True is yes

   subtype Archive_Byte_Offset is Nat_32;
   --  A byte offset in a zip archive

   type File_Info is record
   --  Contains the file info for each file in a zip archive

      Name_First : Archive_Byte_Offset;
      Name_Last  : Archive_Byte_Offset;
      --  Respectively the byte offset where the filename starts and ends. f
      --  the file name is empty then Name_Last = Name_First - 1.

      First : Archive_Byte_Offset;
      Last  : Archive_Byte_Offset;
      --  Respectively the byte offset where the file starts and ends. If the
      --  file has a size of zero then Last = First - 1.

      Compressed : Boolean;
      Encrypted  : Boolean;
      --  Respectively set if the file is compressed/encrypted
   end record;

   type Archive_Directory is array (Natural range <>) of File_Info;

   function Get_Archive_Dir (A : Stream_Of_U1) return Archive_Directory;
   --  A is a zip archive (as a stream of bytes). Return the directory
   --  information of all the files contained in A. Raise Bad_Zip_Archive if A
   --  is not a zip archive or it is a zip archive that the current package
   --  does not know how to handle (e.g. when the output zip file was standard
   --  output or a non seekable device such as a tape).
   --  Compressed_Zip_Archive is raised if the archive is actually compressed.

   type Zip_Archive (Max_Num_Files : Natural) is limited private;
   --  Archive that will contain at most Num_Files. It can have less files at
   --  the end, Num_files is just a maximum value!

   procedure Create_New_Archive (File_Name : String;
                                 Arc       : out Zip_Archive);
   --  Create a new empty archive,
   --  This archive will not have a valid zip format until
   --  it is closed by Close_Archive below
   --  If the file can not be opened, a Name_Error is raised

   procedure Add_Stream_To_Archive (Stream    : Stream_Of_U1;
                                    File_Name : Stream_Of_U1;
                                    Archive   : in out Zip_Archive);
   --  Add a file to the archive created by Create_New_Archive
   --  An exception Bas_Zip_Archive is raised if the Archive is not
   --  opened and the stream could not be written

   procedure Close_Archive (Archive : in out Zip_Archive);
   --  Close the zip file, adding the required tail at the end
   --  of the file

   Bad_Zip_Archive : exception;
   Compressed_Zip_Archive : exception;

private

   subtype String_Length is Int_32 range 0 .. 511;
   type Zip_List_Info (Name_Length : String_Length := 511) is record
      Stream_Length : U4;
      Crc_32        : U4;
      Offset_Start  : U4;
      Offset_End    : U4;
      Name          : Stream_Of_U1 (1 .. Name_Length);
   end record;
   type Zip_List_Info_Array is array (Natural range <>) of Zip_List_Info;

   type Zip_Archive (Max_Num_Files : Natural) is record
      File      : GNAT.OS_Lib.File_Descriptor;
      Num_Files : Natural := 0;
      Is_Closed : Boolean := True;
      File_List : Zip_List_Info_Array (1 .. Max_Num_Files);
   end record;

end J_Zip;
