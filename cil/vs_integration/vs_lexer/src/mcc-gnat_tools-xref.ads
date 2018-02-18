------------------------------------------------------------------------------
--                                                                          --
--                      GNAT VISUAL STUDIO INTEGRATION                      --
--                                                                          --
--                           MCC.GNAT_TOOLS.XREF                            --
--                                                                          --
--                                 S p e c                                  --
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
--  MCC-GNAT_TOOLS-XREF.ADS
--  Description : implements GNAT XREF functionality
--
--  By: Dr. Martin C. Carlisle
--      US Air Force Academy
--
---------------------------------------------------------------

with List;

package MCC.Gnat_Tools.Xref is

   subtype Name_String is String (1 .. 256);
   subtype Type_String is String (1 .. 512);
   subtype Mode_String is String (1 .. 256);

   --  declare data structures
   --  Top level is list of files, each of which has a list of
   --  entities, each of which has a list of references

   type Reference is record
      File_Number : Integer;
      Line        : Integer;
      Col         : Integer;
      Ref_Type    : Character;
   end record;
   type Reference_Pointer is access all Reference;

   package Reference_List_Package is new List (Reference_Pointer);
   subtype Reference_List is Reference_List_Package.Listptr;

   type Entity;
   type Entity_Pointer is access all Entity;
   package Entity_List_Package is new List (Entity_Pointer);
   subtype Entity_List is Entity_List_Package.Listptr;

   type Entity is record
      Line          : Integer;
      Col           : Integer;
      Description   : Character;
      Name          : Name_String;
      Name_Length   : Integer;
      Typ           : Type_String;
      Type_Length   : Integer;
      Param_Modes   : Mode_String := (others => ' ');
      Bracket_Start : Integer := 0;
      Bracket_End   : Integer := 0;
      References    : Reference_List;
      Parameters    : Entity_List;
   end record;

   type Xref_File is record
      Number      : Integer;
      Name        : Name_String;
      Name_Length : Integer;
      Entities    : Entity_List;
   end record;
   type Xref_File_Pointer is access all Xref_File;

   package Xref_File_List_Package is new List (Xref_File_Pointer);
   subtype Xref_File_List is Xref_File_List_Package.Listptr;

   type Xrefs is new Xref_File_List;

   procedure Find
     (Xref        : in     Xrefs;
      File_Name   : in     String;
      Line        : in     Integer;
      Col         : in     Integer;
      X_File      :    out Xref_File_Pointer;
      X_Entity    :    out Entity_Pointer;
      X_Reference :    out Reference_Pointer;
      Found       :    out Boolean);
   --  Look to see if we can find a reference at a particular
   --  line and column of a particular file

   procedure Read_Ali
     (File_Name : in     String;
      Xref      :    out Xrefs);
   --  Create the Xrefs structure from an ALI file

   type Callback_Type is access procedure (Entity : in String);
   procedure Enumerate_Entities
     (Xref     : in Xrefs;
      Callback : in Callback_Type);
   --  Enumerate all entities

   type Extended_Callback_Type is access procedure
     (Filename   : in String;
      The_Entity : in Entity);
   procedure Enumerate_Entities
     (Xref     : in Xrefs;
      Callback : in Extended_Callback_Type);
   --  Same as above, with an extended callback procedure

   type Public_Callback_Type is access procedure
     (The_Entity : in Entity);
   procedure Enumerate_Public_Entities
     (Filename  : in String;
      Xref      : in Xrefs;
      Callback  : in Public_Callback_Type);
   --  Enumerate all public entities

   procedure Free (Xref : in out Xrefs);
   --  Deallocate Xrefs

end MCC.Gnat_Tools.Xref;
