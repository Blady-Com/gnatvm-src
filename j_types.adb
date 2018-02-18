------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J _ T Y P E S                               --
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
with Interfaces;               use Interfaces;

package body J_Types is

   function Strip (S : String) return String;
   --  Strips all leading and trailing spaces from S and returns the stripped
   --  string. (Note: This utility function is copied directly from package
   --  J_Basics in order to break a circularity between this J_Basics and
   --  this package which causes the JGNAt bind to fail. The duplication of
   --  this simple function seems preferable to binding with the -f switch.)

   -----------
   -- Image --
   -----------

   function Image (V : U1) return String is
   begin
      return Strip (U1'Image (V));
   end Image;

   function Image (V : U2) return String is
   begin
      return Strip (U2'Image (V));
   end Image;

   function Image (V : U4) return String is
   begin
      return Strip (U4'Image (V));
   end Image;

   function Image (V : U8) return String is
   begin
      return Strip (U8'Image (V));
   end Image;

   function Image (V : Int_8) return String is
   begin
      return Strip (Int_8'Image (V));
   end Image;

   function Image (V : Int_16) return String is
   begin
      return Strip (Int_16'Image (V));
   end Image;

   function Image (V : Int_32) return String is
   begin
      return Strip (Int_32'Image (V));
   end Image;

   function Image (V : Int_64) return String is
   begin
      return Strip (Int_64'Image (V));
   end Image;

   ----------------
   -- Shift_Left --
   ----------------

   function Shift_Left (Value : U1; Amount : Natural) return U1 is
   begin
      return U1 (Interfaces.Shift_Left (Unsigned_8 (Value), Amount));
   end Shift_Left;

   function Shift_Left (Value : U2; Amount : Natural) return U2 is
   begin
      return U2 (Interfaces.Shift_Left (Unsigned_16 (Value), Amount));
   end Shift_Left;

   function Shift_Left (Value : U4; Amount : Natural) return U4 is
   begin
      return U4 (Interfaces.Shift_Left (Unsigned_32 (Value), Amount));
   end Shift_Left;

   function Shift_Left (Value : U8; Amount : Natural) return U8 is
   begin
      return U8 (Interfaces.Shift_Left (Unsigned_64 (Value), Amount));
   end Shift_Left;

   -----------------
   -- Shift_Right --
   -----------------

   function Shift_Right (Value : U1; Amount : Natural) return U1 is
   begin
      return U1 (Interfaces.Shift_Right (Unsigned_8 (Value), Amount));
   end Shift_Right;

   function Shift_Right (Value : U2; Amount : Natural) return U2 is
   begin
      return U2 (Interfaces.Shift_Right (Unsigned_16 (Value), Amount));
   end Shift_Right;

   function Shift_Right (Value : U4; Amount : Natural) return U4 is
   begin
      return U4 (Interfaces.Shift_Right (Unsigned_32 (Value), Amount));
   end Shift_Right;

   function Shift_Right (Value : U8; Amount : Natural) return U8 is
   begin
      return U8 (Interfaces.Shift_Right (Unsigned_64 (Value), Amount));
   end Shift_Right;

   -----------
   -- Strip --
   -----------

   function Strip (S : String) return String is
      F : Integer := S'First;
      L : Integer := S'Last;

   begin
      while F <= L and then S (F) = ' ' loop
         F := F + 1;
      end loop;

      while F <= L and then S (L) = ' ' loop
         L := L - 1;
      end loop;

      return S (F .. L);
   end Strip;

   ------------------
   -- To_Half_Word --
   ------------------

   function To_Half_Word (I : Int_16) return Half_Word is
   begin
      return To_Half_Word (U2 (I));
   end To_Half_Word;

   procedure To_Half_Word (I : Int_16; B0, B1 : out U1) is
   begin
      To_Half_Word (U2 (I), B0, B1);
   end To_Half_Word;

   function To_Half_Word (U : U2) return Half_Word is
   begin
      return (B0 => U1 (U / 256), B1 => U1 (U mod 256));
   end To_Half_Word;

   procedure To_Half_Word (U : U2; B0, B1 : out U1) is
   begin
      B0 := U1 (U / 256);
      B1 := U1 (U mod 256);
   end To_Half_Word;

   ----------------
   -- To_IEEE_32 --
   ----------------

   function To_IEEE_Float_32 (V : U4) return IEEE_Float_32 is
      function Convert is new Unchecked_Conversion (U4, IEEE_Float_32);
   begin
      return Convert (V);
   end To_IEEE_Float_32;

   ----------------
   -- To_IEEE_64 --
   ----------------

   function To_IEEE_Float_64 (V : U8) return IEEE_Float_64 is
      function Convert is new Unchecked_Conversion (U8, IEEE_Float_64);
   begin
      return Convert (V);
   end To_IEEE_Float_64;

   --------------
   -- To_Int_8 --
   --------------

   function To_Int_8 (V : U1) return Int_8 is
      function Convert is new Unchecked_Conversion (U1, Int_8);
   begin
      return Convert (V);
   end To_Int_8;

   ---------------
   -- To_Int_16 --
   ---------------

   function To_Int_16 (B0, B1 : U1) return Int_16 is
   begin
      return Int_16 (To_U2 (B0, B1));
   end To_Int_16;

   function To_Int_16 (H : Half_Word) return Int_16 is
   begin
      return Int_16 (To_U2 (H));
   end To_Int_16;

   function To_Int_16 (V : U2) return Int_16 is
      function Convert is new Unchecked_Conversion (U2, Int_16);
   begin
      return Convert (V);
   end To_Int_16;

   ---------------
   -- To_Int_32 --
   ---------------

   function To_Int_32 (B0, B1, B2, B3 : U1) return Int_32 is
   begin
      return Int_32 (To_U4 (B0, B1, B2, B3));
   end To_Int_32;

   function To_Int_32 (W : Word) return Int_32 is
   begin
      return Int_32 (To_U4 (W));
   end To_Int_32;

   function To_Int_32 (V : U4) return Int_32 is
      function Convert is new Unchecked_Conversion (U4, Int_32);
   begin
      return Convert (V);
   end To_Int_32;

   ---------------
   -- To_Int_64 --
   ---------------

   function To_Int_64 (V : U8) return Int_64 is
      function Convert is new Unchecked_Conversion (U8, Int_64);
   begin
      return Convert (V);
   end To_Int_64;

   -----------
   -- To_U1 --
   -----------

   function To_U1 (V : Int_8) return U1 is
      function Convert is new Unchecked_Conversion (Int_8, U1);
   begin
      return Convert (V);
   end To_U1;

   -----------
   -- To_U2 --
   -----------

   function To_U2 (B0, B1 : U1) return U2 is
   begin
      return U2 (B0) * 256 + U2 (B1);
   end To_U2;

   function To_U2 (H : Half_Word) return U2 is
   begin
      return To_U2 (H.B0, H.B1);
   end To_U2;

   function To_U2 (V : Int_16) return U2 is
      function Convert is new Unchecked_Conversion (Int_16, U2);
   begin
      return Convert (V);
   end To_U2;

   -----------
   -- To_U4 --
   -----------

   function To_U4 (B0, B1, B2, B3 : U1) return U4 is
   begin
      return ((U4 (B0) * 256 + U4 (B1)) * 256 + U4 (B2)) * 256 + U4 (B3);
   end To_U4;

   function To_U4 (W : Word) return U4 is
   begin
      return To_U4 (W.B0, W.B1, W.B2, W.B3);
   end To_U4;

   procedure To_U4 (U : U8; Hi, Lo : out U4) is
   begin
      Hi := U4 (U / 2 ** 32);
      Lo := U4 (U mod 2 ** 32);
   end To_U4;

   function To_U4 (V : Int_32) return U4 is
      function Convert is new Unchecked_Conversion (Int_32, U4);
   begin
      return Convert (V);
   end To_U4;

   function To_U4 (V : IEEE_Float_32) return U4 is
      function Convert is new Unchecked_Conversion (IEEE_Float_32, U4);
   begin
      return Convert (V);
   end To_U4;

   -----------
   -- To_U8 --
   -----------

   function To_U8 (Hi, Lo : U4) return U8 is
   begin
      return U8 (Hi) * (2 ** 32) + U8 (Lo);
   end To_U8;

   function To_U8 (V : Int_64) return U8 is
      function Convert is new Unchecked_Conversion (Int_64, U8);
   begin
      return Convert (V);
   end To_U8;

   function To_U8 (V : IEEE_Float_64) return U8 is
      function Convert is new Unchecked_Conversion (IEEE_Float_64, U8);
   begin
      return Convert (V);
   end To_U8;

   -------------
   -- To_Word --
   -------------

   function To_Word (I : Int_32) return Word is
   begin
      return To_Word (U4 (I));
   end To_Word;

   procedure To_Word (I : Int_32; B0, B1, B2, B3 : out U1) is
   begin
      To_Word (U4 (I), B0, B1, B2, B3);
   end To_Word;

   function To_Word (U : U4) return Word is
   begin
      return (B0 => U1  (U / 2 ** 24),
              B1 => U1 ((U / 2 ** 16) mod 256),
              B2 => U1 ((U / 2 **  8) mod 256),
              B3 => U1  (U            mod 256));
   end To_Word;

   procedure To_Word (U : U4; B0, B1, B2, B3 : out U1) is
   begin
      B0 := U1  (U / 2 ** 24);
      B1 := U1 ((U / 2 ** 16) mod 256);
      B2 := U1 ((U / 2 **  8) mod 256);
      B3 := U1  (U            mod 256);
   end To_Word;

end J_Types;
