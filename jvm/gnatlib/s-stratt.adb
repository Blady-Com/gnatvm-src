------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             S Y S T E M . S T R E A M _ A T T R I B U T E S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1998-2010, AdaCore                     --
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

--  This is the JGNAT-specific version of the body of System.Stream_Attributes

with Ada.IO_Exceptions;
with Ada.Streams; use Ada.Streams;

package body System.Stream_Attributes is

   Err : exception renames Ada.IO_Exceptions.End_Error;
   --  Exception raised if insufficient data read (note that the RM implies
   --  that Data_Error might be the appropriate choice, but AI195-00132
   --  decides with a binding interpretation that End_Error is preferred).

   SU : constant := System.Storage_Unit;

   subtype SEA is Ada.Streams.Stream_Element_Array;
   subtype SEO is Ada.Streams.Stream_Element_Offset;

   --  generic function UC renames Unchecked_Conversion;

   --  Subtypes used to define Stream_Element_Array values that map
   --  into the elementary types, using unchecked conversion.

   --  (unused constants)
   --  Thin_Pointer_Size : constant := System.Address'Size;
   --  Fat_Pointer_Size  : constant := System.Address'Size * 2;

   --  (unused subtypes)
   --  subtype S_AD  is
   --    SEA (1 .. (Fat_Pointer_Size              + SU - 1) / SU);
   --  subtype S_AS  is
   --    SEA (1 .. (Thin_Pointer_Size             + SU - 1) / SU);
   subtype S_B   is SEA (1 .. (Boolean'Size                  + SU - 1) / SU);
   subtype S_C   is SEA (1 .. (Character'Size                + SU - 1) / SU);
   subtype S_F   is SEA (1 .. (Float'Size                    + SU - 1) / SU);
   subtype S_I   is SEA (1 .. (Integer'Size                  + SU - 1) / SU);
   subtype S_LF  is SEA (1 .. (Long_Float'Size               + SU - 1) / SU);
   subtype S_LI  is SEA (1 .. (Long_Integer'Size             + SU - 1) / SU);
   subtype S_LLF is SEA (1 .. (Long_Long_Float'Size          + SU - 1) / SU);
   subtype S_LLI is SEA (1 .. (Long_Long_Integer'Size        + SU - 1) / SU);
   subtype S_LLU is SEA (1 .. (UST.Long_Long_Unsigned'Size   + SU - 1) / SU);
   subtype S_LU  is SEA (1 .. (UST.Long_Unsigned'Size        + SU - 1) / SU);
   subtype S_SF  is SEA (1 .. (Short_Float'Size              + SU - 1) / SU);
   subtype S_SSI is SEA (1 .. (Short_Short_Integer'Size      + SU - 1) / SU);
   subtype S_SSU is SEA (1 .. (UST.Short_Short_Unsigned'Size + SU - 1) / SU);
   subtype S_SU  is SEA (1 .. (UST.Short_Unsigned'Size       + SU - 1) / SU);
   subtype S_U   is SEA (1 .. (UST.Unsigned'Size             + SU - 1) / SU);
   subtype S_WC  is SEA (1 .. (Wide_Character'Size           + SU - 1) / SU);

   --  Unchecked conversions from the elementary type to the stream type

   --  The JVM does not directly support unchecked conversions from
   --  scalar types to byte arrays, so we import a set of conversion
   --  methods that are implemented using the API DataOutputStream class.

   --  ??? function From_AD  (From : Fat_Pointer) return S_AD;
   --  ??? function From_AS  (From : Thin_Pointer) return S_AS;

   function From_C   (From : Character) return S_C;
   pragma Import (Java, From_C, "jgnat.adalib.Prim_Conversions.From_C");

   function From_F   (From : Float) return S_F;
   pragma Import (Java, From_F, "jgnat.adalib.Prim_Conversions.From_F");

   function From_I   (From : Integer) return S_I;
   pragma Import (Java, From_I, "jgnat.adalib.Prim_Conversions.From_I");

   function From_LF  (From : Long_Float) return S_LF;
   pragma Import (Java, From_LF, "jgnat.adalib.Prim_Conversions.From_LF");

   function From_LI  (From : Long_Integer) return S_LI;
   pragma Import (Java, From_LI, "jgnat.adalib.Prim_Conversions.From_LI");

   --  function From_LLF (From : Long_Long_Float) return S_LLF;
   --  pragma Import (Java, From_LLF, "jgnat.adalib.Prim_Conversions.From_LF");

   --  function From_LLI (From : Long_Long_Integer) return S_LLI;
   --  pragma Import (Java, From_LLI, "jgnat.adalib.Prim_Conversions.From_LI");

   function From_LLU (From : UST.Long_Long_Unsigned) return S_LLU;
   pragma Import (Java, From_LLU, "jgnat.adalib.Prim_Conversions.From_LI");

   function From_LU  (From : UST.Long_Unsigned) return S_LU;
   pragma Import (Java, From_LU, "jgnat.adalib.Prim_Conversions.From_LI");

   function From_SF  (From : Short_Float) return S_SF;
   pragma Import (Java, From_SF, "jgnat.adalib.Prim_Conversions.From_F");

   --  function From_SI  (From : Short_Integer) return S_SI;
   --  pragma Import (Java, From_SI, "jgnat.adalib.Prim_Conversions.From_SI");

   function From_SSI (From : Short_Short_Integer) return S_SSI;
   pragma Import (Java, From_SSI, "jgnat.adalib.Prim_Conversions.From_B");

   function From_SSU (From : UST.Short_Short_Unsigned) return S_SSU;
   pragma Import (Java, From_SSU, "jgnat.adalib.Prim_Conversions.From_B");

   function From_SU  (From : UST.Short_Unsigned) return S_SU;
   pragma Import (Java, From_SU, "jgnat.adalib.Prim_Conversions.From_I");
   --  pragma Import (Java, From_SU, "jgnat.adalib.Prim_Conversions.From_SI");

   function From_U   (From : UST.Unsigned) return S_U;
   pragma Import (Java, From_U, "jgnat.adalib.Prim_Conversions.From_I");

   function From_WC  (From : Wide_Character) return S_WC;
   pragma Import (Java, From_WC, "jgnat.adalib.Prim_Conversions.From_WC");

   --  Unchecked conversions from the stream type to elementary type

   --  The JVM does not directly support unchecked conversions from
   --  byte arrays to scalar types, so we import a set of conversion
   --  methods that are implemented using the API DataInputStream class.

   --  ??? function To_AD  (From : S_AD) return Fat_Pointer;
   --  ??? function To_AS  (From : S_AS) return Thin_Pointer;

   function To_C   (From : S_C) return Character;
   pragma Import (Java, To_C, "jgnat.adalib.Prim_Conversions.To_C");

   function To_F   (From : S_F) return Float;
   pragma Import (Java, To_F, "jgnat.adalib.Prim_Conversions.To_F");

   function To_I   (From : S_I) return Integer;
   pragma Import (Java, To_I, "jgnat.adalib.Prim_Conversions.To_I");

   function To_LF  (From : S_LF) return Long_Float;
   pragma Import (Java, To_LF, "jgnat.adalib.Prim_Conversions.To_LF");

   function To_LI  (From : S_LI) return Long_Integer;
   pragma Import (Java, To_LI, "jgnat.adalib.Prim_Conversions.To_LI");

   function To_LLF (From : S_LLF) return Long_Long_Float;
   pragma Import (Java, To_LLF, "jgnat.adalib.Prim_Conversions.To_LF");

   function To_LLI (From : S_LLI) return Long_Long_Integer;
   pragma Import (Java, To_LLI, "jgnat.adalib.Prim_Conversions.To_LI");

   function To_LLU (From : S_LLU) return UST.Long_Long_Unsigned;
   pragma Import (Java, To_LLU, "jgnat.adalib.Prim_Conversions.To_LI");

   function To_LU  (From : S_LU) return UST.Long_Unsigned;
   pragma Import (Java, To_LU, "jgnat.adalib.Prim_Conversions.To_LI");

   function To_SF  (From : S_SF) return Short_Float;
   pragma Import (Java, To_SF, "jgnat.adalib.Prim_Conversions.To_F");

   --  function To_SI  (From : S_SI) return Short_Integer;
   --  pragma Import (Java, To_SI, "jgnat.adalib.Prim_Conversions.To_I");
   --  --  pragma Import (Java, To_SI, "jgnat.adalib.Prim_Conversions.To_SI");

   function To_SSI (From : S_SSI) return Short_Short_Integer;
   pragma Import (Java, To_SSI, "jgnat.adalib.Prim_Conversions.To_B");

   function To_SSU (From : S_SSU) return UST.Short_Short_Unsigned;
   pragma Import (Java, To_SSU, "jgnat.adalib.Prim_Conversions.To_B");

   --  function To_SU  (From : S_SU) return UST.Short_Unsigned;
   --  pragma Import (Java, To_SU, "jgnat.adalib.Prim_Conversions.To_I");
   --  --  pragma Import (Java, To_SU, "jgnat.adalib.Prim_Conversions.To_SI");

   function To_U (From : S_U) return UST.Unsigned;
   pragma Import (Java, To_U, "jgnat.adalib.Prim_Conversions.To_I");

   function To_WC  (From : S_WC) return Wide_Character;
   pragma Import (Java, To_WC, "jgnat.adalib.Prim_Conversions.To_WC");

   ----------
   -- I_AD --
   ----------

   function I_AD (Stream : not null access RST) return Fat_Pointer is
   begin
      --  Reading of fat pointers not supported for JGNAT ???

      raise Program_Error;

      return (Null_Address, Null_Address);
   end I_AD;

   ----------
   -- I_AS --
   ----------

   function I_AS (Stream : not null access RST) return Thin_Pointer is
   begin
      --  Reading of thin pointers not supported for JGNAT ???

      raise Program_Error;

      return (P1 => Null_Address);
   end I_AS;

   ---------
   -- I_B --
   ---------

   function I_B (Stream : not null access RST) return Boolean is
      T : S_B;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return Boolean'Val (T (1));
      end if;
   end I_B;

   ---------
   -- I_C --
   ---------

   function I_C (Stream : not null access RST) return Character is
      T : S_C;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_C (T);
      end if;
   end I_C;

   ---------
   -- I_F --
   ---------

   function I_F (Stream : not null access RST) return Float is
      T : S_F;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_F (T);
      end if;
   end I_F;

   ---------
   -- I_I --
   ---------

   function I_I (Stream : not null access RST) return Integer is
      T : S_I;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_I (T);
      end if;
   end I_I;

   ----------
   -- I_LF --
   ----------

   function I_LF (Stream : not null access RST) return Long_Float is
      T : S_LF;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_LF (T);
      end if;
   end I_LF;

   ----------
   -- I_LI --
   ----------

   function I_LI (Stream : not null access RST) return Long_Integer is
      T : S_LI;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_LI (T);
      end if;
   end I_LI;

   -----------
   -- I_LLF --
   -----------

   function I_LLF (Stream : not null access RST) return Long_Long_Float is
      T : S_LLF;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_LLF (T);
      end if;
   end I_LLF;

   -----------
   -- I_LLI --
   -----------

   function I_LLI (Stream : not null access RST) return Long_Long_Integer is
      T : S_LLI;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_LLI (T);
      end if;
   end I_LLI;

   -----------
   -- I_LLU --
   -----------

   function I_LLU
     (Stream : not null access RST) return UST.Long_Long_Unsigned
   is
      T : S_LLU;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_LLU (T);
      end if;
   end I_LLU;

   ----------
   -- I_LU --
   ----------

   function I_LU (Stream : not null access RST) return UST.Long_Unsigned is
      T : S_LU;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_LU (T);
      end if;
   end I_LU;

   ----------
   -- I_SF --
   ----------

   function I_SF (Stream : not null access RST) return Short_Float is
      T : S_SF;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_SF (T);
      end if;
   end I_SF;

   ----------
   -- I_SI --
   ----------

   function I_SI (Stream : not null access RST) return Short_Integer is
      --  Read the value in as a four-byte int value rather than
      --  a two-byte short value. Workaround needed because currently
      --  we map Short_Integer values to 32 bits.
      --
      --  T : S_SI;
      T : S_I;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         --  return To_SI (T);
         return Short_Integer (To_I (T));
      end if;
   end I_SI;

   -----------
   -- I_SSI --
   -----------

   function I_SSI (Stream : not null access RST) return Short_Short_Integer is
      T : S_SSI;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_SSI (T);
      end if;
   end I_SSI;

   -----------
   -- I_SSU --
   -----------

   function I_SSU
     (Stream : not null access RST) return UST.Short_Short_Unsigned
   is
      T : S_SSU;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_SSU (T);
      end if;
   end I_SSU;

   ----------
   -- I_SU --
   ----------

   function I_SU (Stream : not null access RST) return UST.Short_Unsigned is
      --  Read the value in as a four-byte value rather than
      --  a two-byte short value. Workaround needed because
      --  currently we map Short_Unsigned values to 32 bits.
      --
      --  T : S_SU;
      T : S_U;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return UST.Short_Unsigned (To_U (T));
      end if;
   end I_SU;

   ---------
   -- I_U --
   ---------

   function I_U (Stream : not null access RST) return UST.Unsigned is
      T : S_U;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_U (T);
      end if;
   end I_U;

   ----------
   -- I_WC --
   ----------

   function I_WC (Stream : not null access RST) return Wide_Character is
      T : S_WC;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return To_WC (T);
      end if;
   end I_WC;

   -----------
   -- I_WWC --
   -----------

   function I_WWC (Stream : not null access RST) return Wide_Wide_Character is
      T : S_I;
      L : SEO;

   begin
      Ada.Streams.Read (Stream.all, T, L);

      if L < T'Last then
         raise Err;
      else
         return Wide_Wide_Character'Val (To_I (T));
      end if;
   end I_WWC;

   -----------------
   -- Block_IO_OK --
   -----------------

   function Block_IO_OK return Boolean is
   begin
      return True;
   end Block_IO_OK;

   ----------
   -- W_AD --
   ----------

   procedure W_AD (Stream : not null access RST; Item : Fat_Pointer) is
   begin
      --  Writing of fat pointers not supported for JGNAT ???

      raise Program_Error;
   end W_AD;

   ----------
   -- W_AS --
   ----------

   procedure W_AS (Stream : not null access RST; Item : Thin_Pointer) is
   begin
      --  Writing of thin pointers not supported for JGNAT ???

      raise Program_Error;
   end W_AS;

   ---------
   -- W_B --
   ---------

   procedure W_B (Stream : not null access RST; Item : Boolean) is
      T : S_B;
   begin
      T (1) := Boolean'Pos (Item);
      Ada.Streams.Write (Stream.all, T);
   end W_B;

   ---------
   -- W_C --
   ---------

   procedure W_C (Stream : not null access RST; Item : Character) is
      T : constant S_C := From_C (Item);
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_C;

   ---------
   -- W_F --
   ---------

   procedure W_F (Stream : not null access RST; Item : Float) is
      T : constant S_F := From_F (Item);
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_F;

   ---------
   -- W_I --
   ---------

   procedure W_I (Stream : not null access RST; Item : Integer) is
      T : constant S_I := From_I (Item);
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_I;

   ----------
   -- W_LF --
   ----------

   procedure W_LF (Stream : not null access RST; Item : Long_Float) is
      T : constant S_LF := From_LF (Item);
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_LF;

   ----------
   -- W_LI --
   ----------

   procedure W_LI (Stream : not null access RST; Item : Long_Integer) is
      T : constant S_LI := From_LI (Item);
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_LI;

   -----------
   -- W_LLF --
   -----------

   procedure W_LLF (Stream : not null access RST; Item : Long_Long_Float) is
      T : constant S_LF := From_LF (Long_Float (Item));
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_LLF;

   -----------
   -- W_LLI --
   -----------

   procedure W_LLI (Stream : not null access RST; Item : Long_Long_Integer) is
      T : constant S_LI := From_LI (Long_Integer (Item));
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_LLI;

   -----------
   -- W_LLU --
   -----------

   procedure W_LLU
     (Stream : not null access RST; Item : UST.Long_Long_Unsigned)
   is
      T : constant S_LLU := From_LLU (Item);
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_LLU;

   ----------
   -- W_LU --
   ----------

   procedure W_LU (Stream : not null access RST; Item : UST.Long_Unsigned) is
      T : constant S_LU := From_LU (Item);
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_LU;

   ----------
   -- W_SF --
   ----------

   procedure W_SF (Stream : not null access RST; Item : Short_Float) is
      T : constant S_SF := From_SF (Item);
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_SF;

   ----------
   -- W_SI --
   ----------

   procedure W_SI (Stream : not null access RST; Item : Short_Integer) is
      T : constant S_I := From_I (Integer (Item));
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_SI;

   -----------
   -- W_SSI --
   -----------

   procedure W_SSI
     (Stream : not null access RST; Item : Short_Short_Integer)
   is
      T : constant S_SSI := From_SSI (Item);
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_SSI;

   -----------
   -- W_SSU --
   -----------

   procedure W_SSU
     (Stream : not null access RST; Item : UST.Short_Short_Unsigned)
   is
      T : constant S_SSU := From_SSU (Item);
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_SSU;

   ----------
   -- W_SU --
   ----------

   procedure W_SU (Stream : not null access RST; Item : UST.Short_Unsigned) is
      T : constant S_SU := From_SU (Item);
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_SU;

   ---------
   -- W_U --
   ---------

   procedure W_U (Stream : not null access RST; Item : UST.Unsigned) is
      T : constant S_U := From_U (Item);
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_U;

   ----------
   -- W_WC --
   ----------

   procedure W_WC (Stream : not null access RST; Item : Wide_Character) is
      T : constant S_WC := From_WC (Item);
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_WC;

   -----------
   -- W_WWC --
   -----------

   procedure W_WWC
     (Stream : not null access RST; Item : Wide_Wide_Character)
   is
      T : constant S_I := From_I (Wide_Wide_Character'Pos (Item));
   begin
      Ada.Streams.Write (Stream.all, T);
   end W_WWC;

end System.Stream_Attributes;
