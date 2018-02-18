------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                ADA.NUMERICS.GENERIC_ELEMENTARY_FUNCTIONS                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
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

--  This is the Cert specific version of a-ngelfu.adb

--  This body does not implement Ada.Numerics.Generic_Elementary_Functions as
--  defined by the standard. See the package specification for more details.

with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Numerics.Long_Long_Elementary_Functions;

use Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Long_Elementary_Functions;

package body Ada.Numerics.Generic_Elementary_Functions is

   subtype T is Float_Type'Base;

   subtype F is Float;
   subtype LF is Long_Float;
   subtype LLF is Long_Long_Float;

   Is_Float : constant Boolean :=
     T'Machine_Mantissa = Float'Machine_Mantissa
     and then Float (T'First) = Float'First
     and then Float (T'Last) = Float'Last;

   Is_Long_Float : constant Boolean :=
     T'Machine_Mantissa = Long_Float'Machine_Mantissa
     and then Long_Float (T'First) = Long_Float'First
     and then Long_Float (T'Last) = Long_Float'Last;

   Is_Long_Long_Float : constant Boolean :=
     not (T'Machine_Mantissa = Long_Float'Machine_Mantissa)
     and then T'Machine_Mantissa = Long_Long_Float'Machine_Mantissa
     and then Long_Long_Float (T'First) = Long_Long_Float'First
     and then Long_Long_Float (T'Last) = Long_Long_Float'Last;

   ----------
   -- "**" --
   ----------

   function "**" (Left, Right : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (F (Left) ** F (Right));

      elsif Is_Long_Float then
         return T (LF (Left) ** LF (Right));

      elsif Is_Long_Long_Float then
         return T (LLF (Left) ** LLF (Right));
      end if;

      raise Program_Error;
   end "**";

   ------------
   -- Arccos --
   ------------

   --  Natural cycle

   function Arccos (X : Float_Type'Base) return Float_Type'Base is

   begin
      if Is_Float then
         return T (Arccos (F (X)));

      elsif Is_Long_Float then
         return T (Arccos (LF (X)));

      elsif Is_Long_Long_Float then
         return T (Arccos (LLF (X)));
      end if;

      raise Program_Error;
   end Arccos;

   --  Arbitrary cycle

   function Arccos (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (Arccos (F (X), F (Cycle)));

      elsif Is_Long_Float then
         return T (Arccos (LF (X), LF (Cycle)));

      elsif Is_Long_Long_Float then
         return T (Arccos (LLF (X), LLF (Cycle)));
      end if;

      raise Program_Error;
   end Arccos;

   ------------
   -- Arccot --
   ------------

   --  Natural cycle

   function Arccot
     (X    : Float_Type'Base;
      Y    : Float_Type'Base := 1.0)
      return Float_Type'Base
   is
   begin
      if Is_Float then
         return T (Arccot (F (X), F (Y)));

      elsif Is_Long_Float then
         return T (Arccot (LF (X), LF (Y)));

      elsif Is_Long_Long_Float then
         return T (Arccot (LLF (X), LLF (Y)));
      end if;

      raise Program_Error;
   end Arccot;

   --  Arbitrary cycle

   function Arccot
     (X     : Float_Type'Base;
      Y     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return  Float_Type'Base
   is
   begin
      if Is_Float then
         return T (Arccot (F (X), F (Y), F (Cycle)));

      elsif Is_Long_Float then
         return T (Arccot (LF (X), LF (Y), LF (Cycle)));

      elsif Is_Long_Long_Float then
         return T (Arccot (LLF (X), LLF (Y), LLF (Cycle)));
      end if;

      raise Program_Error;
   end Arccot;

   ------------
   -- Arcsin --
   ------------

   --  Natural cycle

   function Arcsin (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (Arcsin (F (X)));

      elsif Is_Long_Float then
         return T (Arcsin (LF (X)));

      elsif Is_Long_Long_Float then
         return T (Arcsin (LLF (X)));
      end if;

      raise Program_Error;
   end Arcsin;

   --  Arbitrary cycle

   function Arcsin (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (Arcsin (F (X), F (Cycle)));

      elsif Is_Long_Float then
         return T (Arcsin (LF (X), LF (Cycle)));

      elsif Is_Long_Long_Float then
         return T (Arcsin (LLF (X), LLF (Cycle)));
      end if;

      raise Program_Error;
   end Arcsin;

   ------------
   -- Arctan --
   ------------

   --  Natural cycle

   function Arctan
     (Y    : Float_Type'Base;
      X    : Float_Type'Base := 1.0)
      return Float_Type'Base
   is
   begin
      if Is_Float then
         return T (Arctan (F (Y), F (X)));

      elsif Is_Long_Float then
         return T (Arctan (LF (Y), LF (X)));

      elsif Is_Long_Long_Float then
         return T (Arctan (LLF (Y), LLF (X)));
      end if;

      raise Program_Error;
   end Arctan;

   --  Arbitrary cycle

   function Arctan
     (Y     : Float_Type'Base;
      X     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return  Float_Type'Base
   is
   begin
      if Is_Float then
         return T (Arctan (F (Y), F (X), F (Cycle)));

      elsif Is_Long_Float then
         return T (Arctan (LF (Y), LF (X), LF (Cycle)));

      elsif Is_Long_Long_Float then
         return T (Arctan (LLF (Y), LLF (X), LLF (Cycle)));
      end if;

      raise Program_Error;
   end Arctan;

   ---------
   -- Cos --
   ---------

   --  Natural cycle

   function Cos (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (Cos (F (X)));

      elsif Is_Long_Float then
         return T (Cos (LF (X)));

      elsif Is_Long_Long_Float then
         return T (Cos (LLF (X)));
      end if;

      raise Program_Error;
   end Cos;

   --  Arbitrary cycle

   function Cos (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (Cos (F (X), F (Cycle)));

      elsif Is_Long_Float then
         return T (Cos (LF (X), LF (Cycle)));

      elsif Is_Long_Long_Float then
         return T (Cos (LLF (X), LLF (Cycle)));
      end if;

      raise Program_Error;
   end Cos;

   ---------
   -- Cot --
   ---------

   --  Natural cycle

   function Cot (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (Cot (F (X)));

      elsif Is_Long_Float then
         return T (Cot (LF (X)));

      elsif Is_Long_Long_Float then
         return T (Cot (LLF (X)));
      end if;

      raise Program_Error;
   end Cot;

   --  Arbitrary cycle

   function Cot (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (Cot (F (X), F (Cycle)));

      elsif Is_Long_Float then
         return T (Cot (LF (X), LF (Cycle)));

      elsif Is_Long_Long_Float then
         return T (Cot (LLF (X), LLF (Cycle)));
      end if;

      raise Program_Error;
   end Cot;

   ---------
   -- Exp --
   ---------

   function Exp (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (Exp (F (X)));

      elsif Is_Long_Float then
         return T (Exp (LF (X)));

      elsif Is_Long_Long_Float then
         return T (Exp (LLF (X)));
      end if;

      raise Program_Error;
   end Exp;

   ---------
   -- Log --
   ---------

   --  Natural base

   function Log (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (Log (F (X)));

      elsif Is_Long_Float then
         return T (Log (LF (X)));

      elsif Is_Long_Long_Float then
         return T (Log (LLF (X)));
      end if;

      raise Program_Error;
   end Log;

   --  Arbitrary base

   function Log (X, Base : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (Log (F (X), F (Base)));

      elsif Is_Long_Float then
         return T (Log (LF (X), LF (Base)));

      elsif Is_Long_Long_Float then
         return T (Log (LLF (X), LLF (Base)));
      end if;

      raise Program_Error;
   end Log;

   ---------
   -- Sin --
   ---------

   --  Natural cycle

   function Sin (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (Sin (F (X)));

      elsif Is_Long_Float then
         return T (Sin (LF (X)));

      elsif Is_Long_Long_Float then
         return T (Sin (LLF (X)));
      end if;

      raise Program_Error;
   end Sin;

   --  Arbitrary cycle

   function Sin (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (Sin (F (X), F (Cycle)));

      elsif Is_Long_Float then
         return T (Sin (LF (X), LF (Cycle)));

      elsif Is_Long_Long_Float then
         return T (Sin (LLF (X), LLF (Cycle)));
      end if;

      raise Program_Error;
   end Sin;

   ----------
   -- Sqrt --
   ----------

   function Sqrt (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (Sqrt (F (X)));

      elsif Is_Long_Float then
         return T (Sqrt (LF (X)));

      elsif Is_Long_Long_Float then
         return T (Sqrt (LLF (X)));
      end if;

      raise Program_Error;
   end Sqrt;

   ---------
   -- Tan --
   ---------

   --  Natural cycle

   function Tan (X : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (Tan (F (X)));

      elsif Is_Long_Float then
         return T (Tan (LF (X)));

      elsif Is_Long_Long_Float then
         return T (Tan (LLF (X)));
      end if;

      raise Program_Error;
   end Tan;

   --  Arbitrary cycle

   function Tan (X, Cycle : Float_Type'Base) return Float_Type'Base is
   begin
      if Is_Float then
         return T (Tan (F (X), F (Cycle)));

      elsif Is_Long_Float then
         return T (Tan (LF (X), LF (Cycle)));

      elsif Is_Long_Long_Float then
         return T (Tan (LLF (X), LLF (Cycle)));
      end if;

      raise Program_Error;
   end Tan;

end Ada.Numerics.Generic_Elementary_Functions;
