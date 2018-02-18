------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                  ADA.NUMERICS.LONG_ELEMENTARY_FUNCTIONS                  --
--                                                                          --
--                                 S p e c                                  --
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

--  This is the Cert specific version of a-nlelfu.ads.
--  The separate version is necessary, because this system does not
--  provide an implementation of tanh, among other hyperbolic functions.
--  The run time currently has no code to implement this function,
--  so the only short term option was to remove the hyperbolic functions.

with System.Generic_C_Math_Interface;

package Ada.Numerics.Long_Elementary_Functions is
pragma Pure (Long_Elementary_Functions);

   function Sqrt    (X           : Long_Float) return Long_Float;
   function Log     (X           : Long_Float) return Long_Float;
   function Log     (X, Base     : Long_Float) return Long_Float;
   function Exp     (X           : Long_Float) return Long_Float;
   function "**"    (Left, Right : Long_Float) return Long_Float;

   function Sin     (X           : Long_Float) return Long_Float;
   function Sin     (X, Cycle    : Long_Float) return Long_Float;
   function Cos     (X           : Long_Float) return Long_Float;
   function Cos     (X, Cycle    : Long_Float) return Long_Float;
   function Tan     (X           : Long_Float) return Long_Float;
   function Tan     (X, Cycle    : Long_Float) return Long_Float;
   function Cot     (X           : Long_Float) return Long_Float;
   function Cot     (X, Cycle    : Long_Float) return Long_Float;

   function Arcsin  (X           : Long_Float) return Long_Float;
   function Arcsin  (X, Cycle    : Long_Float) return Long_Float;
   function Arccos  (X           : Long_Float) return Long_Float;
   function Arccos  (X, Cycle    : Long_Float) return Long_Float;

   function Arctan
     (Y   : Long_Float;
      X   : Long_Float := 1.0)
     return Long_Float;

   function Arctan
     (Y     : Long_Float;
      X     : Long_Float := 1.0;
      Cycle : Long_Float)
      return  Long_Float;

   function Arccot
     (X   : Long_Float;
      Y   : Long_Float := 1.0)
     return Long_Float;

   function Arccot
     (X     : Long_Float;
      Y     : Long_Float := 1.0;
      Cycle : Long_Float)
     return   Long_Float;

private
   pragma Assert (Long_Float'Machine_Radix = 2,
      "only binary Long_Floating-point types supported");

   function C_Sqrt  (X    : Long_Float) return Long_Float;
   function C_Log   (X    : Long_Float) return Long_Float;
   function C_Exp   (X    : Long_Float) return Long_Float;
   function C_Pow   (X, Y : Long_Float) return Long_Float;

   function C_Sin   (X    : Long_Float) return Long_Float;
   function C_Cos   (X    : Long_Float) return Long_Float;
   function C_Tan   (X    : Long_Float) return Long_Float;

   function C_Asin  (X    : Long_Float) return Long_Float;
   function C_Acos  (X    : Long_Float) return Long_Float;
   function C_Atan2 (Y, X : Long_Float) return Long_Float;

   pragma Import (C, C_Sqrt, "sqrt");
   pragma Import (C, C_Log, "log");
   pragma Import (C, C_Exp, "exp");
   pragma Import (C, C_Pow, "pow");

   pragma Import (C, C_Sin, "sin");
   pragma Import (C, C_Cos, "cos");
   pragma Import (C, C_Tan, "tan");

   pragma Import (C,  C_Asin, "asin");
   pragma Import (C,  C_Acos, "acos");
   pragma Import (C,  C_Atan2, "atan2");

   package CMI is new System.Generic_C_Math_Interface (Long_Float);

   function Sqrt (X           : Long_Float) return Long_Float renames CMI.Sqrt;
   function Log  (X           : Long_Float) return Long_Float renames CMI.Log;
   function Log  (X, Base     : Long_Float) return Long_Float renames CMI.Log;
   function Exp  (X           : Long_Float) return Long_Float renames CMI.Exp;
   function "**" (Left, Right : Long_Float) return Long_Float renames CMI."**";

   function Sin  (X           : Long_Float) return Long_Float renames CMI.Sin;
   function Sin  (X, Cycle    : Long_Float) return Long_Float renames CMI.Sin;
   function Cos  (X           : Long_Float) return Long_Float renames CMI.Cos;
   function Cos  (X, Cycle    : Long_Float) return Long_Float renames CMI.Cos;
   function Tan  (X           : Long_Float) return Long_Float renames CMI.Tan;
   function Tan  (X, Cycle    : Long_Float) return Long_Float renames CMI.Tan;
   function Cot  (X           : Long_Float) return Long_Float renames CMI.Cot;
   function Cot  (X, Cycle    : Long_Float) return Long_Float renames CMI.Cot;

   function Arcsin (X : Long_Float) return Long_Float renames CMI.Arcsin;

   function Arcsin
     (X, Cycle : Long_Float)
     return      Long_Float renames CMI.Arcsin;

   function Arccos (X : Long_Float) return Long_Float renames CMI.Arccos;

   function Arccos
     (X, Cycle : Long_Float)
     return      Long_Float renames CMI.Arccos;

   function Arctan
     (Y     : Long_Float;
      X     : Long_Float := 1.0)
     return   Long_Float renames CMI.Arctan;

   function Arctan
     (Y     : Long_Float;
      X     : Long_Float := 1.0;
      Cycle : Long_Float)
     return   Long_Float renames CMI.Arctan;

   function Arccot
     (X     : Long_Float;
      Y     : Long_Float := 1.0)
     return   Long_Float renames CMI.Arccot;

   function Arccot
     (X     : Long_Float;
      Y     : Long_Float := 1.0;
      Cycle : Long_Float)
     return   Long_Float renames CMI.Arccot;

end Ada.Numerics.Long_Elementary_Functions;
