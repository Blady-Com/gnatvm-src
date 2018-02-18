------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--     A D A . N U M E R I C S . E L E M E N T A R Y _ F U N C T I O N S    --
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

--  This is the Cert specific version of a-ngelfu.adb

--  The separate version is necessary, because this system does not provide an
--  implementation of tanh, among other hyperbolic functions.

--  The run time currently has no code to implement this function, so the only
--  short term option was to remove the hyperbolic functions.

with System.Generic_C_Math_Interface;

package Ada.Numerics.Elementary_Functions is
pragma Pure (Elementary_Functions);

   function Sqrt    (X           : Float) return Float;
   function Log     (X           : Float) return Float;
   function Log     (X, Base     : Float) return Float;
   function Exp     (X           : Float) return Float;
   function "**"    (Left, Right : Float) return Float;

   function Sin     (X           : Float) return Float;
   function Sin     (X, Cycle    : Float) return Float;
   function Cos     (X           : Float) return Float;
   function Cos     (X, Cycle    : Float) return Float;
   function Tan     (X           : Float) return Float;
   function Tan     (X, Cycle    : Float) return Float;
   function Cot     (X           : Float) return Float;
   function Cot     (X, Cycle    : Float) return Float;

   function Arcsin  (X           : Float) return Float;
   function Arcsin  (X, Cycle    : Float) return Float;
   function Arccos  (X           : Float) return Float;
   function Arccos  (X, Cycle    : Float) return Float;

   function Arctan
     (Y   : Float;
      X   : Float := 1.0) return Float;

   function Arctan
     (Y     : Float;
      X     : Float := 1.0;
      Cycle : Float) return  Float;

   function Arccot
     (X   : Float;
      Y   : Float := 1.0)
     return Float;

   function Arccot
     (X     : Float;
      Y     : Float := 1.0;
      Cycle : Float)
     return   Float;

private
   pragma Assert (Float'Machine_Radix = 2,
      "only binary floating-point types supported");

   function C_Sqrt  (X    : Float) return Float;
   function C_Log   (X    : Float) return Float;
   function C_Exp   (X    : Float) return Float;
   function C_Pow   (X, Y : Float) return Float;

   function C_Sin   (X    : Float) return Float;
   function C_Cos   (X    : Float) return Float;
   function C_Tan   (X    : Float) return Float;

   function C_Asin  (X    : Float) return Float;
   function C_Acos  (X    : Float) return Float;
   function C_Atan2 (Y, X : Float) return Float;

   package CMI is new System.Generic_C_Math_Interface (Float);

   function Sqrt    (X           : Float) return Float renames CMI.Sqrt;
   function Log     (X           : Float) return Float renames CMI.Log;
   function Log     (X, Base     : Float) return Float renames CMI.Log;
   function Exp     (X           : Float) return Float renames CMI.Exp;
   function "**"    (Left, Right : Float) return Float renames CMI."**";

   function Sin     (X           : Float) return Float renames CMI.Sin;
   function Sin     (X, Cycle    : Float) return Float renames CMI.Sin;
   function Cos     (X           : Float) return Float renames CMI.Cos;
   function Cos     (X, Cycle    : Float) return Float renames CMI.Cos;
   function Tan     (X           : Float) return Float renames CMI.Tan;
   function Tan     (X, Cycle    : Float) return Float renames CMI.Tan;
   function Cot     (X           : Float) return Float renames CMI.Cot;
   function Cot     (X, Cycle    : Float) return Float renames CMI.Cot;

   function Arcsin  (X           : Float) return Float renames CMI.Arcsin;
   function Arcsin  (X, Cycle    : Float) return Float renames CMI.Arcsin;
   function Arccos  (X           : Float) return Float renames CMI.Arccos;
   function Arccos  (X, Cycle    : Float) return Float renames CMI.Arccos;

   function Arctan
     (Y   : Float;
      X   : Float := 1.0) return Float
     renames CMI.Arctan;

   function Arctan
     (Y     : Float;
      X     : Float := 1.0;
      Cycle : Float) return Float
     renames CMI.Arctan;

   function Arccot
     (X   : Float;
      Y   : Float := 1.0) return Float
     renames CMI.Arccot;

   function Arccot
     (X     : Float;
      Y     : Float := 1.0;
      Cycle : Float) return Float
     renames CMI.Arccot;

end Ada.Numerics.Elementary_Functions;
