------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--     A D A . N U M E R I C S . E L E M E N T A R Y _ F U N C T I O N S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

--  This is the Cert specific version of a-nuelfu.adb. (rts-cert,
--  rts-ravenscar-cert, rts-ravenscar-cert-rtp)

--  The separate version is necessary, because this system does not
--  provide an implementation of tanh, among other hyperbolic functions.
--  The run time currently has no code to implement this function,
--  so the only short term option was to remove the hyperbolic functions.

package body Ada.Numerics.Elementary_Functions is

   package LF is
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

      pragma Import (C, C_Asin, "asin");
      pragma Import (C, C_Acos, "acos");
      pragma Import (C, C_Atan2, "atan2");
   end LF;

   ------------
   -- C_Acos --
   ------------

   function C_Acos (X : Float) return Float is
   begin
      return Float (LF.C_Acos (Long_Float (X)));
   end C_Acos;

   -------------
   -- C_Atan2 --
   -------------

   function C_Atan2 (Y, X : Float) return Float is
   begin
      return Float (LF.C_Atan2 (Long_Float (Y), Long_Float (X)));
   end C_Atan2;

   ------------
   -- C_Asin --
   ------------

   function C_Asin (X : Float) return Float is
   begin
      return Float (LF.C_Asin (Long_Float (X)));
   end C_Asin;

   -----------
   -- C_Cos --
   -----------

   function C_Cos (X : Float) return Float is
   begin
      return Float (LF.C_Cos (Long_Float (X)));
   end C_Cos;

   -----------
   -- C_Exp --
   -----------

   function C_Exp   (X    : Float) return Float is
   begin
      return Float (LF.C_Exp (Long_Float (X)));
   end C_Exp;

   -----------
   -- C_Log --
   -----------

   function C_Log   (X    : Float) return Float is
   begin
      return Float (LF.C_Log (Long_Float (X)));
   end C_Log;

   -----------
   -- C_Pow --
   -----------

   function C_Pow   (X, Y : Float) return Float is
   begin
      return Float (LF.C_Pow (Long_Float (X), Long_Float (Y)));
   end C_Pow;

   -----------
   -- C_Sin --
   -----------

   function C_Sin (X : Float) return Float is
   begin
      return Float (LF.C_Sin (Long_Float (X)));
   end C_Sin;

   ------------
   -- C_Sqrt --
   ------------

   function C_Sqrt (X : Float) return Float is
   begin
      return Float (LF.C_Sqrt (Long_Float (X)));
   end C_Sqrt;

   -----------
   -- C_Tan --
   -----------

   function C_Tan (X : Float) return Float is
   begin
      return Float (LF.C_Tan (Long_Float (X)));
   end C_Tan;

end Ada.Numerics.Elementary_Functions;
