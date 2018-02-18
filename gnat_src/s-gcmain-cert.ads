------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--       S Y S T E M . G E N E R I C _ C _ M A T H _ I N T E R F A C E      --
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

--  This is the Cert specific version of s-gcmain.ads.

--  The separate version is necessary, because this system does not
--  provide an implementation of tanh, among other hyperbolic functions.
--  The run time currently has no code to implement this function,
--  so the only short term option was to remove the hyperbolic functions.

--  This package is an implementation of the Ada elementary functions
--  using the C math library. The C library is assumed to be conforming
--  with ISO/IEC 9899:1999 (C99). In particular, all identities specified
--  in chapter F.9 must hold. Furthermore, the accuracy of the various
--  functions is assumed to be sufficient for the strict mode specified
--  in Annex G of the Ada standard.

--  For environments where this is not true, the generic Ada implementations
--  should be used. These only require the standard arithmetic operations.

--  Typically, the generic functions are imported from C as follows.
--  For the C type "float":
--    function Sin (X : Float) return Float;
--    pragma Import (C, Sin, "sinf");

--  or for the C type "double":
--    function Sin (X : Long_Float) return Long_Float;
--    pragma Import (C, Sin, "sin");

--  or for the C type "long double":
--    function Sin (X : Long_Long_Float) return Long_Long_Float
--    pragma Import (C, Sin, "sinl");

generic
   type Float_Type is digits <>;

   with function C_Sqrt  (X    : Float_Type) return Float_Type is <>;
   with function C_Log   (X    : Float_Type) return Float_Type is <>;
   with function C_Exp   (X    : Float_Type) return Float_Type is <>;
   with function C_Pow   (X, Y : Float_Type) return Float_Type is <>;

   with function C_Sin   (X    : Float_Type) return Float_Type is <>;
   with function C_Cos   (X    : Float_Type) return Float_Type is <>;
   with function C_Tan   (X    : Float_Type) return Float_Type is <>;

   with function C_Asin  (X    : Float_Type) return Float_Type is <>;
   with function C_Acos  (X    : Float_Type) return Float_Type is <>;
   with function C_Atan2 (Y, X : Float_Type) return Float_Type is <>;

package System.Generic_C_Math_Interface is
pragma Pure (Generic_C_Math_Interface);

   pragma Assert (Float_Type'Signed_Zeros);
   pragma Assert (Float_Type'Machine_Radix = 2);

   function Sqrt    (X           : Float_Type'Base) return Float_Type'Base;
   function Log     (X           : Float_Type'Base) return Float_Type'Base;
   function Log     (X, Base     : Float_Type'Base) return Float_Type'Base;
   function Exp     (X           : Float_Type'Base) return Float_Type'Base;
   function "**"    (Left, Right : Float_Type'Base) return Float_Type'Base;

   function Sin     (X           : Float_Type'Base) return Float_Type'Base;
   function Sin     (X, Cycle    : Float_Type'Base) return Float_Type'Base;
   function Cos     (X           : Float_Type'Base) return Float_Type'Base;
   function Cos     (X, Cycle    : Float_Type'Base) return Float_Type'Base;
   function Tan     (X           : Float_Type'Base) return Float_Type'Base;
   function Tan     (X, Cycle    : Float_Type'Base) return Float_Type'Base;
   function Cot     (X           : Float_Type'Base) return Float_Type'Base;
   function Cot     (X, Cycle    : Float_Type'Base) return Float_Type'Base;

   function Arcsin  (X           : Float_Type'Base) return Float_Type'Base;
   function Arcsin  (X, Cycle    : Float_Type'Base) return Float_Type'Base;
   function Arccos  (X           : Float_Type'Base) return Float_Type'Base;
   function Arccos  (X, Cycle    : Float_Type'Base) return Float_Type'Base;

   function Arctan
     (Y     : Float_Type'Base;
      X     : Float_Type'Base := 1.0) return Float_Type'Base;

   function Arctan
     (Y     : Float_Type'Base;
      X     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base) return Float_Type'Base;

   function Arccot
     (X     : Float_Type'Base;
      Y     : Float_Type'Base := 1.0) return Float_Type'Base;

   function Arccot
     (X     : Float_Type'Base;
      Y     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base) return Float_Type'Base;

end System.Generic_C_Math_Interface;
