------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                ADA.NUMERICS.GENERIC_ELEMENTARY_FUNCTIONS                 --
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

--  This is the Cert specific version of a-ngelfu.ads

--  Although this version of Ada.Numerics.Generic_Elementary_Functions is very
--  similar to the package defined by the Ada 95 standard, it is not intended
--  to fully implement the standard. Rather, the intent is to provide the
--  functionality of the environment in a convenient way. In particular, this
--  package does not implement Annex G, Numerics, and thus does not implement
--  strict mode.

--  One particular implication of the above is that, in contrast to libraries
--  adhering to the Ada 95 standard:

--     *  Sqrt (1.0 - Cos(X) * Cos(x)) might raise an exception,
--         because Cos(x) might be slightly above 1.0

--     *  Sin (X) might be slightly larger than X

--     *  Sqrt (X) might be negative for very small X

--  This is not to say that these situations will occur. Just that there is no
--  requirement that explicitly states it will not occur.

--  Furthermore, hyperbolic functions are not provided, as they are not
--  implemented in the environment.

generic
   type Float_Type is digits <>;

package Ada.Numerics.Generic_Elementary_Functions is
pragma Pure (Generic_Elementary_Functions);

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
     (Y   : Float_Type'Base;
      X   : Float_Type'Base := 1.0)
     return Float_Type'Base;

   function Arctan
     (Y     : Float_Type'Base;
      X     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
      return  Float_Type'Base;

   function Arccot
     (X   : Float_Type'Base;
      Y   : Float_Type'Base := 1.0)
     return Float_Type'Base;

   function Arccot
     (X     : Float_Type'Base;
      Y     : Float_Type'Base := 1.0;
      Cycle : Float_Type'Base)
     return   Float_Type'Base;

private
   pragma Assert (Float_Type'Machine_Radix = 2,
      "only binary floating-point types supported");

   pragma Inline ("**");
   pragma Inline (Arccos);
   pragma Inline (Arccot);
   pragma Inline (Arcsin);
   pragma Inline (Arctan);
   pragma Inline (Cos);
   pragma Inline (Cot);
   pragma Inline (Exp);
   pragma Inline (Log);
   pragma Inline (Sin);
   pragma Inline (Sqrt);
   pragma Inline (Tan);

end Ada.Numerics.Generic_Elementary_Functions;
