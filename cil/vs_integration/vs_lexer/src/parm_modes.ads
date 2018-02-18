-----------------------------------------------------------------------
-- parm_modes.ads
--
-- Author: Robert A. French
-- E-mail: rfrench99@hotmail.com
--
-- Description:
-- This provides a type and procedure for reading possible parameter
-- modes from a parameter declaration and returning the given modes.
-- This aids in vertical alignment of parameter modes.
-----------------------------------------------------------------------
with Lexer;
-----------------------------------------------------------------------
-- PARM_MODES
-- This package provides for reading possible parameter modes from the
-- input string.
-----------------------------------------------------------------------
package Parm_Modes is

   type Mode is (No_Mode, In_Mode, Out_Mode, In_Out_Mode, Access_Mode);
   --| This type indicates the mode of a formal parameter.

   procedure Get (
         Value :    out Mode;
         From  : in     Lexer.StringPointer;
         First : in out Positive
       );
   --| This procedure gets a possible parameter mode (in, out, etc)
   --| from the input string. If none are found, the input string is
   --| not advanced.

end Parm_Modes;
-----------------------------------------------------------------------
