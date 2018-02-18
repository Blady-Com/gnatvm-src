------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             S Y S T E M . T R A C E B A C K _ C O N T R O L              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--             Copyright (C) 2012, Free Software Foundation, Inc.           --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This is the spec of the System.Traceback_Control helper for the PPC/cert
--  version of System.Traceback. See comments in the System.Traceback body for
--  explanations on what we are providing here.

package System.Traceback_Control is

   function Return_Address_Offset return System.Address;
   --  Relative to a given frame pointer, the constant offset at which the
   --  return address for this frame is stored.

   --  Return_Address_Offset is declared as a function instead of as named
   --  constant to allow exposing a single spec for all the supported targets,
   --  as the constant value varies across ABIs. Request inlining to minimize
   --  the cost of this extra abstraction layer:

   pragma Inline (Return_Address_Offset);

   function Is_Topframe_Retaddr (Retaddr : System.Address) return Boolean;
   --  Whether the provided Retaddr signals a top-of-stack frame.

end System.Traceback_Control;
