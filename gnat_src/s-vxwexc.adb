------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--              S Y S T E M . V X W O R K S _ E X C E P T I O N S           --
--                                                                          --
--                                   B o d y                                --
--                                                                          --
--           Copyright (C) 2004-2012, Free Software Foundation, Inc.        --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

package body System.VxWorks_Exceptions is

   --  Note: We are moving to a scheme where we more closely rely on the
   --  VxWorks mechanisms to register exception tables. This change requests
   --  it by default (-auto-register) and removes the incomplete circuitry
   --  that was taking care of the registration as part of the Ada RTS
   --  initialization.

   --  As part of this, the following routine is made into a nop. Eventually
   --  this entire file will disappear.

   ------------------
   -- Setup_For_EH --
   ------------------

   procedure Setup_For_EH is
   begin
      null;
   end Setup_For_EH;

end System.VxWorks_Exceptions;
