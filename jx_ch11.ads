------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J X _ C H 1 1                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2013, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by --
-- AdaCore - http://www.adacore.com                                         --
--                                                                          --
------------------------------------------------------------------------------

with JVM;   use JVM;
with Types; use Types;
with Uintp; use Uintp;

package Jx_Ch11 is

   --  Processing for exception declarations, raises, and handlers

   procedure Translate_Exception_Declaration (Exc_Decl : Node_Id);
   --  Creates a class for an Ada exception

   procedure Translate_Raise_Statement (Raise_Stmt : Node_Id);
   --  Generates code for a raise statement

   procedure Translate_Predefined_Raise (Raise_Node : Node_Id);
   --  Generates code for a front-end generated raise node (Nkind in subtype
   --  N_Raise_xxx_Error).

   procedure Translate_Exception_Handlers
     (Handlers  : List_Id;
      Start_Lbl : Label_Id;
      End_Lbl   : Label_Id;
      Exit_Lbl  : Label_Id);
   --  Processes the handlers of an exception handler part, entering
   --  exception table entries for the handlers in the exception table
   --  of the current method and generating code for each of the handlers.

   procedure Gen_Load_Current_Exception;
   --  Loads the value of the currently active exception handler's
   --  current exception occurrence. This procedure must only be
   --  called when during translation of an exception handler.
   --  (This is called when translating a call to the intrinsic
   --  Current_Target_Exception.)

   procedure Generate_Exception_And_Throw
     (Exc_Class : Class_Id;
      Raise_Loc : Source_Ptr;
      Reason    : Uint;
      Gen_Call  : Boolean := False);
   --  Generates code to create a new exception object, invoke the
   --  exception's constructor with a string describing the raise
   --  statement's source, and throw the new exception. If Gen_Call
   --  is True, then pass the initialized exception object to
   --  GNAT_libc.Reraise_Occurrence_No_Defer rather than throwing
   --  it directly (works around odd behavior of the Sun JVM that
   --  leads it to associate an unconditionally raised exception
   --  with the wrong handlers).

   procedure Push_Exception_Label_Stack (Push_Node : Node_Id);
   --  Pushes a label on the appropriate exception label stack associated with
   --  the passed node, which must have a Node_Kind in N_Push_xxx_Label.

   procedure Pop_Exception_Label_Stack (Pop_Node : Node_Id);
   --  Pops the appropriate exception label stack associated with the passed
   --  node, which must have a Node_Kind in N_Pop_xxx_Label.

end Jx_Ch11;
