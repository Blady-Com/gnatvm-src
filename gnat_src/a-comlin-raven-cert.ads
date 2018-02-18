------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     A D A . C O M M A N D _ L I N E                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  RCI custom version

pragma Compiler_Unit;

package Ada.Command_Line is
   pragma Preelaborate;

   function Command_Name return String;
   --  If the external execution environment supports passing arguments to
   --  a program, then Command_Name returns an implementation-defined value
   --  corresponding to the name of the command invoking the program.
   --  Otherwise Command_Name returns the null string.
   --
   --  in GNAT: Corresponds to argv [0] in C.

   --  type Exit_Status is new Integer;

   --  Success : constant Exit_Status;
   --  Failure : constant Exit_Status;

   --  procedure Set_Exit_Status (Code : Exit_Status);

   ------------------------------------
   -- Note on Interface Requirements --
   ------------------------------------

   --  Services in this package are not supported during the elaboration of an
   --  auto-initialized Stand-Alone Library.

   --  If the main program is in Ada, this package works as specified without
   --  any other work than the normal steps of WITH'ing the package and then
   --  calling the desired routines.

   --  If the main program is not in Ada, then the information must be made
   --  available for this package to work correctly. In particular, it is
   --  required that the global variable "gnat_argc" contain the number of
   --  arguments, and that the global variable "gnat_argv" points to an
   --  array of null-terminated strings, the first entry being the command
   --  name, and the remaining entries being the command arguments.

   --  These correspond to the normal argc/argv variables passed to a C
   --  main program, and the following is an example of a complete C main
   --  program that stores the required information:

   --    main(int argc, char **argv, char **envp)
   --    {
   --       extern int    gnat_argc;
   --       extern char **gnat_argv;
   --       extern char **gnat_envp;
   --       gnat_argc = argc;
   --       gnat_argv = argv;
   --       gnat_envp = envp;

   --       adainit();
   --       adamain();
   --       adafinal();
   --    }

   --  The assignment statements ensure that the necessary information is
   --  available for finding the command name and command line arguments.

end Ada.Command_Line;
