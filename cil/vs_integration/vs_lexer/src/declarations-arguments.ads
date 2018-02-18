------------------------------------------------------------------------------
--                                                                          --
--                      GNAT VISUAL STUDIO INTEGRATION                      --
--                                                                          --
--                          DECLARATIONS.ARGUMENTS                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2007, AdaCore                        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by --
-- AdaCore - http://www.adacore.com                                         --
--                                                                          --
-- This work is based on A# Visual Studio integration by  Prof. Martin C.   --
-- Carlisle of the United States Air Force Academy.                         --
--                                                                          --
------------------------------------------------------------------------------
-----------------------------------------------------------------------
--  declarations-arguments.ads
--
--  Author: Robert A. French
--  E-mail: rfrench99@hotmail.com
--
--  Description:
--  Defines the "Arguments" subclass. This is basically used for
--  anything that could possibly approximate arguments, whether they be
--  named with formal parameters, simply separated by commas, whatever.
-----------------------------------------------------------------------
with Lexer;
-----------------------------------------------------------------------
--  DECLARATIONS.ARGUMENTS
--  This package defines the Argument subclass of Declaration. This
--  class is used anywhere there are arguments, such as procedure calls,
--  generic instantiations, etc. It is a child of Declarations in order
--  to have visibility into the private part of Declarations.
-----------------------------------------------------------------------
package Declarations.Arguments is

   type Argument is new Declaration with private;
   --  This is the basic Argument class, containing slightly more data
   --  than a simple abstract Declaration.

   procedure Initialize (Object : in out Argument);
   procedure Adjust (Object : in out Argument);
   procedure Finalize (Object : in out Argument);
   --  These handle the newly added fields.

   procedure Read
     (Item  :    out Argument;
      From  :        Lexer.StringPointer;
      First : in out Positive);
   --  This method reads the input string for an Argument and stores it
   --  in the given Argument object.

   procedure Write
     (Item   : in out Argument;
      Into   : in out Lexer.StringPointer;
      Index  : in out Positive;
      Indent : in     Natural := 0);
   --  This just writes a newline after the Argument if it has a comma
   --  afterward.

   procedure Suppress_Indent;
   --  This procedure suppresses indentation of the first line on the
   --  next call to Reformat; it is only effective on the next call.
   --  This is used to start the Argument list immediately following a
   --  left paren, rather than on its own line.

   procedure Continuation_List;
   --  call this if last was comment so that
   --  a single item list is handled properly (on its own line)

   function Ending_Comment return Boolean;
   --  did the argument list end with a comment?

   procedure Reformat
     (From   : in     Lexer.StringPointer;
      First  : in out Positive;
      Into   : in out Lexer.StringPointer;
      Index  : in out Positive;
      Indent : in     Natural := 0);
   --  This is the interface for reformatting lists of Argument
   --  Declarations. It will format a list of Arguments, including
   --  possible embedded comments, and write them (formatted) into the
   --  output string.

private

   type Argument is new Declaration with
      record
         Has_Comma,
         --  This signifies the termination of the current Argument.
         Has_Value : Boolean := False;
         Value_Str : Lexer.StringPointer;
         Value_End : Positive;
         --  The presence of a default value specification means that
         --  it was preceeded by an arrow operator. This is an optional
         --  field, and entries with it can be mixed in a list with
         --  entries without.
         Nested_Name  : Boolean := False;
         --  found an argument list before arrow
         Nested_Value : Boolean := False;
         --  found an argument list after arrow
      end record;

end Declarations.Arguments;
