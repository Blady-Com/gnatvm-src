------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M _ W A L K                              --
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

--  This package contains:
--
--    o A generic routine, Process_Class, providing a generic traversal
--      of a Class_File record and all the data structures it contains
--
--    o A generic routine, Process_Instruction, providing a generic traversal
--      of an Instruction record.
--
--  Both these routines can be used to read/write the corresponding data
--  structure(s) from/to a stream of bytes (typically JVM bytecode).

with J_Types;  use J_Types;
with JVM_File; use JVM_File;

package JVM_Walk is

   generic
      with procedure Process (D : in out CP_Tag)         is <>;
      with procedure Process (D : in out Attribute_Kind) is <>;
      with procedure Process (D : in out U1)             is <>;
      with procedure Process (D : in out U2)             is <>;
      with procedure Process (D : in out U4)             is <>;
      --  Each of these routines should specify how to handle a field of the
      --  corresponding discrete type. Package JVM_File defines several
      --  discrete types in addition to types U1, U2 and U4. These types are
      --  processed using the Process routine of the underlying U? base type.

   procedure Process_Class (R : in out Class_File; Check : Boolean := False);
   --  Given an object R of type Class_File, this procedure provides a generic
   --  way of visiting each field in R. R is "in out" so that it can be used
   --  for read, write and read-write processing. To instantiate Process_Class
   --  one must provide, as parameters to the generic, the routines to be used
   --  when visiting each of R's scalar field.  When processing a table T, if T
   --  is unallocated then it is allocated using the value of the length field
   --  which is processed immediately before the Table. T's elements are
   --  visited by processing each element in turn.
   --  When parameter Check is set to True, this procedure checks that the
   --  resulting class file is well formed and consistent. If a check fails a
   --  message is output to standard out and execution is halted.

   generic
      with procedure Process (D : Operation) is <>;
      with procedure Process (D : in out U1)        is <>;
      with procedure Process (D : in out U2)        is <>;
      with procedure Process (D : in out U4)        is <>;
      --  Same meaning as the corresponding generic formal parameters in
      --  Process_Class above.

   procedure Process_Instruction (R : in out Instruction);
   --  Generic traversal for an Instruction. R's discriminants must be properly
   --  set when calling Process_Instruction and the value of R's field must be
   --  in range. Note that the wide instruction modifiers of instructions Iinc,
   --  [ILFDA]load/store or Ret must be processed within routine "Process (D :
   --  Operation)", that is while processing R's opcode .

end JVM_Walk;
