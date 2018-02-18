------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--              A D A . T A S K _ I D E N T I F I C A T I O N               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 1992-2010, Free Software Foundation, Inc.        --
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

--  .NET/JVM version

with System.Task_Primitives.Operations;
--  used for Self

pragma Warnings (Off);
--  Allow withing of non-Preelaborated units in Ada 2005 mode where this
--  package will be categorized as Preelaborate. See AI-362 for details.
--  It is safe in the context of the run-time to violate the rules!

with System.Tasking.Stages;
--  used for Terminated
--           Abort_Tasks

with System.Tasking.Rendezvous;
--  used for Callable

pragma Warnings (On);

package body Ada.Task_Identification is

   function Convert_Ids (T : Task_Id) return System.Tasking.Task_Id;
   function Convert_Ids (T : System.Tasking.Task_Id) return Task_Id;
   pragma Inline (Convert_Ids);

   function Convert_Ids (T : Task_Id) return System.Tasking.Task_Id is
   begin
      return System.Tasking.Task_Id (T);
   end Convert_Ids;

   function Convert_Ids (T : System.Tasking.Task_Id) return Task_Id is
   begin
      return Task_Id (T);
   end Convert_Ids;

   ---------
   -- "=" --
   ---------

   function  "=" (Left, Right : Task_Id) return Boolean is
   begin
      return System.Tasking."=" (Convert_Ids (Left), Convert_Ids (Right));
   end "=";

   -----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_Id) is
   begin
      if T = Null_Task_Id then
         raise Program_Error;
      else
         System.Tasking.Stages.Abort_Tasks
           (System.Tasking.Task_List'(1 => Convert_Ids (T)));
      end if;
   end Abort_Task;

   ------------------
   -- Current_Task --
   ------------------

   function Current_Task return Task_Id is
   begin
      return Convert_Ids (System.Task_Primitives.Operations.Self);
   end Current_Task;

   -----------
   -- Image --
   -----------

   function Image (T : Task_Id) return String is
   begin
      if T = Null_Task_Id then
         return "";
      else
         return T.Common.Task_Image (1 .. T.Common.Task_Image_Len);
      end if;
   end Image;

   -----------------
   -- Is_Callable --
   -----------------

   function Is_Callable (T : Task_Id) return Boolean is
   begin
      if T = Null_Task_Id then
         raise Program_Error;
      else
         return System.Tasking.Rendezvous.Callable (Convert_Ids (T));
      end if;
   end Is_Callable;

   -------------------
   -- Is_Terminated --
   -------------------

   function Is_Terminated (T : Task_Id) return Boolean is
   begin
      if T = Null_Task_Id then
         raise Program_Error;
      else
         return System.Tasking.Stages.Terminated (Convert_Ids (T));
      end if;
   end Is_Terminated;

end Ada.Task_Identification;
