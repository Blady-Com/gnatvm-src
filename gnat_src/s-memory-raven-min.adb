------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . M E M O R Y                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--              Copyright (C) 2013, Free Software Foundation, Inc.          --
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

--  Simple implementation for use with Ravenscar Minimal. This implementation
--  is based on a simple static buffer (whose bounds are defined in the linker
--  script), and allocation is performed through a protected object to
--  protect against concurrency.

with System.Storage_Elements;
with Unchecked_Conversion;

package body System.Memory is

   use System.Storage_Elements;

   Heap_Start : Character;
   for Heap_Start'Alignment use Standard'Maximum_Alignment;
   pragma Import (C, Heap_Start, "__heap_start");
   --  The address of the variable is the start of the heap

   Heap_End : Character;
   pragma Import (C, Heap_End, "__heap_end");
   --  The address of the variable is the end of the heap

   protected Reserve is
      procedure Alloc (Size : Storage_Count; Res : out Address);
      --  Memory allocation is performed within a protected object to protect
      --  against concurrency

      pragma Priority (System.Priority'Last);
      --  Any task at non-interrupt level priority can allocate memory

   private
      Top : Address := Heap_Start'Address;
      --  First not used address
   end Reserve;

   -----------
   -- Alloc --
   -----------

   function Alloc (Size : size_t) return System.Address is
      Max_Align : constant := Standard'Maximum_Alignment;
      Max_Size  : Storage_Count;
      Res : Address;
   begin
      --  Detect too large allocation

      if Size = size_t'Last or else Size >= size_t (Storage_Count'Last) then
         raise Storage_Error;
      end if;

      --  Compute aligned size

      Max_Size :=
        ((Storage_Count (Size) + Max_Align - 1) / Max_Align) * Max_Align;

      --  Change size from zero to non-zero. We still want a proper pointer
      --  for the zero case because pointers to zero length objects have to
      --  be distinct, but we can't just go ahead and allocate zero bytes,
      --  since some malloc's return zero for a zero argument.

      if Max_Size = 0 then
         Max_Size := Max_Align;
      end if;

      --  Protect against concurrency

      Reserve.Alloc (Max_Size, Res);

      return Res;
   end Alloc;

   protected body Reserve is

      -----------
      -- Alloc --
      -----------

      procedure Alloc (Size : Storage_Count; Res : out Address) is
      begin
         --  Compute result

         Res := Top;

         --  Allocate memory

         Top := Top + Size;

         --  Detect out of memory

         if Top > Heap_End'Address then
            raise Storage_Error;
         end if;
      end Alloc;
   end Reserve;
end System.Memory;
