------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 B o d y                                  --
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

with Unchecked_Conversion;

package body System.Secondary_Stack is

   use type SSE.Storage_Offset;

   type Memory is array (Mark_Address range <>) of SSE.Storage_Element;

   type Stack_Id is record
      Top  : Mark_Address;
      Last : Mark_Address;
      Mem  : Memory (1 .. Mark_Address'Last);
   end record;
   pragma Suppress_Initialization (Stack_Id);

   type Stack_Ptr is access Stack_Id;

   function From_Addr is new Unchecked_Conversion (Address, Stack_Ptr);

   function Get_Sec_Stack return Stack_Ptr;

   -----------------
   -- SS_Allocate --
   -----------------

   procedure SS_Allocate
     (Address      : out System.Address;
      Storage_Size : SSE.Storage_Count)
   is
      Max_Align    : constant Mark_Address :=
                       Mark_Address (Standard'Maximum_Alignment);
      Max_Size     : constant Mark_Address :=
                       ((Mark_Address (Storage_Size) + Max_Align - 1)
                          / Max_Align) * Max_Align;
      Sec_Stack    : constant Stack_Ptr := Get_Sec_Stack;

   begin
      if Sec_Stack.Top + Max_Size > Sec_Stack.Last then
         raise Storage_Error;
      end if;

      Address := Sec_Stack.Mem (Sec_Stack.Top)'Address;
      Sec_Stack.Top := Sec_Stack.Top + Max_Size;
   end SS_Allocate;

   -------------
   -- SS_Init --
   -------------

   procedure SS_Init
     (Stk  : System.Address;
      Size : Natural := Default_Secondary_Stack_Size)
   is
      Stack : constant Stack_Ptr := From_Addr (Stk);
   begin
      pragma Assert (Size >= 2 * Mark_Address'Max_Size_In_Storage_Elements);
      Stack.Top := Stack.Mem'First;
      Stack.Last :=
        Mark_Address (Size) - 2 * Mark_Address'Max_Size_In_Storage_Elements;
   end SS_Init;

   -------------
   -- SS_Mark --
   -------------

   function SS_Mark return Mark_Id is
   begin
      return (Mark_Addr => Get_Sec_Stack.Top, Unused => Null_Address);
   end SS_Mark;

   ----------------
   -- SS_Release --
   ----------------

   procedure SS_Release (M : Mark_Id) is
   begin
      Get_Sec_Stack.Top := M.Mark_Addr;
   end SS_Release;

   -------------------------
   -- Package Elaboration --
   -------------------------

   --  Allocate a secondary stack for the main program to use

   --  We make sure that the stack has maximum alignment. Some systems require
   --  this (e.g. Sun), and in any case it is a good idea for efficiency.

   Stack : aliased Memory (1 .. Mark_Address (Default_Secondary_Stack_Size));
   for Stack'Alignment use Standard'Maximum_Alignment;

   Stack_Address : constant Address := Stack'Address;

   function Get_Sec_Stack return Stack_Ptr is
   begin
      return From_Addr (Stack_Address);
   end Get_Sec_Stack;

begin
   SS_Init (Stack_Address, Default_Secondary_Stack_Size);
end System.Secondary_Stack;
