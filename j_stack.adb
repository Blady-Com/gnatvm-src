------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J _ S T A C K                               --
--                                                                          --
--                                 B o d y                                  --
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

package body J_Stack is
   Stack   : array (Natural range 1 .. Max_Depth) of Element_Type;
   Stk_Top : Natural := 0;

   -------------
   -- Element --
   -------------

   function Element (Stack_Position : Natural) return Element_Type is
   begin
      pragma Assert (Stack_Position <= Stk_Top);
      return Stack (Stack_Position);
   end Element;

   -----------
   -- Empty --
   -----------

   function Empty return Boolean is
   begin
      return Stk_Top = 0;
   end Empty;

   ------------------
   -- Num_Elements --
   ------------------

   function Num_Elements return Natural is
   begin
      return Stk_Top;
   end Num_Elements;

   ---------
   -- Pop --
   ---------

   procedure Pop is
   begin
      pragma Assert (Stk_Top >= Stack'First);
      Stk_Top := Stk_Top - 1;
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop return Element_Type is
   begin
      pragma Assert (Stk_Top >= Stack'First);
      Stk_Top := Stk_Top - 1;
      return Stack (Stk_Top + 1);
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (Elmt : Element_Type) is
   begin
      pragma Assert (Stk_Top < Stack'Last);
      Stk_Top := Stk_Top + 1;
      Stack (Stk_Top) := Elmt;
   end Push;

   ---------
   -- Top --
   ---------

   function Top return Element_Type is
   begin
      pragma Assert (Stk_Top >= Stack'First);
      return Stack (Stk_Top);
   end Top;

end J_Stack;
