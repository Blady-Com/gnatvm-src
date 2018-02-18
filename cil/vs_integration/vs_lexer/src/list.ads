------------------------------------------------------------------------------
--                                                                          --
--                      GNAT VISUAL STUDIO INTEGRATION                      --
--                                                                          --
--                                 VS_LEXER                                 --
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
-------------------------------------------------------------
--
-- Generic list package
--
-- By: Dr. Martin C. Carlisle
--     US Air Force Academy
--
-------------------------------------------------------------

generic

   type Listitem is private; -- list of what?

package List is

   type Node is private;

   type Listptr is access Node;

   type Listitemproc is access procedure (Item : Listitem);

   --  Emulate LISP functions

   function Car (Ptr : Listptr) return Listitem;
   --  Car is first item in list

   function Cdr (Ptr : Listptr) return Listptr;
   --  Cdr is list containing 2nd element and following

   function Cons (Val : Listitem; Ptr : Listptr) return Listptr;
   --  Cons places val at the front of list and returns new list

   function Length (Ptr : Listptr) return Integer;
   --  Aength : how many items are in list?

   procedure Append (Val : Listitem; Ptr : in out Listptr);
   --  Append : add val to end of list, NOTE ptr (list) may be modified!

   function Cadr (Ptr : Listptr) return Listitem;
   --  Like second: cadr(ptr) = car(cdr(ptr))

   function Caddr (Ptr : Listptr) return Listitem;
   --  Like third: caddr(ptr) = car(cdr(cdr(ptr)))

   function Cadddr (Ptr : Listptr) return Listitem;
   --  Like fourth: cadddr(ptr) = car(cdr(cdr(cdr(ptr))))

   procedure Foreach (Ptr : Listptr; Fnptr : Listitemproc);
   --  Foreach does fnptr(x) for each x in list

   procedure Deallocate (Ptr : in out Listptr);
   --  Deallocate the list
   --  use Foreach to deallocate each item first

private

   type Node is record
      Nodecar : Listitem;
      Nodecdr : Listptr;
   end record;

end List;
