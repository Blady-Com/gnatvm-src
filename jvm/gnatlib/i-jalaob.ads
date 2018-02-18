------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         I N T E R F A C E S . J A V A . L A N G . O B J E C T            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1999-2010, AdaCore                     --
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
-- The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by --
-- AdaCore - http://www.adacore.com                                         --
--                                                                          --
------------------------------------------------------------------------------

--  This child package corresponds to the Java class java.lang.Object.
--  Please consult package Interfaces.Java for an explanation of the
--  mapping between a Java Class and an Ada package.

package Interfaces.Java.Lang.Object is
   pragma Preelaborate;

   type Typ (<>) is tagged limited private;

   type Ref       is access all Typ;
   type Ref_Class is access all Typ'Class;

   procedure Wait   (This : not null access Typ);
   procedure Wait   (This : not null access Typ; Timeout : long; Nanos : int);
   procedure Notify (This : not null access Typ);

   function new_Object (This : Ref := null) return Ref;

   package J_Object renames Interfaces.Java.Lang.Object;

private
   type Typ is tagged limited null record;
   pragma Convention (Java, Typ);

   pragma Import (Java, Wait,   "wait");
   pragma Import (Java, Notify, "notify");

   pragma Java_Constructor (new_Object);

end Interfaces.Java.Lang.Object;

pragma Import (Java, Interfaces.Java.Lang.Object, "java.lang.Object");
