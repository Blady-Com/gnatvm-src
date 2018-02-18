------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         I N T E R F A C E S . J A V A . L A N G . T H R E A D            --
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

--  This child package corresponds to the Java class java.lang.Thread.
--  Please consult package Interfaces.Java for an explanation of the
--  mapping between a Java Class and an Ada package.

with Interfaces.Java.Lang.Object;

package Interfaces.Java.Lang.Thread is
   pragma Preelaborate;

   type Typ is new Object.Typ with null record;

   type Ref       is access all Typ;
   type Ref_Class is access all Typ'Class;

   MIN_PRIORITY : int;
   MAX_PRIORITY : int;

   procedure Set_Priority (This   : not null access Typ; New_Priority : int);
   function  Get_Priority (This   : not null access Typ) return int;
   procedure Set_Daemon   (This   : not null access Typ; Status : boolean);
   procedure Start        (This   : not null access Typ);
   procedure Sleep        (Millis : long);
   procedure Yield;
   function  Current_Thread return Ref_Class;

private
   pragma Convention (Java, Typ);

   pragma Import (Java, MIN_PRIORITY, "MIN_PRIORITY");
   pragma Import (Java, MAX_PRIORITY, "MAX_PRIORITY");

   pragma Import (Java, Set_Priority,   "setPriority");
   pragma Import (Java, Get_Priority,   "getPriority");
   pragma Import (Java, Sleep,          "sleep");
   pragma Import (Java, Yield,          "yield");
   pragma Import (Java, Current_Thread, "currentThread");
   pragma Import (Java, Set_Daemon,     "setDaemon");
   pragma Import (Java, Start,          "start");

end Interfaces.Java.Lang.Thread;

pragma Import (Java, Interfaces.Java.Lang.Thread, "java.lang.Thread");
