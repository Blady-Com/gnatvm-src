------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               I N T E R F A C E S . C I L . T H R E A D                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1999-2008, AdaCore                     --
--                                                                          --
-- The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by --
-- AdaCore - http://www.adacore.com                                         --
--                                                                          --
------------------------------------------------------------------------------

--  This child package corresponds to the CIL class System.Threading.Thread.

with Interfaces.CIL.Object;
with Interfaces.CIL.Threading.Priority;

package Interfaces.CIL.Threading.Thread is
   pragma Preelaborate;

   type Typ is new Object.Typ with null record;

   type Ref       is access all Typ;
   type Ref_Class is access all Typ'Class;

   procedure Set_Priority
     (This : access Typ; New_Priority : Priority.ValueType);
   function  Get_Priority (This : access Typ) return Integer;
   procedure Set_Daemon   (This   : access Typ; Status : Boolean);
   procedure Start        (This   : access Typ);
   procedure Sleep        (Millis : Integer);
   procedure Yield        (Millis : Integer := 0);
   function  Current_Thread return Ref_Class;

private
   pragma Convention (CIL, Typ);

   pragma Import (CIL, Set_Priority,   "set_Priority");
   pragma Import (CIL, Get_Priority,   "get_Priority");
   pragma Import (CIL, Sleep,          "Sleep");
   pragma Import (CIL, Yield,          "Sleep");
   pragma Import (CIL, Current_Thread, "get_CurrentThread");
   pragma Import (CIL, Set_Daemon,     "set_IsBackground");
   pragma Import (CIL, Start,          "Start");

end Interfaces.CIL.Threading.Thread;

pragma Import
  (CIL, Interfaces.CIL.Threading.Thread, "[mscorlib]System.Threading.Thread");
