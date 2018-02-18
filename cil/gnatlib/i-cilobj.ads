------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               I N T E R F A C E S . C I L . O B J E C T                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1999-2009, AdaCore                     --
--                                                                          --
--                                                                          --
-- The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by --
-- AdaCore - http://www.adacore.com                                         --
--                                                                          --
------------------------------------------------------------------------------

--  This child package corresponds to the CIL class System.Object.

package Interfaces.CIL.Object is
   pragma Preelaborate;

   type Typ is tagged limited private;

   type Ref       is access all Typ;
   type Ref_Class is access all Typ'Class;

   procedure M_Enter  (This : Ref);
   procedure M_Exit  (This : Ref);
   function Wait   (This : Ref) return Boolean;
   function Wait   (This : Ref; Timeout : Integer) return Boolean;
   procedure Notify (This : Ref);

   function new_Object (This : Ref := null) return Ref;
   function Box (Val : Long_Float) return access Typ'Class;
   function Box (Val : Integer) return access Typ'Class;

private
   type Typ is tagged limited null record;
   pragma Convention (CIL, Typ);

   pragma Import (CIL, M_Enter, "mgnat.adalib.MonitorEx.Enter");
   pragma Import (CIL, Wait, "mgnat.adalib.MonitorEx.Wait");
   pragma Import (CIL, Notify, "mgnat.adalib.MonitorEx.Pulse");
   pragma Import (CIL, Box, "mgnat.adalib.GNAT_libc.box");
   pragma Import (CIL, M_Exit, "mgnat.adalib.MonitorEx.Exit");

   pragma Cil_Constructor (new_Object);

end Interfaces.CIL.Object;
pragma Import (CIL, Object, "[mscorlib]System.Object");
