------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     Interfaces.CIL.Threading.Priority                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1999-2008, AdaCore                     --
--                                                                          --
-- The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by --
-- AdaCore - http://www.adacore.com                                         --
--                                                                          --
------------------------------------------------------------------------------

--  This child package corresponds to the CIL class System.Threading.Priority.

package Interfaces.CIL.Threading.Priority is
   pragma Preelaborate;

   type ValueType is (
      Lowest,
      BelowNormal,
      Normal,
      AboveNormal,
      Highest);
   pragma Convention (CIL, ValueType);

end Interfaces.CIL.Threading.Priority;

pragma Import
  (CIL, Interfaces.CIL.Threading.Priority,
   "[mscorlib]System.Threading.Priority");
