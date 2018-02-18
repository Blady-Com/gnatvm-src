---------------------------------------------------------------
--
--  ADA GNAT INTEGRATED DEVELOPMENT ENVIRONMENT (AdaGIDE)
--
--  RTF.ADS
--  Description : provides constants describing the
--                Rich Text Format (RTF)
--
--  By: Dr. Martin C. Carlisle
--
-- RTF is free software; you can redistribute it and/or
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly
-- indicate if it has been modified.
--
-- RTF is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
--
---------------------------------------------------------------
--  3-Feb-2002 (GdM): Header as function, to match Options.Colors

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
package body RTF is
  Stored_Font_Size : Unbounded_String;
  ------------
  -- Header --
  ------------

  function Header return String is
  begin
    return "{\rtf1\ansi\deff0\deftab720" &
      Standard.ASCII.LF &
      "{\fonttbl{\f0\fnil FixedSys;}{\f1\fprq1 Courier New;}{\f2\fprq1 Lucida Console;}}" &
      Standard.ASCII.LF &
      Standard.ASCII.LF &
      "\pard";
  end Header;

  procedure Set_Font_Size(X : String) is
  begin
     Stored_Font_Size := To_Unbounded_String(X);
  end Set_Font_Size;

  function Font_Size return String is
  begin
     return To_String(Stored_Font_Size);
  end Font_Size;
begin
   Stored_Font_Size := To_Unbounded_String("\f0\fs20 ");
end RTF;

