------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A D A . C A L E N D A R                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2013, AdaCore                     --
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

--  This is the rts-cert version of this package

with Interfaces.C; use Interfaces.C;
with Ada.Unchecked_Conversion;

package body Ada.Calendar is

   ------------------------
   -- Local Declarations --
   ------------------------

   Secs_Per_Day : constant := 86_400.0;
   --  Number of seconds in a day

   Radix_Time : Time;
   --  This is the zero point for time values. Time_Of returns 1 Jan 1970,
   --  0 UTC for this value. The time at which the board boots is given this
   --  conventional Time value. The BSP can adjust the initial real time
   --  relative to this value.

   --  Julian day range covers Ada.Time range requirement with smallest
   --  possible delta within 64 bits. This type is used for calculations that
   --  do not need to preserve the precision of type Duration and that need a
   --  larger Julian Day range than the Modified Julian Day provides, because
   --  of the algorithms used

   type Julian_Day is
     delta Duration'Small / (86_400.0 / 2.0 ** 5)
     range 2415_385.5 .. 2488_069.5;
   for Julian_Day'Small use Duration'Small / (86_400.0 / 2.0 ** 5);

   function Trunc (Arg : Time) return Integer;
   --  Truncate Time

   function Trunc (Arg : Duration) return Integer;
   --  Truncate Duration

   function Trunc (Arg : Julian_Day'Base) return Integer;
   --  Truncate Julian Day

   function Valid_Date
     (D : Day_Number;
      M : Month_Number;
      Y : Year_Number) return Boolean;
   --  Check for valid Gregorian calendar date

   ---------
   -- "+" --
   ---------

   function "+" (Left : Time; Right : Duration) return Time is
      pragma Unsuppress (Overflow_Check, Time);
      Result : Time;
   begin
      Result := Left + Time (Right / Secs_Per_Day);
      return Result;
   exception
      when Constraint_Error =>
         raise Time_Error;
   end "+";

   function "+" (Left : Duration; Right : Time) return Time is
      pragma Unsuppress (Overflow_Check, Time);
      Result : Time;
   begin
      Result := Time (Left / Secs_Per_Day) + Right;
      return Result;
   exception
      when Constraint_Error =>
         raise Time_Error;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left : Time; Right : Duration)  return Time is
      pragma Unsuppress (Overflow_Check, Time);
      Result : Time;
   begin
      Result  := Left - Time (Right / Secs_Per_Day);
      return Result;
   exception
      when Constraint_Error =>
         raise Time_Error;
   end "-";

   function "-" (Left : Time; Right : Time) return Duration is
      pragma Unsuppress (Overflow_Check, Time);
      Temp   : Time;
      Result : Duration;
   begin
      Temp  := Left - Right;
      Result := Duration (Secs_Per_Day * Temp);
      return Result;
   exception
      when Constraint_Error =>
         raise Time_Error;
   end "-";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Time) return Boolean is
      use type Modified_Julian_Day;
   begin
      return Modified_Julian_Day (Left) < Modified_Julian_Day (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Time) return Boolean is
      use type Modified_Julian_Day;
   begin
      return Modified_Julian_Day (Left) <= Modified_Julian_Day (Right);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Time) return Boolean is
      use type Modified_Julian_Day;
   begin
      return  Modified_Julian_Day (Left) > Modified_Julian_Day (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Time) return Boolean is
      use type Modified_Julian_Day;
   begin
      return Modified_Julian_Day (Left) >= Modified_Julian_Day (Right);
   end ">=";

   -----------
   -- Clock --
   -----------

   function Clock return Time is separate;

   ---------
   -- Day --
   ---------

   function Day (Date : Time) return Day_Number is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;
   begin
      Split (Date, DY, DM, DD, DS);
      return DD;
   end Day;

   -----------
   -- Month --
   -----------

   function Month (Date : Time) return Month_Number is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;
   begin
      Split (Date, DY, DM, DD, DS);
      return DM;
   end Month;

   -------------
   -- Seconds --
   -------------

   function Seconds (Date : Time) return Day_Duration is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;
   begin
      Split (Date, DY, DM, DD, DS);
      return DS;
   end Seconds;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date    : Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration)
   is
      --  Algorithm is the standard astronomical one for conversion
      --  from a Julian Day number to the Gregorian calendar date.
      --  Adapted from J. Meeus' "Astronomical Algorithms" pp 63 - 4.

      --  No need to check for Trunc (Date) < 2299_161 (Ada "Time" is
      --  always a Gregorian date).

      --  Split off fractional part before losing precision:

      F : constant Time := Date - Time (Trunc (Date));

      --  Do remainder of calcs on full Julian day number with less precision
      --  in fractional part (not necessary for these calcs)

      JD_Date : constant Julian_Day :=
        Julian_Day'Base (Date) +
        Julian_Day'Base'(240_0000.5);

      Z  : constant Integer := Trunc (JD_Date + Julian_Day'Base (0.5));

      A, B, C, D, E : Integer;

      Alpha : constant Integer :=
        Trunc
          (Julian_Day'Base
               ((Julian_Day'Base (Z) -
                  Julian_Day'Base'(1867_216.25)) /
                  Julian_Day'Base'(36524.25)));

   begin
      --  Generate intermediate values

      A := Z + 1 + Alpha - Alpha / 4;
      B := A + 1524;
      C := Trunc (Julian_Day'Base
                   ((Julian_Day'Base (B) - Julian_Day'Base'(122.1))
                     / Julian_Day'Base'(365.25)));
      D := Trunc (Julian_Day'Base
                   (Julian_Day'Base'(365.25) * Julian_Day'Base (C)));
      E := Trunc (Duration (Duration (B - D) / 30.6001));

      --  Generate results from intermediate values

      Month := E - (if E < 14 then 1 else 13);
      Year  := C - (if Month > 2 then 4716 else 4715);
      Day   := B - D - Trunc (Duration (30.6001 * Duration (E)));

      --  Restore seconds from precise fractional part

      Seconds := Day_Duration (86_400.0 * F);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0) return Time
   is
      --  This is an adaptation of the standard astronomical algorithm for
      --  conversion from a Gregorian calendar date to a Julian day number.
      --  Taken from J. Meeus' "Astronomical Algorithms" pp 60 - 1.

      Month_Num   : Natural := Month;
      Year_Num    : Integer := Year;
      A, B        : Integer;

      type Day_Type is delta Modified_Julian_Day'Delta range 0.0 .. 32.0;
      for Day_Type'Small use Modified_Julian_Day'Small;

      Day_Val : constant Day_Type :=
        Day_Type (Day) + Day_Type (Seconds / Secs_Per_Day);

      subtype Month_Fixed is Duration range 4.0 .. 15.0;

   begin
      --  Check for valid date

      if not Valid_Date (Day, Month, Year) then
         raise Time_Error;
      end if;

      if Month_Num <= 2 then
         Year_Num := Year_Num - 1;
         Month_Num := Month_Num + 12;
      end if;

      A := Year_Num / 100;
      B := 2 - A + A / 4;

      return Time'Base (Day_Val)
        + Time
        (Julian_Day'Base
         (
          (B
           + Trunc (Duration (30.6001 * Month_Fixed (Month_Num + 1)))
           + Trunc (Julian_Day'Base (Julian_Day'Base'(365.25)
                                * Julian_Day'Base (Year_Num + 4716)))))
         - Julian_Day'Base'(1524.5) - Julian_Day'Base'(240_0000.5));
   end Time_Of;

   -----------
   -- Trunc --
   -----------

   function Trunc (Arg : Time) return Integer is
      Rounded : constant Integer := Integer (Arg);
      Sign    : constant Integer := Rounded / abs (Rounded);
   begin
      if abs (Time (Rounded)) > abs (Arg) then
         return Rounded - Sign;
      else
         return Rounded;
      end if;
   end Trunc;

   function Trunc (Arg : Duration) return Integer is
      Rounded : constant Integer := Integer (Arg);
   begin
      if Rounded = 0 or else abs (Duration (Rounded)) <= abs (Arg) then
         return Rounded;
      else
         return Rounded - Rounded / abs (Rounded);
      end if;
   end Trunc;

   function Trunc (Arg : Julian_Day'Base) return Integer is
      Rounded : constant Integer := Integer (Arg);
   begin
      if Rounded = 0 or else abs (Julian_Day'Base (Rounded)) <= abs (Arg) then
         return Rounded;
      else
         return Rounded - Rounded / abs (Rounded);
      end if;
   end Trunc;

   ----------------
   -- Valid_Date --
   ----------------

   function Valid_Date
     (D : Day_Number;
      M : Month_Number;
      Y : Year_Number) return Boolean
   is
   begin
      --  Check that input values have valid representations. This check is not
      --  strictly required, since the only way we could fail this test is if
      --  the Time_Of caller suppressed checks, in which case we have erroneous
      --  execution. However, raising Time_Error in this case seems a friendly
      --  way to handle the erroneous action.

      if not (Y'Valid and then M'Valid and then D'Valid) then
         return False;
      end if;

      --  Deal with checking day according to month

      case M is
         when    4  |  6  |  9  | 11  =>

            --  Apr | Jun | Sep | Nov

            return D <= 30;

            --  Note: lower bound OK due to 'Valid. Lower bound check would be
            --  optimized away anyway, and resulted in a compilation warning.

         when    2  =>

            --  Feb

            --  Do leap year check. Note that we do not need to check centuries
            --  due to the limited range of Year_Number.

            if Y mod 4 = 0 then
               return D <= 29;

               --  Note: lower bound OK due to 'Valid
            else
               return D <= 28;

               --  Note: lower bound OK due to 'Valid
            end if;

         when    1  |  3  |  5  |  7  |  8  | 10  | 12  =>

            --  Jan | Mar | May | Jul | Aug | Oct | Dec

            return True;
      end case;
   end Valid_Date;

   ----------
   -- Year --
   ----------

   function Year (Date : Time) return Year_Number is
      DY : Year_Number;
      DM : Month_Number;
      DD : Day_Number;
      DS : Day_Duration;
   begin
      Split (Date, DY, DM, DD, DS);
      return DY;
   end Year;

--  Start of elaboration code for Ada.Calendar

begin
   Radix_Time := Time_Of (1970, 1, 1, 0.0);

end Ada.Calendar;
