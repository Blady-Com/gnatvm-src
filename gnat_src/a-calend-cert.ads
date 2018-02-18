------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A D A . C A L E N D A R                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  This version is for the Cert runtime. It assumes type
--  Duration is represented on 64-bits.

--  RQ 0: Provide the language-defined library package Ada.Calendar.

--  RQ 1: Provide the specification of the Ada.Calendar package.

package Ada.Calendar is

   type Time is private;
   --  RQ 1-1: Time type shall allow to represent any point in time between
   --  1'st January 1901 and 31'st December 2099 with the precision of at
   --  least System.Tick seconds.
   --
   --  Note: Time type is represented as a Modified Julian Day Number (MJDN).
   --  MJDN is a number of days elapsed since midnight of November 16th/17th,
   --  1858, so day 0 in the system of MJDN is the day 17th November 1858.

   --  Declarations representing limits of allowed local time values. Note that
   --  these do NOT constrain the possible stored values of time which may well
   --  permit a larger range of times (this is explicitly allowed in Ada 95).

   subtype Year_Number  is Integer range 1901 .. 2099;
   subtype Month_Number is Integer range 1 .. 12;
   subtype Day_Number   is Integer range 1 .. 31;

   subtype Day_Duration is Duration range 0.0 .. 86_400.0;

   function Clock return Time;
   --  RQ 2.9: This function shall return the current time in days which
   --  elapsed since 17'th November 1858.

   function Year    (Date : Time) return Year_Number;
   --  RQ 2.19: This function shall return year of the specified time.

   function Month   (Date : Time) return Month_Number;
   --  RQ 2.11: This function shall return month of the specified time.

   function Day     (Date : Time) return Day_Number;
   --  RQ 2.10: This function shall return day of the specified time.

   function Seconds (Date : Time) return Day_Duration;
   --  RQ 2.12: This function shall return seconds of the specified time.

   procedure Split
     (Date    : Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration);
   --  RQ 2.13: This function shall get a Gregorian date (year number, month
   --  number, day number and duration) corresponding to the specified value
   --  of the Time type.
   --  Note: Algorithm used to convert modified Julian day number to Gregorian
   --  date has been adopted from the one described by the Jean Meeus in
   --  "Astronomical Algorithms" p. 63-4.

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0)
     return    Time;
   --  RQ 2.14: This function shall convert the specified Greogorian date into
   --  a value of type Time.
   --  RQ 2.14.1: If the specified values do not represent a valid Gregorian
   --  calendar date, then raise the Time_Error.

   function "+" (Left : Time;     Right : Duration) return Time;
   --  RQ 2.1: This function shall add the specified duration to the given
   --  point in time.
   --  RQ 2.1-1: If the result is not representable in the type Time, then
   --  raise the Time_Error.

   function "+" (Left : Duration; Right : Time)     return Time;
   --  RQ 2.2: This function shall add the specified duration to the given
   --  point in time.
   --  RQ 2.2-1: If the result is not representable in the type Time, then
   --  raise the Time_Error.

   function "-" (Left : Time;     Right : Duration) return Time;
   --  RQ 2.3: This function shall subtract the specified duration from the
   --  given point in time.
   --  RQ 2.3-1: If the result is not representable in the type Time, then
   --  raise the Time_Error.

   function "-" (Left : Time;     Right : Time)     return Duration;
   --  RQ 2.4: This function shall determine the amount of time which elapsed
   --  since <Left> point in time till <Right>.
   --  RQ 2.4-1: If the result is not representable in the type Duration, then
   --  raise the Time_Error.

   function "<"  (Left, Right : Time) return Boolean;
   --  RQ 2.5: This function shall return True if the <Left> point in time is
   --  before the <Right> point in time, False otherwise.

   function "<=" (Left, Right : Time) return Boolean;
   --  RQ 2.6: This function shall return True if both times are equal or the
   --  <Left> point in time is before the <Right> point in time, False
   --  otherwise.

   function ">"  (Left, Right : Time) return Boolean;
   --  RQ 2.7: This function shall return True if the <Left> point in time is
   --  after the <Right> point in time, False otherwise.

   function ">=" (Left, Right : Time) return Boolean;
   --  RQ 2.8: This function shall return True if both times are equal or the
   --  <Left> point in time is after the <Right> point in time, False
   --  otherwise.

   Time_Error : exception;

private
   pragma Inline (Year);
   pragma Inline (Month);
   pragma Inline (Day);

   pragma Inline ("<");
   pragma Inline ("<=");
   pragma Inline (">");
   pragma Inline (">=");

   --  Time is represented as a Modified Julian Day number plus fraction of a
   --  day within the range of dates defined for Ada.Calendar.Time.  The small
   --  of the type preserves the precision of type Duration (64 bits).

   --  We introduce type Modified_Julian_Day so that expressions will be
   --  cleaner when we want the operations from Standard as opposed to those
   --  defined in Ada.Calendar on type Time.

   --  The Julian Day approach simplifies Time_Of and Split substantially, as
   --  well as being well-known algorithms.  The use of the Modified Julian Day
   --  number allows us to preserve the precision of type Duration.

   --  Time zones are not supported in this implementation

   --  Modified Julian Day.  The range is exactly that of the dates supported
   --  within the constraints on the components that make up a date.

   type Constrained_Modified_Julian_Day is delta Duration'Small / 86_400.0
      range 15_385.0 .. 88_069.0;
   for Constrained_Modified_Julian_Day'Small use Duration'Small / 86_400.0;

   subtype Modified_Julian_Day is Constrained_Modified_Julian_Day'Base;

   type Time is new Modified_Julian_Day;

end Ada.Calendar;
