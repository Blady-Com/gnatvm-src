------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                             C I L _ T y p e s                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                   Copyright (C) 2006-2010, AdaCore                       --
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
-- This work is partially  based on A#, an Ada  compiler for .NET by  Prof. --
-- Martin C. Carlisle of the United States Air Force Academy.               --
------------------------------------------------------------------------------

with System;

package CIL_Types is
   pragma Preelaborate;

   subtype float32 is Float;
   type float32_arr is array (Natural range <>) of Float;
   type float32_array is access all float32_arr;
   type float32_array_array is access all float32_array;
   pragma Convention (CIL, float32_arr);
   pragma Convention (CIL, float32_array);
   pragma Convention (CIL, float32_array_array);

   subtype float64 is Long_Float;
   type float64_arr is array (Natural range <>) of Long_Float;
   type float64_array is access all float64_arr;
   type float64_array_array is access all float64_array;
   pragma Convention (CIL, float64_arr);
   pragma Convention (CIL, float64_array);
   pragma Convention (CIL, float64_array_array);

   subtype bool is Boolean;
   type bool_arr is array (Natural range <>) of bool;
   type bool_array is access all bool_arr;
   pragma Convention (CIL, bool_arr);
   pragma Convention (CIL, bool_array);

   type int8 is new Short_Short_Integer;
   type int8_arr is array (Natural range <>) of int8;
   type int8_array is access all int8_arr;
   type int8_array_array is access all int8_array;
   type int8_array_addrof is access all int8_array;
   pragma Convention (CIL, int8_arr);
   pragma Convention (CIL, int8_array);
   pragma Convention (CIL, int8_array_array);

   type unsigned_int8 is mod 2**8;
   type unsigned_int8_arr is array (Natural range <>) of unsigned_int8;
   type unsigned_int8_array is access all unsigned_int8_arr;
   type unsigned_int8_array_array is access all unsigned_int8_array;
   type unsigned_int8_array_addrof is access all unsigned_int8_array;
   pragma Convention (CIL, unsigned_int8_arr);
   pragma Convention (CIL, unsigned_int8_array);
   pragma Convention (CIL, unsigned_int8_array_array);

   subtype char is Wide_Character;
   type char_arr is array (Natural range <>) of char;
   type char_array is access all char_arr;
   pragma Convention (CIL, char_arr);
   pragma Convention (CIL, char_array);

   type int16 is new Short_Integer;
   type int16_arr is array (Natural range <>) of int16;
   type int16_array is access all int16_arr;
   type int16_array_array is access all int16_array;
   pragma Convention (CIL, int16_arr);
   pragma Convention (CIL, int16_array);
   pragma Convention (CIL, int16_array_array);

   type unsigned_int16 is mod 2**16;
   type unsigned_int16_arr is array (Natural range <>) of unsigned_int16;
   type unsigned_int16_array is access all unsigned_int16_arr;
   type unsigned_int16_array_array is access all unsigned_int16_array;
   pragma Convention (CIL, unsigned_int16_arr);
   pragma Convention (CIL, unsigned_int16_array);
   pragma Convention (CIL, unsigned_int16_array_array);

   subtype int32 is Integer;
   type int32_arr is array (Natural range <>) of Integer;
   type int32_array is access all int32_arr;
   type int32_array_array is access all int32_array;
   pragma Convention (CIL, int32_arr);
   pragma Convention (CIL, int32_array);
   pragma Convention (CIL, int32_array_array);

   type unsigned_integer is mod 2**32;
   type unsigned_integer_arr is array (Natural range <>) of unsigned_integer;
   type unsigned_integer_array is access all unsigned_integer_arr;
   type unsigned_integer_array_array is access all unsigned_integer_array;
   pragma Convention (CIL, unsigned_integer_arr);
   pragma Convention (CIL, unsigned_integer_array);
   pragma Convention (CIL, unsigned_integer_array_array);

   type unsigned_long_long_integer is mod 2**64;
   type unsigned_long_long_integer_arr is
     array (Natural range <>) of unsigned_long_long_integer;
   type unsigned_long_long_integer_array is
     access all unsigned_long_long_integer_arr;
   type unsigned_long_long_integer_array_array is
     access all unsigned_long_long_integer_array;
   pragma Convention (CIL, unsigned_long_long_integer_arr);
   pragma Convention (CIL, unsigned_long_long_integer_array);
   pragma Convention (CIL, unsigned_long_long_integer_array_array);

   type int64_arr is array (Natural range <>) of Long_Long_Integer;
   type int64_array is access all int64_arr;
   type int64_array_array is access all int64_array;
   pragma Convention (CIL, int64_arr);
   pragma Convention (CIL, int64_array);
   pragma Convention (CIL, int64_array_array);

   type void is new int32;
   type void_addrof is access all void;

   --  the following names are fixed
   --  because the compiler depends on them
   subtype native_int is System.Address;

   type unsigned_int8_addrof is mod 2**32; --  access all unsigned_int8;
   type unsigned_int16_addrof is mod 2**32; --  access all unsigned_int16;
   type unsigned_integer_addrof is mod 2**32; --  access all unsigned_integer;
   type unsigned_long_long_integer_addrof is mod 2 ** 32;
   --  access all unsigned_long_long_integer;

   type int8_addrof is mod 2**32; --  access all int8;
   type int16_addrof is mod 2**32; --  access all int16;
   type int32_addrof is mod 2**32; --  access all int32;
   type int64_addrof is mod 2**32; --  access all long_long_integer;
   type float32_addrof is mod 2**32; --  access all float32;
   type float64_addrof is mod 2**32; --  access all float64;
   type bool_addrof is mod 2**32;  --  access all bool;
   type char_addrof is mod 2**32; --  access all char;
   --  end of fixed names

   type native_int_addrof is access all native_int;
   type native_int_Arr is array (Natural range <>) of native_int;
   type Typedref is new int32;

end CIL_Types;
