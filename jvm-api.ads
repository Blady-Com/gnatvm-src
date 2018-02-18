------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J V M . A P I                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2013, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by --
-- AdaCore - http://www.adacore.com                                         --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides access to various predefined classes, fields,
--  and methods existing in the Java API, as well as access to certain
--  predefined Ada-specific classes.

package JVM.API is

   --  Names denoting the predefined Java and Ada API classes

   type API_Class_Name is
     (Lang_Object,               --  java.lang.Object
      Lang_Class,                --  java.lang.Class
      Lang_Math,                 --  java.lang.Math
      Lang_String,               --  java.lang.String
      Lang_System,               --  java.lang.System
      Lang_Throwable,            --  java.lang.Throwable
      Lang_Error,                --  java.lang.Error
      Lang_VirtualMachineError,  --  java.lang.VirtualMachineError
      Lang_OutOfMemoryError,     --  java.lang.OutOfMemoryError
      Lang_StackOverflowError,   --  java.lang.StackOverflowError
      Lang_Exception,            --  java.lang.Exception
      Lang_RuntimeException,     --  java.lang.RuntimeException
      Lang_Thread,               --  java.lang.Thread
      IndexOutOfBoundsException, --  java.lang.IndexOutOfBoundsException
      NullPointerException,      --  System.NullPointerException
      ArithmeticException,       --  System.ArithmeticException
      InvalidCastException,      --  System.InvalidCastException
      Ada_Abort_Signal,          --  Standard.Abort_Signal
      Ada_Constraint_Error,      --  Standard.Constraint_Error
      Ada_Program_Error,         --  Standard.Program_Error
      Ada_Storage_Error,         --  Standard.Storage_Error
      Ada_Tasking_Error,         --  Standard.Tasking_Error
      Ada_String,                --  Descriptor class of String
      Ada_Wide_String,           --  Descriptor class of Wide_String
      Ada_Wide_Wide_String,      --  Descriptor class of Wide_Wide_String
      Ada_Int,                   --  Descriptor class of int type
      Ada_Lng,                   --  Descriptor class of long type
      Ada_UInt,                  --  Descriptor class of uns int type
      Ada_ULng,                  --  Descriptor class of uns long type
      Ada_Flt,                   --  Descriptor class of float type
      Ada_Dbl,                   --  Descriptor class of double type
      Ada_Acc,                   --  Descriptor class of access type
      Ada_Activation_Rec,        --  up-level addressing class
      GNAT_libc,                 --  class for imported run-time routines
      Ada_Exceptions,            --  regular Ada.Exceptions class
      System_Array);

   --  Names denoting the predefined Java API interfaces

   type API_Interface_Name is
      (IO_Serializable);          --  java.io.Serializable

   --  Names denoting the predefined Java and Ada API methods

   type API_Method_Name is
     (Object_getClass,     --  java.lang.Object.getClass
      Class_forName,       --  java.lang.Class.forName
      Math_round_double,   --  java.lang.Math.Round
      Math_round_float,    --  java.lang.Math.Round
      String_Ascii_Init,   --  java.lang.String.Ascii_Init
      String_getBytes,     --  GNAT_Libc.getBytes
      System_arraycopy,    --  java.lang.System.arraycopy
      Thread_sleep,        --  java.lang.Thread.sleep
      Reraise_No_Defer,    --  GNAT_Libc.reraise_occurrence_no_defer
      To_JVM_String);

   --  Names denoting the predefined Java and Ada API fields

   type API_Field_Name is
     (Str_All,             --  standard$string.all
      Str_First,           --  standard$string.first
      Str_Last,            --  standard$string.last
      Wide_Str_All,        --  standard$string.all
      Wide_Str_First,      --  standard$string.first
      Wide_Str_Last,       --  standard$string.last
      Wide_Wide_Str_All,   --  standard$string.all
      Wide_Wide_Str_First, --  standard$string.first
      Wide_Wide_Str_Last,  --  standard$string.last
      Int_All,             --  Int.all
      Lng_All,             --  Lng.all
      UInt_All,            --  UInt.all
      ULng_All,            --  ULng.all
      Flt_All,             --  Flt.all
      Dbl_All,             --  Dbl.all
      Acc_All,             --  Acc.all
      AR_Static_Link,      --  Activation record static link
      Gnat_Argv,           --  Reference to command-line arguments
      Command_Name);       --  Reference to command-line arguments

   function Ada_Lib_Package return String_Id;
   --  Returns a String_Id denoting the name of the Java package containing
   --  various predefined Ada library and support classes

   function API_Class (Name : API_Class_Name) return Class_Id;
   --  Returns the Class_Id associated with the given API class.

   function API_Interface (Name : API_Interface_Name) return Class_Id;
   --  Returns the Class_Id associated with the given API interface.

   function API_Method (Name : API_Method_Name) return Method_Id;
   --  Returns the Method_Id associated with the given API method.

   function API_Field (Name : API_Field_Name) return Field_Id;
   --  Returns the Field_Id associated with the given API Field.

   procedure Gen_Invoke_API_Method (Name : API_Method_Name);
   --  Emits a call to the given method.

   procedure Initialize;
   --  Creates JVM entity symbols for the various API entities.
   --  This procedure must be called prior to calling any of the
   --  operations in this package (and after JVM.Initialize has
   --  been called).

   function Is_API_Class (Class : Class_Id) return Boolean;
   --  Returns true if Class corresponds with an API class

end JVM.API;
