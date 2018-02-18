------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M _ A D A                                --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Text_IO;
with GNAT.IO_Aux;
with GNAT.Spitbol;
with J_Basics;
with J_List;
with J_Utils;                     use J_Utils;
with J_Zip;
with JVM_File;                    use JVM_File;
with JVM_Utils;                   use JVM_Utils;
with Osint;

package body JVM_Ada is

   ------------------------
   -- Local Types & Data --
   ------------------------

   function Get_JNI_Type (C : Character) return String;
   --  Return the string representation of the JNI_Type to use for
   --  the C Character representing a Java primitive type as used
   --  in field descriptor.

   function Get_JNI_Letter (C : Character) return Character;
   --  Return the letter used in the jvalue union type.

   function Get_Java_Type (Descriptor : String) return String;
   pragma Inline (Get_Java_Type);
   --  Return the string of the Java type represented in Descriptor.
   --  For primitive types, it returns the primitive name.
   --  For all other types (class, arrays), it returns "Object".
   --  Descriptor must be a Java type descriptor and not a method
   --  Descriptor.

   function Print_JNI_Conversion (C : Character) return String;
   --  Return the string that represent the type conversion or the
   --  function call needed to use a type defined in Java_Primitives
   --  when a JNI type is expected

   ------------------------------------
   -- Global Class File Symbol Table --
   ------------------------------------

   --  The following types are used to implement a symbol table where we keep
   --  certain critical informations concerning JVM classes. This information
   --  is needed, for instance, to determine whether a JVM class is a JVM
   --  exception or not.

   package String_List is new J_List (String);
   procedure Sort is new String_List.Sort ("<");
   --  Implements a list of strings.

   use String_List;

   type Class_Info is record
      Super_Class_Name : String_Ptr;
      --  The name of the class this class derives from.

      Implements : String_List.List;
      --  List of all the public interfaces this class implements in sorted
      --  order.  Needed so that we can output all the required Ada
      --  discriminants for this class.

      Is_Public : Boolean;
      --  Set if this class is public

      Is_Exception : Boolean;
      --  Set to YES if this class derives directly or indericetly form class
      --  java.lang.Throwable.
   end record;

   Unknown_Class : constant Class_Info
     := (Super_Class_Name => new String'("java/lang/Object"),
         Implements       => String_List.Empty_List,
         Is_Public        => True,
         Is_Exception     => False);

   function Do_Nothing (Info : Class_Info) return String;
   --  Returns string "Do nothing", needed for the following generic
   --  instantiation.

   package Symbol_Table_Pkg is
      new GNAT.Spitbol.Table (Class_Info, Unknown_Class, Do_Nothing);

   Symbol_Table : Symbol_Table_Pkg.Table (2048);
   --  This is the symbol table that contains all the classes already parsed by
   --  jvm2ada. This is used to avoid redundant parsing of the classes.

   function Enter_Info (CF : Class_File) return Class_Info;
   --  Enters the information relative to CF in Symbol_Table. If an
   --  entry for CF was already present that entry is updated.

   procedure Enter_Info (CF : Class_File);
   --  Same as above but the information entred is not returned.

   function Get_Info (Name : String) return Class_Info;
   --  Returns the information known about the class whose full name is
   --  Name. Name is of the form "java/lang/Object".  If this class has never
   --  been looked at before, the class it is parsed and the information is
   --  stored in the Symbol_Table.

   --------------------------
   -- With Clause Handling --
   --------------------------

   package String_Set is new Ada.Containers.Indefinite_Ordered_Sets (String);

   procedure Remove_From_Limited_With_List
     (Limited_With_List : in out String_Set.Set;
      With_List         : in out String_List.List;
      Outer_With_List   : String_List.List);
   --  Compare the content of Limited_With_List and With_List and remove the
   --  entries in Limited_With_List to make the context clause valid.
   --
   --  Outer_With_List is a non empty list if the current package is a nested
   --  package. In that case, we must also remove some entries of the
   --  Limited_With_List to avoid conflicts with the context clause of
   --  the parent package.

   ---------------------------------
   -- Package Generation Handling --
   ---------------------------------

   Generated_Package_Set : String_Set.Set;
   --  Set containing the name of all the packages that have already been
   --  generated

   ----------------------------------------
   -- Method and Constructor Id Handling --
   ----------------------------------------

   package String_Natural_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Natural, Ada.Strings.Hash, "=");

   Used_Names : String_Natural_Map.Map;
   --  Map of names already in use in the Ada package spec and body currently
   --  being generated. This map is used to avoid name clashing, since Ada is
   --  case insensitive whereas Java is not. If a string S is present in the
   --  Used_Names, the name S is in use. The number associated with it
   --  gives the number of times the name is in use in the Ada spec and
   --  body. If this number is 1 then a "_K" must be appended at the end of
   --  the new instance of this name. Otherwise if this number is nnn then
   --  a "_Knnn" must be appended.
   --  All strings must be stored in upper case.

   function Subprogram_ID_Name (Name : String) return String;
   --  Return the name of the variable used to store the JNI subprogram ID.

   --------------------------------
   -- With List of Outer Classes --
   --------------------------------

   package String_String_List_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (String, String_List.List, Ada.Strings.Hash, "=");

   Outer_Class_Map : String_String_List_Map.Map;
   --  Map containing the With_List of classes containing at least one public
   --  nested class.

   --------------------------
   -- Search File Handling --
   --------------------------

   --  When an archive is specified via a -I or -L switch the whole archive is
   --  kept in memory, uncompressed. When the archive is read, its content is
   --  parsed and the archive directory is kept so that looking for a file in
   --  the archived is fast.

   package Archive_Search is new J_List (J_Utils.Archive_Directory_Access);

   Classes_Search_List : Archive_Search.List := Archive_Search.Empty_List;
   --  List of archives we must look in to locate .class files.

   Sources_Search_List : Archive_Search.List := Archive_Search.Empty_List;
   --  List of archives we must look in to locate source files.

   generic
      The_Search_List : in out Archive_Search.List;
   procedure Generic_Search_In (Zip : String);
   --  Generic version of Search_Classes_In and Search_Sources_In.

   ----------------------------
   -- File Location Routines --
   ----------------------------

   --  All file location routines return a File_Id given below.

   type File_Id is record
      Stream : Stream_Of_U1_Ptr;
      --  The zip archive containing the file.

      Info   : J_Zip.File_Info;
      --  The location of the file within the zip archive.
   end record;

   No_File : constant File_Id := (null, (1, 0, 1, 0, False, False));

   function Find_Class_File
     (File          : String;
      Ignore_Errors : Boolean := False;
      Ignore_Casing : Boolean := False)
      return File_Id;
   --  Locate class file File (which is expected to be of the form
   --  "java/lang/Object.class") and returns its File_Id. File is searched in:
   --
   --    (1) If we are currently processing a zip archive look there first
   --    (2) Then look in each of the archives specified by the -Lzip
   --        switches (i.e. look through the Classes_Search_List)
   --
   --  If no class file is found, then a warning message is printed if
   --  Ignore_Errors is False and No_File is returned. If Ignore_Casing is
   --  set then we ignore casing differences in file names when looking for
   --  a file.

   --  (currently unused)
   --  function Find_Source_File
   --    (File          : String;
   --     Ignore_Errors : Boolean := not Verbose_Mode)
   --    return File_Id;
   --  --  Tries to locate source file File by looking in each of the locations
   --  --  specified by the -Izip switches (i.e. look through the
   --  --  Sources_Search_List). If no class file is found, then a warning
   --  --  message is printed when Ignore_Errors is False and No_File is
   --  --  returned.

   generic
      The_Search_List         : Archive_Search.List;
      --  The archive search list to look into

      Look_In_Current_Archive : Boolean;
      --  If this is set look in the current zip archive being processed if
      --  any.

   function Generic_Find_File
     (File          : String;
      Ignore_Errors : Boolean;
      Ignore_Casing : Boolean)
      return File_Id;
   --  Generic version of Find_Class_File & Find_Source_File.

   type Ada_File_Type is (Spec_File, Body_File);

   function Get_File_Extension (Output_Type : Ada_File_Type) return String;
   --  Return .ads for Spec_File and .adb for Body_File.

   -----------------------------
   -- Pretty Printing Package --
   -----------------------------

   package Pretty_Print is
      function Open_File
        (Class_Name  : String; Spec_Only : Boolean := True) return Boolean;
      --  Open a new output file to store the specifications corresponding to
      --  Class_Name. If Spec_Only is False, also open a new output file to
      --  store the body corresponding to Class_Name. This has sense only
      --  if Generation_Mode = JNI.
      --  Then the output of all Pretty_Print subprograms is set by default to
      --  the spec file. Return False if the file(s) could not be open.

      procedure Select_Output (Output : Ada_File_Type);
      --  Select the output of all Pretty_Print subprograms, ie either the
      --  spec or the body. This has only a meaning when Generation_Mode = JNI.
      --  When Generation_Mode = JGNAT, this procedure does nothing.

      procedure Close_File;
      --  Close the output file(s)

      function Current_Column return Ada.Text_IO.Count;
      --  Returns the current column in the currently selected output file

      procedure Incr_Indent (Step : Integer);
      --  Adds or subtracts 3 * Step to the current indentation level. The new
      --  indentation will be used after the line currently being output has
      --  been terminated with by a Print_Line.

      function Get_Indent return Ada.Text_IO.Count;
      --  Return the current indentation level.

      procedure Set_Tmp_Indent (Value : Ada.Text_IO.Count);
      --  Set the indentation at absolue value Value.

      procedure Print (S : String);
      --  Print S to the selected output file at the current column
      --  and set the column to the current indentation level.

      procedure Print_Line (S : String := "");
      --  Print S to the selected output file at the current column
      --  and add a New_Line at the end. Then set the column at the
      --  current indentation level.

      procedure Print_Empty_Line (S : String := "");
      --  Print S to the selected output file at the current column
      --  and add two New_Line calls at the end. Then set the column at the
      --  current indentation level.

   private
      Spec_Output_File    : aliased Ada.Text_IO.File_Type;
      Body_Output_File    : aliased Ada.Text_IO.File_Type;
      Current_Output      : Ada_File_Type := Spec_File;
      Spec_Only           : Boolean;
      Current_Indentation : Natural := 1;
   end Pretty_Print;

   --  Convenient renamings

   procedure P   (S : String)       renames Pretty_Print.Print;
   procedure PL  (S : String := "") renames Pretty_Print.Print_Line;
   procedure PEL (S : String := "") renames Pretty_Print.Print_Empty_Line;

   --------------------------
   -- Identifiers handling --
   --------------------------

   function Get_Identifier
     (Entity          : String;
      Short           : Boolean := False;
      Standard_Prefix : Boolean := False)
      return   String;
   --  Returns the Ada type name corresponding to the JVM class Entity. If
   --  Short is set, ignore the leading package name (e.g. in
   --  java/lang/Object, just process Object).
   --  If Standard_Prefix is set, add "Standard." as a prefix. This option
   --  only works if Short is not set.

   function Get_Class_Identifier (CF : Class_File) return String;
   --  Return the Class_Identifier name used to define the access type
   --  representing the Java class.

   function Is_Ada_Keyword (Identifier : String) return Boolean;
   --  Return true if Identifier is an Ada keyword

   function Ada_Identifier (Name : String) return String;
   --  Given a Java identifier it turns it into an Ada one.

   type Ada_Type_Format is
     (Regular_Type, Parameter_Type, Conversion_Type, Use_As_Name);
   --  The format of the string returned by the Ada_Type function. See below.

   function Ada_Type
     (D      : String;
      Format : Ada_Type_Format := Regular_Type)
      return String;
   --  Returns a string which is the Ada type equivalent for the JVM type
   --  descriptor D. If Format = Regular_Type then the string returned can be
   --  used in a regular Ada context except for a parameter type. If Format =
   --  Parameter_Type then the string returned can be used as a parameter
   --  type. Finally if Format = Use_As_Name then the string returned is the
   --  Ada type that can be used for appending to a parameter identifier name
   --  (e.g. P1_Int or P3_Object or P2_Object_Arr).
   --  If Format = Conversion_Type, the string returned is the full type (not
   --  the access one) that can be used to do a conversion.

   ------------------------------
   -- Field Accessors Routines --
   ------------------------------

   type Accessor is (Get, Set);
   --  The two different kinds of field accessors.

   function Get_Prefix (Kind : Accessor) return String;
   pragma Inline (Get_Prefix);
   --  Return the prefix used for accessors, ie "Get_" or "Set_".

   function Field_Method_Name
     (Descriptor : String;
      Is_Static  : Boolean;
      Kind       : Accessor)
      return String;
   --  Return the name of the [Get|Set]_<Type>_[Static_]Field method to use
   --  to get or set a Java field.

   function Is_Constant_Field (F : Member_Info) return Boolean;
   --  Returns True if field F is a constant.

   -------------------------------------------
   -- Public Methods Already Added Routines --
   -------------------------------------------

   type Method_Info is record
      Name       : String_Ptr;
      Descriptor : String_Ptr;
   end record;

   function Hash (Element : Method_Info) return Ada.Containers.Hash_Type;
   --  Function to hash the elements. This is done by concatenating
   --  Name.all and Descriptor.all and using Ada.Strings.Hash. Note that the
   --  comparison is case insensitive.

   function Equivalent_Methods (Left, Right : Method_Info) return Boolean;
   --  Two elements are the same if Name.all and Descriptor.all are the sames
   --  in both Left and Right. Note that the comparison is case insensitive.

   package Method_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Method_Info, Hash, Equivalent_Methods);

   --------------------
   -- Print Routines --
   --------------------

   procedure Print_Box (S : String);
   --  Print a standard Ada comment box around S which will look like that:
   --
   --  -------
   --  -- S --
   --  -------

   function Get_Array_Suffix (Dim : Natural) return String;
   --  Return the array suffix according to Dim, the dimension of the array
   --  If Dim = 0, return the empty string.
   --  If Dim = 1, return Array_Suffix.
   --  If Dim > 1, return Array_Suffix concatenated with '_' and Dim

   procedure Print_Get_Length_Signature;
   procedure Print_Get_Length_Implementation;
   procedure Print_Array_Constructor_Signature
     (Short_Id : String;
      Long_Id  : String;
      Dim      : Positive);
   procedure Print_Get_Array_Element_Signature
     (Short_Id : String;
      Dim         : Positive;
      Is_Abstract : Boolean);
   procedure Print_Set_Array_Element_Signature
     (Short_Id : String;
      Long_Id  : String;
      Dim      : Positive);
   --  Print the signature of the different array primitive methods without
   --  the ending ';'.
   --  Dim indicate the dimension of the array type.
   --  Id is the short Ada identifier of the class.
   --  Is_Abstract is set if the array component class is a Java abstract
   --  class.

   procedure Print_Get_Env;
   pragma Inline (Print_Get_Env);
   --  Print the statements to get the environment.

   procedure Print_Get_Env_Variables;
   pragma Inline (Print_Get_Env_Variables);
   --  Print the variables definition needed for the Print_Get_Env statements

   procedure Print_Check_Class;
   --  Print the statements which check that the class is set.

   procedure Print_Check_Method_ID
     (ID         : String;
      Descriptor : String;
      Is_Static  : Boolean);
   --  Print the statements which check that the method ID is set
   --  ID is the name of the method ID
   --  Descriptor is the method descriptor
   --  Is_Static is set if the method is static.

   procedure Print_Check_Field_ID
     (ID         : String;
      Descriptor : String;
      Is_Static  : Boolean);
   --  Print the statements which check that the field ID is set.

   procedure Print_Array_Declaration
     (Short_Id    : String;
      Long_Id     : String;
      Dim         : Positive;
      Is_Abstract : Boolean);
   --  Print the array visible type definitions and method specifications.
   --  Is_Abstract is set if the array component class is a Java abstract
   --  class.
   --  Dim = 1 prints the declaration for Class[], Dim = 2 prints the
   --  declaration for Class[][], ...

   procedure Print_Private_Array_Declaration (Dim : Positive);
   --  Print the private part of the array type declaration.
   --  Dim = 1 prints the declaration for Class[], Dim = 2 prints the
   --  declaration for Class[][], ...

   procedure Print_Array_Method_Body
     (Class_Name  : String;
      Short_Id    : String;
      Dim         : Positive;
      Is_Abstract : Boolean);
   --  Print the array method bodies.
   --  Is_Abstract is set when the array component class is a Java abstract
   --  class.
   --  Dim = 1 prints the declaration for Class[], Dim = 2 prints the
   --  declaration for Class[][], ...

   procedure Print_With
     (CF                      : Class_File;
      Has_Nested_Public_Class : Boolean;
      Outer_With_List         : String_List.List);
   --  Print the with statements for the class file. We print with statements
   --  for 'extends' and 'implements' clauses in the JVM class and in all
   --  other cases we print 'limited with' statements.
   --  Has_Nested_Public_Class is set when the CF contains nested public class.
   --  Outer_With_List contains the list of packages found in the with
   --  statements of the parent class of CF.

   procedure Print_With_For_Body (CF : JVM_File.Class_File);
   --  Print the with statements for the JNI body package.

   procedure Add_Packages
     (CF   : Class_File;
      M    : Member_Info;
      List : in out String_Set.Set);
   --  Add the package name to List for all the reference types found in the
   --  member descriptor.

   procedure Print_Obj_Declaration
     (CF          : Class_File;
      Pragma_List : in out String_List.List);
   --  Print the 'type Typ is ... ' declaration. Pragma_List is the
   --  list current list of pragmas to add to the private part of the
   --  Ada package spec.

   procedure Print_Field_Accessors (CF : Class_File);
   --  Process the fields of the Java class file CF and print the Get and Set
   --  subprograms to access them.

   procedure Print_Field_Accessor_Body (CF : Class_File);
   --  Process the fields of the Java class file CF and print the
   --  implementation of the Get and Set subprograms.

   procedure Print_Fields
     (CF            : Class_File;
      Static_Fields : Boolean;
      Pragma_List   : in out String_List.List);
   --  Process the fields of the Java class file CF. If Static_Fields is
   --  True, only static fields from the Java file will be printed, otherwise
   --  only the instance fields will be printed. Pragma_List is the list of
   --  pragmas to append to the private part of the Ada package spec.

   procedure Print_Methods
     (CF          : Class_File;
      Pragma_List : in out String_List.List);
   --  Process the methods of the class file CF. Class_Name is CF's name as
   --  it appears in CF. Pragma_List is the list of pragmas to append in the
   --  private part of the generated Ada package spec.

   procedure Print_Method_Signature (CF : Class_File; M : Member_Info);
   --  Print the method signature without the leading ";". It can thus be used
   --  for both spec and body.

   procedure Print_Exception_Comment (CF : Class_File; M : Member_Info);
   --  In JNI mode, print in a comment the exceptions that can be raised
   --  by the method described in M.

   procedure Generate_Needed_Packages (Class_Name : String);
   --  Generate the parent package (corresponding to directory information) for
   --  Class_Name. This is only done once per directory when generating all
   --  the classes of an archive file.
   --
   --  If Class_Name is "java/lang/String", then it will generate java.ads and
   --  java-lang.ads for instance.

   procedure Print_Variables (CF : Class_File; M : Member_Info);
   --  Print the variable useful for the method statements.

   procedure Print_Method_Statements
     (CF  : Class_File;
      M   : Member_Info;
      ID  : String);
   --  Print the statements following the method signature.

   procedure Print_Utils (CF : Class_File);
   --  Print utility subprograms.

   procedure Process_Method (CF : Class_File; M : Member_Info);
   --  Process the method M of the class C.

   function Call_Method_Name
     (Descriptor : String; Is_Static : Boolean) return String;
   pragma Inline (Call_Method_Name);
   --  Return the string of the JNI method used to call a Java method
   --  with a return Type represented by Descriptor.

   ---------------
   -- Utilities --
   ---------------

   function To_String (T : Utf8.Table) return String
     renames JVM_File.To_String;

   function Get_Public_Super_Class (CF : Class_File) return String;
   --  Returns the first public superclass of CF in the format "java/lang/Void"
   --  In fact, the super class must not always be used because it is sometimes
   --  not public and thus the corresponding package is not built by jvm2ada...

   function Access_Rights_OK (Flags : Access_Mask) return Boolean;
   --  Returns True if the accesss Flags indicates that the field or the method
   --  should be converted (this is the case for public and protected items
   --  only).

   procedure Check_Visibility
     (CF                      : Class_File;
      Is_Public               : out Boolean;
      Has_Nested_Public_Class : out Boolean;
      Is_Nested               : out Boolean;
      Outer_Class             : out CP_Index_Class);
   --  Given CF, a class file, this routine sets Is_Public to True if the CF
   --  is a bona fide public class. That is:
   --    (a) CF is marked as public
   --    (b) If CF is an inner class there must be an Inner_Classes attribute
   --        in CF and the public flag must be set in there.
   --    (c) CF is not one of Sun's implementation classes (unless swithc -s
   --        is set).
   --  Flag Has_Nested_Public_Classes is set iff CF has nested public
   --  classes. The side effect of this routine is to enter in the Used_Names
   --  table all the names of the public inner class of CF.
   --  Is_Nested is set to true if CF is a nested class and in that case,
   --  Outer_Class is set to the index position of the class containing CF.

   function Is_Public_Class (CF : Class_File) return Boolean;
   --  Returns True if the CF is a bona fide public class as defined above.

   function Is_Public_Member (M : Member_Info; T : CP.Table) return Boolean;
   --  Returns True if the Field or Method is public. A member is public if its
   --  access flag is either public or protected and if all its class types are
   --  public.

   function Contains_Public_Method
     (CF : Class_File; Method_Name : String) return Boolean;
   --  Return true if CF contains a public method named Method_Name, using
   --  the definition of public used in Is_Public_Member.

   function Is_Constructor (CF : Class_File; M : Member_Info) return Boolean;
   --  Return True if the method is a constructor.

   function Is_Abstract_Method
     (CF : Class_File; M : Member_Info) return Boolean;
   --  Return True if the method is abstract or is part of an abstract class.

   function Is_Abstract_Class (CF : Class_File) return Boolean;
   --  Return True id the class is abstract.

   function Dottify (Entity : String) return String;
   --  Replaces all "/" with "."

   procedure Warning (Msg : String);
   --  Print a warning message if not is quiet mode.

   function Get_Return_Type_Descriptor (Descriptor : String) return String;
   --  Descriptor must be a method descriptor.
   --  Return the descriptor of the return type.

   ----------
   -- Hash --
   ----------

   function Hash (Element : Method_Info) return Ada.Containers.Hash_Type is
   begin
      if Element.Name = null or else Element.Descriptor = null then
         return Ada.Strings.Hash ("");
      else
         return Ada.Strings.Hash
           (To_Upper (Element.Name.all) & To_Upper (Element.Descriptor.all));
      end if;
   end Hash;

   ------------------------
   -- Equivalent_Methods --
   ------------------------

   function Equivalent_Methods (Left, Right : Method_Info) return Boolean is
   begin
      if Left.Name = null or else Right.Name = null or else
        Left.Descriptor = null or else Left.Descriptor = null
      then
         return False;
      else
         return To_Upper (Left.Name.all) = To_Upper (Right.Name.all)
           and then Left.Descriptor.all = Right.Descriptor.all;
      end if;
   end Equivalent_Methods;

   ----------------------
   -- Access_Rights_OK --
   ----------------------

   function Access_Rights_OK (Flags : Access_Mask) return Boolean is
   begin
      return Is_Set (Flags, ACC_Public) or else Is_Set (Flags, ACC_Protected);
   end Access_Rights_OK;

   --------------------
   -- Ada_Identifier --
   --------------------

   function Ada_Identifier (Name : String) return String is
      function Ada_Identifier_Rec (Name : String) return String;

      ------------------------
      -- Ada_Identifier_Rec --
      ------------------------

      function Ada_Identifier_Rec (Name : String) return String is
      begin
         if Name'Length = 0 then
            return "";

         elsif Name'Length = 8 and then To_Upper (Name) = "STANDARD" then
            return Name & "_C";

            --  A single underscore is mapped into a U

         elsif Name = "_" then
            return "U";

            --  Replace leading underscores with a U_

         elsif Name (Name'First) = '_' then
            return
              "U_" & Ada_Identifier_Rec (Name (Name'First + 1 .. Name'Last));

            --  Ada N to a leading digit

         elsif Name (Name'First) in '0' .. '9' then
            return Ada_Identifier_Rec ("N" & Name);
         end if;

         --  Replace multiple "_" with "U_"

         for K in Name'Range loop
            if Name (K) = '_' then
               if K = Name'Last then
                  return Name & "U";
               elsif Name (K + 1) = '_' then
                  return Name (Name'First .. K)
                    & Ada_Identifier_Rec (Name (K + 1 .. Name'Last));
               end if;
            end if;
         end loop;

         return
           To_Upper (Name (Name'First)) & Name (Name'First + 1 .. Name'Last);
      end Ada_Identifier_Rec;

   begin
      if Keep_Original_Identifiers then
         return Name;
      elsif Is_Ada_Keyword (Name) then
         return Name & "_K";
      else
         return Ada_Identifier_Rec (Name);
      end if;
   end Ada_Identifier;

   --------------
   -- Ada_Type --
   --------------

   function Ada_Type
     (D      : String;
      Format : Ada_Type_Format := Regular_Type)
      return String
   is
      function Dimension_To_String (D : Integer) return String;
      --  If D = 1 returns the empty string otherwise it returns the string
      --  image of D preceded with a "_".

      function Scalar_Prefix return String;
      --  Returns the proper prefix for the scalar type

      -------------------------
      -- Dimension_To_String --
      -------------------------

      function Dimension_To_String (D : Integer) return String is
      begin
         if D = 1 then
            return "";
         else
            return "_" & Image (U1 (D));
         end if;
      end Dimension_To_String;

      -------------------
      -- Scalar_Prefix --
      -------------------

      function Scalar_Prefix return String is
      begin
         case Format is
            when Use_As_Name =>
               return "";
            when others      =>
               if Generation_Mode = JGNAT then
                  return "Java.";
               else
                  --  Prefix with Standard to avoid name conflicts with
                  --  packages containing "interfaces" in their name like
                  --  java.security.interfaces.DSAKeyPairGenerator.
                  return "Standard.Java_Primitives.";
               end if;
         end case;
      end Scalar_Prefix;

   begin
      case D (D'First) is
         when JVM_Class  =>
            for K in D'Range loop
               if D (K) = ';' then
                  declare
                     Java_Type : constant String := D (D'First + 1 .. K - 1);
                  begin
                     case Format is

                        when Parameter_Type =>
                           return "access " &
                             Get_Identifier (Java_Type,
                                             Standard_Prefix => True) &
                             '.' & Type_String & "'Class";

                        when Regular_Type =>
                           return "access " & Get_Identifier (Java_Type) &
                           "." & Type_String & "'Class";

                        when Conversion_Type =>
                           return Get_Identifier (Java_Type,
                                                  Standard_Prefix => True) &
                             '.' & Type_String;

                        when Use_As_Name =>
                           return Get_Identifier (Java_Type, Short => True);

                     end case;
                  end;
               end if;
            end loop;

            pragma Assert (False);
            return "";  --  This return should never be taken

         when JVM_Array =>
            declare
               Dim : Integer := 0;
               Pos : Natural := D'First;
            begin
               while D (Pos) = JVM_Array loop
                  Pos := Pos + 1;
                  Dim := Dim + 1;
               end loop;

               --  Is this is an array of class references

               if D (Pos) = JVM_Class then
                  case Format is

                     when Use_As_Name =>
                        return
                          Ada_Type (D (Pos .. D'Last), Use_As_Name) &
                          Get_Array_Suffix (Dim);

                     when Parameter_Type =>

                        if Generation_Mode = JGNAT then
                           declare
                              S : constant String :=
                                    Get_Identifier (D (Pos + 1 .. D'Last - 1));
                           begin
                              return "access " & S & ".Arr"
                                  & Dimension_To_String (Dim) & "_Obj";
                           end;

                        else
                           --  Generation_Mode = JNI
                           return "access " &
                             Get_Identifier (D (Pos + 1 .. D'Last - 1),
                                             Standard_Prefix => True) &
                             '.' & Type_String &
                             Get_Array_Suffix (Dim) & "'Class";
                        end if;

                     when Regular_Type =>

                        --  TODO

                        --  Downward conversion between class wide
                        --  interfaces are not supported currently.

                        --  For the time being, return Object, since this
                        --  a concrete type (as opposed to an interface)
                        --  and this kind of conversions are supported.

                        if Generation_Mode = JGNAT then
                           return "Standard.Java.Lang.Object.Ref";
                        else
                           --  Generation_Mode = JNI
                           return "Standard.Java.Lang.Object.Object";
                        end if;

                     when Conversion_Type =>
                        return Get_Identifier (D (Pos + 1 .. D'Last - 1),
                                               Standard_Prefix => True) &
                          '.' & Type_String & Get_Array_Suffix (Dim);

                        --  The real code should be the same as the one
                        --  used for Parameter_Type.

                  end case;

               --  Otherwise we have a scalar type array
               else

                  if Generation_Mode = JNI and then
                    not (Format = Use_As_Name)
                  then
                     if Format = Conversion_Type then
                        return "Standard.Java_Arrays." &
                          Ada_Type (D (Pos .. Pos), Use_As_Name) &
                          '_' & Type_String & Get_Array_Suffix (Dim);
                     else
                        if Format = Regular_Type then
                           --  TODO

                           --  Downward conversion between class wide
                           --  interfaces are not yet supported.

                           --  For the time being, return Object, since this
                           --  a concrete type (as opposed to an interface)
                           --  and this kind of conversion are supported.

                           return "Standard.Java.Lang.Object.Object";

                           --  The real code should be the same as the one
                           --  used for Parameter_Type.

                        else
                           --  Format = Parameter_Type
                           return "access Standard.Java_Arrays." &
                             Ada_Type (D (Pos .. Pos), Use_As_Name) &
                             '_' & Type_String & Get_Array_Suffix (Dim) &
                             "'Class";
                        end if;
                     end if;
                  else
                     if Dim > 1 then
                        return
                          Ada_Type (D (Pos .. Pos), Format) & Array_Suffix
                          & '_' & Image (U1 (Dim));
                     else
                        return Ada_Type (D (Pos .. Pos), Format) &
                          Array_Suffix;
                     end if;

                  end if;
               end if;
            end;

         when JVM_Byte    =>
            return Scalar_Prefix & "Byte";

         when JVM_Char    =>
            return Scalar_Prefix & "Char";

         when JVM_Double  =>
            return Scalar_Prefix & "Double";

         when JVM_Float   =>
            return Scalar_Prefix & "Float";

         when JVM_Int     =>
            return Scalar_Prefix & "Int";

         when JVM_Long    =>
            return Scalar_Prefix & "Long";

         when JVM_Short   =>
            return Scalar_Prefix & "Short";

         when JVM_Boolean =>
            return Scalar_Prefix & "Boolean";

         when others =>
            Osint.Fail ("Invalid type descriptor : " & D);
            return "";
      end case;
   end Ada_Type;

   ----------------------
   -- Check_Visibility --
   ----------------------

   procedure Check_Visibility
     (CF                      : Class_File;
      Is_Public               : out Boolean;
      Has_Nested_Public_Class : out Boolean;
      Is_Nested               : out Boolean;
      Outer_Class             : out CP_Index_Class)
   is
      use Class_Attribute;
      use Inner_Class;
      use String_Natural_Map;

      Class_Name : constant String := Get_String (CF, CF.This_Class);
      CA         : Class_Attribute_Info;
      ICI        : Inner_Class_Info;

      Is_A_Public_Nested_Class : Boolean := False;

   begin
      Has_Nested_Public_Class := False;
      Is_Nested               := False;
      Outer_Class             := CP_Empty;

      --  Skip SUN implementation classes if their mapping has not been
      --  requested.

      if Skip_Sun_Classes
        and then (Head (Class_Name, 4) = "sun/"
                  or else Head (Class_Name, 5) = "sunw/"
                  or else Head (Class_Name, 8) = "com/sun/")
      then
         Is_Public := False;
         return;
      end if;

      Is_Public := Is_Set (CF.Access_Flags, ACC_Public);

      --  Now look at the class attributes and check if this is a public
      --  nested class and whether it has any public nested classes.

      for J in 0 .. Last (CF.Attributes) loop
         CA := Get (CF.Attributes, J);

         if CA.Kind = Attr_Inner_Classes then
            for K in 0 .. Last (CA.Classes) loop
               ICI := Get (CA.Classes, K);

               --  Is this a nested class ?

               if ICI.Inner_Class_Info_Index = CF.This_Class then
                  Is_Nested := True;
                  Outer_Class := ICI.Outer_Class_Info_Index;
               end if;

               --  Is this a public nested class ?

               if Is_Public
                 and then ICI.Inner_Class_Info_Index = CF.This_Class
               then
                  --  If there is no name this was an anonymous class.

                  if ICI.Inner_Name_Index = 0 then
                     Is_Public := False;
                  elsif Is_Set (ICI.Inner_Class_Access_Flags, ACC_Public) then
                     Is_A_Public_Nested_Class := True;
                  end if;

               elsif ICI.Outer_Class_Info_Index = CF.This_Class then
                  --  If the nested class is anonymous nothing to do.

                  if ICI.Inner_Name_Index = 0 then
                     null;
                  elsif Is_Set (ICI.Inner_Class_Access_Flags, ACC_Public) then
                     Has_Nested_Public_Class := True;
                     declare
                        IC_Name : constant String :=
                          To_String
                            (J_Basics.Get_Utf8
                               (CF.Constant_Pool, ICI.Inner_Name_Index));
                        U_IC_Name : constant String := To_Upper (IC_Name);
                     begin
                        if Used_Names.Find (U_IC_Name) = No_Element then
                           Used_Names.Insert (U_IC_Name, 1);
                        end if;
                     end;
                  end if;
               end if;
            end loop;
         end if;
      end loop;

      --  If the class name has a $ in it, it is a nested class. If no nested
      --  class attribute was found then this class is not public.

      if Is_Public
        and then Ada.Strings.Fixed.Index (Class_Name, "$") /= 0
        and then not Is_A_Public_Nested_Class
      then
         Is_Public := False;
      end if;
   end Check_Visibility;

   --------------------
   -- Convert_To_Ada --
   --------------------

   procedure Convert_To_Ada (Bytes : Stream_Of_U1) is
      use Class_Attribute;
      use Class_Index;
      use Inner_Class;

      CF : constant Class_File := JVM_File.Read (Bytes, Check => True);
      Class_Name : constant String := Get_String (CF, CF.This_Class);

      Pragma_List : String_List.List;
      --  List of the pragmas to generate in the private part of the Ada
      --  package spec being generated.

      Iter : String_List.List_Iterator;

      Ada_Package : constant String := Get_Identifier (Class_Name);
      Short_Id    : constant String := Get_Class_Identifier (CF);
      Long_Id     : constant String := Ada_Package & '.' & Short_Id;
      Is_Public             : Boolean;
      Is_Nested             : Boolean;
      Is_Interface          : constant Boolean :=
                                Is_Set (CF.Access_Flags, ACC_Interface);
      Is_Abstract           : constant Boolean := Is_Abstract_Class (CF);
      Is_Exception          : Boolean;
      Contains_Public_Class : Boolean;
      Is_Java_Lang_Object   : constant Boolean :=
                                Class_Name = "java/lang/Object";

      Parent_Interface_Defined : Boolean := False;
      --  Boolean set when the current interface inherits from at least one
      --  of its parent interfaces in the generated code. This is needed
      --  because only public parent interfaces are considered.

      Outer_Class : CP_Index_Class;
      --  If the current class is nested, return the index of the outer class.
      Outer_With_List : String_List.List;
      --  List of with statements of the outer class for if the current
      --  class is a nested package.
   begin
      --  Update the Symbol_Table information with data about CF
      Enter_Info (CF);

      --  Call Is_Exception after calling Enter_Info (CF) otherwise we
      --  get a spurious warning message.
      Is_Exception := Get_Info (Class_Name).Is_Exception;

      Check_Visibility
        (CF, Is_Public, Contains_Public_Class, Is_Nested, Outer_Class);

      --  Special processing for nested classes:
      --  Nested classes must be processed after their containing class
      --  to construct the correct context clause. The child package
      --  created for the nested class thus need to know the with statements
      --  of its parent package in order to generate its own correct
      --  context clause.
      if Is_Nested and then Is_Public and then Generation_Mode = JNI then
         declare
            Outer_Class_Name : constant String :=
              Get_String (CF, Outer_Class);
            --  Name of the class in which the current class is defined
         begin
            if Outer_Class_Map.Contains (Outer_Class_Name) then
               --  The outer_class has already been processed
               Outer_With_List := Outer_Class_Map.Element (Outer_Class_Name);
            else
               --  The outer class has not been processed yet. This is only
               --  possible when processing files individually since archive
               --  file are sorted to ensure that nested class are processed
               --  after their containing class.
               Ada.Text_IO.Put_Line
                 ("WARNING: " & Outer_Class_Name & " should be processed " &
                  "before " & Class_Name & " otherwise there is no waranty " &
                  "that the package for " & Class_Name & " is correct.");
            end if;
         end;
      end if;

      if not Is_Public then
         --  If this is not a public class, but it contains a public nested
         --  class generate an empty package spec so that the nested class has
         --  a proper Ada package parent (e.g. java.awt.font.TextLine and
         --  java.awt.font.TextLine.TextLineMetrics).

         if Contains_Public_Class then
            if Pretty_Print.Open_File (Ada_Package) then
               if Verbose_Mode then
                  Ada.Text_IO.Put_Line ("  " & Class_Name & ".class");
               end if;

               if Generation_Mode = JGNAT then
                  PEL ("pragma Extensions_Allowed (On);");
               else
                  --  The class has not with statements but it contains
                  --  a nested class so Outer_Class_Map must be filled.
                  Outer_Class_Map.Insert (Class_Name, String_List.Empty_List);
               end if;

               P ("package " & Ada_Package & " is");

               if Generation_Mode = JGNAT then
                  Pretty_Print.Incr_Indent (1);
                  PL;
                  Pretty_Print.Incr_Indent (-1);
                  PL ("pragma Preelaborate;");
               else
                  PL;
               end if;

               PL ("end " & Ada_Package & ';');

               if Generation_Mode = JGNAT then
                  PL ("pragma Import (Java, " & Ada_Package & ", """
                      & Dottify (Class_Name) & """);");
                  PL ("pragma Extensions_Allowed (Off);");
               end if;
               Pretty_Print.Close_File;

            end if;
         end if;

         Used_Names.Clear;
         return;
      end if;

      --  If we can not create the output file, just print a warning and exit.

      if not Pretty_Print.Open_File (Ada_Package, Spec_Only => False) then
         Used_Names.Clear;
         return;
      elsif Verbose_Mode then
         Ada.Text_IO.Put_Line ("  " & Class_Name & ".class");
      end if;

      --  Grab the following names since these are already in use in the
      --  generated specs.

      Used_Names.Insert ("STANDARD", 1);
      Used_Names.Insert ("TYP", 1);
      Used_Names.Insert (U_Exception_String, 1);

      if Generation_Mode = JGNAT then
         Used_Names.Insert ("REF", 1);
         Used_Names.Insert ("ARR", 1);
         Used_Names.Insert ("ARR_2", 1);
         Used_Names.Insert ("ARR_3", 1);
      else
         Used_Names.Insert (U_Constructor_String, 1);
         --  Add the name of all the methods used in the body implementation
         --  that ends by "_ID" since they could conflict with the generated
         --  method and field accessor ID.
         Used_Names.Insert ("GET_METHOD_ID", 1);
         Used_Names.Insert ("GET_STATIC_METHOD_ID", 1);
         Used_Names.Insert ("GET_FIELD_ID", 1);
         Used_Names.Insert ("GET_STATIC_FIELD_ID", 1);
         --  Add the name of all types and variables that also ends with "_ID"
         Used_Names.Insert ("J_FIELD_ID", 1);
      end if;

      if Generation_Mode = JGNAT then

         --  Allow the extensions when interfacing to the java API

         PL ("pragma Extensions_Allowed (On);");

      end if;

      --  Print the list of required with_statements and with_type_statements

      Print_With (CF, Contains_Public_Class, Outer_With_List);
      PL;

      --  Print the definition of the class

      PEL ("package " & Ada_Package & " is");

      if Generation_Mode = JGNAT then

         PEL ("pragma Preelaborate;");

      else
         Pretty_Print.Select_Output (Body_File);
         Print_With_For_Body (CF);
         PL;
         PEL ("package body " & Ada_Package & " is");
         Pretty_Print.Select_Output (Spec_File);
      end if;

      --  Print the incomplete declaration for types

      Pretty_Print.Incr_Indent (1);
      --  Increment the indentation in order to print the
      --  content of the package

      PL;
      Print_Box ("Type Declarations");
      PL;

      if Is_Set (CF.Access_Flags, ACC_Final) then
         PL ("--  final class");
      end if;

      if Generation_Mode = JGNAT then

         if not Is_Java_Lang_Object then
            PL ("type Typ;");
            String_List.Append ("pragma Convention (Java, Typ);",
                                Pragma_List);
         else
            PL ("type " & Type_String & " is tagged limited null record;");
            PL ("pragma Convention (Java, " & Type_String & ");");
         end if;

         PL ("type Ref is access all " & Type_String & "'Class;");

      else
         --  Generation_Mode = JNI

         if Is_Interface then
            P ("type " & Type_String & " is interface");
         elsif not Is_Java_Lang_Object then

            if Is_Exception or else Is_Abstract then
               P ("--  ");

               if Is_Abstract then
                  P ("abstract ");
               end if;

               if Is_Exception then
                  P ("exception ");
               end if;

               PL ("class");

            end if;

            P ("type " & Type_String & " is abstract new " &
               Get_Identifier (Get_Public_Super_Class (CF),
                               Standard_Prefix => True) &
               '.' & Type_String);
         else
            P ("type " & Type_String & " is abstract new JNI_Data");
         end if;

         --  Implemented interfaces

         if Length (CF.Interfaces) /= 0 then

            for K in 0 .. Last (CF.Interfaces) loop
               declare
                  Interf_Index : constant CP_Index
                    := Get (CF.Interfaces, K);
                  Interf_Name  : constant String
                    := Get_String (CF, Interf_Index);
               begin
                  --  Ignore non-public interfaces

                  if Get_Info (Interf_Name).Is_Public then
                     Parent_Interface_Defined := True;
                     PL;
                     P ("  and " & Get_Identifier (Interf_Name,
                                                   Standard_Prefix => True) &
                        '.' & Type_String);
                  end if;
               end;
            end loop;
         end if;

         if Is_Interface and not Parent_Interface_Defined then
            --  If the current interface inherits from no public interface, it
            --  has to inherit explicitely from Root_Interface, since all
            --  interface types have to inherit from Root_Interface.
            P (" and JNI_Object.Root_Interface");
         end if;

         if Is_Interface then
            PL (";");
         else
            --  The class types have a private view which is not abstract.
            --  The public view is abstract to avoid the creationg of objects
            --  without using a constructor. This is not needed for interfaces.
            PL (" with private;");
         end if;

         --  Print the access type.
         PEL ("type " & Short_Id & " is access all " & Type_String &
              "'Class;");

         --  Print the exception type.
         if Is_Exception then
            --  Create an exception variable, even for abstract tagged types.
            --  This will be useful for the complete implementation of the
            --  exception mechanism allowing to convert between the Ada
            --  exception type (which has no meaning and no information
            --  except a String) and the real tagged type mapping the Java
            --  exception object (which can store information in exactly
            --  the same way as other Java objects).
            PL;
            PEL (Exception_String & " : exception;");
         end if;

      end if;

      if Generation_Mode = JNI then
         Pretty_Print.Select_Output (Body_File);

         PL;
         Print_Box ("JNI variables");

         PL;
         PEL (Class_String & ": J_Class := J_Null_Class;");

         --  Print all the utility subprograms.
         Print_Utils (CF);

         Pretty_Print.Select_Output (Spec_File);
      end if;

      --  Print the array declarations.

      if Generation_Mode = JGNAT then

         PL;
         Print_Box ("Array Declarations");

         PL;
         PL  ("type Arr_Obj is array (Natural range <>) of Ref;");
         PL  ("type Arr     is access all Arr_Obj;");
         PL  ("type Arr_2_Obj is array (Natural range <>) of Arr;");
         PL  ("type Arr_2     is access all Arr_2_Obj;");
         PL  ("type Arr_3_Obj is array (Natural range <>) of Arr_2;");
         PEL ("type Arr_3     is access all Arr_3_Obj;");
         PL;

         --  Complete type declaration.

         if not Is_Java_Lang_Object then
            Print_Obj_Declaration (CF, Pragma_List);
         end if;

         if Class_Name = "java/lang/String" then
            Print_Box ("Java String to Ada String Conversion Routine");
            PL ("type String_Access is access all Standard.String;");
            PL ("function ""+"" (S : Ref) return String_Access;");
            PL ("function ""+"" (S : Standard.String) return Ref;");
            String_List.Append
              ("pragma Import (Java, ""+"", "
               & """jgnat.adalib.GNAT_libc.to_string"");",
               Pragma_List);
         end if;

         Print_Methods (CF, Pragma_List);

         Print_Fields (CF, Static_Fields => True,
                       Pragma_List => Pragma_List);

      else
         --  Generation_Mode = JNI

         if Is_Java_Lang_Object then

            --  Change the order of the declarations to ensure that
            --  Root_Array inherits from all the Object methods.
            --  This is only possible because Object methods do not use
            --  arrays of Objects...

            Print_Field_Accessors (CF);
            Print_Methods (CF, Pragma_List);

            --  Add the root array type in Java.Lang.Object only.

            PL;
            Print_Box ("Root Array Type");
            PL;
            PL ("--  All array types are descendant of " & Root_Array & '.');
            PL ("--  This allows to share the implementation of Get_Length");
            PL ("--  which is exactly the same for each array type");
            PEL ("type " & Root_Array & " is abstract new " & Type_String &
                 " with private;");
            PL;
            Print_Get_Length_Signature;
            PL (";");
            PL ("--  Arr should never be the null value.");
            PEL ("--  Constraint_Error will be raised in such a situation");

            Pretty_Print.Select_Output (Body_File);

            --  Print the implementation of Get_Length.
            if Class_Name = "java/lang/Object" then
               PL;
               Print_Get_Length_Implementation;

               --  Print all the method and field accessor implementations.
               Print_Field_Accessor_Body (CF);
            end if;

            Pretty_Print.Select_Output (Spec_File);
         end if;

         PL;
         Print_Box ("Array Declarations");

         PL;
         PL ("--  Unidimensional array");
         Print_Array_Declaration (Short_Id, Long_Id, 1, Is_Abstract);

         --  Print the array method implementation.
         Pretty_Print.Select_Output (Body_File);
         Print_Array_Method_Body (Class_Name, Short_Id, Dim => 1,
                                  Is_Abstract => Is_Abstract);
         Pretty_Print.Select_Output (Spec_File);

         --  The array component is never abstract for arrays of Dimension >= 2
         for I in 2 .. Number_Of_Array_Types loop
            PL;
            PL  ("-- " & Integer'Image (I) & " dimensional array");
            Print_Array_Declaration (Short_Id, Long_Id, I, False);

            Pretty_Print.Select_Output (Body_File);
            Print_Array_Method_Body (Class_Name, Short_Id, Dim => I,
                                     Is_Abstract => False);
            Pretty_Print.Select_Output (Spec_File);

         end loop;

         if not Is_Java_Lang_Object then
            --  In all other classes, field and methods must appear after
            --  the array declaration since they can use them. This is the
            --  case for instance in java.lang.Class.
            Print_Field_Accessors (CF);

            --  Print all the method and field accessor implementations.
            Pretty_Print.Select_Output (Body_File);
            Print_Field_Accessor_Body (CF);
            Pretty_Print.Select_Output (Spec_File);

            Print_Methods (CF, Pragma_List);
         end if;

      end if;

      --  Private part

      Pretty_Print.Incr_Indent (-1);
      PL ("private");
      Pretty_Print.Incr_Indent (1);

      if Generation_Mode = JNI then

         --  No private part needed for the interface types

         if not Is_Interface then

            PL;

            if not Is_Java_Lang_Object then

               P ("type " & Type_String & " is ");

               if Is_Abstract then
                  P ("abstract ");
               end if;

               P ("new " &
                  Get_Identifier (Get_Public_Super_Class (CF),
                                  Standard_Prefix => True) &
                  '.' & Type_String);

            else
               P ("type " & Type_String &
                  " is new JNI_Data");
            end if;

            --  Implemented interfaces

            if Length (CF.Interfaces) /= 0 then

               for K in 0 .. Last (CF.Interfaces) loop
                  declare
                     Interf_Index : constant CP_Index
                       := Get (CF.Interfaces, K);
                     Interf_Name  : constant String
                       := Get_String (CF, Interf_Index);
                  begin
                     --  Ignore non-public interfaces

                     if Get_Info (Interf_Name).Is_Public then
                        PL;
                        P ("  and " &
                           Get_Identifier (Interf_Name,
                                           Standard_Prefix => True) &
                           '.' & Type_String);
                     end if;
                  end;
               end loop;

            end if;

            PEL (" with null record;");

            if not Is_Abstract then
               PL;
               PEL ("function " & Constructor_String &
                    " (Params : not null access Parameters) " &
                    "return " & Type_String & ';');
            end if;
         end if;

         --  private part for the array types

         if Is_Java_Lang_Object then
            PL;
            PL ("--  " & Root_Array & " type");
            PEL ("type " & Root_Array & " is abstract new " & Type_String &
                " with null record;");
         end if;

         PL;
         PL ("--  Unidimensional array");
         Print_Private_Array_Declaration (Dim => 1);

         for I in 2 .. Number_Of_Array_Types loop
            PL;
            PL  ("-- " & Integer'Image (I) & " dimensional array");
            Print_Private_Array_Declaration (Dim => I);
         end loop;

      end if;

      --  Print the pragmas associated with the current class and clean up
      --  the list of pragmas.

      Associate (Pragma_List, Iter);
      declare
         Current_Indentation : constant Ada.Text_IO.Count
           := Pretty_Print.Get_Indent;
      begin
         while not Is_Last (Iter) loop
            Pretty_Print.Set_Tmp_Indent (Current_Indentation);
            PEL (Get (Iter));
            Next (Iter);
         end loop;
      end;
      Clean (Pragma_List);

      if Generation_Mode = JNI then
         Pretty_Print.Select_Output (Body_File);

         --  Print the elaboration code if needed.

         if not Is_Abstract then
            PEL;
            PL ("begin");
            P ("Register (""" & Dottify (Class_Name) & """, Typ'Tag);");
         end if;
         Pretty_Print.Select_Output (Spec_File);
      end if;

      --  Terminate the package declaration

      Pretty_Print.Incr_Indent (-1);
      PEL;
      PL ("end " & Ada_Package & ';');

      if Generation_Mode = JNI then
         Pretty_Print.Select_Output (Body_File);

         --  Print the end of the package body

         PEL;
         PL ("end " & Ada_Package & ';');
         Pretty_Print.Select_Output (Spec_File);
      end if;

      if Generation_Mode = JGNAT then
         PL ("pragma Import (Java, " & Ada_Package
             & ", """ & Dottify (Class_Name) & """);");
         PL ("pragma Extensions_Allowed (Off);");
      end if;

      --  Clear the map of names already in use for the next use.
      --  This has to be done after the body generation in JNI mode for
      --  instance to avoid conflicts between ID names (that are generated
      --  only in the body) and method or field accessor names.

      Used_Names.Clear;
      Pretty_Print.Close_File;

      --  Generate the parent packages needed to compile the package
      --  generated for the current Java class. This is needed for
      --  standalone class files and for class files stored in .jar
      --  created with the "jar" program found in JDK >= 1.3 since the
      --  directory information is not stored in separate entries
      --  anymore (it was the case up to JDK 1.2.*) thus
      --  Convert_Directory_To_Add is never called in J_Utils.Process.
      Generate_Needed_Packages (Class_Name);

   end Convert_To_Ada;

   ------------------------------
   -- Generate_Needed_Packages --
   ------------------------------

   procedure Generate_Needed_Packages (Class_Name : String) is
      use String_Set;

      Start : constant Integer := Class_Name'First;
      Pos   : Integer := Class_Name'First;
   begin
      while Pos <= Class_Name'Last loop

         if Class_Name (Pos) = '/' then
            if Generated_Package_Set.Find (Class_Name (Start .. Pos - 1)) =
              No_Element
            then
               Generated_Package_Set.Insert (Class_Name (Start .. Pos - 1));
               Convert_Directory_To_Ada (Class_Name (Start .. Pos - 1));
               --  Generate the parent Ada package
            end if;
         end if;

         Pos := Pos + 1;
      end loop;
   end Generate_Needed_Packages;

   ------------------------------
   -- Convert_Directory_To_Ada --
   ------------------------------

   procedure Convert_Directory_To_Ada (Name : String) is
      use String_Set;

      Name_L   : constant String := To_Lower (Name);
      Ada_Name : constant String := Get_Identifier (Name);

   begin
      --  JGNAT already contains a package Java in its library

      if Name_L = "java" and then Generation_Mode = JGNAT then
         if Verbose_Mode then
            Ada.Text_IO.Put_Line
              ("java.ads do not need to be generated for JGNAT");
         end if;
         return;
      end if;

      --  Skip the Sun packages if not requested

      if Skip_Sun_Classes
        and then (Name_L = "sun"
                  or else Head (Name_L, 4) = "sun/"
                  or else Head (Name_L, 4) = "sun\"

                  or else Name_L = "sunw"
                  or else Head (Name_L, 5) = "sunw/"
                  or else Head (Name_L, 5) = "sunw\"

                  or else Name_L = "com/sun"
                  or else Name_L = "com\sun"
                  or else Head (Name_L, 8) = "com/sun/"
                  or else Head (Name_L, 8) = "com\sun\")
      then
         if Verbose_Mode then
            Ada.Text_IO.Put_Line
              ("Sun directories are not processed: " & Name);
         end if;
         return;
      end if;

      --  To be a Java package the directory name must contain only letters,
      --  digits or underscores

      if not Is_Letter (Name (Name'First)) then
         if Verbose_Mode then
            Ada.Text_IO.Put_Line
              ("Directories not starting with a letter are not processed: " &
               Name);
         end if;
         return;
      end if;

      for J in Name'Range loop
         if not (Is_Letter (Name (J))
                 or else Is_Digit (Name (J))
                 or else Name (J) = '_'
                 or else Name (J) = '/'
                 or else Name (J) = '\')
         then
            if Verbose_Mode then
               Ada.Text_IO.Put_Line
                 ("Directories containing illegal characters (" & Name (J) &
                  ") are not processed: " & Name);
            end if;
            return;
         end if;
      end loop;

      --  Do nothing if there is already a class with the same (case
      --  insensitive) name, since it's that class that will generate the
      --  corresponding Ada package spec. This occurs for instance in the
      --  Java API with directory java/awt/font and class java/awt/Font.class.

      if Find_Class_File
           (Name & ".class",
            Ignore_Errors => True,
            Ignore_Casing => True) /= No_File
      then
         if Verbose_Mode then
            Ada.Text_IO.Put_Line
              ("Directory not processed because a class has the same name: " &
               Name);
         end if;
         return;
      end if;

      if not Quiet_Mode then
         Ada.Text_IO.Put_Line ("Processing: " & Name & " ...");
      end if;

      --  Avoid a duplicated generation. This has been added to support the
      --  Generate_Needed_Packages procedure introduced for .jar files
      --  produced with JDK >= 1.3.

      if Generated_Package_Set.Find (Name) = No_Element then
         Generated_Package_Set.Insert (Name);
      end if;

      --  Otherwise create a very simple Ada package spec corresponding to
      --  the Java package. We use Ada_Name here instead of name since
      --  otherwise conflicts are triggered between the Ada package Standard
      --  and intermediate packages generated for classes that belong to a Java
      --  package named 'standard' (e.g javax/print/attribute/standard).

      if not Pretty_Print.Open_File (Ada_Name) then
         return;
      end if;

      if Generation_Mode = JGNAT then
         PL ("pragma Extensions_Allowed (On);");
         Pretty_Print.Incr_Indent (1);
      end if;

      PL ("package " & Ada_Name & " is");

      if Generation_Mode = JGNAT then
         Pretty_Print.Incr_Indent (-1);
         PL ("pragma Preelaborate;");
      end if;

      PL ("end " & Ada_Name & ';');

      if Generation_Mode = JGNAT then
         PL ("pragma Import (Java, " & Ada_Name & ", """
             & Dottify (Name) & """);");
         PL ("pragma Extensions_Allowed (Off);");
      end if;

      Pretty_Print.Close_File;
   end Convert_Directory_To_Ada;

   ----------------
   -- Do_Nothing --
   ----------------

   function Do_Nothing (Info : Class_Info) return String is
      pragma Unreferenced (Info);
      --  Parameter needed for instantiation of GNAT.Spitbol.Table

   begin
      return "Nothing to do";
   end Do_Nothing;

   -------------
   -- Dottify --
   -------------

   function Dottify (Entity : String) return String is
      Tmp : String := Entity;
   begin
      for J in Tmp'Range loop
         if Tmp (J) = '/' then
            Tmp (J) := '.';
         end if;
      end loop;
      return Tmp;
   end Dottify;

   ----------------
   -- Enter_Info --
   ----------------

   pragma Warnings (Off);
   procedure Enter_Info (CF : Class_File) is
      Ignore : constant Class_Info := Enter_Info (CF);
   begin
      null;
   end Enter_Info;
   pragma Warnings (On);

   ----------------
   -- Enter_Info --
   ----------------

   function Enter_Info (CF : Class_File) return Class_Info is
      use Class_Index;
      use Class_Attribute;

      Class_Name       : constant String := Get_String (CF, CF.This_Class);
      Super_Class_Name : String_Ptr;
      Implements       : String_List.List;
      Is_Exception     : Boolean;
      Info             : Class_Info;

   begin
      --  Debug_Msg (Class_Name & ": added to Symbol Table");

      --  Get the name of the super class

      Super_Class_Name := new String'(Get_String (CF, CF.Super_Class));

      --  First copy the list of interfaces the superclass implements ad then
      --  add the new interfaces this class implements.

      Copy (Get_Info (Super_Class_Name.all).Implements, Implements);

      for K in 0 .. Last (CF.Interfaces) loop
         declare
            Interf_Index : constant CP_Index := Get (CF.Interfaces, K);
            Interf_Name  : constant String   := Get_String (CF, Interf_Index);
         begin
            --  Ignore non-public interfaces

            if Get_Info (Interf_Name).Is_Public then
               String_List.Append_If_Uniq (Interf_Name, Implements);
            end if;
         end;
      end loop;

      Sort (Implements);

      --  Figure out whether this class is a java exception.  There are
      --  only two cases where we know for sure whether or not we have an
      --  exception. In all other cases we must look at the parent class.

      Is_Exception := Get_Info (Super_Class_Name.all).Is_Exception;

      Info := (Super_Class_Name => Super_Class_Name,
               Implements       => Implements,
               Is_Public        => Is_Public_Class (CF),
               Is_Exception     => Is_Exception);

      Symbol_Table_Pkg.Set (Symbol_Table, Class_Name, Info);
      return Info;
   end Enter_Info;

   -----------------------
   -- Generic_Find_File --
   -----------------------

   function Generic_Find_File
     (File          : String;
      Ignore_Errors : Boolean;
      Ignore_Casing : Boolean)
      return File_Id
   is
      use J_Basics;
      use Archive_Search;

      subtype Zip_Data is J_Utils.Archive_Directory_Access;

      function Find (A_File : String; Zip : Zip_Data) return File_Id;
      --  Look for File in the zip archive Zip. If it is not there return
      --  No_File, otherwise return the proper File_Id.

      ----------
      -- Find --
      ----------

      function Find (A_File : String; Zip : Zip_Data) return File_Id is
      begin
         if Zip.Stream = null then
            return No_File;
         end if;

         for J in Zip.Archive'Range loop
            declare
               F    : J_Zip.File_Info renames Zip.Archive (J);
               Name : constant String :=
                 To_String (Zip.Stream (F.Name_First .. F.Name_Last));
            begin
               if Name = A_File
                 or else
                 (Ignore_Casing and then To_Upper (Name) = To_Upper (A_File))
               then
                  return (Stream => Zip.Stream, Info => F);
               end if;
            end;
         end loop;

         return No_File;
      end Find;

      Iterator : Archive_Search.List_Iterator;
      F_Id     : File_Id;

   begin --  of Generic_Find_File
      if Look_In_Current_Archive then
         F_Id := Find (File, J_Utils.Get_Current_Archive);
         if F_Id /= No_File then
            return F_Id;
         end if;
      end if;

      Associate (The_Search_List, Iterator);

      while not Is_Last (Iterator) loop
         F_Id := Find (File, Get (Iterator));
         if F_Id /= No_File then
            return F_Id;
         end if;
         Next (Iterator);
      end loop;

      if not Ignore_Errors then
         Warning ("WARNING: Can't find: " & File);
      end if;

      return No_File;
   end Generic_Find_File;

   ---------------------
   -- Find_Class_File --
   ---------------------

   function Find_Class_File
     (File          : String;
      Ignore_Errors : Boolean := False;
      Ignore_Casing : Boolean := False)
      return File_Id
   is
      function Find_Class_File_Instance is
         new Generic_Find_File (The_Search_List         => Classes_Search_List,
                                Look_In_Current_Archive => True);
   begin
      return Find_Class_File_Instance (File, Ignore_Errors, Ignore_Casing);
   end Find_Class_File;

   ----------------------
   -- Find_Source_File --
   ----------------------

   --  (currently unused)
   --  function Find_Source_File
   --    (File          : String;
   --     Ignore_Errors : Boolean := not Verbose_Mode)
   --    return File_Id
   --  is
      --  function Find_Source_File_Instance is
         --  new Generic_Find_File (The_Search_List => Sources_Search_List,
         --                         Look_In_Current_Archive => False);
   --  begin
   --     return Find_Source_File_Instance (File, Ignore_Errors, False);
   --  end Find_Source_File;

   -----------------------
   -- Generic_Search_In --
   -----------------------

   procedure Generic_Search_In (Zip : String) is
      Bytes : Stream_Of_U1_Ptr;
      --  Contains the bytes of the input archive or null if we have a
      --  directory.

   begin
      --  Make sure we have an uncompressed zip archive

      Bytes := J_Basics.Get_Stream_Of_U1 (Zip);

      declare
         use J_Zip;
         Archive : constant J_Utils.Archive_Directory_Access :=
           (Stream  => Bytes,
            Archive => new Archive_Directory'(Get_Archive_Dir (Bytes.all)));
      begin
         Archive_Search.Append (Archive, The_Search_List);
      end;

   exception
      when J_Zip.Bad_Zip_Archive =>
         Osint.Fail (Zip & " is not a zip archive");

      when J_Zip.Compressed_Zip_Archive =>
         Osint.Fail
           ("Compressed archive: " &
            Zip & " jvm2ada can only handle uncompressed archives.");
   end Generic_Search_In;

   --------------------
   -- Get_Identifier --
   --------------------

   function Get_Identifier
     (Entity          : String;
      Short           : Boolean := False;
      Standard_Prefix : Boolean := False)
      return   String
   is
      S : constant String := Entity;
   begin
      if S'Length = 0 then
         return "";
      end if;

      for J in reverse S'Range loop
         if S (J) = '/' or S (J) = '$' then
            declare
               Id : constant String := Ada_Identifier (S (J + 1 .. S'Last));
            begin
               if Short then
                  return Id;
               else
                  if Standard_Prefix then
                     --  A full name has to be prefixed with "Standard." when
                     --  used in a type string because otherwise name
                     --  conflicts occur. For instance, Awt is not found in
                     --  Com.Sun.Java.Swing.Plaf.Motif.MotifBorders with an
                     --  argument whose type is Java.Awt.Graphics.Typ'Class...
                     --  It works with Standard.Java.Awt.Graphics.Typ'Class.

                     --  TODO: Some more code is needed to insure that Standard
                     --  is never used as a package name.
                     return "Standard." &
                       Get_Identifier (S (S'First .. J - 1)) & '.' & Id;
                  else
                     return Get_Identifier (S (S'First .. J - 1)) & '.' & Id;
                  end if;
               end if;
            end;
         end if;
      end loop;

      return Ada_Identifier (S);
   end Get_Identifier;

   --------------
   -- Get_Info --
   --------------

   function Get_Info (Name : String) return Class_Info is
      use Class_Index;

      File : File_Id;
      CF   : Class_File;

   begin
      --  Debug_Msg ("    -> " & Name & ": Symbol Table lookup ... ");

      if Name = "" then
         return Unknown_Class;

      --  If the class info is already in the symbol table then return it.
      --  The entries for "java/lang/Object" and "java/lang/Throwable" have
      --  been entered in the Symbol_Table when this package is elaborated
      --  (see the begin-end section at the end of this package body).

      elsif Symbol_Table_Pkg.Present (Symbol_Table, Name) then
         return Symbol_Table_Pkg.Get (Symbol_Table, Name);
      end if;

      --  Otherwise, the class hasn't been looked at yet, so load it and
      --  parse it to get the information we need.

      File := Find_Class_File (Name & ".class");

      if File = No_File then
         return Unknown_Class;
      end if;

      CF := Read (File.Stream (File.Info.First .. File.Info.Last), True);

      return Enter_Info (CF);
   end Get_Info;

   ----------------------------
   -- Get_Public_Super_Class --
   ----------------------------

   function Get_Public_Super_Class (CF : Class_File) return String is
      use type Ada.Text_IO.Count;

      Class_Name : constant String := Get_String (CF, CF.This_Class);
      Info       : constant Class_Info := Get_Info (Class_Name);

      Super_Info : Class_Info;
      Super_Name : String_Ptr;

   begin
      Super_Name := Info.Super_Class_Name;

      loop
         Super_Info := Get_Info (Super_Name.all);

         if Super_Info.Is_Public then
            return Super_Name.all;
         end if;

         Super_Name := Super_Info.Super_Class_Name;
      end loop;
   end Get_Public_Super_Class;

   --------------------
   -- Is_Ada_Keyword --
   --------------------

   function Is_Ada_Keyword (Identifier : String) return Boolean is
      Str : constant String := To_Upper (Identifier);

   begin
      if Str'Length < 2 or else Str'Length > 12 then
         --  The longest Ada keyword is
         return False;
      end if;

      case Str (Str'First) is
         when 'A' =>
            case Str (Str'First + 1) is
               when 'B' => return False
                 or else Str = "ABORT"
                 or else Str = "ABS"
                 or else Str = "ABSTRACT";
               when 'C' => return False
                 or else Str = "ACCEPT"
                 or else Str = "ACCESS";
               when 'L' => return False
                 or else Str = "ALL"
                 or else Str = "ALIASED";
               when 'N' => return False
                 or else Str = "AND";
               when 'R' => return False
                 or else Str = "ARRAY";
               when 'T' => return False
                 or else Str = "AT";
               when others => return False;
            end case;

         when 'B' => return False
           or else Str = "BEGIN"
           or else Str = "BODY";

         when 'C' => return False
           or else Str = "CASE"
           or else Str = "CONSTANT";

         when 'D' =>
            case Str (Str'First + 1) is
               when 'E' => return False
                 or else Str = "DECLARE"
                 or else Str = "DELAY"
                 or else Str = "DELTA";
               when 'I' => return Str = "DIGITS";
               when 'O' => return Str = "DO";
               when others => return False;
            end case;

         when 'E' =>
            case Str (Str'First + 1) is
               when 'L' => return False
                 or else Str = "ELSE"
                 or else Str = "ELSIF";
               when 'N' => return False
                 or else Str = "END"
                 or else Str = "ENTRY";
               when 'X' => return False
                 or else Str = "EXCEPTION"
                 or else Str = "EXIT";
               when others => return False;
            end case;

         when 'F' => return False
           or else Str = "FOR"
           or else Str = "FUNCTION";

         when 'G' => return False
           or else Str = "GENERIC"
           or else Str = "GOTO";

         when 'I' =>
            case Str (Str'First + 1) is
               when 'F' => return Str = "IF";
               when 'N' => return False
                 or else Str = "IN"
                 or else Str = "INTERFACE";
               when 'S' => return Str = "IS";
               when others => return False;
            end case;

         when 'L' => return False
           or else Str = "LIMITED"
           or else Str = "LOOP";

         when 'M' => return False
           or else Str = "MOD";

         when 'N' => return False
           or else Str = "NEW"
           or else Str = "NOT"
           or else Str = "NULL";

         when 'O' => return False
           or else Str = "OF"
           or else Str = "OR"
           or else Str = "OTHERS"
           or else Str = "OUT"
           or else Str = "OVERRIDING";

         when 'P' =>
            case Str (Str'First + 1) is
               when 'A' => return False
                 or else Str = "PACKAGE";
               when 'R' => return False
                 or else Str = "PRAGMA"
                 or else Str = "PRIVATE"
                 or else Str = "PROCEDURE"
                 or else Str = "PROTECTED";
               when others => return False;
            end case;

         when 'R' =>
            case Str (Str'First + 1) is
               when 'A' => return False
                 or else Str = "RAISE"
                 or else Str = "RANGE";
               when 'E' => return False
                 or else Str = "RECORD"
                 or else Str = "REM"
                 or else Str = "RENAMES"
                 or else Str = "RETURN"
                 or else Str = "REVERSE"
                 or else Str = "REQUEUE";
               when others => return False;
            end case;

         when 'S' => return False
           or else Str = "SELECT"
           or else Str = "SEPARATE"
           or else Str = "SUBTYPE"
           or else Str = "SYNCHRONIZED";

         when 'T' => return False
           or else Str = "TASK"
           or else Str = "TERMINATE"
           or else Str = "THEN"
           or else Str = "TYPE"
           or else Str = "TAGGED";

         when 'U' => return False
           or else Str = "USE"
           or else Str = "UNTIL";

         when 'W' => return False
           or else Str = "WHEN"
           or else Str = "WHILE"
           or else Str = "WITH";

         when 'X' => return False
           or else Str = "XOR";

         when others => return False;
      end case;
   end Is_Ada_Keyword;

   ---------------------
   -- Is_Public_Class --
   ---------------------

   pragma Warnings (Off);
   function Is_Public_Class (CF : Class_File) return Boolean is
      Is_Public : Boolean;
      Is_Nested : Boolean;
      Ignore    : Boolean;
      Outer_Class : CP_Index_Class;
   begin
      Check_Visibility (CF, Is_Public, Ignore, Is_Nested, Outer_Class);
      return Is_Public;
   end Is_Public_Class;
   pragma Warnings (On);

   --------------------
   -- Is_Constructor --
   --------------------

   function Is_Constructor (CF : Class_File; M : Member_Info) return Boolean is
      use J_Basics;

      T : constant CP.Table := CF.Constant_Pool;
      Name : constant String := To_String (Get_Utf8 (T, M.Name_Index));
   begin
      return Name (Name'First) = '<';
   end Is_Constructor;

   ------------------------
   -- Is_Abstract_Method --
   ------------------------

   function Is_Abstract_Method
     (CF : Class_File; M : Member_Info) return Boolean is
   begin
      return Is_Set (CF.Access_Flags, ACC_Interface) or else
        Is_Set (M.Access_Flags, ACC_Abstract);
   end Is_Abstract_Method;

   -----------------------
   -- Is_Abstract_Class --
   -----------------------

   function Is_Abstract_Class (CF : Class_File) return Boolean is
   begin
      return Is_Set (CF.Access_Flags, ACC_Abstract);
   end Is_Abstract_Class;

   pragma Inline (Is_Abstract_Class);

   ----------------------
   -- Is_Public_Member --
   ----------------------

   function Is_Public_Member (M : Member_Info; T : CP.Table) return Boolean is
      use Member_Attribute;

      Descriptor : constant String :=
        To_String (J_Basics.Get_Utf8 (T, M.Descriptor_Index));

      K : Natural;
      Start_Class : Natural;

   begin
      if not Access_Rights_OK (M.Access_Flags) then
         return False;
      end if;

      --  If the Member is Deprecated or Synthetic skip it

      for J in 0 .. Last (M.Attributes) loop
         case Get (M.Attributes, J).Kind is
            when Attr_Deprecated =>
               --  Do not skip deprecated methods in JNI mode.
               --  Otherwise, it can break the type hierarchy.
               --  This is the case, for instance, in java.awt.BorderLayout
               --  which has a deprecated addLayoutComponent method but
               --  which is required because it is a method inherited from
               --  the implemented interface java.awt.LayoutManager2
               if Generation_Mode = JGNAT then
                  return False;
               end if;
            when Attr_Synthetic  =>
               return False;
            when others =>
               null;
         end case;
      end loop;

      K := Descriptor'First;
      while K < Descriptor'Last loop
         case Descriptor (K) is
            when JVM_Class  =>
               K := K + 1;
               Start_Class := K;

               while Descriptor (K) /= ';' loop
                  K := K + 1;
                  if K > Descriptor'Last then
                     Osint.Fail ("Bad type descriptor: " & Descriptor);
                  end if;
               end loop;
               if
                 not Get_Info (Descriptor (Start_Class .. K - 1)).Is_Public
               then
                  return False;
               end if;

            when others =>
               K := K + 1;
         end case;
      end loop;

      return True;
   end Is_Public_Member;

   ----------------------------
   -- Contains_Public_Method --
   ----------------------------

   function Contains_Public_Method
     (CF : Class_File; Method_Name : String) return Boolean is
      use J_Basics;

      T : constant CP.Table := CF.Constant_Pool;
      M : Member_Info;
   begin
      for K in 0 .. Member.Last (CF.Methods) loop
         M := Member.Get (CF.Methods, K);

         if Is_Public_Member (M, T) and then
           Ada_Identifier (To_String (Get_Utf8 (T, M.Name_Index))) =
           Method_Name
         then
            return True;
         end if;
      end loop;
      return False;
   end Contains_Public_Method;

   --------------------------
   -- Get_Class_Identifier --
   --------------------------

   function Get_Class_Identifier (CF : Class_File) return String is
      Class_Name : constant String := Get_String (CF, CF.This_Class);
      Short_Name : constant String :=
                     Get_Identifier (Class_Name, Short => True);
   begin
      if Contains_Public_Method (CF, Short_Name) then
         --  Add "_K" suffix to avoid conflicts with public methods
         return Short_Name & "_K";
      else
         return Short_Name;
      end if;
   end Get_Class_Identifier;

   ------------------
   -- Pretty_Print --
   ------------------

   package body Pretty_Print is

      procedure Close_Spec_File;
      procedure Close_Body_File;

      function Current_Output_File
        return not null access Ada.Text_IO.File_Type;

      ---------------------
      -- Close_Spec_File --
      ---------------------

      procedure Close_Spec_File is
      begin
         Ada.Text_IO.Close (Spec_Output_File);
      end Close_Spec_File;

      ---------------------
      -- Close_Body_File --
      ---------------------

      procedure Close_Body_File is
      begin
         Ada.Text_IO.Close (Body_Output_File);
      end Close_Body_File;

      -------------------------
      -- Current_Output_File --
      -------------------------

      function Current_Output_File
        return not null access Ada.Text_IO.File_Type is
      begin
         case Current_Output is
            when Spec_File => return Spec_Output_File'Access;
            when Body_File => return Body_Output_File'Access;
         end case;
      end Current_Output_File;

      ----------------
      -- Close_File --
      ----------------

      procedure Close_File is
      begin
         Ada.Text_IO.Close (Spec_Output_File);
         if Generation_Mode = JNI and not Spec_Only then
            Ada.Text_IO.Close (Body_Output_File);
         end if;
      end Close_File;

      --------------------
      -- Current_Column --
      --------------------

      function Current_Column return Ada.Text_IO.Count is
      begin
         return Ada.Text_IO.Col (Current_Output_File.all);
      end Current_Column;

      -----------------
      -- Incr_Indent --
      -----------------

      procedure Incr_Indent (Step : Integer) is
      begin
         Current_Indentation := Current_Indentation + Step * 3;
      end Incr_Indent;

      ----------------
      -- Set_Tmp_Indent --
      ----------------

      procedure Set_Tmp_Indent (Value : Ada.Text_IO.Count) is
      begin
         Ada.Text_IO.Set_Col
           (Current_Output_File.all, Ada.Text_IO.Positive_Count (Value));
      end Set_Tmp_Indent;

      ----------------
      -- Get_Indent --
      ----------------

      function Get_Indent return Ada.Text_IO.Count is
      begin
         return Ada.Text_IO.Count (Current_Indentation);
      end Get_Indent;

      ---------------
      -- Open_File --
      ---------------

      function Open_File
        (Class_Name  : String; Spec_Only : Boolean := True) return Boolean
      is
         use Ada.Text_IO;

         File : String := Class_Name;
         Len  : constant Natural := File'Length;

      begin
         Pretty_Print.Spec_Only := Spec_Only;
         --  Store this state to avoid error when closing files

         --  Convert the Class_Name into a file name
         for Index in File'Range loop
            if File (Index) = '.'
              or else File (Index) = '/'
              or else File (Index) = '\'
              or else File (Index) = '$'
            then
               File (Index) := '-';
            end if;
         end loop;

         --  Convert the package name to a file name.

         for K in File'Range loop
            if Osint.Is_Directory_Separator (File (K)) then
               File (K) := '-';
            else
               File (K) := To_Lower (File (K));
            end if;
         end loop;

         --  If there is already an open output file (from a previous class)
         --  we just close it.

         if Ada.Text_IO.Is_Open (Spec_Output_File) then
            Close_Spec_File;
         end if;

         if Generation_Mode = JNI and then not Spec_Only and then
           Ada.Text_IO.Is_Open (Body_Output_File)
         then
            Close_Body_File;
         end if;

         declare
            S : constant String := Output_Dir.all
              & File (File'First .. File'First + Len - 1);

            Spec : constant String := S & Get_File_Extension (Spec_File);
            Bod : constant String := S & Get_File_Extension (Body_File);
         begin
            --  Do not allow overwritting a file, unless the user specified
            --  it is ok to do so.

            if GNAT.IO_Aux.File_Exists (Spec) and then not Overwrite_Files then
               Put_Line
                 ("*** Can't create " & Spec & ": file exists - skipping "
                  & Class_Name);
               return False;
            else
               begin
                  Ada.Text_IO.Create (Spec_Output_File, Name => Spec);
               exception
                  when others =>
                     Put_Line
                       ("*** Can't create " & Spec & ": file error - skipping "
                        & Class_Name);
                     return False;
               end;
            end if;

            Current_Output := Spec_File;

            if Generation_Mode = JGNAT or else Spec_Only then
               return True;
            end if;

            if GNAT.IO_Aux.File_Exists (Bod) and then not Overwrite_Files then
               Put_Line
                 ("*** Can't create " & Bod & ": file exists - skipping "
                  & Class_Name);
               return False;
            else
               begin
                  Ada.Text_IO.Create (Body_Output_File, Name => Bod);
               exception
                  when others =>
                     Put_Line
                       ("*** Can't create " & Bod & ": file error - skipping "
                        & Class_Name);
                     return False;
               end;
            end if;
         end;
         return True;
      end Open_File;

      -------------------
      -- Select_Output --
      -------------------

      procedure Select_Output (Output : Ada_File_Type) is
      begin
         if Generation_Mode /= JGNAT then
            Current_Output := Output;
         end if;
      end Select_Output;

      -----------
      -- Print --
      -----------

      procedure Print (S : String) is
      begin
         Ada.Text_IO.Put (Current_Output_File.all, S);
      end Print;

      ----------------
      -- Print_Line --
      ----------------

      procedure Print_Line (S : String := "") is
      begin
         Ada.Text_IO.Put_Line (Current_Output_File.all, S);
         Ada.Text_IO.Set_Col
           (Current_Output_File.all,
            Ada.Text_IO.Positive_Count (Current_Indentation));
      end Print_Line;

      ----------------------
      -- Print_Empty_Line --
      ----------------------

      procedure Print_Empty_Line (S : String := "") is
      begin
         Ada.Text_IO.Put_Line (Current_Output_File.all, S);
      end Print_Empty_Line;

   end Pretty_Print;

   ----------------------
   -- Get_Array_Suffix --
   ----------------------

   function Get_Array_Suffix (Dim : Natural) return String is
      S : constant String := Positive'Image (Dim);
   begin
      if Dim = 0 then
         if Generation_Mode = JGNAT then
            return "_";
         else
            return "";
         end if;
      elsif Dim = 1 then
         return Array_Suffix;
      else
         --  Since Natural is always >= 0, there is always a trailing space
         --  in the result of 'Image
         return Array_Suffix & '_' & S (S'First + 1);
      end if;
   end Get_Array_Suffix;

   --------------------------------
   -- Print_Get_Length_Signature --
   --------------------------------

   procedure Print_Get_Length_Signature is
   begin
      PL ("function Get_Length (" & Array_String & " : access " & Root_Array &
          ')');
      P ("                     return Standard.Java_Primitives.Int");
   end Print_Get_Length_Signature;

   -------------------------------------
   -- Print_Get_Length_Implementation --
   -------------------------------------

   procedure Print_Get_Length_Implementation is
   begin
      Print_Box ("Get_Length");
      PL;
      Print_Get_Length_Signature;
      PL;
      Pretty_Print.Incr_Indent (1);
      PL ("is");
      Print_Get_Env_Variables;
      Pretty_Print.Incr_Indent (-1);
      PL;
      Pretty_Print.Incr_Indent (1);
      PL ("begin");
      Print_Get_Env;
      PL;
      Pretty_Print.Incr_Indent (-1);
      PL ("return Get_Array_Length (Env, J_Array (Arr.Get_J_Object));");
      PEL ("end Get_Length;");
   end Print_Get_Length_Implementation;

   -----------------------------
   -- Print_Array_Declaration --
   -----------------------------

   procedure Print_Array_Declaration
     (Short_Id    : String;
      Long_Id     : String;
      Dim         : Positive;
      Is_Abstract : Boolean) is
   begin
      --  Print the type declaration.

      PL ("type " & Type_String & Get_Array_Suffix (Dim) &
          " is abstract new " &
          Get_Identifier ("java/lang/Object", Standard_Prefix => True) &
          '.' & Root_Array &
          " with private;");
      PEL ("type " & Short_Id &
           Get_Array_Suffix (Dim) & " is access all " & Type_String &
           Get_Array_Suffix (Dim) & "'Class;");

      --  Print the primitive methods.

      PL;
      Print_Array_Constructor_Signature (Short_Id, Long_Id, Dim);
      PEL (";");

      PL;
      Print_Get_Array_Element_Signature (Short_Id, Dim,
                                         Is_Abstract => Is_Abstract);
      PEL (";");

      PL;
      Print_Set_Array_Element_Signature (Short_Id, Long_Id, Dim);
      PEL (";");
   end Print_Array_Declaration;

   ---------------------------------------
   -- Print_Array_Constructor_Signature --
   ---------------------------------------

   procedure Print_Array_Constructor_Signature
     (Short_Id : String;
      Long_Id  : String;
      Dim      : Positive)
   is
      Start_Col : Ada.Text_IO.Positive_Count := 1;
   begin
      P ("function New_" & Short_Id & Get_Array_Suffix (Dim) & " (");
      Start_Col := Pretty_Print.Current_Column;
      PL (Length_String & "        : Standard.Java_Primitives.Int;");
      Pretty_Print.Set_Tmp_Indent (Start_Col);
      PL (Default_Value_String & " : " & "Standard." & Long_Id &
          Get_Array_Suffix (Dim - 1) & " := null)");
      --  Use a Long_Id to avoid name conflicts like it occured in
      --  Sun.Awt.SunHints.Value.

      Pretty_Print.Set_Tmp_Indent (Start_Col);
      P ("return " & Short_Id & Get_Array_Suffix (Dim));
   end Print_Array_Constructor_Signature;

   ---------------------------------------
   -- Print_Get_Array_Element_Signature --
   ---------------------------------------

   procedure Print_Get_Array_Element_Signature
     (Short_Id    : String;
      Dim         : Positive;
      Is_Abstract : Boolean)
   is
      Start_Col  : Ada.Text_IO.Positive_Count := 1;
      Element_Id : constant String := Short_Id & Get_Array_Suffix (Dim - 1);
   begin
      P ("function Get_" & Element_Id & " (");
      Start_Col := Pretty_Print.Current_Column;

      PL (Array_String & " : access " &
          Type_String & Get_Array_Suffix (Dim) & ';');

      Pretty_Print.Set_Tmp_Indent (Start_Col);
      PL ("Pos : Standard.Java_Primitives.Int)");

      Pretty_Print.Set_Tmp_Indent (Start_Col);
      if Is_Abstract then
         --  TODO: could be improved

         --  We should return Element_Id as in the general case.
         --  However, the value returned by Get_Object_Array_Element is
         --  a JNI J_Object. We don't easily know the concrete type of
         --  the returned object and since we can't allocate a object of
         --  an abstract type, the easy solution is to return an Object.
         --
         --  A more complex solution would be to use the information
         --  returned by the getClass method on the J_Object and then
         --  create the right Ada type on the fly. (maybe using a map
         --  between class names and class instance object. The map
         --  could be initialized during the elaboration for instance)

         P ("return Standard.Java.Lang.Object.Object");
      else
         P ("return " & Element_Id);
      end if;
   end Print_Get_Array_Element_Signature;

   ---------------------------------------
   -- Print_Set_Array_Element_Signature --
   ---------------------------------------

   procedure Print_Set_Array_Element_Signature
     (Short_Id : String;
      Long_Id  : String;
      Dim      : Positive)
   is
      Start_Col  : Ada.Text_IO.Positive_Count := 1;
      Short_Element_Id : constant String :=
                           Short_Id & Get_Array_Suffix (Dim - 1);
      Long_Element_Id  : constant String :=
                           Long_Id & Get_Array_Suffix (Dim - 1);
      --  Use a Long_Id to avoid name conflicts like it occured in
      --  Sun.Awt.SunHints.Value.
   begin
      P ("procedure Set_" & Short_Element_Id & " (");
      Start_Col := Pretty_Print.Current_Column;

      PL (Array_String & "   : access " &
          Type_String & Get_Array_Suffix (Dim) & ';');

      Pretty_Print.Set_Tmp_Indent (Start_Col);
      PL ("Pos   : Standard.Java_Primitives.Int;");

      Pretty_Print.Set_Tmp_Indent (Start_Col);
      P (Value_String & " : " & "Standard." & Long_Element_Id & ")");
   end Print_Set_Array_Element_Signature;

   -------------------------------------
   -- Print_Private_Array_Declaration --
   -------------------------------------

   procedure Print_Private_Array_Declaration (Dim : Positive) is
   begin
      PEL ("type " & Type_String & Get_Array_Suffix (Dim) & " is new " &
           Get_Identifier ("java/lang/Object", Standard_Prefix => True) &
           '.' & Root_Array &
           " with null record;");
      PL;
      P ("function " & Constructor_String &
         " (Params : not null access Parameters) " &
         "return " & Type_String & Get_Array_Suffix (Dim) & ';');
      PEL;
   end Print_Private_Array_Declaration;

   -----------------------------
   -- Print_Array_Method_Body --
   -----------------------------

   procedure Print_Array_Method_Body
     (Class_Name  : String;
      Short_Id    : String;
      Dim         : Positive;
      Is_Abstract : Boolean)
   is
      Element_Id           : constant String :=
                               Short_Id & Get_Array_Suffix (Dim - 1);
      Array_Suffix_String  : constant String := Get_Array_Suffix (Dim);
      New_Function_String  : constant String :=
                               "New_" & Short_Id & Array_Suffix_String;
      Get_Function_String  : constant String := "Get_" & Element_Id;
      Set_Procedure_String : constant String := "Set_" & Element_Id;
      Long_Id              : constant String :=
                               Get_Identifier (Class_Name) & '.' & Short_Id;
   begin
      PL;
      Print_Box (New_Function_String);
      PL;
      Print_Array_Constructor_Signature (Short_Id, Long_Id, Dim);
      PL;
      Pretty_Print.Incr_Indent (1);
      PL ("is");
      Print_Get_Env_Variables;
      PL ("J_Obj_Arr  : J_Object_Array;");
      --  The class to create depends on the dimension of the array
      if Dim > 1 then
         P (Class_Name_String & " : Standard.String := """);
         for I in 1 .. Dim - 1 loop
            P ("[");
         end loop;
         PL ("L" & Class_Name & ";"";");
         PL (Class_String & "   : J_Class;");
      end if;
      Pretty_Print.Incr_Indent (-1);
      PL ("Result     : " & Short_Id & Array_Suffix_String &
          " := new " & Type_String & Array_Suffix_String & ';');
      Pretty_Print.Incr_Indent (1);
      PL ("begin");
      Print_Get_Env;

      PL;
      PL ("--  Check that the class is set");
      --  For Dim = 1, we create an array of the current class
      if Dim = 1 then
         PL ("if " & Class_String & " = J_Null_Class then ");
         PL ("   Set_Class (Env);");
         PEL ("end if;");
      else
         PL (Class_String & " := Find_Class (Env, " & Class_Name_String &
             ");");
         PL ("if " & Class_String & " = J_Null_Class then");
         PL ("   raise Program_Error;");
         PEL ("end if;");
      end if;

      PL;
      PL ("--  Call the constructor");
      PL ("J_Obj_Arr := New_Object_Array (Env, " & Length_String & ", " &
          Class_String & ',');
      PL ("                               Secure_Get_J_Object (" &
          Default_Value_String & "));");
      PL ("if J_Obj_Arr = J_Null_Object_Array then");
      PL ("   raise Program_Error;");
      PEL ("end if;");
      PL;
      PL ("Result.Set_J_Object (J_Object (J_Obj_Arr));");
      Pretty_Print.Incr_Indent (-1);
      PL ("return Result;");
      PEL ("end " & New_Function_String & ';');
      PL;

      --  Get Element function.
      Print_Box (Get_Function_String);
      PL;
      Print_Get_Array_Element_Signature (Short_Id, Dim, Is_Abstract);
      PL;
      Pretty_Print.Incr_Indent (1);
      PL ("is");
      Print_Get_Env_Variables;
      PL ("J_Obj  : J_Object;");
      Pretty_Print.Incr_Indent (-1);
      if Is_Abstract then
         --  We can't allocate an abstract object.
         --  For more information, have a look at the comment in
         --  Print_Get_Array_Element_Signature implementation.
         PL ("Result : Standard.Java.Lang.Object.Object;");
      else
         PL ("Result : " & Element_Id & ';');
      end if;
      Pretty_Print.Incr_Indent (1);
      PL ("begin");
      Print_Get_Env;
      PL;
      PL ("J_Obj := Get_Object_Array_Element (Env," &
          " J_Array (Arr.Get_J_Object),");
      PL ("                                   Pos);");
      if Is_Abstract then
         --  We can't allocate an abstract object so we create a new Object.
         --  This is valid (and type safe) since all concrete objects are
         --  descendant of Object.
         --  For more information, have a look at the comment in
         --  Print_Get_Array_Element_Signature implementation.
         PL ("Result := New_Object;");
      else
         PL ("Result := new " &
             Type_String & Get_Array_Suffix (Dim - 1) & ';');
      end if;
      PL ("Result.Set_J_Object (J_Obj);");
      Pretty_Print.Incr_Indent (-1);
      PL ("return Result;");
      PEL ("end " & Get_Function_String & ';');
      PL;

      --  Set Element function.
      Print_Box (Set_Procedure_String);
      PL;
      Print_Set_Array_Element_Signature (Short_Id, Long_Id, Dim);
      PL;
      Pretty_Print.Incr_Indent (1);
      PL ("is");
      Print_Get_Env_Variables;
      Pretty_Print.Incr_Indent (-1);
      PL;
      Pretty_Print.Incr_Indent (1);
      PL ("begin");
      Print_Get_Env;
      PL;
      PL ("Set_Object_Array_Element (Env, J_Array (Arr.Get_J_Object),");
      Pretty_Print.Incr_Indent (-1);
      PL ("                          Pos, Secure_Get_J_Object (" &
          Value_String & "));");
      PEL ("end " & Set_Procedure_String & ';');
      PL;

      --  Constructor function
      Print_Box (Constructor_String);
      PL;
      PL ("function " & Constructor_String &
          " (Params : not null access Parameters) " &
          "return " & Type_String & Array_Suffix_String);
      PL ("is");
      Pretty_Print.Incr_Indent (+1);
      PL ("begin");
      Pretty_Print.Incr_Indent (-1);
      PL ("return (Root_Array with null record);");
      PEL ("end " & Constructor_String & ';');
   end Print_Array_Method_Body;

   -----------------------------
   -- Print_Get_Env_Variables --
   -----------------------------

   procedure Print_Get_Env_Variables is
   begin
      PL ("Env : aliased " & Env_Type_String & ';');
      PL ("I   : J_Int;");
   end Print_Get_Env_Variables;

   -------------------
   -- Print_Get_Env --
   -------------------

   procedure Print_Get_Env is
   begin
      PL  ("--  Get the Environment");
      PL  ("I := Get_Env");
      PL  ("       (VM   => Current_VM,");
      PEL ("        Penv => Env'Access);");
      PL;
      Pretty_Print.Incr_Indent (+1);
      PL ("if I /= JNI_OK then");
      Pretty_Print.Incr_Indent (-1);
      PL ("raise Program_Error;");
      PEL ("end if;");
   end Print_Get_Env;

   -----------------------
   -- Print_Check_Class --
   -----------------------

   procedure Print_Check_Class is
   begin
      PL ("--  Check that the Class is set");
      Pretty_Print.Incr_Indent (+1);
      PL ("if " & Class_String & " = J_Null_Class then");
      Pretty_Print.Incr_Indent (-1);
      PL ("Set_Class (Env);");
      PEL ("end if;");
   end Print_Check_Class;

   ---------------------------
   -- Print_Check_Method_ID --
   ---------------------------

   procedure Print_Check_Method_ID
     (ID         : String;
      Descriptor : String;
      Is_Static  : Boolean)
   is
      Col : Ada.Text_IO.Positive_Count := 1;
   begin
      PL ("--  Check that the method ID is set");
      Pretty_Print.Incr_Indent (+1);
      PL ("if " & ID & " = J_Null_Method_Id then");
      P  (ID & " := Get_");
      if Is_Static then
         P ("Static_");
      end if;
      P ("Method_ID (");
      Col := Pretty_Print.Current_Column;
      PL ("Env, " & Class_String & ",");
      Pretty_Print.Set_Tmp_Indent (Col);
      PL ("Method_Name,");
      Pretty_Print.Set_Tmp_Indent (Col);
      PL ("""" & Descriptor & """);");
      PL;
      PL ("--  TODO: Should throw a Java exception instead");
      Pretty_Print.Incr_Indent (+1);
      PL ("if " & ID & " = J_Null_Method_Id then");
      Pretty_Print.Incr_Indent (-1);
      PL ("raise Program_Error;");
      Pretty_Print.Incr_Indent (-1);
      PL ("end if;");
      PEL ("end if;");
   end Print_Check_Method_ID;

   --------------------------
   -- Print_Check_Field_ID --
   --------------------------

   procedure Print_Check_Field_ID
     (ID         : String;
      Descriptor : String;
      Is_Static  : Boolean)
   is
      Col : Ada.Text_IO.Positive_Count := 1;
   begin
      PL ("--  Check that the field ID is set");
      Pretty_Print.Incr_Indent (+1);
      PL ("if " & ID & " = J_Null_Field_Id then");
      P  (ID & " := Get_");
      if Is_Static then
         P ("Static_");
      end if;
      P ("Field_ID (");
      Col := Pretty_Print.Current_Column;
      PL ("Env, " & Class_String & ",");
      Pretty_Print.Set_Tmp_Indent (Col);
      PL ("Field_Name,");
      Pretty_Print.Set_Tmp_Indent (Col);
      PL ("""" & Descriptor & """);");
      PL;
      PL ("--  TODO: Should throw a Java exception instead");
      Pretty_Print.Incr_Indent (+1);
      PL ("if " & ID & " = J_Null_Field_Id then");
      Pretty_Print.Incr_Indent (-1);
      PL ("raise Program_Error;");
      Pretty_Print.Incr_Indent (-1);
      PL ("end if;");
      PEL ("end if;");
   end Print_Check_Field_ID;

   ----------------
   -- Get_Prefix --
   ----------------

   function Get_Prefix (Kind : Accessor) return String is
   begin
      case Kind is
         when Set => return "Set_";
         when Get => return "Get_";
      end case;
   end Get_Prefix;

   -----------------------
   -- Field_Method_Name --
   -----------------------

   function Field_Method_Name
     (Descriptor : String;
      Is_Static  : Boolean;
      Kind       : Accessor)
      return String is
   begin
      if Is_Static then
         return Get_Prefix (Kind) & "Static_" &
           Get_Java_Type (Descriptor) & "_Field";
      else
         return Get_Prefix (Kind) & Get_Java_Type (Descriptor) & "_Field";
      end if;
   end Field_Method_Name;

   ----------------------
   -- Call_Method_Name --
   ----------------------

   function Call_Method_Name
     (Descriptor : String; Is_Static : Boolean) return String
   is
      Java_Type : constant String := Get_Java_Type (Descriptor);
   begin
      if Is_Static then
         return "Call_Static_" & Java_Type & "_Method_A";
      else
         return "Call_" & Java_Type & "_Method_A";
      end if;
   end Call_Method_Name;

   -----------------------
   -- Is_Constant_Field --
   -----------------------

   function Is_Constant_Field (F : Member_Info) return Boolean is
      use Member_Attribute;
   begin
      for K in 0 .. Member_Attribute.Last (F.Attributes) loop
         if Get (F.Attributes, K).Kind = Attr_Constant_Value then
            return True;
         end if;
      end loop;
      return False;
   end Is_Constant_Field;

   ---------------------------
   -- Print_Field_Accessors --
   ---------------------------

   procedure Print_Field_Accessors (CF : Class_File) is
      use J_Basics;
      use String_Natural_Map;

      T           : constant CP.Table := CF.Constant_Pool;
      F           : Member_Info;
      Start_Col   : Ada.Text_IO.Positive_Count := 1;
      Box_Printed : Boolean := False;

   begin
      for K in 0 .. Member.Last (CF.Fields) loop
         F := Member.Get (CF.Fields, K);

         if Is_Public_Member (F, T) then
            declare
               F_Name       : constant String :=
                 To_String (Get_Utf8 (T, F.Name_Index));
               Get_Name   : constant String :=
                 "Get_" & Ada_Identifier (F_Name);
               U_Get_Name : constant String := To_Upper (Get_Name);

               Descriptor   : constant String :=
                 To_String (Get_Utf8 (T, F.Descriptor_Index));

               Is_Protected : constant Boolean :=
                 Is_Set (F.Access_Flags, ACC_Protected);
               Is_Final     : constant Boolean :=
                 Is_Set (F.Access_Flags, ACC_Final);
               Is_Static    : constant Boolean :=
                 Is_Set (F.Access_Flags, ACC_Static);

            begin
               if not Box_Printed then
                  PL;
                  Print_Box ("Field Accessors");
                  Box_Printed := True;
               end if;

               PL;

               if Is_Protected then
                  PL ("--  protected");
               end if;

               P ("function " & Get_Name & ' ');

               if Used_Names.Find (U_Get_Name) =
                 String_Natural_Map.No_Element
               then
                  Used_Names.Insert (U_Get_Name, 1);
               end if;

               if not Is_Static then
                  P ("(");
                  Start_Col := Pretty_Print.Current_Column;
                  PL ("This : access " & Type_String & ")");
                  Pretty_Print.Set_Tmp_Indent (Start_Col);
               end if;

               PEL ("return " & Ada_Type (Descriptor) & ';');

               if not Is_Final and then not Is_Constant_Field (F) then
                  declare
                     Set_Name   : constant String :=
                       "Set_" & Ada_Identifier (F_Name);
                     U_Set_Name : constant String := To_Upper (Set_Name);
                  begin

                     PL;
                     P ("procedure " & Set_Name & " (");

                     if Used_Names.Find (U_Set_Name) =
                       String_Natural_Map.No_Element
                     then
                        Used_Names.Insert (U_Set_Name, 1);
                     end if;

                     if not Is_Static then
                        Start_Col := Pretty_Print.Current_Column;
                        PL ("This : access " & Type_String & "; ");
                        Pretty_Print.Set_Tmp_Indent (Start_Col);
                     end if;

                     PEL ("F : " &
                          Ada_Type (Descriptor, Format => Parameter_Type) &
                          ");");
                  end;
               end if;

            end;
         end if;
      end loop;
   end Print_Field_Accessors;

   -------------------------------
   -- Print_Field_Accessor_Body --
   -------------------------------

   procedure Print_Field_Accessor_Body (CF : Class_File) is
      use J_Basics;

      T : constant CP.Table := CF.Constant_Pool;
      F : Member_Info;

      Start_Col   : Ada.Text_IO.Positive_Count := 1;
   begin
      for K in 0 .. Member.Last (CF.Fields) loop
         F := Member.Get (CF.Fields, K);

         if Is_Public_Member (F, T) then
            declare
               F_Name            : constant String :=
                                     To_String (Get_Utf8 (T, F.Name_Index));
               Get_Name          : constant String :=
                                     "Get_" & Ada_Identifier (F_Name);
               Get_Name_ID       : constant String :=
                                     Subprogram_ID_Name (Get_Name);
               Descriptor        : constant String :=
                                     To_String
                                       (Get_Utf8 (T, F.Descriptor_Index));
               Ada_Type_Name     : constant String :=
                                     Ada_Type (Descriptor);
               Is_Final          : constant Boolean :=
                                     Is_Set (F.Access_Flags, ACC_Final);
               Is_Static         : constant Boolean :=
                                     Is_Set (F.Access_Flags, ACC_Static);
               Is_Reference_Type : constant Boolean :=
                                     Get_Java_Type (Descriptor) = "Object";

            begin

               PL;
               PEL (Get_Name_ID & " : J_Field_ID := J_Null_Field_ID;");
               PL;
               Print_Box (Get_Name);
               PL;

               P ("function " & Get_Name & ' ');

               if not Is_Static then
                  P ("(");
                  Start_Col := Pretty_Print.Current_Column;
                  PL ("This : access " & Type_String & ")");
                  Pretty_Print.Set_Tmp_Indent (Start_Col);
               end if;

               PL ("return " & Ada_Type_Name);
               Pretty_Print.Incr_Indent (+1);
               PL ("is");
               Print_Get_Env_Variables;
               Pretty_Print.Incr_Indent (-1);
               PL ("Field_Name : Standard.String := """ & F_Name & """;");
               Pretty_Print.Incr_Indent (+1);
               PL ("begin");

               Print_Get_Env;
               PL;
               Print_Check_Class;
               PL;
               Print_Check_Field_ID (Get_Name_ID, Descriptor,
                                     Is_Static => Is_Static);

               PL;

               --  TODO: Add some checks ??

               P ("return ");

               if Is_Reference_Type then
                  P ("new " & Ada_Type (Descriptor, Conversion_Type)
                     & "'Class'(" & Ada_Type (Descriptor, Conversion_Type)
                     & "'Class (Create_Ada_Object (");
               end if;

               P (Field_Method_Name (Descriptor, Is_Static, Get) &
                  " (Env, ");
               if Is_Static then
                  P (Class_String);
               else
                  P ("This.Get_J_Object");
               end if;
               P (", " & Get_Name_ID & ")");

               if Is_Reference_Type then
                  PL (").all));");
                  --  TODO: This is to workaround a current limitation of GNAT.
               else
                  P (";");
               end if;

               Pretty_Print.Incr_Indent (-1);
               PL;

               PEL ("end " & Get_Name & ';');

               if not Is_Final and then not Is_Constant_Field (F) then
                  declare
                     Set_Name     : constant String :=
                       "Set_" & Ada_Identifier (F_Name);
                     Set_Name_ID   : constant String :=
                       Subprogram_ID_Name (Set_Name);
                  begin
                     PL;
                     PEL (Set_Name_ID & " : J_Field_ID := J_Null_Field_ID;");
                     PL;
                     Print_Box (Set_Name);
                     PL;

                     P ("procedure " & Set_Name & " (");

                     if not Is_Static then
                        Start_Col := Pretty_Print.Current_Column;
                        PL ("This : access " & Type_String & "; ");
                        Pretty_Print.Set_Tmp_Indent (Start_Col);
                     end if;

                     PL ("F : " &
                         Ada_Type (Descriptor, Format => Parameter_Type) &
                         ")");
                     Pretty_Print.Incr_Indent (+1);
                     PL ("is");
                     Print_Get_Env_Variables;
                     Pretty_Print.Incr_Indent (-1);
                     PL ("Field_Name : Standard.String := """ & F_Name &
                         """;");
                     Pretty_Print.Incr_Indent (+1);
                     PL ("begin");
                     Print_Get_Env;
                     PL;
                     Print_Check_Class;
                     PL;
                     Print_Check_Field_ID (Set_Name_ID, Descriptor,
                                           Is_Static => Is_Static);

                     PL;
                     P (Field_Method_Name (Descriptor, Is_Static, Set) &
                        " (Env, ");
                     if Is_Static then
                        P (Class_String);
                     else
                        P ("This.Get_J_Object");
                     end if;
                     P (", " & Get_Name_ID);
                     if Is_Reference_Type then
                        PL (", Secure_Get_J_Object (F));");
                     else
                        PL (", F);");
                     end if;

                     Pretty_Print.Incr_Indent (-1);
                     PL;
                     PEL ("end " & Set_Name & ';');
                  end;
               end if;

            end;
         end if;
      end loop;
   end Print_Field_Accessor_Body;

   ------------------
   -- Print_Fields --
   ------------------

   procedure Print_Fields
     (CF            : Class_File;
      Static_Fields : Boolean;
      Pragma_List   : in out String_List.List)
   is
      use J_Basics;
      use Member_Attribute;

      function Field_Name (F_Name : String) return String;
      --  Given the original field name F_Name returns the Ada identifier to
      --  use.

      function Is_Constant_Field (F : Member_Info) return Boolean;
      --  Returns True if field F is a constant.

      ----------------
      -- Field_Name --
      ----------------

      function Field_Name (F_Name : String) return String is
         use String_Natural_Map;

         Ada_Name   : constant String := Ada_Identifier (F_Name);
         Ada_Name_U : constant String := To_Upper (Ada_Name);
         Nb_Clashes : Integer;
         C          : Cursor;
      begin
         if not Static_Fields or else Keep_Original_Identifiers then
            return Ada_Name;
         end if;

         C := Used_Names.Find (Ada_Name_U);

         if C = String_Natural_Map.No_Element then
            Used_Names.Insert (Ada_Name_U, 1);
            return Ada_Name;
         else
            Nb_Clashes := Element (C);

            if Nb_Clashes = 1 then
               Used_Names.Replace_Element (C, 2);
               return Ada_Name & "_K";
            else
               Used_Names.Replace_Element (C, Nb_Clashes + 1);
               return Ada_Name & "_K" & Image (U4 (Nb_Clashes));
            end if;
         end if;
      end Field_Name;

      -----------------------
      -- Is_Constant_Field --
      -----------------------

      function Is_Constant_Field (F : Member_Info) return Boolean is
      begin
         for K in 0 .. Member_Attribute.Last (F.Attributes) loop
            if Get (F.Attributes, K).Kind = Attr_Constant_Value then
               return True;
            end if;
         end loop;
         return False;
      end Is_Constant_Field;

      T : constant CP.Table := CF.Constant_Pool;
      F : Member_Info;

      Field_Printed : Boolean := False;
      --  Set to True if we printed at least one field declaration

   --  Beginning of Print_Fields

   begin
      for K in 0 .. Member.Last (CF.Fields) loop
         F := Member.Get (CF.Fields, K);

         if Is_Set (F.Access_Flags, ACC_Static) = Static_Fields
           and then Is_Public_Member (F, T)
         then
            if not Field_Printed then
               if Static_Fields then
                  PL;
                  Print_Box ("Variable Declarations");
               else
                  Pretty_Print.Incr_Indent (1);
                  PL (" with record");
                  PL;
                  Print_Box ("Field Declarations");
               end if;
            end if;

            Field_Printed := True;

            declare
               F_Name       : constant String :=
                 To_String (Get_Utf8 (T, F.Name_Index));
               Ada_F_Name   : constant String := Field_Name (F_Name);
               F_Descriptor : constant String :=
                 To_String (Get_Utf8 (T, F.Descriptor_Index));

               Is_Protected : constant Boolean :=
                 Is_Set (F.Access_Flags, ACC_Protected);
               Is_Final     : constant Boolean :=
                 Is_Set (F.Access_Flags, ACC_Final);

            begin
               if Is_Protected or else Is_Final then
                  PL;
                  P ("--");

                  if Is_Protected then
                     P ("  protected");
                  end if;

                  if Is_Final then
                     P ("  final");
                  end if;
               end if;

               PL;
               P (Ada_F_Name);
               P (" : ");

               if Is_Constant_Field (F) and then Static_Fields then
                  P ("constant ");
               end if;

               P (Ada_Type (F_Descriptor));
               P (";");

               if not Static_Fields then
                  PL;
                  PEL ("pragma Import (Java, " & Ada_F_Name & ", """
                      & F_Name & """);");
               else
                  PEL;
                  String_List.Append_If_Uniq
                    ("pragma Import (Java, " & Ada_F_Name & ", """
                     & F_Name & """);",
                     Pragma_List);
               end if;
            end;
         end if;
      end loop;

      if not Static_Fields then
         if not Field_Printed then
            Pretty_Print.Incr_Indent (1);
            PL;
            P ("with null record;");
            Pretty_Print.Incr_Indent (-1);
            PEL;
         else
            Pretty_Print.Incr_Indent (-1);
            PL;
            PEL ("end record;");
         end if;
      end if;
   end Print_Fields;

   --------------------------
   -- Print_JNI_Conversion --
   --------------------------

   function Print_JNI_Conversion (C : Character) return String is
   begin
      case C is
         when JVM_Byte    =>
            return "J_Byte (";

         when JVM_Char    =>
            return "J_Char (";

         when JVM_Double  =>
            return "J_Double (";

         when JVM_Float   =>
            return "J_Float (";

         when JVM_Int     =>
            return "J_Int (";

         when JVM_Long    =>
            return "J_Long (";

         when JVM_Short   =>
            return "J_Short (";

         when JVM_Boolean =>
            return "J_Boolean (";

         when others =>
            return "Secure_Get_J_Object (";
      end case;
   end Print_JNI_Conversion;

   ------------------
   -- Get_JNI_Type --
   ------------------

   function Get_JNI_Type (C : Character) return String is
   begin
      case C is
         when JVM_Byte    =>
            return "Jbyte";

         when JVM_Char    =>
            return "Jchar";

         when JVM_Double  =>
            return "Jdouble";

         when JVM_Float   =>
            return "Jfloat";

         when JVM_Int     =>
            return "Jint";

         when JVM_Long    =>
            return "Jlong";

         when JVM_Short   =>
            return "Jshort";

         when JVM_Boolean =>
            return "Jboolean";

         when others =>
            return "Jobject";
      end case;
   end Get_JNI_Type;

   --------------------
   -- Get_JNI_Letter --
   --------------------

   function Get_JNI_Letter (C : Character) return Character is
   begin
      case C is
         when JVM_Byte | JVM_Char | JVM_Double | JVM_Float |
           JVM_Int | JVM_Long | JVM_Short | JVM_Boolean =>
            return C;

         when others =>
            return JVM_Class;
      end case;
   end Get_JNI_Letter;

   -------------------
   -- Get_Java_Type --
   -------------------

   function Get_Java_Type (Descriptor : String) return String is
   begin
      if Descriptor'Length > 1 and then
        Descriptor (Descriptor'Last - 1) = '['
      then
         --  This is an array type
         return "Object";
      elsif Descriptor'Length = 1 then
         --  This is a primitive type
         case Descriptor (Descriptor'Last) is
            when JVM_Boolean => return "Boolean";
            when JVM_Byte    => return "Byte";
            when JVM_Char    => return "Char";
            when JVM_Short   => return "Short";
            when JVM_Int     => return "Int";
            when JVM_Long    => return "Long";
            when JVM_Float   => return "Float";
            when JVM_Double  => return "Double";
            when JVM_Void    => return "Void";
            when others =>
               Osint.Fail ("Invalid type descriptor : " & Descriptor);
               return "";
         end case;
      else
         return "Object";
      end if;
   end Get_Java_Type;

   --------------------------------
   -- Get_Return_Type_Descriptor --
   --------------------------------

   function Get_Return_Type_Descriptor (Descriptor : String) return String is
   begin
      return Descriptor (Index (Descriptor, ")") + 1 .. Descriptor'Last);
   end Get_Return_Type_Descriptor;

   -----------------------------
   -- Print_Exception_Comment --
   -----------------------------

   procedure Print_Exception_Comment (CF : Class_File; M : Member_Info) is
      use Member_Attribute;
      use Class_Index;
      use J_Basics;
      use CP;

      T              : constant CP.Table := CF.Constant_Pool;
      CA             : Member_Attribute_Info;
      Exception_Info : CP_Info;
      Nb             : Int_32;
   begin
      for I in 0 .. Last (M.Attributes) loop
         CA := Get (M.Attributes, I);
         if CA.Kind = Attr_Exceptions then
            Nb := Last (CA.Exception_Index_Table);

            if Nb >= 0 then
               PL;
               P ("--  can raise ");
            end if;

            for I in 0 .. Nb loop
               Exception_Info := Get (T, Get (CA.Exception_Index_Table, I));
               if I = Nb and then I /= 0 then
                  PL (" and");
                  P ("--  ");
               elsif I /= 0 then
                  PL (",");
                  P ("--  ");
               end if;
               P (Get_Identifier
                  (To_String (Get_Utf8 (T, Exception_Info.Class_Name_Index))) &
                  '.' & Exception_String);
            end loop;

         end if;
      end loop;
   end Print_Exception_Comment;

   ----------------------------
   -- Print_Method_Signature --
   ----------------------------

   procedure Print_Method_Signature (CF : Class_File; M : Member_Info) is
      use J_Basics;
      use String_Natural_Map;

      T                : constant CP.Table := CF.Constant_Pool;
      Class_Name       : constant String :=
                           Get_String (CF, CF.This_Class);
      Class_Identifier : constant String   := Get_Class_Identifier (CF);

      F    : constant Access_Mask := M.Access_Flags;
      Name : constant String := To_String (Get_Utf8 (T, M.Name_Index));
      D    : constant String := To_String (Get_Utf8 (T, M.Descriptor_Index));

      Is_Procedure    : constant Boolean := D (D'Last) = 'V';
      Is_Constructor  : constant Boolean := Name (Name'First) = '<';

      Is_Static       : constant Boolean := Is_Set (F, ACC_Static);
      Is_Abstract     : constant Boolean :=
        Is_Set (CF.Access_Flags, ACC_Interface) or Is_Set (F, ACC_Abstract);

      Has_Parameters : constant Boolean :=
        D (D'First + 1) /= ')' or
        (not Is_Static and then
         not (Generation_Mode = JNI and Is_Constructor));
      --  Constructors when generating for JNI have no parameters
      --  With JGNAT, they have one.

      Current_Pos : Natural := D'First + 1;
      End_Pos     : Natural;
      Param       : U2 := 1;
      Start_Col   : Ada.Text_IO.Positive_Count := 1;

      Need_Semicolon : Boolean := False;

      Subprog_Name     : constant String := Ada_Identifier (Name);
      Constructor_Name : constant String :=
                           "New_" & Get_Identifier
                             (Class_Name, Short => True);

   begin
      if Is_Constructor then
         if Generation_Mode = JGNAT then
            PL;
            P ("function " & Constructor_Name & " (");
         else
            PL;
            P ("function " & Constructor_Name & " ");
            if Has_Parameters then
               P ("(");
            end if;

         end if;

         if Used_Names.Find (To_Upper (Constructor_Name)) = No_Element then
            Used_Names.Insert (To_Upper (Constructor_Name), 1);
         end if;

      else
         if Is_Procedure then
            PL;
            P ("procedure " & Subprog_Name & " ");
         else
            PL;
            P ("function "  & Subprog_Name & " ");
         end if;

         if Used_Names.Find (To_Upper (Subprog_Name)) = No_Element then
            Used_Names.Insert (To_Upper (Subprog_Name), 1);
         end if;

         --  For non-static methods, the first argument is 'This'

         if Has_Parameters then
            P ("(");
         end if;
      end if;

      Start_Col := Pretty_Print.Current_Column;

      if not Is_Static and then not Is_Constructor then
         P ("This : access " & Type_String);
         Need_Semicolon := True;
      end if;

      --  Print the arguments

      while D (Current_Pos) /= ')' loop
         End_Pos := Next_Declaration_Pos (D (Current_Pos .. D'Last));

         if Need_Semicolon then
            PL (";");
            Pretty_Print.Set_Tmp_Indent (Start_Col);
         end if;

         P ("P" & Image (Param) & "_"
            & Ada_Type (D (Current_Pos .. End_Pos - 1),
                        Format => Use_As_Name)
            & " : ");
         P (Ada_Type (D (Current_Pos .. End_Pos - 1),
                      Format => Parameter_Type));

         Param := Param + 1;
         Current_Pos := End_Pos;

         Need_Semicolon := True;
      end loop;

      --  If we had a constructor

      if Is_Constructor then

         if Generation_Mode = JGNAT then

            if Need_Semicolon then
               PL ("; ");
               Pretty_Print.Set_Tmp_Indent (Start_Col);
            end if;

            PL ("This : Ref := null)");
            Pretty_Print.Set_Tmp_Indent (Start_Col);
            P ("return Ref");

         else
            --  Generation_Mode = JNI

            if Has_Parameters then
               PL (")");
               Pretty_Print.Set_Tmp_Indent (Start_Col);
            else
               P (" ");
            end if;

            P ("return " & Class_Identifier);
         end if;

      else
         if Has_Parameters or else not Is_Static then
            P (")");
         end if;

         if not Is_Procedure then
            Pretty_Print.Set_Tmp_Indent (Start_Col);
            P ("return " & Ada_Type (D (Current_Pos + 1 .. D'Last)));
         end if;

         --  Special case for abstract procedures and methods

         if Is_Abstract then
            P (" is abstract");
         end if;

      end if;
   end Print_Method_Signature;

   -------------------
   -- Print_Methods --
   -------------------

   procedure Print_Methods
     (CF          : Class_File;
      Pragma_List : in out String_List.List)
   is
      use Method_Sets;
      use Member_Attribute;
      use J_Basics;

      Class_Name          : constant String :=
                              Get_String (CF, CF.This_Class);
      Is_Java_Lang_Object : constant Boolean :=
                              Class_Name = "java/lang/Object";
      T : CP.Table := CF.Constant_Pool;

      M : Member_Info;
      Public_Method_Set : Method_Sets.Set;
      --  Set that contains the Method_Info structure of all the methods
      --  implemented (or overridden) by the current class

      Constructor_Printed : Boolean := False;
      Method_Printed      : Boolean := False;

   begin
      --  First print the public methods of the current class
      for K in 0 .. Member.Last (CF.Methods) loop
         M := Member.Get (CF.Methods, K);

         if Is_Public_Member (M, T) and then
           (Generation_Mode = JGNAT or else
            not (Is_Constructor (CF, M) and then
                 Is_Abstract_Class (CF)))
         --  In JNI mode, we do not print the constructor of abstract classes
         then
            declare
               Name : constant String_Ptr := new String'
                 (To_String (Get_Utf8 (T, M.Name_Index)));
               Desc : constant String_Ptr := new String'
                 (To_String (Get_Utf8 (T, M.Descriptor_Index)));
               Is_Constructor    : constant Boolean := Name (Name'First) = '<';
               Is_Abstract_Class : constant Boolean :=
                                     Is_Set (CF.Access_Flags, ACC_Abstract);
               Constructor_Name  : constant String :=
                                     "New_" & Get_Identifier
                                       (Class_Name, Short => True);
               Subprog_Name      : constant String :=
                                     Ada_Identifier (Name.all);
            begin
               if Name.all /= To_Upper ("<clinit>") then

                  --  In case two Java methods have signatures that only differ
                  --  from one another by casing, generating code blindly for
                  --  for the two of them will result in a name clash since Ada
                  --  is case insensitive. As such an occurrence is mainly used
                  --  as a technique to perform a renaming, no code is
                  --  generated for the second method an a warning is emitted
                  --  instead.
                  --  ??? We might consider using a hashed map and used the
                  --  '_KNNN' techique here as well. Another possibility would
                  --  be to prefix each upper case letter with un underscore in
                  --  case a name clash is encountered.

                  if Public_Method_Set.Contains
                    (Method_Info'(Name       => Name,
                                  Descriptor => Desc))
                  then
                     Warning ("Name clash (casing). Not binding " &
                              Class_Name & "/" & Name.all & ".");
                  else
                     Public_Method_Set.Insert
                       (Method_Info'(Name       => Name,
                                     Descriptor => Desc));
                     --  Insert all the public method defined

                     if Is_Constructor then
                        if not Constructor_Printed and then not
                          Is_Abstract_Class
                        then
                           PL;
                           Print_Box ("Constructor Declarations");
                           Constructor_Printed := True;
                        end if;

                     elsif not Method_Printed then
                        PL;
                        Print_Box ("Method Declarations");
                        Method_Printed := True;
                     end if;

                     if Generation_Mode = JGNAT then
                        if Is_Constructor then
                           String_List.Append_If_Uniq
                             ("pragma Java_Constructor (" &
                              Constructor_Name & ");",
                              Pragma_List);
                        else
                           String_List.Append_If_Uniq
                             ("pragma Import (Java, " & Subprog_Name &
                              ", """ & Name.all & """);",
                              Pragma_List);
                        end if;
                     end if;

                     Process_Method (CF, M);
                  end if;
               end if;
            end;
         end if;
      end loop;

      if Generation_Mode = JGNAT then
         return;
      end if;

      --  Print the Constructor method in the body.

      if not Is_Abstract_Class (CF) then
         Pretty_Print.Select_Output (Body_File);

         --  Add the Constructor method body.
         PL;
         Print_Box (Constructor_String);
         PL;
         PL ("function " & Constructor_String &
             " (Params : not null access Parameters) " &
             "return " & Type_String);
         PL ("is");
         Pretty_Print.Incr_Indent (+1);
         PL ("begin");
         Pretty_Print.Incr_Indent (-1);
         if Is_Java_Lang_Object then
            PL ("return (JNI_Data with null record);");
         else
            PL ("return (" & Get_Identifier (Get_Public_Super_Class (CF),
                                             Standard_Prefix => True) &
                '.' & Type_String & " with null record);");
         end if;
         PEL ("end " & Constructor_String & ';');

         Pretty_Print.Select_Output (Spec_File);
      end if;

      --  Then print the public methods of all the super classes up to
      --  the first public super class (only in JNI mode).
      declare
         Class_Name : constant String := Get_String (CF, CF.This_Class);
         Info       : constant Class_Info := Get_Info (Class_Name);

         Super_Info : Class_Info;
         Super_Name : String_Ptr;
         File       : File_Id;
         Super_CF   : Class_File;
      begin
         Super_Name := Info.Super_Class_Name;
         loop
            Super_Info := Get_Info (Super_Name.all);
            if Super_Info.Is_Public then
               --  As soon as we have reached the first public super class,
               --  this is over.
               return;
            else
               --  This is a non-public super class

               File := Find_Class_File (Super_Name.all & ".class");

               if not (File = No_File) then
                  --  Ensure that the file has been found.

                  Super_CF :=
                    Read (File.Stream (File.Info.First .. File.Info.Last),
                          True);

                  T := Super_CF.Constant_Pool;

                  for K in 0 .. Member.Last (Super_CF.Methods) loop
                     M := Member.Get (Super_CF.Methods, K);

                     if Is_Public_Member (M, T) and then
                       not Is_Constructor (Super_CF, M)
                     --  For the non public parent classes, we only look
                     --  for public methods and not constructors since
                     --  they must not be inherited.
                     then
                        declare
                           Name : constant String_Ptr := new String'
                             (To_String (Get_Utf8 (T, M.Name_Index)));
                           Desc : constant String_Ptr := new String'
                             (To_String (Get_Utf8 (T, M.Descriptor_Index)));
                        begin
                           if Name.all /= "<clinit>" and then
                           --  Check that this method has not yet been
                           --  added since we want to avoid duplicates
                             Public_Method_Set.Find
                             (Method_Info'(Name       => Name,
                                           Descriptor => Desc)) = No_Element
                           then
                              Public_Method_Set.Include
                                (Method_Info'(Name       => Name,
                                              Descriptor => Desc));
                              --  Insert all the public method defined

                              Process_Method (Super_CF, M);

                           end if;
                        end;
                     end if;
                  end loop;
               end if;
            end if;

            Super_Name := Super_Info.Super_Class_Name;
         end loop;
      end;
   end Print_Methods;

   ---------------------------
   -- Print_Obj_Declaration --
   ---------------------------

   procedure Print_Obj_Declaration
     (CF           : Class_File;
      Pragma_List  : in out String_List.List)
   is
      use type Ada.Text_IO.Count;

      Class_Name : constant String := Get_String (CF, CF.This_Class);
      Info       : constant Class_Info := Get_Info (Class_Name);

      Super_Name : constant String     := Get_Public_Super_Class (CF);
      Super_Info : constant Class_Info := Get_Info (Super_Name);

      Iter       : String_List.List_Iterator;

      Is_Interface      : constant Boolean :=
        Is_Set (CF.Access_Flags, ACC_Interface);

      Has_Discriminants : constant Boolean :=
        Is_Interface or else Info.Implements /= Empty_List;

      Start_Col : Ada.Text_IO.Count;

   begin
      P ("type " & Type_String);

      --  Print the discriminants associated with the interface CF implements.

      if Has_Discriminants then
         P ("(");
         Start_Col := Pretty_Print.Current_Column;
         Associate (Info.Implements, Iter);

         if Is_Interface then
            P ("Self : access Standard.Java.Lang.Object.Typ'Class");
         else
            P (Get_Identifier (Get (Iter), Short => True) & "_I"
               & " : " & Get_Identifier (Get (Iter)) & ".Ref");
            Next (Iter);
         end if;

         while not Is_Last (Iter) loop
            PL (";");
            Pretty_Print.Set_Tmp_Indent (Start_Col);
            P (Get_Identifier (Get (Iter), Short => True) & "_I"
               & " : " & Get_Identifier (Get (Iter)) & ".Ref");
            Next (Iter);
         end loop;

         PL (")");
      end if;

      P (" is ");

      if Is_Set (CF.Access_Flags, ACC_Abstract) then
         P ("abstract ");
      end if;

      --  Print the super class info

      P ("new " & Get_Identifier (Super_Name) & '.' & Type_String);

      --  Constrain the parent type with the discriminants that correspond to
      --  the interfaces the parent class implements if any.

      if Super_Info.Implements /= Empty_List then
         P ("(");
         Start_Col := Pretty_Print.Current_Column;
         Associate (Super_Info.Implements, Iter);

         P (Get_Identifier (Get (Iter), Short => True) & "_I");
         Next (Iter);

         while not Is_Last (Iter) loop
            PL (",");
            Pretty_Print.Set_Tmp_Indent (Start_Col);
            P (Get_Identifier (Get (Iter), Short => True) & "_I");
            Next (Iter);
         end loop;

         P (")");
      end if;

      Print_Fields (CF, Static_Fields => False, Pragma_List => Pragma_List);

      if Is_Interface then
         PL ("pragma Java_Interface (Typ);");
         PEL;
      end if;

      if Get_Info (Class_Name).Is_Exception then
         Print_Box ("Exception Declaration");
         PL (Exception_String & " : Exception;");
         PEL;
         Append_If_Uniq
           ("pragma Import (Java, Except, """ & Dottify (Class_Name) & """);",
            Pragma_List);
      end if;
   end Print_Obj_Declaration;

   ----------------
   -- Print_With --
   ----------------

   procedure Print_With
     (CF                      : Class_File;
      Has_Nested_Public_Class : Boolean;
      Outer_With_List         : String_List.List)
   is
      use String_Set;

      Class_Name        : constant String   := Get_String (CF, CF.This_Class);
      Parent_Class_Name : constant String   := Get_Public_Super_Class (CF);
      T                 : constant CP.Table := CF.Constant_Pool;

      First    : constant Integer := Class_Name'First;
      Lower_CN : constant String := To_Lower (Class_Name);

      With_List : String_List.List;
      --  This is the list of all the 'with' statements to emit.  The
      --  strings in this list should include everything to be printed
      --  except the initial "with " string and the ending ';'.
      Limited_With_List : String_Set.Set;
      --  This is the list of all the 'limited with' statements to emit.  The
      --  strings in this list should include everything to be printed
      --  except the initial "limited with " string and the ending ';'.
      --  Use a Set since some elements will have to be removed, which is not
      --  possible with the J_List package.

      Is_Interface : constant Boolean :=
                       Is_Set (CF.Access_Flags, ACC_Interface);

      Position : String_Set.Cursor;
      Inserted : Boolean;
      --  Variables needed to make the Insert call

      procedure Print_With_Recursive (CF : Class_File);
      --  Collect the 'with' statements for CF interfaces and super class
      --  and call the procedure below for each of its public members.

      procedure Print_With_Descriptor (M : Member_Info);
      --  Collect the with-type statements for field or method M. M's
      --  descriptor is parsed for all the class names that appear in it, and
      --  the required with-type are emitted. We need to generate a with-type
      --  access clause for field types and function return types, while for
      --  parameters we need with-type tagged clause.

      ---------------------------
      -- Print_With_Descriptor --
      ---------------------------

      procedure Print_With_Descriptor (M : Member_Info) is
         D : constant Utf8.Table := J_Basics.Get_Utf8 (T, M.Descriptor_Index);
         W : constant String     := To_String (D);

         Index : Natural := W'First;
         Save  : Natural;

         Array_Dim : U1 := 0;
      begin
         while Index <= W'Last loop
            case W (Index) is
               when '(' | ')' =>
                  Array_Dim := 0;
                  Index     := Index + 1;

               when JVM_Array =>
                  Array_Dim := 0;
                  loop
                     Array_Dim := Array_Dim + 1;
                     Index := Index + 1;
                     exit when W (Index) /= JVM_Array;
                  end loop;

               when JVM_Class =>
                  Save := Index + 1;
                  while W (Index) /= ';' loop
                     Index := Index + 1;
                  end loop;
                  Index := Index + 1;

                  declare
                     Id     : constant String := W (Save .. Index - 2);
                     Ada_Id : constant String := Get_Identifier (Id);
                  begin
                     if Id /= Class_Name  and then Id /= Parent_Class_Name then
                        --  A "with java.lang.Object;" is added for array
                        --  types so the limited with clause for the same
                        --  package is forbidden.

                        if Id /= "java/lang/Object" then

                           --  If Id is an ancestor of the current package,
                           --  there is no need to add a context clause
                           --  since a child package has the visibility on
                           --  its parents by default.

                           --  We must check that the package name is
                           --  complete, ie that there is an ending
                           --  '/' or '$' otherwise we would have wrong
                           --  results like for instance:
                           --
                           --  Java.Beans.Beancontext.BeanContext used in
                           --  Java.Beans.Beancontext.BeanContextChild

                           --  Otherwise, add a limited with clause.

                           if not
                             (Id'Length < Class_Name'Length and then
                                (To_Lower (Id) =
                                   Lower_CN (First .. First + Id'Length - 1))
                              and then
                                (Class_Name (First + Id'Length) = '/' or else
                                   Class_Name (First + Id'Length) = '$'))
                           then
                              String_Set.Insert
                                (Limited_With_List, Ada_Id, Position,
                                 Inserted);
                           end if;
                        end if;
                     end if;

                     Array_Dim := 0;
                  end;

               when others =>
                  Array_Dim := 0;
                  Index     := Index + 1;
            end case;
         end loop;
      end Print_With_Descriptor;

      --------------------------
      -- Print_With_Recursive --
      --------------------------

      procedure Print_With_Recursive (CF : Class_File) is
         Iter : String_List.List_Iterator;
      begin
         --  Print a with statement for the parent class except when there is
         --  no parent package.

         if Class_Name /= "java/lang/Object" then
            Append (Get_Identifier (Get_Public_Super_Class (CF)), With_List);

            if Get_Public_Super_Class (CF) /= "java/lang/Object" then

               --  Added for array types that inherits from java.lang.Object
               Append (Get_Identifier ("java/lang/Object"), With_List);
            end if;
         elsif Generation_Mode = JNI then
            Append ("Interfaces.Java.JNI; use Interfaces.Java.JNI", With_List);
         end if;

         if Generation_Mode = JNI then
            Append ("JNI_Object; use JNI_Object", With_List);
            --  Required for the Parameters type used in the Constructor method
         end if;

         --  Then print a with-type is access clause for every public interface
         --  the current class or its parents extends (non public interfaces
         --  have not been added to the Implements list collected by Get_Info).

         Associate (Get_Info (Class_Name).Implements, Iter);

         while not Is_Last (Iter) loop
            Append_If_Uniq
              (Get_Identifier (Get (Iter)), With_List);
            Next (Iter);
         end loop;

         --  Print the with_type statements for the fields.

         for K in 0 .. Member.Last (CF.Fields) loop
            declare
               Field : constant Member_Info := Member.Get (CF.Fields, K);
            begin
               if Is_Public_Member (Field, T) then
                  Print_With_Descriptor (Field);
               end if;
            end;
         end loop;

         --  Print the with_type statements for the methods.

         for K in 0 .. Member.Last (CF.Methods) loop
            declare
               Method : constant Member_Info := Member.Get (CF.Methods, K);
            begin
               if Is_Public_Member (Method, T) then
                  Print_With_Descriptor (Method);
               end if;
            end;
         end loop;
      end Print_With_Recursive;

      Iter : String_List.List_Iterator;
      Cur  : String_Set.Cursor;

   --  Beginning of Print_With

   begin
      if Generation_Mode = JNI then
         --  Used for the primitive types
         String_List.Append_If_Uniq ("Java_Primitives", With_List);
         String_Set.Insert (Limited_With_List, "Java_Arrays",
                            Position, Inserted);
         if Is_Interface then
            --  Needed since all interfaces are descendant of
            --  JNI_Object.Root_Interface
            String_List.Append_If_Uniq
              ("JNI_Object", With_List);
         end if;
      end if;

      --  Compute all the strings to display.

      Print_With_Recursive (CF);

      --  Sort the list.

      Sort (With_List);

      --  In JNI mode, we also use limited with clause but it is not possible
      --  to mix limited with clause and with clause freely.

      Remove_From_Limited_With_List
        (Limited_With_List, With_List, Outer_With_List);

      --  Then print the limited with clauses
      Cur := String_Set.First (Limited_With_List);

      while Cur /= String_Set.No_Element loop
         PL ("limited with " & String_Set.Element (Cur) & ';');
         String_Set.Next (Cur);
      end loop;

      String_List.Associate (With_List, Iter);

      while not String_List.Is_Last (Iter) loop
         PL ("with " & String_List.Get (Iter) & ';');
         String_List.Next (Iter);
      end loop;

      if Has_Nested_Public_Class and Generation_Mode = JNI then
         --  Store the list of with statements for the inner classes
         --  This is needed to build a valid context clause of the inner
         --  classes.
         Outer_Class_Map.Insert (Class_Name, With_List);
      else
         String_List.Clean (With_List);
      end if;
      String_Set.Clear (Limited_With_List);

   end Print_With;

   ------------------
   -- Add_Packages --
   ------------------

   procedure Add_Packages
     (CF   : Class_File;
      M    : Member_Info;
      List : in out String_Set.Set)
   is
      use J_Basics;
      use String_Set;

      T : constant CP.Table := CF.Constant_Pool;

      Class_Name : constant String := Get_String (CF, CF.This_Class);

      Descriptor : constant String :=
        To_String (J_Basics.Get_Utf8 (T, M.Descriptor_Index));

      Index : Natural := Descriptor'First;
      Save  : Natural;

      Position : String_Set.Cursor;
      Inserted : Boolean;
      --  Variables needed to make the Insert call

   begin

      while Index <= Descriptor'Last loop
         case Descriptor (Index) is
            when JVM_Class =>
               Save := Index + 1;

               while Descriptor (Index) /= ';' loop
                  Index := Index + 1;
               end loop;

               declare
                  Id     : constant String := Descriptor (Save .. Index - 1);
                  Ada_Id : constant String := Get_Identifier (Id);
               begin
                  if Id /= Class_Name then
                     List.Insert (Ada_Id, Position, Inserted);
                  end if;
               end;
               Index := Index + 1;

            when others =>
               Index := Index + 1;
         end case;
      end loop;

   end Add_Packages;

   -------------------------
   -- Print_With_For_Body --
   -------------------------

   procedure Print_With_For_Body (CF : Class_File) is
      use String_Set;

      T : constant CP.Table := CF.Constant_Pool;

      With_List : String_Set.Set;
      --  Store the package names

      Cur : Cursor;
   begin
      --  Compute all the packages to add in the context clause.

      PL ("with Java_Primitives; use Java_Primitives;");
      PL ("with JNI;             use JNI;");
      PL ("with JNI_JVM;         use JNI_JVM;");
      PL ("with JNI_Object;      use JNI_Object;");
      --  Used by all get accessors
      PL ("with Java.Lang.Object; use Java.Lang.Object;");
      --  Needed for primitve array parameters
      PL ("with Java_Arrays;");

      --  A with clause for all reference types used in method calls
      --  is needed since we want to have visibility on the type and
      --  thus on the parent type.
      --  This is needed to allow the calls to Secure_Get_J_Object,
      --  Get_J_Object and Set_J_Object.
      for K in 0 .. Member.Last (CF.Methods) loop
         declare
            Method : constant Member_Info := Member.Get (CF.Methods, K);
         begin
            if Is_Public_Member (Method, T) then
               Add_Packages (CF, Method, With_List);
            end if;
         end;
      end loop;

      --  A with clause for all reference types used in accessor methods
      --  is needed since we want to have visibility on the type and
      --  thus on the parent type.
      --  This is needed to allow the cass to Secure_Get_J_Object,
      --  Get_J_Object and Set_J_Object
      for K in 0 .. Member.Last (CF.Fields) loop
         declare
            Field : constant Member_Info := Member.Get (CF.Fields, K);
         begin
            if Is_Public_Member (Field, T) then
               Add_Packages (CF, Field, With_List);
            end if;
         end;
      end loop;

      --  Print the with clauses.

      Cur := With_List.First;
      while Cur /= No_Element loop
         PL ("with " & Element (Cur) & ';');
         Next (Cur);
      end loop;

      With_List.Clear;

   end Print_With_For_Body;

   ---------------------
   -- Print_Variables --
   ---------------------

   procedure Print_Variables (CF : Class_File; M : Member_Info) is
      use J_Basics;

      Class_Identifier : constant String := Get_Class_Identifier (CF);

      T : constant CP.Table := CF.Constant_Pool;

      Descriptor : constant String :=
        To_String (J_Basics.Get_Utf8 (T, M.Descriptor_Index));

      Parameter_Nb : constant Natural := Parameter_Count (Descriptor);

      Method_Name : constant String := To_String (Get_Utf8 (T, M.Name_Index));

      Is_Constructor : constant Boolean
        := Method_Name (Method_Name'First) = '<';

   begin
      Print_Get_Env_Variables;

      if Is_Constructor then
         PL ("Result : " & Class_Identifier & " := new " & Type_String & ';');
         PL ("J_Obj  : J_Object := J_Null_Object;");
      end if;

      PL ("Args   : J_Value_Array (1 .." & Integer'Image (Parameter_Nb) &
          ");");
      PL ("Method_Name : constant Standard.String := """ &
          Method_Name & """;");

   end Print_Variables;

   -----------------------------
   -- Print_Method_Statements --
   -----------------------------

   procedure Print_Method_Statements
     (CF : Class_File;
      M  : Member_Info;
      ID : String)
   is
      use J_Basics;

      T : constant CP.Table := CF.Constant_Pool;

      Descriptor : constant String :=
                     To_String (J_Basics.Get_Utf8 (T, M.Descriptor_Index));

      Return_Type_Descriptor : constant String :=
                                 Get_Return_Type_Descriptor (Descriptor);

      Method_Name       : constant String :=
                            To_String (Get_Utf8 (T, M.Name_Index));
      Is_Constructor    : constant Boolean :=
                            Method_Name (Method_Name'First) = '<';
      Is_Procedure      : constant Boolean :=
                            Descriptor (Descriptor'Last) = 'V';
      Is_Static         : constant Boolean :=
                            Is_Set (M.Access_Flags, ACC_Static);
      Is_Reference_Type : constant Boolean :=
                            Get_Java_Type (Return_Type_Descriptor) = "Object";

      Parameter_Nb      : constant Natural := Parameter_Count (Descriptor);

      Current_Pos : Natural := Descriptor'First + 1;
      End_Pos     : Natural;

      C     : Character;
   begin
      --  Code common to both Methods and Constructors

      PL;
      Print_Get_Env;
      PL;

      Print_Check_Class;
      PL;

      Print_Check_Method_ID (ID, Descriptor, Is_Static);
      PL;

      if Parameter_Nb /= 0 then
         P ("--  Construct the parameter table");

         for I in 1 .. Parameter_Nb loop
            PL;
            P ("Args (");
            End_Pos := Next_Declaration_Pos
              (Descriptor (Current_Pos .. Descriptor'Last));

            C := Get_JNI_Letter (Descriptor (Current_Pos));

            P (Strip (Integer'Image (I)) & ") := J_Value'(T => " &
               Get_JNI_Type (Descriptor (Current_Pos)) &
               ", " & C & " => ");
            P (Print_JNI_Conversion (C));
            P ("P" & Strip (Integer'Image (I)) & "_" &
               Ada_Type (Descriptor (Current_Pos .. End_Pos - 1),
                         Format => Use_As_Name) & ')');
            P (");");
            Current_Pos := End_Pos;
         end loop;

         PEL;
         PL;
      end if;

      if Is_Constructor then

         PL ("--  Call the constructor");
         PL ("J_Obj := New_Object_A (Env, " & Class_String & ", " & ID &
             ", Args);");
         Pretty_Print.Incr_Indent (+1);
         PL ("If J_Obj = J_Null_Object then");
         Pretty_Print.Incr_Indent (-1);
         PL ("raise Program_Error;");
         PL ("end if;");
         PL ("Result.Set_J_Object (J_Obj);");
         PL ("return Result;");
      else
         --  This is a subprogram

         if not Is_Procedure then
            P ("return ");
         end if;

         if Is_Reference_Type then
            P ("new " & Ada_Type (Return_Type_Descriptor, Conversion_Type)
               & "'Class'("
               & Ada_Type (Return_Type_Descriptor, Conversion_Type)
               & "'Class (Create_Ada_Object (");
         end if;

         P (Call_Method_Name (Return_Type_Descriptor, Is_Static) &
            " (Env, ");

         if Is_Static then
            P (Class_String);
         else
            P ("This.Get_J_Object");
         end if;

         P (", " & ID & ", Args)");

         if Is_Reference_Type then
            PL (").all));");
            --  TODO: This is to workaround a current limitation of GNAT
         else
            PL (";");
         end if;

      end if;
   end Print_Method_Statements;

   ------------------------
   -- Subprogram_ID_Name --
   ------------------------

   function Subprogram_ID_Name (Name : String) return String is
      use String_Natural_Map;
      use J_Basics;

      Cur         : Cursor;
      ID_String   : constant String := Name & "_ID";
      U_ID_String : constant String := To_Upper (ID_String);
      Number      : Natural;
   begin
      Cur := Used_Names.Find (U_ID_String);
      if Cur = No_Element then
         Used_Names.Insert (U_ID_String, 1);
         return ID_String;
      else
         Number := Element (Cur);
         Used_Names.Replace (U_ID_String, Number + 1);
         return ID_String & '_' & Strip (Natural'Image (Number));
      end if;
   end Subprogram_ID_Name;

   --------------------
   -- Process_Method --
   --------------------

   procedure Process_Method (CF : Class_File; M : Member_Info) is
      use J_Basics;

      function Select_Name
        (Constructor_Name : String;
         Method_Name      : String;
         Is_Constructor   : Boolean)
         return String;
      --  Return Constructor_Name if Is_Constructor is set, Method_Name
      --  otherwise

      -----------------
      -- Select_Name --
      -----------------

      function Select_Name
        (Constructor_Name : String;
         Method_Name      : String;
         Is_Constructor   : Boolean)
         return String is
      begin
         if Is_Constructor then
            return Constructor_Name;
         else
            return Method_Name;
         end if;
      end Select_Name;

      --  Variable declarations

      F : constant Access_Mask := M.Access_Flags;

      Is_Protected    : constant Boolean := Is_Set (F, ACC_Protected);
      Is_Final        : constant Boolean := Is_Set (F, ACC_Final);
      Is_Synchronized : constant Boolean := Is_Set (F, ACC_Synchronized);

      T : constant CP.Table := CF.Constant_Pool;

      Method_Name : constant String := Ada_Identifier
        (To_String (Get_Utf8 (T, M.Name_Index)));

      Is_Constructor : constant Boolean :=
                         Method_Name (Method_Name'First) = '<';

      Class_Identifier : constant String :=
                           Get_String (CF, CF.This_Class);
      Constructor_Name : constant String :=
                           "New_" & Get_Identifier
                             (Class_Identifier, Short => True);
      Current_Name     : constant String :=
                           Select_Name
                           (Constructor_Name, Method_Name, Is_Constructor);
   begin

      --  Specification part
      if Is_Final or else Is_Protected or else Is_Synchronized then
         PL;
         P ("--");

         if Is_Final then
            P ("  final");
         end if;

         if Is_Protected then
            P ("  protected");
         end if;

         if Is_Synchronized then
            P ("  synchronized");
         end if;

      end if;

      Print_Method_Signature (CF, M);
      P (";");
      Print_Exception_Comment (CF, M);
      PEL;

      if Generation_Mode = JGNAT or else Is_Abstract_Method (CF, M) then
         --  We do not print the implementation for JGNAT
         --  or when the method is abstract in JNI
         return;
      end if;

      --  Declare the ID_String in that block to avoid the creation
      --  of ID for abstract methods.
      declare
         ID_String : constant String := Subprogram_ID_Name (Current_Name);
      begin
         --  Body part
         Pretty_Print.Select_Output (Body_File);

         PL;
         PEL (ID_String & " : J_Method_ID := J_Null_Method_ID;");
         PL;
         Print_Box (Current_Name);

         Print_Method_Signature (CF, M);

         PL;
         Pretty_Print.Incr_Indent (+1);
         PL ("is");

         Print_Variables (CF, M);

         Pretty_Print.Incr_Indent (-1);
         PL;
         Pretty_Print.Incr_Indent (+1);
         PL ("begin");

         Print_Method_Statements (CF, M, ID_String);

         Pretty_Print.Incr_Indent (-1);
         PL;
         PEL ("end " & Current_Name & ';');

         Pretty_Print.Select_Output (Spec_File);
      end;
   end Process_Method;

   -----------------------------------
   -- Remove_From_Limited_With_List --
   -----------------------------------

   procedure Remove_From_Limited_With_List
     (Limited_With_List : in out String_Set.Set;
      With_List         : in out String_List.List;
      Outer_With_List   : String_List.List)
   is
      Iter : String_List.List_Iterator;
   begin

      String_List.Associate (With_List, Iter);

      while not String_List.Is_Last (Iter) loop

         declare
            Pkg      : constant String  := String_List.Get (Iter);
            Point_Nb : constant Integer := Count (Pkg, ".");
            Position : Integer := Pkg'First;
         begin
            --  First check that the same package is not in both lists.

            if String_Set.Contains (Limited_With_List, Pkg) then
               String_Set.Delete (Limited_With_List, Pkg);
            end if;

            --  Then check that there is not limited with clause
            --  for one of its parent package.

            for I in 1 .. Point_Nb loop
               --  Position of the Ith '.'
               Position := Index (Pkg, ".", Position + 1);

               if String_Set.Contains (Limited_With_List,
                                       Pkg (Pkg'First .. Position - 1))
               then

                  --  If this is the case, remove the package from the
                  --  limited_with_list and put it in the with_list.

                  String_Set.Delete (Limited_With_List,
                                     Pkg (1 .. Position - 1));
                  String_List.Append_If_Uniq (Pkg (1 .. Position - 1),
                                              With_List);
               end if;
            end loop;

            String_List.Next (Iter);
         end;
      end loop;

      --  Remove from the limited with clause all the packages
      --  that are already withed in the outer package
      --  This is only useful for nested classes.

      String_List.Associate (Outer_With_List, Iter);

      while not String_List.Is_Last (Iter) loop
         declare
            Pkg : constant String := String_List.Get (Iter);
         begin
            if Limited_With_List.Contains (Pkg) then
               Limited_With_List.Delete (Pkg);
            end if;
         end;

         String_List.Next (Iter);
      end loop;

   end Remove_From_Limited_With_List;

   ---------------
   -- Print_Box --
   ---------------

   procedure Print_Box (S : String) is
      Comment_Line : constant String (1 .. S'Length + 6) := (others => '-');
   begin
      PL (Comment_Line);
      PL ("-- " & S & " --");
      PEL (Comment_Line);
   end Print_Box;

   -----------------
   -- Print_Utils --
   -----------------

   procedure Print_Utils (CF : Class_File) is
   begin
      PL;
      Print_Box ("Utility subprograms");
      PL;
      PEL ("procedure " & Set_Class & " (Env : " & Env_Type_String & ");");
      PL;
      Print_Box (Set_Class);
      PL;
      Pretty_Print.Incr_Indent (+1);
      PL ("procedure " & Set_Class & " (Env : " & Env_Type_String & ") is");
      Pretty_Print.Incr_Indent (-1);
      PL ("C : constant Standard.String := """ &
          Get_String (CF, CF.This_Class) & """;");
      Pretty_Print.Incr_Indent (+1);
      PL ("begin");
      PL (Class_String & " := Find_Class (Env, C);");
      Pretty_Print.Incr_Indent (+1);
      PL ("if " & Class_String & " = J_Null_Class then");
      Pretty_Print.Incr_Indent (-1);
      PL ("raise Program_Error with """ & Set_Class & ": "" & C & " &
          """.class not found."";");
      Pretty_Print.Incr_Indent (-1);
      PL ("end if;");
      PEL ("end " & Set_Class & ';');
   end Print_Utils;

   -----------------------
   -- Search_Classes_In --
   -----------------------

   procedure Search_Classes_In (Zip : String) is
      procedure Search_Classes_Instance is
        new Generic_Search_In (The_Search_List => Classes_Search_List);
   begin
      Search_Classes_Instance (Zip);
   end Search_Classes_In;

   -----------------------
   -- Search_Sources_In --
   -----------------------

   procedure Search_Sources_In (Zip : String) is
      procedure Search_Sources_Instance is
        new Generic_Search_In (The_Search_List => Sources_Search_List);
   begin
      Search_Sources_Instance (Zip);
   end Search_Sources_In;

   -------------
   -- Warning --
   -------------

   procedure Warning (Msg : String) is
   begin
      if not Quiet_Mode then
         Ada.Text_IO.Put_Line ("*** " & Msg);
      end if;
   end Warning;

   ------------------------
   -- Get_File_Extension --
   ------------------------

   function Get_File_Extension (Output_Type : Ada_File_Type) return String is
   begin
      case Output_Type is
         when Body_File => return ".adb";
         when Spec_File => return ".ads";
      end case;
   end Get_File_Extension;

begin
   Used_Names.Clear;

   --  Add entries for "java/lang/Object" and "java/lang/Throwable" to
   --  the Symbol_Table.

   declare
      Serializable : String_List.List;
   begin
      Symbol_Table_Pkg.Set
        (Symbol_Table,
         Name  => "java/lang/Object",
         Value => (Super_Class_Name => new String'(""),
                   Implements       => String_List.Empty_List,
                   Is_Public        => True,
                   Is_Exception     => False));

      String_List.Append ("java/io/Serializable", Serializable);

      Symbol_Table_Pkg.Set
        (Symbol_Table,
         Name  => "java/lang/Throwable",
         Value => (Super_Class_Name => new String'("java/lang/Object"),
                   Implements       => Serializable,
                   Is_Public        => True,
                   Is_Exception     => True));
   end;
end JVM_Ada;
