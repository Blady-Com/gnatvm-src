------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M . E M I T                              --
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
-- This work is partially  based on A#, an Ada  compiler for .NET by  Prof. --
-- Martin C. Carlisle of the United States Air Force Academy.               --
--                                                                          --
------------------------------------------------------------------------------

with Atree;        use Atree;
with Ada.Text_IO;  use Ada.Text_IO;
with Debug;        use Debug;
with Einfo;        use Einfo;
with Elists;       use Elists;
with Exp_Disp;     use Exp_Disp;
with Exp_Tss;      use Exp_Tss;
with Errout;       use Errout;
with Gnatvsn;      use Gnatvsn;
with J_String;     use J_String;
with Jx_Decl;      use Jx_Decl;
with JVM.Code;     use JVM.Code;
with JVM.Dbg;      use JVM.Dbg;
with JVM.Emit.CIL;
with JVM.Info;     use JVM.Info;
with JVM.Map;      use JVM.Map;
with JVM.Pool;     use JVM.Pool;
with JVM.Verifier; use JVM.Verifier;
with JVM_File;     use JVM_File;
with Lib;          use Lib;
with Opt;
with Osint;        use Osint;
with Output;       use Output;
with Sem_Disp;     use Sem_Disp;
with Sem_Util;     use Sem_Util;
with Sinfo;        use Sinfo;
with Sinput;       use Sinput;
with Snames;

with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body JVM.Emit is

   function Check_Reserved (Name : String) return String;
   --  Check if Name is a reserved word, and escape it if so

   procedure Emit_Code
     (File   : Ada.Text_IO.File_Type;
      Method : Method_Id);
   --  Subsidiary of Emit_Method

   procedure Emit_Code_Sequence
     (File       : Ada.Text_IO.File_Type;
      Method     : Method_Id;
      Method_Seq : Code_Sequence);
   --  Subsidiary of Emit_Code

   procedure Emit_Class
     (File  : Ada.Text_IO.File_Type;
      Class : Class_Id);
   --  Subsidiary of Produce_Class_File

   procedure Emit_Method
     (File   : Ada.Text_IO.File_Type;
      Method : Method_Id);
   --  Subsidiary of Emit_Class

   function External_Name (Name  : String)   return String;
   function External_Name (Class : Class_Id) return String;
   function External_Name (Name  : Name_Id)  return String;

   function Params_String
     (Method : Method_Id;
      Names  : Boolean := True) return String;
   --  Returns the string containing the profile associated with Method

   function Trim_Namespace (X : String) return String;
   --  Returns the string available after the first dot. That is, returns:
   --    X (Index (X, '.') + 1 .. X'Last)

   ------------------
   -- Add_Assembly --
   ------------------

   procedure Add_Assembly (Name : String) is
   begin
      CIL.Add_Assembly (Name);
   end Add_Assembly;

   procedure Add_Assembly (Version : String; Name : String_Id) is
   begin
      CIL.Add_Assembly (Version, Name);
   end Add_Assembly;

   --------------------
   -- Check_Reserved --
   --------------------

   function Check_Reserved (Name : String) return String is
   begin
      --  ??? Replace by a table

      if Name = "add"
        or else Name = "algorithm"
        or else Name = "alignment"
        or else Name = "ansi"
        or else Name = "any"
        or else Name = "arglist"
        or else Name = "as"
        or else Name = "assembly"
        or else Name = "assert"
        or else Name = "auto"
        or else Name = "autochar"
        or else Name = "beforefieldinit"
        or else Name = "beq"
        or else Name = "bge"
        or else Name = "bgt"
        or else Name = "ble"
        or else Name = "blob"
        or else Name = "blt"
        or else Name = "bne"
        or else Name = "bool"
        or else Name = "box"
        or else Name = "br"
        or else Name = "brfalse"
        or else Name = "brinst"
        or else Name = "brnull"
        or else Name = "brtrue"
        or else Name = "brzero"
        or else Name = "bstr"
        or else Name = "bytearray"
        or else Name = "byvalstr"
        or else Name = "call"
        or else Name = "calli"
        or else Name = "callmostderived"
        or else Name = "callvirt"
        or else Name = "carray"
        or else Name = "castclass"
        or else Name = "catch"
        or else Name = "cdecl"
        or else Name = "ceq"
        or else Name = "cf"
        or else Name = "cgt"
        or else Name = "char"
        or else Name = "cil"
        or else Name = "ckfinite"
        or else Name = "class"
        or else Name = "clsid"
        or else Name = "clt"
        or else Name = "cpblk"
        or else Name = "cpobj"
        or else Name = "currency"
        or else Name = "custom"
        or else Name = "date"
        or else Name = "decimal"
        or else Name = "default"
        or else Name = "demand"
        or else Name = "deny"
        or else Name = "div"
        or else Name = "dup"
        or else Name = "endfault"
        or else Name = "endfilter"
        or else Name = "endfinally"
        or else Name = "endmac"
        or else Name = "enum"
        or else Name = "error"
        or else Name = "explicit"
        or else Name = "extends"
        or else Name = "extern"
        or else Name = "false"
        or else Name = "famandassem"
        or else Name = "family"
        or else Name = "famorassem"
        or else Name = "fastcall"
        or else Name = "fault"
        or else Name = "field"
        or else Name = "filetime"
        or else Name = "filter"
        or else Name = "final"
        or else Name = "finally"
        or else Name = "fixed"
        or else Name = "flags"
        or else Name = "float"
        or else Name = "float32"
        or else Name = "float64"
        or else Name = "forwardref"
        or else Name = "fromunmanaged"
        or else Name = "handler"
        or else Name = "hidebysig"
        or else Name = "hresult"
        or else Name = "idispatch"
        or else Name = "il"
        or else Name = "illegal"
        or else Name = "implements"
        or else Name = "implicitcom"
        or else Name = "implicitres"
        or else Name = "import"
        or else Name = "inheritcheck"
        or else Name = "init"
        or else Name = "initblk"
        or else Name = "initobj"
        or else Name = "initonly"
        or else Name = "instance"
        or else Name = "int"
        or else Name = "int8"
        or else Name = "int16"
        or else Name = "int32"
        or else Name = "int64"
        or else Name = "interface"
        or else Name = "internalcall"
        or else Name = "isinst"
        or else Name = "iunknown"
        or else Name = "jmp"
        or else Name = "lasterr"
        or else Name = "lcid"
        or else Name = "ldarg"
        or else Name = "ldarga"
        or else Name = "ldelem"
        or else Name = "ldelema"
        or else Name = "ldfld"
        or else Name = "ldflda"
        or else Name = "ldftn"
        or else Name = "ldlen"
        or else Name = "ldloc"
        or else Name = "ldloca"
        or else Name = "ldnull"
        or else Name = "ldobj"
        or else Name = "ldsfld"
        or else Name = "ldsflda"
        or else Name = "ldstr"
        or else Name = "ldtoken"
        or else Name = "ldvirtftn"
        or else Name = "leave"
        or else Name = "linkcheck"
        or else Name = "literal"
        or else Name = "localloc"
        or else Name = "lpstr"
        or else Name = "lpvoid"
        or else Name = "lpwstr"
        or else Name = "managed"
        or else Name = "marshal"
        or else Name = "method"
        or else Name = "mkrefany"
        or else Name = "modopt"
        or else Name = "modreq"
        or else Name = "mul"
        or else Name = "native"
        or else Name = "neg"
        or else Name = "neg"
        or else Name = "nested"
        or else Name = "newarr"
        or else Name = "newobj"
        or else Name = "newslot"
        or else Name = "noappdomain"
        or else Name = "noinlining"
        or else Name = "nomachine"
        or else Name = "nomangle"
        or else Name = "nometadata"
        or else Name = "noncasdemand"
        or else Name = "noncasinheritance"
        or else Name = "noncaslinkdemand"
        or else Name = "nop"
        or else Name = "noprocess"
        or else Name = "not_in_gc_heap"
        or else Name = "notremotable"
        or else Name = "notserialized"
        or else Name = "object"
        or else Name = "objectref"
        or else Name = "off"
        or else Name = "on"
        or else Name = "opt"
        or else Name = "optil"
        or else Name = "permitonly"
        or else Name = "pinned"
        or else Name = "pinvokeimpl"
        or else Name = "pop"
        or else Name = "prefix1"
        or else Name = "prefix2"
        or else Name = "prefix3"
        or else Name = "prefix4"
        or else Name = "prefix5"
        or else Name = "prefix6"
        or else Name = "prefix7"
        or else Name = "prefixref"
        or else Name = "prejitdeny"
        or else Name = "prejitgrant"
        or else Name = "preservsig"
        or else Name = "privatescope"
        or else Name = "public"
        or else Name = "readonly"
        or else Name = "refany"
        or else Name = "refanytype"
        or else Name = "refanyval"
        or else Name = "rem"
        or else Name = "reqmin"
        or else Name = "reqopt"
        or else Name = "reqrefuse"
        or else Name = "reqsecobj"
        or else Name = "request"
        or else Name = "ret"
        or else Name = "retargetable"
        or else Name = "rethrow"
        or else Name = "retval"
        or else Name = "rtspecialname"
        or else Name = "runtime"
        or else Name = "safearray"
        or else Name = "sealed"
        or else Name = "sequential"
        or else Name = "serializable"
        or else Name = "shl"
        or else Name = "shr"
        or else Name = "sizeof"
        or else Name = "special"
        or else Name = "specialname"
        or else Name = "starg"
        or else Name = "static"
        or else Name = "stdcall"
        or else Name = "stelem"
        or else Name = "stfld"
        or else Name = "stloc"
        or else Name = "stobj"
        or else Name = "storage"
        or else Name = "stored_object"
        or else Name = "stream"
        or else Name = "streamed_object"
        or else Name = "strict"
        or else Name = "string"
        or else Name = "struct"
        or else Name = "stsfld"
        or else Name = "sub"
        or else Name = "switch"
        or else Name = "synchronized"
        or else Name = "syschar"
        or else Name = "sysstring"
        or else Name = "tbstr"
        or else Name = "thiscall"
        or else Name = "throw"
        or else Name = "tls"
        or else Name = "to"
        or else Name = "true"
        or else Name = "typedref"
        or else Name = "unbox"
        or else Name = "unicode"
        or else Name = "unmanaged"
        or else Name = "unmanagedexp"
        or else Name = "unsigned"
        or else Name = "uint"
        or else Name = "unused"
        or else Name = "userdefined"
        or else Name = "value"
        or else Name = "valuetype"
        or else Name = "vararg"
        or else Name = "variant"
        or else Name = "vector"
        or else Name = "virtual"
        or else Name = "void"
        or else Name = "wchar"
        or else Name = "winapi"
        or else Name = "wrapper"
      then
         return "'" & Name & "'";
      else
         return Name;
      end if;
   end Check_Reserved;

   -------------------
   -- External_Name --
   -------------------

   function External_Name (Name : String) return String is

      function Starts_With (S, Prefix : String) return Boolean;
      --  Return True if the contents of S starts with Prefix

      -----------------
      -- Starts_With --
      -----------------

      function Starts_With (S, Prefix : String) return Boolean is
      begin
         return S'Length >= Prefix'Length
           and then S (S'First .. S'First + Prefix'Length - 1) = Prefix;
      end Starts_With;

   --  Start of processing for External_Name

   begin
      --  Make sure that the appropriate prefix is added for the built-in
      --  library packages. Perhaps this is a stopgap measure until they are
      --  redone (instead of just being translated using jbimp and then
      --  modified by hand)

      if Starts_With (Name, "mgnat.adalib")
        or else Starts_With (Name, "ada.io_exceptions")
      then
         return "[mgnat]" & Name;

      elsif Starts_With (Name, "System") then
         return "[mscorlib]" & Name;

      else
         return Check_Reserved (Name);
      end if;
   end External_Name;

   -------------------
   -- External_Name --
   -------------------

   function External_Name (Name : Name_Id) return String is
   begin
      return External_Name (Name_String (Name));
   end External_Name;

   -------------------
   -- External_Name --
   -------------------

   function External_Name (Class : Class_Id) return String is
   begin
      if Outer_Class (Class) /= Null_Class then
         return External_Name (Outer_Class (Class))
                  & "/"
                  & Check_Reserved (Name_String (Name (Class)));
      end if;

      --  Because some references are generated outside of jvm-api
      --  make sure the assembly prefix is added if needed

      declare
         Pkg : constant String := Str (Pkg_Name (Class));

      begin
         if Pkg /= "" then
            return External_Name (Pkg)
                     & "."
                     & Check_Reserved (Name_String (Name (Class)));
         else
            return External_Name (Name (Class));
         end if;
      end;
   end External_Name;

   -------------------
   -- Params_String --
   -------------------

   function Params_String
     (Method : Method_Id; Names : Boolean := True) return String
   is
      function Params_String_Helper
        (Params : Local_Var_Id;
         Names  : Boolean := True) return String;

      --------------------------
      -- Params_String_Helper --
      --------------------------

      function Params_String_Helper
        (Params : Local_Var_Id;
         Names  : Boolean := True) return String is
      begin
         if Params = Null_Local_Var
           or else not Is_Param (Params)
         then
            return "";

         else
            --  Recursively apply the function to the remainder of the
            --  parameters in the list, concatenating the type descriptors
            --  of the parameters.

            if Names then
               declare
                  Rest : constant String :=
                           Params_String_Helper
                             (Next_Local_Var (Params), Names);
                  This : constant String :=
                           Type_String
                             (Type_Of (Params))
                                & " "
                                & External_Name (Name (Params));
               begin
                  if Rest = "" then
                     return This;
                  else
                     return This & ", " & Rest;
                  end if;
               end;
            else
               declare
                  Rest : constant String :=
                           Params_String_Helper
                             (Next_Local_Var (Params), Names);
                  This : constant String :=
                           Type_String (Type_Of (Params));

               begin
                  if Rest = "" then
                     return This;
                  else
                     return This & ", " & Rest;
                  end if;
               end;
            end if;
         end if;
      end Params_String_Helper;

      --  Local variables

      First_Param : Local_Var_Id := First_Local_Var (Method);

   --  Start of processing for Params_String

   begin
      if not Is_Static (Method) then
         First_Param := Next_Local_Var (First_Param);
      end if;

      return "(" & Params_String_Helper (First_Param, Names) & ")";
   end Params_String;

   -----------------
   -- Type_String --
   -----------------

   function Type_String (Typ : Type_Id) return String is
   begin
      case Type_Kind (Typ) is
         when Boolean_Kind | Byte_Kind  | Char_Kind | Short_Kind =>
            pragma Assert (False);
            return "";

         when Int_Kind =>
            return Name_String (Name (Typ));

         when Void_Kind | Long_Kind | Float_Kind | Double_Kind =>
            return Name_String (Name (Typ));

         when Array_Kind =>
            return Type_String (Element_Type (Typ))  &
               (Positive (Dimensions (Typ)) * "[]");

         when Class_Kind =>
            if External_Name (Name (Class (Typ))) = "string" then
               return "string";

            elsif External_Name (Name (Class (Typ))) = "native int" then
               return "native int";

            elsif Superclass (Class (Typ)) /= 0
              and then Is_Value_Type (Class (Typ))
            then
               return "valuetype " & External_Name (Class (Typ));

            else
               return "class " & External_Name (Class (Typ));
            end if;

         when Return_Addr_Kind =>
            pragma Assert (False);
            return Name_String (Name (Typ));
      end case;
   end Type_String;

   Last_Source_Location : Source_Ptr := No_Location;
   --  Keep track of last known source location, to be able to generate error
   --  messages

   procedure Emit_Code_Sequence
     (File       : Ada.Text_IO.File_Type;
      Method     : Method_Id;
      Method_Seq : Code_Sequence)
   is
      function Add_Slashes (Str : String) return String;
      --  In case the string contains quotation marks, or slashes, precede them
      --  with slashes

      procedure Emit_CP_Double (File : Ada.Text_IO.File_Type; Value : Ureal);
      procedure Emit_CP_Float  (File : Ada.Text_IO.File_Type; Value : Ureal);

      -----------------
      -- Add_Slashes --
      -----------------

      function Add_Slashes (Str : String) return String is

         function Byte_Array (Str : String) return String;

         function Byte_Array (Str : String) return String is

            function To_Hex (C : Character) return String;

            function To_Hex (C : Character) return String is
               Result : String (1 .. 6);
            begin
               Put (To => Result, Item => Character'Pos (C), Base => 16);

               if Result (4) = '#' then
                  Result (4) := '0';
               end if;

               return Result (4 .. 5);
            end To_Hex;

            --  Local variables

            Result  : String (1 .. Str'Length * 6);
            Counter : Natural := 1;

         --  Start of processing for Byte_Array

         begin
            for J in Str'Range loop
               Result (Counter .. Counter + 5) := To_Hex (Str (J)) & " 00 ";
               Counter := Counter + 6;
            end loop;

            return Result;
         end Byte_Array;

         --  Local variables

         Result    : String (1 .. Str'Length * 2);
         Last      : Natural := 0;
         Is_Binary : Boolean := False;

      --  Start of processing for Add_Slashes

      begin
         for J in Str'Range loop
            if not (Is_Graphic (Str (J)))
              and then Str (J) /= ASCII.LF
              and then Str (J) /= ASCII.CR
              and then Str (J) /= ASCII.HT
            then
               Is_Binary := True;
            end if;
         end loop;

         for J in Str'Range loop
            case Str (J) is
               when '"' | '\' | ''' | '?' =>
                  Last := Last + 1;
                  Result (Last) := '\';
                  Last := Last + 1;
                  Result (Last) := Str (J);

               when ASCII.HT =>
                  Last := Last + 1;
                  Result (Last) := '\';
                  Last := Last + 1;
                  Result (Last) := 't';

               when ASCII.LF =>
                  Last := Last + 1;
                  Result (Last) := '\';
                  Last := Last + 1;
                  Result (Last) := 'n';

               when ASCII.CR =>
                  Last := Last + 1;
                  Result (Last) := '\';
                  Last := Last + 1;
                  Result (Last) := 'r';

               when others =>
                  if Is_Graphic (Str (J)) then
                     Last := Last + 1;
                     Result (Last) := Str (J);
                  end if;
            end case;
         end loop;

         if not Is_Binary then
            return '"'
                     & Result (1 .. Last)
                     & '"';
         else
            return "bytearray("
                     & Byte_Array (Str)
                     & ") // "
                     & Result (1 .. Last);
         end if;
      end Add_Slashes;

      --------------------
      -- Emit_CP_Double --
      --------------------

      procedure Emit_CP_Double (File : Ada.Text_IO.File_Type; Value : Ureal) is
         Num      : Long_Long_Float := 0.0;
         Den      : Long_Long_Float := 0.0;
         Sign     : Long_Long_Float := 1.0;
         Tmp      : Uint;
         Index    : Integer;
         Answer   : U8;

      begin
         if UR_Is_Negative (Value) then
            Sign := -1.0;
         end if;

         --  In the following calculus, we consider numbers modulo 2 ** 31,
         --  so that we don't have problems with signed Int...

         Tmp := abs (Numerator (Value));
         Index := 0;
         while Tmp > 0 loop
            Num := Num
              + Long_Long_Float (UI_To_Int (Tmp mod (Uint_2 ** 31)))
              * (2.0 ** Index);
            Tmp := Tmp / Uint_2 ** 31;
            Index := Index + 31;
         end loop;

         Tmp := abs (Denominator (Value));
         if Rbase (Value) /= 0 then
            Tmp := Rbase (Value) ** Tmp;
         end if;

         Index := 0;
         while Tmp > 0 loop
            Den := Den
              + Long_Long_Float (UI_To_Int (Tmp mod (Uint_2 ** 31)))
              * (2.0 ** Index);
            Tmp := Tmp / Uint_2 ** 31;
            Index := Index + 31;
         end loop;

         --  If the denominator denotes a negative power of Rbase,
         --  then multiply by the denominator.

         if Rbase (Value) /= 0 and then Denominator (Value) < 0 then
            Answer := To_U8 (IEEE_Float_64 (Sign * Num * Den));

         --  Otherwise compute the quotient

         else
            Answer := To_U8 (IEEE_Float_64 (Sign * Num / Den));
         end if;

         declare
            package U8_IO is new Ada.Text_IO.Modular_IO (U8);

            Answer_String : String (1 .. 20);
            First         : Natural;
            Last          : Natural;

         begin
            U8_IO.Put (To => Answer_String, Item => Answer, Base => 16);

            First := Ada.Strings.Fixed.Index (Answer_String, "#");
            Last  := Ada.Strings.Fixed.Index (Answer_String, "#", Backward);

            Put (File,
              "float64(0x"
              & Answer_String (First + 1 .. Last - 1)
              & ")");
         end;
      end Emit_CP_Double;

      -------------------
      -- Emit_CP_Float --
      -------------------

      procedure Emit_CP_Float (File : Ada.Text_IO.File_Type; Value : Ureal) is
         Num    : Long_Long_Float := 0.0;
         Den    : Long_Long_Float := 0.0;
         Sign   : Long_Long_Float := 1.0;
         Tmp    : Uint;
         Index  : Integer;
         Answer : U4;

      begin
         if UR_Is_Negative (Value) then
            Sign := -1.0;
         end if;

         --  In the following calculus, we consider numbers modulo 2 ** 31,
         --  so that we don't have problems with signed Int...

         Tmp := abs (Numerator (Value));
         Index := 0;
         while Tmp > 0 loop
            Num := Num
              + Long_Long_Float (UI_To_Int (Tmp mod (Uint_2 ** 31)))
              * (2.0 ** Index);
            Tmp := Tmp / Uint_2 ** 31;
            Index := Index + 31;
         end loop;

         Tmp := abs (Denominator (Value));
         if Rbase (Value) /= 0 then
            Tmp := Rbase (Value) ** Tmp;
         end if;

         Index := 0;
         while Tmp > 0 loop
            Den := Den
              + Long_Long_Float (UI_To_Int (Tmp mod (Uint_2 ** 31)))
              * (2.0 ** Index);
            Tmp := Tmp / Uint_2 ** 31;
            Index := Index + 31;
         end loop;

         --  If the denominator denotes a negative power of Rbase,
         --  then multiply by the denominator.

         if Rbase (Value) /= 0 and then Denominator (Value) < 0 then
            Answer := To_U4 (IEEE_Float_32 (Sign * Num * Den));

         --  Otherwise compute the quotient

         else
            Answer := To_U4 (IEEE_Float_32 (Sign * Num / Den));
         end if;

         declare
            package U4_IO is new Ada.Text_IO.Modular_IO (U4);
            Answer_String : String (1 .. 12);
            First : Natural;
            Last  : Natural;

         begin
            U4_IO.Put (To => Answer_String, Item => Answer, Base => 16);

            First := Ada.Strings.Fixed.Index (Answer_String, "#");
            Last  := Ada.Strings.Fixed.Index (Answer_String, "#", Backward);

            Put (File,
              "float32(0x"
              & Answer_String (First + 1 .. Last - 1)
              & ")");
         end;
      end Emit_CP_Float;

      --  Local variables

      Instr     : Instr_Id := First (Method_Seq);
      CIL_Instr : JVM.Code.Instruction;

      pragma Unreferenced (Method);

   --  Start of processing for Emit_Code_Sequence

   begin
      while Instr /= Null_Instr loop
         CIL_Instr := Get (Instr);

         if CIL_Instr.Op = NOP then
            if CIL_Instr.Line_Number /= No_Location then
               Last_Source_Location := CIL_Instr.Line_Number;

               if Opt.Debugger_Level > 0 then
                  Put (File, "      .line");
                  Put (File,
                    Integer'Image
                      (Integer
                        (Sinput.Get_Physical_Line_Number
                          (CIL_Instr.Line_Number))));
                  Put (File,
                    " '"
                    & CIL.Translate_File_Name
                        (Name_Id
                          (Full_Debug_Name
                            (Get_Source_File_Index (CIL_Instr.Line_Number))))
                    & "'");
                  New_Line (File);
               end if;
            end if;

            if CIL_Instr.Label_Def /= Null_Label then
               Put (File, "   L");
               Put (File, Label_Number (CIL_Instr.Label_Def), 0);
               Put (File, ":");
               New_Line (File);
            end if;
         end if;

         if CIL_Instr.Op /= NOP then
            Put (File, "      ");
         end if;

         --  These instructions need to be output after something, or
         --  may be changed altogether (as a multi-dimensional array
         --  is done by a call to a class constructor)

         if CIL_Instr.Op /= NEWARR
           and then CIL_Instr.Op /= SWITCH
           and then CIL_Instr.Op /= NOP
           and then CIL_Instr.Op /= CALL
           and then CIL_Instr.Op /= LDFLD
           and then CIL_Instr.Op /= STFLD
           and then CIL_Instr.Op /= LDFLDA
           and then CIL_Instr.Op /= CLASS_CONVERSION
           and then CIL_Instr.Op /= LDSFLD
           and then CIL_Instr.Op /= STSFLD
           and then CIL_Instr.Op /= RND_R8
         then
            Put (File, Image (CIL_Instr.Op));
         end if;

         case CIL_Instr.Op is
            when NOP =>
               null;

            when BREAK .. STLOC_3 =>
               New_Line (File); --  no parameters

            when LDARG_S .. STLOC_S =>
               Put (File,
                 Integer'Image (Integer (Local_Index (CIL_Instr.Local))));
               New_Line (File); --  uint8 parameter

            when LDNULL .. LDC_I4_8 =>
               New_Line (File); --  no parameter

            when LDC_I4_S =>
               Put (File, " " & Integer'Image (
                 Integer (CIL_Instr.Sint)));
               New_Line (File); --  int8 parameter

            when LDC_I4 =>
               Uintp.UI_Image (Pool_Integer (CIL_Instr.Pool_Item), Decimal);
               Put (File, " " &
                 Uintp.UI_Image_Buffer (1 .. UI_Image_Length));
               New_Line (File); --  int32 parameter

            when LDC_I8 =>
               Uintp.UI_Image (Pool_Long (CIL_Instr.Pool_Item), Decimal);
               Put (File, " " &
                 Uintp.UI_Image_Buffer (1 .. UI_Image_Length));
               New_Line (File); --  int64 parameter

            when LDC_R4 =>
               Put (File, " ");
               Emit_CP_Float (File, Pool_Float (CIL_Instr.Pool_Item));
               New_Line (File); --  float32 parameter

            when LDC_R8 =>
               Put (File, " ");
               Emit_CP_Double (File, Pool_Double (CIL_Instr.Pool_Item));
               New_Line (File); --  float64 parameter

            when DUP .. POP =>
               New_Line (File); --  no parameter

            when JMP =>
               Put (File, " ");
               Put (File, External_Name (Name (CIL_Instr.Target)));
               New_Line (File); --  method parameter (target)

            when CALL =>
               if External_Name
                 (Name (Ref_Method (CIL_Instr.Pool_Item))) = "+"
               then
                  Put (File, "add ");
               else
                  Put (File, "call ");

                  if not Is_Static (Ref_Method (CIL_Instr.Pool_Item)) then
                     Put (File, "instance ");
                  end if;

                  Put (File,
                    Type_String
                      (Result_Type (Ref_Method (CIL_Instr.Pool_Item)))
                    & " ");
                  Put (File,
                    Type_String
                      (Type_Of (Class (Ref_Method (CIL_Instr.Pool_Item))))
                    & "::");
                  Put (File,
                    External_Name (Name (Ref_Method (CIL_Instr.Pool_Item))));
                  Put (File,
                    Params_String (Ref_Method (CIL_Instr.Pool_Item), False));
               end if;

               New_Line (File); --  method parameter (pool)

            when CALLI =>
               Put (File, " ");
               Put (File,
                 Type_String (Result_Type (Ref_Method (CIL_Instr.Pool_Item)))
                 & " ");
               Put (File,
                 Params_String (Ref_Method (CIL_Instr.Pool_Item), False));
               New_Line (File); --  signature parameter

            when RET =>
               New_Line (File); --  no parameter

            when BR_S .. BLT_UN_S =>
               pragma Assert (False);
               New_Line (File); --  int8 parameter

            when BR .. BLT_UN =>
               Put (File, " L");
               Put (File, Label_Number (CIL_Instr.Target), 0);
               New_Line (File); --  label parameter

            when SWITCH =>
               declare
                  Switch_Pair    : Switch_Pair_Id;
                  This_Value     : Int_32;
                  Previous_Value : Int_32;

               begin
                  Switch_Pair := First_Pair (CIL_Instr.Switch_Pairs);
                  Put_Line (File,
                    "ldc.i4 "
                    & Image (Match_Value (Switch_Pair)));
                  Put_Line (File, "      sub");
                  Put_Line (File, "      switch (");
                  Put (File, "              L");
                  Put (File, Label_Number (Match_Label (Switch_Pair)), 0);

                  Previous_Value := Match_Value (Switch_Pair);

                  loop
                     Switch_Pair := Next_Pair (Switch_Pair);
                     exit when Switch_Pair = Null_Switch_Pair;
                     This_Value := Match_Value (Switch_Pair);

                     --  We have switch pairs for all values, so if
                     --  something is missing, it should go to the default
                     --  label

                     for J in 1 .. This_Value - Previous_Value - 1 loop
                        Put_Line (File, ",");
                        Put (File, "              L");
                        Put (File, Label_Number (CIL_Instr.Default_Label), 0);
                     end loop;

                     Put_Line (File, ",");
                     Put (File, "              L");
                     Put (File, Label_Number (Match_Label (Switch_Pair)), 0);

                     Previous_Value := This_Value;
                  end loop;
               end;

               Put_Line (File, ")");
               Free_Switch_List (CIL_Instr.Switch_Pairs);

               Put (File, "      br L");
               Put (File, Label_Number (CIL_Instr.Default_Label), 0);
               New_Line (File); --  bunch of label parameters

            when LDIND_I1 .. CONV_U8 =>
               New_Line (File); --  no parameter

            when CALLVIRT =>
               Put (File, " instance ");
               Put (File,
                 Type_String (Result_Type (Ref_Method (CIL_Instr.Pool_Item)))
                 & " ");
               Put (File,
                 Type_String
                   (Type_Of (Class (Ref_Method (CIL_Instr.Pool_Item))))
                 & "::");
               Put (File,
                 External_Name (Name (Ref_Method (CIL_Instr.Pool_Item))));
               Put (File,
                 Params_String (Ref_Method (CIL_Instr.Pool_Item), False));
               New_Line (File); --  method parameter

            when CPOBJ .. LDOBJ =>
               Put (File,
                 " " & External_Name (Ref_Class (CIL_Instr.Pool_Item)));
               New_Line (File); --  class (valuetype) parameter

            when LDSTR =>
               Put (File, " ");

               begin
                  declare
                     S : constant String :=
                           Str (Pool_String (CIL_Instr.Pool_Item));
                  begin
                     --  String parameter, quotes added by Add_Slashes if
                     --  needed or maybe will by bytearray
                     Put_Line (File, Add_Slashes (S));
                  end;
               exception
                  when others =>
                     --  The above does not handle wide characters, so we may
                     --  end up with an assert failure.

                     if Last_Source_Location /= No_Location then
                        Error_Msg
                          ("wide characters not supported",
                           Last_Source_Location);
                     end if;

                     raise;
               end;

            when NEWOBJ =>
               Put (File, " ");
               Put (File, " instance ");
               Put (File,
                 Type_String (Result_Type (Ref_Method (CIL_Instr.Pool_Item)))
                 & " ");
               Put (File,
                 Type_String
                   (Type_Of (Class (Ref_Method (CIL_Instr.Pool_Item))))
                 & "::");
               Put (File,
                 External_Name (Name (Ref_Method (CIL_Instr.Pool_Item))));
               Put (File,
                 Params_String (Ref_Method (CIL_Instr.Pool_Item), False));
               New_Line (File); --  method parameter

            when CASTCLASS .. ISINST | UNBOX | BOX =>
               Put (File, " ");

               if Is_Array_Type (CIL_Instr.Pool_Item) then
                  Put (File,
                    Type_String (Ref_Class_Type (CIL_Instr.Pool_Item)));
               else
                  Put (File,
                    External_Name (Ref_Class (CIL_Instr.Pool_Item)));
               end if;

               New_Line (File); --  type parameter

            when CONV_R_UN | THROW  =>
               New_Line (File); --  no parameter

            when LDFLD .. STSFLD =>
               Put (File, Image (CIL_Instr.Op));
               Put (File, " " & Type_String (Field_Type (CIL_Instr.Field)));
               Put (File, " " & External_Name (CIL_Instr.Class));
               Put (File, "::" & External_Name (Name (CIL_Instr.Field)));
               New_Line (File); --  field parameter

            when STOBJ =>
               Put (File,
                 " " & External_Name (Ref_Class (CIL_Instr.Pool_Item)));
               New_Line (File); --  type parameter

            when CONV_OVF_I1_UN .. CONV_OVF_U_UN =>
               New_Line (File); --  no parameter

            when LDELEMA =>
               Put (File, " ");

               if Type_Kind (Ref_Type (CIL_Instr.Pool_Item)) = Class_Kind then
                  Put (File,
                    External_Name
                      (Class_Of_Type (Ref_Type (CIL_Instr.Pool_Item))));
               else
                  Put (File,
                    External_Name (Name (Ref_Type (CIL_Instr.Pool_Item))));
               end if;

               New_Line (File); --  no parameter

            when RND_R8 =>
               Put (File,
                 "call float64 class "
                 & "[mgnat]mgnat.adalib.GNAT_libc::round (float64)");
               New_Line (File);

            when NEWARR =>
               if CIL_Instr.Dimensions > 1 then
                  --  Put (File, "newobj instance void ");
                  --  put the type name on the stack and store it
                  --  in the array_constructor class

                  Put (File, "ldstr """);

                  if Type_Kind (CIL_Instr.Element_Type) = Class_Kind then
                     declare
                        X : constant Class_Id :=
                              Class_Of_Type (CIL_Instr.Element_Type);
                        Y : constant String := External_Name (X);

                     begin
                        if Y = "Standard.String" then
                           Put (File, "System.Byte[]");

                        elsif Y = "unsigned int8[]" then
                           Put (File, "System.Byte[]");

                        elsif Y = "int8[]" then
                           Put (File, "System.SByte[]");

                        else
                           Put (File, Y);
                        end if;
                     end;
                  else
                     declare
                        Y : constant String := Type_String (
                          CIL_Instr.Element_Type);
                     begin
                        if Y = "Standard.String" then
                           Put (File, "System.Byte[]");

                        elsif Y = "bool" then
                           Put (File, "System.Boolean");

                        elsif Y = "int8" then
                           Put (File, "System.SByte");

                        elsif Y = "int8[]" then
                           Put (File, "System.SByte[]");

                        elsif Y = "unsigned int8" then
                           Put (File, "System.Byte");

                        elsif Y = "unsigned int8[]" then
                           Put (File, "System.Byte[]");

                        elsif Y = "char" then
                           Put (File, "System.Char");

                        elsif Y = "int16" then
                           Put (File, "System.Int16");

                        elsif Y = "unsigned int16" then
                           Put (File, "System.UInt16");

                        elsif Y = "int32" then
                           Put (File, "System.Int32");

                        elsif Y = "unsigned int32" then
                           Put (File, "System.UInt32");

                        elsif Y = "int64" then
                           Put (File, "System.Int64");

                        elsif Y = "unsigned int64" then
                           Put (File, "System.UInt64");

                        elsif Y = "float32" then
                           Put (File, "System.Single");

                        elsif Y = "float64" then
                           Put (File, "System.Double");

                        else
                           Put (File, Y);
                        end if;
                     end;
                  end if;

                  Put_Line (File, """");

                  Put (File, "      ");
                  Put (File, "call void [mgnat]mgnat.adalib.");
                  Put (File, "array_constructor::set_type_name(string)");
                  New_Line (File);

                  Put (File, "      ");
                  Put (File, "call class [mscorlib]System.Array ");
                  Put (File, "[mgnat]mgnat.adalib.");
                  Put (File, "array_constructor::make_array(");
                  Put (File, Integer (CIL_Instr.Dimensions - 1) * "int32,");
                  Put (File, "int32)");
                  New_Line (File);

                  Put (File, "      ");
                  Put (File, "castclass ");

                  if Type_Kind (CIL_Instr.Element_Type) = Class_Kind then
                     declare
                        X : constant Class_Id :=
                              Class_Of_Type (CIL_Instr.Element_Type);
                        Y : constant String := External_Name (X);

                     begin
                        Put (File, "class ");

                        if Y /= "Standard.String" then
                           Put (File, Y);
                        else
                           Put (File, "unsigned int8[]");
                        end if;
                     end;
                  else
                     declare
                        Y : constant String :=
                              Type_String (CIL_Instr.Element_Type);
                     begin
                        if Y /= "Standard.String" then
                           Put (File, Y);
                        else
                           Put (File, "unsigned int8[]");
                        end if;
                     end;
                  end if;

                  Put (File, Integer (CIL_Instr.Dimensions) * "[]");
                  New_Line (File);

               else
                  Put (File, "newarr ");

                  if Type_Kind (CIL_Instr.Element_Type) = Class_Kind then
                     declare
                        X : constant Class_Id :=
                              Class_Of_Type (CIL_Instr.Element_Type);
                        Y : constant String := External_Name (X);

                     begin
                        if Y /= "Standard.String" then
                           Put (File, Y);
                        else
                           Put (File, "unsigned int8[]");
                        end if;
                     end;
                  else
                     declare
                        Y : constant String :=
                              Type_String (CIL_Instr.Element_Type);
                     begin
                        if Y /= "Standard.String" then
                           Put (File, Y);
                        else
                           Put (File, "unsigned int8[]");
                        end if;
                     end;
                  end if;
               end if;

               New_Line (File); --  type parameter

            when LDLEN | LDELEM_I1 .. CONV_OVF_U8 | CKFINITE =>
               New_Line (File); --  no parameter

            when REFANYVAL | MKREFANY =>
               Put (File, " ");
               Put (File, External_Name (Ref_Class (CIL_Instr.Pool_Item)));
               New_Line (File); --  type parameter

            when LDTOKEN =>
               pragma Assert (False);
               New_Line (File); --  type/field/method parameter

            when CONV_U2 .. ENDFINALLY | STIND_I .. CLT_UN =>
               New_Line (File); --  no parameter

            when LEAVE =>
               if CIL_Instr.Target /= Null_Label then
                  Put (File, " L");
                  Put (File, Label_Number (CIL_Instr.Target), 0);
               else
                  Put (File, " Return");
               end if;

               New_Line (File); --  int32 parameter

            when LEAVE_S =>
               pragma Assert (False);
               New_Line (File); --  int8 parameter

            when LDFTN .. LDVIRTFTN =>
               if not Is_Static (Ref_Method (CIL_Instr.Pool_Item)) then
                  Put (File, " instance ");
               else
                  Put (File, " ");
               end if;

               Put (File,
                 Type_String (Result_Type (Ref_Method (CIL_Instr.Pool_Item)))
                 & " ");
               Put (File,
                 External_Name (Class (Ref_Method (CIL_Instr.Pool_Item)))
                 & "::");
               Put (File,
                 External_Name (Name (Ref_Method (CIL_Instr.Pool_Item))));
               Put (File,
                 Params_String (Ref_Method (CIL_Instr.Pool_Item), False));
               New_Line (File); --  method parameter

            when LDARG .. STLOC =>
               Put (File,
                 Integer'Image (Integer (Local_Index (CIL_Instr.Local))));

               --  Add comment to the right of instructions load and store
               --  to leave more visible the associated variable and thus
               --  facilitate reading the generated CIL code.

               Put (File, "  // " & External_Name (Name (CIL_Instr.Local)));

               --  Add also character '*' to descriptors to leave them more
               --  visible in the output file.

               if Is_Descriptor (Type_Of (CIL_Instr.Local)) then
                  Put (File, "*");
               end if;

               New_Line (File); --  uint32 parameter

            when LOCALLOC .. ENDFILTER | VOLATILE .. TAIL |
               CPBLK .. RETHROW | REFANYTYPE =>
               New_Line (File); --  no parameter

            when UNALIGNED =>
               pragma Assert (False);
               New_Line (File); --  uint8 parameter

            when INITOBJ =>
               Put (File,
                 " " & External_Name (Ref_Class (CIL_Instr.Pool_Item)));
               New_Line (File); --  type parameter

            when SIZEOF =>
               Put (File,
                 " " & Type_String (Ref_Type (CIL_Instr.Pool_Item)));
               New_Line (File); --  type parameter

            when CLASS_CONVERSION =>
               Put_Line
                 (File,
                  "ldtoken "
                  & External_Name (Ref_Class (CIL_Instr.Pool_Item)));
               Put_Line
                 (File,
                  "      call object class "
                  & "[mgnat]mgnat.adalib.object_convert::class_conversion "
                  &  "(object, valuetype [mscorlib]System.RuntimeTypeHandle)");
               Put
                 (File,
                  "      castclass "
                  & External_Name (Ref_Class (CIL_Instr.Pool_Item)));
               New_Line (File); --  object parameter

            when others =>
               pragma Assert (False);
               New_Line (File); --  no parameter
         end case;

         Instr := CIL_Instr.Next;
      end loop;
   end Emit_Code_Sequence;

   ---------------
   -- Emit_Code --
   ---------------

   procedure Emit_Code
     (File : Ada.Text_IO.File_Type; Method : Method_Id)
   is
      procedure Generate_Handler_Entries
        (File   : Ada.Text_IO.File_Type;
         Method : Method_Id);

      ------------------------------
      -- Generate_Handler_Entries --
      ------------------------------

      procedure Generate_Handler_Entries
        (File   : Ada.Text_IO.File_Type;
         Method : Method_Id)
      is
         Handler_Seq : Handler_Sequence := Method_Handlers (Method);
         Hdlr_Id     : Handler_Id       := First (Handler_Seq);
         Hdlr_Entry  : Handler_Entry;

      begin
         while Hdlr_Id /= Null_Handler loop
            Hdlr_Entry := Get (Hdlr_Id);

            Put (File, "      .try L");
            Put (File, Label_Number (Hdlr_Entry.Start_Lbl), 0);
            Put (File, " to L");
            Put (File, Label_Number (Hdlr_Entry.End_Lbl), 0);
            New_Line (File);

            if Hdlr_Entry.Kind = Non_Filter then
               Put (File, "         catch ");

               if Hdlr_Entry.Exc_Class = Null_Pool_Item then
                  Put_Line (File, "[mscorlib]System.Exception");
               else
                  Put_Line (File,
                    External_Name (Ref_Class (Hdlr_Entry.Exc_Class)));
               end if;
            else
               Put (File, "            filter L");
               Put (File, Label_Number (Hdlr_Entry.Filter_Lbl), 0);
            end if;

            Put (File, "            handler L");
            Put (File, Label_Number (Hdlr_Entry.Handler_Lbl), 0);
            Put (File, " to L");
            Put (File, Label_Number (Hdlr_Entry.End_Handler_Lbl), 0);
            New_Line (File);

            Hdlr_Id := Hdlr_Entry.Next;
         end loop;

         if First (Handler_Seq) /= Null_Handler then
            Put_Line (File, "Return:");

            if Result_Type (Method) /= Void_Type then
               Put_Line (File, "      ldloc _retval");
            end if;

            Put_Line (File, "      ret");
         end if;

         Free_Sequence (Handler_Seq);
      end Generate_Handler_Entries;

      --  Local variables

      Method_Seq  : Code_Sequence := Method_Code (Method);
      Local_Count : Integer := 0;

   --  Start of processing for Emit_Code

   begin
      --  Need to skip this for an Export Stdcall method

      if Exported_Stdcall (Method) /= No_String then
         Free_Sequence (Method_Seq);
         return;
      end if;

      --  Switch -gnatd.o generates the listing of CIL code

      if Debug_Flag_Dot_O then
         Print_Jcode (Method);
      end if;

      --  Do not analyze the code if the backend reported errors on
      --  unsupported constructs or if the verifier is disabled.

      if Serious_Errors_Detected = 0
        and then Debug_Flag_Dot_P
      then
         --  Avoid calling the verifier if the number of CIL instructions
         --  exceeds the capacity of its internal data structures

         declare
            Num_CIL_Instr : constant Natural :=
                              Count_Sequence (Method_Code (Method));

         begin
            if Num_CIL_Instr < 50000 then
               Verify (Method);

            else
               Write_Str ("warning: cannot verify method ");
               Write_Name (Name (Method));
               Write_Str (" (" & Num_CIL_Instr'Img & " CIL instructions )");
               Write_Eol;
            end if;
         end;
      end if;

      Put_Line (File, "      .maxstack " &
        Integer'Image (Integer (Max_Stack_Depth (Method))));

      declare
         Param          : Local_Var_Id := First_Local_Var (Method);
         Is_First_Local : Boolean      := True;

      begin
         if not Is_Static (Method) then
            Param := Next_Local_Var (Param);
         end if;

         loop
            exit when Param = Null_Local_Var;

            if not Is_Param (Param) then
               if Is_First_Local then
                  Put (File, "      .locals init(");
                  Is_First_Local := False;
               else
                  Put (File, "      ");
               end if;

               --  On the JVM, the parameters are treated just like
               --  local variables, so the counter is offset by
               --  the number of parameters.  In CIL, you need
               --  to restart the counter at zero, so I reset
               --  all of the local variable indices here

               Set_Local_Index (Param, Local_Variable_Index (Local_Count));
               Local_Count := Local_Count + 1;

               Put (File, "[");
               Put (File, Integer (Local_Index (Param)), 0);
               Put (File, "]");
               Put (File, Type_String (Type_Of (Param)));
               Put (File, " ");
               Put (File, External_Name (Name (Param)));
            end if;

            Param := Next_Local_Var (Param);

            if Param /= Null_Local_Var
              and then not Is_First_Local
            then
               Put_Line (File, ",");

            elsif not Is_First_Local then
               Put_Line (File, ")");
            end if;
         end loop;
      end;

      Emit_Code_Sequence (File, Method, Method_Seq);

      Generate_Handler_Entries (File, Method);

      --  Set the Line number attribute table and the Local_Variable
      --  table. This has to be done after the Generate_Code function

      --  Emit_Line_Numbers (Code_Info.Attributes, Method);
      --  Emit_Local_Variables (Code_Info, Method);

      Free_Sequence (Method_Seq);
   end Emit_Code;

   -----------------
   -- Emit_Method --
   -----------------

   procedure Emit_Method (File : Ada.Text_IO.File_Type; Method : Method_Id) is

      function Extract_Library (Subp : String) return String;
      --  Get the library of [library]subprogram

      ---------------------
      -- Extract_Library --
      ---------------------

      function Extract_Library (Subp : String) return String is
         Loc1, Loc2 : Integer;

      begin
         Loc1 := Index (Subp, "[");
         Loc2 := Index (Subp, "]");
         return Subp (Loc1 + 1 .. Loc2 - 1);
      end Extract_Library;

      --  Local variables

      Subp : constant Entity_Id := Ada_Entity (Method);

   --  Start of processing for Emit_Method

   begin
      --  Disable generation of predefined primitives in interface derivations
      --  because they are inherited from its parent interface.

      if Present (Subp)
        and then Is_Dispatching_Operation (Subp)
        and then Is_Predefined_Dispatching_Operation (Subp)
        and then Is_Interface (Find_Dispatching_Type (Subp))

         --  Exclude inherited predefined primitives (this case occurs
         --  in interface derivations)

        and then (Present (Alias (Subp))

         --  Exclude also predefined primitives that are still unsupported???
         --  Supported predefined primitives are "=", assign

                    or else Chars (Subp) = Snames.Name_uAlignment
                    or else Chars (Subp) = Snames.Name_uSize
                    or else Get_TSS_Name (Subp) = TSS_Stream_Input
                    or else Get_TSS_Name (Subp) = TSS_Stream_Output
                    or else Get_TSS_Name (Subp) = TSS_Stream_Read
                    or else Get_TSS_Name (Subp) = TSS_Stream_Write)
      then
         return;
      end if;

      Put (File, "   .method ");

      case Access_Mode (Method) is
         when Public_Access =>
            Put (File, "public ");

         when Package_Access =>
            Put (File, "assembly ");

         when Protected_Access =>
            Put (File, "famorassem ");

         when Private_Access =>
            Put (File, "private ");
      end case;

      if Exported_Stdcall (Method) /= No_String
        or else Is_Delegate (Method)
      then
         Put (File, "hidebysig ");
      end if;

      if Is_Abstract (Method) then
         Put (File, "abstract ");
      end if;

      if Is_Static (Method) then
         Put (File, "static ");

      elsif Name_String (Name (Method)) = ".ctor" then
         if Is_Delegate (Method) then
            Put (File, "specialname rtspecialname instance ");
         else
            Put (File, "specialname instance ");
         end if;

      else
         Put (File, "virtual instance ");
      end if;

      if Is_Final (Method) then
         Put (File, "final ");
      end if;

      if Is_Synchronized (Method) then
         Put (File, "synchronized ");
      end if;

      if Exported_Stdcall (Method) /= No_String then
         Put (File, "pinvokeimpl(""" &
           Extract_Library (Str (Exported_Stdcall (Method))) &
           """ winapi) ");
      end if;

      Put (File, Type_String (Result_Type (Method)) & " ");
      Put (File, Check_Reserved (Name_String (Name (Method))));

      Put (File, Params_String (Method));

      if Is_Delegate (Method) then
         Put (File, " runtime managed");

      elsif Exported_Stdcall (Method) /= No_String then
         Put (File, " cil managed preservesig");
      end if;

      Put_Line (File, " { ");

      if Name_String (Name (Method)) = "_main" then
         Put_Line (File, "      .entrypoint");
      end if;

      if Is_Abstract (Method) or else Is_Delegate (Method) then
         null;

      --  Nonabstract methods have two attributes: one for their associated
      --  bytecode and the other to indicate the set of exceptions that are
      --  thrown by the method (which we currently treat as empty and which
      --  is not enforced by the JVM in any case).

      else
         Emit_Code (File, Method);
      end if;

      Put (File,
        "   } // end .method "
        & Check_Reserved
            (Trim_Namespace (Name_String (Name (Class (Method)))))
        & "::"
        & Check_Reserved (Name_String (Name (Method))));

      if Is_Interface_Wrapper (Method) then
         Put (File,
           " ("
           & Check_Reserved
               (Trim_Namespace
                 (Name_String (Name (Class_Of_Wrapped_Interface (Method)))))
           & " wrapper)");
      end if;

      New_Line (File);
      New_Line (File);
   end Emit_Method;

   --------------------
   -- Trim_Namespace --
   --------------------

   function Trim_Namespace (X : String) return String is
      Where : Natural;

   begin
      Where := Index (X, ".", Backward);
      return X (Where + 1 .. X'Last);
   end Trim_Namespace;

   ----------------
   -- Emit_Class --
   ----------------

   procedure Emit_Class
     (File  : Ada.Text_IO.File_Type;
      Class : Class_Id)
   is
      procedure Emit_Field (File : Ada.Text_IO.File_Type; Field : Field_Id);
      function Namespace_Of (X : String) return String;
      function Needs_Namespace (X : String) return Boolean;

      ----------------
      -- Emit_Field --
      ----------------

      procedure Emit_Field (File : Ada.Text_IO.File_Type; Field : Field_Id) is
      begin
         Put (File, "   .field public ");

         if Is_Static (Field) then
            Put (File, "static ");
         end if;

         Put (File, Type_String (Type_Of (Field)) & " ");
         Put_Line (File, External_Name (Name (Field)));
      end Emit_Field;

      ------------------
      -- Namespace_Of --
      ------------------

      function Namespace_Of (X : String) return String is
         Where : Natural;
      begin
         Where := Index (X, ".", Backward);
         return X (X'First .. Where - 1);
      end Namespace_Of;

      ---------------------
      -- Needs_Namespace --
      ---------------------

      function Needs_Namespace (X : String) return Boolean is
      begin
         return Index (X, ".") >= X'First;
      end Needs_Namespace;

   --  Start of processing for Emit_Class

   begin
      if Outer_Class (Class) = Null_Class
        and then Needs_Namespace (Name_String (Name (Class)))
      then
         Put_Line (File,
           ".namespace "
           & Check_Reserved (Namespace_Of (Name_String (Name (Class))))
           & " {");
      end if;

      Put (File, ".class ");

      if Outer_Class (Class) /= Null_Class then
         Put (File, "nested ");
      end if;

      if Is_Public (Class) then
         Put (File, "public ");
      else
         Put (File, "private ");
      end if;

      if Is_Final (Class) then
         Put (File, "sealed ");
      end if;

      if Is_Interface (Class) then
         Put (File, "interface ");
      end if;

      if Is_Abstract (Class) then
         Put (File, "abstract ");
      end if;

      Put (File, "serializable ");

      Put (File,
        Check_Reserved (Trim_Namespace (Name_String (Name (Class)))));

      if Type_Of (Class) /= Null_Type
        and then Is_Descriptor (Type_Of (Class))
      then
         Put (File, " // Descriptor");
      end if;

      New_Line (File);

      --  Handled type extensions and covered interface types

      if Is_Interface (Class) then
         if Is_Interface (Superclass (Class)) then
            Put (File, "   implements ");
            Put (File, External_Name (Superclass (Class)));
         end if;

      else
         --  In the VM we cannot derive a class from an interface type. Hence,
         --  if the parent is an interface type we do the following
         --  transformation:
         --      type DT is new I1 and I2 with ...
         --  ... is generated as:
         --      type DT is new System.Object and I1 and I2 ...

         if not Is_Interface (Superclass (Class)) then
            Put (File, "   extends " & External_Name (Superclass (Class)));
         else
            Put (File, "   extends [mscorlib]System.Object");
            New_Line (File);
            Put (File, "   implements ");
            Put (File, External_Name (Superclass (Class)));
         end if;
      end if;

      --  Step: Establish references to all implemented interfaces

      declare
         Interface_Ref : JVM_Entity_Ref := First_Interface_Ref (Class);

      begin
         if Interface_Ref /= Null_Entity_Ref then
            if not Is_Interface (Superclass (Class)) then
               New_Line (File);
               Put (File, "   implements ");
            else
               Put (File, ", ");
            end if;

            --  Add a reference to interfaces implemented by this class

            Put (File, External_Name (Get_Interface (Interface_Ref)));

            loop
               Interface_Ref := Next_Interface_Ref (Interface_Ref);
               exit when Interface_Ref = Null_Entity_Ref;
               Put_Line (File, ",");
               Put (File,
                 "         "
                 & External_Name (Get_Interface (Interface_Ref)));
            end loop;

            New_Line (File);
         end if;
      end;

      Put_Line (File, " {");

      --  Step: Emit inner classes

      declare
         Inner_Class : Class_Id := First_Nested_Class (Class);

      begin
         while Inner_Class /= Null_Class loop
            Emit_Class (File, Inner_Class);
            Inner_Class := Next_Nested_Class (Inner_Class);
         end loop;
      end;

      --  Step: Emit fields

      declare
         Field : Field_Id := First_Field (Class);

      begin
         --  ??? Interfaces don't have fields. The front end will add a tag to
         --  them and we probably should try to fix it there but it is much
         --  easier to do here.

         while Field /= Null_Field and not Is_Interface (Class) loop
            Emit_Field (File => File, Field => Field);
            Field := Next_Field (Field);
         end loop;
      end;

      --  Step: Emit method info

      declare
         Method : Method_Id := First_Method (Class);

      begin
         while Method /= Null_Method loop
            --  Takes care of emitting code and exception tables
            --  for each method.

            Emit_Method (File, Method);
            Method := Next_Method (Method);
         end loop;
      end;

      --  Step: Emit .override directives

      declare
         Tag_Typ : constant Entity_Id := Ada_Entity (Class_Type (Class));

         Disp_Method  : Method_Id;
         Iface_Method : Method_Id;
         Elmt         : Elmt_Id;
         Prim         : Entity_Id;
         Ifaces_List  : Elist_Id;
         Iface        : Elmt_Id;
         Iface_Prim   : Elmt_Id;
         TSS_Name     : TSS_Name_Type;

      begin
         if Present (Tag_Typ)
           and then not Is_Interface (Tag_Typ)
           and then Has_Interfaces (Tag_Typ)
           and then not Is_Protected_Type (Tag_Typ)
           and then not Is_Task_Type (Tag_Typ)
         then
            Collect_Interfaces (Tag_Typ, Ifaces_List,
              Exclude_Parents => True);

            Elmt := First_Elmt (Primitive_Operations (Tag_Typ));
            while Present (Elmt) loop
               Prim := Node (Elmt);

               if Is_Predefined_Dispatching_Operation (Prim)

                  --  Exclude predefined primitives for which we generate a
                  --  wrapper

                 and then Chars (Prim) /= Snames.Name_Op_Eq
                 and then Chars (Prim) /= Snames.Name_uAssign

                  --  The .override directive must only reference methods
                  --  defined for the current class (inherited methods are
                  --  not allowed).

                 and then No (Alias (Prim))

                  --  Exclude unsupported predefined primitives???

                  --  Note: These cases must be also left excluded in
                  --  subprogram Emit_Method

                 and then Chars (Prim) /= Snames.Name_uAlignment
                 and then Chars (Prim) /= Snames.Name_uSize
                 and then Get_TSS_Name (Prim) /= TSS_Stream_Input
                 and then Get_TSS_Name (Prim) /= TSS_Stream_Output
                 and then Get_TSS_Name (Prim) /= TSS_Stream_Read
                 and then Get_TSS_Name (Prim) /= TSS_Stream_Write
                 and then Get_TSS_Name (Prim) /= TSS_Deep_Adjust
                 and then Get_TSS_Name (Prim) /= TSS_Deep_Finalize
               then
                  Disp_Method := Next_Method (JVM_Method (Prim));
                  TSS_Name := Get_TSS_Name (Prim);

                  Iface := First_Elmt (Ifaces_List);
                  while Present (Iface) loop

                     if Etype (Node (Iface)) = Node (Iface)
                       and then Node (Iface) /= Root_Type (Tag_Typ)
                     then
                        --  Search for the partner entity in the interface type

                        Iface_Prim :=
                          First_Elmt (Primitive_Operations (Node (Iface)));
                        while Present (Iface_Prim) loop
                           if TSS_Name /= TSS_Null then
                              exit when TSS_Name
                                          = Get_TSS_Name (Node (Iface_Prim));
                           else
                              exit when Chars (Prim)
                                          = Chars (Node (Iface_Prim));
                           end if;

                           Next_Elmt (Iface_Prim);
                        end loop;
                        pragma Assert (Present (Iface_Prim));

                        Iface_Method := JVM_Method (Node (Iface_Prim));

                        Put_Line (File,
                          "   .override "
                          & Check_Reserved
                             (Name_String (Name (Class_Of (Iface_Method))))
                          & "::"
                          & Check_Reserved
                             (Name_String (Name (Iface_Method))));

                        Put_Line (File,
                          "     with instance "
                          & Type_String (Result_Type (Disp_Method))
                          & " "
                          & Check_Reserved
                             (Name_String (Name (Associated_Class (Prim))))
                          & "::"
                          & Check_Reserved (Name_String (Name (Disp_Method))));

                        Put_Line (File,
                          "      "
                          & Params_String (Disp_Method));
                     end if;

                     Next_Elmt (Iface);
                  end loop;
               end if;

               Next_Elmt (Elmt);
            end loop;
         end if;
      end;

      Put_Line (File,
        "} // end .class "
        & Check_Reserved (Trim_Namespace (Name_String (Name (Class)))));

      if Outer_Class (Class) = Null_Class
        and then Needs_Namespace (Name_String (Name (Class)))
      then
         Put_Line (File,
           "} // end .namespace "
           & Check_Reserved (Namespace_Of (Name_String (Name (Class)))));
         New_Line (File);
      end if;
   end Emit_Class;

   ------------------------
   -- Produce_Class_File --
   ------------------------

   procedure Produce_Class_File (Class : Class_Id) is

      procedure Generate_External_Assemblies (File : Ada.Text_IO.File_Type);

      procedure Emit_Assembly_Info
        (To_File      : Ada.Text_IO.File_Type;
         AssemblyName : String);

      ------------------------
      -- Emit_Assembly_Info --
      ------------------------

      procedure Emit_Assembly_Info
        (To_File      : Ada.Text_IO.File_Type;
         AssemblyName : String)
      is
         Info_File : Ada.Text_IO.File_Type;
         Line      : String (1 .. 500);
         Last      : Natural;
         Fullspec  : Boolean := False;
      begin
         Open (Info_File, In_File, "assemblyinfo");
         Get_Line (Info_File, Line, Last);

         --  The first line should be ".assembly ....."

         if Last > 9 and then Line (1 .. 9) = ".assembly" then
            Fullspec := True;
         else
            --  No full assemblyspec (just contents)
            --  Create a default assembly header
            Put_Line (To_File,
              ".assembly " & Check_Reserved (AssemblyName) & " {");
         end if;

         while not End_Of_File (Info_File) loop
            Put_Line (To_File, Line (1 .. Last));
            Get_Line (Info_File, Line, Last);
         end loop;

         Put_Line (To_File, Line (1 .. Last));

         if not Fullspec then
            Put_Line (To_File, " }");
         end if;

         Close (Info_File);

      exception
         when Name_Error | Mode_Error | Use_Error =>
            Put_Line
              (To_File,
               ".assembly " & Check_Reserved (AssemblyName) & " {");
            Put_Line (To_File, "  .ver 0:0:0:0");
            Put_Line (To_File, " }");
      end Emit_Assembly_Info;

      ----------------------------------
      -- Generate_External_Assemblies --
      ----------------------------------

      procedure Generate_External_Assemblies (File : Ada.Text_IO.File_Type) is
         Names        : constant String  := To_String (CIL.Assembly_Names);
         Start        : Natural := Names'First;
         Finish       : Natural;
         Next         : Natural;
         Middle       : Natural;
         Retargetable : Natural;

      begin
         loop
            Finish := Index (Names (Start .. Names'Last), ",");
            exit when Finish < Start;

            Next := Finish + 1;
            Middle := Index (Names (Start .. Finish), "#");

            if Middle > Start then
               Retargetable := Index
                 (Names (Middle + 1 .. Finish - 1), "retargetable");

               if Retargetable > Middle + 1 then
                  Finish := Retargetable;

                  Put (File, ".assembly extern retargetable ");
               else
                  Put (File, ".assembly extern ");
               end if;

               Put_Line
                 (File,
                  Check_Reserved (Names (Start + 1 .. Middle - 1))
                  & " { "
                  & Names (Middle + 1 .. Finish - 1)
                  & " }");
            else
               Put_Line (File,
                 ".assembly extern "
                 & Check_Reserved (Names (Start + 1 .. Finish - 1))
                 & " { }");
            end if;

            Start := Next;
         end loop;
      end Generate_External_Assemblies;

      --  Local variables

      C_Name      : constant String :=
                      CIL.Translate_File_Name
                        (Name_Id (Unit_File_Name (Main_Unit)));
      File        : Ada.Text_IO.File_Type;
      First_Index : Natural;

   --  Start of processing for Produce_Class_File

   begin
      if CIL.First_Class_Opened then
         Name_Buffer (1 .. 12) := "mscorlib.txt";
         Name_Len := 12;

         Read_Source_File
           (Name_Find,
            Lo  => 0,
            Hi  => CIL.Mscorlib_Hi,
            Src => CIL.Mscorlib_Text);

         Name_Buffer (1 .. 11) := "gnatlib.txt";
         Name_Len := 11;

         Read_Source_File
           (Name_Find,
            Lo  => 0,
            Hi  => CIL.Gnatlib_Hi,
            Src => CIL.Gnatlib_Text);

         Create
           (File => File,
            Mode => Out_File,
            Name => CIL.Output_File_Name);

         --  Add a comment to the the header of the IL file with the version
         --  of GNAT used to generate it

         Put_Line (File,
           "// File generated by GNAT "
           & Gnat_Version_String);

         New_Line (File);

         --  Generate the header of the external libs

         Put (File,
           String (CIL.Mscorlib_Text (0 .. CIL.Mscorlib_Hi - 1)));

         if not Opt.GNAT_Mode then
            Put (File,
              String (CIL.Gnatlib_Text (0 .. CIL.Gnatlib_Hi - 1)));
         end if;

         CIL.First_Class_Opened := False;

      else
         Open
           (File => File,
            Mode => Append_File,
            Name => CIL.Output_File_Name);
      end if;

      Generate_External_Assemblies (File);

      if CIL.Mscorlib_Text = null then
         Write_Line ("fatal error, run-time library not installed correctly");
         Write_Line ("cannot locate file mscorlib.txt");
         raise Unrecoverable_Error;
      end if;

      if CIL.Gnatlib_Text = null then
         Write_Line ("fatal error, run-time library not installed correctly");
         Write_Line ("cannot locate file gnatlib.txt");
         raise Unrecoverable_Error;
      end if;

      New_Line (File);

      if C_Name'Length > 4
        and then (C_Name (C_Name'First .. C_Name'First + 1) = "b~"
                  or else C_Name (C_Name'First .. C_Name'First + 2) = "b__")
      then
         First_Index := C_Name'First + 2;

         if C_Name (C_Name'First + 1) = '_' then
            First_Index := First_Index + 1;
         end if;

         for J in reverse First_Index .. C_Name'Last - 1 loop
            if C_Name (J) = '-' then
               First_Index := J + 1;
               exit;
            end if;
         end loop;

         Emit_Assembly_Info
           (File, C_Name (First_Index .. Index (C_Name, ".") - 1));
      end if;

      Emit_Class (File, Class);

      Close (File => File);
   end Produce_Class_File;

end JVM.Emit;
