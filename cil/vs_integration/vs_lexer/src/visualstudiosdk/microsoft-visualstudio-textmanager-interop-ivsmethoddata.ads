-- Autogenerated by CIL2Ada v. 2
with MSSyst.Object;
with CIL_Types;
use CIL_Types;
limited with Microsoft.VisualStudio.TextManager.Interop.MethodTextType;
limited with Microsoft.VisualStudio.TextManager.Interop.ParameterTextType;
package Microsoft.VisualStudio.TextManager.Interop.IVsMethodData is
   type Typ is interface;
   type Ref is access all Typ'Class;
   type Ref_addrof is access all Ref;
   type Ref_Arr is array (Natural range <>) of Ref;
   type Ref_Array is access all Ref_Arr;
   type Ref_Array_addrof is access all Ref_Array;
   function GetContextStream
     (This : access Typ;
      piPos : CIL_Types.Int32_addrof;
      piLength : CIL_Types.Int32_addrof) return Integer is abstract;
   function GetCurMethod
     (This : access Typ) return Integer is abstract;
   function GetCurrentParameter
     (This : access Typ;
      iMethod : Integer) return Integer is abstract;
   function GetMethodText
     (This : access Typ;
      iMethod : Integer;
      type_k : Microsoft.VisualStudio.TextManager.Interop.MethodTextType.Valuetype) return CIL_Types.native_int is abstract;
   function GetOverloadCount
     (This : access Typ) return Integer is abstract;
   function GetParameterCount
     (This : access Typ;
      iMethod : Integer) return Integer is abstract;
   function GetParameterText
     (This : access Typ;
      iMethod : Integer;
      iParm : Integer;
      type_k : Microsoft.VisualStudio.TextManager.Interop.ParameterTextType.Valuetype) return CIL_Types.native_int is abstract;
   function NextMethod
     (This : access Typ) return Integer is abstract;
   procedure OnDismiss
     (This : access Typ) is abstract;
   function PrevMethod
     (This : access Typ) return Integer is abstract;
   procedure UpdateView
     (This : access Typ) is abstract;
private
   pragma Import (CIL, GetContextStream, "GetContextStream");
   pragma Import (CIL, GetCurMethod, "GetCurMethod");
   pragma Import (CIL, GetCurrentParameter, "GetCurrentParameter");
   pragma Import (CIL, GetMethodText, "GetMethodText");
   pragma Import (CIL, GetOverloadCount, "GetOverloadCount");
   pragma Import (CIL, GetParameterCount, "GetParameterCount");
   pragma Import (CIL, GetParameterText, "GetParameterText");
   pragma Import (CIL, NextMethod, "NextMethod");
   pragma Import (CIL, OnDismiss, "OnDismiss");
   pragma Import (CIL, PrevMethod, "PrevMethod");
   pragma Import (CIL, UpdateView, "UpdateView");
end Microsoft.VisualStudio.TextManager.Interop.IVsMethodData;
pragma Import (CIL, Microsoft.VisualStudio.TextManager.Interop.IVsMethodData,
   ".ver 7:1:40304:0 .publickeytoken=( b0 3f 5f 7f 11 d5 0a 3a )",
   "[Microsoft.VisualStudio.TextManager.Interop]Microsoft.VisualStudio.TextManager.Interop.IVsMethodData");