-- Autogenerated by CIL2Ada v. 2
with MSSyst.Object;
with CIL_Types;
use CIL_Types;
limited with MSSyst.String;
limited with MSSyst.Type_k;
with MSSyst.ValueType;
package Microsoft.VisualStudio.OLE.Interop.POINT is
   type ValueType is new MSSyst.ValueType.Typ with null record;
   type ValueType_Arr is array (Natural range <>) of ValueType;-- start at 0
   type ValueType_Array is access all ValueType_Arr;
   type ValueType_addrof is access all ValueType;
   type ValueType_array_addrof is access all ValueType_Array;
private
end Microsoft.VisualStudio.OLE.Interop.POINT;
pragma Import (CIL, Microsoft.VisualStudio.OLE.Interop.POINT,
   ".ver 7:1:40304:0 .publickeytoken=( b0 3f 5f 7f 11 d5 0a 3a )",
   "[Microsoft.VisualStudio.OLE.Interop]Microsoft.VisualStudio.OLE.Interop.POINT");