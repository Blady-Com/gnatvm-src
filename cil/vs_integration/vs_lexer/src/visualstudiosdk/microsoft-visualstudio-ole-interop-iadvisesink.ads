-- Autogenerated by CIL2Ada v. 2
with MSSyst.Object;
with CIL_Types;
use CIL_Types;
with Microsoft.VisualStudio.OLE.Interop.FORMATETC;
limited with Microsoft.VisualStudio.OLE.Interop.IMoniker;
with Microsoft.VisualStudio.OLE.Interop.STGMEDIUM;
package Microsoft.VisualStudio.OLE.Interop.IAdviseSink is
   type Typ is interface;
   type Ref is access all Typ'Class;
   type Ref_addrof is access all Ref;
   type Ref_Arr is array (Natural range <>) of Ref;
   type Ref_Array is access all Ref_Arr;
   type Ref_Array_addrof is access all Ref_Array;
   procedure OnClose
     (This : access Typ) is abstract;
   procedure OnDataChange
     (This : access Typ;
      pFormatetc : access Microsoft.VisualStudio.OLE.Interop.FORMATETC.Valuetype_arr;
      pStgmed : access Microsoft.VisualStudio.OLE.Interop.STGMEDIUM.Valuetype_arr) is abstract;
   procedure OnRename
     (This : access Typ;
      pmk : access Microsoft.VisualStudio.OLE.Interop.IMoniker.Typ'Class) is abstract;
   procedure OnSave
     (This : access Typ) is abstract;
   procedure OnViewChange
     (This : access Typ;
      dwAspect : CIL_Types.Unsigned_Integer;
      lindex : Integer) is abstract;
private
   pragma Import (CIL, OnClose, "OnClose");
   pragma Import (CIL, OnDataChange, "OnDataChange");
   pragma Import (CIL, OnRename, "OnRename");
   pragma Import (CIL, OnSave, "OnSave");
   pragma Import (CIL, OnViewChange, "OnViewChange");
end Microsoft.VisualStudio.OLE.Interop.IAdviseSink;
pragma Import (CIL, Microsoft.VisualStudio.OLE.Interop.IAdviseSink,
   ".ver 7:1:40304:0 .publickeytoken=( b0 3f 5f 7f 11 d5 0a 3a )",
   "[Microsoft.VisualStudio.OLE.Interop]Microsoft.VisualStudio.OLE.Interop.IAdviseSink");
