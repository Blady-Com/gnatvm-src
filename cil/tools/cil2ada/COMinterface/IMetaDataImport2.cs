using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Reflection;

namespace cil2ada.Interfaces
{
  using HRESULT = System.UInt32;
  using ULONG32 = System.UInt32;

  using MD_TOKEN = System.UInt32;                 //Generic token
  using MD_MODULE = System.UInt32;                // Module token (roughly, a scope)
  using MD_TYPE_REF = System.UInt32;              // TypeRef reference (this or other scope)
  using MD_TYPE_DEF = System.UInt32;              // using in this scope
  using MD_FIELD_DEF = System.UInt32;             // Field in this scope
  using MD_METHOD_DEF = System.UInt32;            // Method in this scope
  using MD_PARAM_DEF = System.UInt32;             // param token
  using MD_INTERFACE_IMPL = System.UInt32;        // interface implementation token
  using MD_MEMBER_REF = System.UInt32;            // MemberRef (this or other scope)
  using MD_CUSTOM_ATTRIBUTE = System.UInt32;      // attribute token
  using MD_PERMISSION = System.UInt32;            // DeclSecurity
  using MD_SIGNATURE = System.UInt32;             // Signature object
  using MD_EVENT = System.UInt32;                 // event token
  using MD_PROPERTY = System.UInt32;              // property token
  using MD_MODULE_REF = System.UInt32;            // Module reference (for the imported modules)

  // Assembly tokens.
  using MD_ASSEMBLY = System.UInt32;              // Assembly token.
  using MD_ASSEMBLY_REF = System.UInt32;          // AssemblyRef token.
  using MD_FILE = System.UInt32;                  // File token.
  using MD_EXPORTED_TYPE = System.UInt32;         // ExportedType token.
  using MD_MANIFEST_RESOURCE = System.UInt32;     // ManifestResource token.
  using MD_TYPE_SPEC = System.UInt32;             // TypeSpec object
  using MD_GENERIC_PARAM = System.UInt32;         // formal parameter to generic type or method
  using MD_METHOD_SPEC = System.UInt32;           // instantiation of a generic method
  using MD_GENERIC_PARAM_CONSTRAINT = System.UInt32;// constraint on a formal generic parameter

  // Application string.
  using MD_STRING = System.UInt32;                // User literal string token.
  using MD_CPTOKEN = System.UInt32;               // constantpool token

  [ComImport]
  [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
  [GuidAttribute("FCE5EFA0-8BBA-4f8e-A036-8F2022B08466")]
  public interface IMetaDataImport2
  {
    // from IMetaDataImport
    HRESULT CloseEnum (IntPtr hEnum);

    HRESULT CountEnum (
        IntPtr hEnum,
        [MarshalAs (UnmanagedType.I4)] out Int32 count
        );

    HRESULT ResetEnum (
         [In] IntPtr hEnum,
         [MarshalAs (UnmanagedType.U4)] UInt32 ulPos
         );

    HRESULT EnumTypeDefs (
        [In, Out] ref IntPtr hEnum,
        [MarshalAs (UnmanagedType.LPArray, SizeParamIndex = 2)] MD_TYPE_DEF[] rTypeDefs,
        [In, MarshalAs (UnmanagedType.I4)] Int32 cMax,
        [MarshalAs (UnmanagedType.I4)] out Int32 pcTypeDefs
        );

    HRESULT EnumInterfaceImpls (
        [In, Out] ref IntPtr hEnum,
        [In, MarshalAs (UnmanagedType.U4)] MD_TYPE_DEF td,
        [MarshalAs (UnmanagedType.LPArray, SizeParamIndex = 2)] MD_INTERFACE_IMPL[] rImpls,
        [In, MarshalAs (UnmanagedType.I4)] Int32 cMax,
        [MarshalAs (UnmanagedType.I4)] out Int32 pcTokens
        );

    HRESULT EnumMemberRefs (
        [In, Out] ref IntPtr hEnum,
        [In, MarshalAs (UnmanagedType.U4)] MD_TOKEN tkParent,
        [MarshalAs (UnmanagedType.LPArray, SizeParamIndex = 3)] MD_MEMBER_REF[] rMemberRefs,
        [In, MarshalAs (UnmanagedType.I4)] Int32 cMax,
        [MarshalAs (UnmanagedType.I4)] out Int32 pcTokens
        );

    uint FindTypeDefByName([MarshalAs(UnmanagedType.LPWStr)]string szTypeDef, uint tkEnclosingClass, out uint ptd);

    uint GetScopeProps([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 0)]char[] szName, uint cchName, out uint pchName, out Guid pmvid);

    uint GetModuleFromScope(out uint pmd);

    HRESULT GetTypeDefProps (
        [In, MarshalAs (UnmanagedType.U4)] MD_TYPE_DEF td,
        [MarshalAs (UnmanagedType.LPArray, SizeParamIndex = 2)] char[] szTypeDef,
        [In, MarshalAs (UnmanagedType.I4)] Int32 cchTypeDef,
        [MarshalAs (UnmanagedType.I4)] out Int32 pchTypeDef,
        [MarshalAs (UnmanagedType.U4)] out TypeAttributes pdwTypeDefFlags,
        [MarshalAs (UnmanagedType.U4)] out MD_TOKEN ptkExtends
        );

    uint GetInterfaceImplProps(uint iiImpl, out uint pClass, out uint ptkIface);

    HRESULT GetTypeRefProps (
        [In, MarshalAs (UnmanagedType.U4)] MD_TYPE_REF tr,
        [MarshalAs (UnmanagedType.U4)] out MD_TOKEN ptkResolutionScope,
        [MarshalAs (UnmanagedType.LPArray, SizeParamIndex = 3)] char[] szName,
        [In, MarshalAs (UnmanagedType.I4)] Int32 cchName,
        [MarshalAs (UnmanagedType.I4)] out Int32 pchName
        );

    [PreserveSig]
    HRESULT ResolveTypeRef (
        [In, MarshalAs (UnmanagedType.U4)] MD_TYPE_REF tr,
        [MarshalAs (UnmanagedType.Struct)] ref Guid riid,
        [MarshalAs (UnmanagedType.Interface)] out object ppIScope,
        [MarshalAs (UnmanagedType.U4)] out MD_TYPE_DEF ptd
        ); 

    uint EnumMembers(ref uint phEnum, uint cl, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)]uint[] rMembers, uint cMax, out uint pcTokens);

    uint EnumMembersWithName(ref uint phEnum, uint cl, [MarshalAs(UnmanagedType.LPWStr)]string szName, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4)]uint[] rMembers, uint cMax, out uint pcTokens);

    HRESULT EnumMethods (
        [In, Out] ref IntPtr hEnum,
        [In, MarshalAs (UnmanagedType.U4)] MD_TYPE_DEF cl,
        [MarshalAs (UnmanagedType.LPArray, SizeParamIndex = 3)] MD_METHOD_DEF[] rMethods,
        [In, MarshalAs (UnmanagedType.I4)] Int32 cMax,
        [MarshalAs (UnmanagedType.I4)] out Int32 pcTokens
        );

    uint EnumMethodsWithName(ref uint phEnum, uint cl, [MarshalAs(UnmanagedType.LPWStr)]string szName, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4)]uint[] rMethods, uint cMax, out uint pcTokens);

    HRESULT EnumFields (
        [In, Out] ref IntPtr hEnum,
        [MarshalAs (UnmanagedType.U4)] MD_TYPE_DEF cl,
        [MarshalAs (UnmanagedType.LPArray, SizeParamIndex = 3)] MD_FIELD_DEF[] rFields,
        [MarshalAs (UnmanagedType.I4)] Int32 cMax,
        [MarshalAs (UnmanagedType.I4)] out Int32 pcTokens
        );
    
    uint EnumFieldsWithName(ref uint phEnum, uint cl, [MarshalAs(UnmanagedType.LPWStr)]string szName, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4)]uint[] rFields, uint cMax, out uint pcTokens);

    HRESULT EnumParams (
        [In, Out] ref IntPtr hEnum,
        [In, MarshalAs (UnmanagedType.U4)] MD_METHOD_DEF cl,
        [MarshalAs (UnmanagedType.LPArray, SizeParamIndex = 3)] MD_PARAM_DEF[] rParams,
        [In, MarshalAs (UnmanagedType.I4)] Int32 cMax,
        [MarshalAs (UnmanagedType.I4)] out Int32 pcTokens
        );

    uint EnumMemberRefs(ref uint phEnum, uint tkParent, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)]uint[] rMemberRefs, uint cMax, out uint pcTokens);

    uint EnumMethodImpls(ref uint phEnum, uint td, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]uint[] rMethodBody, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)]uint[] rMethodDecl, uint cMax, out uint pcTokens);

    uint EnumPermissionSets(ref uint phEnum, uint tk, uint dwActions, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)]uint[] rPermission, uint cMax, out uint pcTokens);

    uint FindMember(uint td, [MarshalAs(UnmanagedType.LPWStr)]string szName, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]byte[] pvSigBlob, uint cbSigBlob, out uint pmb);

    uint FindMethod(uint td, [MarshalAs(UnmanagedType.LPWStr)]string szName, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]byte[] pvSigBlob, uint cbSigBlob, out uint pmb);

    uint FindField(uint td, [MarshalAs(UnmanagedType.LPWStr)]string szName, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]byte[] pvSigBlob, uint cbSigBlob, out uint pmb);

    uint FindMemberRef(uint td, [MarshalAs(UnmanagedType.LPWStr)]string szName, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]byte[] pvSigBlob, int cbSigBlob, out uint pmr);

    uint GetMethodProps(uint mb, out uint pClass, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]char[] szMethod, uint cchMethod, out uint pchMethod, out uint pdwAttr, out IntPtr ppvSigBlob, out uint pcbSigBlob, out uint pulCodeRVA, out uint pdwImplFlags);

    uint GetMemberRefProps(uint mr, out uint ptk, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]char[] szMember, uint cchMember, out uint pchMember, out IntPtr ppvSigBlob, out uint pbSigBlob);

    uint EnumProperties(ref uint phEnum, uint td, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]uint[] rProperties, uint cMax, out uint pcProperties);

    uint EnumEvents(ref uint phEnum, uint td, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]uint[] rEvents, uint cMax, out uint pcEvents);

    uint GetEventProps(uint ev, out uint pClass, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]char[] szEvent, uint cchEvent, out uint pchEvent, out uint pdwEventFlags, out uint ptkEventType, out uint pmdAddOn, out uint pmdRemoveOn, out uint pmdFire, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 10)]uint[] rmdOtherMethod, uint cMax, out uint pcOtherMethod);

    uint EnumMethodSemantics(ref uint phEnum, uint mb, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]uint[] rEventProp, uint cMax, out uint pcEventProp);

    uint GetMethodSemantics(uint mb, uint tkEventProp, out uint pdwSemanticsFlags);

    uint GetClassLayout(uint td, out uint pdwPackSize, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]long[] rFieldOffset, uint cMax, out uint pcFieldOffset, out uint pulClassSize);

    uint GetFieldMarshal(uint tk, out IntPtr ppvNativeType, out uint pcbNativeType);

    uint GetRVA(uint tk, out uint pulCodeRVA, out uint pdwImplFlags);

    uint GetPermissionSetProps(uint pm, out uint pdwAction, out IntPtr ppvPermission, out uint pcbPermission);

    uint GetSigFromToken(uint mdSig, out IntPtr ppvSig, out uint pcbSig);

    uint GetModuleRefProps(uint mur, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)]char[] szName, uint cchName, out uint pchName);

    uint EnumModuleRefs(ref uint phEnum, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)]uint[] rModuleRefs, uint cmax, out uint pcModuleRefs);

    uint GetTypeSpecFromToken(uint typespec, out IntPtr ppvSig, out uint pcbSig);

    uint GetNameFromToken(uint tk, out IntPtr pszUtf8NamePtr);

    uint EnumUnresolvedMethods(ref uint phEnum, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)]uint[] rMethods, uint cMax, out uint pcTokens);

    uint GetUserString(uint stk, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] char[] szString, uint cchString, out uint pchString);

    uint GetPinvokeMap(uint tk, out CorPinvokeMap pdwMappingFlags, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]char[] szImportName, uint cchImportName, out uint pchImportName, out uint pmrImportDLL);

    uint EnumSignatures(ref uint phEnum, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)]uint[] rSignatures, uint cmax, out uint pcSignatures);

    uint EnumTypeSpecs(ref uint phEnum, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)]uint[] rTypeSpecs, uint cmax, out uint pcTypeSpecs);

    uint EnumUserStrings(ref uint phEnum, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)]uint[] rStrings, uint cmax, out uint pcStrings);

    uint GetParamForMethodIndex(uint md, uint ulParamSeq, out uint ppd);

    uint EnumCustomAttributes(ref uint phEnum, uint tk, uint tkType, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)]uint[] rCustomAttributes, uint cMax, out uint pcCustomAttributes);

    uint GetCustomAttributeProps(uint cv, out uint ptkObj, out uint ptkType, out IntPtr ppBlob, out uint pcbSize);

    uint FindTypeRef(uint tkResolutionScope, [MarshalAs(UnmanagedType.LPWStr)]string szName, out uint ptr);

    uint GetMemberProps(uint mb, out uint pClass, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]char[] szMember, uint cchMember, out uint pchMember, out uint pdwAttr, out IntPtr ppvSigBlob, out uint pcbSigBlob, out uint pulCodeRVA, out uint pdwImplFlags, out uint pdwCPlusTypeFlag, out IntPtr ppValue, out uint pcchValue);

    uint GetFieldProps(uint mb, out uint pClass, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]char[] szField, uint cchField, out uint pchField, out uint pdwAttr, out IntPtr ppvSigBlob, out uint pcbSigBlob, out uint pdwDefType, out IntPtr ppValue, out uint pcchValue);

    uint GetPropertyProps(uint prop, out uint pClass, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]char[] szProperty, uint cchProperty, out uint pchProperty, out uint pdwPropFlags, out IntPtr ppvSig, out uint pbSig, out uint pdwCPlusTypeFlag, out IntPtr ppDefaultValue, out uint pcchDefaultValue, out uint pmdSetter, out uint pmdGetter, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 13)]uint[] rmdOtherMethod, uint cMax, out uint pcOtherMethod);

    uint GetParamProps(uint tk, out uint pmd, out uint pulSequence, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 3)]char[] szName, uint cchName, out uint pchName, out uint pdwAttr, out uint pdwCPlusTypeFlag, out IntPtr ppValue, out uint pcchValue);

    uint GetCustomAttributeByName(uint tkObj, [MarshalAs(UnmanagedType.LPWStr)]string szName, out IntPtr ppData, out uint pcbData);

    bool IsValidToken(uint tk);

    uint GetNestedClassProps(uint tdNestedClass, out uint ptdEnclosingClass);

    uint GetNativeCallConvFromSig(IntPtr pvSig, uint cbSig, out uint pCallConv);

    uint IsGlobal(uint pd, out uint pbGlobal);

    // IMetaDataImport2 part
    HRESULT EnumGenericParams(
      [In, Out] ref IntPtr phEnum,
      [MarshalAs (UnmanagedType.U4)] MD_TOKEN tk,
      [MarshalAs (UnmanagedType.LPArray, SizeParamIndex = 2)]uint[] rGenericParams,
      [MarshalAs (UnmanagedType.I4)] Int32 cMax,
      [MarshalAs (UnmanagedType.I4)] out Int32 pcGenericParams);

    uint GetGenericParamProps(uint gp, out uint pulParamSeq, out uint pdwParamFlags, out uint ptOwner, out uint reserved, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]char[] wzName, uint cchName, out uint pchName);

    uint GetMethodSpecProps(uint mi, out uint tkParent, out IntPtr ppvSigBlob, out uint pbSigBlob);

    HRESULT EnumGenericParamConstraints(
      [In, Out] ref IntPtr hEnum,
      [MarshalAs (UnmanagedType.U4)] MD_TOKEN tk,
      [MarshalAs (UnmanagedType.LPArray, SizeParamIndex = 2)]MD_GENERIC_PARAM_CONSTRAINT[] rGenericParamConstraints,
      [MarshalAs (UnmanagedType.I4)] Int32 cMax,
      [MarshalAs (UnmanagedType.I4)] out Int32 pcGenericParamConstraints);

    uint GetGenericParamConstraintProps(uint gpc, out uint ptGenericParam, out uint ptkConstraintType);

    uint GetPEKind(out uint pdwPEKind, out uint pdwMAchine);

    uint GetVersionString([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]char[] pwzBuf, uint ccBufSize, out uint pccBufSize);

    uint EnumMethodSpecs(ref uint phEnum, uint tk, [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]uint[] rMethodSpecs, uint cMax, out uint pcMethodSpecs);
  }
}
