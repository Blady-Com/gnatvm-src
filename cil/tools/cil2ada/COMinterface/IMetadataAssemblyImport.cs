using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;

namespace cil2ada.Interfaces
{
  [StructLayout(LayoutKind.Sequential)]
  struct ASSEMBLYMETADATA
  {
    public ushort usMajorVersion;         // Major Version.
    public ushort usMinorVersion;         // Minor Version.
    public ushort usBuildNumber;          // Build Number.
    public ushort usRevisionNumber;       // Revision Number.
    public IntPtr szLocale;               // Locale.
    public uint cbLocale;               // [IN/OUT] Size of the buffer in wide chars/Actual size.
    public IntPtr rProcessor;            // Processor ID array.
    public uint ulProcessor;            // [IN/OUT] Size of the Processor ID array/Actual # of entries filled in.
    public IntPtr rOS;                   // OSINFO array.
    public uint ulOS;                   // [IN/OUT]Size of the OSINFO array/Actual # of entries filled in.
  }

  [ComImport, GuidAttribute("EE62470B-E94B-424e-9B7C-2F00C9249F93"), InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
  interface IMetaDataAssemblyImport
  {
    uint GetAssemblyProps(
        uint mda,
        out IntPtr ppbPublicKey,
        out uint pcbPublicKey,
        out uint pulHashAlgId,
        [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4)] char[] szName,
        uint cchName,
        out uint pchName,
        out ASSEMBLYMETADATA pMetaData,
        out uint pdwAssemblyFlags
    );
    uint GetAssemblyRefProps(
        uint mdar,
        out IntPtr ppbPublicKeyOrToken,
        out uint pcbPublicKeyOrToken,
        [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4)] char[] szName,
        uint cchName,
        out uint pchName,
        out ASSEMBLYMETADATA pMetaData,
        out IntPtr ppbHashValue,
        out uint pcbHashValue,
        out uint pdwAssemblyRefFlags
    );
    uint GetFileProps(
        uint mdf,
        [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] char[] szName,
        uint cchName,
        out uint pchName,
        out IntPtr ppbHashValue,
        out uint pcbHashValue,
        out uint pdwFileFlags
    );
    uint GetExportedTypeProps(
        uint mdct,
        [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] char[] szName,
        uint cchName,
        out uint pchName,
        out uint ptkImplementation,
        out uint ptkTypeDef,
        out uint pdwExportedTypeFlags
    );
    uint GetManifestResourceProps(
        uint mdmr,
        [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] char[] szName,
        uint cchName,
        out uint pchName,
        out uint ptkImplementation,
        out uint pdwOffset,
        out uint pdwResourceFlags
    );

    uint EnumAssemblyRefs(
        ref uint phEnum,
        [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] uint[] rAssemblyRefs,
        uint cMax,
        out uint pcTokens
    );
    uint EnumFiles(
        ref uint phEnum,
        [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] uint[] rFiles,
        uint cMax,
        out uint pcTokens
    );
    uint EnumExportedTypes(
        ref uint phEnum,
        [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] uint[] rExportedTypes,
        uint cMax,
        out uint pcTokens
    );

    uint EnumManifestResources(
        ref uint phEnum,
        [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)] uint[] rManifestResources,
        uint cMax,
        out uint pcTokens
    );

    uint GetAssemblyFromScope(
        out uint ptkAssembly
    );

    uint FindExportedTypeByName(
        [MarshalAs(UnmanagedType.LPWStr)] string szName,
        uint mdtExportedType,
        out uint ptkExportedType
    );

    uint FindManifestResourceByName(
        [MarshalAs(UnmanagedType.LPWStr)] string szName,
        out uint ptkManifestResource
    );

    void CloseEnum(
        uint hEnum
    );

    uint FindAssembliesByName(
        [MarshalAs(UnmanagedType.LPWStr)] string szAppBase,
        [MarshalAs(UnmanagedType.LPWStr)] string szPrivateBin,
        [MarshalAs(UnmanagedType.LPWStr)] string szAssemblyName,
        //[MarshalAs(UnmanagedType.Interface)] out object ppIUnk,
        [MarshalAs(UnmanagedType.LPArray, ArraySubType=UnmanagedType.Interface)] object[] ppIUnk,
      // [out] IUnknown *ppIUnk[],
        uint cMax,
        out uint pcAssemblies
    );
  }
}
