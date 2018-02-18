using System;

namespace cil2ada.Interfaces
{

  [Flags]
  public enum CorTokenType
  {
    mdtMask = 0x7f000000,
    mdtModule = 0x00000000,
    mdtTypeRef = 0x01000000,
    mdtTypeDef = 0x02000000,
    mdtFieldDef = 0x04000000,
    mdtMethodDef = 0x06000000,
    mdtParamDef = 0x08000000,
    mdtInterfaceImpl = 0x09000000,
    mdtMemberRef = 0x0a000000,
    mdtCustomAttribute = 0x0c000000,
    mdtPermission = 0x0e000000,
    mdtSignature = 0x11000000,
    mdtEvent = 0x14000000,
    mdtProperty = 0x17000000,
    mdtModuleRef = 0x1a000000,
    mdtTypeSpec = 0x1b000000,
    mdtAssembly = 0x20000000,
    mdtAssemblyRef = 0x23000000,
    mdtFile = 0x26000000,
    mdtExportedType = 0x27000000,
    mdtManifestResource = 0x28000000,
    mdtGenericParam = 0x2a000000,
    mdtMethodSpec = 0x2b000000,
    mdtGenericParamConstraint = 0x2c000000,
    mdtString = 0x70000000,
    mdtName = 0x71000000,
    mdtBaseType = 0x72000000
  }

  [Flags]
  public enum CorTypeAttr
  {
    tdVisibilityMask = 0x00000007,
    tdNotPublic = 0x00000000,
    tdPublic = 0x00000001,
    tdNestedPublic = 0x00000002,
    tdNestedPrivate = 0x00000003,
    tdNestedFamily = 0x00000004,
    tdNestedAssembly = 0x00000005,
    tdNestedFamANDAssem = 0x00000006,
    tdNestedFamORAssem = 0x00000007,

    tdLayoutMask = 0x00000018,
    tdAutoLayout = 0x00000000,
    tdSequentialLayout = 0x00000008,
    tdExplicitLayout = 0x00000010,

    tdClassSemanticsMask = 0x00000060,
    tdClass = 0x00000000,
    tdInterface = 0x00000020,

    tdAbstract = 0x00000080,
    tdSealed = 0x00000100,
    tdSpecialName = 0x00000400,

    tdImport = 0x00001000,
    tdSerializable = 0x00002000,

    tdStringFormatMask = 0x00030000,
    tdAnsiClass = 0x00000000,
    tdUnicodeClass = 0x00010000,
    tdAutoClass = 0x00020000,
    tdCustomFormatClass = 0x00030000,
    tdCustomFormatMask = 0x00C00000,

    tdBeforeFieldInit = 0x00100000,
    tdForwarder = 0x00200000,

    tdReservedMask = 0x00040800,
    tdRTSpecialName = 0x00000800,
    tdHasSecurity = 0x00040000,
  }

  [Flags]
  public enum CorFieldAttr
  {
    fdFieldAccessMask = 0x0007,
    fdPrivateScope = 0x0000,
    fdPrivate = 0x0001,
    fdFamANDAssem = 0x0002,
    fdAssembly = 0x0003,
    fdFamily = 0x0004,
    fdFamORAssem = 0x0005,
    fdPublic = 0x0006,

    fdStatic = 0x0010,
    fdInitOnly = 0x0020,
    fdLiteral = 0x0040,
    fdNotSerialized = 0x0080,

    fdSpecialName = 0x0200,

    fdPinvokeImpl = 0x2000,

    fdReservedMask = 0x9500,
    fdRTSpecialName = 0x0400,
    fdHasFieldMarshal = 0x1000,
    fdHasDefault = 0x8000,
    fdHasFieldRVA = 0x0100
  }

  [Flags]
  public enum CorMethodAttr
  {
    mdMemberAccessMask = 0x0007,
    mdPrivateScope = 0x0000,
    mdPrivate = 0x0001,
    mdFamANDAssem = 0x0002,
    mdAssembly = 0x0003,
    mdFamily = 0x0004,
    mdFamORAssem = 0x0005,
    mdPublic = 0x0006,

    mdStatic = 0x0010,
    mdFinal = 0x0020,
    mdVirtual = 0x0040,
    mdHideBySig = 0x0080,

    mdVtableLayoutMask = 0x0100,
    mdReuseSlot = 0x0000,
    mdNewSlot = 0x0100,

    mdCheckAccessOnOverride = 0x0200,
    mdAbstract = 0x0400,
    mdSpecialName = 0x0800,

    mdPinvokeImpl = 0x2000,
    mdUnmanagedExport = 0x0008,

    mdReservedMask = 0xd000,
    mdRTSpecialName = 0x1000,
    mdHasSecurity = 0x4000,
    mdRequireSecObject = 0x8000,
  }

  [Flags]
  public enum CorParamAttr
  {
    pdIn = 0x0001,
    pdOut = 0x0002,
    pdOptional = 0x0010,

    pdReservedMask = 0xf000,
    pdHasDefault = 0x1000,
    pdHasFieldMarshal = 0x2000,

    pdUnused = 0xcfe0
  };

  [Flags]
  public enum CorGenericParamAttr
  {
    gpVarianceMask = 0x0003,
    gpNonVariant = 0x0000,
    gpCovariant = 0x0001,
    gpContravariant = 0x0002,

    gpSpecialConstraintMask = 0x001C,
    gpNoSpecialConstraint = 0x0000,
    gpReferenceTypeConstraint = 0x0004,
    gpNotNullableValueTypeConstraint = 0x0008,
    gpDefaultConstructorConstraint = 0x0010
  }

  [Flags]
  public enum CorAssemblyFlags
  {
    afPublicKey = 0x0001,
    afPA_None = 0x0000,
    afPA_MSIL = 0x0010,
    afPA_x86 = 0x0020,
    afPA_IA64 = 0x0030,
    afPA_AMD64 = 0x0040,
    afPA_Specified = 0x0080,
    afPA_Mask = 0x0070,
    afPA_FullMask = 0x00F0,
    afPA_Shift = 0x0004,

    afEnableJITcompileTracking = 0x8000,
    afDisableJITcompileOptimizer = 0x4000,

    afRetargetable = 0x0100,
  }

  [Flags]
  public enum CorCallingConvention
  {
    DEFAULT = 0x0,

    VARARG = 0x5,
    FIELD = 0x6,
    LOCAL_SIG = 0x7,
    PROPERTY = 0x8,
    UNMGD = 0x9,
    GENERICINST = 0xa,
    NATIVEVARARG = 0xb,
    MAX = 0xc,

    MASK = 0x0f,
    UPMASK = 0xf0,
    HASTHIS = 0x20,
    EXPLICITTHIS = 0x40,
    GENERIC = 0x10
  }

  [Flags]
  public enum CorElementType
  {
    END = 0x0,
    VOID = 0x1,
    BOOLEAN = 0x2,
    CHAR = 0x3,
    I1 = 0x4,
    U1 = 0x5,
    I2 = 0x6,
    U2 = 0x7,
    I4 = 0x8,
    U4 = 0x9,
    I8 = 0xa,
    U8 = 0xb,
    R4 = 0xc,
    R8 = 0xd,
    STRING = 0xe,

    PTR = 0xf,
    BYREF = 0x10,

    VALUETYPE = 0x11,
    CLASS = 0x12,
    VAR = 0x13,
    ARRAY = 0x14,
    GENERICINST = 0x15,
    TYPEDBYREF = 0x16,

    I = 0x18,
    U = 0x19,
    FNPTR = 0x1B,
    OBJECT = 0x1C,
    SZARRAY = 0x1D,
    MVAR = 0x1e,

    CMOD_REQD = 0x1F,
    CMOD_OPT = 0x20,

    INTERNAL = 0x21,
    MAX = 0x22,

    MODIFIER = 0x40,
    SENTINEL = 0x01 | MODIFIER,
    PINNED = 0x05 | MODIFIER,
    R4_HFA = 0x06 | MODIFIER,
    R8_HFA = 0x07 | MODIFIER
  }
}
