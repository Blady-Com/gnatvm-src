using System;
using System.Collections.Generic;
using System.Text;

namespace cil2ada.Interfaces
{
  /// <summary>
  /// Flags de PInvoke
  /// </summary>
  public enum CorPinvokeMap
  {
    NoMangle = 0x0001,

    CharSetAnsi = 0x0002,
    CharSetUnicode = 0x0004,
    CharSetAuto = 0x0006,

    BestFitEnabled = 0x0010,
    BestFitDisabled = 0x0020,

    ThrowOnUnmappableCharEnabled = 0x1000,
    ThrowOnUnmappableCharDisabled = 0x2000,

    SupportsLastError = 0x0040,

    CallConvWinapi = 0x0100,
    CallConvCdecl = 0x0200,
    CallConvStdcall = 0x0300,
    CallConvThiscall = 0x0400,
    CallConvFastcall = 0x0500
  }
}
