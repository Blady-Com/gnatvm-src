//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                      cil2ada.AssemblyParser.Decoder                      //
//                                                                          //
//                     Copyright (C) 2006-2008, AdaCore                     //
//                                                                          //
// GNAT is free software;  you can  redistribute it  and/or modify it under //
// terms of the  GNU General Public License as published  by the Free Soft- //
// ware  Foundation;  either version 2,  or (at your option) any later ver- //
// sion.  GNAT is distributed in the hope that it will be useful, but WITH- //
// OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY //
// or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License //
// for  more details.  You should have  received  a copy of the GNU General //
// Public License  distributed with GNAT;  see file COPYING.  If not, write //
// to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, //
// MA 02111-1307, USA.                                                      //
//                                                                          //
// The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by //
// AdaCore - http://www.adacore.com                                         //
//                                                                          //
// This work is partially  based on A#, an Ada  compiler for .NET by  Prof. //
// Martin C. Carlisle of the United States Air Force Academy.               //
//////////////////////////////////////////////////////////////////////////////

using System;

namespace cil2ada
{
  class Decoder
  {
    public static unsafe Byte ReadValue(IntPtr sig, ref int idx)
    {
      Byte b = System.Runtime.InteropServices.Marshal.ReadByte(sig, idx);
      idx++;
      return b;
    }

    public static uint ReadTypeDefOrRefEncoded(IntPtr sig, ref int idx)
    {
      uint code = (uint)ReadCompressed(sig, ref idx);
      if ((code & 0x3) == 0) // typeDef token
        return (code >> 2) | 0x2000000;
      else if ((code & 0x3) == 1) // typeRef token
        return (code >> 2) | 0x1000000;

      throw new ArgumentException
        ("Unsupported encoded Token Type code 0x" + (code & 0x3).ToString());
    }

    public static unsafe int ReadCompressed(IntPtr sig, ref int idx)
    {
      Byte b1 = ReadValue(sig, ref idx);
      Byte b2 = 0;
      Byte b3 = 0;
      Byte b4 = 0;
      uint code = 0;

      if ((b1 | 0x7F) == 0x7F)
      {
        code = (uint)b1;
      }
      else if ((b1 & 0xC0) == 0x80)
      {
        b1 = (Byte)(b1 & 0x3F);
        b2 = ReadValue(sig, ref idx);
        code = ((uint)b1 << 8) + (uint)b2;
      }
      else if ((b1 & 0xE0) == 0xC0)
      {
        b1 = (Byte)(b1 & 0x3F);
        b2 = ReadValue(sig, ref idx);
        b3 = ReadValue(sig, ref idx);
        b4 = ReadValue(sig, ref idx);
        code = ((uint)b1 << 24) + ((uint)b2 << 16) + ((uint)b3 << 8) + (uint)b4;
      }

      return (int)code;
    }

    public static unsafe Boolean ReadBoolean(IntPtr ptr)
    {
      return *(bool*)ptr;
    }
    public static unsafe SByte ReadSByte(IntPtr ptr)
    {
      return *(SByte*)ptr;
    }
    public static unsafe Byte ReadByte(IntPtr ptr)
    {
      return *(Byte*)ptr;
    }
    public static unsafe Byte ReadByte(IntPtr ptr, int idx)
    {
      return System.Runtime.InteropServices.Marshal.ReadByte(ptr, idx);
    }
    public static unsafe Char ReadChar(IntPtr ptr)
    {
      return *(Char*)ptr;
    }
    public static unsafe Int16 ReadInt16(IntPtr ptr)
    {
      return *(Int16*)ptr;
    }
    public static unsafe UInt16 ReadUInt16(IntPtr ptr)
    {
      return *(UInt16*)ptr;
    }
    public static unsafe Int32 ReadInt32(IntPtr ptr)
    {
      return *(Int32*)ptr;
    }
    public static unsafe UInt32 ReadUInt32(IntPtr ptr)
    {
      return *(UInt32*)ptr;
    }
    public static unsafe Int64 ReadInt64(IntPtr ptr)
    {
      return *(Int64*)ptr;
    }
    public static unsafe UInt64 ReadUInt64(IntPtr ptr)
    {
      return *(UInt64*)ptr;
    }
    public static unsafe Single ReadFloat32(IntPtr ptr)
    {
      return *(Single*)ptr;
    }
    public static unsafe Double ReadFloat64(IntPtr ptr)
    {
      return *(Double*)ptr;
    }
    public static unsafe string ReadString(char[] ptr, int size)
    {
      if (size == 0) return "";
      return new string(ptr, 0, size - 1);
    }
    public static unsafe string ReadString(IntPtr ptr, int size)
    {
      if (size == 0) return "";
      return new string((char*)ptr, 0, size - 1);
    }
  }
}
