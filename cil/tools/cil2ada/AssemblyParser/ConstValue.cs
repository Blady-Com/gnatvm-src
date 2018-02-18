//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                    cil2ada.AssemblyParser.ConstValue                     //
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
using System.Runtime.InteropServices;
using cil2ada.Interfaces;

namespace cil2ada
{
  public enum ConstValueType
  {
    BOOL, I1, U1, CHAR, I2, U2, I4, U4, I8, U8, R4, R8, STRING, CLASS
  }

  class ConstValue : IComparable<ConstValue>
  {
    public ConstValueType valueType;

    public Boolean boolVal;
    public Byte byteVal;
    public SByte sbyteVal;
    public Char charVal;
    public Int16 int16Val;
    public UInt16 uint16Val;
    public Int32 int32Val;
    public UInt32 uint32Val;
    public Int64 int64Val;
    public UInt64 uint64Val;
    public Single singleVal;
    public Double doubleVal;
    public string stringVal;

    public ConstValue(uint code, IntPtr loc, uint locSize)
    {
      switch ((CorElementType)code)
      {
        case CorElementType.BOOLEAN:
          this.valueType = ConstValueType.BOOL;
          this.boolVal = Decoder.ReadBoolean(loc);
          break;
        case CorElementType.I1:
          this.valueType = ConstValueType.I1;
          this.sbyteVal = Decoder.ReadSByte(loc);
          break;
        case CorElementType.U1:
          this.valueType = ConstValueType.U1;
          this.byteVal = Decoder.ReadByte(loc);
          break;
        case CorElementType.CHAR:
          this.valueType = ConstValueType.CHAR;
          this.charVal = Decoder.ReadChar(loc);
          break;
        case CorElementType.I2:
          this.valueType = ConstValueType.I2;
          this.int16Val = Decoder.ReadInt16(loc);
          break;
        case CorElementType.U2:
          this.valueType = ConstValueType.U2;
          this.uint16Val = Decoder.ReadUInt16(loc);
          break;
        case CorElementType.I4:
          this.valueType = ConstValueType.I4;
          this.int32Val = Decoder.ReadInt32(loc);
          break;
        case CorElementType.U4:
          this.valueType = ConstValueType.U4;
          this.uint32Val = Decoder.ReadUInt32(loc);
          break;
        case CorElementType.I8:
          this.valueType = ConstValueType.I8;
          this.int64Val = Decoder.ReadInt64(loc);
          break;
        case CorElementType.U8:
          this.valueType = ConstValueType.U8;
          this.uint64Val = Decoder.ReadUInt64(loc);
          break;
        case CorElementType.R4:
          this.valueType = ConstValueType.R4;
          this.singleVal = Decoder.ReadFloat32(loc);
          break;
        case CorElementType.R8:
          this.valueType = ConstValueType.R8;
          this.doubleVal = Decoder.ReadFloat64(loc);
          break;
        case CorElementType.STRING:
          this.valueType = ConstValueType.STRING;
          this.stringVal = Decoder.ReadString(loc, (int)locSize);
          break;
        case CorElementType.CLASS:
          this.valueType = ConstValueType.CLASS;
          break;
        default:
          throw new System.ArgumentException("Incorrect element type 0x" + code.ToString("x") + " in default value");
      }
    }

    public ConstValueType ValueType { get { return this.valueType; } }

    public int CompareTo(ConstValue o)
    {
      if (o.valueType != this.valueType)
        throw new System.ArgumentException ("values need to be the same type");

      switch (this.valueType)
      {
        case ConstValueType.BOOL:
          return this.boolVal.CompareTo(o.boolVal);
        case ConstValueType.I1:
          return this.sbyteVal.CompareTo(o.sbyteVal);
        case ConstValueType.U1:
          return this.byteVal.CompareTo(o.byteVal);
        case ConstValueType.CHAR:
          return this.charVal.CompareTo(o.charVal);
        case ConstValueType.I2:
          return this.int16Val.CompareTo(o.int16Val);
        case ConstValueType.U2:
          return this.uint16Val.CompareTo(o.uint16Val);
        case ConstValueType.I4:
          return this.int32Val.CompareTo(o.int32Val);
        case ConstValueType.U4:
          return this.uint32Val.CompareTo(o.uint32Val);
        case ConstValueType.I8:
          return this.int64Val.CompareTo(o.int64Val);
        case ConstValueType.U8:
          return this.uint64Val.CompareTo(o.uint64Val);
        case ConstValueType.R4:
          return this.singleVal.CompareTo(o.singleVal);
        case ConstValueType.R8:
          return this.doubleVal.CompareTo(o.doubleVal);
        case ConstValueType.STRING:
          return this.stringVal.CompareTo(o.stringVal);
        case ConstValueType.CLASS:
          return 0;
      }
      return 0;
    }

    public string AdaImage()
    {
      switch (this.valueType)
      {
        case ConstValueType.BOOL:
          if (this.boolVal) return "True";
          else return "False";
        case ConstValueType.I1:
          return this.sbyteVal.ToString();
        case ConstValueType.U1:
          return this.byteVal.ToString();
        case ConstValueType.CHAR:
          return "+'" + this.charVal + "'";
        case ConstValueType.I2:
          return this.int16Val.ToString();
        case ConstValueType.U2:
          return this.uint16Val.ToString();
        case ConstValueType.I4:
          return this.int32Val.ToString();
        case ConstValueType.U4:
          return this.uint32Val.ToString();
        case ConstValueType.I8:
          return this.int64Val.ToString();
        case ConstValueType.U8:
          return this.uint64Val.ToString();
        case ConstValueType.R4:
          return this.singleVal.ToString();
        case ConstValueType.R8:
          return this.doubleVal.ToString();
        case ConstValueType.STRING:
          return "+\"" + this.stringVal.ToString() + "\"";
        case ConstValueType.CLASS:
          return "null";
      }
      throw new System.ArgumentException("ConstValueType uninitialized");
    }
  }
}
