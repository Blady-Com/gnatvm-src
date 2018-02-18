//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                       cil2ada.AssemblyParser.Param                       //
//                                                                          //
//                     Copyright (C) 2006-2009, AdaCore                     //
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
using cil2ada.Interfaces;

namespace cil2ada.AssemblyParser
{

  public enum ParamMode
  {
    Mode_In,
    Mode_Out,
    Mode_Optional
  }

  public enum ParamKind { Type, TypedByRef, Void };

  class Param : Token
  {
    // from token
    private string name = null;
    private CorParamAttr flags;
    private Method method;
    // from signature
    private ParamKind kind = ParamKind.Void;
    public bool byRef = false;
    public Type type = null;

    public Param(IntPtr sig, ref int idx, Method enclosing)
      : base(0)
    {
      //  creation of unnamed parameter
      this.name = null;
      this.flags = CorParamAttr.pdIn;
      this.method = enclosing;
      this.SetSig(sig, ref idx, enclosing);
    }

    public Param(uint token, Method enclosing)
      : base(token)
    {
      char[] szName = new char[1024];
      uint outNameSize;
      uint tokmethod, seq, attr, constValueType, constValueSize;
      IntPtr constValue;

      // from the Param table
      if (((CorTokenType)token & CorTokenType.mdtMask) != CorTokenType.mdtParamDef)
        throw new ArgumentException
          ("Invalid token type while creating a Param");

      Context.importIntf.GetParamProps
        (this.token, out tokmethod, out seq, szName, 1024, out outNameSize,
         out attr, out constValueType, out constValue, out constValueSize);

      if ((int)outNameSize == 0)
      {
        throw new ArgumentException("invalid name as parameter");
      }
      else
      {
        this.name = Decoder.ReadString(szName, (int)outNameSize);
      }

      this.flags = (CorParamAttr)attr;
      this.method = enclosing;
    }

    public void SetSig(IntPtr sig, ref int idx, Token context)
    {

      CustomMod.ReadCustomMods(sig, ref idx);

      CorElementType b = (CorElementType)Decoder.ReadByte(sig, idx);
      if (b == CorElementType.TYPEDBYREF)
      {
        idx++;
        this.kind = ParamKind.TypedByRef;
      }
      else if (b == CorElementType.VOID)
      {
        idx++;
        this.kind = ParamKind.Void;
      }
      else
      {
        this.kind = ParamKind.Type;
        if (b == CorElementType.BYREF)
        {
          idx++;
          this.byRef = true;
        }
        this.type = new Type (sig, ref idx, context);
        if (this.type.IsGenericInstance)
          throw new System.ArgumentException
            ("Unsupported generic instance as parameter");
      }

      // Mode_Out and by ref parameters are not supported. Raise an error in those cases.
      if ((this.Mode == ParamMode.Mode_Out) || (this.byRef))
        throw new ArgumentException
          ("Mode Out or ByRef parameters are not handled");
    }

    public string Name { get { return this.name; } }
    public ParamKind Kind { get { return this.kind; } }
    public ParamMode Mode
    {
      get
      {
        if ((this.flags & CorParamAttr.pdIn) != 0)
          return ParamMode.Mode_In;
        else if ((this.flags & CorParamAttr.pdOut) != 0)
          return ParamMode.Mode_Out;
        else if ((this.flags & CorParamAttr.pdOptional) != 0)
          return ParamMode.Mode_Optional;
        return ParamMode.Mode_In;
      }
    }

    public string AdaImage(bool isRet, bool isRetOfDelegate, Type enclosingType)
    {
      string str = "";

      if (this.name != null)
        str += CheckAdaKeywords(this.Name) + " : ";

      if (this.kind == ParamKind.TypedByRef)
      {
        if (isRet)
          str += "MSSyst.TypedReference.Valuetype";
        else
          str += "MSSyst.TypedReference.Valuetype'Class";
        AddLimitedWith("MSSyst.TypedReference");
      }
      else if (this.kind == ParamKind.Void)
        throw new System.ArgumentException("Void parameter cannot be printed");
      else if (this.type.FullName == enclosingType.FullName)
      {
          if (enclosingType.IsValueType)
            if (isRet)
               str += "ValueType";
             else
               str += "ValueType'Class";
           else if (enclosingType.IsEnum)
            str += "ValueType";
          else if (enclosingType.IsDelegate)
            str += "Delegate";
          else
            str += "access Typ'Class";
      }
      else
      {
        this.type.AddWith (isRet && isRetOfDelegate && this.type.IsValueType);

        //  The following code is commented out as Mode_Out and ByRef parameters are not handled
        //  for bindings : they require specific calling convention that is not handled by the .NET backend

        // if ((this.Mode == ParamMode.Mode_Out) || (this.byRef))
        //   if (this.type.Name == "CIL_Types")
        //     str += this.type.FullAdaTypeName(true, false, false) + "_addrof";
        //   else if (this.type.IsValueType)
        //     str += "access " + this.type.FullAdaTypeName (false, false, false);
        //   else
        //     str += "access " + this.type.FullAdaTypeName(true, false, false);
        // else
        str += this.type.FullAdaTypeName(isRet, false);
      }

      return str;
    }
  }
}
