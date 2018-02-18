//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                       cil2ada.AssemblyParser.Field                       //
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
using System.Collections.Generic;
using System.Reflection;
using cil2ada.Interfaces;

namespace cil2ada.AssemblyParser
{
  class Field : Token, IComparable<Field>
  {
    private string name;
    private Type declaringType;
    private CorFieldAttr flags;
    private ConstValue constValue = null;
    public Type type;

    public Field(uint token)
      : base(token)
    {
      char[] szName = new char[1024];
      uint outNameSize;
      uint tokclass, attr, sigBlobSize, constValueType, constValueSize;
      IntPtr sigBlob, constValue;

      Context.importIntf.GetFieldProps
        (token, out tokclass, szName, 1024, out outNameSize, out attr,
         out sigBlob, out sigBlobSize,
         out constValueType, out constValue, out constValueSize);

      this.name = Decoder.ReadString(szName, (int)outNameSize);
      this.flags = (CorFieldAttr)attr;
      this.declaringType = Type.Get(tokclass, null);

      int idx = 0;
      CorCallingConvention b = (CorCallingConvention)Decoder.ReadValue(sigBlob, ref idx);
      if (b != CorCallingConvention.FIELD)
        throw new ArgumentException("Not a field signature");
      CustomMod.ReadCustomMods(sigBlob, ref idx);
      this.type = new Type(sigBlob, ref idx, this.declaringType);

      if (this.IsLiteral)
        this.constValue = new ConstValue(constValueType, constValue, constValueSize);
    }

    public Type DeclaringType { get { return this.declaringType; } }

    public int CompareTo(Field o)
    {
      if ((!o.IsLiteral) || (!this.IsLiteral))
        return this.Name.CompareTo(o.Name);
      return this.Literal.CompareTo(o.Literal);
    }

    public string Name { get { return this.name; } }
    public bool IsPrivateScope
    { get { return (this.flags & CorFieldAttr.fdFieldAccessMask) == CorFieldAttr.fdPrivateScope; } }
    public bool IsPrivate
    { get { return (this.flags & CorFieldAttr.fdFieldAccessMask) == CorFieldAttr.fdPrivate; } }
    public bool IsFamANDAssem
    { get { return (this.flags & CorFieldAttr.fdFieldAccessMask) == CorFieldAttr.fdFamANDAssem; } }
    public bool IsAssembly
    { get { return (this.flags & CorFieldAttr.fdFieldAccessMask) == CorFieldAttr.fdAssembly; } }
    public bool IsFamily
    { get { return (this.flags & CorFieldAttr.fdFieldAccessMask) == CorFieldAttr.fdFamily; } }
    public bool IsFamORAssem
    { get { return (this.flags & CorFieldAttr.fdFieldAccessMask) == CorFieldAttr.fdFamORAssem; } }
    public bool IsPublic
    { get { return (this.flags & CorFieldAttr.fdFieldAccessMask) == CorFieldAttr.fdPublic; } }

    public bool IsStatic
    { get { return (this.flags & CorFieldAttr.fdStatic) != 0; } }
    public bool IsInitOnly
    { get { return (this.flags & CorFieldAttr.fdInitOnly) != 0; } }
    public bool IsLiteral
    { get { return (this.flags & CorFieldAttr.fdLiteral) != 0; } }
    public ConstValue Literal
    { get { return this.constValue; } }
    public bool IsNotSerialized
    { get { return (this.flags & CorFieldAttr.fdNotSerialized) != 0; } }

    public bool IsNotSpecialName
    { get { return (this.flags & CorFieldAttr.fdSpecialName) != 0; } }

    public bool IsPinvokeImpl
    { get { return (this.flags & CorFieldAttr.fdPinvokeImpl) != 0; } }

    public bool IsRTSpecialName
    { get { return (this.flags & CorFieldAttr.fdRTSpecialName) != 0; } }
    public bool IsHasFieldMarshal
    { get { return (this.flags & CorFieldAttr.fdHasFieldMarshal) != 0; } }
    public bool IsHasDefault
    { get { return (this.flags & CorFieldAttr.fdHasDefault) != 0; } }
    public bool IsHasFieldRVA
    { get { return (this.flags & CorFieldAttr.fdHasFieldRVA) != 0; } }

    public string AdaImage(uint indent)
    {
      string ret;

      // handle "with" clauses
      Token.AddWith(type);

      // and return the Ada-style field
      ret = Indent (indent) + CheckAdaKeywords(this.Name) + " : ";
      if (this.IsInitOnly)
        ret += "constant ";
      ret += this.type.FullAdaTypeName(false, true);        
      if (this.IsLiteral && this.declaringType.IsEnum)
        ret += " := " + this.constValue.AdaImage();
      ret += ";\n";

      ret += Indent (indent) + "pragma Import (CIL, " + 
               CheckAdaKeywords (this.Name) + ", \"" +
               this.Name + "\");\n";
      
      return ret;
    }
  }

  class EnumComparer : IComparer<Field>
  {
    public int Compare (Field x, Field y)
    {
      if (x.Literal == null && y.Literal == null) return 0;
      if (x.Literal == null) return -1;
      if (y.Literal == null) return 1;
      return x.Literal.CompareTo (y.Literal);
    }
  }

  class FieldComparer : IComparer<Field>
  {
    public int Compare(Field x, Field y)
    {
      return string.Compare(x.Name, y.Name);
    }
  }
}
