//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                   cil2ada.AssemblyParser.GenericParam                    //
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
using cil2ada.Interfaces;

namespace cil2ada.AssemblyParser
{
  public enum Variance
  {
    NonVariant = 0x00,
    CoVariant = 0x01,
    ContraVariant = 0x02
  }

  public enum Constraints
  {
    None = 0x00,
    ReferenceType = 0x04,
    NotNullableValueType = 0x08,
    DefaultConstructor = 0x10
  }

  class GenericParam : Token
  {
    private string name = null;
    private CorGenericParamAttr flags;
    private Type constraint;

    public GenericParam(uint token)
      : base(token)
    {
      char[] szName = new char[1024];
      uint outNameSize, reserved, tokcontext, seq, attr;

      // from the generic parameter table
      if ((token & 0xFF000000) != 0x2A000000)
        throw new ArgumentException
          ("Invalid token while creating a GenericParam");

      Context.importIntf.GetGenericParamProps
        (this.token, out seq, out attr, out tokcontext, out reserved,
         szName, 1024, out outNameSize);

      if ((int)outNameSize == 0)
      {
        throw new ArgumentException("invalid name as parameter");
      }
      else
      {
        this.name = Decoder.ReadString(szName, (int)outNameSize);
      }

      this.flags = (CorGenericParamAttr)attr;

      if (this.GetVariance != Variance.NonVariant)
        throw new ArgumentException
          ("generic covariant or contravariant are not supported");

      IntPtr hHandle = IntPtr.Zero;
      int countTokens = 0;
      uint[] tokenRef = new uint[1];
      uint thistoken, typeToken;

      uint res = Context.importIntf.EnumGenericParamConstraints
        (ref hHandle, this.token, tokenRef, 1, out countTokens);
      if (countTokens > 0)
      {
        Context.importIntf.GetGenericParamConstraintProps
          (tokenRef[0], out thistoken, out typeToken);
        this.constraint = Type.Get(typeToken, null);
      }
      Context.importIntf.CloseEnum (hHandle);

    }

    public string Name { get { return this.name; } }
    public Variance GetVariance
    { get { return (Variance)(this.flags & CorGenericParamAttr.gpVarianceMask); } }

    public bool HasReferenceTypeConstraint
    { get { return (this.flags & CorGenericParamAttr.gpSpecialConstraintMask & CorGenericParamAttr.gpReferenceTypeConstraint) != 0; } }
    public bool HasNotNullableConstraint
    { get { return (this.flags & CorGenericParamAttr.gpSpecialConstraintMask & CorGenericParamAttr.gpNotNullableValueTypeConstraint) != 0; } }
    public bool HasDefaultConstructorConstraint
    { get { return (this.flags & CorGenericParamAttr.gpSpecialConstraintMask & CorGenericParamAttr.gpDefaultConstructorConstraint) != 0; } }

    public string AdaImage()
    {
      string ret = "";
      ret += "type " + this.Name;
      if (this.constraint != null)
      {
        AddWith(this.constraint);
        if (this.constraint.IsValueType || this.constraint.IsEnum)
          ret += " is new " + this.constraint.FullPackageName + ".ValueType with private;";
        else
          ret += " is new " + this.constraint.FullPackageName + ".Typ with private;";
      }
      else if (this.HasReferenceTypeConstraint)
      {
        AddWith("MSSyst.Object");
        ret += " is new MSSyst.Object.Typ with private;";
      }
      else if (this.HasNotNullableConstraint)
      {
        AddWith("MSSyst.ValueType");
        ret += " is new MSSyst.ValueType.Typ with private;";
      }
      else
        ret += " is private;";

      return ret;
    }
  }
}
