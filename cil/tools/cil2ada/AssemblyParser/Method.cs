//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                      cil2ada.AssemblyParser.Method                       //
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
using System.Collections.Generic;
using System.Reflection;

using cil2ada.Interfaces;

namespace cil2ada.AssemblyParser
{
  public enum CallingConvention { Default, Vararg, Generic }

  class Method : Token
  {
    private string name;
    private Type declaringType = null;
    public bool hasThis = false;
    public bool explicitThis = false;
    public CallingConvention callconv = CallingConvention.Default;
    private CorMethodAttr flags = CorMethodAttr.mdPrivateScope;

    private List<Param> parameters = null;
    private List<GenericParam> genParameters = null;
    public Param retType = null;

    public Method(uint token, Type declaringType)
      : base(token)
    {
      char[] szName = new char[1024];
      uint outNameSize;
      uint tokclass, attr, sigBlobSize, codeRVA, implFlags;
      IntPtr sigBlob;

      Context.importIntf.GetMethodProps
       (this.token, out tokclass, szName, 1024, out outNameSize, out attr,
        out sigBlob, out sigBlobSize,
        out codeRVA, out implFlags);

      if ((int)outNameSize <= 1)
        throw new ArgumentException
          ("Method without names unsupported: token is 0x" + token.ToString("x"));
      else
        this.name = Decoder.ReadString(szName, (int)outNameSize);

      int idx = 0;
      this.flags = (CorMethodAttr)attr;
      this.declaringType = declaringType;

      // start reading the signature of this method
      int nParams, nGenericParams;
      CorCallingConvention b = (CorCallingConvention)Decoder.ReadValue(sigBlob, ref idx);

      // first byte contains the flags for HASTHIS, EXPLICITTHIS AND Default/Vararg/Generic calling conventions.

      if ((b & CorCallingConvention.UPMASK) == CorCallingConvention.HASTHIS)
      {
        this.hasThis = true;
        if ((b & CorCallingConvention.UPMASK) == CorCallingConvention.EXPLICITTHIS)
          this.explicitThis = true;
      }

      nGenericParams = 0;
      if ((b & CorCallingConvention.VARARG) != 0)
        throw new System.ArgumentException(this.name + ": Vararg methods not supported");
      else if ((b & CorCallingConvention.GENERIC) != 0)
      {
        this.callconv = CallingConvention.Generic;
        nGenericParams = (int)Decoder.ReadCompressed(sigBlob, ref idx);
      }

      // Retrieve first the generic parameters, as the regular ones might reverence those

      if (nGenericParams > 0)
      {
        IntPtr hHandle = IntPtr.Zero;
        int countTokens = 0;
        uint[] tokenRef = new uint[1];

        Context.importIntf.EnumGenericParams(ref hHandle, this.token, tokenRef, 1, out countTokens);
        this.genParameters = new List<GenericParam>();
        while (countTokens > 0)
        {
          try
          {
            this.genParameters.Add(new GenericParam(tokenRef[0]));
          }
          catch (System.ArgumentException e)
          {
            // better message formating
            throw new System.ArgumentException(string.Format("Unsupported generic argument in method {0} : {1}", e.Message, this.name));
          }
          Context.importIntf.EnumGenericParams(ref hHandle, this.token, tokenRef, 1, out countTokens);
        }
        Context.importIntf.CloseEnum(hHandle);
      }

      // Now read the parameters
      nParams = Decoder.ReadCompressed(sigBlob, ref idx);
      this.retType = new Param(sigBlob, ref idx, this);

      {
        this.parameters = new List<Param>();

        IntPtr hHandle = IntPtr.Zero;
        int countTokens = 0;
        uint[] tokenRef = new uint[1];
        Context.importIntf.EnumParams(ref hHandle, this.token, tokenRef, 1, out countTokens);
        while (countTokens > 0)
        {
          // create the corresponding object
          Param p = null;
          // some parameters returned by the enumerator are useless informative token
          // this raises an exception that we ignore here.
          try
          {
            p = new Param(tokenRef[0], this);
          }
          catch
          {
            p = null;
          }
          if (p != null)
            try
            {
              p.SetSig(sigBlob, ref idx, this);
              this.parameters.Add(p);
            }
            catch (System.ArgumentException e)
            {
              throw new System.ArgumentException
                ("Unsupported parameter " + p.Name + " in method " + this.Name + ": " + e.Message);
            }
          Context.importIntf.EnumParams(ref hHandle, this.token, tokenRef, 1, out countTokens);
        }
        Context.importIntf.CloseEnum(hHandle);
      }
    }

    public bool Equivalent (Method m)
    {
      if (m.Name != this.Name) return false;
      if (m.parameters.Count != this.parameters.Count) return false;
      for (int j = 0; j < this.parameters.Count; j++)
      {
           if (m.parameters[j].type.FullName != this.parameters[j].type.FullName)
               return false;
           if (m.parameters[j].type.FullName == "CIL_Types")
               if (m.parameters[j].type.FullAdaTypeName(false, false)
                   != this.parameters[j].type.FullAdaTypeName(false, false))
                   return false;
      }
      return true;
    }

    public string Name { get { return this.name; } }
    public bool IsPrivateScope
    { get { return (this.flags & CorMethodAttr.mdMemberAccessMask) == CorMethodAttr.mdPrivateScope; } }
    public bool IsPrivate
    { get { return (this.flags & CorMethodAttr.mdMemberAccessMask) == CorMethodAttr.mdPrivate; } }
    public bool IsFamANDAssem
    { get { return (this.flags & CorMethodAttr.mdMemberAccessMask) == CorMethodAttr.mdFamANDAssem; } }
    public bool IsAssembly
    { get { return (this.flags & CorMethodAttr.mdMemberAccessMask) == CorMethodAttr.mdAssembly; } }
    public bool IsFamily
    { get { return (this.flags & CorMethodAttr.mdMemberAccessMask) == CorMethodAttr.mdFamily; } }
    public bool IsFamORAssem
    { get { return (this.flags & CorMethodAttr.mdMemberAccessMask) == CorMethodAttr.mdFamORAssem; } }
    public bool IsPublic
    { get { return (this.flags & CorMethodAttr.mdMemberAccessMask) == CorMethodAttr.mdPublic; } }

    public bool IsStatic
    { get { return (this.flags & CorMethodAttr.mdStatic) != 0; } }
    public bool IsFinal
    { get { return (this.flags & CorMethodAttr.mdFinal) != 0; } }
    public bool IsVirtual
    { get { return (this.flags & CorMethodAttr.mdVirtual) != 0; } }
    public bool IsHideBySig
    { get { return (this.flags & CorMethodAttr.mdHideBySig) != 0; } }

    public bool IsReuseSlot
    { get { return (this.flags & CorMethodAttr.mdVtableLayoutMask) == CorMethodAttr.mdReuseSlot; } }
    public bool IsNewSlot
    { get { return (this.flags & CorMethodAttr.mdVtableLayoutMask) == CorMethodAttr.mdNewSlot; } }

    public bool IsCheckAccessOnOverride
    { get { return (this.flags & CorMethodAttr.mdCheckAccessOnOverride) != 0; } }
    public bool IsAbstract
    { get { return (this.flags & CorMethodAttr.mdAbstract) != 0; } }
    public bool IsSpecialName
    { get { return (this.flags & CorMethodAttr.mdSpecialName) != 0; } }

    public bool IsGeneric
    { get { return this.genParameters != null; } }

    public GenericParam GetGenericParameter(int pos)
    { return this.genParameters[pos]; }

    public Type DeclaringType
    { get { return this.declaringType; } }

    public string AdaImage(ref uint indent, bool IsEnum, bool IsDelegate)
    {
      string ret = "";

      ret += Indent(indent);

      if (IsDelegate)
        ret += "type Delegate is access ";
      else if (this.IsGeneric)
      {
        ret += "generic\n";
        indent++;
        foreach (GenericParam gp in this.genParameters)
          ret += Indent(indent) + gp.AdaImage() + "\n";
        indent--;
        ret += Indent(indent);
      }

      if ((this.retType.Kind == ParamKind.Void) && this.Name != ".ctor")
        ret += "procedure ";
      else
        ret += "function ";

      if (!IsDelegate)
      {
        if (this.Name == ".ctor")
        {
          ret += "new_" + CheckAdaKeywords(this.declaringType.Name) + " ";
          SetHasConstructor("new_" + CheckAdaKeywords(this.declaringType.Name));
        }
        else
          ret += CheckAdaKeywords(this.Name) + " ";
      }

      Queue<string> outParams = new Queue<string>();

      if (this.hasThis && !this.explicitThis && !IsDelegate)
      {
        if (this.Name == ".ctor")
        {
          if (this.declaringType.IsDelegate)
            outParams.Enqueue("This : Delegate := null");
          else if (!this.declaringType.IsValueType && !this.declaringType.IsEnum && !IsEnum)
            outParams.Enqueue("This : access Typ'Class := null");
        }
        else if (this.declaringType.IsValueType || this.declaringType.IsEnum || IsEnum)
          outParams.Enqueue("This : ValueType");
        else
          outParams.Enqueue("This : access Typ");
      }

      foreach (Param p in this.parameters)
        outParams.Enqueue(p.AdaImage(false, false, this.declaringType));

      if (outParams.Count > 0)
      {
        ret += "\n" + Indent(indent) + "  (";
        indent++;
        while (outParams.Count > 0)
        {
          ret += outParams.Dequeue();
          if (outParams.Count > 0) ret += ";\n" + Indent(indent);
        }
        ret += ")";
        indent--;
      }

      if (this.Name == ".ctor")
      {
        if (this.declaringType.IsValueType || this.declaringType.IsEnum)
          ret += " return ValueType";
        else if (this.declaringType.IsDelegate)
          ret += " return Delegate";
        else
          ret += " return Ref";
      }
      else if (this.retType.Kind != ParamKind.Void)
      {
        ret += " return " + this.retType.AdaImage(true, IsDelegate, this.declaringType);
      }

      if (this.IsAbstract)
        ret += " is abstract;";
      else
        ret += ";";

      ret += "\n";

      if (!IsDelegate && this.Name != ".ctor")
        ret += Indent(indent) + "pragma Export (CIL, " + CheckAdaKeywords(this.Name) + ", \"" + this.Name + "\");\n";

      if (this.IsFamily)
      {
        ret += Indent(indent) + "--  This subprogram has 'family' visibility: only children of the type can\n";
        ret += Indent(indent) + "--   have access to it.\n";
      }

      return ret;
    }
  }

  class MethodComparer : IComparer<Method>
  {
    public int Compare(Method x, Method y)
    {
      if (x.IsStatic != y.IsStatic)
        return x.IsStatic ? 1 : -1;
      return string.Compare(x.Name, y.Name);
    }
  }
}
