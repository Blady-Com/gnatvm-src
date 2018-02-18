//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                       cil2ada.AssemblyParser.Type                        //
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
  public enum TypeKind
  {
    None, Builtin, Class, MVar, Ptr, SzArray, ValueType, Var
  }

  public enum TokenKind { TypeDef, TypeSpec, TypeRefResolved, TypeRefUnresolved, BuiltIn };

  //////////
  // Type //
  //////////

  /// <summary>
  /// This class represents the Type definitions that can be found in an assembly.
  ///
  /// Two main kind of types can be found in an assembly: those that have a name or directly
  /// reference one with a name, and the built-in types. Both kind are handled here.
  /// </summary>
  class Type : Token
  {
    public static bool Quiet = false;
    public static bool Verbose = false;

    // this dictionary contains the collection of token-based types
    private static System.Collections.Generic.Dictionary<UInt64, Type> dict =
        new System.Collections.Generic.Dictionary<UInt64, Type> ();

    private TypeKind kind = TypeKind.None;
    private string name = null;
    private string nspace = null;
    private string adaname = null;
    private string shortadaname = null;

    private TokenKind tokenKind = TokenKind.BuiltIn;
    private IMetaDataImport2 scope = null;
    private System.UInt32 tokenscope = 0;

    private CorTypeAttr flags = CorTypeAttr.tdNotPublic;
    private Type extends = null;
    private Type outer = null;
    private List<Type> nested = null;
    private List<Field> fields = null;
    private List<Method> methods = null;
    private List<Type> interfaces = null;
    private List<GenericParam> genParameters = null;
    private List<Type> genInstParameters = null;
    private GenericParam var = null;

    // ARRAY AND SZARRAY
    private Type elementType = null;

    // VAR AND MVAR : Method or type generic parameter reference
    private int varNum;

    private void initFromType (Type t)
    {
      this.kind = t.kind;
      this.name = t.name;
      this.nspace = t.nspace;
      this.adaname = t.adaname;
      this.shortadaname = t.shortadaname;
      this.token = t.token;
      this.tokenKind = t.tokenKind;
      this.scope = t.scope;
      this.tokenscope = t.tokenscope;
      this.flags = t.flags;
      this.extends = t.extends;
      this.outer = t.outer;
      this.nested = t.nested;
      this.fields = t.fields;
      this.methods = t.methods;
      this.interfaces = t.interfaces;
      this.genParameters = t.genParameters;
      this.var = t.var;
      this.elementType = t.elementType;
      this.varNum = t.varNum;
    }

    static public Type Get (uint token, Token context)
    {
      Type type;
      UInt64 key = (UInt64)token +
        ((UInt64)Context.importIntf.GetHashCode () << 32);

      if (Type.dict.TryGetValue (key, out type))
        return type;
      try
      {
        type = new Type (token);
        Type.dict.Add (key, type);
        type.initFromToken (token, context);

        return type;
      }
      catch
      {
        Type.dict.Remove (key);
        Type.dict.Add (key, null);
        return null;
      }
    }

    private Type (uint token)
      : base (token)
    {
    }

    private Type (uint token, Token context)
      : base (token)
    {
      this.initFromToken (token, context);
    }

    public Type (IntPtr sig, ref int idx, Token context)
      : base (0)
    {
      this.initFromSig (sig, ref idx, context);
    }

    private void initFromToken (uint token, Token context)
    {
      // Not found: actually initialize the type
      CorTokenType tokenType = (CorTokenType)token & CorTokenType.mdtMask;
      char[] szName = new char[1024];
      int outNameSize;
      TypeAttributes flags = 0;
      uint extends = 0;
      uint scope = 0;
      IntPtr sigBlob;
      uint sigBlobSize;
      int idx = 0;

      this.token = token;
      this.tokenscope = token;

      // First verify what token type we have
      switch (tokenType)
      {
        case CorTokenType.mdtTypeRef:
          this.tokenKind = TokenKind.TypeRefUnresolved;
          break;
        case CorTokenType.mdtTypeSpec:
          this.tokenKind = TokenKind.TypeSpec;
          break;
        case CorTokenType.mdtTypeDef:
          this.tokenKind = TokenKind.TypeDef;
          break;
        default:
          throw new System.ArgumentException ("Invalid token type when creating a type");
      }

      // by default set to class kind.
      if (this.kind == TypeKind.None)
      {
        this.kind = TypeKind.Class;
        this.adaname = "Ref";
      }

      //  try to resolve type ref
      if (this.tokenKind == TokenKind.TypeRefUnresolved)
      {
        Object rawMetaDataImportScope;
        try
        {
          uint res = Context.importIntf.ResolveTypeRef
            (this.token, ref Assembly.gMetaDataImportGuid,
             out rawMetaDataImportScope, out this.tokenscope);

          if (res == 0)
          {
            // we've got the defining assembly. Let's set it as current context.
            // note that we need to use the new token (TypeDef token) for continuing the analysis.
            this.scope = (IMetaDataImport2) rawMetaDataImportScope;
            Context.SetContext (this.scope);
            this.tokenKind = TokenKind.TypeRefResolved;
          }
        }
        catch {}

      }
      // simple handling of TypeSpec tokens
      else if (this.tokenKind == TokenKind.TypeSpec)
      {
        Context.importIntf.GetTypeSpecFromToken
          (this.tokenscope, out sigBlob, out sigBlobSize);
        idx = 0;
        this.flags = CorTypeAttr.tdPublic;
        this.initFromSig (sigBlob, ref idx, context);
        return;
      }

      //  now get as much information as we can from TypeDef/TypeRef tokens
      if (this.tokenKind == TokenKind.TypeRefUnresolved)
        Context.importIntf.GetTypeRefProps
          (this.tokenscope, out scope, szName, 1024, out outNameSize);
      else
        Context.importIntf.GetTypeDefProps
          (this.tokenscope, szName, 1024, out outNameSize, out flags, out extends);

      // resolve the class name
      string name = "";
      try
      {
        name = Decoder.ReadString (szName, (int)outNameSize);
        if (name == "") throw new ArgumentException ();

        idx = name.LastIndexOf ('.');
        if (idx >= 0)
        {
          this.nspace = name.Substring (0, idx);
          this.name = name.Substring (idx + 1);
        }
        else
          this.name = name;

        this.adaname = "Typ";

        // if (this.name.StartsWith ("_"))
        //  throw new ArgumentException ();
      }
      catch
      {
        // we exit now, so we release the context
        if (this.tokenKind == TokenKind.TypeRefResolved) Context.ReleaseContext ();
        throw new ArgumentException
          ("token 0x" + this.tokenscope.ToString ("x") + " has an invalid name for a type");
      }

      // if current token is a TypeDef (or a resolved TypeRef), we can
      // retrieve its parent and outer class, and its potential generic parameters.
      if (this.tokenKind != TokenKind.TypeRefUnresolved)
      {
        this.flags = (CorTypeAttr)flags;
        this.extends = Get (extends, context);

        // now that extends is initialized, we can verify if we have a class or a valuetype
        if (this.extends != null)
          // Types extending System.ValueType are valuetypes, except System.Enum
          if ((this.extends.FullName == "System.ValueType") && (this.FullName != "System.Enum"))
          {
            this.kind = TypeKind.ValueType;
            this.adaname = "ValueType";
          }
          else if (this.extends.FullName == "System.Enum")
          {
            this.kind = TypeKind.ValueType;
            this.adaname = "ValueType";
          }
          else if (this.IsDelegate)
            this.adaname = "Delegate";


        if (this.IsNested)
        {
          uint outer;
          Context.importIntf.GetNestedClassProps (this.tokenscope, out outer);
          this.outer = Get (outer, context);
          if (this.outer.nested == null)
            this.outer.nested = new List<Type> ();
          this.outer.nested.Add (this);
        }

        IntPtr hHandle = IntPtr.Zero;
        int countTokens = 0;
        uint[] tokenRef = new uint[1];

        Context.importIntf.EnumGenericParams (ref hHandle, this.tokenscope, tokenRef, 1, out countTokens);
        if (countTokens > 0)
          this.genParameters = new List<GenericParam> ();
        while (countTokens > 0)
        {
          // create the corresponding object
          try
          {
              this.genParameters.Add(new GenericParam(tokenRef[0]));
          } catch {
              if (this.tokenKind == TokenKind.TypeRefResolved)
                 Context.ReleaseContext();
              throw;
          }
          Context.importIntf.EnumGenericParams (ref hHandle, this.tokenscope, tokenRef, 1, out countTokens);
        }
        Context.importIntf.CloseEnum (hHandle);

        // and finally release the context
        if (this.tokenKind == TokenKind.TypeRefResolved) Context.ReleaseContext ();
      }
      else if (Verbose)
        System.Console.WriteLine ("(dbg) COULD NOT RESOLVE " + this.FullName);
    }

    public void initFromSig (IntPtr sig, ref int idx, Token context)
    {
      List<CustomMod> customMods;
      Byte b = Decoder.ReadValue (sig, ref idx);
      this.flags = CorTypeAttr.tdPublic;
      switch ((CorElementType)b)
      {
        case CorElementType.BOOLEAN:
          this.kind = TypeKind.Builtin;
          this.name = "CIL_Types";
          this.adaname = "bool";
          this.shortadaname = "Standard.Boolean";
          break;

        case CorElementType.CHAR:
          this.kind = TypeKind.Builtin;
          this.name = "CIL_Types";
          this.adaname = "char";
          this.shortadaname = "Wide_Character";
          break;

        case CorElementType.I1:
          this.kind = TypeKind.Builtin;
          this.name = "CIL_Types";
          this.adaname = "int8";
          break;

        case CorElementType.U1:
          this.kind = TypeKind.Builtin;
          this.name = "CIL_Types";
          this.adaname = "unsigned_int8";
          break;

        case CorElementType.I2:
          this.kind = TypeKind.Builtin;
          this.name = "CIL_Types";
          this.adaname = "int16";
          this.shortadaname = "Short_Integer";
          break;

        case CorElementType.U2:
          this.kind = TypeKind.Builtin;
          this.name = "CIL_Types";
          this.adaname = "unsigned_int16";
          break;

        case CorElementType.I4:
          this.kind = TypeKind.Builtin;
          this.name = "CIL_Types";
          this.adaname = "int32";
          this.shortadaname = "Integer";
          break;

        case CorElementType.U4:
          this.kind = TypeKind.Builtin;
          this.name = "CIL_Types";
          this.adaname = "unsigned_integer";
          break;

        case CorElementType.I8:
          this.kind = TypeKind.Builtin;
          this.name = "CIL_Types";
          this.adaname = "int64";
          this.shortadaname = "Long_Long_Integer";
          break;

        case CorElementType.U8:
          this.kind = TypeKind.Builtin;
          this.name = "CIL_Types";
          this.adaname = "unsigned_long_long_integer";
          break;

        case CorElementType.R4:
          this.kind = TypeKind.Builtin;
          this.name = "CIL_Types";
          this.adaname = "float32";
          this.shortadaname = "Float";
          break;

        case CorElementType.R8:
          this.kind = TypeKind.Builtin;
          this.name = "CIL_Types";
          this.adaname = "float64";
          this.shortadaname = "Long_Float";
          break;

        case CorElementType.I:
          this.kind = TypeKind.Builtin;
          this.name = "CIL_Types";
          this.adaname = "native_int";
          break;

        case CorElementType.U:
          throw new ArgumentException
            ("Unsigned native int not CLI compliant");

        case CorElementType.ARRAY:
          this.kind = TypeKind.Builtin;
          this.nspace = "System";
          this.name = "Array";
          this.adaname = "Ref";
          break;

        case CorElementType.CLASS:
          this.kind = TypeKind.Class;
          Type t = Type.Get (Decoder.ReadTypeDefOrRefEncoded (sig, ref idx), context);
          if (t != null)
            this.initFromType (t);
          else
            throw new System.ArgumentException ("Unsupported class type");
          break;

        case CorElementType.FNPTR:
          throw new System.ArgumentException ("FNPTR type not supported");

        case CorElementType.GENERICINST:
          this.initFromSig (sig, ref idx, context);
          this.genInstParameters = new List<Type> ();
          int n = Decoder.ReadCompressed (sig, ref idx);
          for (int j = 0; j < n; j++)
            this.genInstParameters.Add(new Type (sig, ref idx, context));
          break;

        case CorElementType.MVAR:
          this.kind = TypeKind.MVar;
          this.varNum = Decoder.ReadCompressed (sig, ref idx);
          if (context is Method)
            this.var = ((Method)context).GetGenericParameter (this.varNum);
          else
            throw new System.ArgumentException ("Expected a Method environment when creating MVAR type");
          break;

        case CorElementType.OBJECT:
          this.kind = TypeKind.Builtin;
          this.nspace = "System";
          this.name = "Object";
          this.adaname = "Ref";
          break;

        case CorElementType.VOID:
          this.kind = TypeKind.Builtin;
          this.name = "CIL_Types";
          this.adaname = "void";
          break;

        case CorElementType.PTR:
          throw new System.ArgumentException ("dereferenced CIL types not supported");
          // this.kind = TypeKind.Ptr;
          // customMods = CustomMod.ReadCustomMods (sig, ref idx);
          // this.elementType = new Type (sig, ref idx, context);
          // break;

        case CorElementType.STRING:
          this.kind = TypeKind.Builtin;
          this.nspace = "System";
          this.name = "String";
          this.adaname = "Ref";
          break;

        case CorElementType.SZARRAY:
          this.kind = TypeKind.SzArray;
          customMods = CustomMod.ReadCustomMods (sig, ref idx);
          this.elementType = new Type (sig, ref idx, context);

          //  We map arrays of arrays to System.Array
          if (this.elementType.Kind == TypeKind.SzArray)
          {
            this.kind = TypeKind.Builtin;
            this.nspace = "System";
            this.name = "Array_k";
            this.adaname = "Ref";
          }

          break;

        case CorElementType.VALUETYPE:
          this.kind = TypeKind.ValueType;
          uint typeDefOrRef = Decoder.ReadTypeDefOrRefEncoded(sig, ref idx);
          Type auxt = Type.Get(typeDefOrRef, context);

          if (auxt != null)
          {
             this.initFromType(auxt);
          }
          else
          {
              throw new System.ArgumentException
                 ("Invalid ElementType during type signature analysis: 0x" 
                  + b.ToString("x"));
          }

          break;

        case CorElementType.VAR:
          //  ??? We should get the actual GenericParam here...
          this.kind = TypeKind.Var;
          this.varNum = Decoder.ReadCompressed (sig, ref idx);
          if (context is Method)
            this.initFromType (((Method)context).DeclaringType.GetGenericParameter (this.varNum));
          else if (context is Type)
            this.initFromType (((Type)context).GetGenericParameter (this.varNum));
          else
            throw new System.ArgumentException ("Expected a Method or Type environment when creating VAR type");
          break;

        default:
          throw new System.ArgumentException
            ("Invalid ElementType during type signature analysis: 0x" + b.ToString ("x"));
      }
    }

    public string Name
    { get { return this.name; } }
    public string Namespace
    { get { return this.nspace; } }
    public string FullName
    {
      get
      {
        if (this.outer != null)
          return this.outer.FullName + "/" + this.name;
        else if (this.nspace != null)
          return this.nspace + "." + this.name;
        else if (this.kind == TypeKind.SzArray)
          return this.elementType.FullName + "[]";
        else if (this.kind == TypeKind.Ptr)
          return "*" + this.elementType.FullName;
        else if (this.kind == TypeKind.Var || this.kind == TypeKind.MVar)
          return this.var.Name;
        else
          return this.name;
      }
    }

    public string FullPackageName
    {
      get
      {
        string ret;

        if (this.outer != null)
          ret = this.outer.FullPackageName + "_" + CheckAdaKeywords (this.name);
        else if (this.nspace != null)
          ret = CheckAdaKeywords (this.nspace) + "." + CheckAdaKeywords (this.name);
        else if (this.kind == TypeKind.SzArray)
          ret = this.elementType.FullPackageName;
        else if (this.kind == TypeKind.Ptr)
          ret = this.elementType.FullPackageName;
        else if (this.kind == TypeKind.Var || this.kind == TypeKind.MVar)
          ret = "";
        else
          ret = CheckAdaKeywords (this.name);

        if (ret.StartsWith ("System."))
          ret = "MSSyst" + ret.Substring (6);

        return ret;
      }
    }

    private enum nameContext
    {
      ContextDefault,
      ContextRet,
      ContextField
    };

    private string Int_FullAdaTypeName (bool withSuffix, nameContext context)
    {
      if (!withSuffix && this.shortadaname != null)
        return this.shortadaname;

      else if (this.kind == TypeKind.SzArray)
      {
        if (!withSuffix && this.elementType.Int_FullAdaTypeName (true, nameContext.ContextDefault) == "CIL_Types.unsigned_int8")
          // both ada "String" and "access String" are mapping arrays of unsigned_int8. So we map to simple
          // strings here for convenience, except when the type is enclosed in an object (e.g. is a field)
          // as Ada refuses unconstrained fields.
          if (context == nameContext.ContextField)
            return "access Standard.String";
          else
            return "Standard.String";

        else if (!withSuffix && this.elementType.Int_FullAdaTypeName (true, nameContext.ContextDefault) == "CIL_Types.char")
          // same remark as above, but for Wide_String.
          if (context == nameContext.ContextField)
            return "access Wide_String";
          else
            return "Wide_String";

        else if (!withSuffix)
          return "access " + this.elementType.Int_FullAdaTypeName (true, nameContext.ContextDefault) + "_Arr";

        else
          return this.elementType.Int_FullAdaTypeName (true, nameContext.ContextDefault) + "_Array";
      }
      // ??? Disabled, as TypeKind.Ptr is not handled
      // else if (this.kind == TypeKind.Ptr)
      // {
      //   if (this.elementType.name == "CIL_Types")
      //     return this.elementType.FullAdaTypeName (true, false, false) + "_addrof";
      //   else if (this.elementType.IsValueType)
      //     return "access " + this.elementType.FullAdaTypeName (false, false, false);
      //   else
      //     return "access " + this.elementType.FullAdaTypeName (true, false, false);
      // }
      else if (this.kind == TypeKind.Var || this.kind == TypeKind.MVar)
      {
        if (withSuffix && this.kind == TypeKind.MVar)
          throw new ArgumentException ("Referenced generic parameter or Array of generic parameters not supported");
        return this.var.Name;
      }
      else
      {
        string ret = this.FullPackageName;
        if (this.FullPackageName.Length > 0) ret += ".";

        if (this.IsDelegate)
          return ret + "Delegate";
        else if (this.IsEnum)
          return ret + "ValueType";
        else if (this.IsValueType)
          if (withSuffix || context != nameContext.ContextDefault)
            return ret + "ValueType";
          else
            return ret + "ValueType'Class";
        else if (this.IsClass || this.IsInterface)
          if (withSuffix)
            return ret + "Ref";
          else
            return "access " + ret + "Typ'Class";
        else if (this.adaname != null)
          if (this.adaname == "Ref" && !withSuffix)
            return "access " + ret + "Typ'Class";
          else
            return ret + this.adaname;
        else
          // ???
          return ret + "Ref";
      }
    }

    public string FullAdaTypeName (bool isRet, bool isField)
    {
      nameContext context = nameContext.ContextDefault;
      if (isRet) context = nameContext.ContextRet;
      else if (isField) context = nameContext.ContextField;
      return Int_FullAdaTypeName (false, context);
    }

    public bool HasElementType { get { return this.kind == TypeKind.SzArray || this.kind == TypeKind.Ptr; } }
    public Type GetElementType { get { return this.elementType; } }
    public bool IsMVar { get { return this.kind == TypeKind.MVar; } }
    public bool IsVar { get { return this.kind == TypeKind.Var; } }
    public int VarNum { get { return (int)this.varNum; } }
    public bool IsNotPublic { get { return (this.flags & CorTypeAttr.tdVisibilityMask) == CorTypeAttr.tdNotPublic; } }
    public bool IsPublic { get { return (this.flags & CorTypeAttr.tdVisibilityMask) == CorTypeAttr.tdPublic; } }
    public bool IsNested { get { return (uint)(this.flags & CorTypeAttr.tdVisibilityMask) > 1; } }
    public bool IsNestedPublic { get { return (this.flags & CorTypeAttr.tdVisibilityMask) == CorTypeAttr.tdNestedPublic; } }
    public bool IsNestedPrivate { get { return (this.flags & CorTypeAttr.tdVisibilityMask) == CorTypeAttr.tdNestedPrivate; } }
    public bool IsNestedFamily { get { return (this.flags & CorTypeAttr.tdVisibilityMask) == CorTypeAttr.tdNestedFamily || this.IsNestedFamORAssem; } }
    public bool IsNestedAssembly { get { return (this.flags & CorTypeAttr.tdVisibilityMask) == CorTypeAttr.tdNestedAssembly || this.IsNestedFamORAssem; } }
    public bool IsNestedFamANDAssem { get { return (this.flags & CorTypeAttr.tdVisibilityMask) == CorTypeAttr.tdNestedFamANDAssem; } }
    public bool IsNestedFamORAssem { get { return (this.flags & CorTypeAttr.tdVisibilityMask) == CorTypeAttr.tdNestedFamORAssem; } }
    public bool IsAutoLayout { get { return (this.flags & CorTypeAttr.tdLayoutMask) == CorTypeAttr.tdAutoLayout; } }
    public bool IsLayoutSequential { get { return (this.flags & CorTypeAttr.tdLayoutMask) == CorTypeAttr.tdSequentialLayout; } }
    public bool IsExplicitLayout { get { return (this.flags & CorTypeAttr.tdLayoutMask) == CorTypeAttr.tdExplicitLayout; } }
    public bool IsClass
    {
      get
      {
        if (this.token != 0)
          return (this.flags & CorTypeAttr.tdClassSemanticsMask) == CorTypeAttr.tdClass;
        else
          return false;
      }
    }
    public bool IsInterface { get { return (this.flags & CorTypeAttr.tdClassSemanticsMask) == CorTypeAttr.tdInterface; } }
    public bool IsAbstract { get { return (this.flags & CorTypeAttr.tdAbstract) != 0; } }
    public bool IsSealed { get { return (this.flags & CorTypeAttr.tdSealed) != 0; } }
    public bool IsSpecialName { get { return (this.flags & CorTypeAttr.tdSpecialName) != 0; } }
    public bool IsImport { get { return (this.flags & CorTypeAttr.tdImport) != 0; } }
    public bool IsSerializable { get { return (this.flags & CorTypeAttr.tdSerializable) != 0; } }
    public bool IsAnsiClass { get { return (this.flags & CorTypeAttr.tdStringFormatMask) == CorTypeAttr.tdAnsiClass; } }
    public bool IsUnicodeClass { get { return (this.flags & CorTypeAttr.tdStringFormatMask) == CorTypeAttr.tdUnicodeClass; } }
    public bool IsAutoClass { get { return (this.flags & CorTypeAttr.tdStringFormatMask) == CorTypeAttr.tdAutoClass; } }
    public bool IsValueType { get { return this.kind == TypeKind.ValueType; } }
    public bool IsEnum { get { return this.extends != null && this.extends.FullName == "System.Enum"; } }
    public bool IsDelegate { get { return this.IsSubclassOf ("System.Delegate"); } }
    public bool IsException { get { return this.IsSubclassOf ("System.Exception"); } }
    public bool IsGeneric { get { return (this.genParameters != null && this.genInstParameters == null); } }
    public bool IsGenericInstance { get { return (this.genParameters != null && this.genInstParameters != null); } }
    public Type GetGenericParameter (int pos)
    {
      if (this.extends == null)
        throw new System.ArgumentException ("Not an instance");
      if (this.extends.genInstParameters == null)
        throw new System.ArgumentException ("Not an instance");
      return this.extends.genInstParameters[pos];
    }
    public TypeKind Kind { get { return this.kind; } }
    public Type ElementType { get { return this.elementType; } }

    public Type GetBaseType { get { return this.extends; } }
    public Type GetOuterType { get { return this.outer; } }

    public List<Type> GetInterfaces ()
    {
      if (this.tokenKind == TokenKind.TypeSpec || this.tokenKind == TokenKind.TypeRefUnresolved)
        throw new System.ArgumentException ("Cannot retrieve members from a non TypeDef token");
      if (this.interfaces != null)
        return this.interfaces;
      if (this.extends != null && this.extends.genInstParameters != null)
      {
        this.interfaces = this.extends.GetInterfaces ();
        return this.interfaces;
      }

      if (this.tokenKind == TokenKind.TypeRefResolved)
        Context.SetContext (this.scope);

      this.interfaces = new List<Type> ();

      IntPtr hHandle = IntPtr.Zero;
      int countTokens = 0;
      uint[] tokenRef = new uint[1];

      Context.importIntf.EnumInterfaceImpls
        (ref hHandle, this.tokenscope, tokenRef, 1, out countTokens);
      while (countTokens > 0)
      {
        uint intfclass, intftoken;
        // get interface props
        Context.importIntf.GetInterfaceImplProps (tokenRef[0], out intfclass, out intftoken);
        // create the corresponding object
        try
        {
          Type type = new Type (intftoken, null);
          this.interfaces.Add (type);
        }
        catch {}
        Context.importIntf.EnumInterfaceImpls (ref hHandle, this.tokenscope, tokenRef, 1, out countTokens);
      }
      Context.importIntf.CloseEnum (hHandle);

      this.interfaces.Sort (new TypeComparer ());

      if (this.tokenKind == TokenKind.TypeRefResolved)
        Context.ReleaseContext ();

      return this.interfaces;
    }

    public List<Field> GetFields ()
    {
      if (this.tokenKind == TokenKind.TypeSpec || this.tokenKind == TokenKind.TypeRefUnresolved)
        throw new System.ArgumentException ("Cannot retrieve members from a non TypeDef token");
      if (this.fields != null)
        return this.fields;
      if (this.extends != null && this.extends.genInstParameters != null)
      {
        this.fields = this.extends.GetFields ();
        return this.fields;
      }

      if (this.tokenKind == TokenKind.TypeRefResolved)
        Context.SetContext (this.scope);

      this.fields = new List<Field> ();
      IntPtr hHandle = IntPtr.Zero;
      int countTokens = 0;
      uint[] tokenRef = new uint[1];

      // First retrieve fields

      Context.importIntf.EnumFields (ref hHandle, this.tokenscope, tokenRef, 1, out countTokens);
      while (countTokens > 0)
      {
        try
        {
          // create the corresponding object
          this.fields.Add (new Field (tokenRef[0]));
        }
        catch { }
        Context.importIntf.EnumFields (ref hHandle, this.tokenscope, tokenRef, 1, out countTokens);
      }
      Context.importIntf.CloseEnum (hHandle);

      this.fields.Sort (new FieldComparer ());

      if (this.tokenKind == TokenKind.TypeRefResolved)
        Context.ReleaseContext ();

      return this.fields;
    }

    private void InternalGetMethods (Type fromType, Boolean addConstructors)
    {
      IntPtr hHandle = IntPtr.Zero;
      int countTokens = 0;
      uint[] tokenRef = new uint[1];

      Context.importIntf.EnumMethods (ref hHandle, fromType.tokenscope, tokenRef, 1, out countTokens);
      while (countTokens > 0)
      {
        try
        {
          bool insertMethod = true;
          Method m = new Method (tokenRef[0], this);
          foreach (Method old in this.methods) {
            if (old.Equivalent (m)) insertMethod = false;
          }
          if (insertMethod && (addConstructors || m.Name != ".ctor"))
             this.methods.Add (m);
        }
        catch (System.ArgumentException e)
        {
          if (Verbose)
          {
            System.Console.WriteLine
              (string.Format ("(Info) A Method of {0} will not be binded:", this.FullName));
            System.Console.WriteLine
              ("       " + e.Message);
          }
        }
        catch (System.Exception e)
        {
          System.Console.WriteLine (e.ToString ());
        }
        Context.importIntf.EnumMethods (ref hHandle, fromType.tokenscope, tokenRef, 1, out countTokens);
      }
      Context.importIntf.CloseEnum (hHandle);
    }

    public List<Method> GetMethods ()
    {
      if (this.tokenKind == TokenKind.TypeSpec || this.tokenKind == TokenKind.TypeRefUnresolved)
        throw new System.ArgumentException ("Cannot retrieve members from a non TypeDef token");
      if (this.methods != null)
        return this.methods;

      if (this.tokenKind == TokenKind.TypeRefResolved)
        Context.SetContext (this.scope);

      this.methods = new List<Method> ();

      this.InternalGetMethods (this, true);

      // in case of generic instance, we evaluate the generic's methods so as the binding do not make
      // any reference to the generic (unsupported by the Ada compiler)
      if (this.extends != null && this.extends.genInstParameters != null)
        this.InternalGetMethods (this.extends, false);

      this.methods.Sort (new MethodComparer ());

      if (this.tokenKind == TokenKind.TypeRefResolved)
        Context.ReleaseContext ();

      return this.methods;
    }

    public bool IsSubclassOf (string ancestorName)
    {
      if (this.extends == null) return false;
      if (this.extends.FullName == ancestorName) return true;
      return this.extends.IsSubclassOf (ancestorName);
    }
    public bool IsSubclassOf (Type ancestor)
    {
      return this.IsSubclassOf (ancestor.FullName);
    }

    public void AddWith ()
    {
      this.AddWith (false);
    }

    public void AddWith (bool forceWith)
    {
      if (this.FullPackageName == "") return;
      else if (this.HasElementType &&
          (this.ElementType.IsValueType ||
           this.ElementType.IsDelegate))
        AddWith (this.ElementType);
      else if (this.IsDelegate || forceWith)
        AddWith (this);
      else
        AddLimitedWith (this);
    }

    private string AdaEnum (ref uint indent)
    {
      string ret;
      string vals;
      List<Field> fields = this.GetFields ();
      List<Field> duplicated = new List<Field> ();
      Field last = null;
      fields.Sort (new EnumComparer());

      ret = Indent (indent) + "type ValueType is\n";
      vals = "";
      ret += Indent (indent) + "  (";
      indent++;
      bool firstitem = true;
      foreach (Field f in fields)
      {
        if (!f.IsLiteral) continue;
        if (last != null)
          if (last.CompareTo (f) == 0)
          {
            duplicated.Add (f);
            continue;
          }

        if (!firstitem)
        {
          ret += ",\n" + Indent (indent);
          vals += ",\n" + Indent (indent);
        }

        firstitem = false;
        ret += CheckAdaKeywords (f.Name);
        vals += CheckAdaKeywords (f.Name) + " => " + f.Literal.AdaImage ();
        last = f;
      }
      ret += ");\n";
      indent--;
      ret += Indent (indent) + "pragma Convention (CIL, ValueType);\n";
      ret += Indent (indent) + "for ValueType use\n";
      ret += Indent (indent) + "  (" + vals + ");\n\n";

      ret += Indent (indent) + "type ValueType_Arr is array (Natural range <>) of ValueType;-- start at 0\n";
      ret += Indent (indent) + "pragma Convention (CIL, ValueType_Arr);\n";
      ret += Indent (indent) + "type ValueType_Array is access all ValueType_Arr;\n";

      foreach (Field f in duplicated)
      {
        ret += Indent (indent) + CheckAdaKeywords (f.Name) + " : constant ValueType := ";
        foreach (Field f2 in fields)
          if (f2.CompareTo (f) == 0)
          {
            ret += CheckAdaKeywords (f2.Name) + ";\n";
            break;
          }
      }
      if (duplicated.Count > 0)
        ret += "\n";

      ret += this.AdaMethods (ref indent, false);

      return ret;
    }

    private string AdaClass (ref uint indent)
    {
      string ret = "";
      string Typ = "Typ";

      if (this.IsValueType) Typ = "ValueType";

      if (this.FullPackageName == "MSSyst.Object")
      {
        ret += Indent (indent) + "type Typ is tagged limited null record;\n";
      }
      else if (this.FullPackageName == "MSSyst.ValueType")
      {
        ret += Indent (indent) + "type Typ is tagged null record;\n";
      }
      else
      {
        ret += Indent (indent) + "type " + Typ + " is ";

        if (this.IsInterface)
        {
          ret += "limited interface;\n";
        }
        else
        {
          if (this.IsAbstract)
            ret += "abstract ";

          //  Try handling inheritance from generic packages
          Type extends = this.extends;
          if (this.extends != null)
          {
            while (extends != null && extends.IsGenericInstance)
            {
                extends = extends.extends;
            }
          }
          if (extends != null)
          {
            ret += "new " + extends.FullPackageName + ".Typ\n";
            AddWith (extends);

            foreach (Type intf in this.GetInterfaces ())
              if (intf.IsPublic && !intf.IsGeneric && !intf.IsGenericInstance)
              {
                ret += Indent (indent) + "  and " + intf.FullPackageName + ".Typ\n";
                AddWith (intf);
              }
            ret += Indent (indent) + "  with";
          }
          else
          {
            ret += "tagged limited";
          }

          int count = 0;
          foreach (Field f in this.GetFields())
            if (f.IsPublic && !f.IsStatic)
              count++;

          if (count > 0)
          {
            ret += " record\n";
            indent++;
            foreach (Field f in this.GetFields())
              if (f.IsPublic && !f.IsStatic)
                ret += f.AdaImage(indent);
            indent--;
            ret += Indent(indent) + "end record;\n";
          }
          else
            ret += " null record;\n";
        }
      }

      if (this.IsSealed)
        ret += Indent (indent) +
                "--  This type is marked 'sealed': it can't be inherited.\n";
      if (!this.IsInterface)
        ret += Indent (indent) + "pragma Convention (CIL, " + Typ + ");\n\n";

      if (this.IsValueType)
      {
        if (!this.IsAbstract)
        {
          ret += Indent (indent) + "type ValueType_Arr is array (Natural range <>) of ValueType;-- start at 0\n";
          ret += Indent (indent) + "pragma Convention (CIL, ValueType_Arr);\n";
          ret += Indent (indent) + "type ValueType_Array is access all ValueType_Arr;\n";
        }
      }
      else
      {
        ret += Indent (indent) + "type Ref is access all Typ'Class;\n";
        ret += Indent (indent) + "pragma Convention (CIL, Ref);\n";
        ret += Indent (indent) + "type Ref_Arr is array (Natural range <>) of Ref;\n";
        ret += Indent (indent) + "pragma Convention (CIL, Ref_Arr);\n";
        ret += Indent (indent) + "type Ref_Array is access all Ref_Arr;\n";
      }
      ret += "\n";

      ret += this.AdaMethods (ref indent, false);
      ret += this.AdaStaticFields (ref indent);

      if (this.FullPackageName == "MSSyst.String")
      {
        ret += Indent (indent) + "function \"+\" (R : Standard.String) return Mssyst.String.Ref;\n";
        ret += Indent (indent) + "function \"+\" (R : access Mssyst.String.Typ'Class) return Standard.String;\n";
        ret += Indent (indent) + "pragma Convention (Intrinsic, \"+\");\n\n";
      }

      if (this.FullPackageName == "MSSyst.Object")
      {
        ret += Indent (indent) + "function Box (Val : Integer) return Ref;\n";
        ret += Indent (indent) + "function Box (Val : Long_Float) return Ref;\n";
        ret += Indent (indent) + "pragma Import (CIL, Box, \"mgnat.adalib.GNAT_libc.box\");\n\n";
        ret += Indent (indent) + "function Unbox_Integer (Val : access Typ'Class) return Integer;\n";
        ret += Indent (indent) + "function Unbox_Double (Val : access Typ'Class) return Long_Float;\n";
        ret += Indent (indent) + "pragma Import (CIL, Unbox_Integer, \"mgnat.adalib.GNAT_libc.unbox_int\");\n\n";
        ret += Indent (indent) + "pragma Import (CIL, Unbox_Double, \"mgnat.adalib.GNAT_libc.unbox_double\");\n\n";
      }

      if (this.IsValueType)
      {
	AddWith ("MSSyst.Object");
        ret += Indent (indent) + "function \"+\" (R : ValueType'Class) return Mssyst.Object.Ref;\n";
        ret += Indent (indent) + "function \"+\" (R : access Mssyst.Object.Typ'Class) return ValueType'Class;\n";
        ret += Indent (indent) + "pragma Convention (Intrinsic, \"+\");\n\n";
      }

      return ret;
    }

    private string AdaDelegate (ref uint indent)
    {
      string str = "";

      Method invoke = null;
      Method ctor = null;

      foreach (Method m in this.GetMethods ())
      {
        if (m.Name == "Invoke")
        {
          invoke = m;
        } else if (m.Name == ".ctor") {
          ctor = m;
        }
      }

      if (invoke != null)
      {
        str += invoke.AdaImage (ref indent, false, true);
        str += Indent (indent) + "pragma Import (CIL, Delegate, \"" + this.Name + "\");\n\n";
        if (ctor != null) {
          str += ctor.AdaImage (ref indent, false, false);
          str += Indent (indent) + "--  This constructor is used to create a delegate from an instance method.\n";
          str += Indent (indent) + "--  It needs to be called with a dispatching method as parameter, this\n";
          str += Indent (indent) + "--   method having the same profile as 'Delegate', with an added first\n";
          str += Indent (indent) + "--   dispatching parameter 'This'\n";
          str += Indent (indent) + "--  For example if Delegate is\n";
          str += Indent (indent) + "--   'type Delegate is access procedure (someparam: sometype)'\n";
          str += Indent (indent) + "--   then the the constructor should be called as: \n";
          str += Indent (indent) + "--  declare\n";
          str += Indent (indent) + "--     procedure MyProc (This : access MyType; someparam: sometype) is ...\n";
          str += Indent (indent) + "--     Obj : access MyType := new MyType;\n";
          str += Indent (indent) + "--     Del : Delegate;\n";
          str += Indent (indent) + "--  begin\n";
          str += Indent (indent) + "--     Del := new_Delegate (null, Obj, MyProc'Address);\n";
          str += Indent (indent) + "--  end;\n\n";

        }
      }

      return str;
    }

    public string AdaImage (ref uint indent)
    {
      string ret = "";
      string myName;

      // ??? This code is unused for now, as CIL generics cannot be instantiated from Ada
      if ((this.genParameters != null) && (this.genInstParameters == null))
      {
        ret += Indent (indent) + "generic\n\n";
        indent++;
        foreach (GenericParam param in this.genParameters)
          ret += Indent (indent) + param.AdaImage () + "\n";
        indent--;
        ret += "\n";
      }

      myName = this.FullPackageName;

      ret += Indent (indent) + "package " + myName + " is\n\n";
      indent++;

      // ??? This code is unused for now, as CIL generics cannot be instantiated from Ada
      if ((this.genParameters != null) && (this.genInstParameters == null))
      {
        foreach (GenericParam param in this.genParameters)
        {
          ret += Indent (indent) + "type " + param.Name + "_Arr is array (Natural range <>) of " + param.Name + ";\n";
        }
        ret += "\n";
      }

      if (this.IsEnum)
        ret += this.AdaEnum (ref indent);
      else if (this.IsDelegate)
        ret += this.AdaDelegate (ref indent);
      else if (this.IsAbstract && this.IsSealed)
      {
        ret += this.AdaMethods (ref indent, true);
        ret += this.AdaStaticFields (ref indent);
      }
      else
        ret += this.AdaClass (ref indent);

      indent--;

      ret += Indent (indent) + "private\n\n";
      indent++;
      ret += Token.GetPrivate (indent);
      ret += "\n";
      indent--;

      ret += Indent (indent) + "end " + myName + ";\n";
      ret += Indent (indent) + "pragma Import (CIL, " + myName + ",\n";
      ret += Indent (indent) + "   \"" + Context.mainAsm.AssemblyVersionString + "\",\n";
      string valuetype = "";
      if (this.IsValueType) valuetype = "valuetype ";
      ret += string.Format
            (Indent (indent) + "   \"{0}[{1}]{2}\");\n", valuetype, Context.mainAsm.Name, this.FullName);

      return ret;
    }

    private string AdaStaticFields (ref uint indent)
    {
      int count = 0;
      string ret = "";

      foreach (Field f in this.GetFields ())
        if (f.IsPublic && f.IsStatic)
        {
          count++;
          ret += f.AdaImage (indent);
        }
      if (count > 0) ret += "\n";
      return ret;
    }

    private string AdaMethods (ref uint indent, bool staticonly)
    {
      int count = 0;
      string ret = "";
      List<Method> methods;

      methods = this.GetMethods ();

      foreach (Method m in methods)
        if ((m.IsPublic || m.IsFamily) && !m.IsGeneric && (m.IsStatic || !staticonly))
        {
          try
          {
            ret += m.AdaImage (ref indent, this.IsEnum, false) + "\n";
            count++;
          }
          catch (System.Exception e)
          {
            Console.WriteLine (e.ToString ());
          }
        }
      if (count > 0) ret += "\n";
      return ret;
    }

    private void EnsureParent (string unitname, string outDir)
    {
      int idx = unitname.LastIndexOf (".");
      if (idx < 0) return;
      string newunit = unitname.Substring (0, idx);
      string filename = newunit.Replace (".", "-").ToLower () + ".ads";

      if (outDir != null)
        filename = System.IO.Path.Combine (outDir, filename);

      if (System.IO.File.Exists (filename)) return;

      string ret = "";
      ret += "package " + newunit + " is\n";
      ret += "end " + newunit + ";\n";
      ret += "pragma Import (CIL, " + newunit + ",\n";
      ret += "   \"" + Context.mainAsm.AssemblyVersionString + "\",\n";
      ret += string.Format
            ("   \"[{0}]{1}\");", Context.mainAsm.Name, this.FullName);

      System.IO.TextWriter file =
        new System.IO.StreamWriter (filename);
      file.Write (ret);
      file.Close ();

      EnsureParent (newunit, outDir);
    }

    public void DumpToAda (bool quiet, string outDir)
    {
      Quiet = quiet;

      if (outDir != "")
        if (!System.IO.Directory.Exists(outDir))
          System.IO.Directory.CreateDirectory(outDir);

      if (this.genParameters == null)
      {
        EnsureParent (this.FullPackageName, outDir);

        string filename = this.FullPackageName.Replace (".", "-").ToLower () + ".ads";

        if (!Quiet)
          System.Console.WriteLine ("writing: " + filename);
        if (outDir != null)
          filename = System.IO.Path.Combine (outDir, filename);
        System.IO.TextWriter file =
          new System.IO.StreamWriter (filename);

        Token.Init (this.FullPackageName);
        uint indent = 0;
        string img = this.AdaImage (ref indent);
        file.Write (Token.GetWiths () + "\n");
        file.Write (img);
        file.Close ();
      }
    }
  }

  class TypeComparer : IComparer<Type>
  {
    public int Compare (Type x, Type y)
    {
      return string.Compare (x.FullPackageName, y.FullPackageName);
    }
  }
}
