//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                     cil2ada.AssemblyParser.Assembly                      //
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
using System.Runtime.InteropServices;
using cil2ada.Interfaces;

namespace cil2ada.AssemblyParser
{

  class Assembly
  {
    public IMetaDataImport2 importIntf = null;
    public IMetaDataAssemblyImport asmImportIntf = null;

    static public Guid gMetaDataImportGuid = 
      new Guid ("FCE5EFA0-8BBA-4f8e-A036-8F2022B08466");
    static public Guid gMetaDataAsmImportGuid = 
      new Guid ("EE62470B-E94B-424e-9B7C-2F00C9249F93");
    static public IMetadataDispenserEx dispenser = new MetaDataDispenserEx ();
    
    private string name;
    private string pubKey;
    private string shortPubKey;
    private ASSEMBLYMETADATA data;
    private bool retargetable = false;
    private string frameworkDir;
    private string assemblyDir;

    public Assembly (string assemblyName, string frameworkDir, string assemblyDir)
    {
      // resolved assembly path
      string assemblyPath;
      string name = assemblyName;
      //intermediate objects
      object rawMetaDataImportScope = null;
      object rawMetaDataAsmImportScope = null;

      // save framework and assembly directories for further use
      this.frameworkDir = frameworkDir;
      this.assemblyDir = assemblyDir;

      // First step: try to find the actual file

      if (System.IO.Path.GetExtension (name) != ".dll")
        name += ".dll";

      if (System.IO.File.Exists (System.IO.Path.Combine (assemblyDir, name)))
        assemblyPath = System.IO.Path.Combine (assemblyDir, name);
      else if (System.IO.File.Exists (System.IO.Path.Combine (frameworkDir, name)))
        assemblyPath = System.IO.Path.Combine (frameworkDir, name);
      else
        assemblyPath = System.IO.Path.Combine (".", name);
      // ??? At this point, we should try the GAC instead ???

      // open the assembly to retrieve the two COM interfaces to access data
      try
      {
        dispenser.OpenScope
          (assemblyPath, 0x10, ref gMetaDataImportGuid, out rawMetaDataImportScope);
        dispenser.OpenScope
          (assemblyPath, 0x10, ref gMetaDataAsmImportGuid, out rawMetaDataAsmImportScope);
      }
      catch
      {
        throw new System.ArgumentException ("Cannot open " + assemblyPath + " or not a valid assembly.");
      }

      // let's convert the intermediate objects into the expected COM interfaces
      this.importIntf = (IMetaDataImport2)rawMetaDataImportScope;
      this.asmImportIntf = (IMetaDataAssemblyImport)rawMetaDataAsmImportScope;

      // now that we have the COM interfaces OK, we can retrieve the assembly
      // information
      this.InitInformation();
    }

    public List<Type> GetTypes ()
    {
      IntPtr hHandle = IntPtr.Zero;
      int countTokens = 0;
      uint[] tokenRef = new uint[1];
      List<Type> ret = new List<Type> ();

      this.importIntf.EnumTypeDefs (ref hHandle, tokenRef, 1, out countTokens);
      while (countTokens > 0)
      {
        // create the corresponding Assembly.Type object
        try
        { Type auxt = Type.Get (tokenRef[0], null);
          if (auxt != null)
             ret.Add (Type.Get (tokenRef[0], null));
        }
        catch { }
        this.importIntf.EnumTypeDefs
          (ref hHandle, tokenRef, 1, out countTokens);
      }
      this.importIntf.CloseEnum (hHandle);

      ret.Sort (new TypeComparer ());

      return ret;
    }

    public void GetAsmRefs (Dictionary<String,Assembly> list)
    {
      uint hHandle = 0;
      uint countTokens = 0;
      uint[] tokenRef = new uint[1];

      this.asmImportIntf.EnumAssemblyRefs
        (ref hHandle, tokenRef, 1, out countTokens);
      while (countTokens > 0)
      {
        IntPtr pubkey, hashValue;
        char[] name = new char[1024];
        uint pubkeySize, nameSize, hashValueSize, flags;
        ASSEMBLYMETADATA data;
        string asmName;

        this.asmImportIntf.GetAssemblyRefProps
          (tokenRef[0], out pubkey, out pubkeySize, 
           name, 1024, out nameSize, out data, 
           out hashValue, out hashValueSize, out flags);

        // compute name
        asmName = Decoder.ReadString (name, (int)nameSize);
        if (!list.ContainsKey (asmName))
        {
          try
          {
            Assembly imported = new Assembly (asmName, this.frameworkDir, this.assemblyDir);
            list.Add (asmName, imported);
            imported.GetAsmRefs (list);
          }
          catch
          {
            System.Console.WriteLine ("IMPORT FAILED for " + asmName);
            System.Console.WriteLine (" searched in " + this.assemblyDir);
            System.Console.WriteLine (" searched in " + this.frameworkDir);
          }
        }
        this.asmImportIntf.EnumAssemblyRefs
          (ref hHandle, tokenRef, 1, out countTokens);
      }
      this.asmImportIntf.CloseEnum (hHandle);
    }

    public string Name { get { return this.name; } }

    public string AssemblyVersionString
    {
      get
      {
        string ret = string.Format
          (".ver {0}:{1}:{2}:{3} .publickeytoken=( {4})",
           this.data.usMajorVersion, this.data.usMinorVersion,
           this.data.usBuildNumber, this.data.usRevisionNumber,
           this.pubKey);
      
        if (this.retargetable) ret += " retargetable";
        return ret;
      }
    }

    private void InitInformation ()
    {
      uint myself;

      // retrieve my token
      this.asmImportIntf.GetAssemblyFromScope (out myself);

      IntPtr pubkey;
      uint pubkeySize, hashAlg, nameSize;
      char[] name = new char[1024];
      uint flags;

      this.asmImportIntf.GetAssemblyProps
        (myself, out pubkey, out pubkeySize, out hashAlg,
         name, 1024, out nameSize,
         out this.data, out flags);

      // init assembly name
      this.name = Decoder.ReadString (name, (int)nameSize);

      // retrieve the public key token from the public key
      byte[] orig = new byte[pubkeySize];
      IntPtr tokenPtr;
      int tokenPtrSize;

      if (pubkeySize > 0)
      {
        // copy from unmanaged memory to a local buffer
        System.Runtime.InteropServices.Marshal.Copy 
          (pubkey, orig, 0, (int)pubkeySize);

        // and retrieve the PublicKeyToken from the PublicKey
        bool ret = StrongNameTokenFromPublicKey 
          (orig, (int)pubkeySize, out tokenPtr, out tokenPtrSize);

        // construct the string for "pragma Import"
        this.pubKey = "";
        for (int j = 0; j < tokenPtrSize; j++)
        {
          string b = Decoder.ReadByte (tokenPtr, j).ToString ("x");
          if (b.Length == 1)
            this.pubKey += "0";
          this.pubKey += b + " ";
        }
        // and construct the string for display
        this.shortPubKey = "";
        for (int j = 0; j < tokenPtrSize; j++)
        {
          string b = Decoder.ReadByte (tokenPtr, j).ToString ("x");
          if (b.Length == 1)
            this.shortPubKey += "0";
          this.shortPubKey += b;
        }
      }
      else
      {
        this.shortPubKey = "00 00 00 00 00 00 00 00";
      }

      this.retargetable = 
        ((CorAssemblyFlags)flags & CorAssemblyFlags.afRetargetable) != 0;
    }

    [DllImport ("mscoree.dll")]
    private extern static bool StrongNameTokenFromPublicKey 
      ([In, MarshalAs (UnmanagedType.LPArray, SizeParamIndex = 1)]byte[] pbPublicKeyBlob,
       int cbPublicKeyBlob,
       [Out]out IntPtr ppbStrongNameToken,
       [Out, MarshalAs (UnmanagedType.U4)]out int pcbStrongNameToken);
  }


}
