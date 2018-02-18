//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                      cil2ada.AssemblyParser.Context                      //
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

  static class Context
  {
    private static System.Collections.Generic.Stack<IMetaDataImport2> stack =
      new System.Collections.Generic.Stack<IMetaDataImport2> ();

    static public Assembly mainAsm = null;
    static public IMetaDataImport2 importIntf = null;

    public static uint GetContextToken ()
    {
      uint ret;
      importIntf.GetModuleFromScope (out ret);
      return ret;
    }

    public static void SetContext (IMetaDataImport2 intf)
    {
      stack.Push (importIntf);
      importIntf = intf;
    }

    public static void ReleaseContext ()
    {
      importIntf = stack.Pop ();
    }

    public static void Init (Assembly asm)
    {
      mainAsm = asm;
      importIntf = asm.importIntf;
    }
  }

}