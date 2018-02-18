//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                     cil2ada.AssemblyParser.CustomMod                     //
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
using System.Text;
using cil2ada.Interfaces;

namespace cil2ada.AssemblyParser
{
  ///////////////
  // CustomMod //
  ///////////////

  class CustomMod
  {
    private bool isRequired = false;
    private Type token = null;
    // private int value;

    static public bool IsCustomMod(IntPtr sig, int idx)
    {
      CorElementType b = (CorElementType)Decoder.ReadByte(sig, idx);
      if ((b == CorElementType.CMOD_OPT) || (b == CorElementType.CMOD_REQD))
        return true;
      return false;
    }

    static public List<CustomMod> ReadCustomMods(IntPtr sig, ref int idx)
    {
      List<CustomMod> customMods = null;

      while (CustomMod.IsCustomMod(sig, idx))
      {
        if (customMods == null)
          customMods = new List<CustomMod>();

        customMods.Add(new CustomMod(sig, ref idx));
      }

      return customMods;
    }

    public CustomMod(IntPtr sig, ref int idx)
    {
      CorElementType b = (CorElementType)Decoder.ReadValue(sig, ref idx);
      if (b == CorElementType.CMOD_OPT)
        this.isRequired = false;
      else if (b == CorElementType.CMOD_REQD)
        this.isRequired = true;
      else
        throw new System.ArgumentException
          ("Error while decoding CustomMod: unexpected value in signature");

      this.token = Type.Get(Decoder.ReadTypeDefOrRefEncoded(sig, ref idx), null);
    }

    public string Image()
    {
      string mod;
      if (this.isRequired)
        mod = "modreq";
      else
        mod = "modopt";
      return mod + "(" + this.token.FullName + ")";
    }
  }
}
