//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                       cil2ada.AssemblyParser.Token                       //
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
using System.Collections;

namespace cil2ada.AssemblyParser
{
  abstract class Token
  {
    protected uint token = 0;
    protected Token(uint token) { this.token = token; }

    private static SortedList withList =
       new SortedList();
    private static SortedList limitedWithList =
       new SortedList();
    private static String main = null;
    private static string constructorName = null;
    private static SortedList keywords = null;

    static public void Init(string pkg)
    {
      main = pkg;
      withList.Clear();
      limitedWithList.Clear();
      constructorName = null;
    }

    static private bool checkWith(string dep)
    {
      if (main == dep) return false;
      if (main.StartsWith(dep))
        if (main.Substring(dep.Length).StartsWith("."))
          return false;
      return true;
    }

    static public void AddLimitedWith(string dep)
    {
      if (!checkWith(dep)) return;
      if (withList.Contains(dep)) return;
      if (limitedWithList.Contains(dep)) return;
      limitedWithList.Add(dep, dep);
    }
    static public void AddLimitedWith(Type deptype)
    {
      string dep = deptype.FullPackageName;
      if (deptype.IsGeneric) return;
      if (dep == "CIL_Types" || dep == "MSSyst.Object")
        AddWith(deptype);
      else if (!deptype.IsSubclassOf(main))
        AddLimitedWith(dep);
    }

    static public void AddWith(String dep)
    {
      if (!checkWith(dep)) return;
      if (withList.Contains(dep)) return;
      if (limitedWithList.Contains(dep)) limitedWithList.Remove(dep);
      withList.Add(dep, dep);
    }


    static public void AddWith(Type deptype)
    {
      string dep = deptype.FullPackageName;
      if (!deptype.IsSubclassOf(main))
        AddWith(dep);
    }

    static public void SetHasConstructor(string constructor)
    {
      constructorName = constructor;
    }

    static public string GetWiths()
    {
      string ret = "";
      foreach (DictionaryEntry s in withList)
        ret += "with " + s.Value + ";\n";
      foreach (DictionaryEntry s in limitedWithList)
        ret += "limited with " + s.Value + ";\n";
      return ret;
    }

    static public string GetPrivate(uint indent)
    {
      string ret = "";

      if (constructorName != null)
        ret += Indent(indent) +
          "pragma CIL_Constructor (" + constructorName + ");\n";

      return ret;
    }

    static public string Indent(uint indent)
    {
      char[] ret = new char[3 * indent];
      for (int i = 0; i < ret.Length; i++) ret[i] = ' ';
      return new string(ret);
    }

    static private void AddKeyword(string name)
    {
      keywords.Add(name.GetHashCode(), name);
    }

    static public string CheckAdaKeywords(string unitName)
    {
      if (keywords == null)
      {
        keywords = new SortedList();
        AddKeyword("abort");
        AddKeyword("abs");
        AddKeyword("abstract");
        AddKeyword("accept");
        AddKeyword("access");
        AddKeyword("aliased");
        AddKeyword("all");
        AddKeyword("and");
        AddKeyword("array");
        AddKeyword("at");
        AddKeyword("begin");
        AddKeyword("body");
        AddKeyword("case");
        AddKeyword("constant");
        AddKeyword("declare");
        AddKeyword("delay");
        AddKeyword("delta");
        AddKeyword("digits");
        AddKeyword("do");
        AddKeyword("else");
        AddKeyword("elsif");
        AddKeyword("end");
        AddKeyword("entry");
        AddKeyword("exception");
        AddKeyword("exit");
        AddKeyword("for");
        AddKeyword("function");
        AddKeyword("generic");
        AddKeyword("goto");
        AddKeyword("if");
        AddKeyword("in");
        AddKeyword("interface");
        AddKeyword("is");
        AddKeyword("limited");
        AddKeyword("loop");
        AddKeyword("mod");
        AddKeyword("new");
        AddKeyword("not");
        AddKeyword("null");
        AddKeyword("of");
        AddKeyword("or");
        AddKeyword("others");
        AddKeyword("out");
        AddKeyword("overriding");
        AddKeyword("package");
        AddKeyword("pragma");
        AddKeyword("private");
        AddKeyword("procedure");
        AddKeyword("protected");
        AddKeyword("raise");
        AddKeyword("range");
        AddKeyword("record");
        AddKeyword("rem");
        AddKeyword("renames");
        AddKeyword("requeue");
        AddKeyword("return");
        AddKeyword("reverse");
        AddKeyword("select");
        AddKeyword("separate");
        AddKeyword("subtype");
        AddKeyword("synchronized");
        AddKeyword("tagged");
        AddKeyword("task");
        AddKeyword("terminate");
        AddKeyword("then");
        AddKeyword("type");
        AddKeyword("until");
        AddKeyword("use");
        AddKeyword("when");
        AddKeyword("while");
        AddKeyword("with");
        AddKeyword("xor");
        AddKeyword("duration");
        AddKeyword("integer");
        AddKeyword("standard");
        AddKeyword("float");
      }
      string[] words = unitName.Split('.');
      string ret = "";
      foreach (string w in words)
      {
          string name = w;
          if (ret != "") ret += ".";
          if (name.StartsWith("_"))
              name = "u" + name.Substring(1);
          ret += name.Replace("__", "_u");
          if (keywords.Contains(name.ToLower().GetHashCode()))
              ret += "_k";
      }
      ret = ret.Replace('`', '_');
      return ret;
  }

  }
}
