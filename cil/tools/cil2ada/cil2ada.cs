//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                              c i l 2 a d a                               //
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

using Microsoft.Win32;

using cil2ada.AssemblyParser;

namespace cil2ada
{
  class cil2ada
  {
    static void About ()
    {
      Console.WriteLine ("");
      Console.WriteLine ("CIL2ADA " + vsn.Version_String);
      Console.WriteLine
        ("Copyright (C) 2006-" + vsn.Current_Year + ", AdaCore");
      Console.WriteLine
        ("This is free software; see the source for copying conditions.");
      Console.WriteLine
        ("See your AdaCore support agreement for details of warranty and support.");
      Console.WriteLine
        ("If you do not have a current support agreement, then there is absolutely");
      Console.WriteLine
        ("no warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR");
      Console.WriteLine ("PURPOSE.");
    }

    static void Print_Usage()
    {
      Console.WriteLine ("Usage:");
      Console.WriteLine (" cil2ada [-compact|-framework path] [-o dir] [-r] [-q] assembly");
      Console.WriteLine (" cil2ada -V");
      Console.WriteLine (" cil2ada [-h]");
      Console.WriteLine ("");
      Console.WriteLine ("Where assembly should be an assembly file or an assembly name");
      Console.WriteLine (" (e.g. \"System.Drawing\" or \"mscorlib\")");
      Console.WriteLine ("If no path is given, then the assembly will be searched for in the");
      Console.WriteLine (" .NET framework repository");
      Console.WriteLine ("");

      Console.WriteLine ("options:");
      Console.WriteLine ("  -compact     Search assemblies from the .NET compact framework 2.0 repository");
      Console.WriteLine ("  -framework   Specify the .NET framework main repository");
      Console.WriteLine ("  -h           Print this message");
      Console.WriteLine ("  -o           Create files in the specified directory");
      Console.WriteLine ("  -q -quiet    Quiet mode");
      Console.WriteLine ("  -r           Analyse the assembly and its referenced assemblies");
      Console.WriteLine ("  -V --version Print the current version");
      Console.WriteLine ("");
    }

    static string Get_Path(string regkey, string regvalue)
    {
      RegistryKey TheKey;
      String str;

      TheKey =
        Registry.LocalMachine.OpenSubKey(regkey);

      if (TheKey == null)
        TheKey =
          Registry.LocalMachine.OpenSubKey("\\" + regkey);

      if (TheKey == null)
        if (regkey.Contains("SOFTWARE") && !regkey.Contains("Wow6432Node"))
        {
          str = regkey.Replace("SOFTWARE", "SOFTWARE\\Wow6432Node");
          return Get_Path(str, regvalue);
        }
        else
        {
          return "";
        }

      str = (string)TheKey.GetValue(regvalue);
      TheKey.Close();

      return str;
    }

    //////////
    // MAIN //
    //////////

    [STAThread]
    static void Main(string[] args)
    {
      bool quiet = false;
      string assemblyName;
      string frameworkDir = null;
      string assemblyDir = "";

      bool option_recurse = false;
      bool option_compact = false;
      string option_dir = "";
      string option_name = "";
      bool next_is_dir = false;
      bool next_is_framework_dir = false;
      string compact_target = "WindowsCE";

      foreach (string arg in args)
      {
        if (next_is_dir)
        {
          option_dir = arg;
          next_is_dir = false;
          continue;
        }
        else if (next_is_framework_dir)
        {
          frameworkDir = arg;
          next_is_framework_dir = false;
          continue;
        }

        switch (arg)
        {
          case "-compact":
            if (frameworkDir != null)
            {
              Console.WriteLine ("Cannot use both -framework and -compact options");
              return;
            }
            option_compact = true;
            break;

          case "-framework":
            if (option_compact)
            {
              Console.WriteLine ("Cannot use both -framework and -compact options");
              return;
            }
            next_is_framework_dir = true;
            break;

          case "-o":
            next_is_dir = true;
            break;

          case "-q":
          case "-quiet":
            quiet = true;
            break;

          case "-r":
            option_recurse = true;
            break;

          case "-V":
          case "--version":
            About ();
            return;

          case "-h":
          case "--help":
          case "/?":
            Print_Usage ();
            return;

          default:
            // Detect incorrect switches
            if (arg.StartsWith("-"))
            {
              Print_Usage ();
              return;
            }
            option_name = arg;
            break;
        }
      }
      if (option_name == "")
      {
        Print_Usage ();
        return;
      }
      else if (option_name == "compact_mscorlib")
      {
        assemblyName = "mscorlib.dll";
        option_compact = true;
      }
      else
      {
        if (System.IO.Path.GetExtension (option_name) != ".dll")
          option_name += ".dll";
        if (System.IO.File.Exists (option_name))
        {
          string full = System.IO.Path.GetFullPath (option_name);
          assemblyDir = System.IO.Path.GetDirectoryName (full);
          assemblyName = System.IO.Path.GetFileName (full);
        }
        else
        {
          assemblyName = option_name;
        }
      }

      if (frameworkDir == null)
      {
        if (option_compact)
        {
          frameworkDir = Get_Path
            ("SOFTWARE\\Microsoft\\.NETCompactFramework\\v2.0.0.0\\" +
             compact_target +
             "\\AssemblyFoldersEx", "");
          if (frameworkDir == "")
          {
            Console.WriteLine
              ("* ERROR: cil2ada could not locate the compact framework.");
            Console.WriteLine
              ("   Please make sure to install the compact framework before calling");
            Console.WriteLine
              ("   cil2ada with -compact or mscorlib_compact options.");
            return;
          }
        }
        else
        {
          //  get the full framework directory
          frameworkDir = Get_Path ("SOFTWARE\\Microsoft\\.NETFramework", "InstallRoot");
          if (frameworkDir == "")
          {
            Console.WriteLine ("* ERROR: cil2ada could not locate the .NET framework.");
            Console.WriteLine ("   Please make sure that the .NET framework v2.0 is installed.");
            return;
          }
          frameworkDir += "v2.0.50727\\";
        }
      }
      Assembly asm;
      List<Assembly> asmList = new List<Assembly> ();
      Dictionary<String,Assembly> refsList = new Dictionary<String,Assembly> ();
      try
      {
        asm = new Assembly(assemblyName, frameworkDir, assemblyDir);
        // retrieve the list in all cases (even in non recursive analysis mode)
        // so that a call to OpenScope is performed on all assembly refs.
        // This allows further translations from TypeRefs tokens to TypeDefs
        // tokens.
        asm.GetAsmRefs(refsList);
      }
      catch (System.Exception e)
      {
        System.Console.WriteLine(assemblyName + " is not a valid CIL assembly.");
        System.Console.WriteLine(e.Message);
        return;
      }
      asmList.Add (asm);
      if (option_recurse)
        foreach (KeyValuePair<String,Assembly> kvp in refsList)
          asmList.Add (kvp.Value);

      foreach (Assembly a in asmList)
      {
        Console.WriteLine ("Parsing assembly " + a.Name);
        Context.Init (a);
        foreach (AssemblyParser.Type type in a.GetTypes())
        {
          if (!type.IsPublic &&
              !type.IsNestedPublic &&
              !type.IsNestedFamily)
            continue;
          if (type.IsNested && !type.GetOuterType.IsPublic) continue;
          type.DumpToAda(quiet, option_dir);
        }
      }

    } // end Main
  } // end class cil2ada
} // end namespace cil2ada
