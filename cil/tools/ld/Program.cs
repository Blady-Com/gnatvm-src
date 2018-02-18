using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;

using Microsoft.Win32;

namespace dotnet_ld
{
  class Program
  {
    static private Boolean verbose = false;
    static private Boolean veryVerbose = false;
    static private string outputFileName = null;
    static private List<String> linkerOptions = new List<String> ();
    static private List<String> linkerObjects = new List<String> ();

    class OutputReader
    {
      public string output = "";
      public void outputHandler (object sendingProcess, DataReceivedEventArgs outline)
      {
        if (!String.IsNullOrEmpty(outline.Data))
        {
          this.output += outline.Data + Environment.NewLine;
        }
      }
    }

    static void processArgs (string[] args)
    {
      // First check for --version or --help
      for (int j = 0; j < args.Length; j++)
      {
        if (args[j] == "--version")
        {
          Console.WriteLine ("DOTNET-LD " + vsn.Version_String);
          Console.WriteLine ("Copyright (C) 2010-" + vsn.Current_Year + ", AdaCore");
          Console.WriteLine
              ("This is free software; see the source for copying conditions.");
          Console.WriteLine
            ("See your AdaCore support agreement for details of warranty and support.");
          Console.WriteLine
            ("If you do not have a current support agreement, then there is absolutely");
          Console.WriteLine
            ("no warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR");
          Console.WriteLine ("PURPOSE.");
          Console.WriteLine ();
          Environment.Exit (0);
        }
        else if (args[j] == "--help")
        {
          Console.WriteLine ("Usage: dotnet-ld switches mainprog.il [objects.il]");
          Console.WriteLine ();
          Console.WriteLine ("  mainprog.il   the object file of the main program");
          Console.WriteLine ();
          Console.WriteLine ("  -g    Link with debug information");
          Console.WriteLine ("  -v    verbose mode");
          Console.WriteLine ("  -v -v very verbose mode");
          Console.WriteLine ();
          Console.WriteLine ("  -o nam     Use 'nam' as the name of the executable");
          Console.WriteLine ();
          Console.WriteLine ("  [non-Ada-objects]  list of non Ada object files");
          Console.WriteLine ("  [linker-options]   other options for the linker");
          Console.WriteLine ();
          Console.WriteLine ("Report bugs to report@adacore.com");
          Environment.Exit (0);
        }
      }

      bool skip_next = false;

      for (int j = 0; j < args.Length; j++)
      {
        string arg = args[j];

        if (skip_next)
        {
          // This argument must not be parsed.
          skip_next = false;
        }
        else if (arg == "-g")
        {
          linkerOptions.Add ("/DEBUG");
        }
        else if (arg.StartsWith ("-L") || arg.StartsWith ("-l") || arg.StartsWith ("-O") ||
          arg.StartsWith ("-Wl") || arg.StartsWith ("-sh"))
        {
          // Nothing to do for those with .NET, the assemblies being fetched from the GAC
          continue;
        }
        else if (arg == "-v")
        {
          if (!verbose)
            verbose = true;
          else
            veryVerbose = true;
        }
        else if (arg == "-o")
        {
          if (j == args.Length - 1)
          {
            Console.Error.WriteLine ("Missing argument for -o");
            Environment.Exit (1);
          }

          skip_next = true;
          outputFileName = args[j + 1];
        }
        else if (arg.StartsWith ("-") || arg.StartsWith ("/"))
        {
          // Fallback: if the argument starts with a '-' but has not been analyzed above, then
          //  try to pass it underneath
          linkerOptions.Add (arg);
        }
        else if (arg.StartsWith ("@"))
        {
          try
          {
            // Response file handling
            using (StreamReader sr = new StreamReader (arg.Substring (1)))
            {
              String line;

              while ((line = sr.ReadLine ()) != null)
              {
                linkerObjects.Add (line);
              }
            }
          }
          catch (Exception e)
          {
            Console.Error.WriteLine ("Could not read " + arg.Substring (1));
            Console.Error.WriteLine (" underlying error: " + e.Message.ToString ());
            Environment.Exit (1);
          }
        }
        else
        {
          if (arg.EndsWith (".il"))
            linkerObjects.Add (arg);
          else
            linkerOptions.Add (arg);
        }
      }

      if (outputFileName == null)
      {
        Console.Error.WriteLine ("Error: missing output file name");
        Environment.Exit (1);
      }

      linkerOptions.Add ("/QUIET");
      linkerOptions.Add ("/OUTPUT=\"" + outputFileName + "\"");
    }

    static string getRegValue (string regKey, string regVal)
    {
      RegistryKey theKey;
      theKey = Registry.LocalMachine.OpenSubKey (regKey);
      if (theKey == null)
      {
        theKey = Registry.LocalMachine.OpenSubKey ("\\" + regKey);
      }

      if (theKey == null) {
        //  let's try with the 32-bit registry, if we're on a 64bit machine
        if (!regKey.Contains ("Wow6432Node"))
        {
          return getRegValue (regKey.Replace ("SOFTWARE", "SOFTWARE\\Wow6432Node"), regVal);
        }
        else
        {
          return null;
        }
      }

      string ret = (string)theKey.GetValue (regVal);
      theKey.Close ();

      return ret;
    }

    static string findIlasm ()
    {
      // First pass: try to find ilasm from PATH
      string searchPath = Environment.GetEnvironmentVariable ("PATH");
      string[] paths = searchPath.Split (System.IO.Path.PathSeparator);
      foreach (string path in paths) {
        string full = System.IO.Path.Combine (path, "ilasm.exe");
        if (File.Exists (full))
          return full;
        
        full = System.IO.Path.Combine (path, "ilasm");
        if (File.Exists (full))
          return full;
      }
      // ilasm not found using PATH. Let's have a look at the registry
      string dotnetPath = getRegValue ("SOFTWARE\\Microsoft\\.NETFramework", "InstallRoot");
      if (dotnetPath != null) {
        dotnetPath = System.IO.Path.Combine (System.IO.Path.Combine (dotnetPath, "v4.0.30319"), "ilasm.exe");
        if (File.Exists (dotnetPath)) return dotnetPath;
        dotnetPath = System.IO.Path.Combine (System.IO.Path.Combine (dotnetPath, "v2.0.50727"), "ilasm.exe");
        if (File.Exists (dotnetPath)) return dotnetPath;
      }
      //  at this point, there is not much we can do. Let's exit with a proper error message.
      Console.Error.WriteLine ("Error: could not find ilasm.");
      Console.Error.WriteLine ("Please verify that the .NET framework is installed on your machine.");
      Environment.Exit (1);
      return null;
    }

    static void Main (string[] args)
    {
      processArgs (args);
      if (veryVerbose)
      {
        Console.WriteLine ("   Parsed command line:");
        foreach (string option in linkerOptions)
        {
          Console.WriteLine ("   - " + option);
        }
        Console.WriteLine ("   Objects to link:");
        foreach (string obj in linkerObjects)
        {
          Console.WriteLine ("   - " + obj);
        }
      }

      // Find ilasm
      String ilasm = findIlasm ();

      // Construct the command line
      String optionString = "";
      String objsString = "";
      String responseFile = null;

      foreach (string option in linkerOptions)
        optionString += option + " ";
      foreach (string obj in linkerObjects)
        objsString += "\"" + obj + "\" ";

      if (ilasm.Length + optionString.Length + objsString.Length > 2080)
      {
        // The command line is too large. Let's concatenate the object files.
        // Note that we need to make sure that this response file needs to be created
        //  in the current directory, so that external resources are correctly fetched
        //  by ilasm.
        do
        {
          responseFile = System.IO.Path.Combine (System.Environment.CurrentDirectory,
                                                 System.IO.Path.GetRandomFileName ());
          // Make sure that the extension has no 'meaning' for ilasm, such as .com or whatever
          // windows meaningful extensions.
          responseFile = System.IO.Path.ChangeExtension (responseFile, ".tmp");
        } while (File.Exists (responseFile));

        using (StreamWriter sw = new StreamWriter (responseFile))
        {
          foreach (string obj in linkerObjects)
          {
            using (StreamReader sr = new StreamReader (obj))
            {
              String line;
              while ((line = sr.ReadLine ()) != null)
                sw.WriteLine (line);
            }
          }
        }

        objsString = "\"" + responseFile + "\"";
      }

      if (verbose)
      {
        Console.WriteLine ("Current directory: " + Environment.CurrentDirectory);
        Console.WriteLine (ilasm + " " + optionString + objsString);
      }

      ProcessStartInfo startInfo = new ProcessStartInfo (ilasm, optionString + objsString);
      startInfo.CreateNoWindow = true;
      startInfo.ErrorDialog = false;
      startInfo.LoadUserProfile = false;
      startInfo.UseShellExecute = false;
      startInfo.WorkingDirectory = Environment.CurrentDirectory;
      startInfo.RedirectStandardError = true;
      startInfo.RedirectStandardOutput = true;

      Process proc = new Process ();
      OutputReader output = new OutputReader ();
      proc.StartInfo = startInfo;

      proc.OutputDataReceived += new DataReceivedEventHandler (output.outputHandler);
      proc.ErrorDataReceived += new DataReceivedEventHandler (output.outputHandler);

      proc.Start ();
      proc.BeginOutputReadLine ();
      proc.BeginErrorReadLine ();
      proc.WaitForExit ();

      if (responseFile != null)
        File.Delete (responseFile);

      if (proc.ExitCode != 0)
      {
        Console.Error.WriteLine ("error when calling ilasm");
        Console.Error.WriteLine (output.output);
        Environment.Exit (1);
      }
    }
  }
}
