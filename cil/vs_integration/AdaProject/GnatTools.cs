using System;
using System.Diagnostics;

namespace AdaCore.AdaPackage
{
    public static class Trace
    {
        public static void Write(string Str)
        {
            System.IO.File.AppendAllText(@"c:\tracevisualstudio.txt", Str);
        }
        public static void WriteLine(string Str)
        {
            Write(Str + "\r\n");
        }
    }

   public static class DefaultLocation
   {
      private static string adainclude;
      private static string adalib;
      private static Boolean initialized = false;

      private static void CheckInitialized()
      {
         if (initialized == true) return;

         ProcessStartInfo startInfo = new ProcessStartInfo("dotnet-gnatls.exe");
         startInfo.WindowStyle = ProcessWindowStyle.Hidden;
         startInfo.CreateNoWindow = true;
         startInfo.UseShellExecute = false;
         startInfo.RedirectStandardInput = true;
         startInfo.RedirectStandardOutput = true;
         startInfo.RedirectStandardError = true;
         startInfo.Arguments = "-v";

         Process Gnatls = Process.Start(startInfo);
         Gnatls.WaitForExit();
         string line;
         // Read and display lines from the file until the end of
         // the file is reached.
         while ((line = Gnatls.StandardOutput.ReadLine()) != null)
         {

            if (line == "Source Search Path:")
            {
               while ((line = Gnatls.StandardOutput.ReadLine()) != null)
               {
                  line = line.Trim();
                  // ??? We need to handle the case when multiple paths are defined
                  if ((line.Length > 0) && (line[0] != '<'))
                  {
                     adainclude = line;
                     break;
                  }
               }
            }
            if (line == "Object Search Path:")
            {
               while ((line = Gnatls.StandardOutput.ReadLine()) != null)
               {
                  line = line.Trim();
                  // ??? We need to handle the case when multiple paths are defined
                  if ((line.Length > 0) && (line[0] != '<'))
                  {
                     adalib = line;
                     break;
                  }
               }
            }
         }
         initialized = true;
      }

      public static string GetDefaultIncludePath()
      {
         CheckInitialized();
         return adainclude;
      }

      public static string GetDefaultLibPath()
      {
         CheckInitialized();
         return adalib;
      }

      private static string Compose(string rootDir, string name, string extension)
      {
         if (rootDir[rootDir.Length - 1] != '\\') return Compose(rootDir + '\\', name, extension);

         return rootDir + name + '.' + extension;
      }

      public static string Find_File(string name, string compilationFlags, string currentDir, bool findAliFile)
      {
         string extension;
         string fullname;

         if (!findAliFile)
            extension = "ads";
         else
            extension = "ali";


         // ??? should read all paths from project file...
         fullname = Compose(currentDir, name, extension);
         if (System.IO.File.Exists(fullname)) return fullname;

         fullname = Compose(currentDir + "\\obj\\debug", name, extension);
         if (System.IO.File.Exists(fullname)) return fullname;

         fullname = Compose(currentDir + "\\obj\\release", name, extension);
         if (System.IO.File.Exists(fullname)) return fullname;

         fullname = Compose(currentDir + "\\bin\\debug", name, extension);
         if (System.IO.File.Exists(fullname)) return fullname;

         fullname = Compose(currentDir + "\\bin\\release", name, extension);
         if (System.IO.File.Exists(fullname)) return fullname;

         CheckInitialized();
         if (!findAliFile)
            fullname = Compose(adainclude, name, extension);
         else
            fullname = Compose(adalib, name, extension);
         if (System.IO.File.Exists(fullname)) return fullname;

         return String.Empty;
      }
   }
}
