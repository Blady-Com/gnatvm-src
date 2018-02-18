//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                             P r o c e s s e s                            //
//                                                                          //
//                     Copyright (C) 2008, AdaCore                          //
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
//////////////////////////////////////////////////////////////////////////////

#if !SILVERLIGHT
using System;
using System.Collections;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;

namespace mgnat.adalib {

public partial class GNAT_libc {

   public static int __gnat_argument_needs_quote = 1;

   private static string to_string (mgnat.adalib.standard.ada_string[] Args) {
      string theArgs = "";

      for (int j=0; j < Args.Length; j++) {
        if (j != 0) theArgs += " ";
        theArgs += to_string (Args[j].all);
      }

      return theArgs;
   }

#if !COMPACT
   private class ProcessDictionary : DictionaryBase {
      public MyProcess this [int key] {
        get  {
          return ((MyProcess) Dictionary[key]);
        }
        set  {
          Dictionary [key] = value;
        }
      }

      public ICollection Keys  {
        get  {
          return (Dictionary.Keys);
        }
      }

      public ICollection Values  {
        get  {
          return (Dictionary.Values);
        }
      }

      public void Add (int key, MyProcess value)  {
        Dictionary.Add (key, value);
      }

      public bool Contains (int key)  {
        return (Dictionary.Contains (key));
      }

      public void Remove (int key)  {
        Dictionary.Remove (key);
      }
   }

   private static ProcessDictionary Processes = null;
#endif

   private class MyProcess : System.Diagnostics.Process {
     System.IO.TextWriter outstream;
     bool stderr_to_stdout;

     public MyProcess():base()
     {
       this.outstream = null;
       this.stderr_to_stdout = false;
       this.StartInfo.UseShellExecute = false;
#if !COMPACT
       this.StartInfo.RedirectStandardInput = true;
       this.StartInfo.RedirectStandardOutput = true;
       this.StartInfo.RedirectStandardError = true;
       this.StartInfo.CreateNoWindow = true;
       this.OutputDataReceived += new DataReceivedEventHandler (this.ReadStdOut);
       this.ErrorDataReceived += new DataReceivedEventHandler (this.ReadStdErr);
#endif
     }

     public void RedirectOutput (System.IO.TextWriter stream, bool stderr_to_out)
     {
       this.outstream = stream;
       this.stderr_to_stdout = stderr_to_out;
     }

     public int Spawn (Byte[] filename, mgnat.adalib.standard.ada_string[] Args, bool insertInList)
     {
       string theArgs = to_string (Args);

       this.StartInfo.FileName = to_string (filename);
       this.StartInfo.Arguments = theArgs;

       try {
         if (this.Start()) {
#if !COMPACT
           if (insertInList) {
             if (Processes == null) Processes = new ProcessDictionary();
             Processes.Add (this.Id, this);
           }

           this.BeginOutputReadLine();
           this.BeginErrorReadLine();
#endif

           return this.Id;
         } else {
           return -1;
         }
       } catch {
         return -1;
       }
     }
#if !COMPACT
     private void ReadStdOut(object sendingProcess, DataReceivedEventArgs outLine)
     {
       if (!String.IsNullOrEmpty(outLine.Data)) {
          if (this.outstream != null) {
             this.outstream.WriteLine (outLine.Data);
          } else
             Console.Out.WriteLine (outLine.Data);
       }
     }

     private void ReadStdErr(object sendingProcess, DataReceivedEventArgs outLine)
     {
       if (!String.IsNullOrEmpty(outLine.Data)) {
         if (this.stderr_to_stdout)
           this.ReadStdOut (sendingProcess, outLine);
         else
           Console.Error.Write (outLine.Data);
       }
     }
#endif
   }

   public static int __gnat_portable_spawn (Byte[] filename, mgnat.adalib.standard.ada_string[] Args, int fd, bool stderr_to_out) {
      MyProcess process = new MyProcess();
#if !COMPACT
      System.IO.TextWriter stream = getWriter (fd);
      if (stream != null) process.RedirectOutput (stream, stderr_to_out);
#endif
      int ret = process.Spawn (filename, Args, false);
      if (ret != -1) {
        int ExitCode = -1;
        process.WaitForExit();
        ExitCode = process.ExitCode;
        process.Close();
        return ExitCode;
      }
      process.Close();
      return ret;
   }

   public static int __gnat_portable_no_block_spawn (Byte[] filename, mgnat.adalib.standard.ada_string[] Args, int fd, bool stderr_to_out) {
      MyProcess process = new MyProcess();
#if !COMPACT
      System.IO.TextWriter stream = getWriter (fd);
      if (stream != null) process.RedirectOutput (stream, stderr_to_out);
#endif
      return process.Spawn (filename, Args, true);
   }

   public static int __gnat_portable_wait (Object Status) {
#if !COMPACT
      int nbDead = 0;
      int nbProc = 0;
#endif

      ((Int)Status).all = -1;

#if !COMPACT
      if (Processes == null) return -1;

      foreach (MyProcess proc in Processes.Values) {
        if (proc.HasExited) nbDead ++;
        nbProc ++;
      }

      // if no process exist in any of the lists, just return
      if (nbProc == 0) return -1;

      if (nbDead == 0) {
        // currently no dead process. Let's wait for one of the active
        // processes to terminate
        System.Threading.Mutex[] handles = new System.Threading.Mutex[nbProc];
        int j = 0;

        foreach (MyProcess proc in Processes.Values) {
          handles[j] = new System.Threading.Mutex(false);
          handles[j].SafeWaitHandle =
            new Microsoft.Win32.SafeHandles.SafeWaitHandle (proc.Handle, true);
          j++;
        }

        System.Threading.Mutex.WaitAny (handles);
      }

      foreach (MyProcess proc in Processes.Values) {
         if (proc.HasExited) {
           int pid;
           Processes.Remove (proc.Id);
          ((Int)Status).all = proc.ExitCode;
           pid = proc.Id;
           proc.Close();
           return pid;
         }
      }
#endif

      return -1;
   }

}
}
#endif
