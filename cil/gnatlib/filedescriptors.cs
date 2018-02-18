//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                       f i l e d e s c r i p t o r                        //
//                                                                          //
//                     Copyright (C) 2008-2009, AdaCore                     //
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

using System;
using System.Collections;
using System.IO;

namespace mgnat.adalib {

public partial class GNAT_libc {

#if !COMPACT
   static ArrayList files = new ArrayList();

   private class myFile {
      private System.IO.FileStream stream;
      private System.IO.TextReader reader;
      private System.IO.TextWriter writer;
      public int fd;

      public myFile (Byte[] name, FileMode mode, FileAccess access, bool BinaryMode)
      {
         string Filename = to_string (name);
         this.stream = null;
         try {
           this.stream = new FileStream (Filename, mode, access);
           if ((access == FileAccess.Read) || (access == FileAccess.ReadWrite))
              this.reader = new StreamReader (this.stream);
           if ((access == FileAccess.Write) || (access == FileAccess.ReadWrite))
              this.writer = new StreamWriter (this.stream);
           files.Add (this);
           this.fd = files.IndexOf (this) + 3;
         } catch {
           this.fd = -1;
         }
      }

      public static myFile get (int fd)
      {
         try {
           myFile file = (myFile)files[fd - 3];
           return file;
         } catch {
           return null;
         }
      }

      public void Close()
      {
         if (this.writer != null) this.writer.Close();
         else if (this.reader != null) this.reader.Close();
         else if (this.stream != null) this.stream.Close();
         if (this.fd != 0) files [this.fd - 3] = null;
         this.writer = null;
         this.reader = null;
         this.stream = null;
         this.fd = 0;
      }

      public FileStream Stream { get { return this.stream; } }
      public TextReader Reader { get { return this.reader; } }
      public TextWriter Writer { get { return this.writer; } }
   }

   private static FileStream getStream (int fd)
   {
     if (fd < 3)
        return null;
     else {
        myFile file = myFile.get (fd);
        if (file != null)
          return file.Stream;
        else
          return null;
     }
   }

   private static TextWriter getWriter (int fd)
   {
     if (fd < 0)
        return null;
     else if (fd == 0)
        return null;
     else if (fd == 1)
        return Console.Out;
     else if (fd == 2)
        return Console.Error;
     else {
        myFile file = myFile.get (fd);
        if (file != null)
          return file.Writer;
        else
          return null;
     }
   }
#endif

   public static int __gnat_open_read (Byte[] name, int mode) {
#if !COMPACT
     myFile file = new myFile (name, FileMode.Open, FileAccess.Read, mode == 0);
     if (file.fd > 0) return file.fd;
#endif
      return -1;
   }

   public static int __gnat_open_rw (Byte[] name, int mode) {
#if !COMPACT
     myFile file = new myFile (name, FileMode.OpenOrCreate, FileAccess.ReadWrite, mode == 0);
     if (file.fd > 0) return file.fd;
#endif
      return -1;
   }

   public static int __gnat_open_create (Byte[] name, int mode) {
#if !COMPACT
     myFile file = new myFile (name, FileMode.Create, FileAccess.Write, mode == 0);
     if (file.fd > 0) return file.fd;
#endif
      return -1;
   }

   public static int __gnat_open_new (Byte[] name, int mode) {
#if !COMPACT
     myFile file = new myFile (name, FileMode.CreateNew, FileAccess.Write, mode == 0);
     if (file.fd > 0) return file.fd;
#endif
      return -1;
   }

   public static int __gnat_create_output_file (Byte[] name) {
#if !COMPACT
     myFile file = new myFile (name, FileMode.CreateNew, FileAccess.Write, false);
     if (file.fd > 0) return file.fd;
#endif
      return -1;
   }

   public static int __gnat_open_new_temp (System.Object name, int mode) {
     // ??? not implemented
     return -1;
   }

   public static void __gnat_lseek (int fd, Int64 offset, int origin) {
#if !COMPACT
     Stream file = getStream (fd);
     if (file == null) return;
     System.IO.SeekOrigin seekorigin;

     if (origin == 0)
       seekorigin = System.IO.SeekOrigin.Begin;
     else if (origin == 1)
       seekorigin = System.IO.SeekOrigin.Current;
     else
       seekorigin = System.IO.SeekOrigin.End;

     try {
       file.Seek ((long)offset, seekorigin);
     } catch {
     }
#endif
   }

   public static int read (int fd, System.Object buffer, uint len) {
#if !COMPACT
     Stream file = getStream (fd);
     if (file == null) return 0;

     try {
       return file.Read ((byte[])buffer, 0, (int)len);
     } catch {
       return 0;
     }
#else
      return 0;
#endif
   }

   public static int write (int fd, System.Object buffer, uint len) {
#if !COMPACT
     Stream file = getStream (fd);
     if (file == null) return 0;

     try {
       file.Write ((byte[])buffer, 0, (int)len);
       return (int)len;
     } catch {
       return 0;
     }
#else
      return 0;
#endif
   }

   public static int close (int fd) {
#if !COMPACT
     myFile file = myFile.get (fd);
     if (file == null) return -1;

     file.Close();
     return 0;
#else
      return -1;
#endif
   }

   public static bool __gnat_copy_file (int fdorig, int fddest) {
#if !COMPACT
     bool ret;
     Stream f_orig = getStream (fdorig);
     Stream f_dest = getStream (fddest);
     int nBytes;
     byte[] data = new byte[1024];

     if ((f_orig == null) || (f_dest == null)) return false;

     try {
       do {
         nBytes = f_orig.Read (data, 0, data.Length);
         if (nBytes > 0) f_dest.Write (data, 0, nBytes);
       } while (nBytes > 0);

       ret = true;
     } catch {
       ret = false;
     }

     close (fdorig);
     close (fddest);
     return ret;
#else
     return false;
#endif
   }

   public static Int32 __gnat_file_time_fd (int fd) {
#if !COMPACT
     FileStream file = getStream (fd);
     if (file == null) return -1;

     return secsFromTicks (System.IO.File.GetLastWriteTime (file.Name).Ticks);
#else
     return -1;
#endif
   }

   public static int __gnat_set_close_on_exec (int fd, int close_on_exec) {
     //  ??? Not implemented
     return -1;
   }

   public static bool __gnat_is_regular_file (Byte[] name) {
     string filename = to_string (name);
     return System.IO.File.Exists (filename);
   }

   public static bool __gnat_is_directory (Byte[] name) {
     string filename = to_string (name);
     return System.IO.Directory.Exists (filename);
   }

   public static Byte[] __gnat_locate_exec_on_path (Byte[] name) {
     string filename = to_string (name);

#if !MONO
     // first we verify the extension
     string extension = System.IO.Path.GetExtension (filename);
     if (extension == String.Empty)
       filename = System.IO.Path.ChangeExtension (filename, ".exe");
#endif

#if !COMPACT
     string paths = System.Environment.GetEnvironmentVariable ("PATH");
#else
     // Environment variables do not exist in compact framework. Let's look
     // in the current directory instead.
     string paths = System.IO.Directory.GetCurrentDirectory ();
#endif

     return internal_locate_regular_file (filename, paths);
   }

   public static Byte[] __gnat_locate_regular_file (Byte[] name, Byte[] paths) {
     return internal_locate_regular_file (to_string (name), to_string (paths));
   }

   private static Byte[] internal_locate_regular_file (string name, string paths) {
     //  We verify if the path is absolute
     if (System.IO.Path.IsPathRooted (name)) {
       if (System.IO.File.Exists (name))
         return getBytes (name);
       else
         return new byte[0];
     }

     // Is the file in the current directory ?
     if (System.IO.File.Exists (name)) {
       return getBytes
         (System.IO.Path.GetFullPath
            (System.IO.Path.Combine (System.IO.Directory.GetCurrentDirectory(),
                                     name)));
     }
     // Look through furnished paths list.

     if (paths == null) return new byte[0];

     string[] thePaths = null;

     // Handle both native or unix style paths
     if (paths.IndexOf (System.IO.Path.PathSeparator) >= 0)
        thePaths = paths.Split (new char[]{System.IO.Path.PathSeparator});
     else
        thePaths = paths.Split (new char[]{':'});

     foreach (string p in thePaths) {
       string newName = System.IO.Path.GetFullPath
                          (System.IO.Path.Combine (p, name));

       if (System.IO.File.Exists (newName)) return getBytes (newName);
     }

     // We did not find the file, let's return an empty string
     return new byte[0];
   }

   public static bool __gnat_rename_file (byte[] oldf, byte[] newf) {
     try {
       System.IO.File.Move (to_string (oldf), to_string (newf));
       return true;
     } catch {
       return false;
     }
   }

   public static bool __gnat_delete_file (Byte[] name) {
     string filename = to_string (name);
     if (!System.IO.File.Exists (filename)) return false;
     try {
       System.IO.File.Delete (filename);
       return true;
     } catch {
       return false;
     }
   }

   public static bool __gnat_is_absolute_path (byte [] name)
   {
      return System.IO.Path.IsPathRooted (to_string (name));
   }

   public static bool __gnat_is_symbolic_link (byte [] name)
   {
      // ??? not implemented
      return false;
   }

   public static bool __gnat_is_readable_file (byte [] name)
   {
      // ??? no easy way to determine this, so just return true if the file
      // exists.
      return __gnat_is_regular_file (name);
   }

   public static bool __gnat_is_executable_file (byte [] name)
   {
     //  ??? Not implemented
     return false;
   }

   public static bool __gnat_is_writable_file (byte [] name)
   {
#if !COMPACT
     string filename = to_string (name);
     System.IO.FileAttributes attr = System.IO.File.GetAttributes (filename);
     return (attr | System.IO.FileAttributes.ReadOnly)!=attr;
#else
     return true;
#endif
   }

   public static void __gnat_set_writable (Byte[] name) {
#if !COMPACT
     if (__gnat_is_writable_file (name)) return;
     string filename = to_string (name);
     System.IO.FileAttributes attrs = System.IO.File.GetAttributes (filename);
     attrs ^= System.IO.FileAttributes.ReadOnly;
     System.IO.File.SetAttributes (filename, attrs);
#endif
   }

   public static void __gnat_set_readonly (Byte[] name) {
#if !COMPACT
     string filename = to_string (name);
     System.IO.FileAttributes attrs = System.IO.File.GetAttributes (filename);
     attrs |= System.IO.FileAttributes.ReadOnly;
     System.IO.File.SetAttributes (filename, attrs);
#endif
   }

   public static void __gnat_set_readable (Byte[] name) {
     //  ??? Not implemented
   }

   public static void __gnat_set_non_readable (Byte[] name) {
     //  ??? Not implemented
   }

   public static void __gnat_set_executable (Byte[] name) {
     //  ??? Not implemented
   }

   public static int __gnat_copy_attribs  (Byte[] in_ada_name, Byte[] out_ada_name, int full) {
#if !COMPACT
     string in_name = to_string (in_ada_name);
     string out_name = to_string (out_ada_name);

     if ((!File.Exists (in_name)) || (!File.Exists (out_name))) return -1;
     try {
       File.SetLastWriteTime (out_name, File.GetLastWriteTime (in_name));
       File.SetLastAccessTime (out_name, File.GetLastAccessTime (in_name));
       File.SetCreationTime (out_name, File.GetCreationTime (in_name));
     } catch {
       return -1;
     }

     if (full == 1) {
       try {
         File.SetAttributes (out_name, File.GetAttributes (in_name));
         File.SetAccessControl (out_name, File.GetAccessControl (in_name));
       } catch {
         return -1;
       }
     }
#endif

     return 0;
   }

   public static Int32 __gnat_file_time_name (Byte[] name) {
     string filename = to_string (name);
     if (!__gnat_is_regular_file (name)) return -1;
     return (int)secsFromTicks (System.IO.File.GetLastWriteTime (filename).Ticks);
   }

}
}
