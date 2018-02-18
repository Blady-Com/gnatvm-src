//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                            G N A T _ l i b c                             //
//                                                                          //
//                     Copyright (C) 1998-2012, AdaCore                     //
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
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

//  This C# file is part of the GNAT for .NET library.

//  The GNAT library sources have a number of "pragma Import (C, ..)" sprinkled
//  throughout. Some of these C routines are part of the C library, others are
//  part of the C sources of GNAT.  The purpose of this Java class is to give a
//  Java-compatible definition for all the C routines used in the GNAT sources
//  which are part of the JGNAT library.

//  The actual mapping between the "pragma Import (C, ...)" routines and the
//  methods in this class is performed by the JGNAT compiler:  whenever an Ada
//  subprogram, say <routine>, is imported from C, for instance:
//
//             pragma Import (C, <routine>, <C_routine>);
//
//  the GNAT compiler transforms all Ada calls to <routine> into calls to a
//  public static method mgnat.adalib.GNAT_libc.<C_routine> with the
//  appropriate parameter signature.
using System;
using System.Collections;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using Microsoft.Win32;

namespace mgnat.adalib
{

   public partial class GNAT_libc
   {

      ///////////////////////////////////////////////////////
      ///////////////////////////////////////////////////////
      ////                                               ////
      ////  General Purpose Routines Used in this Class  ////
      ////                                               ////
      ///////////////////////////////////////////////////////
      ///////////////////////////////////////////////////////

      //  Throws an exception if `b' is false

      private static void assert (bool b)
      {
         if (!b)
            raise ("assert failure");
      }

      //  Copy String `name' into a byte array `buffer' which is already allocated
      //  and add zero after the last character of name.

      private static void copy (String name, byte[] buffer)
      {
         int k;

         for (k = 0; k < name.Length; k++)
            buffer[k] = (byte)name[k];

         buffer[k] = 0;
      }

      //  size_t strlen (const char *)

      private static int strlen (byte[] name)
      {
         int zero_pos;

         for (zero_pos = 0; zero_pos < name.Length; zero_pos++)
         {
            if (name[zero_pos] == 0)
               return zero_pos;
         }
         return name.Length;
      }
      public static int strlen (Object name)
      {
         byte[] str = (byte[])name;
         return strlen (str);
      }

      //  Throws a java.lang.Error with message `msg'

      private static void raise (String msg)
      {
         throw new System.SystemException (msg);
      }


      //  Returns 1 if `b' is true, 0 otherwise

      private static int to_int (bool b)
      {
         return b ? 1 : 0;
      }


      ////////////////////////////////////////////////////////////////////////
      ////////////////////////////////////////////////////////////////////////
      ////                                                                ////
      ////  General Purpose Routines Used in Several JGNAT Library Units  ////
      ////                                                                ////
      ////////////////////////////////////////////////////////////////////////
      ////////////////////////////////////////////////////////////////////////

      //  The following routines are used to convert Ada strings into Java Strings
#if !SILVERLIGHT
      private static readonly System.Text.Encoding defaultEncoding =
        System.Text.Encoding.Default;
#else
      private static readonly System.Text.Encoding defaultEncoding =
        System.Text.Encoding.UTF8;
#endif

      private static String to_string (byte[] ada_string)
      {
         return defaultEncoding.GetString
                  (ada_string, 0, strlen (ada_string));
      }

      public static byte[] getBytes (String java_string)
      {
         return defaultEncoding.GetBytes (java_string);
      }

      public static String to_string_with_null (byte[] ada_string)
      {
         return defaultEncoding.GetString
                  (ada_string, 0, ada_string.Length);
      }

      //  Given a java.lang.Class object, the following returns an array of bytes
      //  containing the fully qualified name of that class. Because of the way
      //  DOTGNAT maps Ada tagged (and untagged) records and exceptions to CIL
      //  classes this routine is use to implement Ada.Tags.Expanded_Name and
      //  Ada.Exceptions.Exception_Name.

      //  More specifically if the fully qualified name of an Ada tagged type or
      //  exception D is X.Y.Z.D, where X, Y and Z are all packages, then JGNAT
      //  maps D into class x$y$z$d.class. If, on the other hand, D is declared
      //  inside an inner subprogram Z, then JGNAT adds a `__nnn', where `nnn' is
      //  some unique number, to the end of inner subprogram Z in case Z is
      //  overloaded. For instance if X is a package, Y a procedure declared
      //  inside X and Z an inner procedure of Y which is overloaded with another
      //  procdeure nested inside Y, JGNAT will generate file x$y$z__2$d.class.
      //  After retrieving the external_tag of a java class, the code below
      //  replaces all `$' with `.', removes all `__nnn' and converts the final
      //  string to upper case letters.

      public static byte[] ada_name (Object t)
      {
         //  First get the actual external tag
         byte[] s = external_tag (t);
         int s_length = s.Length;

         //  Search for the comma separator of AssemblyQualifiedName

         int j = 0;
         while ((j < s_length) && (s[j] != ',')) {
            j++;
         }
         s_length = j;

         //  Do not copy text after the comma

         byte[] r = new byte[s_length];

         int r_pos = 0;

         for (int k = 0; k < s_length; k++)
         {
            if ((s[k] == (byte)'_') && (s[k + 1] == (byte)'_'))
            {
               k = k + 2;
               while ((s[k] >= (byte)'0') && (s[k] <= (byte)'9'))
                  k++;
               k--;
            }
            else
            {
               if (s[k] == (byte)'$')
                  r[r_pos] = (byte)'.';
               else
                  r[r_pos] = (byte)System.Char.ToUpper ((char)s[k]);
               r_pos++;
            }
         }

         return r;
      }

      //  Given a JGNAT System.Address (that is a reference to an Object) this
      //  routine returns the hash code integer associated with this reference.
      //  Typically this is the address of the Object, but this is not guaranteed
      //  by the Java API.

      public static int hash_code (Object address)
      {
         return address.GetHashCode ();
      }


      /////////////////////////////////////
      /////////////////////////////////////
      ////                             ////
      ////  Entities in libc <math.h>  ////
      ////                             ////
      /////////////////////////////////////
      /////////////////////////////////////

      public static double sin (double a) { return System.Math.Sin (a); }
      public static double cos (double a) { return System.Math.Cos (a); }
      public static double tan (double a) { return System.Math.Tan (a); }
      public static double asin (double a) { return System.Math.Asin (a); }
      public static double acos (double a) { return System.Math.Acos (a); }
      public static double atan (double a) { return System.Math.Atan (a); }
      public static double exp (double a) { return System.Math.Exp (a); }
      public static double log (double a) { return System.Math.Log (a); }
      public static double sqrt (double a) { return System.Math.Sqrt (a); }
      public static double pow (double a, double b) { return System.Math.Pow (a, b); }

      public static double sinh (double a)
      {
         return (System.Math.Exp (a) - System.Math.Exp (-a)) * 0.5;
      }

      public static double cosh (double a)
      {
         return (System.Math.Exp (a) + System.Math.Exp (-a)) * 0.5;
      }

      public static double tanh (double a)
      {
         return (System.Math.Exp (a) - System.Math.Exp (-a)) /
                (System.Math.Exp (a) + System.Math.Exp (-a));
      }

      public static double round (double a)
      {
#if !COMPACT
         return System.Math.Round (a, System.MidpointRounding.AwayFromZero);
#else
      return System.Math.Round (a);
#endif
      }


      ///////////////////////////////////////
      ///////////////////////////////////////
      ////                               ////
      ////  Entities in libc <stdlib.h>  ////
      ////                               ////
      ///////////////////////////////////////
      ///////////////////////////////////////

      private static int EOF = -1;
      private static int _IOFBF = 0;
      private static int _IOLBF = 1;
      private static int _IONBF = 2;
      private static int SEEK_CUR = 1;
      private static int SEEK_END = 2;
      private static int SEEK_SET = 0;
      private static int L_tmpnam = 256;

      private static PushbackReader stdin = new mgnat.adalib.PushbackReader (Console.In);


      //  WARNING: The following is NOT a general implementation of the C routines
      //  in <stdlib.h>. The following implementation takes advantage of the way
      //  these routines are used in the GNAT sources.

      //  With this implementation the user can create and package an Ada applet
      //  on a UNIX workstation and run it on a Windows platform (which is the
      //  whole point of the JVM & the Java API).  The difference in text file
      //  formats between UNIX and Windows are hidden in the implementation of the
      //  Java API.

      //  void clearerr (FILE *stream)

      public static void clearerr (Object stream)
      {
         //  Every call in the GNAT sources to ferror looks (in spirit) like
         //
         //     if ferror (stream) /= 0 then
         //        raise Device_Error;
         //
         //  Because this functionality is not directly available in the Java API
         //  the C routines that are used in the GNAT sources and that may cause
         //  ferror to return a non zero value (fgetc, getc_immediate,
         //  getc_immediate_nowait, and fread) are implemented as follows:
         //
         //        getc (...) {
         //           try {
         //             ... // try to get the byte
         //           }
         //           catch (IOException e) {
         //              throw new ada$io_exceptions$device_error ();
         //           }
         //        }
         //
         //  All this means that our implementation of ferror can safely return 0.
         // As a result this routine does noting.
      }

      private static System.Type PushbackReaderType = typeof (PushbackReader);
      private static System.Type TextWriterType = typeof (System.IO.TextWriter);
      private static System.Type TextReaderType = typeof (System.IO.TextReader);
      private static System.Type BinaryReaderType = typeof (System.IO.BinaryReader);
      private static System.Type BinaryWriterType = typeof (System.IO.BinaryWriter);
      private static System.Type Object_FileType = typeof (Object_File);
      private static System.Type IntType = typeof (Int);
      private static System.Type LngType = typeof (Lng);
      private static System.Type FltType = typeof (Flt);
      private static System.Type DblType = typeof (Dbl);

      //  int fclose (FILE *stream)

      public static int fclose (Object stream)
      {
         try
         {
            if (PushbackReaderType.IsInstanceOfType (stream))
            {
               ((PushbackReader)stream).Close ();
            }
            else if (TextWriterType.IsInstanceOfType (stream))
            {
               ((System.IO.TextWriter)stream).Close ();
            }
            else if (BinaryReaderType.IsInstanceOfType (stream))
            {
               ((System.IO.BinaryReader)stream).Close ();
            }
            else if (BinaryWriterType.IsInstanceOfType (stream))
            {
               ((System.IO.BinaryWriter)stream).Close ();
            }
            else if (Object_FileType.IsInstanceOfType (stream))
            {
               ((Object_File)stream).Close ();
            }
            else
               assert (false);

            return 0;
         }
         catch (System.Exception)
         {
            return EOF;
         }
      }

      //  FILE *fdopen (int handle, const char *mode)
      //  UNIMPLEMENTED - not needed by the JGNAT library

      //  int feof (FILE *stream)

      public static int __gnat_feof (Object stream)
      {
         int c = ungetc (fgetc (stream), stream);
         return to_int (c == EOF);
      }

      //  int __gnat_ferror (FILE *stream)

      public static int __gnat_ferror (Object stream)
      {
         //  See comment inside method `clearerr' in this class
         return 0;
      }

      //  int fflush (FILE *stream)

      public static int fflush (Object stream)
      {
         if (TextWriterType.IsInstanceOfType (stream))
         {
            ((System.IO.TextWriter)stream).Flush ();
         }
         else if (BinaryWriterType.IsInstanceOfType (stream))
         {
            ((System.IO.BinaryWriter)stream).Flush ();
         }
         else if (Object_FileType.IsInstanceOfType (stream))
         {
            // do nothing
         }
         else
            assert (false);

         return 0;
      }

      //  int fgetc (FILE *stream)

      public static int fgetc (Object stream)
      {
         try
         {
            if (TextReaderType.IsInstanceOfType (stream))
            {
               return ((System.IO.TextReader)stream).Read ();
            }
            else if (PushbackReaderType.IsInstanceOfType (stream))
            {
               return ((PushbackReader)stream).Read ();
            }
            else
            {
               assert (false);
               return 0;
            }
         }
         catch (System.Exception)
         {
            throw new ada.io_exceptions.device_error ();
         }
      }

      //  char *fgets (char *s, int n, FILE *stream)
      //  UNIMPLEMENTED - not needed by the JGNAT library

      //  int __gnat_fileno (FILE *stream)

      public static int __gnat_fileno (Object stream)
      {
         if (stream == stdin)
         {
            return 0;
         }
         else if (stream == System.Console.Out)
         {
            return 1;
         }
         else if (stream == System.Console.Error)
         {
            return 2;
         }
         else
         {
            //  ??? for now always associate the same handle with a stream
            return 99;
         }
      }

      //  Special method to allow the top level file I/O packages
      //  to communicate the class of file API to be used by fopen
      //  (e.g., RandomAccessFile, Object_File). This avoids making
      //  JGNAT-specific changes to the lower-level file support
      //  packages such as System.File_IO, etc (it would be nice
      //  if the file classification could be simply passed to
      //  fopen as part of the mode info).

      public static byte file_class = (byte)'0';

      public static void set_file_class (byte fclass)
      {
         file_class = fclass;
      }

      //  FILE *__gnat_fopen (const char *filename, const char *mode, int encoding, const char *vms_form)

      public static object __gnat_fopen
        (object FILENAME, object MODE, int encoding, object VMS_FORM)
      {
         byte[] filename = (byte[])FILENAME;
         byte[] mode = (byte[])MODE;

         int end = strlen (mode) - 1;

         bool update = (mode[1] == (byte)'+');
         bool text_mode = (mode[end] == (byte)'t');
         bool binary_mode = (mode[end] == (byte)'b');

         assert (text_mode || binary_mode);

         bool read_only = (mode[0] == (byte)'r') && !update;
         bool write_only = (mode[0] == (byte)'w') && !update;
         bool read_update = (mode[0] == (byte)'r') && update;
         bool write_update = (mode[0] == (byte)'w') && update;

         assert (read_only || write_only || read_update || write_update);

         try
         {
            if (text_mode)
            {
               //  When opening a text file in read mode we must provide
               //  "ungetc" capabilities (this is provided by the
               //  PushbackReader).
               //  Furthermore, we must provide the ability of reading
               //  end-of-lines in a portable manner (this is provided by the
               //  StreamReader).
               //  Ensure that the file_class is reset, in case it was set
               //  to 'S' by freopen.
               file_class = (byte)'0';

               if (read_only)
               {
                  System.IO.FileStream r = File.Open
                    (to_string (filename), FileMode.Open,
                     FileAccess.Read, FileShare.Read);
                  return new PushbackReader (new StreamReader (r));
               }

               //  In read_update, we need to open a StreamWriter, setting
               //  the mode to Append to allow update. No action needed if
               //  the file does not exist.

               else if (read_update) {
                  if (__gnat_file_exists (filename) == 0) {
                     return null;
                  }
                  StreamWriter s = new StreamWriter
                    (File.Open (to_string (filename), FileMode.Append));
                  return s;
               }

               //  When opening a text file in write mode we must create it as
               //  a StreamWriter to be compatible with the way Standard.out is
               //  created.
               else
                  return new StreamWriter (File.Create (to_string (filename)));
            }
            else if (binary_mode && file_class == (byte)'S')
            {
               file_class = (byte)'0';
               if (read_only) {
                  return new BinaryReader
                     (File.Open (to_string (filename), FileMode.Open,
                      FileAccess.Read, FileShare.Read));
               }
               else if (read_update) {
                  BinaryWriter s = new BinaryWriter
                    (File.Open (to_string (filename), FileMode.Open));
                  s.BaseStream.Seek (0, SeekOrigin.End);
                  return s;
               }
               else
                  return new BinaryWriter (File.Create (to_string (filename)));
            }
            else
            {
               return new Object_File (to_string (filename),
                                    mode[0] == (byte)'w', update);
            }
         }
         catch (System.Exception)
         {
            return null;
         }
      }

      //  int fputc (int c, FILE *stream)

      public static int fputc (int c, Object stream)
      {
         try
         {
            if (TextWriterType.IsInstanceOfType (stream))
            {
               if (c == 10)
                  ((TextWriter)stream).WriteLine ();
               else
                  ((TextWriter)stream).Write ((char)c);
            }
            else if (BinaryWriterType.IsInstanceOfType (stream))
            {
               ((BinaryWriter)stream).Write (c);
            }
            else
               assert (false);

            return c;
         }
         catch (System.Exception)
         {
            throw new ada.io_exceptions.device_error ();
         }
      }

      //  int fputs (const char *s, FILE *stream)
      //  UNIMPLEMENTED - not needed by the JGNAT library

      //  size_t fread (void *ptr, size_t size, size_t nobj, FILE *stream)

      public static UInt32 fread (Object ptr, UInt32 size, UInt32 nobj, Object stream)
      {
         //  GNAT sources use a size of 1
         assert (size == 1);

         try
         {
            if (IntType.IsInstanceOfType (ptr))
            {
               assert (nobj == 4);
               ((Int)ptr).all = ((BinaryReader)stream).ReadInt32 ();
            }
            else if (LngType.IsInstanceOfType (ptr))
            {
               assert (nobj == 8);
               ((Lng)ptr).all = ((BinaryReader)stream).ReadInt64 ();
            }
            else if (FltType.IsInstanceOfType (ptr))
            {
               assert (nobj == 4);
               ((Flt)ptr).all = ((BinaryReader)stream).ReadSingle ();
            }
            else if (DblType.IsInstanceOfType (ptr))
            {
               assert (nobj == 8);
               ((Dbl)ptr).all = ((BinaryReader)stream).ReadDouble ();
            }
            else
            {
               return fread (ptr, 0, 1, nobj, stream);
            }
            return nobj;
         }
         catch (System.Exception)
         {
            throw new ada.io_exceptions.device_error ();
         }
      }

      //  This function does not exist in the C library but has been introduced
      //  because the GNAT sources take the address an element within the array
      //  and pass that address to fread to indicate that the array should be
      //  filled from that element on. This is not possible in the JVM and the
      //  following routine works exactly like fread except that it puts elements
      //  in buffer `ptr' at starting index `index'.

      public static UInt32 fread
        (Object ptr, UInt32 index, UInt32 size, UInt32 nobj, Object stream)
      {
         byte[] buf = (byte[])ptr;
         UInt32 nb_bytes_read = 0;

         //  GNAT sources use a size of 1
         assert (size == 1);

         try
         {
            if (PushbackReaderType.IsInstanceOfType (stream))
            {
               PushbackReader text_stream = (PushbackReader)stream;
               int k;
               int c;

               for (k = 0; k < nobj; k++)
               {
                  c = text_stream.Read ();

                  if (c != EOF)
                     buf[index + k] = (byte)c;
                  else
                     return nb_bytes_read;

                  nb_bytes_read++;
               }

               return nb_bytes_read;
            }
            else if (BinaryReaderType.IsInstanceOfType (stream))
            {
               return (UInt32)((BinaryReader)stream).Read (buf, (int)index, (int)nobj);
            }
            else if (BinaryWriterType.IsInstanceOfType (stream))
            {
               BinaryReader s =
                 new BinaryReader (((BinaryWriter)stream).BaseStream);

               return (UInt32)s.Read (buf, (int)index, (int)nobj);
            }
            else
               assert (false);
         }
         catch (System.Exception)
         {
            throw new ada.io_exceptions.device_error ();
         }
         return 0;
      }

      //  FILE *__gnat_freopen (const char *filename, const char *mode, FILE *stream, int encoding, const char *vms_form)

      public static Object __gnat_freopen
        (Object filename, Object mode, Object stream, int encoding, Object vms_form)
      {
         //  In the implementation of freopen we do NOT associate the returned
         //  stream with the input stream `stream' because GNAT sources do not
         //  need this and it is too much work to do so.
         fclose (stream);

         //  This is a kludge to ensure that Reset will work properly
         //  for stream files. The file could actually be a text file
         //  (opened for read/update), but fopen will check for the
         //  text file case first, so there won't be any problem with
         //  setting the file class to 'S' in that case.

         if (BinaryReaderType.IsInstanceOfType (stream) ||
             BinaryWriterType.IsInstanceOfType (stream))
         {
            set_file_class ((byte)'S');
         }

         return __gnat_fopen (filename, mode, encoding, vms_form);
      }

      //  int fseek (FILE *stream, long offset, int origin)

      public static int fseek (Object stream, long offset, int origin)
      {
         bool stream_is_binary = (BinaryReaderType.IsInstanceOfType (stream))
                                 || (BinaryWriterType.IsInstanceOfType (stream))
                                 || (Object_FileType.IsInstanceOfType (stream));

         //  The following assertions state the assumptions made in the following
         //  code given the current use of fseek from the GNAT sources.

         //  Calls to fseek with SEEK_CUR have not been implemented

         assert (origin != SEEK_CUR);

         //  Calls to fseek with SEEK_END must have offset == 0 and cannot be from
         //  text streams in read only mode (i.e. PushbackReader streams).

         assert ((origin != SEEK_END) || (offset == 0L));
         assert ((origin != SEEK_END) || !(PushbackReaderType.IsInstanceOfType (stream)));

         //  Calls to fseek with SEEK_SET can only be done for a file opened
         //  in binary mode (i.e., a RandomAccessFile or Object_File).
         //  Need also to be able to SEEK_SET to 0L for text files for Reset

         if (PushbackReaderType.IsInstanceOfType (stream) &&
             (origin == SEEK_SET) && (offset == 0L))
         {
            ((PushbackReader)stream).rewind ();
            return 0;
         }
         assert ((origin != SEEK_SET) || stream_is_binary);

         try
         {
            if (origin == SEEK_SET)
            {
               if (BinaryReaderType.IsInstanceOfType (stream))
               {
                  ((BinaryReader)stream).BaseStream.Seek (offset, SeekOrigin.Begin);
               }
               else if (BinaryWriterType.IsInstanceOfType (stream))
               {
                  ((BinaryWriter)stream).BaseStream.Seek (offset, SeekOrigin.Begin);
               }
               else if (Object_FileType.IsInstanceOfType (stream))
               {
                  Object_File.set_file_index
                          ((Object_File)stream, offset + 1);
               }
            }
            else if (origin == SEEK_END)
            {
               if (stream_is_binary)
               {
                  if (BinaryReaderType.IsInstanceOfType (stream))
                  {
                     BinaryReader s = (BinaryReader)stream;
                     s.BaseStream.Seek (0, SeekOrigin.End);
                  }
                  else if (BinaryWriterType.IsInstanceOfType (stream))
                  {
                     BinaryWriter s = (BinaryWriter)stream;
                     s.BaseStream.Seek (0, SeekOrigin.End);
                  }
                  else if (Object_FileType.IsInstanceOfType (stream))
                  {
                     Object_File s = (Object_File)stream;
                     Object_File.set_file_index
                       (s, Object_File.file_size (s) + 1);
                  }
               }
               //  There is nothing to do for text streams that you can write into
               //  since for these fseek is only called in append mode and the
               //  actual positioning at the end of the file in append mode is
               //  done by fopen.
            }
            return 0;
         }
         catch (System.Exception)
         {
            return 1;
         }
      }

      //  long ftell (FILE *stream)

      public static long ftell (Object stream)
      {
         try
         {
            if (BinaryReaderType.IsInstanceOfType (stream))
               return ((BinaryReader)stream).BaseStream.Seek (0, SeekOrigin.Current);
            else if (BinaryWriterType.IsInstanceOfType (stream))
               return ((BinaryWriter)stream).BaseStream.Seek (0, SeekOrigin.Current);
            else
               assert (false);
         }
         catch (System.Exception)
         {
            throw new ada.io_exceptions.device_error ();
         }
         return -1L;
      }

      //  size_t fwrite (const void *ptr, size_t size, size_t nobj, FILE *stream)

      public static UInt32 fwrite (Object ptr, UInt32 size,
         UInt32 nobj, Object stream)
      {
         //  GNAT sources use either a size of 1 or an nboj of 1

         assert ((size == 1) || (nobj == 1));

         try
         {
            if (IntType.IsInstanceOfType (ptr))
            {
               assert ((size == 4) || (nobj == 4));
               ((BinaryWriter)stream).Write (((Int)ptr).all);
            }
            else if (LngType.IsInstanceOfType (ptr))
            {
               assert ((size == 8) || (nobj == 8));
               ((BinaryWriter)stream).Write (((Lng)ptr).all);
            }
            else if (FltType.IsInstanceOfType (ptr))
            {
               assert ((size == 4) || (nobj == 4));
               ((BinaryWriter)stream).Write (((Flt)ptr).all);
            }
            else if (DblType.IsInstanceOfType (ptr))
            {
               assert ((size == 8) || (nobj == 8));
               ((BinaryWriter)stream).Write (((Dbl)ptr).all);
            }
            else
            {
               byte[] buf = (byte[])ptr;

               if (TextWriterType.IsInstanceOfType (stream))
               {
                  TextWriter text_stream = (TextWriter)stream;
                  int c;
                  int length = (size == 1) ? (int)nobj : (int)size;

                  for (int k = 0; k < length; k++)
                  {
                     c = 255 & buf[k];

                     if (c == 10)
                        text_stream.WriteLine ();
                     else
                        text_stream.Write ((char)c);
                  }
               }
               else if (BinaryWriterType.IsInstanceOfType (stream))
               {
                  ((BinaryWriter)stream).Write (buf, 0, (int)size);
               }
               else
               {
                  assert (false);
               }
            }
            return nobj;
         }
         catch (System.Exception)
         {
            return 0;
         }
      }

      //  void getc_immediate_nowait
      //         (FILE *stream, int *ch, int *end_of_file, int *avail)

      public static void getc_immediate_nowait
                           (Object stream, Int ch, Int end_of_file, Int avail)
      {
         if (PushbackReaderType.IsInstanceOfType (stream))
            try
            {
               if (!((PushbackReader)stream).ready ())
               {
                  end_of_file.all = 0;
                  avail.all = 0;
                  return;
               }
            }
            catch (System.Exception)
            {
               throw new ada.io_exceptions.device_error ();
            }

         avail.all = 1;
         getc_immediate (stream, ch, end_of_file);
      }

      //  void getc_immediate (FILE *stream, int *ch, int *end_of_file)

      public static void getc_immediate (Object stream, Int ch, Int end_of_file)
      {
         ch.all = fgetc (stream);
         end_of_file.all = to_int (ch.all == EOF);
      }

      //  int isatty (int handle)
      //  UNIMPLEMENTED - not needed by the JGNAT library

      //  char *mktemp (char *buf)
      //  UNIMPLEMENTED - not needed by the JGNAT library

      //  void rewind (FILE *stream)

      public static void rewind (Object stream)
      {
         fseek (stream, 0L, SEEK_SET);
      }

      //  int setvbuf (FILE *stream, char *buf, int mode, size_t size)

      public static int setvbuf (Object stream, Object buf, int mode,
         UInt32 size)
      {
         assert (buf == null);

         //  System.out & System.err are unbuffered by default
         if ((stream == Console.Out) || (stream == Console.Error))
         {
            assert (mode == _IONBF);
         }

               //  Input & output text streams are buffered by default
         else if (mode == _IONBF)
         {
            assert (!(TextWriterType.IsInstanceOfType (stream)));
            assert (!(TextReaderType.IsInstanceOfType (stream)));
         }

               //  Random Acces Files are unbuffered by default
         else if ((mode == _IOLBF) || (mode == _IOFBF))
         {
            assert (!(BinaryReaderType.IsInstanceOfType (stream)));
            assert (!(BinaryWriterType.IsInstanceOfType (stream)));
         }

         else
         {
            assert (false);
         }

         return 0;
      }

      //  FILE *tmpfile (void)
      //  UNIMPLEMENTED - not needed by the JGNAT library

      //  char *tmpnam (char s[L_tmpnam])

      static public void tmpnam (Object s)
      {
         byte[] buf = (byte[])s;

         try
         {
            copy (System.IO.Path.GetTempFileName (), buf);
         }
         catch (System.Exception)
         {
            buf[0] = 0;
         }
      }

      static public void __gnat_tmp_name (byte[] s)
      {
         tmpnam (s);
      }

      //  int ungetc (int c, FILE *stream)

      public static int ungetc (int c, Object stream)
      {
         if (c == EOF)
            return EOF;

         try
         {
            if (PushbackReaderType.IsInstanceOfType (stream))
            {
               ((PushbackReader)stream).unread (c);
            }

            //  For a binary stream (i.e.  a RandomAccessFile) ungetc is used as
            //  follows in the GNAT sources:
            //       ungetc (fgetc (stream), Stream)
            //  which allows us to implement ungetc by simply seking back one
            //  character.
            else if (BinaryReaderType.IsInstanceOfType (stream))
            {
               BinaryReader r = (BinaryReader)stream;
               r.BaseStream.Seek (-1, SeekOrigin.Current);
            }

            return c;
         }
         catch (System.Exception)
         {
            raise ("ungetc failed");
            return EOF;
         }
      }

      public static int __gnat_try_lock (Object dir, Object file)
      {
         string path = to_string ((byte[])dir) + "\\" + to_string ((byte[])file);
         if (File.Exists (path))
            return 0;
         else
            return 1;
      }

      //  int unlink (char *)

      private static int _internal_unlink (Object filename)
      {
         try
         {
            System.IO.File.Delete (to_string ((byte[])filename));
            return 0;
         }
         catch (System.Exception)
         {
            return EOF;
         }
      }

      public static void unlink (Object filename)
      {
         _internal_unlink (filename);
      }

      public static int __gnat_unlink (Object filename)
      {
         return _internal_unlink (filename);
      }

      /////////////////////////////////////
      /////////////////////////////////////
      ////                             ////
      ////  Entities in libc <time.h>  ////
      ////                             ////
      /////////////////////////////////////
      /////////////////////////////////////


      private static System.DateTime start_cal =
        new System.DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);

      private static int secsFromTicks (long ticks)
      {
         return (int)((ticks - start_cal.Ticks) / 10000000);
      }
      private static long ticksFromSecs (int timer)
      {
         return (long)timer * 10000000 + start_cal.Ticks;
      }

      public static Int32 __gnat_current_time ()
      {
         return secsFromTicks(System.DateTime.Now.Ticks);
      }

      public static Int64 __gnat_current_time_ms ()
      {
         return (Int64)((System.DateTime.Now.Ticks - start_cal.Ticks) / 10000);
      }

      //  void __gnat_localtime_tzoff
      //         (const time_t *timer, const int *is_historic, long *off)
      public static void __gnat_localtime_tzoff (Int timer, Int is_historic, Lng off)
      {
#if !SILVERLIGHT
         // timer is a time_t value: number of seconds from epoch = start_cal.
         System.DateTime time = new System.DateTime (ticksFromSecs (timer.all));
         off.all = (long)(System.TimeZone.CurrentTimeZone.GetUtcOffset (time).TotalSeconds);
#else
         off.all = 0;
#endif
      }

      public static void __gnat_to_gm_time (Object time_t, Object year, Object month, Object day, Object hours, Object mins, Object secs)
      {
         System.DateTime theTime = new DateTime (ticksFromSecs (((Int)time_t).all));
         ((Int)year).all = theTime.Year;
         ((Int)month).all = theTime.Month;
         ((Int)day).all = theTime.Day;
         ((Int)hours).all = theTime.Hour;
         ((Int)mins).all = theTime.Minute;
         ((Int)secs).all = theTime.Second;
      }

      ////////////////////////////////
      ////////////////////////////////
      ////                        ////
      ////  Entities in adaint.c  ////
      ////                        ////
      ////////////////////////////////
      ////////////////////////////////

      public static byte __gnat_dir_separator = (byte)System.IO.Path.DirectorySeparatorChar;
      public static byte __gnat_path_separator = (byte)System.IO.Path.PathSeparator;

      public static int __gnat_file_exists (Object name)
      {
         try
         {
            return to_int (File.Exists (to_string ((byte[])name)));
         }
         catch (System.Exception) { }

         return 0;
      }

      public static long __gnat_named_file_length (Object name)
      {
         return (new System.IO.FileInfo (to_string ((byte[])name))).Length;
      }

      public static int __gnat_get_file_names_case_sensitive ()
      {
         if (System.IO.Path.DirectorySeparatorChar == '/')
            return 1;
         else
            return 0;
      }
      public static int chdir (byte[] name)
      {
         try
         {
            System.IO.Directory.SetCurrentDirectory (to_string (name));
         }
         catch (System.Exception) { return -1; }

         return 0;
      }
      public static int __gnat_chdir (byte[] name)
      {
         return chdir (name);
      }

      public static int closedir (Object dir)
      {
         ((Directory_Parse)dir).close ();
         return 1;
      }
      public static int __gnat_dir_isopen (Object dir)
      {
         if (((Directory_Parse)dir).get_is_open ())
         {
            return 1;
         }
         else
         {
            return 0;
         }
      }

      public static int __gnat_get_current_dir (byte[] buffer)
      {
         string current = System.IO.Directory.GetCurrentDirectory ();
         byte[] name = getBytes (current);
         for (int j = 0; j < name.Length; j++)
           buffer[j] = name[j];
         return name.Length;
      }
      public static int __gnat_mkdir (byte[] name)
      {
         try
         {
            System.IO.Directory.CreateDirectory (to_string (name));
         }
         catch (System.Exception) { return -1; }

         return 0;
      }
      public static void rmdir (byte[] name)
      {
         System.IO.Directory.Delete (to_string (name));
      }
      public static void __gnat_rmdir (byte[] name)
      {
         rmdir (name);
      }
      public static Object opendir (byte[] name)
      {
         return (Object)new Directory_Parse (to_string (name));
      }
      public static int __gnat_readdir (Object dir, byte[] buffer)
      {
         Directory_Parse dir_parse = (Directory_Parse)dir;
         String s = dir_parse.get_next ();
         if (s == "")
         {
            return 0;
         }
         else
         {
            byte[] name = getBytes (s);
            for (int j = 0; j < name.Length; j++) buffer[j] = name[j];
            return name.Length;
         }
      }
      public static int __gnat_readdir_is_thread_safe ()
      {
         return 0;
      }
      public static void __gnat_filecopy (byte[] from, byte[] to)
      {
         System.IO.File.Copy (to_string (from), to_string (to), true);
      }
      public static void __gnat_touch (byte[] name)
      {
#if !COMPACT
         System.IO.File.SetLastWriteTime (
            to_string (name), System.DateTime.Now);
#endif
      }

      public static int __gnat_is_file (byte[] name)
      {
         if (System.IO.File.Exists (to_string (name)))
            return 1;
         return 0;
      }
      public static void __gnat_filemove (byte[] from, byte[] to)
      {
         System.IO.File.Move (to_string (from), to_string (to));
      }

      public static int __gnat_readlink (Object path, Object buff, Int32 bufsize)
      {
         return -1;
      }

      private static int errno = -1;

      public static void __set_errno (int err)
      {
         errno = err;
      }

      public static int __get_errno ()
      {
         return errno;
      }

      //////////////////////////////////
      //////////////////////////////////
      ////                          ////
      ////  EnvVar  Entities        ////
      ////                          ////
      //////////////////////////////////
      //////////////////////////////////

      public static byte __gnat_environment_char = (byte)'%';

#if !COMPACT
      public static void __gnat_unsetenv (Object name)
      {
         byte[] the_name = (byte[])name;
         System.Environment.SetEnvironmentVariable (to_string (the_name), null);
      }

      public static void __gnat_clearenv ()
      {
         IDictionary environmentVariables = Environment.GetEnvironmentVariables ();
         foreach (DictionaryEntry de in environmentVariables)
         {
            System.Environment.SetEnvironmentVariable ((String)de.Key, null);
         }
      }
      public static bool __gnat_isenv (byte[] name)
      {
         return System.Environment.GetEnvironmentVariable (to_string (name))
           != null;
      }
      public static void __gnat_setenv (byte[] name, byte[] value)
      {
         System.Environment.SetEnvironmentVariable (
            to_string (name), to_string (value));
      }
      public static void __gnat_getenv (byte[] name,
         mgnat.adalib.Int len, mgnat.adalib.Acc value)
      {
         string s =
           System.Environment.GetEnvironmentVariable (to_string (name));
         if (s != null)
         {
            len.all = s.Length;
            value.all = (Object)s;
         }
         else
         {
            len.all = 0;
            value.all = (Object)"";
         }
      }
      public static void __gnat_getenv (object name, object len, object value)
      {
         __gnat_getenv ((byte[])name, (mgnat.adalib.Int) len, (mgnat.adalib.Acc) value);
      }
      public static byte[] __gnat_getenv (byte[] name)
      {
         string s = System.Environment.GetEnvironmentVariable (to_string (name));
         if (s != null) return getBytes (s);
         return getBytes ("");
      }
#endif

      public static void strncpy (Object dest, Object src, int len)
      {
         string s_src = (String)src;
         byte[] l_dst = getBytes (s_src.Substring (0, len));
         for (int j = 0; j < l_dst.Length; j++) ((byte[])dest) [j] = l_dst[j];
      }

#if !COMPACT
      public static Object __gnat_environ ()
      {
         IDictionary id = System.Environment.GetEnvironmentVariables ();
         ICollection keys = id.Keys;
         string[] the_array = new string[keys.Count];
         int i = 0;
         foreach (DictionaryEntry de in id)
         {
            the_array[i++] = ((String)de.Key) + "=" + ((String)de.Value);
         }
         return (Object)the_array;
      }
      public static int __gnat_environ_length (Object env)
      {
         return ((string[])env).Length;
      }
      public static byte[] __gnat_environ_value (Object env, int num)
      {
         string[] keys = (string[])env;
         return getBytes (keys[num - 1]);
      }
#endif


#if !COMPACT
      //////////////////////////////////
      //////////////////////////////////
      ////                          ////
      ////  Registry Entities       ////
      ////                          ////
      //////////////////////////////////
      //////////////////////////////////
      public static Object getPerformance_Data ()
      {
         return Microsoft.Win32.Registry.PerformanceData;
      }
      public static Object getClassesRoot ()
      {
         return Microsoft.Win32.Registry.ClassesRoot;
      }
      public static Object getLocalMachine ()
      {
         return Microsoft.Win32.Registry.LocalMachine;
      }
      public static Object getCurrentConfig ()
      {
         return Microsoft.Win32.Registry.CurrentConfig;
      }
      public static Object getCurrentUser ()
      {
         return Microsoft.Win32.Registry.CurrentUser;
      }
      public static Object getUsers ()
      {
         return Microsoft.Win32.Registry.Users;
      }
      public static void RegCloseKey (Object key)
      {
         ((Microsoft.Win32.RegistryKey)key).Close ();
      }
      public static Object CreateSubKey (Object key, byte[] name, bool write)
      {
         if (!write)
         {
            return ((Microsoft.Win32.RegistryKey)key).CreateSubKey (
               to_string (name), RegistryKeyPermissionCheck.ReadSubTree);
         }
         else
         {
            return ((Microsoft.Win32.RegistryKey)key).CreateSubKey (
               to_string (name), RegistryKeyPermissionCheck.ReadWriteSubTree);
         }
      }
      public static void DeleteSubKeyTree (Object key, byte[] name)
      {
         ((Microsoft.Win32.RegistryKey)key).DeleteSubKeyTree (to_string (name));
      }
      public static void DeleteValue (Object key, byte[] name)
      {
         ((Microsoft.Win32.RegistryKey)key).DeleteValue (to_string (name));
      }
      public static Object OpenSubKey (Object key, byte[] name, bool write)
      {
         if (!write)
         {
            return ((Microsoft.Win32.RegistryKey)key).OpenSubKey (
               to_string (name), RegistryKeyPermissionCheck.ReadSubTree);
         }
         else
         {
            return ((Microsoft.Win32.RegistryKey)key).OpenSubKey (
               to_string (name), RegistryKeyPermissionCheck.ReadWriteSubTree);
         }
      }
      public static int RegGetValue (Object key, byte[] name, byte[] result)
      {
         string s = (string)((Microsoft.Win32.RegistryKey)key).GetValue (
            to_string (name), null,
            RegistryValueOptions.DoNotExpandEnvironmentNames);
         byte[] res = getBytes (s);
         for (int j = 0; j < res.Length; j++) result[j] = res[j];
         return res.Length;
      }
      public static void RegSetValue (Object key, byte[] name, byte[] value)
      {
         ((Microsoft.Win32.RegistryKey)key).SetValue (
            to_string (name), to_string (value));
      }
      private static ArrayList reg_names;
      public static int InitEnumeration (Object key)
      {
         Microsoft.Win32.RegistryKey rk = (Microsoft.Win32.RegistryKey)key;
         String[] names = rk.GetValueNames ();
         reg_names = new ArrayList ();
         for (int i = 0; i < names.Length; i++)
         {
            if (rk.GetValueKind (names[i]) == RegistryValueKind.String ||
                rk.GetValueKind (names[i]) == RegistryValueKind.ExpandString)
            {
               reg_names.Add (names[i]);
            }
         }
         return reg_names.Count;
      }
      public static int RegEnumName (int num, byte[] result)
      {
         string s = (string)reg_names[num - 1];
         byte[] res = getBytes (s);
         for (int j = 0; j < res.Length; j++) result[j] = res[j];
         return res.Length;
      }
      public static int Enumerate (Object key, int num, byte[] result)
      {
         string s = (string)((Microsoft.Win32.RegistryKey)key).GetValue (
            (string)reg_names[num - 1]);
         byte[] res = getBytes (s);
         for (int j = 0; j < res.Length; j++) result[j] = res[j];
         return res.Length;
      }
#endif

      ////////////////////////////////
      ////////////////////////////////
      ////                        ////
      ////  Entities in cstrea.c  ////
      ////                        ////
      ////////////////////////////////
      ////////////////////////////////

      public static readonly int __gnat_constant_eof = EOF;
      public static readonly int __gnat_constant_iofbf = _IOFBF;
      public static readonly int __gnat_constant_iolbf = _IOLBF;
      public static readonly int __gnat_constant_ionbf = _IONBF;
      public static readonly int __gnat_constant_seek_cur = SEEK_CUR;
      public static readonly int __gnat_constant_seek_end = SEEK_END;
      public static readonly int __gnat_constant_seek_set = SEEK_SET;
      public static readonly int __gnat_constant_l_tmpnam = L_tmpnam;

      public static int __gnat_max_path_len = 2048;

      public static void __gnat_full_name (Object name, Object buffer)
      {
         byte[] buff = (byte[])buffer;

         try
         {
            copy
                    (System.IO.Path.GetFullPath (to_string ((byte[])name)),
                      (byte[])buffer);
         }
         catch (System.Exception)
         {
            buff[0] = 0;
         }
      }

      public static int __gnat_is_regular_file_fd (int handle)
      {
         if (handle <= 2)
            return 0;
         else
            //  ??? for now always assume we are dealing with regular files
            return 1;
      }

      public static Object __gnat_constant_stdin () { return stdin; }
      public static Object __gnat_constant_stderr () { return Console.Error; }
      public static Object __gnat_constant_stdout () { return Console.Out; }
      public static int have_console () { return 1; }

      public static void __gnat_to_stderr (byte[] msg)
      {
         Console.Error.Write (to_string ((byte[])msg));
      }

      ////////////////////////////////////
      ////////////////////////////////////
      ////                            ////
      ////  Entities in a-except.adb  ////
      ////                            ////
      ////////////////////////////////////
      ////////////////////////////////////

      private static string __gnat_error_msg (Object file, int line, byte[] msg)
      {
         return (string)file + ":" + line.ToString() + " " + to_string (msg);
      }

      public static void __gnat_raise_constraint_error_msg (Object file, int line, byte[] msg)
      {
         throw new mgnat.adalib.constraint_error
           (__gnat_error_msg (file, line, msg));
      }

      public static void __gnat_raise_program_error_msg (Object file, int line, byte[] msg)
      {
         throw new mgnat.adalib.program_error
           (__gnat_error_msg (file, line, msg));
      }

      public static void __gnat_raise_storage_error_msg (Object file, int line, byte[] msg)
      {
         throw new mgnat.adalib.storage_error
           (__gnat_error_msg (file, line, msg));
      }

      //  function Create_EO (E : Exception_Id; M : String) return EO;

      public static Object create_EO (Object e, byte[] msg)
      {
         try
         {
            System.Type myType = (System.Type)e;
            System.Type[] types = new System.Type[1];
            types[0] = typeof (String);
            System.Reflection.ConstructorInfo c = myType.GetConstructor (types);
            System.Object[] param = { to_string (msg) };
            if (c == null)
            {
               Console.WriteLine (e.ToString ());
               raise ("The String constructor for this exception does not exist");
            }
            return c.Invoke (param);
         }
         catch (System.Security.SecurityException e1)
         {
            Console.WriteLine (e1.ToString ());
            raise ("Not allowed to call the String constructor");
         }
         catch (System.ArgumentException e1)
         {
            Console.WriteLine (e1.ToString ());
            raise ("Wrong number of parameters for the exception constructor");
         }
         catch (System.Exception e1)
         {
            Console.WriteLine (e1.ToString ());
            raise ("Exception raised during call to the constructor");
         }
         return null;
      }

      // function Exception_Identity (X : EO) return Exception_Id;

      public static Object exception_identity (Object x)
      {
         return x.GetType ();
      }

      //  function E_Information (X : EO) return String_Access;

      public static byte[] __gnat_tailored_exception_information (Object x)
      {
#if COMPACT
        return e_information (x);
#else
        return getBytes (((System.Exception)x).Message + ((System.Exception)x).StackTrace);
#endif
      }

      public static byte[] e_information (Object x)
      {
#if COMPACT
        return getBytes (((System.Exception) x).Message);
#else
        return getBytes (((System.Exception)x).StackTrace);
#endif
      }

      //  function E_Message (X : EO) return String_Access;

      public static byte[] e_message (Object x)
      {
         return getBytes (((System.Exception)x).Message);
      }

      //  procedure Reraise_Occurrence_No_Defer (X : EO);

      public static void reraise_occurrence_no_defer (Object e)
      {
         throw (System.Exception)e;
      }

      public static void __gnat_raise_exception (Object e, byte[] msg)
      {
         throw (System.Exception)create_EO (e, msg);
      }

      public static void __gnat_transfer_occurrence (Acc target, Object source)
      {
         target.all = source;
      }

      public static System.Exception c_e = new mgnat.adalib.constraint_error ();
      public static System.Exception p_e = new mgnat.adalib.program_error ();
      public static System.Exception s_e = new mgnat.adalib.storage_error ();
      public static System.Exception t_e = new mgnat.adalib.tasking_error ();

      public static Object constraint_error = exception_identity (c_e);
      public static Object program_error = exception_identity (p_e);
      public static Object storage_error = exception_identity (s_e);
      public static Object tasking_error = exception_identity (t_e);

      //////////////////////////////
      //////////////////////////////
      ////                      ////
      ////  Entities in init.c  ////
      ////                      ////
      //////////////////////////////
      //////////////////////////////

      public static int __gl_main_priority = -1;
      public static int __gl_main_cpu = -1;
      public static int __gl_time_slice_val = -1;
      public static byte __gl_wc_encoding = (byte)'n';
      public static byte __gl_locking_policy = (byte)' ';
      public static byte __gl_queuing_policy = (byte)' ';
      public static byte __gl_task_dispatching_policy = (byte)' ';

      public static System.Object __gl_priority_specific_dispatching = null;
      public static int __gl_num_specific_dispatching = 0;
      public static System.Object __gl_interrupt_states = null;
      public static int __gl_num_interrupt_states = 0;
      public static int __gl_unreserve_all_interrupts = 0;
      public static int __gl_exception_tracebacks = 0;
      public static int __gl_zero_cost_exceptions = 0;
      public static int __gl_detect_blocking = 0;
      public static int __gl_default_stack_size = -1;
      public static int __gl_leap_seconds_support = 0;

      public static int __gnat_handler_installed = 0;

      public static void __gnat_init_float () { }

      public static byte __gnat_get_interrupt_state (int id)
      {
        return (byte)'n';
      }

      ////////////////////////////////
      ////////////////////////////////
      ////                        ////
      ////  Entities in sysdep.c  ////
      ////                        ////
      ////////////////////////////////
      ////////////////////////////////

      public static bool __gnat_text_translation_required = true;
      public static long __gnat_invalid_tzoff = 259273;

      public static void __gnat_set_binary_mode (int handle) { }
      public static void __gnat_set_text_mode (int handle) { }


      //////////////////////////////////
      //////////////////////////////////
      ////                          ////
      ////  Entities in a-tags.adb  ////
      ////                          ////
      //////////////////////////////////
      //////////////////////////////////

      //  function External_Tag (T : Tag) return String;

      public static byte[] external_tag (Object t)
      {
         String s = ((System.Type)t).AssemblyQualifiedName;

         return getBytes (s);
      }

      //  function Internal_Tag (External : String) return Tag;

      public static Object internal_tag (byte[] external)
      {
         return System.Type.GetType (to_string (external));
      }

      //////////////////////////////
      //////////////////////////////
      ////                      ////
      ////  Entities in argv.c  ////
      ////                      ////
      //////////////////////////////
      //////////////////////////////

      public static String[] gnat_argv = null;
      public static String command_name = null;

      public static Boolean __gnat_argv_initialized ()
      {
         return gnat_argv != null;
      }

      public static int __gnat_arg_count ()
      {
         //  We return length+1 to compensate for Ada.Command_Line.Argument_Count,
         //  which subtracts one because of the presence of the command name
         //  argument on non-JVM systems.

         if (gnat_argv == null)
            return 1;
         else
            return gnat_argv.Length + 1;
      }

      public static void __gnat_fill_arg (byte[] arg, int arg_num)
      {
         byte[] arg_string = arg;
         int k;

         if ((arg_num == 0) && (command_name != null))
         {
            for (k = 0; k < command_name.Length; k++)
               arg_string[k] = (byte)command_name[k];

            return;
         }

         if ((gnat_argv == null) || (arg_num < 1) || (arg_num > gnat_argv.Length))
            throw new mgnat.adalib.constraint_error ();

         else
            for (k = 0; k < gnat_argv[arg_num - 1].Length; k++)
               arg_string[k] = (byte)gnat_argv[arg_num - 1][k];
      }

      public static int __gnat_len_arg (int arg_num)
      {
         if (arg_num == 0)
            if (command_name != null)
               return command_name.Length;
            else
               return 0;
         else
            return gnat_argv[arg_num - 1].Length;
      }

      public static void __gnat_set_exit_status (int code)
      {
#if !COMPACT
         System.Environment.ExitCode = code;
#endif
      }

      public static void __gnat_install_SEH_handler (Object o)
      {
         //  Do nothing
      }

#if !COMPACT
      private static void __exception_handler (Exception e)
      {
         try {
            //  get the exception's ada name
            string e_name = to_string (ada_name (exception_identity (e)));
            string e_msg  = e.Message;

            if (e_name.StartsWith ("MGNAT.ADALIB."))
              e_name = e_name.Remove (0, 13);
            else if (e is System.OverflowException) {
              StackTrace stack = new StackTrace (e, true);
              StackFrame frame = stack.GetFrame (0);

              e_name = "CONSTRAINT_ERROR";
              e_msg = frame.GetFileName() + ":" + frame.GetFileLineNumber() + " overflow check failed";
            }

            Console.Error.WriteLine ();

            // Check for special case of raising _ABORT_SIGNAL, which is not
            // really an exception at all. We recognize this by the fact that
            // it is the only exception whose name starts with underscore.

            if (e_name.StartsWith ("_"))
              Console.Error.WriteLine
                ("Execution terminated by abort of environment task");
            else if (__gl_exception_tracebacks == 0)
            {
               Console.Error.WriteLine
                 ("raised " + e_name + " : " + e_msg);
            } else {
               StackTrace stack = new StackTrace (e, true);
               Console.Error.WriteLine
                 ("Execution terminated by unhandled exception");
               Console.Error.WriteLine
                 ("Exception name: " + e_name);
               Console.Error.WriteLine
                 ("Message: " + e_msg);
               Console.Error.WriteLine
                 ("Call stack traceback locations:");
               foreach (StackFrame frame in stack.GetFrames())
               {
                  Console.Error.WriteLine
                    ("   " + frame.GetMethod() + " at " + frame.GetFileName() + ":" + frame.GetFileLineNumber());
               }
            }
         } finally {
            Environment.Exit(1);
         }
      }

      private static void __app_exception_handler (object sender, System.Threading.ThreadExceptionEventArgs args)
      {
         __exception_handler ((Exception) args.Exception);
      }

      private static void __appdomain_exception_handler (object sender, UnhandledExceptionEventArgs args)
      {
         __exception_handler ((Exception) args.ExceptionObject);
      }
#endif

      public static void __gnat_install_handler ()
      {
#if !COMPACT
         System.Windows.Forms.Application.ThreadException +=
            new System.Threading.ThreadExceptionEventHandler(__app_exception_handler);
         System.AppDomain.CurrentDomain.UnhandledException +=
            new UnhandledExceptionEventHandler(__appdomain_exception_handler);
#endif
         __gnat_handler_installed = 1;
      }

      public static void debug_string (byte[] s)
      {
         Console.WriteLine (to_string (s));
      }

      public static void debug_split (long ms, int ns)
      {
         Console.WriteLine ("split:" + ms + "," + ns);
      }

      public static void Debug_Sleep (long ms, long ns)
      {
         Console.WriteLine ("sleep:" + ms + "," + ns);
      }

      public static void __gnat_os_exit (int code)
      {
#if !COMPACT
         Environment.Exit (code);
#endif
      }

      public static void abort ()
      {
#if !COMPACT
         Environment.Exit (0);
#endif
      }

      /////////////////////////
      // Support for GNAT.IO //
      /////////////////////////

      public static void putchar (byte c)
      {
         Console.Write ((char)c);
      }

      public static void put_char (byte c)
      {
         Console.Write ((char)c);
      }

      public static void put_char_stderr (byte c)
      {
         Console.Write ((char)c);
      }

      public static void put_int (int i)
      {
         Console.Write (i);
      }

      public static void put_int_stderr (int i)
      {
         Console.Write (i);
      }

      public static int get_int ()
      {
         return Convert.ToInt32 (Console.ReadLine ());
      }

      public static byte get_char ()
      {
         return (byte)Console.In.Read ();
      }

     public static object box (Double d)
     {
       return d;
     }

     public static object box (int i)
     {
       return i;
     }
     public static Double unbox_double (object obj)
     {
       return (Double)obj;
     }

     public static int unbox_int (object obj)
     {
       return (int)obj;
     }

     public static bool __gnat_fl_valid (Single fl)
     {
        return !(Single.IsNaN(fl) || Single.IsNegativeInfinity(fl) || Single.IsPositiveInfinity(fl));
     }

     public static bool __gnat_fl_valid (Double fl)
     {
        return !(Double.IsNaN(fl) || Double.IsNegativeInfinity(fl) || Double.IsPositiveInfinity(fl));
     }

     public static int __gnat_number_of_cpus ()
     {
        // ??? dummy implementation
        return 1;
     }
   }
} //namespace
