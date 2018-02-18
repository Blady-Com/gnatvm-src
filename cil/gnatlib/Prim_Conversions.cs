//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                P r i m i t i v e _ C o n v e r s i o n s                 //
//                                                                          //
//                     Copyright (C) 1998-2007, AdaCore                     //
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

//  This C# file is part of the GNAT for .NET library and provides low-level
//  conversions between primitive types and byte arrays for the purpose
//  of supporting the implementation of the predefined stream attributes
//  (see 5jstratt.adb). It uses Data streams attached to the JGNAT
//  utility classes Prim_Input_Stream and Prim_Output_Stream to effect
//  these conversions.

using System;

namespace mgnat.adalib {
public class Prim_Conversions {

  //  The following streams provide low-level facilities for storing
  //  and retrieving byte sequences representating scalar values
  //  (to be read and written via call to the Data*Streams in_stream
  //  and out_stream declared below).

  private static Prim_Input_Stream  in_bytes  = new Prim_Input_Stream ();
  private static Prim_Output_Stream out_bytes = new Prim_Output_Stream ();

  //  The DataInputStream used to implement conversions from byte sequences
  //  saved in in_bytes to scalar values of various types.

  private static System.IO.BinaryReader in_stream
    = new System.IO.BinaryReader (in_bytes);

  //  The DataOutputStream used to implement conversions from scalar values
  //  of various types to byte sequences that are retrieved from out_bytes.

  private static System.IO.BinaryWriter out_stream
    = new System.IO.BinaryWriter (out_bytes);

  //  Converts an unsigned byte to an array of one byte

  public static byte [] From_B (byte b) {
     out_bytes.Flush ();
     out_stream.Write (b); 
     return out_bytes.stream_bytes ();
  }

  public static byte [] From_B (SByte b) {
     out_bytes.Flush ();
     out_stream.Write (b); 
     return out_bytes.stream_bytes ();
  }

  //  Converts an 8-bit character to an array of one byte

  public static byte [] From_C (byte c) {
     out_bytes.Flush ();
     out_stream.Write (c); 
     return out_bytes.stream_bytes ();
  }

  //  Converts a 16-bit Wide_Character to an array of two bytes

  public static byte [] From_WC (char w) {
     out_bytes.Flush ();
     out_stream.Write (w); 
     return out_bytes.stream_bytes ();
  }

  //  Converts a 16-bit integer to an array of two bytes

  public static byte [] From_SI (short s) {
     out_bytes.Flush ();
     out_stream.Write (s); 
     return out_bytes.stream_bytes ();
  }

  //  Converts a 32-bit integer to an array of four bytes

  public static byte [] From_I (int i) {
     out_bytes.Flush ();
     out_stream.Write (i); 
     return out_bytes.stream_bytes ();
  }

  public static byte [] From_I (UInt32 i) {
     out_bytes.Flush ();
     out_stream.Write (i); 
     return out_bytes.stream_bytes ();
  }

  //  Converts a 64-bit integer to an array of eight bytes

  public static byte [] From_LI (long l) {
     out_bytes.Flush ();
     out_stream.Write (l); 
     return out_bytes.stream_bytes ();
  }

  public static byte [] From_LI (ulong l) {
     out_bytes.Flush ();
     out_stream.Write (l); 
     return out_bytes.stream_bytes ();
  }

  //  Converts a 32-bit floating-point value to an array of four bytes
  public static byte [] From_F (float f) {
     out_bytes.Flush ();
     out_stream.Write (f); 
     return out_bytes.stream_bytes ();
  }

  //  Converts a 64-bit floating-point value to an array of eight bytes

  public static byte [] From_LF (double d) {
     out_bytes.Flush ();
     out_stream.Write (d); 
     return out_bytes.stream_bytes ();
  }

  //  Converts an array of one byte to an unsigned byte

  public static SByte To_B (byte [] bytes) {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);

     return in_stream.ReadSByte (); 
  }

  public static byte To_BU (byte [] bytes) {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);

     return in_stream.ReadByte (); 
  }

  //  Converts an array of one byte to an 8-bit character

  public static byte To_C (byte [] bytes) {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);

     return in_stream.ReadByte (); 
  }

  //  Converts an array of two bytes to a 16-bit Wide_Character

  public static char To_WC (byte [] bytes) {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);
     return in_stream.ReadChar (); 
  }

  //  Converts an array of two bytes to a 16-bit integer

  public static short To_SI (byte [] bytes) {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);

     return in_stream.ReadInt16 (); 
  }

  public static ushort To_SU (byte [] bytes) {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);

     return in_stream.ReadUInt16 (); 
  }

  //  Converts an array of four bytes to a 32-bit integer

  public static int To_I (byte [] bytes) {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);

     return in_stream.ReadInt32 (); 
  }

  public static uint To_U (byte [] bytes) {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);

     return in_stream.ReadUInt32 (); 
  }

  //  Converts an array of eight bytes to a 64-bit integer

  public static long To_LI (byte [] bytes) {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);
     return in_stream.ReadInt64 (); 
  }

  public static ulong To_LU (byte [] bytes) {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);
     return in_stream.ReadUInt64 (); 
  }

  //  Converts an array of four bytes to a 32-bit floating-point value
  public static float To_F (byte [] bytes) {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);
     return (float) in_stream.ReadSingle (); 
  }

  //  Converts an array of eight bytes to a 64-bit floating-point value

  public static double To_LF (byte [] bytes) {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);
     return in_stream.ReadDouble (); 
  }

}
}
