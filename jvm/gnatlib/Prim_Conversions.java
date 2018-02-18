//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                P r i m i t i v e _ C o n v e r s i o n s                 //
//                                                                          //
//                     Copyright (C) 2000-2005, AdaCore                     //
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
// JGNAT - The GNAT Ada 95 toolchain for the Java Virtual Machine is        //
//         maintained by Ada Core Technologies, Inc. - http://www.gnat.com  //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

//  This Java class is part of the JGNAT library and provides low-level
//  conversions between primitive types and byte arrays for the purpose
//  of supporting the implementation of the predefined stream attributes
//  (see 5jstratt.adb). It uses Data streams attached to the JGNAT
//  utility classes Prim_Input_Stream and Prim_Output_Stream to effect
//  these conversions.

package jgnat.adalib;

import java.io.DataInputStream;
import java.io.DataOutputStream;

import jgnat.adalib.Prim_Input_Stream;
import jgnat.adalib.Prim_Output_Stream;

public class Prim_Conversions {

  //  The following streams provide low-level facilities for storing
  //  and retrieving byte sequences representating scalar values
  //  (to be read and written via call to the Data*Streams in_stream
  //  and out_stream declared below).

  private static Prim_Input_Stream  in_bytes  = new Prim_Input_Stream ();
  private static Prim_Output_Stream out_bytes = new Prim_Output_Stream ();

  //  The DataInputStream used to implement conversions from byte sequences
  //  saved in in_bytes to scalar values of various types.

  private static java.io.DataInputStream in_stream
    = new java.io.DataInputStream (in_bytes);

  //  The DataOutputStream used to implement conversions from scalar values
  //  of various types to byte sequences that are retrieved from out_bytes.

  private static java.io.DataOutputStream out_stream
    = new java.io.DataOutputStream (out_bytes);

  //  Converts an unsigned byte to an array of one byte

  public static byte [] From_B (byte b) throws java.io.IOException {
     out_bytes.flush ();
     out_stream.writeByte (b);
     return out_bytes.stream_bytes ();
  }

  //  Converts an 8-bit character to an array of one byte

  public static byte [] From_C (byte c) throws java.io.IOException {
     out_bytes.flush ();
     out_stream.writeByte (c);
     return out_bytes.stream_bytes ();
  }

  //  Converts a 16-bit Wide_Character to an array of two bytes

  public static byte [] From_WC (char w) throws java.io.IOException {
     out_bytes.flush ();
     out_stream.writeChar (w);
     return out_bytes.stream_bytes ();
  }

  //  Converts a 16-bit integer to an array of two bytes

  public static byte [] From_SI (short s) throws java.io.IOException {
     out_bytes.flush ();
     out_stream.writeShort (s);
     return out_bytes.stream_bytes ();
  }

  //  Converts a 32-bit integer to an array of four bytes

  public static byte [] From_I (int i) throws java.io.IOException {
     out_bytes.flush ();
     out_stream.writeInt (i);
     return out_bytes.stream_bytes ();
  }

  //  Converts a 64-bit integer to an array of eight bytes

  public static byte [] From_LI (long l) throws java.io.IOException {
     out_bytes.flush ();
     out_stream.writeLong (l);
     return out_bytes.stream_bytes ();
  }

  //  Converts a 32-bit floating-point value to an array of four bytes

  public static byte [] From_F (float f) throws java.io.IOException {
     out_bytes.flush ();
     out_stream.writeFloat (f);
     return out_bytes.stream_bytes ();
  }

  //  Converts a 64-bit floating-point value to an array of eight bytes

  public static byte [] From_LF (double d) throws java.io.IOException {
     out_bytes.flush ();
     out_stream.writeDouble (d);
     return out_bytes.stream_bytes ();
  }

  //  Converts an array of one byte to an unsigned byte

  public static byte To_B (byte [] bytes) throws java.io.IOException {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);

     return in_stream.readByte ();
  }

  //  Converts an array of one byte to an 8-bit character

  public static byte To_C (byte [] bytes) throws java.io.IOException {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);

     return in_stream.readByte ();
  }

  //  Converts an array of two bytes to a 16-bit Wide_Character

  public char To_WC (byte [] bytes) throws java.io.IOException {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);
     return in_stream.readChar ();
  }

  //  Converts an array of two bytes to a 16-bit integer

  public static short To_SI (byte [] bytes) throws java.io.IOException {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);

     return in_stream.readShort ();
  }

  //  Converts an array of four bytes to a 32-bit integer

  public static int To_I (byte [] bytes) throws java.io.IOException {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);

     return in_stream.readInt ();
  }

  //  Converts an array of eight bytes to a 64-bit integer

  public static long To_LI (byte [] bytes) throws java.io.IOException {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);
     return in_stream.readLong ();
  }

  //  Converts an array of four bytes to a 32-bit floating-point value

  public static float To_F (byte [] bytes) throws java.io.IOException {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);
     return in_stream.readFloat ();
  }

  //  Converts an array of eight bytes to a 64-bit floating-point value

  public static double To_LF (byte [] bytes) throws java.io.IOException {
     in_bytes.reset ();
     in_bytes.set_stream_bytes (bytes);
     return in_stream.readDouble ();
  }

}

