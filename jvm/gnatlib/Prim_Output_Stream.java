//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                   P r i m _ O u t p u t _ S t r e a m                    //
//                                                                          //
//                     Copyright (C) 2000-2005 AdaCore                      //
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
//  support for stream attributes of scalar types. It supports unchecked
//  conversion of scalar values to byte sequences via DataOutputStreams.

package jgnat.adalib;

public class Prim_Output_Stream extends java.io.OutputStream {

  //  The buffer used for holding bytes to be obtained from
  //  scalar values via calls from DataOutputStream

  private byte [] bytes = new byte [100];

  //  Current index of the next byte to be written to the buffer

  private int bytes_index = 0;

  public void write (int b) {
     //  Mask the int to get the lowest-order byte

     bytes [bytes_index] = (byte) (b & 255);
     bytes_index += 1;
  }

  public void write (byte [] b)
     throws java.io.IOException, NullPointerException
  {
     write (b, 0, b.length);
  }

  public void write (byte [] b, int off, int len)
     throws java.io.IOException, NullPointerException,
       IndexOutOfBoundsException
  {
     System.arraycopy (b, off, bytes, bytes_index, len);
     bytes_index += len;
  }

  public void flush () throws java.io.IOException {
     bytes_index = 0;
  }

  public void close () throws java.io.IOException {
  }

  //  Returns a copy of the first 'bytes_index' bytes in the buffer

  public byte [] stream_bytes () {
     byte [] result = new byte [bytes_index];

     System.arraycopy (bytes, 0, result, 0, bytes_index);

     return result;
  }

}

