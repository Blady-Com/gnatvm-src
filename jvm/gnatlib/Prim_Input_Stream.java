//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                    P r i m _ I n p u t _ S t r e a m                     //
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
//  support for stream attributes of scalar types. It supports unchecked
//  conversion of byte sequences to scalar values via DataInputStreams.

package jgnat.adalib;

public class Prim_Input_Stream extends java.io.InputStream {

  //  The buffer used for holding bytes to be converted to
  //  scalar values via calls from DataInputStream

  private byte [] bytes = new byte [100];

  //  Current index of the next byte to be read from the buffer

  private int bytes_index = 0;

  //  A count of the bytes available to be read

  private int bytes_avail = 0;

  public int read () {
     if (bytes_index == bytes.length) {
        return -1;
     }

     bytes_index += 1;
     bytes_avail -= 1;

     //  Mask the byte to prevent returning a sign-extended int

     return 255 & bytes [bytes_index - 1];
  }

  public int read (byte [] b)
     throws java.io.IOException, NullPointerException
  {
     return read (b, 0, b.length);
  }

  public int read (byte [] b, int off, int len)
     throws java.io.IOException, NullPointerException,
       IndexOutOfBoundsException
  {
     System.arraycopy (bytes, bytes_index, b, off, len);

     bytes_index += len;
     bytes_avail -= len;

     return len;
  }

  public long skip (long n)
     throws java.io.EOFException
  {
     if (bytes_index + n > bytes.length) {
        throw new java.lang.Error ();
     }

     bytes_index += n;

     return n;
  }

  public int available () throws java.io.IOException {
     return bytes_avail;
  }

  public void close () throws java.io.IOException {
  }

  public void mark (int readlimit) {
     //  Not yet implemented ???
  }

  public void reset () throws java.io.IOException {
     bytes_index = 0;
     bytes_avail = 0;
  }

  public int stream_index () {
     return bytes_index;
  }

  public void set_stream_index (int index)
     throws java.io.IOException
  {
     if (index > bytes.length - 1) {
        throw new java.lang.Error ();
     }

     bytes_index = index;
  }

  //  This method will fill the byte stream buffer with
  //  the contents of the given byte array. Note that it
  //  does not advance the buffer index, since it's intended
  //  that the caller wants to read back the contents just
  //  written into the buffer (via calls to the DataInputStream
  //  class). If the caller wants to advance the buffer
  //  index, that must be done via an explicit call to
  //  set_stream_index.

  public void set_stream_bytes (byte [] b) {
     System.arraycopy (b, 0, bytes, bytes_index, b.length);

     bytes_avail = b.length;
  }

}

