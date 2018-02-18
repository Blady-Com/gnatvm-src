//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                    P r i m _ I n p u t _ S t r e a m                     //
//                                                                          //
//                     Copyright (C) 1998-2006, AdaCore                     //
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
//  support for stream attributes of scalar types. It supports unchecked
//  conversion of byte sequences to scalar values via DataInputStreams.
using System.IO;
using System;

namespace mgnat.adalib {

public class Prim_Input_Stream : System.IO.Stream {

  //  The buffer used for holding bytes to be converted to
  //  scalar values via calls from DataInputStream

  private byte [] bytes = new byte [100];

  //  Current index of the next byte to be read from the buffer

  private int bytes_index = 0;

  //  A count of the bytes available to be read

  private int bytes_avail = 0;

  public override int ReadByte () {
     if (bytes_index == bytes.Length) {
        return -1;
     }

     bytes_index += 1;
     bytes_avail -= 1;

     //  Mask the byte to prevent returning a sign-extended int

     return 255 & bytes [bytes_index - 1];
  }

  public int Read (byte [] b)
  {
     return Read (b, 0, b.Length);
  }

  public override int Read (byte [] b, int off, int len)
  {
     System.Array.Copy (bytes, bytes_index, b, off, len);

     bytes_index += len;
     bytes_avail -= len;

     return len;
  }

  public override void Write (byte [] b, int off, int len)
  {
     // not allowed -- ignore
  }

  public long skip (long n)
  {
     if (bytes_index + n > bytes.Length) {
        throw new System.SystemException ("skip in Prim_Input_Stream");
     }

     bytes_index += (int) n;

     return n;
  }

  public override bool CanRead 
  {
      get {return true;}
          
  }
  public override bool CanSeek 
  {
      get {return false;}
          
  }
  public override long Length 
  {
      get {return (long) available();}
          
  }
  public override void SetLength(long length) {
      // not allowed
  }
  public override long Position 
  {
      get {return (long) bytes_index;}
      set {bytes_index = (int) value;}
          
  }
  public override long Seek(long l, System.IO.SeekOrigin s) {
     // not allowed
     return 0;
  }

  public override bool CanWrite 
  {
      get {return false;}
          
  }
  public override void Flush() {
      this.reset();
  }

  public int available ()  {
     return bytes_avail;
  }

  public override void Close ()  {
  }

  public void mark (int readlimit) {
     //  Not yet implemented ???
  }

  public void reset () {
     bytes_index = 0;
     bytes_avail = 0;
  }

  public int stream_index () {
     return bytes_index;
  }

  public void set_stream_index (int index)
  {
     if (index > bytes.Length - 1) {
        throw new System.SystemException ("set_stream_index in Prim_Input_Stream");
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
     System.Array.Copy (b, 0, bytes, bytes_index, b.Length);

     bytes_avail = b.Length;
  }

}
}
