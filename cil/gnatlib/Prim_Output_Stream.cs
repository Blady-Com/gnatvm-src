//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                   P r i m _ O u t p u t _ S t r e a m                    //
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
//  conversion of scalar values to byte sequences via DataOutputStreams.

using System.IO;
using System;

namespace mgnat.adalib {

public class Prim_Output_Stream : System.IO.Stream {
  //  The buffer used for holding bytes to be obtained from
  //  scalar values via calls from DataOutputStream

  private byte [] bytes = new byte [100];

  //  Current index of the next byte to be written to the buffer

  private int bytes_index = 0;

  public void Write (byte b) {
     //  Mask the int to get the lowest-order byte

     bytes [bytes_index] = (byte) (b & 255);
     bytes_index += 1;
  }

  public void Write (byte [] b)
  {
     Write (b, 0, b.Length);
  }

  public override bool CanRead 
  {
      get {return false;}
          
  }
  public override bool CanSeek 
  {
      get {return false;}
          
  }
  public override long Length 
  {
      get {return 0;}
          
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
      get {return true;}
          
  }
  public override int Read (byte [] b, int off, int len)
  {
     return 0; //not allowed
  }

  public override void Write (byte [] b, int off, int len)
  {
     System.Array.Copy (b, off, bytes, bytes_index, len);
     bytes_index += len;
  }

  public override void Flush ()  {
     bytes_index = 0;
  }

  public override void Close () {
  }

  //  Returns a copy of the first 'bytes_index' bytes in the buffer

  public byte [] stream_bytes () {
     byte [] result = new byte [bytes_index];

     System.Array.Copy (bytes, 0, result, 0, bytes_index);

     return result;
  }

}
}  //namespace
