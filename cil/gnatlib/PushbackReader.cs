//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                       P u s h b a c k R e a d e r                        //
//                                                                          //
//                     Copyright (C) 2004-2006, AdaCore                     //
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
 
using System.IO;
using System;

namespace mgnat.adalib {
public class PushbackReader  {
   TextReader reader;
   bool have_unread = false;
   int unread_value;

   public PushbackReader(TextReader t) {
      this.reader = t;
   }
   
   public void Close () {
      this.reader.Close();
   }
   public int Read () {
      int x;
      if (have_unread) {
         have_unread = false;
         return unread_value;
      }
      else {
         x = this.reader.Read();
         // Ada doesn't expect the CR before the LF
         if (x==13) {
            x = this.Read();
         }
         return x;
      }
   }
   public void unread(int x) {
      have_unread = true;
      unread_value = x;
   }
   public bool ready () {
      return this.reader.Peek() != -1;
   }
   public void rewind() {
      ((StreamReader) this.reader).BaseStream.Seek(0,SeekOrigin.Begin);
   }

}
}
