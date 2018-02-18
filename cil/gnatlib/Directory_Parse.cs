//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                      D i r e c t o r y _ P a r s e                       //
//                                                                          //
//                     Copyright (C) 2004-2008, AdaCore                     //
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

public class Directory_Parse {

  public String[] filenames;

  public bool is_open;
  private int current;

  public bool get_is_open() {
     return this.is_open;
  }

  public Directory_Parse (String dirname)
  {
     System.IO.DirectoryInfo dirinfo = new System.IO.DirectoryInfo (dirname);
     System.IO.FileSystemInfo[] FSInfo = dirinfo.GetFileSystemInfos();

     // .NET framework does not return "." and ".." directories. Let's emulate
     // the behavior.

     int added_dirs = 1; // always add "."
     if (dirinfo.Parent != null) added_dirs = 2; // also add ".."

     this.filenames = new String[FSInfo.Length + added_dirs];

     this.filenames[0] = ".";
     if (added_dirs == 2) this.filenames[1] = "..";

     for (int i = 0; i < FSInfo.Length; i++)
       filenames[i + added_dirs] = FSInfo[i].Name;

     this.current = 0;
     this.is_open = true;
  }

  public String get_next()
  {
     if (this.current>=this.filenames.Length) {
        return "";
     }
     else
     {
        return System.IO.Path.GetFileName (this.filenames[this.current++]);
     }
  }

  //  Closes the file of objects, writing out the sequence of objects
  //  contained in the file_elements Vector in the case of an output
  //  file.
  public void close () {
     this.is_open = false;
  }
}
}
