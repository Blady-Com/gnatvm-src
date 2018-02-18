//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                       S t d _ e x c e p t i o n s                        //
//                                                                          //
//                     Copyright (C) 1998-2009, AdaCore                     //
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

// This C# file is part of the GNAT for .NET library and corresponds to the Ada
// exception Storage_Error.

using System;
namespace mgnat.adalib {
  public class constraint_error : System.SystemException
  {
    public constraint_error () : base () {}
    public constraint_error (string msg) : base (msg) {}
  }
  public class program_error : System.SystemException
  {
    public program_error () : base () {}
    public program_error (string msg) : base (msg) {}
  }
  public class storage_error : System.SystemException
  {
    public storage_error () : base () {}
    public storage_error (string msg) : base (msg) {}
  }
  public class tasking_error : System.SystemException
  {
    public tasking_error () : base () {}
    public tasking_error (string msg) : base (msg) {}
  }
}
