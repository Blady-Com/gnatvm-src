//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                    a d a . i o _ e x c e p t i o n s                     //
//                                                                          //
//                      Copyright (C) 2009, AdaCore                         //
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

// We declare those exceptions here so that they are accessible from the
// C# part of the GNAT run-time. The corresponding Ada spec is now only
// a binding to this package.
namespace ada.io_exceptions
{
  public class data_error : System.SystemException
  {
    public data_error () : base () {}
    public data_error (string msg) : base (msg) {}
  }
  public class device_error : System.SystemException
  {
    public device_error () : base () {}
    public device_error (string msg) : base (msg) {}
  }
  public class end_error : System.SystemException
  {
    public end_error () : base () {}
    public end_error (string msg) : base (msg) {}
  }
  public class layout_error : System.SystemException
  {
    public layout_error () : base () {}
    public layout_error (string msg) : base (msg) {}
  }
  public class mode_error : System.SystemException
  {
    public mode_error () : base () {}
    public mode_error (string msg) : base (msg) {}
  }
  public class name_error : System.SystemException
  {
    public name_error () : base () {}
    public name_error (string msg) : base (msg) {}
  }
  public class status_error : System.SystemException
  {
    public status_error () : base () {}
    public status_error (string msg) : base (msg) {}
  }
  public class use_error : System.SystemException
  {
    public use_error () : base () {}
    public use_error (string msg) : base (msg) {}
  }
}
