//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                        _ a b o r t _ s i g n a l                         //
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

// This C# file is part of the GNAT for .NET library and corresponds to the
// GNAT special exception _abort_signal used by the GNARL.

// Unlike the other predefined Ada exceptions, _abort_signal does not
// extend SystemException since we do *not* want to catch this in a
// "when others" clause. The right parent class of _abort_signal is
// Exception.

using System;

namespace mgnat.adalib {

public class _abort_signal : Exception {
    public _abort_signal ()         {  }
    public _abort_signal (String s) {  }
}
}

