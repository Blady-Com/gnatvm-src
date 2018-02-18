//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                        _ a b o r t _ s i g n a l                         //
//                                                                          //
//                     Copyright (C) 1998-2005, AdaCore                     //
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

// This Java class is part of the JGNAT library and corresponds to the GNAT
// special exception _abort_signal used by the GNARL.

// Unlike the other predefined Ada exceptions, _abort_signal does not
// extend RuntimeException since we do *not* want to catch this in a
// "when others" clause. The right parent class of _abort_signal is
// Error, since Error is a subclass of Throwable that indicates
// serious problems that a reasonable application should not try to
// catch. This is precisely the intent of _abort_signal which should
// only be caught by GNARL.

package jgnat.adalib;

public class _abort_signal extends Error {
    public _abort_signal ()         { super (); }
    public _abort_signal (String s) { super (s); }
}

