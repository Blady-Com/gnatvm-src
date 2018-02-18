//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//         s t a n d a r d $ a d a _ w i d e _ w i d e _ s t r i n g        //
//                                                                          //
//                     Copyright (C) 2009-2009, AdaCore                     //
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
//////////////////////////////////////////////////////////////////////////////

// This C# file is part of the GNAT for .NET library and corresponds to type
// Wide_Wide_String declared in Ada's package Standard. We need to produce a
// class file for it since Standard is not actually compiled at the source
// level. It must be marked Serializeable because of the way Ada.Sequential_IO
// and Ada.Direct_IO are implemented. See the body of these packages for more
// information.

using System;
namespace mgnat.adalib.standard {
#if !COMPACT
[Serializable]
#endif
public class ada_wide_wide_string {
  public int [] all;
  public int first;
  public int last;
}
}
