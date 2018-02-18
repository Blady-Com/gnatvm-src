//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                            G N A T _ l i b c                             //
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
// JGNAT - The GNAT Ada 95 toolchain for the Java Virtual Machine is        //
//         maintained by Ada Core Technologies, Inc. - http://www.gnat.com  //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

//  This Java class is part of the JGNAT library.

//  This class implements the global exception handler used by Ada executables
//  to report unhandled exceptions.

package jgnat.adalib;

class __gnat_Exception_Handler implements Thread.UncaughtExceptionHandler
{
   public void uncaughtException(Thread t, Throwable e)
   {
      //  Get class name, removing the "class " prefix
      String name = e.getClass().toString().toUpperCase().substring (6);
      if (name.startsWith ("JGNAT.ADALIB."))
      {
         name = name.substring (13);
      }

      System.err.println("");
      System.err.println(name + " " + e.getMessage());

      if (GNAT_libc.__gl_exception_tracebacks == 1) e.printStackTrace();
   }
}
