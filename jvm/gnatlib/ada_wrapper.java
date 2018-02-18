//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                          a d a _ w r a p p e r                           //
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

// This Java class is part of the JGNAT library and is used by the JVM
// implementation of GNULLI (i.e. to implement Ada Tasking on the JVM).

package jgnat.adalib;

import java.lang.Thread;

public final class ada_wrapper extends Thread {
   private Object self_id;

   public ada_wrapper (Object self_id) {
      this.self_id = self_id;
   }

   public void run () {
      system$tasking$stages.task_wrapper
        ((system$tasking$ada_task_control_block) self_id);
   }

   public static Object getSelfId ()
   {
      Object t;

      try {
         return ((ada_wrapper) currentThread()).self_id;
      }

      // If the above cast fails, then we must be in the environment task
      // (or a foreign thread...).
      catch (ClassCastException e) {
         return (Object) system$task_primitives$operations.environment_task_id;
      }
   }
}

