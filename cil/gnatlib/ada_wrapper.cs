//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                          a d a _ w r a p p e r                           //
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

// This C# file is part of the GNAT for .NET library and is by the .NET
// implementation of GNULLI (i.e. to implement Ada Tasking on .NET).

using System;
using System.Threading;
namespace mgnat.adalib {

public delegate void TaskWrapper (object self_id);

public sealed class ada_wrapper {
  public Thread thd;
  private Object self_id;
  private TaskWrapper wrapper;
#if !COMPACT || SILVERLIGHT
  [ThreadStatic]
  static private Object thd_self_id;
#else
  static LocalDataStoreSlot data_store = Thread.AllocateDataSlot();
#endif

  public ada_wrapper (Object myself, TaskWrapper wrapper) {
    this.self_id = myself;
    this.wrapper = wrapper;
    this.thd = new Thread (new ThreadStart(this.run));
  }

   public void run () {
#if !COMPACT || SILVERLIGHT
      // now that the task is started, we can set thd_self_id
      thd_self_id = this.self_id;
#else
      Thread.SetData (data_store, this.self_id);
#endif
      this.wrapper (this.self_id);
   }

   public Thread Get_Thread() {
      return this.thd;
   }

   public static Object getSelfId ()
   {
#if !COMPACT || SILVERLIGHT
      return thd_self_id;
#else
      Object data;
      data = Thread.GetData (data_store);
      return data;
#endif
   }
}

}
