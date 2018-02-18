//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                           M o n i t o r E x                              //
//                                                                          //
//                     Copyright (C) 2004-2009, AdaCore                     //
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

using System;
using System.Threading;
using System.Collections.Generic;

namespace mgnat.adalib
{
  // Addendum to the .Net compact framework Monitor class

  public static class MonitorEx
  {
    //  ??? We don't call Monitor directly as Mono seems to absolutely want to
    //  inline those calls (thus they won't work with il, where we don't)
    public static void Enter (object currentObj)
    {
      System.Threading.Monitor.Enter (currentObj);
    }
    public static void Exit (object currentObj)
    {
      System.Threading.Monitor.Exit (currentObj);
    }

#if COMPACT

    private class ThreadInfo
    {
      public object         threadObj;
      public object         stateLock;
      public int            lockCount;
      public AutoResetEvent waitPulseEvent;
      public int            waitCounter;
    }

    static List<ThreadInfo> threadList = new List<ThreadInfo> ();

    private static ThreadInfo Find (object currentObj)
    {
      foreach (ThreadInfo obj in threadList)
        if (obj.threadObj.Equals (currentObj))
          return obj;
      return null;
    }

    /// Pulses the monitor once - a single waiting thread will be released
    /// and continue its execution after the current thread has exited the
    /// monitor. Unlike Pulse on the normal framework, no guarantee is
    /// made about which thread is woken.
    /// <exception cref="SynchronizationLockException">If the
    /// current thread does not own the monitor.</exception>
    public static void Pulse (object currentObj)
    {
      ThreadInfo cur = Find (currentObj);

      if (cur == null)
      {
        throw new
          SynchronizationLockException
            ("Thread not found (no wait issued).");
      }

      lock (cur.stateLock)
      {
        // Don't bother setting the event if no-one's waiting - we'd only end
        // up having to reset the event manually.
        if (cur.waitCounter == 0) return;

        cur.waitPulseEvent.Set ();
        cur.waitCounter -= 1;
      }
    }

    // ??? PulseAll is unused. I keep the code just in case, but this can
    // safely be deleted

    /// <summary>
    /// Pulses the monitor such that all waiting threads are woken up.
    /// All threads will then try to regain the lock on this monitor.
    /// No order for regaining the lock is specified.
    /// </summary>
    /// <exception cref="SynchronizationLockException">If the current
    /// thread does not own the monitor.</exception>
    // public static void PulseAll (object currentObj)
    // {
    //   ThreadInfo cur = Find (currentObj);
    //
    //   if (cur == null)
    //   {
    //     throw new
    //       SynchronizationLockException
    //         ("Thread not found (no wait issued).");
    //   }
    //
    //   lock (cur.stateLock)
    //   {
    //     for (int i = 0; i < cur.waitCounter; i++)
    //       cur.waitPulseEvent.Set ();
    //     cur.waitCounter = 0;
    //   }
    // }

    /// <summary>
    /// Relinquishes the lock on this monitor (whatever the lock count is)
    /// and waits for the monitor to be pulsed. After the monitor has been
    /// pulsed, the thread blocks again until it has regained the lock (at
    /// which point it will have the same lock count as it had before), and
    /// then the method returns.
    /// </summary>
    public static Boolean Wait (object currentObj)
    {
      int oldLockCount;

      ThreadInfo cur = Find (currentObj);

      if (cur == null)
      {
        cur = new ThreadInfo ();
        cur.threadObj = currentObj;
        cur.stateLock = new object ();
        cur.lockCount = 0;
        cur.waitPulseEvent = new AutoResetEvent (false);
        cur.waitCounter = 0;

        threadList.Add (cur);
      }

      lock (cur.stateLock)
      {
        oldLockCount = cur.lockCount;
        // Make Exit() set the enterExitEvent
        cur.lockCount = 1;
        Monitor.Exit (cur.threadObj);
        cur.waitCounter += 1;
      }

      cur.waitPulseEvent.WaitOne ();
      Monitor.Enter (cur.threadObj);
      // By now we own the lock again
      lock (cur.stateLock)
      {
        cur.lockCount = oldLockCount;
      }

      return true;
    }

    class PulseTimer
    {
      // This method is called by the timer delegate.
      public void timedPulse (Object threadObj)
      {
        Pulse (threadObj);
      }
    }


    public static Boolean Wait (object currentObj, int timeout)
    {
      PulseTimer pulseTimer = new PulseTimer ();

      TimerCallback timerDelegate =
        new TimerCallback (pulseTimer.timedPulse);

      // setup a timer to call the TimerCallback object
      System.Threading.Timer waitPulseTimer = new System.Threading.Timer
       (timerDelegate,
        currentObj,
        timeout,
        System.Threading.Timeout.Infinite);

      return Wait (currentObj);
    }
#else
    public static void Pulse (object currentObj)
    {
      System.Threading.Monitor.Pulse (currentObj);
    }

    public static bool Wait (object currentObj)
    {
      return System.Threading.Monitor.Wait (currentObj);
    }

    public static bool Wait (object currentObj, int timeout)
    {
      return System.Threading.Monitor.Wait (currentObj, timeout);
    }
#endif

  }

  /// <summary>
  /// Exception thrown by MonitorEx when threading rules are violated (usually
  /// due to an operation being invoked on a monitor not owned by the current
  /// thread).
  /// </summary>
  public class SynchronizationLockException : SystemException
  {
    public SynchronizationLockException (string message) : base (message)
    {
    }
  }
}
