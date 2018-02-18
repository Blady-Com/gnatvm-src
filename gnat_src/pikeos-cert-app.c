/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                            P I K E O S - A P P                           *
 *                                                                          *
 *          Copyright (C) 2009-2012, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 *                                                                          *
 *                                                                          * 
 *                                                                          * 
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This file provides stack declaration and startup code.  */
/* The user is free to recompile this file to fit his needs.  Only 2 macros
   may be modified:

   DEBUG (not defined by default)
     If defined, the application will include a gdb stub and stop before
     initialization.

   STACK_SIZE (0x4000 by default)
     Define the stack size of the environment task.
*/

#include <vm.h>
#ifdef DEBUG
#include <vm_debug.h>
#endif

#ifndef STACK_SIZE
#define STACK_SIZE 0x4000
#endif

/* Stack declaration.  */
VM_DECLARE_STACK(STACK_SIZE)

extern void main (void);

void exit (int status) __attribute__((noreturn));
void abort (void) __attribute__((noreturn));

void exit (int status)
{
  while (1)
    vm_shutdown (VM_RESPART_MYSELF);
}

void abort (void)
{
  exit (1);
}

void _p4_entry(void)
{
  /* Initialize the system software.  */
  vm_init ();

#ifdef DEBUG
  /* Initialize the gdb stub.  */
  init_gdbstub ("muxa:/%s/%s/dbg");
  gdb_breakpoint ();
#endif

  /* Run the Ada application.  */
  main ();

  exit (0);
}
