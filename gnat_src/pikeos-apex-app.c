/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                      P I K E O S - A P E X - A P P                       *
 *                                                                          *
 *          Copyright (C) 2009-2011, Free Software Foundation, Inc.         *
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

#include <apex_config.h>

#ifndef STACK_SIZE
#define STACK_SIZE 0x4000
#endif

/* Stack declaration, using global APEX configuration */
apex_config_t apex_config = {
        STACK_SIZE /* stack size for MAIN() */
};

extern void main (void);

void exit (int status) __attribute__((noreturn));
void abort (void) __attribute__((noreturn));

void exit (int status)
{
  STOP_SELF();
}

void abort (void)
{
  exit (1);
}

void MAIN(void)
{
#ifdef DEBUG
  /* Break before main, so that the debugger can be attached */
  gdb_breakpoint ();
#endif

  /* Run the Ada application.  */
  main ();

  exit (0);
}
