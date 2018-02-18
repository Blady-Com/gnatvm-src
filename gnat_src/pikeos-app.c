/****************************************************************************
 *                                                                          *
 *                         GNAT RUN-TIME COMPONENTS                         *
 *                                                                          *
 *                            P I K E O S - A P P                           *
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

   CHECK_MCP (1 by default)
     If not 0, MCP (Maximum Controlled Priority) will be checked at
     initialization.
*/

#include <vm.h>
#ifdef DEBUG
#include <vm_debug.h>
#endif

#ifndef STACK_SIZE
#define STACK_SIZE 0x4000
#endif

#ifndef CHECK_MCP
#define CHECK_MCP 1
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
  P4_prio_t mcp;

  /* Initialize the system software.  */
  vm_init ();

#ifdef DEBUG
  /* Initialize the gdb stub.  */
  init_gdbstub ("muxa:/%s/%s/dbg");
  gdb_breakpoint ();
#endif

  /* By default, the initial priority of the initial thread is MCP - which
     breaks Ravenscar implementation.
     Lower priority if the tasking RTS is not initialized.  */
  mcp = p4_fast_set_prio (112);
#if CHECK_MCP
  if (mcp < 240)
    {
      static const char mcp_msg[] = "MCP too low";
      vm_hm_raise_error (VM_HM_EI_APP_ERROR,
			 VM_HM_ERR_MSG_T_CUSTOM,
			 mcp_msg, sizeof (mcp_msg) - 1);
    }
#endif /* CHECK_MCP */

  /* Run the Ada application.  */
  main ();

  exit (0);
}
