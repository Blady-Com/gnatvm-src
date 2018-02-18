/****************************************************************************
 *                                                                          *
 *                GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                  *
 *                                                                          *
 *                          T R A P _ H A N D L E R                         *
 *                                                                          *
 *                               Assembly File                              *
 *                                                                          *
 *      Copyright (C) 1999-2002 Universidad Politecnica de Madrid           *
 *             Copyright (C) 2003-2005 The European Space Agency            *
 *                    Copyright (C) 2003-2013 AdaCore                       *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNARL was developed by the GNARL team at Florida State University.       *
 * Extensive contributions were provided by Ada Core Technologies, Inc.     *
 * The  executive  was developed  by the  Real-Time  Systems  Group  at the *
 * Technical University of Madrid.                                          *
 *                                                                          *
 ****************************************************************************/

/* This file is a SPARC specific implementation */

#include "sparc.h"

#ifdef XTRATUM
/* Include the para-virtualized hypervisor services */
#include <xm.h>
#endif

/*
   This is the service routine shared by all user handlers (for synchronous
   and asynchronous traps). It prepares the machine for safe execution of
   handlers by means of saving the state of the interrupted task, setting up
   the register windows, and enabling traps among other things.
   We enter this handler from the four instructions loaded in the trap table
   by the install handler routines. Apart from jumping to the user handler,
   these four instructions move the value of the PSR to l0 and the trap
   type (plus bit number 8 set to 1 for synchronous traps) to l3. When
   acknowledging a trap, the SPARC processor automatically copies the
   interrupted PC and nPC into l1 and l2 respectively. Hence, when starting
   the common handler the following values are in the local registers:
	 l0 <- PSR (when the trap handler started)
	 l1 <- PC (trapped pc)
	 l2 <- nPC
	 l3 <- Trap Type (TT) + synchronous bit
  */

	.text
	.align 4
	.global common_handler
	.type   common_handler,@function
common_handler:
#ifdef XTRATUM
	/* XtratuM needs to first get the PSR before it gets corrupted. For example,
	   the ICC may be modified in this handler (as it actually happens later when
	   checking whether the trap is synchronous or asynchronous). If we have an
	   interrupt happening between two instructions like:
	      cmp %l0, 10
	      bge somewhere
	   the corruption of the ICC has a debastating effect. */

	set     sparc_get_psr_nr, %o0			! get psr
	__XM_AHC
	mov %o0, %l0					! put psr in l0
#endif
	
       /* Adjust return address for synchronous traps. The procedure which
          installs the trap handler will force bit 8 to 1 in case of
          synchronous traps (those that want to return to the instruction
	  following the trap in order to prevent the trap from happening
          again and again). */

	andcc	%l3, SYNCHRONOUS_TRAP_BIT_MASK, %g0	! test if synchronous trap
	be	.Lcheck_window_ov			! if not, skip adjustment
	 andn	 %l3, SYNCHRONOUS_TRAP_BIT_MASK, %l3	! remove sync info from tt
	mov	%l2, %l1				! return to the next instruction:
	add	%l2, 4, %l2				! PC <- nPC, nPC <- nPC + 4

.Lcheck_window_ov:
	mov	%g4, %l4	! save the global registers this block uses
	mov	%g5, %l5

#ifdef XTRATUM
	/* Make space for saving the interrupt context on the stack. XtratuM handles
	   register windows, so we do not do the window overflow handling here. */
	sub	%fp, INTERRUPT_STACK_FRAME_SIZE, %sp
#else
	/* When the SPARC executes a trap handler, it automatically decrements
	   the CWP. We must check whether this new window is an invalid one
	   and process a window overflow if this is the case. */

	rd	%wim, %g4	! get the invalid window
	srl	%g4, %l0, %g5	! shift the WIM to the right CWP times
				! (shift count and CWP use only the 5
				! least significant bits)
	cmp	%g5, 1		! are we in the invalid window?
	bne	.Lno_window_ov	! no, skip window overflow on this window

	/* Window saving. This way we ensure that the trap handler will have,
	   at least, one window to use. */

	 srl	 %g4, 1, %g5
	sll	%g4, NUMBER_OF_REGISTER_WINDOWS - 1, %g4
	or	%g4, %g5, %g4	! rotate WIM register one bit to the right
	save			! decrement CWP

	wr	%g4, %wim	! next three instructions following wr
				! must not be restore, save nor rett

	/* Save the state of window next to the one where the trap handler
	   is executing. */

	std	%l0, [%sp + L0_OFFSET]	! save local registers on the stack
	std	%l2, [%sp + L2_OFFSET]
	std	%l4, [%sp + L4_OFFSET]
	std	%l6, [%sp + L6_OFFSET]

	std	%i0, [%sp + I0_OFFSET]	! save in registers on the stack
	std	%i2, [%sp + I2_OFFSET]
	std	%i4, [%sp + I4_OFFSET]
	std	%i6, [%sp + I6_OFFSET]

	restore

.Lno_window_ov:
	/* We execute the following restore (and the subsequent save) so that
	   the debugger finds a standard prologue (call_trap_handler) at the
	   beginning of the corresponding local function, and hence it is able
	   to unwind through trap handlers. Otherwise, this function would
	   look like a frameless function, while it is not. Note that this
	   restore will move us to the interrupted frame, which cannot be
	   invalid, so that we do not need to worry about window underflow.
	   The global effect of the restore plus the following save is to
	   have a frame big enough for saving the state of the interrupted
	   task and for window overflow/underflow. */

	restore

call_trap_handler:
	/* Ensure that we have enough space for saving the state of the
	   interrupted task (for later retrieve it when finishing interrupt
	   handling), and space for saving this register window to the stack
	   (in case of a context switch before finishing interrupt handling). */

	save	%sp, -INTERRUPT_STACK_FRAME_SIZE, %sp	! make space for window
							! overflow trap and for saving
							! the interrupted context on
							! the stack
#endif

.Lincrease_nesting_level:
	/* Increase the nesting level for the task handling the interrupt */

	/* Get the running task */

        CURRENT_CPU(%l7)

        set     __gnat_running_thread_table, %l6
        sll     %l7, 2, %l7     /* offset in the threads table */
        ld      [%l6 + %l7], %l7

	/* Get the stored value of the nesting level */

	ld	[%l7 + INT_NESTING_OFFSET], %l6

	/* Increase */

	add	%l6, 1, %l6

	/* Save the new value of the nesting level */

	st	%l6, [%l7 + INT_NESTING_OFFSET]

.Lsave_isf:
	/* Save the state of the interrupted task registers (PSR, PC, nPC, Y,
	   global, and input registers) in the Interrupt Stack Frame (ISF).
	   Note that the ISF includes a regular minimum stack frame which will
	   be used, if needed, by register window overflow and underflow
	   handlers. Note that global registers g4 and g5 are still saved in
	   l4 and l5 respectively. */

	rd	%y, %g4
	st	%g4, [%sp + ISF_Y_OFFSET]	! save Y

	std	%l0, [%sp + ISF_PSR_OFFSET]	! save PSR and PC
	std	%l2, [%sp + ISF_NPC_OFFSET]	! save nPC and TT

	! skip g0
	st	%g1, [%sp + ISF_G1_OFFSET]	! save g1
	std	%g2, [%sp + ISF_G2_OFFSET]	! save g2 and g3
	std	%l4, [%sp + ISF_G4_OFFSET]	! save g4 and g5 (from copy)
	std	%g6, [%sp + ISF_G6_OFFSET]	! save g6 and g7

	std	%i0, [%sp + ISF_I0_OFFSET]	! save i0 and i1
	std	%i2, [%sp + ISF_I2_OFFSET]	! save i2 and i3
	std	%i4, [%sp + ISF_I4_OFFSET]	! save i4 and i5
	std	%i6, [%sp + ISF_I6_OFFSET]	! save i6 and i7

	mov	%sp, %g1			! make a copy of SP
	mov	%l3, %g3			! make a copy of Trap Type
						! it will be passed as parameter
						! for the trap handler

#ifdef XTRATUM
	/* With XtratuM, extended interrupts (TT between 0xe0 and 0xff) are
	   treated like hardware interrupts */
	
	cmp	%l3, 0xe0
	bge	.Lfix_pil
#endif

	/* If this is an external interrupt (TT between 0x11 and 0x1f), two
	   things must be done: 1) set the PIL to mask off interrupts and
	   2) change the stack. Note that the original PSR stored in l0 is
	   not modified since it will be restored when the interrupt handler
	   returns. */

	cmp	%l3, 0x11			! l3 contains the trap type
	bl	.Lexecute_handler
	 cmp     %l3, 0x1f
	bg      .Lexecute_handler
	 nop

.Lfix_pil:
#ifdef XTRATUM
	set	sparc_set_pil_nr, %o0		! mask all interrupts
        __XM_AHC
#else
	or	%l0, PSR_PIL_MASK, %l0		! mask all interrupts
	/* Note: We use two wr instructions to change the PIL and the ET flag
	   in the PSR, as specified in the SPARC v7.0 Instruction Set manual. */

	wr	%l0, %psr			! set the appropriate PIL
#endif

.Lswitch_stack:
	/* Switch to the appropriate interrupts stack.
	   The interrupt_stack table contains a list of pointers to the top of
	   the different interrupts' stacks. */

	set	interrupt_stack_table, %l4
#ifdef XTRATUM
	cmp	%l3, 0xe0
	bge	.Lhw_interrupt
	 sub	%l3, 0xe0, %l7		! use TT to calculate offset (extended
					! interrupt)
	sub	%l3, 0x11, %l7		! use TT to calculate offset
.Lhw_interrupt:
#else
	sub	%l3, 0x11, %l7		! use TT to calculate offset
#endif
	
.Lload_stack_address:
	sll	%l7, 2, %l7
	ld	[%l4 + %l7], %g1	! get new stack

.Lexecute_handler:
	/* When being here the following values are in global registers:
	    g1 <- SP to use
	    l0 <- PSR (with the appropriate PIL and traps disabled)
	    g3 <- Trap Type (TT)
	 */

	/* Get the address of the user's handler from the User_Vector_Table */

	sll	%g3, 2, %g4		! use TT to calculate offset in the
					! user vector table. It must be in
					! a global register because we are
					! moving CWP

	set	user_vector_table, %g2
	ld	[%g2 + %g4], %g2	! g2 <- user's handler address

/* Partitions on top of XtratuM cannot enable/disable traps */
#ifndef XTRATUM
	wr	%l0, PSR_ET_MASK, %psr	! ----- Enable traps. -----
	nop
	nop
	nop
#endif
	
	/* Now we know that a save instruction will not overflow. We create
	   a new window for ensuring proper saving of all the register
	   windows in case of a context switch. */

	save	%g1, -MINIMUM_STACK_FRAME_SIZE, %sp	! make space for window
							! overflow trap

	/* Jump to the trap wrapper that will later call the user's handler */

	call	%g2				! call wrapper procedure
	  mov	 %g3, %o0			! pass the trap type as
						! parameter to the handler

	sub	%fp, MINIMUM_STACK_FRAME_SIZE, %sp ! go back to the task stack
	restore

	/* Restore saved registers */

.Lcheck_trap_type:
	/* Retrieve TT of the trap that we are handling for checking whether
	   it is an external interrupt. */

	ld	[%sp + ISF_TT_OFFSET], %l3	! retrieve trap type

#ifdef XTRATUM
	/* With XtratuM, extended interrupts are treated like hardware
	   interrupts */
	
	cmp	%l3, 0xe0
	bge	.Lcheck_context_switch
#endif
	
	/* If this is an external interrupt (TT between 0x11 and 0x1f), we
	    must tell the executive that interrupt handling is finishing. */

	cmp	%l3, 0x11
	bl	.Lrestore_from_isf
	 cmp	 %l3, 0x1f
	bg	.Lrestore_from_isf
	 nop

.Lcheck_context_switch:
	/* We need to check whether there is any scheduling event to handle */

	call	context_switch_needed
	 nop

	cmp	%o0, 0
	be	.Lrestore_from_isf		! no context switch needed
	 nop

.Ldo_context_switching:
	/* When being here traps are enabled, but interrupts are disabled
	   (PIL=15). */

	call	context_switch
	 nop

.Lrestore_from_isf:
	ld	[%sp + ISF_Y_OFFSET], %g2
	wr	%g2, %y				! restore Y register

#ifdef XTRATUM
	set     sparc_get_psr_nr, %o0		! get psr
        __XM_AHC
	mov	%o0, %l0
#else
	rd	%psr, %l0
#endif

	set	(PSR_EF_MASK | PSR_CWP_MASK), %g3
	and	%l0, %g3, %l4			! get current EF and CWP

	ldd	[%sp + ISF_PSR_OFFSET], %l0	! retrieve PSR and PC
	ldd	[%sp + ISF_NPC_OFFSET], %l2	! retrieve nPC and TT

	andn	%l0, %g3, %l0			! take everything but the EF
						! bit and the CWP from the
						! saved PSR
	or	%l4, %l0, %l0			! install PSR later ...
						! keep traps disabled

	!skip g0
	ld	[%sp + ISF_G1_OFFSET], %g1	! restore global registers
	ldd	[%sp + ISF_G2_OFFSET], %g2
	ldd	[%sp + ISF_G4_OFFSET], %g4
	ldd	[%sp + ISF_G6_OFFSET], %g6

	ldd	[%sp + ISF_I0_OFFSET], %i0	! restore input registers
	ldd	[%sp + ISF_I2_OFFSET], %i2
	ldd	[%sp + ISF_I4_OFFSET], %i4
	ldd	[%sp + ISF_I6_OFFSET], %i6

/* XtratuM will handle invalid windows */

#ifndef XTRATUM
	/* If (CWP + 1) is an invalid window, we need to reload it. */

	rd	%wim, %l5
	add	%l0, 1, %l6			! CWP + 1
	and	%l6, NUMBER_OF_REGISTER_WINDOWS - 1, %l6 ! do modulo on it
	srl	%l5, %l6, %l7			! l7 = WIM >> (CWP + 1)
	cmp	%l7, 1				! is the window invalid?
	bne	.Lvalid_window			! no, do not restore window

	/* Rotate wim one bit to the left since the window that is currently
	   invalid will be restored. Note that traps are disabled and we
	   cannot use the standard window underflow mechanism.*/

	 sll	 %l5, 1, %l7
	srl	%l5, NUMBER_OF_REGISTER_WINDOWS - 1, %l5
	wr	%l5, %l7, %wim
	nop
	nop
	nop

	/* Restore the window the same way as in a window underflow trap */

	restore					! now into the previous window

	ldd	[%sp + L0_OFFSET], %l0		! restore the local registers
	ldd	[%sp + L2_OFFSET], %l2
	ldd	[%sp + L4_OFFSET], %l4
	ldd	[%sp + L6_OFFSET], %l6

	ldd	[%sp + I0_OFFSET], %i0		! restore the output registers
	ldd	[%sp + I2_OFFSET], %i2
	ldd	[%sp + I4_OFFSET], %i4
	ldd	[%sp + I6_OFFSET], %i6

	save

.Lvalid_window:

#endif

.Ldecrease_nesting_level:
	/* Decrease the nesting level for the task handling the interrupt */

	/* Get the running task */

        CURRENT_CPU(%l7)

        set     __gnat_running_thread_table, %l6
        sll     %l7, 2, %l7     /* offset in the threads table */
        ld      [%l6 + %l7], %l7

	/* Get the stored value of the nesting level */

	ld	[%l7 + INT_NESTING_OFFSET], %l6

	/* Decrease */

	add	%l6, -1, %l6

	/* Save the new value of the nesting level */

	st	%l6, [%l7 + INT_NESTING_OFFSET]

#if defined (LEON) && !defined (XTRATUM)
	/* Before leaving, restore the base state of the Cache Control
	   Register if we are at the outermost interrupt. */

	cmp	%l6, 0				! is outermost interrupt?

	bne	.Lreturn_from_trap

	/* Get the stored value of the base Cache Control Register. This is
	   the value that is not affected by any interrupt handling. */

	 ld	[%l7 + BASE_CCR_OFFSET], %l6

.Lset_base_ccr:
#if defined (LEON2)
	sethi	%hi(0x80000000), %l5
	st	%l6, [%l5 + 0x14]		! set CCR
#elif defined (LEON3)
	sta	%l6, [%g0] 2			! set CCR
#endif

.Lreturn_from_trap:
#endif

	/* Set the PSR as it was before the trap and return from trap */

#ifdef XTRATUM
	mov	%l0, %o1			! Use o1 to pass the saved PSR
	set	sparc_set_psr_nr, %o0		! as argument to the hypercall
	__XM_AHC

	restore
	save
	
	set	sparc_iret_nr, %o0
	__XM_AHC
#else
	wr	%l0, %psr			! disable traps. They will be
	nop					! enabled later by rett
	nop
	nop

	jmp	%l1
	 rett	 %l2
#endif

	.size common_handler, . -common_handler
