/* This file contains BIFs for the unsafe module. These are (hopefully) the
 * only BIFs we will need to add to the BEAM for our OS.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "erl_binary.h"
#include "erl_bits.h"

// A small interpreter for C-speed memory manipulation tasks

Eterm exec_instruction(Eterm instr, Eterm *hp_end) {
	// Return Eterms and make sure to set hp_end to include any
	// new heap alloc'd objects.
	
	// TODO: Actually implement instruction execution.
}

BIF_RETTYPE os_lli_run_1(BIF_ALIST_1) {
	Eterm list = BIF_ARG_1;
	Eterm res = am_ok;
	// Keep track of the heap, so that we can free non-final
	Eterm* hp = HEAP_TOP(BIF_P), *hp_end = hp;

	if(!is_list(list))
		// The given argument is not a list, it is a single instruction
		res = exec_instruction(list, &hp_end);
	else {
		// Execute all of the instruction in the list, returning the
		// result of the final one.
		while(is_list(list)) {
			res = exec_instruction(CAR(list_val(list)), &hp_end);
			list = CDR(list_val(list));
			if(is_list(list)) {
				// If we are still executing instructions, free the alloc'd
				// memory from the heap, as the variable is thrown away.
				if(hp != hp_end)
					HRelease(BIF_P, hp_end, hp);
			}
		}
	}

	BIF_RET(res);
}
