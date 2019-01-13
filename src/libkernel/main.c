/* This is the main kernel function.
 * At this point we are in 32-bit protected mode with 32kb of the kernel
 * copied into memory. The GDT has also been crafted to allow us a code and
 * data segment that cover the first 4gb of the available RAM.
 */

#include <console.h>
#include <memory.h>
#include <string.h>
#include <debug.h>
#include <sys_info.h>
#include <interrupts.h>
#include <apic.h>
#include <types.h>
#include <paging.h>
#include <sys_dup.h>
#include <system.h>

char* argv[] = {"erl"};

int erl_main(int proc_id, int procs, char first_boot);
extern char _binary_gen_mod_package_bin_start[];
extern char _binary_gen_mod_package_bin_end[];

// The start of the main BEAM execution function
extern char process_main;

#define VAR_BUF (char*)0x900000
//#define VAR_INC 2
#define VAR_INC 512
#define VID_BUF (char*)0xb8000
#define VID_BUF_SZ (80 * 25)

char first_boot = 0;

void run_beam(void) {
	erl_main(system_get_proc_id(), system_get_cores(), first_boot);
}

void kmain(int proc_id) {

	if((proc_id == 0) && (system_get_done_first_boot() == 0)) {
		// We are the BSP.

		// This is the first node in the machine, booting for the first time
		first_boot = 1;
		
		// Initialize the read-only shared memory data
		mp_init();
		system_set_proc_id(proc_id);
		system_set_memory(system_get_cmos_memory());

		// Initialize the console.
		console_clear_all();
		console_init_single();
		console_clear();

		// Setup interrupts.
		idt_init();

		// Write the AP bootloader trampoline.
		mp_start_bootstrap();

		system_set_done_first_boot(1);
	}
	else {
		system_set_proc_id(proc_id);
		first_boot = 0;
		console_init();
		idt_init();
	}

	console_printf("Node %d of %d is booting...\n",
		system_get_proc_id() + 1, system_get_cores());

	run_beam();
}
