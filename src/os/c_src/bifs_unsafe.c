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
#include <system.h>
#include <console.h>

void interrupts_forward(long error, long inum, long addr);

// RAW MEMORY ACCESS FUNCTIONS

BIF_RETTYPE os_unsafe_do_read_2(BIF_ALIST_2) {
	byte* addr = (byte*) unsigned_val(BIF_ARG_1);
	u64int size = unsigned_val(BIF_ARG_2), i;
	byte* buf = erts_alloc(ERTS_ALC_T_BINARY_BUFFER,size);
	Eterm bin;

	memcpy(buf, addr, size);
	
	bin = new_binary(BIF_P, buf, size);

	erts_free(ERTS_ALC_T_BINARY_BUFFER, buf);

	BIF_RET(bin);
}

BIF_RETTYPE os_unsafe_do_write_2(BIF_ALIST_2) {
	byte* addr = (byte*) unsigned_val(BIF_ARG_1);
	unsigned int size = binary_size(BIF_ARG_2), i;
	u16int bit_offs, bit_size;
	byte* bytes;

	ERTS_GET_BINARY_BYTES(BIF_ARG_2, bytes, bit_offs, bit_size);

	memcpy(addr, bytes, size);

	BIF_RET(make_small(size)); // Return the number of bytes written
}

BIF_RETTYPE os_unsafe_do_write_3(BIF_ALIST_3) {
	byte* addr = (byte*) unsigned_val(BIF_ARG_1);
	unsigned int size = binary_size(BIF_ARG_2), i;
	unsigned int copy_size = unsigned_val(BIF_ARG_3);
	byte* bytes;
	u16int bit_offs, bit_size;
	u16int* u16;
	u32int* u32;
	u64int* u64;
	u16int* u16_addr = (u16int*) addr;
	u32int* u32_addr = (u32int*) addr;
	u64int* u64_addr = (u64int*) addr;

	ERTS_GET_BINARY_BYTES(BIF_ARG_2, bytes, bit_offs, bit_size);
	u16 = (u16int*) bytes;
	u32 = (u32int*) bytes;
	u64 = (u64int*) bytes;

	for(i = 0; i <= (size / copy_size); i++) {
		switch(copy_size) {
			case 8:
				addr[i] = bytes[i];
				break;
			case 16:
				u16_addr[i] = u16[i];
				break;
			case 32:
				u32_addr[i] = u32[i];
				break;
			case 64:
				u64_addr[i] = u64[i];
				break;
		}
	}

	BIF_RET(make_small(i)); // Return the number of bytes written
}

// LOCKING FUNCTIONS

BIF_RETTYPE os_unsafe_do_lock_1(BIF_ALIST_1) {
	byte* addr = (byte*) unsigned_val(BIF_ARG_1);

	if(__sync_bool_compare_and_swap(addr, 0, 1))
		BIF_RET(am_true);
	else
		BIF_RET(am_false);
}

BIF_RETTYPE os_unsafe_do_unlock_1(BIF_ALIST_1) {
	byte* addr = (byte*) unsigned_val(BIF_ARG_1);

	if(__sync_bool_compare_and_swap(addr, 1, 0))
		BIF_RET(am_true);
	else
		BIF_RET(am_false);
}

// MEMORY ALLOCATION AND FREEING

BIF_RETTYPE os_unsafe_do_malloc_1(BIF_ALIST_1) {
	u64int amount = (u64int) unsigned_val(BIF_ARG_1);
	u32int res;

	res = (u32int) malloc(amount);

	BIF_RET(make_small(res));
}

BIF_RETTYPE os_unsafe_do_free_1(BIF_ALIST_1) {
	u64int ptr = (u64int) unsigned_val(BIF_ARG_1);

	free((void*)ptr);

	BIF_RET(am_ok);
}

// PORT FUNCTIONS

BIF_RETTYPE os_unsafe_do_port_in_1(BIF_ALIST_1) {
	unsigned char port = (unsigned char) unsigned_val(BIF_ARG_1), res;

	asm("in %%dx, %%al" : "=a" (res) : "d" (port));

	BIF_RET(make_small(res));
}

BIF_RETTYPE os_unsafe_do_port_out_2(BIF_ALIST_2) {
	char port = (char) unsigned_val(BIF_ARG_1);
	char byte = (char) unsigned_val(BIF_ARG_2);

	asm("out %%al, %%dx" : : "a" (byte), "d" (port));

	BIF_RET(am_ok);
}

BIF_RETTYPE os_unsafe_do_port_in32_1(BIF_ALIST_1) {
	u16int port = (u16int) unsigned_val(BIF_ARG_1);
	u32int res;

	asm volatile ("inl %w1, %0" : "=a" (res) : "Nd" (port));

	BIF_RET(make_small(res));
}

BIF_RETTYPE os_unsafe_do_port_out32_2(BIF_ALIST_2) {
	u16int port = (u16int) unsigned_val(BIF_ARG_1);
	u32int data = (u32int) unsigned_val(BIF_ARG_2);

	asm volatile ("outl %0, %w1" : : "a" (data), "Nd" (port));

	BIF_RET(am_ok);
}

BIF_RETTYPE os_unsafe_load_pagetable_1(BIF_ALIST_1) {
	u64int addr = unsigned_val(BIF_ARG_1);

	asm("mov %0, %%cr3" :: "r" (addr));

	BIF_RET(am_ok);
}

// INTERRUPT REGISTERING AND FORWARDING

// Store the Process as an Eterm and Process*
// to avoid unnecessary lookup load 

Process* int_sending_proc;
Eterm int_reg_pid;
unsigned char int_proc_registered = 0;

BIF_RETTYPE os_unsafe_register_for_interrupts_1(BIF_ALIST_1) {
	int_sending_proc = BIF_P;
	int_reg_pid = BIF_ARG_1;

	//LOGE("Registered process %T to receive interrupts.\n", LPTR BIF_P);

	int_proc_registered = 1;

	BIF_RET(am_ok);
}

// Generate an interrupt for testing purposes
BIF_RETTYPE os_unsafe_do_interrupt_0(BIF_ALIST_0) {
	LOGV("Deliberately generating interrupt.\n");
	
	LOGV("Can't print this: %d\n", *((u64int*)0x10000000000));
	//asm volatile ("int $0");

	BIF_RET(am_ok);
}

extern int sys_proc_id;
extern int sys_procs;
extern char sys_first_boot;
extern u64int sys_pit_count;
extern u64int sys_lapic_count;
extern u64int sys_lapic_inc;

// Get a system value from C.
// In the end, we should not need this.
BIF_RETTYPE os_unsafe_do_get_sys_val_1(BIF_ALIST_1) {
	Eterm ret_val;
	Eterm* hp;

	switch(BIF_ARG_1) {
		case am_id:
			ret_val = make_small(sys_proc_id);
			break;
		case am_procs:
			ret_val = make_small(sys_procs);
			break;
		case am_first_boot:
			ret_val = make_small(sys_first_boot);
			break;
		case am_pit_count:
			//LOGV("sys_pit_count: %d\n", sys_pit_count);
			ret_val = make_small(sys_pit_count);
			break;
		case am_lapic_count:
			ret_val = make_small(sys_lapic_count);
			break;
		case am_time:
			ret_val = make_small(system_get_global_time());
			break;
		case am_cursor:
			hp = HAlloc(BIF_P, 2 + 1);
			// Generate the tuple
			ret_val = make_tuple(hp);
			// Define the header (with arity 2)
			hp[0] = make_arityval(2);

			// Add the values
			hp[1] = make_small(cursor.y);
			hp[2] = make_small(cursor.x);
			break;
		default:
			LOGV("os_unsafe:get_sys_val -- Value not found."
					" Perhaps rebuild the atom table.\n");
	}

	BIF_RET(ret_val);
}

// Set a system value that can be accessed from the C.
BIF_RETTYPE os_unsafe_do_set_sys_val_2(BIF_ALIST_2) {

	switch(BIF_ARG_1) {
		case am_lapic_inc:
			sys_lapic_inc = unsigned_val(BIF_ARG_2);
			break;
		case am_time:
			system_set_global_time(unsigned_val(BIF_ARG_2));
			break;
		default:
			LOGV("os_unsafe:set_sys_val -- Value not found."
					" Perhaps rebuild the atom table.\n");
	}

	BIF_RET(am_ok);
}

// Retreive a binary of the modules package from the OS image
extern char _binary_gen_mod_package_bin_start[];
extern char _binary_gen_mod_package_bin_end[];

BIF_RETTYPE os_unsafe_get_os_package_0(BIF_ALIST_0) {
	byte* addr = (byte*) &_binary_gen_mod_package_bin_start;
	u64int size = _binary_gen_mod_package_bin_end -
					_binary_gen_mod_package_bin_start;
	byte* buf = erts_alloc(ERTS_ALC_T_BINARY_BUFFER,size);
	Eterm bin;

	memcpy(buf, addr, size);
	
	bin = new_binary(BIF_P, addr, size);

	erts_free(ERTS_ALC_T_BINARY_BUFFER, buf);

	BIF_RET(bin);
}

BIF_RETTYPE os_unsafe_do_instruction_1(BIF_ALIST_1) {
	Eterm res = am_ok;	// The resulting eterm
	Eterm* hp;			// A heap variable, if required
	// Instruction dependent variables
	int cycles_high, cycles_low;
		
	switch(BIF_ARG_1) {
		case am_cli:
			asm("cli");
			break;
		case am_sti:
			asm("sti");
			break;
		case am_hlt:
			asm("hlt");
			break;
		case am_break:
			BREAK();
			break;
		case am_eoi:
			*((u32int*) 0xfee000b0) = (u32int) 0;
			break;
		case am_rdtsc:
			// Read the cycles, making sure to clear the pipeline
			asm volatile (
     			"cpuid \n"
     			"rdtsc" 
   				: "=a"(cycles_low), "=d"(cycles_high)	// Outputs
   				: "a"(0)				// Inputs
   				: "%ebx", "%ecx");		// Clobbers

			// Alloc space for 2 small values and the tuple
			hp = HAlloc(BIF_P, 2 + 1);
			// Generate the tuple
			res = make_tuple(hp);
			// Define the header (with arity 2)
			hp[0] = make_arityval(2);

			// Add the values
			hp[1] = make_small(cycles_high);
			hp[2] = make_small(cycles_low);
			break;
		case am_hang:
			LOGV("HANGING\n");
			while(1);
		case am_clear_screen:
			console_clear_all();
			break;
		case am_single_screen:
			console_init_single();
			break;
		case am_multi_screen:
			console_init();
			break;
		default:
			LOGV("os_unsafe:instruction -- Instruction not found.\n");
	}

	BIF_RET(res);
}

// This needs a better home!
BIF_RETTYPE os_unsafe_do_format_1(BIF_ALIST_1)
{
	Eterm res;
	Eterm *hp;
	erts_dsprintf_buf_t *dsbufp = erts_create_tmp_dsbuf(64);       

	erts_dsprintf(dsbufp, "%.*T\n", INT_MAX, BIF_ARG_1);
	hp = HAlloc(BIF_P, 2*dsbufp->str_len); /* we need length * 2 heap words */
	res = buf_to_intlist(&hp, dsbufp->str, dsbufp->str_len, NIL);
	erts_destroy_tmp_dsbuf(dsbufp);
	BIF_RET(res);
}

BIF_RETTYPE os_unsafe_do_print_string_1(BIF_ALIST_1)
{
	unsigned int size = binary_size(BIF_ARG_1);
	u16int bit_offs, bit_size;
	byte* bytes;

	ERTS_GET_BINARY_BYTES(BIF_ARG_1, bytes, bit_offs, bit_size);

	console_printsn(bytes, size);

    BIF_RET(am_true);
}

/* The current process has caused a CPU exception.
 * Kill the process and cause scheduling to occur
 * on return from the interrupt. */

// TODO: This will eventually cause a stack overflow
void interrupts_reschedule(void) {
	Process* c_p = erts_get_current_process();

	process_main();	
}

// The function in src/kernel/main.c that allows us to restart the node. 
void run_beam(void);

void interrupts_kill_process(long stack_addr) {
	Process* rp = erts_get_current_process();
	ErtsProcLocks rp_locks = ERTS_PROC_LOCKS_XSIG_SEND;
	
	if(ERTS_PROC_IS_EXITING(rp) || (rp == NULL)) {
		LOGV("An interrupt has been issued and recovery has failed.\n");
		LOGV("This node will now restart...\n\n\n\n\n");
		run_beam();
	}

	LOGV("Killing process %h due to CPU exception.\n", LPTR rp);

	erts_proc_inc_refc(rp); // TODO: This is almost certainly unnecessary
	erts_smp_proc_lock(rp, rp_locks);
#if 0
	erts_send_exit_signal(NULL, // Send the exit signal itself
		NIL,
		rp,
		&rp_locks,
		am_kill,
		NIL,
		NULL,
		0
	);
#else
	erts_do_exit_process(rp, am_normal);
#endif


	erts_smp_proc_unlock(rp, rp_locks); // Unset locks
	erts_proc_dec_refc(rp);

	*((long*) stack_addr) = interrupts_reschedule;
}

// This function is called by the interrupt handler
void interrupts_forward(long error, long inum, long addr) {
	DeclareTmpHeapNoproc(t_heap, 4); // Declare the tuple on the temp heap
	UseTmpHeapNoproc(4);

	if(!int_proc_registered) {
		LOGV("Not forwarding interrupt -- no listener registered.\n");
		return;
	}

	erl_send(
		int_sending_proc,
		int_reg_pid,
		TUPLE3(t_heap,
			make_small(inum),
			make_small(error),
			make_small(addr)
		)
	);
	UnUseTmpHeapNoproc(4); // Free our data from the temp heap
}
