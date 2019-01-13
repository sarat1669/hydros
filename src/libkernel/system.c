#include <apic.h>
#include <port.h>
#include <types.h>
#include <cpuid.h>
#include <debug.h>

int proc_id;

void system_set_proc_id(int x) {
	proc_id = x;
}

int system_get_proc_id(void) {
	return proc_id;
}

u64int system_get_cmos_memory(void) {
	// TODO: Get the low byte as well.
    //u8int low;
 
    //port_outb(0x70, 0x34);
    //low = port_inb(0x71);
	port_outb(0x70, 0x35);
 
//	return 64 * ((u64int)port_inb(0x71) << 8);
	return 0x80000;
}

u64int system_read_msr(char msr) {
	u64int res;

	asm ("rdmsr" : "=A"(res) : "c"(msr));

	return res;
}

void system_write_msr(char msr, u64int val) {
	asm volatile ("wrmsr" : : "c"(msr), "A"(val));
}

char is_hyper_threaded() {
	unsigned int eax, ebx, ecx, edx;

	// Get CPU features
	eax = 1;

	// Issue the request
	__get_cpuid(1, &eax, &ebx, &ecx, &edx);

	return ((edx & (1 << 28)) != 0) ? 1 : 0;
}
