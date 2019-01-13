/* This file is responsible for the duplication of the microkernel for each of the APs in the
 * system. Accurately placing each of the duplicates is critical to the hop to the new kernel
 * system going smoothly.
 *
 * We duplicate the whole of the first megabyte of RAM (excluding the first 16kb, where the page
 * tables reside) in order to try and avoid as many 'missing memory' and  relocation problems 
 * as possible. For example, 0x8000 (where the kernel is originally located is linked as residing
 * at 0x9000. If it was located at 0xA000 bad things would happen. */

#include <memory.h>
#include <types.h>
#include <console.h>
#include <apic.h>
#include <debug.h>

void sys_dup_kernel(char* location) {
	// We want to start copying from after the relocated local APIC (0x6000),
	// So we have to make the offsets line up by zeroing the area between 0x5000 and 0x6000.
	
	console_printf("Duplicating low memory to %h.\n", location);

	location += 0x100000;

	kmemcpy(location, (char*) 0x100000, 0x500000); // Copy the kernel
}

void sys_dup_all_kernels(void) {
	int dups_remaining = system_get_cores() - 1;
	u32int* meta_table = (u32int *) 0x500;
	// Start at the second entry of the table
	meta_table++;

	// For each entry in the meta-paging table make a copy of the kernel.
	while(dups_remaining > 0) {
		sys_dup_kernel((char*) ((u64int)*meta_table));
		meta_table++;
		dups_remaining--;
	}
}
