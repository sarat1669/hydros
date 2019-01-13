/* Handle initiliazation and contact with the local APIC. */

#include <types.h>
#include <debug.h>
#include <system.h>
#include <string.h>
#include <console.h>
#include <memory.h>

#define MP_FP_TABLE_LOC 0xfa580 // The static location of the MP table for BOCHS. We should be finding this,

#define LAPIC_BASE 0xfee00000	// The location of the LAPIC version register after relocation.

#define LAPIC_ID 0x20
#define LAPIC_VERSION 0x30
#define LAPIC_IPI 0x300
#define LAPIC_SVR 0xF0
#define LAPIC_EOI 0xB0	// End of interrupt

struct mp_fp_table {
	char signature[4];
	u32int mp_config_table_ptr;
	u8int length;
	u8int spec_rev;
	u8int checksum;
	u8int mp_feature_1;
	u8int mp_feature_2;
	u8int mp_feature_3;
	u8int mp_feature_4;
	u8int mp_feature_5;
} __attribute__((packed));

struct mp_config_table {
	char signature[4];
	u16int length;
	u8int spec_rev;
	u8int checksum;
	char oem_id[8];
	char product_id[12];
	u32int oem_table_ptr;
	u16int oem_table_size;
	u16int entry_count;
	u32int local_apic_ptr;
	u16int extended_length;
	u8int extended_checksum;
	u8int reserved;
} __attribute__((packed));

struct mp_io_apic {
	u8int entry_type;
	u8int id;
	u8int version;
	u8int enabled;
	u32int location;
} __attribute((packed));

struct mp_fp_table* find_mp_fp_table(void) {
	char* bios_area = (char*) 0xf0000;

	console_printf("Starting search for BIOS area...\n");

	while(!kstrncmp("_MP_", bios_area, 4))
		bios_area += 16;

	console_printf("MP floating table location: %h\n", *bios_area);

	return (struct mp_fp_table*) bios_area;
}

struct mp_config_table* find_mp_config_table(void) {
	return (struct mp_config_table*) ((u64int) (find_mp_fp_table())->mp_config_table_ptr);
}

void mp_init(void) {
	// This can only be performed by the BSP!
	struct mp_io_apic* apic;
	struct mp_config_table* tbl = find_mp_config_table();
	char* tbl_ext = (char*) ((u64int)tbl + sizeof(struct mp_config_table));

	int i, cores = 0;

	for(i = 0; i < tbl->entry_count - 1; i++) {
		if(*tbl_ext == 0) {
			cores++;
			tbl_ext += 20; // A CPU entry is 20 bytes including type
		}
		else if(*tbl_ext == 2) {
			apic = (struct mp_io_apic*) tbl_ext;
			if(apic->enabled)
				system_set_io_apic(apic->location);
			tbl_ext += 8;
		}
		else 
			tbl_ext += 8; // All other entries are 8 bytes
	}
	
	system_set_cores(cores);
}

void mp_print_details(void) {
	struct mp_config_table* tbl = find_mp_config_table();
	console_printf("MP Table locate at %h\n", tbl);
	console_printf("CPU OEM ID: %8s\n", tbl->oem_id);
	console_printf("CPU Product ID: %4s\n", tbl->product_id);
	console_printf("MP config table entry count: %d\n", tbl->entry_count);
	console_printf("Cores detected: %d\n", system_get_cores());
}

void apic_set_id(int id) {
	kwrite(LAPIC_BASE + LAPIC_ID, id << 24);
}

void mp_start_bootstrap(void) {
	// Position the code for 'jmp 0:0x7e00'
	// at the position that each new AP
	// will land at.
	
	// 0x7e00 acts as a second boot sector
	// that sets up the other cores like 
	// the boot core.

	*((u16int*)0x90000) = 0x00ea;
	*((u16int*)0x90002) = 0x00c4;
	*((u16int*)0x90004) = 0x0000;
	*((u16int*)0x90006) = 0x0000;
}

void apic_int_start(void) {
	//kwrite(LAPIC_SVR, 0x100);
}

void mp_start(void) {
	// Start all of the other processors
	int i;

	mp_start_bootstrap();

	console_printf("LAPIC Version: %h\n", *((int*) LAPIC_BASE + LAPIC_VERSION));
	console_printf("LAPIC ID: %h\n", *((int*) (LAPIC_BASE + LAPIC_ID >> 24)));

	// Send an INIT IPI that resets all the other cores

	// 0 = Reserved
	// 0 = Reserved
	// D = 1101 : ignored, init
	// C = 1000 : Trigger mode, level, reserved, ignored
	// C = 1100 : Dest shorthand, reserved, reserved
	//kwrite(LAPIC_BASE + LAPIC_IPI, 0x000CCD00);

	// Wait for the cores to boot up
	// This is not the best way to wait (LOL)!
	for(i = 0; i < 100000; i++);

	// Send a StartupIPI!
	//kwrite(LAPIC_BASE + LAPIC_IPI, 0x000CCE90);

	console_printf("Startup IPIs sent!\n");

}

void apic_start(void) {
/*
	// Clear task priority to enable all interrupts
	*((int*) 0xfee00080) = 0;

	// Start receiving 'spurious' interrupts from the local APIC.
	*((int*) 0xfee000f0) = 0x11FF;

	// Timer divide configuration /1
	*((int*) 0xfee003E0) = 0xB;

	// Periodic. IRQ0
	*((int*) 0xfee00320) = 0x00020020;
*/
}
