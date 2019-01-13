// This file defines a number of useful magic system values that are derived from design constraints.

#ifndef SYSTEM_H
#define SYSTEM_H

#include <types.h>

// This value is constrained by the positioning of the meta-paging table.
// If we moved that to somewhere with more space we could easily increase this.
#define SYS_MAX_CORES 256

typedef struct {												// Byte offset
	// Generic system info:
	u32int cores;												// 0x0
	u64int memory;												// 0x4
	u32int io_apic_location;									// 0xC

	// The time, as calculated in milliseconds.	
	u64int global_time;											// 0x1E
	// Whether or not the first boot has taken place
	u8int done_first_boot;
} __attribute__((packed)) SysInfo;

typedef struct {
	u8int boot_drv;		// The drive to copy data from			// 0x00
	// Real Mode Slave data:
	// Whether or not the RMS has already been started
	u8int rms_claimed;											// 0x1	
	// The instruction for the RMS to execute	
	u16int rms_instr;											// 0x2
	// Auxillary data for the RMS
	u16int rms_aux;												// 0x4
	// Indicates that work is ready for the RMS
	u16int rms_ready;											// 0x6
} __attribute__((packed)) BootInfo;

typedef struct {
	u8int pkt_len;			// Always 0x10
	u8int reserved;			// Always 0
	u16int count;			// The number of sectors to read
	u32int addr;			// The address in memory to write to
	u64int start_block;		// The LBA of the address on disk to read
} __attribute((packed)) LBAPacket;

#define BOOT_INFO_ADDR 0x6000
#define SYS_INFO_ADDR 0x0ba00

#define system_set_io_apic(x) (((SysInfo*)SYS_INFO_ADDR)->io_apic_location = ((u32int)x))
#define system_get_io_apic() (((SysInfo*)SYS_INFO_ADDR)->io_apic_location)
#define system_set_cores(x) (((SysInfo*)SYS_INFO_ADDR)->cores = ((u32int)x))
#define system_get_cores() (((SysInfo*)SYS_INFO_ADDR)->cores)
#define system_set_memory(x) (((SysInfo*)SYS_INFO_ADDR)->memory = ((u64int)x))
#define system_get_memory() (((SysInfo*)SYS_INFO_ADDR)->memory)
#define system_set_global_time(x) (((SysInfo*)SYS_INFO_ADDR)->global_time = ((u64int)x))
#define system_get_global_time() (((SysInfo*)SYS_INFO_ADDR)->global_time)
#define system_set_done_first_boot(x) (((SysInfo*)SYS_INFO_ADDR)->done_first_boot = ((u8int)x))
#define system_get_done_first_boot() (((SysInfo*)SYS_INFO_ADDR)->done_first_boot)

#define system_set_rms_claimed(x) (((BootInfo*)BOOT_INFO_ADDR)->rms_claimed = ((u8int)x))
#define system_get_rms_claimed() (((BootInfo*)BOOT_INFO_ADDR)->rms_claimed)
#define system_set_rms_instr(x) (((BootInfo*)BOOT_INFO_ADDR)->rms_instr = ((u16int)x))
#define system_get_rms_instr() (((BootInfo*)BOOT_INFO_ADDR)->rms_instr)
#define system_set_rms_aux(x) (((BootInfo*)BOOT_INFO_ADDR)->rms_aux = ((u16int)x))
#define system_get_rms_aux() (((BootInfo*)BOOT_INFO_ADDR)->rms_aux)
#define system_set_rms_ready() (((BootInfo*)BOOT_INFO_ADDR)->rms_ready = ((u16int)1))
#define system_get_rms_ready() (((BootInfo*)BOOT_INFO_ADDR)->rms_ready)

void system_set_proc_id(int x);
int system_get_proc_id(void);
u64int system_get_cmos_memory(void);

u64int system_read_msr(char msr);
void system_write_msr(char msr, u64int val);
char is_hyper_threaded();
#endif
