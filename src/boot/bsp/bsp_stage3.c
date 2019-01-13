// This is the third stage of the boot process.
// It handles copying the RMS buffer to the correct position (at 0x100000).

#include <types.h>
#include <system.h>
#include <debug.h>

// Define the sizes and sector numbers of the disk constituents.
#define SECTOR_SZ 512
#define DISK_SZ (1024 * 1024 * 7)				// Currently 8MB
#define DISK_SECTORS (DISK_SZ / SECTOR_SZ)
#define BOOTLOADER_SECTORS 63					// Currently a full 'head'
#define BOOTLOADER_SZ (BOOTLOADER_SECTORS * SECTOR_SZ)
#define OS_SZ (DISK_SZ - BOOTLOADER_SZ)
#define BLOCK_SECTORS 64
#define BLOCK_SZ (BLOCK_SECTORS * SECTOR_SZ)

#define SECTORS_TO_READ (OS_SZ / SECTOR_SZ)

#define SHARED_BUF  0xc800

#define LAPIC_BASE 0xfee00000
#define LAPIC_VERSION 0x30
#define LAPIC_IPI 0x300

/////////////////////////////////
///////// RMS FUNCTIONS /////////
/////////////////////////////////

void write(u64int location, int data) {
	*((int*) location) = data;
}

// Copy count bytes from src to dest
void memcpy(unsigned char* dest, unsigned char* src, unsigned int count) {
	asm volatile("cld ; rep movsb"
		:: "S"(src), "D"(dest), "c"(count) : "flags", "memory");
}

void set_lba_request(u16int count, u64int start_block, u32int addr) {
	((LBAPacket*)0xBC00)->pkt_len = 0x10;
	((LBAPacket*)0xBC00)->reserved = 0;
	((LBAPacket*)0xBC00)->count = count;
	((LBAPacket*)0xBC00)->addr = addr;
	((LBAPacket*)0xBC00)->start_block = start_block;
}

void read_kernel(void) {
	u32int sector = BOOTLOADER_SECTORS;
	u8int* dest = (u8int*) 0x100000;

	for(; sector < DISK_SECTORS; sector += BLOCK_SECTORS) {
		// Wait for the RMS to be ready
		while(system_get_rms_ready());

		set_lba_request(BLOCK_SECTORS, sector, SHARED_BUF);

		system_set_rms_ready();

		// Wait for the instruction to be completed
		while(system_get_rms_ready());

		// Copy the data
		memcpy(dest, (void*) SHARED_BUF, BLOCK_SZ);

		// Append the next block of data after the current one
		dest += BLOCK_SZ;
	}
}

void init_rms_trampoline(void) {
	// Create the trampoline area
	*((u16int*)0x90000) = 0x00ea;
	*((u16int*)0x90002) = 0x00c2;
	*((u16int*)0x90004) = 0x0000;
	*((u16int*)0x90006) = 0x0000;
}

void start_ap(void) {
	int i;

	// Startup the RMS
	write(LAPIC_BASE + LAPIC_IPI, 0x000CCD00);

	// Wait for the cores to boot up
	for(i = 0; i < 100000; i++);

	// Send a StartupIPI!
	write(LAPIC_BASE + LAPIC_IPI, 0x000CCE90);
	for(i = 0; i < 100000; i++);
	//write(LAPIC_BASE + LAPIC_IPI, 0x000CCE90);
}

/////////////////////////////////
///////// MAIN FUNCTION /////////
/////////////////////////////////

void stage3_main(void) {
	// Setup the RMS communication data
	system_set_rms_claimed(0);
	system_set_rms_instr(0);

	// Setup RMS trampoline
	init_rms_trampoline();

	// Start the APs
	start_ap();

	// Actually read the data from the RMS...
	read_kernel();

}


