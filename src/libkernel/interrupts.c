#include <types.h>
#include <interrupts.h>
#include <memory.h>
#include <console.h>
#include <debug.h>

struct idt_entry idt[IDT_ENTRIES];
struct idt_ptr idt_ptr;

void idt_set(u8int num, u64int base, u16int selector, u8int flags) {
	// Sort the base location
	idt[num].base_low = base & 0xffff; // Take the first 16 bits, zero the rest
	idt[num].base_mid = (base >> 16) & 0xffff;
	idt[num].base_high = (base >> 32) & 0xffffffff;

	// Set selector and flags
	idt[num].selector = selector;
	idt[num].flags = flags;

	// Zero the zero fields!
	idt[num].zero = 0x0;
	idt[num].zero2 = 0x0;
}

void idt_init(void) {
	idt_ptr.size = (sizeof(struct idt_entry) * IDT_ENTRIES) - 1;
	idt_ptr.base = (u64int) &idt;

	kmemset((char*) &idt, 0, (sizeof(struct idt_entry) * IDT_ENTRIES));

	SET_ISR(0);
	SET_ISR(1);
	SET_ISR(2);
	SET_ISR(3);
	SET_ISR(4);
	SET_ISR(5);
	SET_ISR(6);
	SET_ISR(7);
	SET_ISR(8);
	SET_ISR(9);
	SET_ISR(10);
	SET_ISR(11);
	SET_ISR(12);
	SET_ISR(13);
	SET_ISR(14);
	SET_ISR(15);
	SET_ISR(16);
	SET_ISR(17);
	SET_ISR(18);
	SET_ISR(19);
	SET_ISR(20);
	SET_ISR(21);
	SET_ISR(22);
	SET_ISR(23);
	SET_ISR(24);
	SET_ISR(25);
	SET_ISR(26);
	SET_ISR(27);
	SET_ISR(28);
	SET_ISR(29);
	SET_ISR(30);
	SET_ISR(31);
	SET_ISR(32);
	SET_ISR(33);
	SET_ISR(34);
	SET_ISR(35);
	SET_ISR(36);
	SET_ISR(37);
	SET_ISR(38);
	SET_ISR(39);
	SET_ISR(40);
	SET_ISR(41);
	SET_ISR(42);
	SET_ISR(43);
	SET_ISR(44);
	SET_ISR(45);
	SET_ISR(46);
	SET_ISR(47);
	SET_ISR(48);

	asm volatile ("lidt (%0)" : : "p"((u64int)&idt_ptr));
	console_printf("Done.\n");

	// Set system time to zero
	system_set_global_time(0);
}

void interrupts_forward(long error, long inum, long addr);
void interrupts_kill_process(long stack_addr);

u64int sys_pit_count = 0;
u64int sys_lapic_count = 0;
u64int sys_lapic_inc = 0; // Num of uSec per lapic int
u64int sys_global_time = 0; // The system wide microsecond time

#define get_apic_id() (*((u32int*)0xfee00020) >> 24)

void interrupt_handeler(long error, long inum, long addr, long stack_addr) {
 	if(inum == 0x8) {
		console_printf("Exprienced double fault at %h! Err: %d.\n.",
			addr, error);
		while(1);
	}
	// Kill processes that fault.

	switch(inum) {
		case 0:		// Divide by 0
		case 4:		// Overflow
		case 5:		// Bound rand exceeded
		case 6:		// Invalid Opcode
		case 7:		// Device not available
		case 10:	// Invalid TSS
		case 11:	// Segment not present
		case 12:	// Stack-segment fault
		case 13:	// General protection fault
		case 14:	// Page fault
		case 16:	// FP exception
		case 17:	// Alignment check
		case 18:	// Machine check
		case 19:	// SIMD FP exception
		case 20:	// Virtualization exception
		case 30:	// Security exception
		console_printf(
			"Unhandeled INT: %h (err#: %h, addr: %h)\n",
			inum, error, addr);
		interrupts_kill_process(stack_addr);
		return;
	}

	if(inum == 0x25) {
		if(sys_lapic_inc)
			// The increment is set, update the counter
			system_set_global_time(
				system_get_global_time()
				+ sys_lapic_inc
			);
		else
			// We are configuring. Increment the counter.
			sys_lapic_count++;
		return;
	}
	// Handle the PIT counter
	if(inum == 0x22) {
		sys_pit_count++;
		return;
	}

	interrupts_forward(error, inum, addr);
}
