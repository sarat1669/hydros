#ifndef INTERRUPTS_H
#define INTERRUPTS_H

#define IDT_ENTRIES 256

#define SET_ISR(X) { \
			extern void isr##X(void); \
			idt_set(X, (u64int)isr##X, 0x08, 0x8E); \
		}

struct idt_ptr {
	u16int size; // Size of the IDT (in bytes)
	u64int base; // 64-bit pointer to the base of the IDT.
} __attribute__((packed)); // By default GCC may mess up the packing.

struct idt_entry {
	u16int base_low;
	u16int selector;
	u8int zero;
	u8int flags;
	u16int base_mid;
	u32int base_high;
	u32int zero2;
} __attribute((packed));

void idt_init(void);
void int_start(void);

#endif
