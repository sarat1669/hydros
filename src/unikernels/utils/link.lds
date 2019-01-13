ENTRY(_start)

SECTIONS {
	/* The kernel is placed at 1mb after loading */
	. = 0x100000;

	/* The boot entry point must start at the first byte of the executable */
	.text : {
		../../../libkernel/build/boot_entry_point.o (.text);
	}

	/* The rest of the text section follows */
	.text : { * (.text); }

	/* The data section then follows directly after */
	.data : { * (.data); }
	
	/* Finally, the BSS section */
	.bss : { * (.bss); }
}
