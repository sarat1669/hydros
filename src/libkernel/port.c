char port_inb(char port) {
	char res;
	asm("in %%dx, %%al" : "=a" (res) : "d" (port));
	return res;
}

void port_outb(char port, char byte) {
	asm("out %%al, %%dx" : : "a" (byte), "d" (port));
}
