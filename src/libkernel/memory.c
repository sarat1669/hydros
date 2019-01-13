#include <memory.h>
#include <debug.h>
#include <console.h>

int kread(u64int location) {
	return *((int*) location);
}

void kwrite(u64int location, int data) {
	*((int*) location) = data;
}

void memcpy_naive(char* dest, char* src, unsigned int count) {
	while(count-- > 0)
		*dest++ = *src++;
}

// Copy count bytes from src to dest
void kmemcpy(char* dest, char* src, unsigned int count) {
	asm volatile("cld ; rep movsb"
		:: "S"(src), "D"(dest), "c"(count) : "flags", "memory");

}

// Set count bytes at dest to data
void kmemset(char* dest, char data, unsigned int count) {
	while(count-- > 0)
		*(dest++) = data;
}

// Reverse the order of the bytes from src to count
void kmemrev(char* start, unsigned int count) {
	char* end = (char*) start + count - 1;
	char swap;

	while(start < end) {
		swap = *start;
		*(start++) = *end;
		*(end--) = swap;
	}
}
