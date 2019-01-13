#include <stub.h>
#include <memory.h>
#include <sys/types.h>

#define MALLOC_START ((void*) 0x1100000)

// TODO: Don't just start the free memory area at 17mb
void* free_ptr = MALLOC_START;
int on = 0;

// Initialise the memory allocator, wiping memory if we are restarting.
void malloc_init(void) {
	//while((u64int) ptr < (u64int) free_ptr)
	//	*(ptr++) = 0;

	free_ptr = MALLOC_START;
}

// TODO: Make a serious memory allocation/freeing mechanism.
void* malloc(size_t size) {
	void* ptr = free_ptr;
	char alignment_req;

	*((u64int*) ptr) = size;

	ptr += 8;

	free_ptr = ptr;
	free_ptr += size;

	// Align the new pointer to 8 bytes.
	free_ptr += (alignment_req = ((u64int) free_ptr & 7)) == 0 ? 0 : 8 - alignment_req;
	//console_printf("Malloc %h (size: %d)\n", (u64int) ptr, size);

	return ptr;
}

void free(void* ptr) {

}

void* realloc(void *ptr, size_t size) {
	u64int old_size = *(((u64int*) ptr) - 1);
	void* new_ptr = malloc(size);

	//console_printf("Realloc %h (old size: %d, new size: %d)\n", (u64int) ptr, old_size, size);

	memcpy(new_ptr, ptr, old_size);

	return new_ptr;
}

STUB(memchr, (const void *s, int c, size_t n), void*, NULL);

int memcmp(const void *s1, const void *s2, size_t n) {
	char* cs1 = (char*) s1;
	char* cs2 = (char*) s2;

	while(n--)
		if(*cs1++ != *cs2++)
			return ((unsigned char)*(--cs1)) - ((unsigned char)*(--cs2));

	return 0;
}

void* memcpy(void *dest, const void *src, size_t n) {
	kmemcpy(dest, src, n);

	return dest;
}

void* memset(void *s, int c, size_t n) {
	kmemset(s, c, n);

	return s;
}
