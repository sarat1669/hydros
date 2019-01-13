/* A bunch of useful bytewise operations */

#include <byte_utils.h>

int util_is_aligned(u64int addr, int alignment) {
	return (addr % alignment) == 0;
}

u64int util_align_up(u64int addr, int alignment){
	// TODO: This is really bad, but I am tired...
	while(!util_is_aligned(addr++, alignment));

	return addr - 1;
}

u64int util_align_down(u64int addr, int alignment) {
	return addr - (addr % alignment);	
}

u64int util_align(u64int addr, int alignment, int dir) {
	return (dir ? util_align_up : util_align_down)(addr, alignment);
}


