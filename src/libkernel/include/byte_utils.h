#ifndef byte_utils_h
#define byte_utils_h

#include <types.h>

int util_is_aligned(u64int addr, int alignment);
u64int util_align_up(u64int addr, int alignment);
u64int util_align_down(u64int addr, int alignment);
u64int util_align(u64int addr, int alignment, int dir);

#endif
