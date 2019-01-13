#ifndef MEMORY_H
#define MEMORY_H

#include <stub.h>
#include <memory.h>
#include <sys/types.h>

STUBH(malloc, (size_t sz), void*);
STUBH(free, (void* ptr), void);
STUBH(realloc, (void *ptr, size_t size), void*);

STUBH(memchr, (const void *s, int c, size_t n), void*);
STUBH(memcmp, (const void *s1, const void *s2, size_t n), int);
STUBH(memcpy, (void *dest, const void *src, size_t n), void*);
STUBH(memset, (void *s, int c, size_t n), void*);

#endif
