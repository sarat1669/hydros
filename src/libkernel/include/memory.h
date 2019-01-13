#ifndef MEMORY_H
#define MEMORY_H

#include "types.h"

int kread(u64int location);
void kwrite(u64int location, int data);
void kmemcpy(char* dest, char* src, unsigned int count);
void kmemset(char* dest, char c, unsigned int count);
void kmemrev(char* src, unsigned int count);
#endif
