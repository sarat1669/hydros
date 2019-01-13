# This is a basic makefile for building unikernel images.

# First, import the unikernel make rules.
include ../../utils/unikernel_shared.mk

KERNEL = $(notdir $(shell pwd))
KERNEL_BIN = ../$(KERNEL).bin
.PHONY = $(KERNEL_BIN)

KERNEL_C = $(wildcard *.c)
KERNEL_HEADERS = $(wildcard *.h)
KERNEL_OBJ = $(KERNEL_C:%.c=%.o)

%.o: %.c $(KERNEL_HEADERS)
	$(CC) $(CFLAGS) -ffreestanding -c $< -o $@

$(KERNEL_BIN): $(KERNEL_OBJ)
	$(LD) -m elf_x86_64 -T ../../utils/link.lds -o $@ \
		  --oformat binary $(KERNEL_OBJ) \
		  --whole-archive ../../libuni/libuni.a \
		  --as-needed ../../../libc/libc.a \
		  --as-needed ../../../libkernel/libkernel.a

clean:
	rm -f *.o
	rm -f ../$(KERNEL).bin
