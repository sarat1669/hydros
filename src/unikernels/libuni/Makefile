.DEFAULT_GOAL = libuni.a

CROSS_PREFIX=x86_64-unknown-elf-
CC=$(CROSS_PREFIX)gcc
LD=$(CROSS_PREFIX)ld
AR=$(CROSS_PREFIX)ar

INCLUDES = -isystem src/include \
           -isystem ../../libc/include \
		   -isystem $(CROSS_INSTALL_DIR)/lib/gcc/x86_64-unknown-elf/*/include/

CFLAGS = -ffreestanding -nostdinc -nostdlib -mno-red-zone -m64 $(INCLUDES)

SOURCES =  $(wildcard src/*.c)
HEADERS =  $(wildcard src/include/*.h)

OBJECTS = $(SOURCES:%.c=%.o)

libuni.a: $(OBJECTS) $(HEADERS)
	$(AR) cr $@ $(OBJECTS)

src/%.o: src/%.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf src/*.o
	rm -rf libuni.a
