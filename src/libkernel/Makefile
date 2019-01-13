#########################################
######## BUILD THE KERNEL LIBRARY #######
#########################################

include ../../crosscompiler.mk

INCLUDES = -isystem include \
	-isystem ~/x-tools/x86_64-unknown-elf/lib/gcc/x86_64-unknown-elf/*/include/
DBGCFLAGS = -g
#OPTFLAGS = -Os -flto
OPTFLAGS = 
CFLAGS = $(DBGCFLAGS) $(OPTFLAGS) -std=gnu11 -nostdinc -nostdlib \
		 -mno-red-zone -mno-sse -fno-exceptions -m64 -W -Wall $(INCLUDES)

OBJ_DIR = build
KERNEL_C = $(wildcard *.c)
KERNEL_HEADERS = $(wildcard include/*.h)
KERNEL_C_OBJ = $(KERNEL_C:%.c=$(OBJ_DIR)/%.o)
KERNEL_C_OBJ_NO_MAIN = $(filter-out $(OBJ_DIR)/main.o,$(KERNEL_C_OBJ))

ENTRY_POINT_OBJ = $(OBJ_DIR)/boot_entry_point.o

# Build the libkernel and the boot entry point.
.DEFAULT_GOAL = all

all: libkernel.a $(ENTRY_POINT_OBJ)

libkernel.a: $(OBJ_DIR) $(KERNEL_HEADERS) $(KERNEL_C_OBJ) $(OBJ_DIR)/ints.asm.o
	$(AR) cr $@ $(KERNEL_C_OBJ_NO_MAIN) $(OBJ_DIR)/ints.asm.o

$(OBJ_DIR):
	mkdir -p $@

$(ENTRY_POINT_OBJ): boot_entry_point.asm
	nasm $< -f elf64 -o $@

$(OBJ_DIR)/ints.asm.o: ints.asm
	nasm $< -f elf64 -o $@

$(OBJ_DIR)/%.o: %.c
	$(CC) $(CFLAGS) -ffreestanding -c $< -o $@

clean:
	rm -rf $(OBJ_DIR)
	rm -f libkernel.a
	rm -f $(ENTRY_POINT_OBJ)
