# C options

include ../../../../crosscompiler.mk

INCLUDES = -isystem ../../../libkernel/include \
           -isystem ../../../libc/include \
           -isystem ../../libuni/src/include \
           -isystem $(CROSS_INSTALL_DIR)/lib/gcc/x86_64-unknown-elf/*/include/ 

DBGCFLAGS = -g
OPTFLAG = 
CFLAGS = $(DBGCFLAGS) $(OPTFLAG) -std=gnu11 -nostdinc -nostdlib \
			-mno-red-zone -fno-exceptions -m64 -W -Wall $(INCLUDES)
