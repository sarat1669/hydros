#########################################
################# META ##################
#########################################

.DEFAULT_GOAL = all

rebuild: clean all

commit:
	(cd ../../ && make commit)

diff:
	(cd ../../ && make diff)

cscope:
	(cd ../../ && make cscope)

#########################################
########### BUILD VARIABLES #############
#########################################

# Erlang files

ELIB_INCLUDES = -I src/include
ERL_COMPILE_FLAGS = +warn_obsolete_guard +debug_info $(ELIB_INCLUDES)

ELIB_SRC = $(wildcard src/*/*.erl)
ELIB_SRC_NO_DIR = $(notdir $(ELIB_SRC))
ELIB_BEAM = $(ELIB_SRC_NO_DIR:%.erl=ebin/%.beam)

# C files
include ../../crosscompiler.mk

INCLUDES = -isystem ../libc/include \
			-isystem ../libkernel/include \
			-isystem $(CROSS_INSTALL_DIR)/lib/gcc/x86_64-unknown-elf/*/include/

ERLCINCLUDES = -I../vm/x86_64-unknown-linux-gnu \
				-I../vm/emulator/x86_64-unknown-linux-gnu/opt/plain \
				-I../vm/emulator/beam \
				-I../vm/emulator/sys/unix \
				-I../vm/emulator/sys/common \
				-I../vm/emulator/x86_64-unknown-linux-gnu \
				-I../vm/emulator/pcre \
				-I../vm/emulator/zlib \
				-I../vm/include \
				-I../vm/include/x86_64-unknown-linux-gnu \
				-I../vm/include/internal \
				-I../vm/include/internal/x86_64-unknown-linux-gnu \
				-I../vm/emulator/drivers/common \
				-I../vm/emulator/drivers/unix

ERLCFLAGS = -Wreturn-type -g -O2 -fno-tree-copyrename -D_GNU_SOURCE -DHAVE_CONFIG_H -Wall -Wstrict-prototypes -Wmissing-prototypes -Wdeclaration-after-statement
CFLAGS = -ffreestanding -nostdinc -nostdlib -mno-red-zone -m64 $(ERLCFLAGS) $(INCLUDES) $(ERLCINCLUDES)

C_ELIB_SRC = $(wildcard c_src/*.c)
C_ELIB_SRC_NO_DIR = $(notdir $(C_ELIB_SRC))
C_ELIB_OBJS = $(C_ELIB_SRC_NO_DIR:%.c=bin/%.o)

##################################
######### BUILD THE OS ###########
##################################

all: ebin gen bin/mod_package.o

bin/mod_package.o: bin gen/mod_package.bin
	ld -r -b binary -o $@ gen/mod_package.bin

gen/mod_package.bin: gen $(ELIB_BEAM)
	./build_package $@ ebin/*

# The C objects are requested by the vm Makefile, not the global one, after the vm make rule 'generate' has been executed.

c_bin: bin $(C_ELIB_OBJS)

ebin:
	mkdir ebin

gen:
	mkdir gen

bin:
	mkdir bin

bin/%.o: c_src/%.c
	$(CC) $(CFLAGS) -c $< -o $@

ebin/%.beam: src/*/%.erl
	erlc $(ERL_COMPILE_FLAGS) -o ebin $<

clean:
	rm -rf ebin
	rm -rf bin
	rm -rf gen
