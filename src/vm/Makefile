##################################
###### META AND VARIABLES ########
##################################

.DEFAULT_GOAL = c_bin

include ../../crosscompiler.mk

OBJ_DIR = bin

INCLUDES = -isystem ../libc/include \
			-isystem $(CROSS_INSTALL_DIR)/lib/gcc/x86_64-unknown-elf/*/include/

#INCLUDES = -isystem ../libc/include \
		 	-isystem ../kernel/include \
			-isystem $(CROSS_INSTALL_DIR)/lib/gcc/x86_64-unknown-elf/*/include/  

ERLCINCLUDES = -Ix86_64-unknown-linux-gnu \
				-Iemulator/x86_64-unknown-linux-gnu/opt/plain \
				-Iemulator/beam \
				-Iemulator/sys/unix \
				-Iemulator/sys/common \
				-Iemulator/x86_64-unknown-linux-gnu \
				-Iemulator/pcre \
				-Iemulator/zlib \
				-Iinclude \
				-Iinclude/x86_64-unknown-linux-gnu \
				-Iinclude/internal \
				-Iinclude/internal/x86_64-unknown-linux-gnu \
				-Iemulator/drivers/common \
				-Iemulator/drivers/unix

ERLCFLAGS = -Wreturn-type -g -O2 -fno-tree-copyrename -D_GNU_SOURCE \
		   	-DHAVE_CONFIG_H -Wall -Wstrict-prototypes -Wmissing-prototypes \
			-Wdeclaration-after-statement
#OPTFLAGS = -Os -flto
OPTFLAGS =
CFLAGS = -ffreestanding -nostdinc -nostdlib -mno-red-zone -m64 $(OPTFLAGS) \
		 $(ERLCFLAGS) $(INCLUDES) $(ERLCINCLUDES)
ERL_COMPILE_FLAGS += +warn_obsolete_guard +debug_info

##################################
######### FILE REFERENCES ########
##################################

BEAM_SRC = $(wildcard emulator/beam/*.c)
BEAM_HEADERS = $(wildcard emulator/beam/*.h)
#BEAM_OBJ = $(BEAM_SRC:%.c=%.o)
#BEAM_OBJ = $(BEAM_SRC:emulator/beam/%.c=$(OBJ_DIR)/%.o)

UNIX_SRC = $(wildcard emulator/sys/unix/*.c)
UNIX_HEADERS = $(wildcard emulator/sys/unix/*.h)
#UNIX_OBJ = $(BEAM_SRC:emulator/sys/%.c=$(OBJ_DIR)/%.o)

SYS_COMMON_SRC = $(wildcard emulator/sys/common/*.c)
SYS_COMMON_SRC_HEADERS = $(wildcard emulator/sys/common/*.h)


PLAT_SRC = $(wildcard emulator/x86_64-unknown-linux-gnu/*.c)
PLAT_HEADERS = $(wildcard emulator/x86_64-unknown-linux-gnu/*.h)
#PLAT_OBJ = $(BEAM_SRC:emulator/x86_64-unknown-linux-gnu/%.c=$(OBJ_DIR)/%.o)

PLAT_OPT_SRC = $(wildcard emulator/x86_64-unknown-linux-gnu/opt/plain/*.c)
PLAT_OPT_HEADERS = $(wildcard emulator/x86_64-unknown-linux-gnu/opt/plain/*.h)
#PLAT_OPT_OBJ = $(BEAM_SRC:emulator/x86_64-unknown-linux-gnu/opt/plain/%.c=$(OBJ_DIR)/%.o)

DRVS_COM_SRC = $(wildcard emulator/drivers/common/*.c)
DRVS_COM_HEADERS = $(wildcard emulator/drivers/common/*.h)
#DRVS_COM_OBJ = $(BEAM_SRC:emulator/drivers/%.c=$(OBJ_DIR)/%.o)

DRVS_UNIX_SRC = $(wildcard emulator/drivers/unix/*.c)
#DRVS_UNIX_OBJ = $(BEAM_SRC:emulator/drivers/unix/%.c=$(OBJ_DIR)/%.o)

PCRE_SRC = $(wildcard emulator/pcre/*.c)
PCRE_HEADERS = $(wildcard emulator/pcre/*.h)
#PCRE_OBJ = $(BEAM_SRC:emulator/pcre/%.c=$(OBJ_DIR)/%.o)

ZLIB_SRC = $(wildcard emulator/zlib/*.c)
ZLIB_HEADERS = $(wildcard emulator/zlib/*.h)

LIB_SRC = $(wildcard lib_src/common/*.c)
#LIB_OBJ = $(BEAM_SRC:lib_src/common/%.c=$(OBJ_DIR)/%.o)

LIB_SRC_PTHREAD = $(wildcard lib_src/pthread/*.c)

# Compile the list of raw sources, then filter out the unnecessary ones.
RAWSOURCES = $(BEAM_SRC) $(UNIX_SRC) $(SYS_COMMON_SRC) $(PLAT_SRC) $(PLAT_OPT_SRC) \
			 $(DRVS_COM_SRC) $(DRVS_UNIX_SRC) $(PCRE_SRC) $(ZLIB_SRC) $(LIB_SRC) \
			 $(LIB_SRC_PTHREAD)
FILTEREDSOURCES = $(filter-out emulator/drivers/unix/sig_drv.c emulator/drivers/unix/multi_drv.c emulator/sys/unix/erl_child_setup.c emulator/pcre/dftables.c emulator/pcre/pcre_latin_1_table.c,$(RAWSOURCES))

# Filter out preload.c, then add it again to make sure there is only one copy in the list.
SOURCES = emulator/x86_64-unknown-linux-gnu/preload.c $(filter-out emulator/x86_64-unknown-linux-gnu/preload.c,$(FILTEREDSOURCES))

HEADERS = x86_64-unknown-linux-gnu/config.h $(BEAM_HEADERS) $(UNIX_HEADERS) $(SYS_COMMON_SRC_HEADERS) $(PLAT_HEADERS) $(PLAT_OPT_HEADERS) $(DRVS_COM_HEADERS) $(PCRE_HEADERS) $(ZLIB_HEADERS)
OBJECTS = $(SOURCES:%.c=%.o)

##################################
######### BUILD THE ERTS #########
##################################

# c_bin creates the C object files

c_bin: $(OBJ_DIR)/erts.a

# Generate the C header files and preloaded Erlang modules.

generate: emulator/x86_64-unknown-linux-gnu/generated

emulator/x86_64-unknown-linux-gnu/generated: emulator/x86_64-unknown-linux-gnu/preload.c
	mkdir -p emulator/x86_64-unknown-linux-gnu/opt/plain
	mkdir -p emulator/x86_64-unknown-linux-gnu/os_bifs
	cp erl_compile_flags.h emulator/x86_64-unknown-linux-gnu/opt/plain/
	cat emulator/beam/bif.tab.src ../os/bif.tab.src > emulator/beam/bif.tab
	(cd emulator/utils && \
		./atom_generator ../../preloaded/ebin/* > ../x86_64-unknown-linux-gnu/atom.names.gen )
	(cat emulator/beam/atom.names.src > emulator/beam/atom.names)
	(cd emulator && LANG=C \
		/usr/bin/perl utils/make_alloc_types \
			-src beam/erl_alloc.types \
			-dst x86_64-unknown-linux-gnu/opt/plain/erl_alloc_types.h \
			unix)
	(cd emulator && LANG=C /usr/bin/perl utils/beam_makeops \
		-wordsize 64 \
		-outdir x86_64-unknown-linux-gnu/opt/plain \
		-DUSE_VM_PROBES=0 \
		-emulator genop.tab beam/ops.tab \
		&& echo genop.tab beam/ops.tab utils/beam_makeops > x86_64-unknown-linux-gnu/opt/plain/OPCODES-GENERATED)
	(cd emulator && LANG=C /usr/bin/perl utils/make_tables \
		-src x86_64-unknown-linux-gnu \
		-include x86_64-unknown-linux-gnu\
		beam/atom.names beam/bif.tab \
		&& echo beam/atom.names beam/bif.tab utils/make_tables > x86_64-unknown-linux-gnu/TABLES-GENERATED)
	(cd emulator && LANG=C /usr/bin/perl utils/make_version \
		-o x86_64-unknown-linux-gnu/erl_version.h \
		17 17.4 6.3 x86_64-unknown-linux-gnu)
	(cd emulator && LANG=C /usr/bin/perl utils/make_driver_tab \
		-o x86_64-unknown-linux-gnu/opt/plain/driver_tab.c \
		-nifs -drivers obj/x86_64-unknown-linux-gnu/opt/plain/efile_drv.o \
		obj/x86_64-unknown-linux-gnu/opt/plain/inet_drv.o \
		obj/x86_64-unknown-linux-gnu/opt/plain/zlib_drv.o \
		obj/x86_64-unknown-linux-gnu/opt/plain/ram_file_drv.o \
		obj/x86_64-unknown-linux-gnu/opt/plain/ttsl_drv.o)
	(cd ../os && make c_bin)
	cp ../os/bin/*.o emulator/x86_64-unknown-linux-gnu/os_bifs
	touch $@

emulator/x86_64-unknown-linux-gnu/preload.c: preloaded/src/*.erl ../os/src/*/*.erl ../os/c_src/*.c
	mkdir -p emulator/x86_64-unknown-linux-gnu/opt/plain
	(cd preloaded/ebin && erlc $(ERL_COMPILE_FLAGS) ../src/*.erl)
	cp ../unikernels/ebin/*.beam preloaded/ebin
	emulator/utils/strip_all preloaded/ebin/*.beam
	emulator/utils/make_preload -old preloaded/ebin/*.beam > emulator/x86_64-unknown-linux-gnu/preload.c

clean_generate: clean_preloaded
	rm -rf emulator/x86_64-unknown-linux-gnu
	rm -f emulator/beam/atom.names
	rm -f emulator/beam/bif.tab

$(OBJ_DIR): generate
	mkdir -p $(OBJ_DIR)

$(OBJ_DIR)/%.o: $(SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

$(OBJ_DIR)/erts.a: $(OBJ_DIR) $(OBJECTS) $(HEADERS)
	$(AR) cr $@ $(OBJECTS) emulator/x86_64-unknown-linux-gnu/os_bifs/*.o

clean: clean_generate 
	rm -rf $(OBJECTS) emulator/drivers/unix/sig_drv.o emulator/sys/unix/erl_child_setup.o
	rm -rf $(OBJ_DIR)/erts.a

clean_preloaded:
	rm -rf preloaded/ebin/*
	rm -f emulator/x86_64-unknown-linux-gnu/preload.c

##################################
############# META ###############
##################################

rebuild: clean all

commit:
	(cd ../../ && make commit)

diff:
	(cd ../../ && make diff)

cscope:
	(cd ../../ && make cscope)
