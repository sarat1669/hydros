#########################################
################# META ##################
#########################################

# Alter this to reflect the kernels you
# would like to be loaded into the system.
# The format is <node_id, kernel_location>.
# For example:
#     KERNELS = 1 test.bin
#
#The nodes are zero indexed.
#
# The kernel does not need a bootloader,
# it just needs to be a standard, flat,
# x86_64 compiled binary (no ELF, etc).
KERNELS =
#KERNELS = 1 kernels/hasher.bin

.DEFAULT_GOAL = all

OUTPUT_MOD = ebin/os_unikernels.erl
BOOTLOADER = ../../build/bootloader.bin

rebuild: clean all

commit:
	(cd ../../ && make commit)

diff:
	(cd ../../ && make diff)

cscope:
	(cd ../../ && make cscope)


#########################################
################ RULES ##################
#########################################

# This file can be modified to build your
# kernels too, if you please. Add the main
# kernel binary to the '$(OUTPUT_MOD)' rule
# as a dependency, then add the rest of your
# build system (or get Make to call your
# own Makefile recursively).

all: $(OUTPUT_MOD)

$(OUTPUT_MOD): ebin kernels
	utils/generate $(BOOTLOADER) $(KERNELS)

ebin:
	mkdir -p ebin

kernels: make_libuni
	make -C kernels

make_libuni:
	make -C libuni

clean:
	rm -rf ebin/*
	make -C libuni clean
	make -C kernels clean
