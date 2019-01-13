# Settings for your cross compiler.

# If you did not generate your cross compiler suite with crosstool-ng,
# installing to the default location in ~/x-tools, then you will need 
# to adjust these definitions.

CROSS_PREFIX=x86_64-unknown-elf-
CROSS_INSTALL_DIR=~/x-tools/x86_64-unknown-elf

CC=$(CROSS_PREFIX)gcc
LD=ld
AR=$(CROSS_PREFIX)ar

INCLUDES = -isystem src/libkernel/include \
			 -isystem $(CROSS_INSTALL_DIR)/lib/gcc/x86_64-unknown-elf/*/include/
