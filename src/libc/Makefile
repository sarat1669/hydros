##################################
###### META AND VARIABLES ########
##################################

.DEFAULT_GOAL = libc.a

OBJ_DIR = bin

include ../../crosscompiler.mk

INCLUDES = -isystem include \
           -isystem ../ \
           -isystem ../libkernel/include \
		   -isystem $(CROSS_INSTALL_DIR)/lib/gcc/x86_64-unknown-elf/*/include/

#OPTFLAGS = -Os -flto
OPTFLAGS =
CFLAGS = -ffreestanding -nostdinc -nostdlib -mno-red-zone -m64 $(OPTFLAGS) $(INCLUDES)

C_SOURCES =  $(wildcard src/*.c)
HEADERS =  $(wildcard include/*.h)
HEADERS += $(wildcard include/*/*.h)

C_OBJECTS = $(C_SOURCES:%.c=%.o)

libc.a: $(C_OBJECTS) $(ASM_OBJECTS) $(HEADERS)
	$(AR) cr $@ $(C_OBJECTS) $(ASM_OBJECTS)

$(OBJ_DIR)/%.o: $(C_SOURCES) $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf src/*.o
	rm -rf asm_src/*.o
	rm -rf libc.a

##################################
############# META ###############
##################################

rebuild: clean libc.a

commit:
	(cd ../../ && make commit)

diff:
	(cd ../../ && make diff)

cscope:
	(cd ../../ && make cscope)
