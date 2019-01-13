#########################################
################# META ##################
#########################################

.DEFAULT_GOAL = all

rules:
	make -qp | awk -F':' '/^[a-zA-Z0-9][^$$#\/\t=]*:([^=]|$$)/ {split($$1,A,/ /);for(i in A)print A[i]}' | sort

rebuild: clean all

cscope:
	cscope -R -b
	gvim --cmd ":cs add cscope.out ." -t erl_start

#########################################
############### RELEASE #################
#########################################

MAJOR_VERSION=`cat VERSION`
COMMIT=`hg identify --num`
VERSION=$(MAJOR_VERSION).$(COMMIT)

release: release-src release-bin

release-src: clean
	(tar -czf "hydros-src-$(VERSION).tar.gz" *)

release-bin: all
	cp $(BUILD)/hydros.img "hydros-$(VERSION).img"

#########################################
############# OS SIMULATION #############
#########################################

SIM_CPU = 'cpu: count=1:4:1 model=corei7_haswell_4770'
SIM_CPU2 = 'cpu: count=1:2:1 model=corei7_haswell_4770'
SIM_CPU8 = 'cpu: count=1:8:1 model=corei7_haswell_4770'
SIM_CPU16 = 'cpu: count=2:8:1 model=corei7_haswell_4770'
SIM_CPU32 = 'cpu: count=4:8:1 model=corei7_haswell_4770'
SIM_CPU48 = 'cpu: count=6:8:1 model=corei7_haswell_4770'

SIM_ARGS = -q
SIM_GUI = 'display_library: x, options="gui_debug"'

sim: all
	bochs $(SIM_ARGS) $(SIM_CPU)

sim2: all
	bochs $(SIM_ARGS) $(SIM_CPU2)

sim8: all
	bochs $(SIM_ARGS) $(SIM_CPU8)

sim16: all
	bochs $(SIM_ARGS) $(SIM_CPU16)

sim32: all
	bochs $(SIM_ARGS) $(SIM_CPU32)

sim48: all
	bochs $(SIM_ARGS) $(SIM_CPU48)

sim_gui: all
	bochs $(SIM_ARGS) $(SIM_CPU) $(SIM_GUI)

gdb-ready: all
	objcopy --only-keep-debug build/kernel.elf build/gdb-kernel.sym

qemu: gdb-ready all
	qemu-system-x86_64 -hda build/hydros.img -smp cpus=4 -m 2048 -monitor stdio -S

qemu2: gdb-ready all
	qemu-system-x86_64 -hda build/hydros.img -smp cpus=2 -m 2048 -monitor stdio -S

kvm2: all
	qemu-system-x86_64 -enable-kvm -hda build/hydros.img -smp cpus=2 -m 2048 -monitor stdio -s -S

kvm: all
	qemu-system-x86_64 -enable-kvm -hda build/hydros.img -smp cpus=4 -m 2048 -monitor stdio -s -S

kvm8: all
	qemu-system-x86_64 -enable-kvm -hda build/hydros.img -smp cpus=8 -m 2048 -monitor stdio -s -S

kvm16: all
	qemu-system-x86_64 -enable-kvm -hda build/hydros.img -smp cpus=16 -m 2048 -monitor stdio -s -S

#########################################
############### OS IMAGE ################
#########################################

# Loopback device rules

loop: all
	sudo losetup /dev/loop0 build/hydros.img

parted:
	sudo parted /dev/loop0

fdisk:
	sudo fdisk -l /dev/loop0

dloop:
	sudo losetup -d /dev/loop0

reloop: dloop loop

### USB image writing rules.
### These rules will allow you to write a copy of the OS onto a USB stick for
### testing purposes. However, these rules are extremely dangerous, as they
### write bytes directly to a block device. The default target is /dev/sdc.
### IF YOU DO NOT WANT TO WIPE sdc, you must call make as follows:
###		'make usb USB_TARGET=your_target'
### If you do accidentally overwrite the start of your disk, dd the
### .disk_backup.img file back over the offending area. This file will have
### been created just before the device was overwritten.

USB_TARGET := sdc

usb: all usb_present usb_is_small
	sudo dd if=/dev/$(USB_TARGET) of=.disk_backup.img bs=512 count=16384
	@echo ">>> The start of the write device has been saved to '.disk_backup.img'."
	sudo dd if=build/hydros.img of=/dev/$(USB_TARGET) bs=512
	@echo ">>> The image has been copied."
	sudo sync /dev/$(USB_TARGET)
	sudo eject /dev/$(USB_TARGET)
	@echo ">>> If you have just dd'd over something you shouldn't have, dd " \
		"the '.disk_backup.img' file back over the start of the device quickly."

usb_present:
	@lsblk /dev/$(USB_TARGET) > /dev/null
	@echo ">>> The USB stick appears to present."

usb_is_small:
	# Confirming that the drive you are about to write to is smaller than 9GB
	# (as in, not a desktop computer's harddrive).
	test `lsblk /dev/$(USB_TARGET) -b -o SIZE | tail -n 1` -lt 9000000000
	@echo ">>> The stated drive is less than 9GB in size."

#########################################
########### BUILD VARIABLES #############
#########################################

include crosscompiler.mk

DBGCFLAGS = -g
#OPTFLAGS = -Os -flto
OPTFLAGS = 
CFLAGS = $(DBGCFLAGS) $(OPTFLAGS) -std=gnu11 -nostdinc -nostdlib -mno-red-zone -mno-sse -fno-exceptions -m64 -W -Wall $(INCLUDES)

BUILD = build
KERNEL_C = $(wildcard src/kernel/*.c)
KERNEL_HEADERS = $(wildcard src/kernel/include/*.h)
KERNEL_C_OBJ = $(KERNEL_C:src/kernel/%.c=$(BUILD)/%.o)
KERNEL_ASM_OBJ = $(BUILD)/boot_entry_point.asm.o $(BUILD)/ints.asm.o
KERNEL_OBJ = $(KERNEL_ASM_OBJ) $(KERNEL_C_OBJ)

all: $(BUILD) $(BUILD)/hydros.img $(BUILD)/kernel.sym


#########################################
############# BUILD THE OS ##############
#########################################

erts: preloaded
	(cd src/vm && make -j8)

preloaded: unikernels
	(cd src/os && make -j8)
	(cd src/vm && make generate)

unikernels: libkernel libc $(BUILD)/bootloader.bin
	(cd src/unikernels && make -j8)

libc:
	(cd src/libc && make -j8)

libkernel:
	(cd src/libkernel && make -j8)

$(BUILD):
	mkdir -p $@

$(BUILD)/bsp_start: src/boot/bsp/bsp_start.asm src/boot/bsp/*.asm
	nasm -f bin -o $@ $< -I src/boot/bsp/

$(BUILD)/bsp_stage2: src/boot/bsp/bsp_stage2.asm src/boot/bsp/*.asm
	nasm -f bin -o $@ $< -I src/boot/bsp/

$(BUILD)/bsp_stage3_entry_point.asm.o: src/boot/bsp/bsp_stage3_entry_point.asm
	nasm $< -f elf64 -o $@

$(BUILD)/bsp_stage3.o: src/boot/bsp/bsp_stage3.c $(KERNEL_HEADERS)
	$(CC) $(CFLAGS) -ffreestanding -c $< -o $@
	
$(BUILD)/bsp_stage3: $(BUILD)/bsp_stage3_entry_point.asm.o $(BUILD)/bsp_stage3.o
	$(LD) -m elf_x86_64 -o $@ -Ttext 0x8200 \
		$(BUILD)/bsp_stage3_entry_point.asm.o \
		$(BUILD)/bsp_stage3.o --oformat binary
	truncate -s 16384 $(BUILD)/bsp_stage3

$(BUILD)/ap_start: src/boot/ap/ap_start.asm src/boot/ap/*.asm
	nasm -f bin -o $@ $< -I src/boot/ap/

$(BUILD)/rms_start: src/boot/rms/rms_start.asm src/boot/rms/*.asm
	nasm -f bin -o $@ $< -I src/boot/rms/ -I src/boot/bsp/

$(BUILD)/kernel.bin: $(BUILD)/kernel.elf
	objcopy -O binary $^ $@

$(BUILD)/kernel.elf: libkernel libc erts
	$(LD) $(OPTFLAGS) -m elf_x86_64 -T link.lds \
		-o $@ --oformat elf64-x86-64 \
		src/libkernel/build/main.o \
		--whole-archive src/libkernel/libkernel.a  \
		--whole-archive src/libc/libc.a  \
		--whole-archive src/vm/bin/erts.a \
		--as-needed $(CROSS_INSTALL_DIR)/lib/gcc/x86_64-unknown-elf/*/libgcc.a

$(BUILD)/kernel.sym: $(BUILD)/kernel.elf
	nm $^ | grep "T" | sort | awk '{ print $$1 " " $$3 }' | grep -v "^U" > $@

$(BUILD)/bootloader.bin: $(BUILD)/bsp_start $(BUILD)/bsp_stage2 $(BUILD)/bsp_stage3 $(BUILD)/rms_start $(BUILD)/ap_start
	cat $^ > $@
	truncate -s 32256 $@

$(BUILD)/hydros.img: $(BUILD)/bootloader.bin $(BUILD)/kernel.bin
	cat $^ > $@
	truncate -s 8192000 $@

clean:
	rm -rf $(BUILD)
	rm -rf release
	-find . -name "*scope.out" -type f -delete
	rm -f .disk_backup.img
	(cd src/unikernels && make clean)
	(cd src/libkernel && make clean)
	(cd src/libc && make clean)
	(cd src/vm && make clean)
	(cd src/os && make clean)

#########################################
############ VIM SHORTCUTS ##############
#########################################

EDIT_PROG = gvim
EDIT_CONFIG = Makefile .bochsrc
EDIT_KERNEL = src/kernel/*{.asm,.c} src/kernel/include/*
EDIT_BOOT = src/boot/*/*
EDIT_DOC = doc/*
EDIT_SETUP = -c split -c vsplit -c "resize 35"

edit:
	$(EDIT_PROG) $(EDIT_CONFIG) $(EDIT_KERNEL) $(EDIT_BOOT) $(EDIT_SETUP) &

edit_kernel:
	$(EDIT_PROG) $(EDIT_KERNEL) $(EDIT_SETUP) &

edit_boot:
	$(EDIT_PROG) $(EDIT_BOOT) $(EDIT_SETUP) &

edit_config:
	$(EDIT_PROG) $(EDIT_CONFIG) $(EDIT_SETUP) &

edit_doc:
	$(EDIT_PROG) $(EDIT_DOC) &

make: clean
	vim Makefile

make_erts: clean
	vim src/erts/Makefile

brc:
	vim .bochsrc


#########################################
########### REPO MANAGEMENT #############
#########################################

status: clean
	hg status

history:
	hg history | tac

commit:
	EDITOR=vim hg commit
	hg push

update: clean
	hg pull
	hg update

diff:
	hg extdiff -p meld

#########################################
########### REPO STATISTICS #############
#########################################

sizes: all
	head -c 510 build/bsp_start | sed -r "s/\x00+$$//g" | wc -c
	cat build/bsp_stage2 | sed -r "s/\x00+$$//g" | wc -c
	cat build/bsp_stage3 | sed -r "s/\x00+$$//g" | wc -c
	cat build/rms_start | sed -r "s/\x00+$$//g" | wc -c
	cat build/ap_start | sed -r "s/\x00+$$//g" | wc -c
	wc -c < build/kernel.bin

stats:
	pepper filetypes
	pepper punchcard
	pepper loc
	pepper activity
	cloc .

todo:
	grep --recursive -H -n --color TODO src/
	grep --recursive TODO src/ | wc --lines
