[org 0x7c00] ; The BIOS drops us at 0x7c00, so all of our references should be ref+0x7c00.

; This is the main bootloader entry point from the BIOS.
boot_main:
	mov ax, 0
	mov ds, ax
	mov es, ax
	mov ss, ax
;	jmp 0:enforce_cs

enforce_cs:
	mov bp, 0x7bff			; Allow ourselves ~30kb of stack for now
	mov sp, bp				; The stack pointer must be equal to the stack base to start.

store_drive:
	; Store the boot drive in the shared information area
	mov [0x6000 + 0x00], dl

check_lba_extensions:
	mov ah, 0x41
	mov bx, 0x55aa
	mov dl, [0x6000 + 0x00]
	int 0x13
	jnc read_kernel
;	mov ax, s_lba_support
;	call prints


read_kernel:
	mov si, LBA_PACKET
	mov dl, [0x6000 + 0x00]
	mov ah, 0x42
	int 0x13
	jmp print

s_read: db 'Successfully read stage2',10,13,0
print:
	mov ax, s_read		; Load the read string into ax.
	call prints

	jmp 0x7e00				; Jump to stage2.

LBA_PACKET:
		db		0x10
		db		0
.count:	dw		62
.addr:	dw		0x7e00
		dw		0
.loc:	dq		0x1

%include "boot_print.asm"

hang: jmp $

s_lba_support: db 'Machine does not have required BIOS LBA extensions.',10,13,0

times 510-($-$$) db 0		; Pad the rest of the sector
dw 0xAA55					; The magic boot signature
