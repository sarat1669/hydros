; This file represents the landing position of each new Real Mode Slave (RMS) core.

[org 0xc200]
[bits 16]									; We are in real mode when the new RMS starts

rms_filter:
	mov al, 1								; Atomically set and check the 'claimed' byte
	LOCK xchg al, [0x6000 + 0x01]
	cmp al, 1
	jne rms_start							; If lock is already taken, hlt with no interrupts
	cli
	hlt

rms_start:
	mov bp, 0x7b00							; Allow ourselves ~30kb of stack
	mov sp, bp								; The stack pointer must be equal to the stack base to start.

	mov ax, s_reading						; Load the read string into ax.
	call prints

.main_loop:
	call wait_for_work						; Wait for the BSP to issue an instruction

.read:
	mov si, 0xBC00							; The shared LBA packet is here
	mov ah, 0x42							; Function 42: extended read
	mov dl, [0x6000 + 0x00]					; Load the drive number
	int 0x13

	call notify								; Print the '.' to the console


	call set_done							; Notify the BSP that we are ready for more work

	jmp .main_loop

;;;;;;;;;;;;;;;
;; FUNCTIONS ;;
;;;;;;;;;;;;;;;

; NOTIFY CONSOLE

notify:
	pusha

	mov ax, s_notify
	call prints

	popa
	ret

; REPORT THAT DATA IS READY TO READ

set_done:
	pusha

	mov [dword 0x6000 + 0x06], byte 0x0

	popa
	ret

; WAIT UNTIL READY

wait_for_work:
	pusha

.loop:
	mov ax, [dword 0x6000 + 0x06]		; Read the ready byte
										; 'ready' is defined as the slave has a block
	cmp ax, 0x0							; Is the master done consuming the last block?
	je .loop

	popa
	ret

; RESET THE DISK MOTOR/STATE

reset_disk:
	pusha

	mov ah, 0
	mov dl, 0x80
	int 0x13
	jnc .reset_ok
.reset_failed:
	mov ax, s_reset_error
	call prints

	popa
	ret
.reset_ok:
	mov ax, s_reset_ok
	call prints

	popa
	ret

check_status:
	pusha

	mov ah, 1
	mov dl, 0x80
	int 0x13

	push ax

	cmp al, 0
	je .done

	mov ax, s_check_disk_error
	call prints

	pop ax
	add al, 0x30
	call printc

	mov ax, s_nl
	call prints

	popa
	ret
.done:
	pop ax

	mov ax, s_check_ok
	call prints
	popa
	ret

s_reading: db 'Reading kernel',10,13,0
s_notify: db '.',0
s_check_ok: db 'Checked disk is OK.',10,13,0
s_check_disk_error: db 'Check disk failed, error: ',0
s_nl: db 10,13,0
s_reset_ok: db 'Reset disk OK.',10,13,0
s_reset_error: db 'Error resetting disk.',10,13,0

hang:
	cli
	hlt

%include "boot_print.asm"

times 512-($-$$) db 0		; Pad the rest of the sector so that we can be loaded easily.
