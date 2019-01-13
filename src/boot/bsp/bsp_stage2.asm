[org 0x7e00]

boot_stage2:
	; We are now in the sector that was loaded from the drive. We have 1kb
	; available to this stage at the moment, but that can be increased
	; trivially in bsp_start.asm.
	mov ax, string_reached_s2 ; Load the welcome string into ax.
	call prints


	call enable_a20			; Enable the a20 line
	mov ax, string_a20_set
	call prints

;	mov ax, 0x12
;	call get_gfx_mode_info	; Store the graphics mode information, for later
							; use in the kernel.
;	mov ax, 0x12
;	call set_gfx_mode		; Set the graphics mode, if it needs changing

	jmp switch_to_pm		; Let's switch to protected mode.

hang:						; Error landing spot.
	cli						; Hang forever.
	hlt

%include "boot_a20.asm"		; Include all of the 16bit code from we switch
%include "boot_gfx.asm"		; Include all of the 16bit code from we switch
%include "boot_read.asm"
%include "boot_print.asm"
%include "boot_gpt.asm"
%include "boot_pm.asm"		; At the end of this file we are in 32 bit
							; protected mode.

string_reached_s2: db 'Successfully reached stage2',10,13,0
string_a20_set: db 'Set the a20 line',10,13,0

pm_ret:						; Boot loader entry point after pm switch.
	jmp switch_to_lm

%include "boot_lm.asm"		; At the end of this file we are in 64 bit
							; protected mode.

lm_ret:						; We are now in 64 bit mode
	mov eax, 0x9fc00		; Give the stack 56kb
	mov ebp, eax			; Move the value we have just calculated into
							; the base pointer.
	mov esp, ebp			; Move the base pointer into the stack pointer

	jmp 0x8200				; Jump to stage3

times 1024-($-$$) db 0		; Pad the rest of the sectors
