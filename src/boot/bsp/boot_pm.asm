; This file handles loading the GPT and switching to protected mode.

; Before we switch out of real mode, disable the pesky watchdog timer
switch_to_pm:
	cli					; Stop the real mode interupts firing until we have setup
						; the protected mode interupt vectors.
	lgdt [gdt_descriptor]	; Load the GDT!
	mov eax, cr0		; Load the current value of the control register.
	or eax, 0x1			; Set the protected mode bit.
	mov cr0, eax		; Load it back into the control register.
	jmp CODE_SEG:pm		; Jump to our 32-bit protected mode code!

;;;;;;;;;;;;;;;;;;;;;;;
;;;; PROTECTED MODE ;;;
;;;;;;;;;;;;;;;;;;;;;;;

[bits 32]
pm:						; The landing spot for the far jump to destroy the real mode pipelining.
	mov ax, DATA_SEG	; Set all of the segment registers to refer to the DATA_SEG.
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
	mov ss, ax

	jmp pm_ret			; Jump back to main control flow.
