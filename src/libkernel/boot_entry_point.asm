; This is the main landing point for the kernel after the bootloader has executed.
; This routine must be placed at the beggining of the second sector on disk and
; must be linked against the kernel.

[bits 64]					; We are in 64 bit long mode. Tell the assembler.
[extern kmain]				; The file is linked against the object file containing
							; the kernel and the 'main' function.
[global _start]				; Export the start symbol so that ld can find it.

_start:
	mov rax, 0x9fbff		; Set the stack statically to ~64kb above the rest of the low memory structure
							; The AP/BSP stacks won't collide because they are paged in different locations
	mov rbp, rax			; Move the value we have just calculated into the base pointer
	mov rsp, rbp			; Move the base pointer into the stack pointer
	push 0xba5e				; Show 'ba5e' at the stack base while we are debugging!

.disable_cache:
;	mov rax,cr0				; Cache enable/disable bit is found at position 30.
;	or eax, 0x40000000
;	mov cr0, rax

.test:
; Prepare for input
;	mov al, 0xed
;	out 0x60, al
.kbd_ack1:
;	in ax, 0x60
;	test ax, 0xfa
;	jne .kbd_ack1
; Set caps lock on
;	mov al, 0x3
;	out 0x60, al
;	jmp $

.enable_sse:
	mov rax, cr0
	and ax, 0xFFFB			; Clear coprocessor emulation CR0.EM
	or ax, 0x2				; Set coprocessor monitoring CR0.MP
	mov cr0, rax
	mov rax, cr4
	or ax, 3 << 9			; Set CR4.OSFXSR and CR4.OSXMMEXCPT at the same time
	mov cr4, rax

	call kmain				; Execute the min function of the kernel!
	xchg bx, bx
	jmp $					; Hang. We should never get here though...
