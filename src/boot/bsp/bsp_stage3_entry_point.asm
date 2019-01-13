; This is the main landing point for the kernel after the bootloader has executed.
; This routine must be placed at the beggining of the second sector on disk and
; must be linked against the kernel.

[bits 64]					; We are in 64 bit long mode. Tell the assembler.
[extern stage3_main]		; The file is linked against the object file containing
							; the kernel and the 'main' function.
[global _start]				; Export the start symbol so that ld can find it.


_start:
	call stage3_main		; Execute the stage3 function


	mov rdi, 0				; Tell kmain that we are the first processor

	mov r15, 0x100000
	jmp r15					; Jump to the entry point of the kernel.
