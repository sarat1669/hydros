; This file represents the landing position of each new CPU core.

; We are working to the same framework as boot_sect.asm so that
; we can deploy and compile with the same modules. This requires
; care that we check instructions are encoded in the right bit'age,
; as well as having the correct labels.

; To make things more complex we avoid setting up the stack until
; we have reached long mode, so no 'call's are possible.

[org 0xc400]
[bits 16]					; We are in real mode when the new APs start

ap_start:
	jmp switch_to_pm

%include "../bsp/boot_gpt.asm"
%include "../bsp/boot_pm.asm"

pm_ret:
	mov eax, 0x1000
    mov edi, eax			; Load the page table location from the meta-table
    mov cr3, edi			; Set the page table!
	jmp switch_to_lm_late	; Switch to long mode code, after tables are built

%include "../bsp/boot_lm.asm"

lm_ret:

	; Now we need to load the location of the page tables
	mov ebx, 0xfee00020
	mov eax, [ebx]			; Read the local APIC id.
	shr eax, 24				; The ID is 3 bytes shifted to the left

;.filter:
;	cmp eax, 1
;	je .continue
;	cli
;	hlt

.continue:
	mov ebx, eax			; Save the processor ID into edx
	imul eax, 0x4			; Each entry in the meta-table is 4 bytes long
	add eax, 0x500			; Add the offset of the meta-table

	xor rdi, rdi
	mov edi, [eax]			; Load the page table location

	mov cr3, rdi			; Set the page table!

	mov rdi, rbx			; Copy the processor id for
							; passing to the kernel
	mov r15, 0x100000
	jmp r15					; Jump to the entry point of the kernel.


idt_ptr:					; A temporary IDT setup.
	dw		4095			; Used only until we get inside the kernel
	dq		0x10007c00

times 512-($-$$) db 0		; Pad the rest of the sector so that we
							; can be loaded easily.
