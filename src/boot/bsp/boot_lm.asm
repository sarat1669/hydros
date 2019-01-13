switch_to_lm:
	mov edi, 0x1000				; Set the destination index to 0x1000.
	mov cr3, edi				; Set control register 3 to the destination index.
	xor eax, eax				; Nullify the A-register.
	mov ecx, 0x1000				; Set the C-register to 4096.
	rep stosd					; Clear the memory.

make_pages:
	mov edi, 0x1000				; Load the PML4 address
	mov DWORD [edi], 0x2003		; PML4[0] -> PDPT
	mov edi, 0x2000				; Load the PDPT[0] address
	mov DWORD [edi], 0x3003		; PDPT[0] -> DirectoryPtr

	mov edi, 0x3000				; Load the DirectoryPtr

.fill_dir:
	xor ebx, ebx				; Clear the PDE register.
	or ebx, 1 << 7				; Set the 2MB page flag
	or ebx, 0x3					; Set the starting PDE

	mov ecx, 512				; Page the first GB.

.make_entry:
	mov DWORD [edi], ebx		; Move the PDPTE into position
	add ebx, 0x200000			; The PDE[n+1] will be for +2MB
	add edi, 8					; Increment the PDPTE pointer
	loop .make_entry

make_lapic_entry:				; Page the LAPIC area so we can start APs.
	mov ebx, 0xfee00000			; Clear the PDE register.
	or ebx, 1 << 7				; Set the 2MB page flag
	or ebx, 0x3					; Page is present and R/W

	mov edi, 0x2018				; Load the address of PDPT[3]
	mov DWORD [edi], 0x4003		; PDPT[3] -> DirectoryPtr
	mov edi, 0x4000 + (503 * 8) ; PD[503]
	mov DWORD [edi], ebx

switch_to_lm_late:
	mov eax, cr4				; Set the A-register to control register 4.
	or eax, 1 << 5				; Set the PAE-bit, which is the 6th bit (bit 5).
	mov cr4, eax

	mov ecx, 0xC0000080			; Set the C-register to 0xC0000080, which is the
								; EFER MSR.

	rdmsr						; Read from the model-specific register.
	or eax, 1 << 8				; Set the LM-bit which is the 9th bit (bit 8).
	wrmsr						; Write to the model-specific register.

	mov eax, cr0				; Set the A-register to control register 0.
	or eax, 1 << 31				; Set the PG-bit, which is the 32nd bit (31).
	mov cr0, eax				; Set control register 0 to the A-register.
	lgdt [GDT64.Pointer]		; Load the 64-bit global descriptor table.
	jmp GDT64.Code:Realm64		; Set the code segment and enter 64-bit mode.

GDT64:							; Global Descriptor Table (64-bit).
	.Null: equ $ - GDT64		; The null descriptor.
	dw 0						; Limit (low).
	dw 0						; Base (low).
	db 0						; Base (middle)
	db 0						; Access.
	db 0						; Granularity.
	db 0						; Base (high).
	.Code: equ $ - GDT64		; The code descriptor.
	dw 0						; Limit (low).
	dw 0						; Base (low).
	db 0						; Base (middle)
	db 10011010b				; Access.
	db 00100000b				; Granularity.
	db 0						; Base (high).
	.Data: equ $ - GDT64		; The data descriptor.
	dw 0						; Limit (low).
	dw 0						; Base (low).
	db 0						; Base (middle)
	db 10010010b				; Access.
	db 00000000b				; Granularity.
	db 0						; Base (high).
	.Pointer:					; The GDT-pointer.
	dw $ - GDT64 - 1			; Limit.
	dq GDT64					; Base.


[bits 64]
 
Realm64:
	cli							; Clear the interrupt flag.
	mov ax, GDT64.Data			; Set the A-register to the data descriptor.
	mov ds, ax					; Set the data segment to the A-register.
	mov ss, ax					; Set the data segment to the A-register.
	mov es, ax					; Set the extra segment to the A-register.
	mov fs, ax					; Set the F-segment to the A-register.
	mov gs, ax					; Set the G-segment to the A-register.

	jmp lm_ret
