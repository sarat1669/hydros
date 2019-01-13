gdt:					; The GPT itself.
						; We have 3 entries: null, code and data.
						; Code and data both cover 0 - 4gb
gdt_null:				; Catches segment register not set bugs.
	dd 0x0				; 8 bytes of 0.
	dd 0x0

gdt_code:				; Define the code segment.
	dw 0xffff			; Bits 0-15 of the limit.
	dw 0x0				; Bits 0-15 of the base address.
	db 0x0				; Bits 16-23 of the base address.
	db 10011010b		; Present, highest priv level (2 bits), code/data descriptor
						; Code, not conforming (protected), readbale, not accessed
	db 11001111b		; Granularity (limit*16), 32-bit, not 64-bit, AVL, bits 16-19 of limit.
	db 0x0				; Bits 24-31 of the base address.

gdt_data:				; The same as the code segment, except that the 4th lowest bit in the 
						; first set of flags is 0 (indicating data not code).
	dw 0xffff			; Bits 0-15 of the limit.
	dw 0x0				; Bits 0-15 of the base address.
	db 0x0				; Bits 16-23 of the base address.
	db 10010010b		; Present, highest priv level (2 bits), code/data descriptor
						; Data, not conforming (protected), readbale, not accessed
	db 11001111b		; Granularity (limit*16), 32-bit, not 64-bit, AVL, bits 16-19 of limit.
	db 0x0				; Bits 24-31 of the base address.

gdt_end:				; We mark this so that the gdt descriptor knows where the GDT ends.

gdt_descriptor:
	dw gdt_end - gdt - 1; Calculate the length of the GDT.
	dd gdt				; Note the location that the GDT starts at.

; Define constants that detail the descriptor locations for the code and data segs.

CODE_SEG equ gdt_code - gdt
DATA_SEG equ gdt_data - gdt
