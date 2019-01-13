; Retreive graphics mode details and store them at 0x600
; INPUT: ax = Mode number
; OUTPUT: None.
get_gfx_mode_info:
	pusha				; Push all registers onto the stack

						; Move options into place for int
	;mov es, 0			; es:di is the output block pointer
	mov di, 0x600		; Destination address
	mov cx, ax			; Move the input mode number
	mov ax, 0xF401		; Function one of the VESA function set
	
	int 0x10			; Execute!

						; TODO: Add error handeling here!

	popa				; Pop all registers from the stack
	ret

; Set the graphics mode
; INPUT: ax = mode number
; OUTPUT: None.
set_gfx_mode:
	pusha

	mov bx, ax			; Move the mode number into position
	and bx, 0x4000		; Put the VESA device in linear address mode
	mov ax, 0x4F02		; Set the function number

	int 0x10

	popa
	ret
