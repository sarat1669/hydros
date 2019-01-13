; Print a string that is pointed to from ax.
; INPUT: ax = First char of 0 terminated string.
; OUTPUT: None.
prints:
	pusha				; Push all registers onto the stack
	mov bx, ax			; We need ah/al for the BIOS call, so we move the pointer to bx 

.loop:
	mov al, [bx]		; Load the data found in the pointer bx
	
	cmp al, 0			; If we have reached the end of the string, finish.
	je prints_end
	
	call printc			; If not, print the character
;	add al, '0'
;	call printc

;	mov ah, 0x10		; Wait for keyboard input
;	int 0x16

	inc bx				; Move pointer to the next character.
	jmp .loop			; Loop.

prints_end:
	popa				; Pop all registers from the stack
	ret					; 

; Print a character to the screen using BIOS interupts.
; INPUT: al = The character to print.
; OUTPUT: None.
printc:
	pusha
	mov ah, 0x0e		; (int 10, al=[char], ah=0x0e) == BIOS teletype print char.
	int 0x10
	popa
	ret
