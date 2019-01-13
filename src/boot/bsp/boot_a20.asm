; Enable the A20 line using BIOS interrupt 15,24.
; This method is supported by Bochs, but potentially
; not by all real hardware or other CPU emulators.
enable_a20:
	pusha

	;mov ah, 0x24			; BIOS A20 management functions
	;mov al, 0x01			; Subfunction: enable A20.

	;jc a20_error			; If we are unable to set the line
							; we show a message and then hang.

	in al, 0x92
	or al, 2
	out 0x92, al

	popa
	ret

a20_error:
	mov ax, string_a20_error
	call prints
	jmp hang				; Jump to error hanging state

string_a20_error: db 'Unable to set A20 line.',0
