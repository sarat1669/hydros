; Read sectors from the disk into memory through the BIOS LBA interrupts
; INPUT:
;	ax = number of sectors to read.
;	bx = destination address
;	cx = sector offset on disk (lo)
;	dx = sector offset on disk (hi)

read:
.start:
	pusha
	

	mov [LBAPACK.count], ax			; Load the number of sectors to read
	mov [LBAPACK.dest], bx			; Load the destination address
	mov [LBAPACK.offset_lo1], cl	; Load the sector offset (lo)
	mov [LBAPACK.offset_lo2], ch	; Load the sector offset (lo2)
	mov [LBAPACK.offset_hi1], dl	; Load the sector offset (hi)
	mov [LBAPACK.offset_hi2], dh	; Load the sector offset (hi2)

	xor ax, ax						; Reset the segment registers
	mov ds, ax
	mov es, ax

	mov si, LBAPACK

	xchg bx, bx
	mov dl, [0x6000 + 0x00]			; Load the drive from the shared data
	mov ah, 0x42					; Select extra read functionality
	int 0x13						; Execute read
	jc .error						; Show an error if the operation failed

	popa
	ret

.error:
	mov ax, s_disk_error			; Load the disk read error string
	call prints
.reset:
	mov ax, 0
	int 0x13
.restart:
	popa
	jmp .start						; Restart.

s_disk_error: db 'Error while reading disk. Retrying...',10,13,0

; The data structure used for LBA reading
LBAPACK:
.pack_sz		db	0x10			; The size of the packet
				db	0x00			; (always zero)
.count			dw	0x0000			; The number of sectors to load
.dest			dw	0x0000			; The address to load the data to
				dw	0x0000			; Memory page
.offset_lo1		dd	0				; The block to read from
.offset_lo2		dd	0				; Block to read (high space)
.offset_hi1		dd	0				; The block to read from
.offset_hi2		dd	0				; Block to read (high space)
