[extern interrupt_handeler]

; Register order on stack:
; eflags rsi rax rbx r8 r9 r10 r11 r12 r13 r14 r15
; Memory stored registers:
; rdi rcx rdx

%macro ISR_NO_ERR 1	; Define a macro that takes one parameter
[global isr%1]
isr%1:
	cli
;	xchg bx, bx
	mov [0xfe8], rdi; Store rdi, rcx & rdx at special memlocs
	mov [0xff0], rcx
	mov [0xff8], rdx
	mov rcx, rsp	; Store the return address pointer on the stack
	pop rdx			; Get the location that caused the error
	push rdx		; Push the return address back on to the stack
	pushf			; Store the eflags
	push rsi		; Save the rsi register
	mov rdi, 0x0	; Dummy error code
	mov rsi, %1		; The interrupt number
	jmp isr_common
%endmacro

%macro ISR_ERR 1	; Same as before but no need to push a dummy code
[global isr%1]
isr%1:
	cli
;	xchg bx, bx
	mov [0xfe8], rdi; Store rdi, rcx & rdx at special memlocs
	mov [0xff0], rcx
	mov [0xff8], rdx
	mov rcx, rsp	; Store the return address pointer on the stack
	pop rdi
	pop rdx
	push rdx
	pushf			; Store the eflags
	push rsi		; Save the rsi register
	mov rsi, %1		; The interrupt number
	jmp isr_common
%endmacro

isr_common:
.save_regs:
	push rax
	push rbx
	push r8
	push r9
	push r10
	push r11
	push r12
	push r13
	push r14
	push r15
	call interrupt_handeler
.restore_regs:
	pop r15
	pop r14
	pop r13
	pop r12
	pop r11
	pop r10
	pop r9
	pop r8
	pop rbx
	pop rax
	pop rsi
	mov rdi, [0xfe8]; Restore rdi, rcx & rdx from memory
	mov rcx, [0xff0]
	mov rdx, [0xff8]
	popf
.eoi:
	push rax
	mov eax, 0xfee000b0
	mov dword [rax], 0x0 ; Send EOI to the LAPIC
	pop rax
	sti
;	xchg bx, bx
	iretq           ; Return from the interupt.

ISR_NO_ERR 0
ISR_NO_ERR 1
ISR_NO_ERR 2
ISR_NO_ERR 3
ISR_NO_ERR 4
ISR_NO_ERR 5
ISR_NO_ERR 6
ISR_NO_ERR 7
ISR_ERR 8
ISR_NO_ERR 9
ISR_ERR 10
ISR_ERR 11
ISR_ERR 12
ISR_ERR 13
ISR_ERR 14
ISR_NO_ERR 15
ISR_NO_ERR 16
ISR_NO_ERR 17
ISR_NO_ERR 18
ISR_NO_ERR 19
ISR_NO_ERR 20
ISR_NO_ERR 21
ISR_NO_ERR 22
ISR_NO_ERR 23
ISR_NO_ERR 24
ISR_NO_ERR 25
ISR_NO_ERR 26
ISR_NO_ERR 27
ISR_NO_ERR 28
ISR_NO_ERR 29
ISR_NO_ERR 30
ISR_NO_ERR 31
ISR_NO_ERR 32
ISR_NO_ERR 33
ISR_NO_ERR 34
ISR_NO_ERR 35
ISR_NO_ERR 36
ISR_NO_ERR 37
ISR_NO_ERR 38
ISR_NO_ERR 39
ISR_NO_ERR 40
ISR_NO_ERR 41
ISR_NO_ERR 42
ISR_NO_ERR 43
ISR_NO_ERR 44
ISR_NO_ERR 45
ISR_NO_ERR 46
ISR_NO_ERR 47
ISR_NO_ERR 48
