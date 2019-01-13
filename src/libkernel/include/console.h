#ifndef CONSOLE_H
#define CONSOLE_H

void console_init(void);
void console_clear(void);
void console_clear_all(void);
int console_printf(char* fmt, ...);
void console_prints(char*);
void console_printsn(char*, int);
void console_prints_centered(char* str);
void console_printc(char);
void console_printn(long, int);
void console_printb(long);
void console_printd(long);
void console_printh(long);
void console_printc_at(char, char, char);
void console_newline(void);
void console_set_cursor_pos(char, char);
void console_clear_line(char);

typedef struct {
	char x;
	char y;
} ConsoleCursor;

extern ConsoleCursor cursor;

#define VID_MEM 0xb8000

#if 1
#define VID_WIDTH 80
#else
#define VID_WIDTH 40
#endif
#define VID_HEIGHT 25

#define TEXT_ATTRS { 0x0F, 0x74, 0x6B, 0x1B, 0x41}
#define NUM_TEXT_ATTRS 5

#endif
