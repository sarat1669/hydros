#include <console.h>
#include <debug.h>
#include <memory.h>
#include <string.h>
#include <stdarg.h>
#include <system.h>
#include <ctype.h>

// Internal function defintions
void increment_cursor_pos(void);
void increment_line_pos(void);

// Store our cursor struct
ConsoleCursor cursor = { 0, 0 };

char screen;
char screens;
char x, y, width, height, text_attr;

void console_init(void) {
	char attrs[] = TEXT_ATTRS;
	screen = system_get_proc_id();
	screens = system_get_cores();

	width = VID_WIDTH/2;
	x = (screen % 2) ? (VID_WIDTH/2) : 0;

	height = VID_HEIGHT / (screens / 2);
	y = (screen / 2) * height;
	// If this is the last console on this side of the screen
	// we should absorb any excess text rows
	if(screen >= screens - 2)
		height += VID_HEIGHT - ((screens/2) * height);

	text_attr = attrs[screen % NUM_TEXT_ATTRS];

	console_clear();
#if 0
	console_printf(
		"Screens: %d, x: %d, y: %d, width: %d, height: %d.\n",
		screens, x, y, width, height);
#endif
}

void console_init_single(void) {
	char attrs[] = TEXT_ATTRS;
	x = 0;
	y = 0;
	width = VID_WIDTH;
	height = VID_HEIGHT;
	text_attr = attrs[0];
	console_clear();
}

// Reset the entire console to boot state
void console_clear(void) {
	char i = 0;
	
	console_set_cursor_pos(0, 0);

	while (i++ < height)
		console_newline();
	
	console_set_cursor_pos(0, 0);
}

// Clear all consoles. Should only be called at startup.
void console_clear_all(void) {
	char* pos_ptr = (char*) VID_MEM;

	// Write space characters and a black background to the console until the video buffer is exhausted
	while((long) pos_ptr < (long) VID_MEM + (VID_WIDTH * VID_HEIGHT * 2)) {
		*(pos_ptr++) = ' ';
		*(pos_ptr++) = 0x0F;
	}
}

// Print a null terminated string to the console at the current cursor location
void console_prints(char* str) {
	while(*str != 0)
		console_printc(*(str++));
}

// Print a string of given length
void console_printsn(char* str, int n) {
	while(n--)
		console_printc(*(str++));
}

// Prints some text to the center of the current row.
// The text overwrites all text on the current row.
// Attempts to embed the \n character in centered strings
// are not suggested.
void console_prints_centered(char* str) {
	for(cursor.x = 0; (cursor.x < ((width / 2) - (kstrlen(str) / 2))); console_printc(' '));
	
	console_prints(str);

	console_newline();

}

void console_printb(long i) {
	console_printn(i, 2);
	console_prints("b");
}

void console_printd(long i) {
	console_printn(i, 10);
}

void console_printh(long i) {
	console_prints("0x");
	console_printn(i, 16);
}

// Print a number to the console, in the specified base.
void console_printn(long i, int base) {
	char str[8 * sizeof(long)]; // This is more space than is necessary.

	kitoa(i, base, str);

	console_prints(str);
}

int console_printf(char* fmt, ...) {
	int count = 0;
	int attr = 0;
	va_list args;

	va_start(args, fmt);

	while(*fmt != 0) {
		count++;
		if((*fmt == '%') && (count > 0) && (*(fmt-1) != '\\')) {
			fmt++;

			while(isdigit(*fmt)) {
				attr = (attr * 10) + (*fmt++ - 48);
			}
			
			// Format the string with the given type
			switch (*fmt) {
				case 's':
					if(attr)
						console_printsn(va_arg(args, char*), attr);
					else
						console_prints(va_arg(args, char*));
					break;
				case 'd':
					console_printd(va_arg(args, long));
					break;
				case 'b':
					console_printb(va_arg(args, long));
					break;
				case 'h':
					console_printh(va_arg(args, long));
					break;
			}
			attr = 0;
		}
		else
			console_printc(*fmt);

		fmt++;
	}

	return count;
}

char* pos_to_ptr(int x2, int y2) {
	return
		(char *) ((long) (
			VID_MEM
				+ ((x + x2) * 2)
				+ ((y + y2) * 2 * VID_WIDTH)
		));
}

// Print a single character at the cursor location
void console_printc(char c) {
#if 1
	char* p = pos_to_ptr(cursor.x, cursor.y);

	if(c == '\n') {
		console_newline();
		return;
	}
	else if(c == '\t') {
		console_prints("    ");
		return;
	}
	else if(c == '\b') {
		console_delete_char();
		return;
	}

	*p++ = c;
	*p = text_attr;

	increment_cursor_pos();
#else
	port_outb(0xe9, c);
#endif
}

void console_delete_char() {
	decrement_cursor_pos();
	*((char*)pos_to_ptr(cursor.x, cursor.y)) = ' ';
}

// Print a character at a given position on screen. Do not increment cursor.
void console_printc_at(char c, char x2, char y2) {
	char attrs[] = TEXT_ATTRS;
	char* p = pos_to_ptr(x2, y2);
	
	*p++ = c;
	*p = attrs[system_get_proc_id() % NUM_TEXT_ATTRS];
}

// Print a newline onto the console.
// This is implemented by filling the rest of the row with spaces,
// rather than actually forcing a new line.
void console_newline(void) {
	char spaces = width - cursor.x;

	while(spaces-- > 0)
		console_printc(' ');
}

// Clear a given line on the screen.
void console_clear_line(char line) {
	char* p = pos_to_ptr(0, line);
	char i;

	for(i = 0; i < width; i++) {
		*p++ = ' ';
		*p++ = text_attr;
	}
}

// Set the exact cursor position.
void console_set_cursor_pos(char x, char y) {
	cursor.x = x;
	cursor.y = y;
}

// Move the cursor to the next appropriate cell.
void increment_cursor_pos(void) {
	if(++cursor.x >= width) {
		cursor.x = 0;
		increment_line_pos();
	}
}

// Move the cursor to the previous position
void decrement_cursor_pos(void) {
	if(--cursor.x < 0) {
		cursor.x = width - 1;
		cursor.y--;
	}
}

// Start a new line.
// If required, shift all of the prevous lines up one so that a new line is available
// at the bottom of the screen.
void increment_line_pos(void) {
	int i;

	if(++cursor.y >= height) {
		for(i = 0; i < height; i++) {
			kmemcpy(
				pos_to_ptr(0, i),
				pos_to_ptr(0, i + 1),
				width * 2
			);
		}

		cursor.y = height - 1;
	}

	console_clear_line(cursor.y);
}
