#include <stub.h>
#include <sys/types.h>
#include <stdarg.h>
#include <stdio.h>
#include <errno.h>

FILE * stdin;
FILE * stdout;
FILE * stderr;

void console_printsn(char* str, int n);
void console_printc(char c);

STUB(printf, (const char *format, ...), int, -1);
STUB(fprintf, (FILE *stream, const char *format, ...), int, -1);
STUB(sprintf, (char *str, const char *format, ...), int, -1);
STUB(fopen, (const char *path, const char *mode), FILE*, NULL);
STUB(fdopen, (int fd, const char *mode), FILE*, NULL);
STUB(fclose, (FILE *fp), int, EBADF);
STUB(fread, (void *ptr, size_t size, size_t nmemb, FILE *stream), size_t, 0);

STUBQ(fileno, (FILE *stream), int, 1);

size_t fwrite(const void *str, size_t size, size_t len, FILE *stream) {
	console_printsn((char*) str, len);

	return len;
}

STUB(fflush, (FILE *stream), int, EBADF);
//STUB(putc, (int c, FILE *stream), int, EOF);

int putc(int c, FILE* stream) {
	console_printc(c);
	return c;
}

STUB(sscanf, (const char *str, const char *format, ...), int, EBADF);

STUB(rename, (const char *old, const char *nw), int, -1);

VSTUB(setbuf, (FILE *stream, char *buf));
STUBQ(setvbuf, (FILE *stream, char *buf, int mode, size_t size), int, -1);
