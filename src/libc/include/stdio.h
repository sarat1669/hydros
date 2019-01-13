#ifndef STDIO_H
#define STDIO_H

#include <stub.h>
#include <sys/types.h>
#include <stdarg.h>
#include <errno.h>

#define EOF (-1)

struct __FILE {
  unsigned char *_p;	/* current position in (some) buffer */
  int	_r;		/* read space left for getc() */
  int	_w;		/* write space left for putc() */
  short	_flags;		/* flags, below; this FILE is free if 0 */
  short	_file;		/* fileno, if Unix descriptor, else -1 */
  void* _bf;	/* the buffer (at least 1 byte, if !NULL) */
  int	_lbfsize;	/* 0 or -_bf._size, for inline putc */
};

typedef struct __FILE FILE;


extern FILE * stdin;
extern FILE * stdout;
extern FILE * stderr;

STUBH(printf, (const char *format, ...), int);
STUBH(fprintf, (FILE *stream, const char *format, ...), int);
STUBH(sprintf, (char *str, const char *format, ...), int);
STUBH(fopen, (const char *path, const char *mode), FILE*);
STUBH(fdopen, (int fd, const char *mode), FILE*);
STUBH(fclose, (FILE *fp), int);
STUBH(fread, (void *ptr, size_t size, size_t nmemb, FILE *stream), size_t);
STUBH(fwrite, (const void *ptr, size_t size, size_t nmemb, FILE *stream), size_t);
STUBH(fflush, (FILE *stream), int);
STUBH(putc, (int c, FILE *stream), int);

STUBH(sscanf, (const char *str, const char *format, ...), int);

STUBH(rename, (const char *old, const char *nw), int);

VSTUBH(setbuf, (FILE *stream, char *buf));
STUBH(setvbuf, (FILE *stream, char *buf, int mode, size_t size), int);

STUBH(fileno, (FILE *stream), int);

#endif
