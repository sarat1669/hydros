#ifndef STDLIB_H
#define STDLIB_H

#include <sys/time.h>
#include <sys/types.h>
#include <memory.h>
#include <signal.h>
#include <math.h>
#include <string.h>

#define BUFSIZ 8192

/* The possibilities for the third argument to `setvbuf'.  */
#define _IOFBF 0		/* Fully buffered.  */
#define _IOLBF 1		/* Line buffered.  */
#define _IONBF 2		/* No buffering.  */

extern char **environ;

STUBH(bsearch, (const void *key, const void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *)), void *);

STUBH(putenv, (char *string), int);
STUBH(getenv, (const char *name), char*);
STUBH(unsetenv, (const char *name), int);

VSTUBH(exit, (int status));

VSTUBH(qsort, (void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *)));


#endif
