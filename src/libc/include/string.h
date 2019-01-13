#ifndef STRING_H
#define STRING_H
#include <stub.h>
#include <sys/types.h>

STUBH(strlen, (const char* s), size_t);

STUBH(strcmp, (const char *s1, const char* s2), int);
STUBH(strncmp, (const char *s1, const char* s2, size_t n), int);

STUBH(strcpy, (char* dest, const char* src),char*);
STUBH(strncpy, (char* dest, const char* src, size_t n),char*);

STUBH(strstr, (const char *haystack, const char *needle),char*);

STUBH(strcat, (char *dest, const char *src), char*);
STUBH(strncat, (char *dest, const char *src, size_t n), char*);

STUBH(strerror, (int errnum), char*);

STUBH(strchr, (const char *s, int c), char *);

STUBH(atoi, (const char *nptr), int);

STUBH(strtol, (const char *nptr, char **endptr, int base), long int);
STUBH(strtod, (const char *nptr, char **endptr), double);

#endif
