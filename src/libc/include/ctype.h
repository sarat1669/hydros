#ifndef CTYPE_H
#define CTYPE_H
#include <stub.h>
#include <locale.h>

STUBH(isalnum, (int c), int);
STUBH(isalpha, (int c), int);
STUBH(iscntrl, (int c), int);
STUBH(isgraph, (int c), int);
STUBH(islower, (int c), int);
STUBH(isprint, (int c), int);
STUBH(ispunct, (int c), int);
STUBH(isspace, (int c), int);
STUBH(isupper, (int c), int);
STUBH(isdigit, (int c), int);
STUBH(isxdigit, (int c), int);

STUBH(tolower, (int c), int);
STUBH(toupper, (int c), int);

#endif
