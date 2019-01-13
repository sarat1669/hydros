#include <stub.h>
#include <libkernel/include/string.h>
#include <sys/types.h>

size_t strlen(const char* s) {
	return kstrlen( (char*) s);
}

char basic_strcmp(char* s1, char* s2) {

	while(*s1 && *s2)
		if(*s1++ != *s2++)
			return -1;
	
	return (*s1 + *s2);
}

int strcmp(const char* s1, const char* s2) {
	return basic_strcmp( (char*) s1, (char*) s2);
}

STUB(strncmp, (const char* s1, const char* s2, size_t n), int, NULL);

char* strcpy(char* dest, const char* src) {
	kstrcpy(dest, (char*) src);

	return dest;
}

STUB(strncpy, (char* dest, const char* src, size_t n), char*, NULL);

STUB(strstr, (const char *haystack, const char *needle), char*, NULL);

STUB(strcat, (char *dest, const char *src), char*, NULL);
STUB(strncat, (char *dest, const char *src, size_t n), char*, NULL);

STUB(strerror, (int errnum), char*, NULL);

STUB(strchr, (const char *s, int c), char *, NULL);

int atoi(const char *s) {
	int n;

	n = 0;
	while('0' <= *s && *s <= '9')
		n = n*10 + *s++ - '0';
	return n;
}

// From K&R. 

void reverse(char s[]) {
	int i, j;
	char c;

	for (i = 0, j = strlen(s) - 1; i<j; i++, j--) {
		c = s[i];
		s[i] = s[j];
		s[j] = c;
	}
}

void itoa(int n, char* s) {
	int i, sign;

	if ((sign = n) < 0)  /* record sign */
		n = -n;          /* make n positive */

	i = 0;
	
	do {				 	     /* generate digits in reverse order */
		s[i++] = n % 10 + '0';   /* get next digit */
	} while ((n /= 10) > 0);     /* delete it */

	if (sign < 0)
		s[i++] = '-';

	s[i] = '\0';
	reverse(s);
 }

STUB(strtol, (const char *nptr, char **endptr, int base), long int, 0);
STUB(strtod, (const char *nptr, char **endptr), double, 0);
