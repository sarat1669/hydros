#include <stub.h>
#include <errno.h>
#include <sys/types.h>

char **environ;

unsigned char debugging_enabled = 0;

STUB(bsearch, (const void *key, const void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *)), void *, NULL);

STUB(putenv, (char *string), int, ENOMEM);

char* getenv(const char *name) {
	return NULL;
}

STUB(unsetenv, (const char *name), int, -1);

void exit(int status) {
	console_prints("EXIT REACHED: Halt and Catch Fire\n");
	console_printf("STATUS: %d\n", status);
}

