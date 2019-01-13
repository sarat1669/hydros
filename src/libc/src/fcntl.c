#include <stub.h>
#include <stdarg.h>
#include <errno.h>

int fds = 0;

int fcntl(int fildes, int cmd, ...) {
	return 1;
}

int open(const char *path, int oflag, ...) {
	return fds++;
}
