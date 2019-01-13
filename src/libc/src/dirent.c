#include <dirent.h>
#include <stub.h>
#include <errno.h>

STUB(closedir, (DIR *dirp), int, EBADF);
STUB(opendir, (const char *name), DIR*, ENOENT);
STUB(readdir, (DIR *dirp), struct dirent *, EBADF);
