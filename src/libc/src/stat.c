#include <sys/types.h>
#include <stub.h>
#include <sys/stat.h>
#include <errno.h>

STUB(chmod, (const char *path, mode_t mode), int, ENOENT);
STUB(stat, (const char *path, struct stat *buf), int, ENOENT);
STUB(lstat, (const char *path, struct stat *buf), int, ENOENT);

STUB(mkdir, (const char *path, mode_t mode), int, EACCES);
