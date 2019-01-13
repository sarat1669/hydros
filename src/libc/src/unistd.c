#include <stub.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <stdarg.h>

STUB(access, (const char * c, int x), int, EINVAL);
STUB(alarm, (unsigned x), unsigned, 0);
STUB(chdir, (const char * c), int, ENOENT);
STUB(chown, (const char * c, uid_t x, gid_t y), int, ENOENT);

int close(int x) {
	return 0;
}

STUB(dup2, (int fildes, int fildes2), int, EBADF);
VSTUB(_exit, (int status));
STUB(execle, (const char *path, const char *arg, ...), int, -1);
STUB(execve, (const char *path, char *const argv[], char *const envp[]), int, -1);
STUB(fork, (void), pid_t, -1);
STUB(fsync, (int fildes), int, EBADF);
STUB(ftruncate, (int fildes, off_t length), int, EBADF);
STUB(getcwd, (char *buf, size_t size), char*, NULL);
STUB(gethostname, (char *name, size_t namelen), int, -1);
STUB(getpid, (void), pid_t, -1);
STUBQ(isatty, (int fd), int, 0);
STUB(link, (const char *path1, const char *path2), int, -1);
STUB(lseek, (int fildes, off_t offset, int whence), off_t, -1);
STUB(nice, (int incr), int, -1);

int pipe(int fildes[2]) {
	return 0;
}

STUB(readlink, (const char *path, char *buf, size_t bufsize), size_t, -1);
STUB(rmdir, (const char *path), int, -1);
STUB(setsid, (void), pid_t, -1);
STUB(symlink, (const char *path1, const char *path2), int, -1);
STUB(sysconf,(int name), long, -1);
STUB(ttyname, (int fd), char*, NULL);
STUB(unlink, (const char *path), int, ENOENT);
STUB(write, (int __fd, const void *__buf, size_t __n), ssize_t, ENOENT);
STUB(read, (int __fd, void *__buf, size_t __nbytes), ssize_t, ENOENT);

