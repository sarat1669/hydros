#ifndef FCNTL_H
#define FCNTL_H

#include <sys/types.h>
#include <stub.h>
#include <stdarg.h>

#define O_ACCMODE     0003
#define O_RDONLY      00
#define O_WRONLY      01
#define O_RDWR        02

# define O_CREAT      0100
# define O_EXCL       0200
# define O_NOCTTY     0400
# define O_TRUNC      01000
# define O_APPEND     02000
# define O_NONBLOCK   04000
# define O_SYNC       04010000

/* Values for the second argument to `fcntl'.  */
#define F_DUPFD		0	/* Duplicate file descriptor.  */
#define F_GETFD		1	/* Get file descriptor flags.  */
#define F_SETFD		2	/* Set file descriptor flags.  */
#define F_GETFL		3	/* Get file status flags.  */
#define F_SETFL		4	/* Set file status flags.  */

#define R_OK	4		/* Test for read permission.  */
#define W_OK	2		/* Test for write permission.  */
#define X_OK	1		/* Test for execute permission.  */
#define F_OK	0		/* Test for existence.  */

# define SEEK_SET	0	/* Seek from beginning of file.  */
# define SEEK_CUR	1	/* Seek from current position.  */
# define SEEK_END	2	/* Seek from end of file.  */

# define S_IRUSR	__S_IREAD       /* Read by owner.  */
# define S_IWUSR	__S_IWRITE      /* Write by owner.  */
# define S_IXUSR	__S_IEXEC       /* Execute by owner.  */
/* Read, write, and execute by owner.  */
# define S_IRWXU	(__S_IREAD|__S_IWRITE|__S_IEXEC)

# define S_IRGRP	(S_IRUSR >> 3)  /* Read by group.  */
# define S_IWGRP	(S_IWUSR >> 3)  /* Write by group.  */
# define S_IXGRP	(S_IXUSR >> 3)  /* Execute by group.  */
/* Read, write, and execute by group.  */
# define S_IRWXG	(S_IRWXU >> 3)

# define S_IROTH	(S_IRGRP >> 3)  /* Read by others.  */
# define S_IWOTH	(S_IWGRP >> 3)  /* Write by others.  */
# define S_IXOTH	(S_IXGRP >> 3)  /* Execute by others.  */
/* Read, write, and execute by others.  */
# define S_IRWXO	(S_IRWXG >> 3)

# define S_IFMT		__S_IFMT
# define S_IFDIR	__S_IFDIR
# define S_IFCHR	__S_IFCHR
# define S_IFBLK	__S_IFBLK
# define S_IFREG	__S_IFREG
# define S_IFIFO	__S_IFIFO
# define S_IFLNK	__S_IFLNK

# define S_ISUID	__S_ISUID       /* Set user ID on execution.  */
# define S_ISGID	__S_ISGID       /* Set group ID on execution.  */

STUBH(fcntl, (int fildes, int cmd, ...), int);
STUBH(open, (const char *path, int oflag, ...), int);
STUBH(close, (int x), int);

#endif
