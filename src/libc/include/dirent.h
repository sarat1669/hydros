#ifndef DIRENT_H
#define DIRENT_H

#include <sys/types.h>
#include <stub.h>
#include <errno.h>

struct dirent {
    ino_t          d_ino;       /* inode number */
    off_t          d_off;       /* offset to the next dirent */
    unsigned short d_reclen;    /* length of this record */
    unsigned char  d_type;      /* type of file; not supported
                                   by all file system types */
    char           d_name[256]; /* filename */
};

typedef struct dirent DIR;

STUBH(closedir, (DIR *dirp), int);
STUBH(opendir, (const char *name), DIR*);
STUBH(readdir, (DIR *dirp), struct dirent *);

#endif
