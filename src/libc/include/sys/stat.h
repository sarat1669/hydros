#ifndef STAT_H
#define STAT_H

#include <sys/types.h>
#include <stub.h>


/* Protection bits.  */

#define	__S_ISUID	04000	/* Set user ID on execution.  */
#define	__S_ISGID	02000	/* Set group ID on execution.  */
#define	__S_ISVTX	01000	/* Save swapped text after use (sticky).  */
#define	__S_IREAD	0400	/* Read by owner.  */
#define	__S_IWRITE	0200	/* Write by owner.  */
#define	__S_IEXEC	0100	/* Execute by owner.  */

#define	__S_IFMT	0170000	/* These bits determine file type.  */

/* File types.  */
#define	__S_IFDIR	0040000	/* Directory.  */
#define	__S_IFCHR	0020000	/* Character device.  */
#define	__S_IFBLK	0060000	/* Block device.  */
#define	__S_IFREG	0100000	/* Regular file.  */
#define	__S_IFIFO	0010000	/* FIFO.  */
#define	__S_IFLNK	0120000	/* Symbolic link.  */
#define	__S_IFSOCK	0140000	/* Socket.  */

#define	__S_ISTYPE(mode, mask)	(((mode) & __S_IFMT) == (mask))
#define	S_ISREG(mode)	 __S_ISTYPE((mode), __S_IFREG)

struct stat {
    dev_t st_dev;		/* Device.  */
    unsigned short int __pad1;
    ino_t st_ino;		/* File serial number.	*/
    ino_t __st_ino;			/* 32bit file serial number.	*/
    nlink_t st_nlink;		/* Link count.  */
    mode_t st_mode;		/* File mode.  */
    uid_t st_uid;		/* User ID of the file's owner.	*/
    gid_t st_gid;		/* Group ID of the file's group.*/
    int __pad0;
    dev_t st_rdev;		/* Device number, if device.  */
    unsigned short int __pad2;
    off_t st_size;			/* Size of file, in bytes.  */
    blksize_t st_blksize;	/* Optimal block size for I/O.  */
    blkcnt_t st_blocks;		/* Number 512-byte blocks allocated. */
    time_t st_atime;			/* Time of last access.  */
    unsigned long st_atimensec;	/* Nscecs of last access.  */
    time_t st_mtime;			/* Time of last modification.  */
    unsigned long st_mtimensec;	/* Nsecs of last modification.  */
    time_t st_ctime;			/* Time of last status change.  */
    unsigned long st_ctimensec;	/* Nsecs of last status change.  */
};

STUBH(chmod, (const char *path, mode_t mode), int);
STUBH(stat, (const char *ath, struct stat *buf), int);
STUBH(lstat, (const char *path, struct stat *buf), int);

STUBH(mkdir, (const char *path, mode_t mode), int);

#endif
