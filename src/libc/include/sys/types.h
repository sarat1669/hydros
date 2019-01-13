#ifndef TYPES_H
#define TYPES_H

typedef char u8int;
typedef short u16int;
typedef int u32int;
typedef long u64int;

typedef u8int uint8_t;
typedef u16int uint16_t;
typedef u32int uint32_t;
typedef u64int uint64_t;

typedef __SIZE_TYPE__ size_t;
typedef long ssize_t;
typedef long time_t;

typedef unsigned int pid_t;
typedef unsigned int mode_t;
typedef unsigned int gid_t;
typedef unsigned int uid_t;
typedef unsigned long int ino_t;
typedef unsigned long int nlink_t;
typedef unsigned long int dev_t;
typedef unsigned long int off_t;
typedef unsigned long int blksize_t;
typedef unsigned long int blkcnt_t;
typedef unsigned int socklen_t;

typedef long suseconds_t;

#define NULL 0

typedef unsigned long u_long;

typedef long int __syscall_slong_t;


#endif
