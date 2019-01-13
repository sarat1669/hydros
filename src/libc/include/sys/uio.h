#ifndef UIO_H
#define UIO_H

#include <sys/types.h>

/* Structure for scatter/gather I/O.  */
struct iovec {
    void *iov_base;	/* Pointer to data.  */
    size_t iov_len;	/* Length of data.  */
};

STUBH(writev, (int fildes, const struct iovec *iov, int iovcnt), ssize_t);
#endif
