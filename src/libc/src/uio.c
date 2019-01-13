#include <stub.h>
#include <sys/uio.h>
#include <sys/types.h>

STUB(writev, (int fildes, const struct iovec *iov, int iovcnt), ssize_t, -1);
