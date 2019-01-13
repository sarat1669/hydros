#include <stub.h>
#include <sys/select.h>

STUB(select, (int nfds, fd_set *readfds, fd_set *writefds, fd_set *errorfds, struct timeval *timeout), int, -1);
