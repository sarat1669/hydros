#include <stub.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>

STUB(socket, (int domain, int type, int protocol), int, -1);

STUB(accept, (int socket, struct sockaddr *address, socklen_t *address_len), int, -1);
STUB(bind, (int socket, const struct sockaddr *address, socklen_t address_len), int, EADDRNOTAVAIL);

STUB(getsockname, (int socket, struct sockaddr *address, socklen_t *address_len), int, EBADF);
STUB(getsockopt, (int socket, int level, int option_name, void *option_value, socklen_t *option_len), int, EBADF);
STUB(getpeername, (int socket, struct sockaddr *address, socklen_t *address_len), int, EINVAL);

STUB(connect, (int socket, const struct sockaddr *address, socklen_t address_len), int, EACCES);

STUB(sendto, (int socket, const void *message, size_t length, int flags, const struct sockaddr *dest_addr, socklen_t dest_len), ssize_t, -1);
STUB(send, (int socket, const void *buffer, size_t length, int flags), ssize_t, -1);

STUB(listen, (int socket, int backlog), int, EBADF);

STUB(recv, (int socket, void *buffer, size_t length, int flags), ssize_t, EOPNOTSUPP);
STUB(recvfrom, (int socket, void *buffer, size_t length, int flags, struct sockaddr *address, socklen_t *address_len), ssize_t, EOPNOTSUPP);

STUB(setsockopt, (int socket, int level, int option_name, const void *option_value, socklen_t option_len), int, EBADF);

STUB(shutdown, (int socket, int how), int, -1);
