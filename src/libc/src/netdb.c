#include <netdb.h>
#include <stub.h>
#include <sys/types.h>

STUB(getservbyname, (const char *name, const char *proto), struct servent *, NULL);
STUB(getservbyport, (int port, const char *proto), struct servent *, NULL);
