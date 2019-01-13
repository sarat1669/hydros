#include <stub.h>
#include <sys/types.h>

STUB(htonl, (uint32_t hostlong), uint32_t, 0);
STUB(htons, (uint16_t hostshort), uint16_t, 0);
STUB(ntohs, (uint16_t netshort), uint16_t, 0);
