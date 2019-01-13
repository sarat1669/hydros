#ifndef NETDB_H
#define NETDB_H

#include <stub.h>
#include <sys/types.h>

/* Description of data base entry for a single service.  */
struct servent
{
  char *s_name;			/* Official service name.  */
  char **s_aliases;		/* Alias list.  */
  int s_port;			/* Port number.  */
  char *s_proto;		/* Protocol to use.  */
};

STUBH(getservbyname, (const char *name, const char *proto), struct servent *);
STUBH(getservbyport, (int port, const char *proto), struct servent *);
#endif
