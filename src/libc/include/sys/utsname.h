#ifndef UTSNAME_H
#define UTSNAME_H

#define _UTSNAME_LENGTH 65

/* Structure describing the system and machine.  */
struct utsname {
    /* Name of the implementation of the operating system.  */
    char sysname[_UTSNAME_LENGTH];
    /* Name of this node on the network.  */
    char nodename[_UTSNAME_LENGTH];
    /* Current release level of this implementation.  */
    char release[_UTSNAME_LENGTH];
    /* Current version level of this release.  */
    char version[_UTSNAME_LENGTH];
    /* Name of the hardware type the system is running on.  */
    char machine[_UTSNAME_LENGTH];
};

STUBH(uname, (struct utsname *name), int);

#endif
