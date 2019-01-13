#include <stub.h>
#include <sys/utsname.h>

STUB(uname, (struct utsname *name), int, -1);
