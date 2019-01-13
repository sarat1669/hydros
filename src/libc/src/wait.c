#include <stub.h>
#include <sys/types.h>

STUB(waitpid, (pid_t pid, int *stat_loc, int options), pid_t, -1);
