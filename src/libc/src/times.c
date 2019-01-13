#include <stub.h>
#include <sys/time.h>
#include <sys/types.h>

STUB(times, (struct tms *buffer), clock_t, -1);
STUB(time, (time_t *tloc), time_t, -1);

STUB(localtime, (const time_t *timer), struct tm *, NULL);

STUB(ctime, (const time_t *timep), char *, NULL);
STUB(utime, (const char *path, const struct utimbuf *times), int, -1);

STUB(mktime, (struct tm *tm), time_t, NULL);

