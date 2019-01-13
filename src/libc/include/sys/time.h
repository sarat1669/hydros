#ifndef SYS_TIME_H
#define SYS_TIME_H

#include <sys/types.h>
#include <utime.h>
#include <stub.h>

typedef long int __time_t;
typedef __time_t time_t;

typedef long int __clock_t;
typedef __clock_t clock_t;

struct timeval {
  time_t      tv_sec;
  suseconds_t tv_usec;
};

struct tm
{
  int   tm_sec;
  int   tm_min;
  int   tm_hour;
  int   tm_mday;
  int   tm_mon;
  int   tm_year;
  int   tm_wday;
  int   tm_yday;
  int   tm_isdst;
  long  __TM_GMTOFF;
  const char *__TM_ZONE;
};

struct tms
{
    clock_t tms_utime;		/* User CPU time.  */
    clock_t tms_stime;		/* System CPU time.  */

    clock_t tms_cutime;		/* User CPU time of dead children.  */
    clock_t tms_cstime;		/* System CPU time of dead children.  */
};

struct timespec
  {
    __time_t tv_sec;		/* Seconds.  */
    __syscall_slong_t tv_nsec;	/* Nanoseconds.  */
  };

typedef int clockid_t;

/* Identifier for system-wide realtime clock.  */
#   define CLOCK_REALTIME		0
/* Monotonic system-wide clock.  */
#   define CLOCK_MONOTONIC		1
/* High-resolution timer from the CPU.  */
#   define CLOCK_PROCESS_CPUTIME_ID	2
/* Thread-specific CPU-time clock.  */
#   define CLOCK_THREAD_CPUTIME_ID	3
/* Monotonic system-wide clock, not adjusted for frequency scaling.  */
#   define CLOCK_MONOTONIC_RAW		4
/* Identifier for system-wide realtime clock, updated only on ticks.  */
#   define CLOCK_REALTIME_COARSE	5
/* Monotonic system-wide clock, updated only on ticks.  */
#   define CLOCK_MONOTONIC_COARSE	6
/* Monotonic system-wide clock that includes time spent in suspension.  */
#   define CLOCK_BOOTTIME		7
/* Like CLOCK_REALTIME but also wakes suspended system.  */
#   define CLOCK_REALTIME_ALARM		8
/* Like CLOCK_BOOTTIME but also wakes suspended system.  */
#   define CLOCK_BOOTTIME_ALARM		9
/* Like CLOCK_REALTIME but in International Atomic Time.  */
#   define CLOCK_TAI			11

/* Flag to indicate time is absolute.  */
#   define TIMER_ABSTIME		1


STUBH(gettimeofday, (struct timeval *tp, void *tzp), int);
STUBH(gmtime, (const time_t *timep), struct tm *);

STUBH(times, (struct tms *buffer), clock_t);
STUBH(time, (time_t *tloc), time_t);

STUBH(localtime, (const time_t *timer), struct tm *);

STUBH(ctime, (const time_t *timep), char *);
STUBH(utime, (const char *path, const struct utimbuf *times), int);
STUBH(mktime, (struct tm *tm), time_t);

VSTUBH(tzset, (void));

#endif
