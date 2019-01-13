#ifndef UTIME_H
#define UTIME_H

#include <sys/time.h>

/* Structure describing file times.  */
struct utimbuf
  {
    time_t actime;		/* Access time.  */
    time_t modtime;		/* Modification time.  */
  };

#endif
