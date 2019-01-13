#include <stub.h>
#include <sys/time.h>
#include <sys/types.h>
#include <system.h>


int gettimeofday(struct timeval *tp, void *tzp) {
	tp->tv_sec = (time_t) (system_get_global_time() / 1000000);
	tp->tv_usec = (time_t) system_get_global_time();

	//LOGV("Returning time: %d -> %d (%d)\n",
	//	system_get_global_time(),
	//	tp->tv_sec,
	//	tp->tv_usec);
}

int clock_gettime(clockid_t clk_id, struct timespec *tp) {
	system_set_global_time(
		system_get_global_time() + 1);
	
	tp->tv_sec = (time_t) (system_get_global_time() / 1000000);
	tp->tv_nsec = (time_t) system_get_global_time();

	return 0;
}

STUB(gmtime, (const time_t *timep), struct tm *, NULL);

VSTUBQ(tzset, (void));
