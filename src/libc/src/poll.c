#include <poll.h>
#include <stub.h>
#include <errno.h>

int poll(struct pollfd fds[], nfds_t nfds, int timeout) {
	int i;

#if 0
	LOGV("Sleeping for %d microsecs.\n", timeout);
	for(i = 0; i < nfds;i++)
		LOGV("fd=%d, events=%d, revents=%d\n",
			fds[i].fd,
			fds[i].events,
			fds[i].revents);
#endif

#if 1
	asm volatile("hlt");
#endif

	return 0;
}
