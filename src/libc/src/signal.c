#include <stub.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>

STUBQ(sigaction, (int sig, const struct sigaction *act, struct sigaction *oact), int, 0);
STUB(sigaddset, (sigset_t *set, int signum), int, EINVAL);
STUBQ(sigemptyset, (sigset_t *set), int, 0);
VSTUB(signal, (int sig, void (*func)(int)));
STUB(sigprocmask, (int how, const sigset_t *set, sigset_t *oset), int, EAGAIN);
STUB(sigwait, (const sigset_t *set, int *sig), int, EINVAL);

STUB(pthread_sigmask, (int how, const sigset_t *set, sigset_t *oldset), int, EAGAIN);

VSTUB(abort, (void));
