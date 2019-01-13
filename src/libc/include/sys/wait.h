#ifndef WAIT_H
#define WAIT_H

/* Bits in the third argument to `waitpid'.  */
#define	WNOHANG		1	/* Don't block waiting.  */
#define	WUNTRACED	2	/* Report status of stopped children.  */

/* Bits in the fourth argument to `waitid'.  */
#define WSTOPPED	2	/* Report stopped child (same as WUNTRACED). */
#define WEXITED		4	/* Report dead child.  */
#define WCONTINUED	8	/* Report continued child.  */
#define WNOWAIT		0x01000000 /* Don't reap, just poll status.  */

/* If WIFEXITED(STATUS), the low-order 8 bits of the status.  */
#define WEXITSTATUS(status)	(((status) & 0xff00) >> 8)

#define WIFSIGNALED(status) \
  (((signed char) (((status) & 0x7f) + 1) >> 1) > 0)

/* If WIFSIGNALED(STATUS), the terminating signal.  */
#define	WTERMSIG(status)	((status) & 0x7f)

STUBH(waitpid, (pid_t pid, int *stat_loc, int options), pid_t);

#endif
