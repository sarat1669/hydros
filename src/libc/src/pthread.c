#include <pthread.h>
#include <errno.h>
#include <sys/types.h>
#include <stub.h>

STUB(pthread_cond_broadcast, (pthread_cond_t *cond), int, EINTR);
STUB(pthread_cond_destroy, (pthread_cond_t *cond), int, EAGAIN);
STUB(pthread_cond_init, (pthread_cond_t *cond, const pthread_condattr_t *attr), int, EAGAIN);
STUB(pthread_cond_signal, (pthread_cond_t *cond), int, EINTR);
STUB(pthread_cond_wait, (pthread_cond_t *cond, pthread_mutex_t *mutex), int, ENOTRECOVERABLE);

STUB(pthread_getspecific, (pthread_key_t key), void*, NULL);
STUB(pthread_setspecific, (pthread_key_t key, const void *value), int, NULL);

STUB(pthread_mutex_destroy, (pthread_mutex_t *mutex), int, EAGAIN);
STUB(pthread_mutex_init, (pthread_mutex_t *mutex, const pthread_mutexattr_t *attr), int, EAGAIN);
STUB(pthread_mutex_lock, (pthread_mutex_t *mutex), int, EAGAIN);
STUB(pthread_mutex_trylock, (pthread_mutex_t *mutex), int, EAGAIN);
STUB(pthread_mutex_unlock, (pthread_mutex_t *mutex), int, EAGAIN);

STUB(pthread_spin_lock, (pthread_spinlock_t *lock), int, EBUSY);
STUB(pthread_spin_destroy, (pthread_spinlock_t *lock), int, EAGAIN);
STUB(pthread_spin_init, (pthread_spinlock_t *lock, int pshared), int, EAGAIN);
STUB(pthread_spin_unlock, (pthread_spinlock_t *lock), int, EPERM);

STUB(pthread_attr_init, (pthread_attr_t *attr), int, 0);
STUB(pthread_attr_destroy, (pthread_attr_t *attr), int, 0);
STUB(pthread_attr_setdetachstate, (pthread_attr_t *attr, int detachstate), int, 0);
STUB(pthread_attr_setscope, (pthread_attr_t *attr, int scope), int, EINVAL);
STUB(pthread_attr_setstacksize, (pthread_attr_t *attr, size_t stacksize), int, EINVAL);

STUB(pthread_create, (pthread_t *thread, const pthread_attr_t *attr, void *(*start_routine) (void *), void *arg),
	int, EAGAIN);
STUB(pthread_detach, (pthread_t thread), int, EINVAL);
STUB(pthread_equal, (pthread_t t1, pthread_t t2), int, 0);
VSTUB(pthread_exit, (void *retval));
STUB(pthread_join, (pthread_t thread, void **retval), int, EINVAL);

STUB(pthread_key_create, (pthread_key_t *key, void (*destructor)(void*)), int, EAGAIN);
STUB(pthread_key_delete, (pthread_key_t key), int, EAGAIN);

STUB(pthread_self, (void), pthread_t, 0);

STUB(pthread_yield, (void), int, 0);

STUB(pthread_kill, (pthread_t thread, int sig), int, 0);

STUB(pthread_getname_np, (pthread_t thread, char *name, size_t len), int, 0);
