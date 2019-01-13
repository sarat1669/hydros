#ifndef PTHREAD_H
#define PTHREAD_H
#include <sys/types.h>
#include <stub.h>

#define __SIZEOF_PTHREAD_ATTR_T 56
#define __SIZEOF_PTHREAD_MUTEX_T 40
#define __SIZEOF_PTHREAD_MUTEXATTR_T 4
#define __SIZEOF_PTHREAD_COND_T 48
#define __SIZEOF_PTHREAD_CONDATTR_T 4
#define __SIZEOF_PTHREAD_RWLOCK_T 56
#define __SIZEOF_PTHREAD_RWLOCKATTR_T 8
#define __SIZEOF_PTHREAD_BARRIER_T 32
#define __SIZEOF_PTHREAD_BARRIERATTR_T 4

typedef unsigned long int pthread_t;

/* Keys for thread-specific data */
typedef unsigned int pthread_key_t;

typedef volatile int pthread_spinlock_t;

typedef struct __pthread_internal_list
{
  struct __pthread_internal_list *__prev;
  struct __pthread_internal_list *__next;
} __pthread_list_t;

/* Data structures for mutex handling.  The structure of the attribute
   type is not exposed on purpose.  */
typedef union
{
  struct __pthread_mutex_s
  {
    int __lock;
    unsigned int __count;
    int __owner;
    unsigned int __nusers;
    /* KIND must stay at this position in the structure to maintain
       binary compatibility.  */
    int __kind;
    short __spins;
    short __elision;
    __pthread_list_t __list;
# define __PTHREAD_MUTEX_HAVE_PREV	1
/* Mutex __spins initializer used by PTHREAD_MUTEX_INITIALIZER.  */
# define __PTHREAD_SPINS             0, 0
  } __data;
  char __size[__SIZEOF_PTHREAD_MUTEX_T];
  long int __align;
} pthread_mutex_t;

/* Data structure for conditional variable handling.  The structure of
   the attribute type is not exposed on purpose.  */
typedef union
{
  struct
  {
    int __lock;
    unsigned int __futex;
    unsigned long long int __total_seq;
    unsigned long long int __wakeup_seq;
    unsigned long long int __woken_seq;
    void *__mutex;
    unsigned int __nwaiters;
    unsigned int __broadcast_seq;
  } __data;
  char __size[__SIZEOF_PTHREAD_COND_T];
  long long int __align;
} pthread_cond_t;

typedef union
{
  char __size[__SIZEOF_PTHREAD_CONDATTR_T];
  int __align;
} pthread_condattr_t;

typedef union
{
  char __size[__SIZEOF_PTHREAD_MUTEXATTR_T];
  int __align;
} pthread_mutexattr_t;

union pthread_attr_t
{
  char __size[__SIZEOF_PTHREAD_ATTR_T];
  long int __align;
};

typedef union pthread_attr_t pthread_attr_t;

/* Scope handling.  */
enum
{
  PTHREAD_SCOPE_SYSTEM,
#define PTHREAD_SCOPE_SYSTEM    PTHREAD_SCOPE_SYSTEM
  PTHREAD_SCOPE_PROCESS
#define PTHREAD_SCOPE_PROCESS   PTHREAD_SCOPE_PROCESS
};

/* Detach state.  */
enum
{
  PTHREAD_CREATE_JOINABLE,
#define PTHREAD_CREATE_JOINABLE	PTHREAD_CREATE_JOINABLE
  PTHREAD_CREATE_DETACHED
#define PTHREAD_CREATE_DETACHED	PTHREAD_CREATE_DETACHED
};

STUBH(pthread_cond_broadcast, (pthread_cond_t *cond), int);
STUBH(pthread_cond_destroy, (pthread_cond_t *cond), int);
STUBH(pthread_cond_init, (pthread_cond_t *cond, const pthread_condattr_t *attr), int);
STUBH(pthread_cond_signal, (pthread_cond_t *cond), int);
STUBH(pthread_cond_wait, (pthread_cond_t *cond, pthread_mutex_t *mutex), int);

STUBH(pthread_getspecific, (pthread_key_t key), void*);
STUBH(pthread_setspecific, (pthread_key_t key, const void *value), int);

STUBH(pthread_mutex_destroy, (pthread_mutex_t *mutex), int);
STUBH(pthread_mutex_init, (pthread_mutex_t *mutex, const pthread_mutexattr_t *attr), int);
STUBH(pthread_mutex_lock, (pthread_mutex_t *mutex), int);
STUBH(pthread_mutex_trylock, (pthread_mutex_t *mutex), int);
STUBH(pthread_mutex_unlock, (pthread_mutex_t *mutex), int);

STUBH(pthread_spin_lock, (pthread_spinlock_t *lock), int);
STUBH(pthread_spin_destroy, (pthread_spinlock_t *lock), int);
STUBH(pthread_spin_init, (pthread_spinlock_t *lock, int pshared), int);
STUBH(pthread_spin_unlock, (pthread_spinlock_t *lock), int);


STUBH(pthread_attr_init, (pthread_attr_t *attr), int);
STUBH(pthread_attr_destroy, (pthread_attr_t *attr), int);
STUBH(pthread_attr_setdetachstate, (pthread_attr_t *attr, int detachstate), int);
STUBH(pthread_attr_setscope, (pthread_attr_t *attr, int scope), int);
STUBH(pthread_attr_setstacksize, (pthread_attr_t *attr, size_t stacksize), int);

STUBH(pthread_create, (pthread_t *thread, const pthread_attr_t *attr, void *(*start_routine) (void *), void *arg),
	int);
STUBH(pthread_detach, (pthread_t thread), int);
STUBH(pthread_equal, (pthread_t t1, pthread_t t2), int);
VSTUBH(pthread_exit, (void *retval));
STUBH(pthread_join, (pthread_t thread, void **retval), int);

STUBH(pthread_key_create, (pthread_key_t *key, void (*destructor)(void*)), int);
STUBH(pthread_key_delete, (pthread_key_t key), int);

STUBH(pthread_self, (void), pthread_t);

STUBH(pthread_yield, (void), int);

STUBH(pthread_getname_np, (pthread_t __target_thread, char *__buf,
			       size_t __buflen), int);

STUBH(pthread_kill, (pthread_t thread, int sig), int);

STUBH(pthread_getname_np, (pthread_t thread, char *name, size_t len), int);

#endif
