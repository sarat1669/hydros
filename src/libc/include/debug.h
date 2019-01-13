#ifndef DEBUG_H
#define DEBUG_H

extern int console_printf(char* fmt, ...);
extern int system_get_proc_id(void);

extern unsigned char debugging_enabled;

#define LOGD() DEBUGS("DEBUG!")
#define LOGV(...) console_printf(__VA_ARGS__)
#define LOGE(...) erts_printf(__VA_ARGS__);
#define CLOGD() if(IS_DEBUG()) LOGD()
#define CLOGV(str, ...) if(IS_DEBUG()) LOGV(__VA_ARGS__)
#define DEBUGS(x) console_printf("%s:%s:%d: %s\n", __FILE__, __func__, __LINE__, (x))
#define DEBUGD(x) console_printf("%s:%s:%d: %d\n", __FILE__, __func__, __LINE__, (x))
#define DEBUGH(x) console_printf("%s:%s:%d: %h\n", __FILE__, __func__, __LINE__, (x))
#define DEBUG_ON() debugging_enabled = 1
#define DEBUG_OFF() debugging_enabled = 0
#define IS_DEBUG() (debugging_enabled)
#define BREAK() asm("xchg %bx, %bx\n")
#define IFBREAK(x) if((x)) BREAK()
#define CBREAK(x) IFBREAK((x) == system_get_proc_id())
#define DHANG() { DEBUG(); HANG(); }
#define HANG() asm("xchg %bx, %bx\nmovl 0xDEAD, %eax\nhalt: jmp halt")

// Cast a pointer to a u64int, so that it can be printed without compiler warnings.
#define LPTR (u64int)

/* SCRATCH AREA */
// This zone should be used for temporary debugging functions only

// Visually test whether the code form (am_*) of an atom is equal to it's runtime form
#define AM_CHECK(x) LOGE("TESTING ATOM: " #x " == %T (%d)\n", x, x)


/* END OF SCRATCH AREA */

#endif
