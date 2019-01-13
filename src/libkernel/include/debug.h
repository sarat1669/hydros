#ifndef DEBUG_H
#define DEBUG_H

#include <system.h>
#include <console.h>

#define DEBUG() DEBUGS("DEBUG!")
#define DEBUGS(x) console_printf("%s:%s:%d: %s\n", __FILE__, __func__, __LINE__, (x))
#define DEBUGD(x) console_printf("%s:%s:%d: %d\n", __FILE__, __func__, __LINE__, (x))
#define DEBUGH(x) console_printf("%s:%s:%d: %h\n", __FILE__, __func__, __LINE__, (x))
#define BREAK() asm("xchg %bx, %bx\n")
#define IFBREAK(x) if((x)) BREAK()
#define CBREAK(x) IFBREAK((x) == system_get_proc_id())
#define CHANG(x) { if(x == system_get_proc_id()) HANG(); }
#define DHANG() { DEBUG(); HANG(); }
#define HANG() asm("xchg %bx, %bx\nmovl 0xDEAD, %eax\nhlt")

#define PRIM_PVAL(x, y, n) { *((char*)0xb8000 + (x * 2) + (y * 2 * 80)) = ((char)0x30+n); }
#define PRIM_CVAL(x, y, c) { *((char*)0xb8000 + (x * 2) + (y * 2 * 80)) = ((char)c); }

#endif
