#ifndef STUB_H
#define STUB_h

#include <debug.h>

// Macros that allow one line stubs of library functions.

#define STUBH(name,args,ret_type) ret_type name args;

#define STUB(name,args,ret_type,ret) ret_type name args { console_printf("WARN: Called undefined function '%s'.\n", __func__); {while(1);}; return ret; }
#define STUBQ(name,args,ret_type,ret) ret_type name args { return ret; }

// We need a special case macro for void functions, as they don't return.

#define VSTUBH(name,args) void name args;
#define VSTUB(name,args) void name args { console_printf("WARN: Called undefined function '%s'.\n", __func__); {while(1);}; }
#define VSTUBQ(name,args) void name args {}

#endif
