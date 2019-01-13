#include <sys_info.h>
#include <console.h>

void sys_info_print_types(void) {
	console_prints("Type sizes are as follows:\n");
	
	console_prints("      char: ");
	console_printd(sizeof(char));
	console_newline();
	
	console_prints("      short: ");
	console_printd(sizeof(short));
	console_newline();

	console_prints("      int: ");
	console_printd(sizeof(int));
	console_newline();

	console_prints("      long: ");
	console_printd(sizeof(long));
	console_newline();

	console_prints("      long long: ");
	console_printd(sizeof(long long));
	console_newline();

	console_prints("      float: ");
	console_printd(sizeof(float));
	console_newline();

	console_prints("      double: ");
	console_printd(sizeof(double));
	console_newline();

	console_prints("      long double: ");
	console_printd(sizeof(long double));
	console_newline();
}
