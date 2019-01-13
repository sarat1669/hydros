#include <stub.h>
#include <termios.h>

STUB(tcgetattr, (int fd, struct termios *termios_p), int, -1);
STUB(tcsetattr, (int fd, int optional_actions, const struct termios *termios_p), int, -1);
