/* Stubs that are required for the multikernel interrupt
 * subsystem to work in unikernels. They don't do anything. */

void interrupts_kill_process(long stack_addr) {

}

void interrupts_forward(long error, long inum, long addr) {

}
