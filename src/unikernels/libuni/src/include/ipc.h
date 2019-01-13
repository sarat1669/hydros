#ifndef IPC_H
#define IPC_H

void ipc_init(int);

int ipc_receive(void*);

int ipc_send(int target, void* msg, char msg_len);

unsigned int ipc_queue_length(void);

#endif
