#include <memory.h>

/* Functions for interacting with multikernel/unikernel
 * communication. */

#define BUF_PTR (msg_bufs[node_id])

#define CMPSWAP(ptr, old, new) \
	__sync_bool_compare_and_swap((ptr), (old), (new))

int node_id;

// Hardcored message buffer locations, for now.
static char* msg_bufs[] =
	{
		(char*) 0x5000,
		(char*) 0x5400, 
		(char*) 0x5800, 
		(char*) 0x5B00
	};

// Lock status enums
enum status { BUF_UNLOCKED = 0, BUF_LOCKED = 1};

// Set the node to receive messages from.
void ipc_init(int id) {
	node_id = id;
}

// Procure a lock in order to read the buffer.
void set_locked(int target, char status) {
	char* addr = msg_bufs[target];
	if(status == BUF_LOCKED) {
		// Wait for the buffer to become free
		// Then acquire a lock.
		// TODO: Use cmpswap
		while(CMPSWAP(addr, BUF_UNLOCKED, BUF_LOCKED) == 0);
	}
	else if(status == BUF_UNLOCKED) {
		*addr = BUF_UNLOCKED;
	}
}


// Get the length of the current mailbox queue
unsigned int ipc_queue_length() {
	unsigned int count = 0;
	char* msg_buf = BUF_PTR;

	set_locked(node_id, BUF_LOCKED);
	
	// Skip to the start of the first message.
	msg_buf += 2;

	while(*msg_buf != 0) {
		count++;
		// Add the length to the pointer as well as a byte
		// for the type tag.
		msg_buf += *msg_buf + 2;
	}

	set_locked(node_id, BUF_UNLOCKED);

	return count;
}

// Update the status byte of the message buffer.
void touch_status(void) {
	char* msg_buf = BUF_PTR;

	msg_buf++;

	*msg_buf = (char) *msg_buf + 1;
}

// Reads a message into the given buffer, if one is available.
// Returns 0 if no messages are available, otherwise the
// number of bytes that were read into the buffer.
int ipc_receive(void* buf) {
	char msg_len;
	char* msg_buf = BUF_PTR;

	touch_status();

	// Acquire a lock
	set_locked(node_id, BUF_LOCKED);

	// Increment the msg_buf to the start of the message queue.
	msg_buf += 2;

	if(*msg_buf == 0) {
		// The message buffer is empty. Unlock and return 0.
		set_locked(node_id, BUF_UNLOCKED);
		return 0;
	}

	// There is a message in the queue!
	// Copy it into the given message buffer and return length.
	msg_len = *msg_buf;

	// Reset the buffer. It doesn't matter that we do this
	// before copying the buffer because it is locked.
	*msg_buf = 0;

	// Skip to start of message
	msg_buf += 2;
#if 0
	console_printf("Copying %d bytes from %h to %h.\n",
			msg_len,
			(unsigned long) msg_buf,
			(unsigned long) buf);
#endif
	// Copy the message into the buffer.
	// Make sure to jump over the type tag.
	memcpy(buf, msg_buf, msg_len);

	// Unlock the buffer.
	set_locked(node_id, BUF_UNLOCKED);

	return msg_len;
}

// Send a message to another node.
// If the other node is a multikernel, the first 8 bytes need to
// be a PID. If it is a unikernel, the message can start
// immediately.
int ipc_send(int target, void* msg, char msg_len) {
	char* msg_buf = msg_bufs[target];

	touch_status();
	
	// Acquire a lock
	set_locked(target, BUF_LOCKED);

	// Move to the first length byte.
	msg_buf += 2;

	// Scan through the message queue until you find the end.
	while(*msg_buf)
		msg_buf += *msg_buf + 2;

	// Set the length of the message.
	*(msg_buf++) = msg_len;
	
	// Set the message type.
	*(msg_buf++) = 1;

	// Copy the message into the buffer.
	memcpy(msg_buf, msg, msg_len);

	// Set the length of the next message to zero.
	// This signifies the end of the message queue.
	msg_buf += msg_len + 1;
	*msg_buf = 0;

	set_locked(target, BUF_UNLOCKED);

	return 1;
}
