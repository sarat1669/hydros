#include <ipc.h>
#include <debug.h>
#include <string.h>

/* A unikernel that takes a string and a number of times to hash it.
 * Once it has hashed the string the number of times required, the
 * result is sent to the requester. */

struct request_packet {
	long pid;
	long repetitions;
	char text[256];
} __attribute__((packed));

struct response_packet {
	long pid;
	unsigned long hash;
} __attribute__((packed));

// The DJB2 hashing function.
unsigned long hash_string(unsigned char *str) {
	unsigned long hash = 5381;
	unsigned char c;

	while (c = *str++)
		hash = ((hash << 5) + hash) + c;

	return hash;
}

int kmain(int p_id) {
	struct request_packet req;
	struct response_packet res;

	// Init necessary OS services.
	system_set_proc_id(p_id);
	console_init();
	idt_init();

	// Init the IPC module.
	ipc_init(p_id);

	while(1) {
		// The main server loop.
		if(ipc_receive(&req)) {
			// We got a message. Process it and prepare a response.
			res.pid = req.pid;

			// Hash the value the given number of times.
			while(req.repetitions--) {
				// Hash the value and write the number as a string
				// into the request packet.
				res.hash = hash_string(req.text);
				itoa(res.hash, req.text);
			}
			// Dispatch the response packet.
			ipc_send(0, &res, sizeof(struct response_packet));
		}
	}

	return 0;
}
