/* Functions that interact with PIDs in ways that
 * we cannot do from Erlang */


#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include <system.h>
#include <console.h>


BIF_RETTYPE os_process_get_node_id_1(BIF_ALIST_1) {
	if(is_small(BIF_ARG_1))
		BIF_RET(make_small(BIF_ARG_1));
	else
		BIF_RET(make_small((_GET_PID_DATA(BIF_ARG_1)) >> NODE_DATA_OFFSET));
}

BIF_RETTYPE os_process_pid_to_binary_1(BIF_ALIST_1) {
	unsigned long pid = BIF_ARG_1;
	byte* buf = erts_alloc(ERTS_ALC_T_BINARY_BUFFER, 4);
	Eterm bin;


	memcpy(buf, &pid, 8);

	bin = new_binary(BIF_P, buf, 8);

	BIF_RET(bin);
}

BIF_RETTYPE os_process_binary_to_pid_1(BIF_ALIST_1) {
	Eterm pid;
	u16int bit_offs, bit_size;
	byte* bytes;

	ERTS_GET_BINARY_BYTES(BIF_ARG_1, bytes, bit_offs, bit_size);

	memcpy(&pid, bytes, 8);

	return pid;
}
