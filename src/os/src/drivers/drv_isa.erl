-module(drv_isa).
-export([start/0, read/1, write/2]).

%%% A basic ports based device driver. This will be tremendously slow,
%%% due to writing/reading one byte per port operation. Do not expect it
%%% to execute faster than 16 MB/s.

%%% This interface targets ST506 ATA hardware.

%%% Define the slave/master base port addresses
-define(MASTER_BASE, 16#1F0). % The base addr's for the master/slave devices
-define(SLAVE_BASE, 16#170).

%%% Define the ports with their offsets
-define(DATA, 0).
-define(ERROR, 1). % Read only
-define(SECTOR_COUNT, 2).
-define(SECTOR_NUM, 3).
-define(CYLINDER_LOW, 10). % The low byte of the cylinder addr
-define(CYLINDER_HIGH, 11). % The rest of the cylinder addr
-define(DRIVE_CYLINDER, 12).
-define(STATUS, 7). % Read the status and set the command share a register
-define(COMMAND, 7).

start() -> ok.

%% A function that reads a sector from a cylinder on the first
%% disk.
read(Sector) ->
	prepare(Sector),
	% The the command to read with retry
	drv_port:out(?MASTER_BASE + ?COMMAND, 16#20),
	wait_while_busy(),
	% This means that 512 bytes have been read.
	% Read 128 quads of bytes from the port
	Data =
		lists:foldl(
			fun(_, Acc) ->
				bin_concat(Acc, read32())
			end,
			<<>>,
			lists:seq(1, 128)
		),
	% Read from the regular status port to ack the transfer.
	finish(),
	Data.

%% Write 512 bytes to a sector on a disk
write(Sector, Data) ->
	prepare(Sector),
	% The the command to read with retry
	drv_port:out(?MASTER_BASE + ?COMMAND, 16#30),
	% Pass to read_bytes/1 to actually receive the request.
	wait_while_busy(),
	% We are now ready to send the bytes
	[
		drv_port:out_bin(?MASTER_BASE + ?DATA, X)
	||
		X <- group(Data)
	],
	finish().

%% Align the drive registers to the correct position, from which we can execute
%% a request.
prepare(Sector) ->
	% Set drive 0, head 0
	drv_port:out(?MASTER_BASE + ?DRIVE_CYLINDER, 16#a0),
	% Set the sector to read from (has to be in the first cylinder, for now
	drv_port:out(?MASTER_BASE + ?SECTOR_NUM, Sector),
	% Set the number of sectors to read
	% NOTE: Bochs implies that this value is zero indexed, yet that doesn't
	% appear to be the case in the documentation?
	drv_port:out(?MASTER_BASE + ?SECTOR_COUNT, 0),
	% Set the cylinder to read from (zero for now)
	drv_port:out(?MASTER_BASE + ?CYLINDER_LOW, 0),
	drv_port:out(?MASTER_BASE + ?CYLINDER_HIGH, 0).

%% Finish a sector read/write.
finish() ->
	% Wait for the BSY bit to clear...
	%os_debug:p(waiting_busy),
	wait_while_busy(),
	%os_debug:p(reading_status),
	% Read the status register
	drv_port:in(?MASTER_BASE + ?STATUS),
	% Flush the cache
	drv_port:out(?MASTER_BASE + ?COMMAND, 16#E7).

wait_while_busy() ->
	os_util:until(
		fun() -> (drv_port:in(?MASTER_BASE + ?COMMAND) band 8) =/= 0 end
	).

read32() -> drv_port:in_bin(?MASTER_BASE + ?DATA).

%% Dirty concatenate binaries. This is slow.
%% We should accept IO lists instead.
bin_concat(B1, B2) -> <<B1/binary, B2/binary>>.

group(<<>>) -> [];
group(<< A:8, B:8, C:8, D:8, Rest/binary >>) ->
	[<< A, B, C, D >>|group(Rest)].
