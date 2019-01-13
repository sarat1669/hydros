-module(drv_mp_start).
-export([all/0]).

%%% Manages starting the other processors/cores in the machine.
%%% This includes kernel duplication and unikernel extraction.

%% This is the maximum kernel size. Page tables need to be moved,
%% as well as message buffers in order to increase this value.
-define(KERNEL_SZ, 16#700000).

%% The start address of the kernel in memory.
-define(KERNEL_START, 16#100000).

%% The lcoation of the shared boot system info structure.
-define(SYSINFO_LOC, 16#BA00).

%% The location of the AP bootloader in memory.
-define(AP_BOOTLOADER_LOC, 16#C400).

%% Start all nodes
all() ->
	%io:format("Starting other nodes...~n"),
	UKs = os_unikernels:kernels(),
	Norms = normal_nums(os_system_info:get_num_procs(), UKs),
	UKNums = unikernel_nums(UKs),
	%io:format("Normal kernel nodes: ~p~n", [Norms]),
	%io:format("Unikernel nodes: ~p~n", [UKNums]),
	SysBin = 
		if Norms =/= [] ->
			io:format("Cloning system binary...~n"),
			get_sys_bin();
		true -> undefined
		end,
	lists:foreach(
		fun({Num, Kernel}) ->
			sys_copy(Num, Kernel)
		end,
		UKs ++ [ {N, SysBin} || N <- Norms ]
	),
	%io:format("Kernels copied.~n"),
	drv_lapic:start_aps(),
	ok.

%% Generate the node numbers that are occupied by unikernels.
unikernel_nums(UKs) -> [ N || {N, _} <- UKs ].

%% Generate the normal system node numbers.
normal_nums(NumProcs, UKs) ->
	% The zero'th core is already setup.
	[ N || N <- lists:seq(1, NumProcs - 1) ]
		-- unikernel_nums(UKs).

%% Get a copy of the system kernel
get_sys_bin() ->
	os_unsafe:read(?KERNEL_START, ?KERNEL_SZ).

%% Copy the binary to the correct position for the system
sys_copy(Node, Bin) ->
	% Copy the kernel
	os_unsafe:write(os_paging:node_base_addr(Node) + 16#100000, Bin),
	% Copy the state info
	os_unsafe:write(os_paging:node_base_addr(Node) + ?SYSINFO_LOC,
		os_unsafe:read(?SYSINFO_LOC, 256)),
	% Copy the state info
	os_unsafe:write(os_paging:node_base_addr(Node) + ?AP_BOOTLOADER_LOC,
		os_unsafe:read(?AP_BOOTLOADER_LOC, 512)),
	ok.
