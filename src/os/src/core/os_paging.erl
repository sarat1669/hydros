-module(os_paging).
-export([init/0, init/1]).
-export([map/2, map/3, page/1]).
-export([mailbox_page/1, mailbox_addr/1, node_base_addr/1]).

%%% Manages setting up virtual memory and the mapping of pages.
%%% HydrOS uses 1gb pages. The PML4 is found at 0x1000, and the
%%% directory pointers are found at 0x800000.

%% Define the amount of system RAM
%% TODO: Calculate this dynamically.
%% Currently 2048 MB.
-define(SYS_RAM_SZ, 16#80000000).

%% Define the locations of the tables for mapping the local kernel.
-define(PML4_LOC, 16#1000).
-define(PDPT_LOC, 16#2000).
-define(PD_LOC, 16#800000).

%% Where is the mailbox associated with a node?
-define(MAILBOX_LOC, 16#A00000).

%% Pointer to the start of the node page addresstable.
-define(NODE_PAGE_TAB_LOC, 16#500).

%% Define the size of the tables.
-define(TAB_SZ, 512).

%% How many pages should HydrOS map for each kernel's local space?
-define(LOCAL_PAGES, ?TAB_SZ * ?LOCAL_KERNEL_GB_SZ).
%% How many GB should be mapped for the kernel?
-define(LOCAL_KERNEL_GB_SZ, 4).

%% Where should the node mailbox pages start?
%% 3gib, as Bochs only supports 4GB of paging.
%% If we don't need Bochs support, ?LOCAL_PAGES + 1 should give the
%% correct page number for the start of the node mailboxes.
-define(MAILBOX_START_PAGE, 1907).

%% Setup paging for all cores.
init() ->
	lists:foreach(
		fun(Node) -> init(Node) end,
		lists:seq(0, os_system_info:get_num_procs() - 1)
	).
%% Sets up paging for a core on the system.
init(Node) -> init(Node, node_base_addr(Node)).
init(Node, Offset) ->
	map_kernel_space(Node, Offset),
	generate_pml4(Offset),
	generate_pdpt(Offset),
	map_mailboxes(Offset),
	generate_pml4_pointer(Node, Offset),
	ok.

%% Get the address associated with a mailbox.
mailbox_addr(NodeID) -> page(mailbox_page(NodeID)).

%% Get the page of a node mailbox.
mailbox_page(NodeID) -> ?MAILBOX_START_PAGE + NodeID.

%% Calculate the base address of a node in the system.
node_base_addr(N) -> N * (?SYS_RAM_SZ div os_system_info:get_num_procs()).

%% Map the local area for a node.
map_kernel_space(Node, Offset) -> map_kernel_space(Node, Offset, 0).
map_kernel_space(_Node, _Offset, ?LOCAL_PAGES) -> ok;
map_kernel_space(Node, Offset, PageNum) ->
	map(Offset, PageNum, node_base_addr(Node) + page(PageNum)),
	map_kernel_space(Node, Offset, PageNum + 1).

%% Map space for node mailboxes.
map_mailboxes(Offset) ->
	map_mailboxes(Offset, 0, os_system_info:get_num_procs()).
map_mailboxes(_Offset, Procs, Procs) -> ok;
map_mailboxes(Offset, N, Procs) ->
	map(Offset, mailbox_page(N), node_base_addr(N) + ?MAILBOX_LOC),
	map_mailboxes(Offset, N + 1, Procs).

%% Create a PML4 table, pointing to the directory pointers.
%generate_pml4() -> generate_pml4(0).
generate_pml4(Offset) ->
	os_unsafe:write(
		Offset + ?PML4_LOC,
		<< ((Offset + ?PDPT_LOC) bor 3):64/little-integer >>
	).

%% Generate the main set of page tables. A 512 * 512 array of 8 byte
%% page pointers and attributes.
generate_pdpt(Offset) -> generate_pdpt(Offset, ?PDPT_LOC).
generate_pdpt(Offset, Loc) -> generate_pdpt(Offset, Loc, 0).
generate_pdpt(_, _, ?TAB_SZ) -> ok;
generate_pdpt(Offset, PDPTLoc, N) ->
	os_unsafe:write(
		Offset + PDPTLoc + (N * 8),
		<< ((Offset + ?PD_LOC + (N * 16#1000)) bor 3):64
			/integer-little >>
	),
	generate_pdpt(Offset, PDPTLoc, N + 1).

%% Make an address Addr available at PageNum gigabytes.
map(PageNum, Addr) -> map(0, PageNum, Addr).
map(Offset, PageNum, Addr) ->
	os_unsafe:write(
		Offset + ?PD_LOC + (PageNum * 8),
		<< (Addr bor (1 bsl 7) bor 3):64
			/integer-little >>
	),
	ok.

%% Write an entry into low memory that the bootloader can use to load
%% the page tables.
generate_pml4_pointer(Node, Offset) ->
	os_unsafe:write(
		?NODE_PAGE_TAB_LOC + (Node * 4),
		<< (Offset + ?PML4_LOC):32/integer-little >>
	).

%% Return a pointer to the given page.
page(Num) -> Num * (1 bsl 21).
