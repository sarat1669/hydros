-module(drv_smbios).
-export([start/0]).
-define(VERSION, 2.3).

%%% A driver for reading the SMBIOS tables.
%%% Takes the information from the low memory raw structure, parses them, and
%%% copies the data into an ETS table.

-record(smbios_hdr, {
	magic = {list, 4},
	checksum = byte,
	length = byte,
	major_version = byte,
	minor_version = byte,
	max_sz = short,
	ignore = 6,
	magic2 = {list, 5},
	checksum2 = byte,
	table_len = short,
	table_addr = int,
	num_structs = short
}).

%% Find the table, parse the data, copy it into an ETS table.
start() ->
	case find_table() of
		not_found -> not_started;
		Addr ->
			{table_addr, TabAddr} =
				lists:keyfind(table_addr, 1, Header = parse_header(Addr)),
			{num_structs, NumStructs} = lists:keyfind(num_structs, 1, Header),
			read_structs(TabAddr, NumStructs)
	end.
			
%% Find the table header in memory. It should be 16 byte aligned, between
%% 0xF0000, and 0xFFFFF. Starting with the magic value "_SM_".
%% Return not_found if the table is not located within the specified range,
find_table() -> find_table(16#F0000).
find_table(Addr) when Addr > 16#FFFFF -> not_found;
find_table(Addr) ->
	case (os_struct:parse(Addr, #smbios_hdr{}))#smbios_hdr.magic of
		"_SM_" -> Addr;
		_ -> find_table(Addr + 16)
	end.

%% Get the SMBIOS header information.
parse_header(Addr) ->
	os_struct:parse(Addr, struct(smbios_hdr)).

%% Get a header structure, possibly dependent on the supported SMBIOS
struct(Type) -> struct(Type, ?VERSION).
struct(smbios_hdr, _Vsn) -> #smbios_hdr{};
struct(entry_hdr, _Vsn) ->
	[
		{type, byte},
		{length, byte},
		{handle, short}
	];
struct(mem_dev, _Vsn) ->
	[
		{physical_memory_array_handle, short},
		{memory_error_information_handle, short},
		{total_width, short},
		{data_width, short},
		{size, short},
		{form_factor, byte},
		{device_set, byte},
		{device_locator_str, byte},
		{bank_locator_str, byte},
		{memory_type, byte},
		{type_detail, short}
	].

%% Read individual structures from the table
read_structs(_, 0) -> [];
read_structs(Addr, Num) ->
	Header = os_struct:parse(Addr, struct(entry_hdr)),
	{type, Type} = lists:keyfind(type, 1, Header),
	{length, Length} = lists:keyfind(length, 1, Header),
	{NextAddr, Strs} = read_strings(Addr + Length),
	case process_entry(Type, Addr + 4, Strs) of
		unsupported -> read_structs(NextAddr, Num - 1);
		Processed -> [ Processed | read_structs(NextAddr, Num - 1) ]
	end.

%% Returns a list of strings from the end of a data chunk
read_strings(Addr) ->
	{NextAddr, Str} = read_string(Addr),
	case Str of
		[] -> read_strings(NextAddr + 1, []);
		_ -> read_strings(NextAddr + 1, [Str])
	end.
read_strings(Addr, Strs) ->
	case hd(binary_to_list(os_unsafe:read(Addr, 1))) of
		0 ->
			{Addr + 1, Strs};
		_ ->
			{NextAddr, Str} = read_string(Addr),
			read_strings(NextAddr + 1, Strs ++ [Str])
	end.

%% Return the address of the next string, and the read string
read_string(Addr) -> read_string(Addr, []).
read_string(Addr, Str) ->
	case os_unsafe:read(Addr, 1) of
		<< 0 >> -> {Addr, Str};
		<< Chr >> -> read_string(Addr + 1, Str ++ [Chr])
	end.

%% Process individual structure entries
process_entry(17, Addr, _Strs) ->
	os_struct:parse(Addr, struct(mem_dev));
process_entry(_, _, _) ->
	unsupported.
