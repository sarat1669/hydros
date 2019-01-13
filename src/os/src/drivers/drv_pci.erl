-module(drv_pci).
-export([start/0]).
-include("pci.hrl").

%%% A driver for reading the PCI config space

%% Find the table, parse the data, copy it into an ETS table.
start() ->
	AllDevs = find_devices(),
	IdentDevs =
		lists:filtermap(
			fun identify_device/1,
			AllDevs
		),
	DriverDevs =
		lists:filter(
			fun has_driver/1,
			IdentDevs
		),
	io:format(
		"PCI devs: ~w. Identified: ~w. HaveDriver: ~w.~n",
		[
			length(AllDevs),
			length(IdentDevs),
			length(DriverDevs)
		]
	),
	lists:map(fun start_driver/1, DriverDevs).

%% Start the driver for the given device.
start_driver({Location, Dev}) ->
	spawn(Dev#pci_dev.driver, start, [Location]).

has_driver({_Location, #pci_dev{ driver = Driver }}) ->
	Driver =/= undefined.

%% Identify a PCI device from it's vendor and ID tags. Match it to
%% a #pci_dev if possible.
identify_device({Location, {Vendor, ID}}) ->
	case matching_devices(Vendor, ID) of
		[] -> false;
		[Device] -> {Location, Device}
	end.

%% Find all devices matching the vendor and ID.
matching_devices(Vendor, ID) ->
	[
		X
	||
		X <- ?PCI_DEVICES,
			X#pci_dev.id == ID,
			X#pci_dev.vendor == Vendor
	].

%% Use brute force to find all of the devices attached to the system
find_devices() ->
	lists:filter(
		fun(X) -> X =/= false end,
		[
			case is_present(Bus, Slot) of
				false -> false;
				true ->
					{
						{Bus, Slot},
						{vendor(Bus, Slot), id(Bus, Slot)}
					}
			end
		||
			Bus <- lists:seq(0, 255),
			Slot <- lists:seq(0, 31)
		]
	).

%% Read a 32 bit word from a device's configuration space
read_config(Bus, Slot, Func, Offset) ->
	Addr = (Bus bsl 16) bor (Slot bsl 11) bor
			(Func bsl 8) bor (Offset band 16#fc) bor
			16#80000000,
	os_unsafe:port_out32(16#CF8, Addr),
	os_unsafe:port_in32(16#CFC)
		bsr ((Offset band 2)  band 16#ffff).

%% Check if there is a device in slot X of bus Y
is_present(Bus, Slot) ->
	read_config(Bus, Slot, 0, 0) =/= -1.

%% Get a device's vendor ID
vendor(Bus, Slot) ->
	read_config(Bus, Slot, 0, 0) band 16#FFFF.

%% Get a device's ID
id(Bus, Slot) ->
	read_config(Bus, Slot, 0, 0) bsr 16.
