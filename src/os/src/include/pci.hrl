%%% A record for matching PCI devices with drivers and names

-record(pci_dev, {
	vendor,
	id,
	name,
	driver
}).

-include("pci_devices.hrl").
