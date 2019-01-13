%%% A macro that defines the 'database' of PCI devices supported, or
%%% at least known about, by the OS. To add support for a PCI device,
%%% simply add a #pci_dev entry to the list.

%%% Names do not have to be provided, if the provided device module
%%% has a name/0 function.

-define(PCI_DEVICES,
	[
		%% Devices in Optiplex 780 Mini
		#pci_dev {
			vendor = 16#8086,
			id = 16#2e10,
			name = "82Q45 Host Bridge"
		},
		#pci_dev {
			vendor = 16#8086,
			id = 16#2e11,
			name = "82Q45 PCI Express Bridge"
		},
		#pci_dev {
			vendor = 16#8086,
			id = 16#2e12,
			name = "82Q45 Integrated Graphics Device"
		},
		#pci_dev {
			vendor = 16#8086,
			id = 16#2e14,
			name = "Intel Management Engine Interface (HECI)"
		},
		#pci_dev {
			vendor = 16#8086,
			id = 16#10DE,
			name = "Intel Gigabit network connection"
		},
		#pci_dev {
			vendor = 16#8086,
			id = 16#3A67,
			name = "82801JD USB UHCI Controller"
		},
		#pci_dev {
			vendor = 16#8086,
			id = 16#3A6E,
			name = "82801JD High Definition Audio Controller"
		},
		#pci_dev {
			vendor = 16#8086,
			id = 16#3A70,
			name = "82801JD PCI Express Port"
		},
		#pci_dev {
			vendor = 16#8086,
			id = 16#3A64,
			name = "82801JD USB UHCI Controller"
		},
		#pci_dev {
			vendor = 16#8086,
			id = 16#244E,
			name = "C61x/X99 PCIE"
		},
		#pci_dev {
			vendor = 16#8086,
			id = 16#3A64,
			name = "82801JD USB UHCI Controller"
		},
		%% Devices found in a Bochs machine
		#pci_dev {
			vendor = 16#8086,
			id = 16#1237,
			name = "82441FX (PMC) PCI and Memory Controller"
		},
		#pci_dev {
			vendor = 16#8086,
			id = 16#7000,
			name = "82371SB (PIIX3) PCI-ISA Bridge"
		}
	]
).
