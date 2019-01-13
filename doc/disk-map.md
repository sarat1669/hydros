System Disk Image Map
=================

This map describes the layout of the HydrOS boot image in terms of 512-byte zero-indexed sectors.

| Start Sector | End Sector | Length | Notes                       |
|--------------|------------|--------|-----------------------------|
| 0            | 0          | 1      |BSP Boot Sector              |
| 1            | 2          | 2      |BSP Bootloader Stage 2       |
| 3            | 34         | 32     |BSP Bootloader Stage 3       |
| 35           | 35         | 1      |Real-Mode Slave Sector       |
| 36           | 36         | 1      |AP Boot Sector               |
| 37           | 62         | 26     |*Unallocated Region*         |
| 63           | <15999     | <15936 |Main Kernel Image            |
| 16000        | Undefined  | -      |HydrOS FS Area               |

> **Key:** *Italics* indicate a region of free space.
