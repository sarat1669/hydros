System Memory Map
=================
The operating system represented in this repository uses the following memory layout. In order to avoid all manner of bad things happening in the future, we should keep this document up-to-date.

----------
Low Memory
----------
Low memory contains mostly shared OS and BIOS data. If possible, the shared structures should be found in this area of RAM. Ideally, there should be no synchronisation required when accessing any of these structures. Where possible, if the structure causes a lot of contention it should be generated before the *Application Processors* are started for the sake of simplicity.

|   Start  |     End     |   Length  | Notes                       |
|----------|-------------|-----------|-----------------------------|
|  `0x00000`|`    0x003ff`|        1kb|BIOS Real Mode IVT           |
|  `0x00400`|`    0x004ff`|       256b|BIOS Data Area               |
|  `0x00500`|`   <0x00fe7`|     <2.8kb|**Paging meta-table        **|
|  `0x00fe8`|`   <0x00fff`|        24b|**Interrupt handeler data  **|
|  `0x01000`|`    0x01fff`|        4kb|**PLM4T                    **|
|  `0x02000`|`    0x02fff`|        4kb|**PDPT                     **|
|  `0x03000`|`    0x03fff`|        4kb|**PDT                      **|
|` >0x04000`|`    0x07bff`|        7kb|**BSP startup stack        **|
|  `0x07c00`|`    0x07dff`|       512b|**BSP boot sector          **|
|  `0x07e00`|`    0x081ff`|        1kb|**BSP stage 2              **|
|  `0x08200`|`    0x0b9ff`|       14kb|**BSP stage 3              **|
|  `0x0ba00`|`    0x0c1ff`|        2kb|**Shared system information**|
|  `0x0c200`|`    0x0c3ff`|       512b|**Real Mode Slave boot     **|
|  `0x0c400`|`    0x0c5ff`|       512b|**AP boot sector           **|
|  `0x0c600`|`    0x145ff`|       32kb|**RMS read buffer          **|
|  `0x14600`|`    0x8ffff`|      509kb|*Unallocated region*         |
|  `0x90000`|`    0x90008`|         8b|**Boot trampoline code     **|
| `>0x90009`|`    0x9fbff`|       64kb|**BSP and AP kernel stack  **|
|  `0x9fc00`|`    0x9ffff`|        1kb|BIOS Extended Data Area      |
|  `0xA0000`|`    0xBffff`|      128kb|VGA and text video memory    |
|  `0xC0000`|`    0xfffff`|      256kb|BIOS                         |
| `0x100000`|`  <0x7fffff`|       <7mb|**OS Kernel                **|
| `0x800000`|`   0x9fffff`|        2mb|**Page Directories         **|
| `0xA00000`|`   0xBfffff`|        2mb|**Node Mailbox             **|
|`0x1000000`|  `Undefined`|          -|**Memory Allocation Zone   **|

> **Key:** *Italics* indicate a region of free space, **bolding** indicates that the region has been positioned by the authors and unstyled text indicates that a region that cannot be relocated.

> **Note:**
- The paging data structures found between `0x01000` and `0x04fff` 'identity' map the first 1GB of memory. The structures must be in the first megabyte of RAM. Due to restrictive alignment requirements, the *AP boot trampoline* cannot be moved closer to the beginning of the *BIOS EDA* area, necessitating a small gap in the memory layout.
