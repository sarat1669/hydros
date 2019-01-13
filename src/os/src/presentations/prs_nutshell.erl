-module(prs_nutshell).
-export([slides/0]).

slides() ->
	[
		"HydrOS in a Nutshell",
		{"HydrOS: An OS that Survives Catastrophic Failure",
			[
				"HydrOS is:",
				"- An OS that separates each core in a machine into an individual node in a ",
				"distributed system.",
				"- Each OS node, running on its own core, then communicates with other cores.",
				"- Subsequently, catastrophic failure of one node leaves all other nodes intact.",
				"",
				"HydrOS systems function despite:",
				"- Complete software failure of all bar one core.",
				"- Loss of all machine peripherals.",
				"- CPU core/die failure (in multisocket systems).",
				"- In the future, loss of access to some RAM."
			]
		},
		{"Traditional SMP OS Structures", fun basic_structure/1},
		{"Multikernel OS Structures", fun multikernel_arch/1},
		{"Failure in Multikernel Systems", fun multikernel_failure/1},
		{"Failure Recovery in HydrOS", fun multikernel_recovery/1},
		{"System Components",
			[
				"- Bespoke, co-operative bootloader.",
				"- Precisely minimal C standard library, tailored for the Erlang VM.",
				"- Small library of Erlang wrapper functions.",
				"- OS subsystems, written in Erlang.",
				"- Erlang device drivers."
			]
		},
		{"Security Through Managed Code Execution",
			[
				"- All user code is written in Erlang.",
				"- All low-level and 'dangerous' operations are gated by capability checks.",
				"- By default, processes have the same capabilities as their parent.",
				"- Processes can have capabilities removed at spawn-time, but never added.",
				"- Thus producing trees of decreasingly privileged processes."
			]
		},
		{"Capabilities in a Managed Code Environment",
			[
				"- This system allows users to run potentially dangerous code, safely.",
				"",
				"- Similarly, it allows application and driver writers to highly specify the",
				"requirements of a program, to avoid damage if it begins to malfunction."
			]
		},
		{"Unikernels", fun unikernel_structure/1},
		{"Multiunikernels", fun multiunikernel_structure/1},
		{"Future Possibilities",
			[
				"- Guaranteed software security of the capabilities mechanism, through a ",
				"formally verified VM.",
				"",
				"- Hosting of other operating systems, in a similar way to unikernels, along-",
				"side HydrOS nodes.",
				"",
				"- Exploration of anti-virus systems in a managed code environment."
			]
		},
		"Demos and Questions"
	].

basic_structure(Win) ->
	TopBuf = 7,
	Width = 29,
	LeftBuf = 26,
	RightBuf = LeftBuf + Width,
	wm_window:render_batch(Win,
		[
			{print_boxed,
				[
					LeftBuf + X,
					TopBuf,
					LeftBuf + 5 + X,
					TopBuf + 1,
					" App", white, green
				]
			}
		||
			X <- lists:seq(0, 24, 6)
		] ++
		[
			[
				{rect,
					[
						LeftBuf,
						TopBuf + Offset,
						RightBuf,
						TopBuf + Offset + 3,
						Col
					]},
				{print_str,
					[
						LeftBuf + calculate_centred_offset(Str, Width),
						TopBuf + Offset + 1,
						Str, white, Col
					]
				}
			]
		||
			{Str, Col, Offset} <-
				[
					{"OS Layer", blue, 1},
					{"Kernel", brown, 4},
					{"Hardware", red, 7}
				]
		]
	).

multikernel_arch(Win) ->
	multikernel_structure(Win, 4).

multikernel_failure(Win) ->
	multikernel_structure(Win, 3).

multikernel_recovery(Win) ->
	multikernel_structure(Win, 3, thick).

multikernel_structure(Win, Cores) ->
	multikernel_structure(Win, Cores, thin).
multikernel_structure(Win, Cores, AppBar) -> 
	TopBuf = 7,
	Width = 27,
	LeftBuf = 27,
	RightBuf = LeftBuf + Width,
	wm_window:render_batch(Win,
		[
			[
				{rect,
					[
						LeftBuf,
						TopBuf + Offset - if Thickness == thick -> 1; true -> 0 end,
						RightBuf - case Thickness of thin -> 0; thick -> 7 end,
						TopBuf + Offset + 3, Col
					]
				},
				{print_str,
					[
						LeftBuf +
							calculate_centred_offset(
								Str,
								Width - case Thickness of thin -> 0; thick -> 7 end
							),
						TopBuf + Offset + 1, Str, white, Col
					]
				}
			]
		||
			{Str, Col, Offset, Thickness} <-
				[
					{"Apps", green, 0, AppBar}, {"Hardware", red, 12, thin}
				]
		] ++
		[
			[
				{print_str, [LeftBuf - 10, TopBuf + Offset + 1, Str]}
			] ++
			[
				{rect,
					[
						LeftBuf + (X * 7),
						TopBuf + Offset,
						LeftBuf + (X * 7) + 6,
						TopBuf + Offset + 3,
						Col
					]
				}
			||
				X <- lists:seq(0, Cores - 1)
			]
		||
			{Str, Col, Offset} <-
				[
					{"IPC", cyan, 3}, {"OS Layer", blue, 6}, {"Kernels", brown, 9}
				]
		]
	).

unikernel_structure(Win) ->
	TopBuf = 7,
	Width = 27,
	LeftBuf = 27,
	RightBuf = LeftBuf + Width,
	wm_window:render_batch(Win,
		[
			[
				{rect,
					[
						LeftBuf,
						TopBuf + Offset,
						RightBuf,
						TopBuf + Offset + 3, Col
					]
				},
				{print_str,
					[
						LeftBuf +
							calculate_centred_offset(Str, Width),
						TopBuf + Offset + 1, Str, white, Col
					]
				}
			]
		||
			{Str, Col, Offset} <-
				[
					{"Hypervisor", magenta, 9}, {"Hardware", red, 12}
				]
		] ++
		[
			[
				{rect,
					[
						LeftBuf + ((X - 1) * 7),
						TopBuf + 0,
						LeftBuf + ((X - 1) * 7) + 6,
						TopBuf + 9,
						dark_grey
					]
				},
				{print_str,
					[
						LeftBuf + 1 + ((X - 1) * 7),
						11,
						Str, white, dark_grey
					]
				}
			]
		||
			{X, Str} <- os_util:number(["Uni1", "Uni2", "Uni3", "Uni4"])
		]
	).


multiunikernel_structure(Win) ->
	TopBuf = 7,
	Width = 27,
	LeftBuf = 27,
	NormCores = 2,
	RightBuf = LeftBuf + Width,
	wm_window:render_batch(Win,
		[
			[
				{rect,
					[
						LeftBuf,
						TopBuf + Offset,
						RightBuf,
						TopBuf + Offset + 3, Col
					]
				},
				{print_str,
					[
						LeftBuf +
							calculate_centred_offset(Str, Width),
						TopBuf + Offset + 1, Str, white, Col
					]
				}
			]
		||
			{Str, Col, Offset} <-
				[
					{"App", green, 0}, {"Hardware", red, 12}
				]
		] ++
		[
			[
				{print_str, [LeftBuf - 10, TopBuf + Offset + 1, Str]}
			] ++
			[
				{rect,
					[
						LeftBuf + (X * 7),
						TopBuf + Offset,
						LeftBuf + (X * 7) + 6,
						TopBuf + Offset + 3,
						Col
					]
				}
			||
				X <- lists:seq(0, NormCores - 1)
			]
		||
			{Str, Col, Offset} <-
				[
					{"IPC", cyan, 3}, {"OS Layer", blue, 6}, {"Kernels", brown, 9}
				]
		] ++
		[
			[
				{print_str, [LeftBuf - 10, TopBuf + Offset + 1, Str]}
			] ++
			[
				{rect,
					[
						LeftBuf + (X * 7),
						TopBuf + Offset,
						LeftBuf + (X * 7) + 6,
						TopBuf + Offset + 3,
						Col
					]
				}
			||
				X <- lists:seq(0, NormCores - 1)
			]
		||
			{Str, Col, Offset} <-
				[
					{"IPC", cyan, 3}, {"OS Layer", blue, 6}, {"Kernels", brown, 9}
				]
		] ++
		[
			{rect,
				[
					LeftBuf + (3 * 7),
					TopBuf + 3,
					LeftBuf + (3 * 7) + 6,
					TopBuf + 12,
					dark_grey
				]
			},
			{print_str,
				[
					RightBuf - 5, 14,
					"Uni1", white, dark_grey
				]
			},
			{rect,
				[
					LeftBuf + (2 * 7),
					TopBuf + 3,
					LeftBuf + (2 * 7) + 6,
					TopBuf + 12,
					dark_grey
				]
			},
			{print_str,
				[
					RightBuf - 5 - 7, 14,
					"Uni2", white, dark_grey
				]
			}
		]
	).

%% Calculate the 'centred' position of a list.
calculate_centred_offset(List, Size) ->
	(Size div 2) - (length(List) div 2).
