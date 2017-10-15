%% This is the application resource file (.app file) for the 'base'
%% application.
{application, board_mgr,
[{description, "board manager " },
{vsn, "0.0.1" },
{modules, 
	  [board_mgr_app,board_mgr_sup,board_mgr]},
{registered,[board_mgr]},
{applications, [kernel,stdlib]},
{mod, {board_mgr_app,[]}},
{start_phases, []}
]}.
