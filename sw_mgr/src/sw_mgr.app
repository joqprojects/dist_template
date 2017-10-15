%% This is the application resource file (.app file) for the 'base'
%% application.
{application, sw_mgr,
[{description, "sw_mgr manager " },
{vsn, "0.0.1" },
{modules, 
	  [sw_mgr_app,sw_mgr_sup,sw_mgr]},
{registered,[sw_mgr]},
{applications, [kernel,stdlib]},
{mod, {sw_mgr_app,[]}},
{start_phases, []}
]}.
