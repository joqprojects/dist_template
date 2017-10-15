%% This is the application resource file (.app file) for the 'base'
%% application.
{application, master_node,
[{description, "master_node manager " },
{vsn, "0.0.1" },
{modules, 
	  [master_node_app,master_node_sup,master_node]},
{registered,[master_node]},
{applications, [kernel,stdlib]},
{mod, {master_node_app,[]}},
{start_phases, []}
]}.
