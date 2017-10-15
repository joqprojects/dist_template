%% This is the application resource file (.app file) for the 'base'
%% application.
{application, sd,
[{description, "Joakims service discovery" },
{vsn, "0.0.1" },
{modules, 
	  [sd_app,sd_sup,sd]},
{registered,[sd]},
{applications, [kernel,stdlib]},
{mod, {sd_app,[]}},
{start_phases, []}
]}.
