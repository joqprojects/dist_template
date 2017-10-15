%% This is the application resource file (.app file) for the 'base'
%% application.
{application, dbase,
[{description, "dbase server using dets " },
{vsn, "0.0.1" },
{modules, 
	  [dbase_app,dbase_sup,dbase,dbase_lib]},
{registered,[dbase]},
{applications, [kernel,stdlib]},
{mod, {dbase_app,[]}},
{start_phases, []}
]}.
