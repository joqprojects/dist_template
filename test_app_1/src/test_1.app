%% This is the application resource file (.app file) for the 'base'
%% application.
{application, test_1,
[{description, "test_1 is a test service  " },
{vsn, "0.0.1" },
{modules, 
	  [test_1_app,test_1_sup,test_1,test_1_lib]},
{registered,[test_1]},
{applications, [kernel,stdlib]},
{mod, {test_1_app,[]}},
{start_phases, []}
]}.
