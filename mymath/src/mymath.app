%% This is the application resource file (.app file) for the 'base'
%% application.
{application, mymath,
[{description, "mymath is a test service  " },
{vsn, "0.0.1" },
{modules, 
	  [mymath_app,mymath_sup,mymath,mymath_lib]},
{registered,[mymath]},
{applications, [kernel,stdlib]},
{mod, {mymath_app,[]}},
{start_phases, []}
]}.
