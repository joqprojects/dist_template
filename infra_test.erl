%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% Generic tcp server interface to internet and "middle man". Concept 
%%% described in Joe Armstrong book
%%% -------------------------------------------------------------------
-module(infra_test).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([]).
%%
%% API Functions

init_test()->
    pong=net_adm:ping('master_node@home.joqhome.eu'),
    pong=net_adm:ping('worker_10@home.joqhome.eu'),
    pong=net_adm:ping('worker_11@home.joqhome.eu'),
    ok.

check_worker_10_test()->
    [{mymath,[pid]}]=rpc:call('worker_10@home.joqhome.eu',
		  service_discovery,get_provided,[]),
    [dbase]=rpc:call('worker_10@home.joqhome.eu',
		  service_discovery,get_needed,[]),
    [{dbase,[pid]}]=rpc:call('worker_10@home.joqhome.eu',
		  service_discovery,get_found,[]),
    ok.
check_worker_11_test()->
    [{dbase,[pid]}]=rpc:call('worker_11@home.joqhome.eu',
		  service_discovery,get_provided,[]),
    [mymath]=rpc:call('worker_11@home.joqhome.eu',
		  service_discovery,get_needed,[]),
    [{mymath,[pid]}]=rpc:call('worker_11@home.joqhome.eu',
		  service_discovery,get_found,[]),
    ok.

