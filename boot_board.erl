%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(boot_board).



%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------
%% External exports

-export([start/1]).

%% ====================================================================
%% External functions
%% ====================================================================
start([])->
    'empty_applications_list';
start(L)->
    io:format("L  ~p~n",[L]),    
    StartedApplications=start(L,[]),
    io:format("Started ~p~n",[StartedApplications]),
    StartedApplications.
start([],StartedApplications)->
    StartedApplications;
start([App|T],StartedApplications)->
  %  M=list_to_atom(App),
    M=App,
    io:format("Loading ~p~n",[application:load(M)]),
    io:format("Starting ~p~n",[application:start(M)]),
    start(T,[M|StartedApplications]).
    
