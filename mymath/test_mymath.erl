%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% 
%%% -------------------------------------------------------------------
-module(test_mymath).

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
%%


%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
start_test()->
   pong=net_adm:ping('mymath@home.joqhome.eu'), 
   ok.

add_A_test()->
    Instance=connect(mymath),
    46=do_call(Instance,add,[32,14]),
    disconnect(Instance),
    ok.

add_b_test()->
    Instance=connect(mymath),
    1=do_call(Instance,add,[1,0]),
    3=do_call(Instance,add,[2,1]),
    5=do_call(Instance,add,[3,2]),
    disconnect(Instance),
    ok.


add_B_te()->
    Instance=connect(mymath),
    5.0=do_call(Instance,divi,[10,2]),
    {badrpc,
     {'EXIT',
      {badarith,
       _Err}}}=do_call(Instance,divi,[10,0]),
    disconnect(Instance),
    ok.


   
%%
%% Local Functions
%%
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------




connect(Service)->
    {Name,Node}=rpc:call('mymath@home.joqhome.eu',Service,connect,[]),
    {Name,Node}.

disconnect(Instance)->
    Instance!{self(),exit}.

do_call(Instance,Cmd,Args)->
    {Name,Node}=Instance,
    {Name,Node}!{self(),Cmd,Args},
    receive
	{Name,Reply}->
	    Reply;
	X ->
	   % send error msg to fault manager
	    glurk=X,
	    Reply=glurk
    end,
    Reply.
