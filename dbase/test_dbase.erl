%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% 
%%% -------------------------------------------------------------------
-module(test_dbase).

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
    application:load(dbase),
    application:start(dbase),
    RemoveFile="rm "++atom_to_list(?MODULE),
    os:cmd(RemoveFile),
    ok.

create_A_test()->
    I1=connect(dbase),
    I2=connect(dbase),
    {ok,file_created}=do_call(I1,create,[bag,?MODULE]),
    {ok,file_already_exsist}=do_call(I2,create,[bag,?MODULE]),
    {ok,file_deleted}=do_call(I2,delete,[?MODULE]),
    {ok,file_created}=do_call(I1,create,[bag,?MODULE]),
    {ok,file_deleted}=do_call(I2,delete,[?MODULE]),
    disconnect(I1),
    disconnect(I2),
    ok.

crud_a_test()->
    I1=connect(dbase),
    I2=connect(dbase),
    {ok,file_created}=do_call(I1,create,[set,?MODULE]),
    {ok,store}=do_call(I1,store,[key1,value1,?MODULE]),
    {ok,[{key1,value1}]}=do_call(I1,get,[key1,?MODULE]),
    {ok,[]}=do_call(I1,get,[no_key,?MODULE]),
    {ok,store}=do_call(I1,store,[key2,value2,?MODULE]),
    {ok,[{key2,value2},{key1,value1}]}=do_call(I1,all,[?MODULE]),
    {ok,value1}=do_call(I1,remove,[key1,?MODULE]),
    {ok,[]}=do_call(I1,get,[key1,?MODULE]),  
    {ok,file_deleted}=do_call(I2,delete,[?MODULE]),    
    disconnect(I1),
    disconnect(I2),
    ok.

crud_a_timeout_test()->
    TimeOut=100,
    I1=connect(dbase),
    I2=connect(dbase),
    {ok,file_created}=do_call(I1,create,[set,?MODULE],TimeOut),
    {ok,store}=do_call(I1,store,[key1,value1,?MODULE],TimeOut),
    {ok,[{key1,value1}]}=do_call(I1,get,[key1,?MODULE],TimeOut),
    {ok,[]}=do_call(I1,get,[no_key,?MODULE],TimeOut),
    {ok,store}=do_call(I1,store,[key2,value2,?MODULE],TimeOut),
    {ok,[{key2,value2},{key1,value1}]}=do_call(I1,all,[?MODULE],TimeOut),
    {ok,value1}=do_call(I1,remove,[key1,?MODULE],TimeOut),
    {ok,[]}=do_call(I1,get,[key1,?MODULE],TimeOut),  
    {ok,file_deleted}=do_call(I2,delete,[?MODULE],TimeOut),    
    disconnect(I1),
    disconnect(I2),
    ok.

crud_b_timeout_test()->
    TimeOut=100,
    I1=connect(dbase),
    {ok,file_created}=do_call(I1,create,[set,?MODULE],TimeOut),
    I1!{self(),test,[]}, % test to crash to process
    {ok,file_already_exsist}=do_call(I1,create,[set,?MODULE],TimeOut),
    {ok,file_deleted}=do_call(I1,delete,[?MODULE],TimeOut), 
    disconnect(I1),
    ok.
crud_b_test()->
   TimeOut=500,
    I1=connect(dbase),
    {ok,file_created}=do_call(I1,create,[set,?MODULE],TimeOut),
    I1!{self(),test,[]}, % test to crash to process
    {ok,store}=do_call(I1,store,[key1,value1,?MODULE],TimeOut),
    {ok,[{key1,value1}]}=do_call(I1,get,[key1,?MODULE],TimeOut),
    {ok,[]}=do_call(I1,get,[no_key,?MODULE],TimeOut),
    I1!{self(),test,[]}, % test to crash to process
    {ok,store}=do_call(I1,store,[key2,value2,?MODULE],TimeOut),
    {ok,[{key2,value2},{key1,value1}]}=do_call(I1,all,[?MODULE],TimeOut),
    {ok,value1}=do_call(I1,remove,[key1,?MODULE],TimeOut),
    {ok,[]}=do_call(I1,get,[key1,?MODULE],TimeOut),   
    {ok,file_deleted}=do_call(I1,delete,[?MODULE],TimeOut), 
    disconnect(I1),
    ok.

connect(Service)->
    {Name,Node}=rpc:call('dbase@home.joqhome.eu',Service,connect,[]),
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

do_call(Instance,Cmd,Args,Timeout)->
    Reply=do_call(Instance,Cmd,Args,Timeout,3,[]),
    Reply.

do_call(Instance,Cmd,Args,Timeout,0,Reply)->
    Reply;

do_call(Instance,Cmd,Args,Timeout,N,Reply)->
    Parent=self(),
    Pid=spawn(fun()->do_call_1(Instance,Cmd,Args,Parent) end),
    receive
	{Pid,Reply1}->
	    N1=0;
	X->
	    glurk=X,
	    Reply1=glurk,
	    N1=0
	
    after Timeout ->
	    Reply1={error,timeout},
	    N1=N-1,
	    exit(Pid,kill)
    end,
    do_call(Instance,Cmd,Args,Timeout,N1,Reply1).

do_call_1(Instance,Cmd,Args,Parent)->
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
    Parent!{self(),Reply}.
