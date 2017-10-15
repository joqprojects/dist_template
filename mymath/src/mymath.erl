%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : template multiple 
%%% 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(mymath).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------
%% External exports

-export([connect/0,worker/1]).
%-export([add/2,divi/2]).
-export([start/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).
 
connect()->
    gen_server:call(?MODULE, {connect},infinity).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    io:format("Application Starting ~p~n",[{?MODULE,?LINE}]),
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

handle_call({connect},_From,State) ->
    Parent=self(),
    Pid=spawn(fun()-> loop(Parent) end),
    NameStr=pid_to_list(Pid)++"-"++atom_to_list(node()),
    Name=list_to_atom(NameStr),
    Pid!{Parent,Name},
    true=register(Name,Pid),
    erlang:monitor(process,Pid),
    Reply={Name,node()},
    {reply, Reply, State};

% --------------------------------------------------------------------
%% Function: stop/0
%% Description:
%% 
%% Returns: non
%% --------------------------------------------------------------------

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,time()}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->    
    io:format(" ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
loop(Parent)->
    receive
	{Parent,Name}->
	    worker(Name)
    end.
    

worker(Name)->
    receive
	{From,Cmd,Args}->
	    case rpc:call(node(),mymath_lib,Cmd,Args) of
		{badrpc,Reason}->
		    Reply={error,{badrpc,Reason}};
		R->
		    Reply={ok,R}
	    end;
	{From,exit}->
	    Reply=exit;		 
	Err->
	    From=self(),
	    Reply={error,Err}
    end,
    case Reply of
	exit->
	    exit(self(),{From,time()});
	_->
	    
	    From!{Name,Reply},
	    mymath:worker(Name)
    end.
    
			  
	    
