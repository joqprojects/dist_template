%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%% 1. Starts and keep track on  all tcp server processes
%%% 2. SW management
%%% Configuration file 
%%%  Ip address and port to Service master
%%%  IP address and Allocated ports for this board
%%%  List of specific servcies for the board, ex HW controller telldus or a dbase
%%%  [{IP,Port,Service1},{IP,Port,Service2}]
%%%  Port 
%%%  XXXX: Board_mgr port
%%%  XXXX+N: Service ports
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(sw_mgr).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("kernel/include/file.hrl").
%-include("../../include/infra.hrl").
%% --------------------------------------------------------------------
-define(POLL_TIME,2000).
-define(CONFIG_FILE,"master.config").


%% External exports
-export([connect/0]).

-export([start/0,
	 stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {repositoryPath}).

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
    io:format("Starting ~p~n",[{?MODULE,?LINE}]),
    {ok,MasterConfigList}=file:consult(?CONFIG_FILE),
    {repositoryPath,Repository}=lists:keyfind(repositoryPath,1,MasterConfigList),
    {ok, #state{repositoryPath=Repository}}.

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

% --------------------------------------------------------------------
%% Function: stop/0
%% Description:
%% 
%% Returns: non
%% --------------------------------------------------------------------

handle_call({connect},_From,State) ->
    Parent=self(),
    Repository=State#state.repositoryPath,
    Pid=spawn(fun()-> loop(Parent,Repository) end),
    NameStr=pid_to_list(Pid)++"-"++atom_to_list(node()),
    Name=list_to_atom(NameStr),
    Pid!{Parent,Name},
    true=register(Name,Pid),
    erlang:monitor(process,Pid),
    Reply={Name,node()},
    {reply, Reply, State};

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
    io:format("unmatched match cast ~p~n",[{time(),?MODULE,?LINE,Info}]),
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

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

loop(Parent,Repository)->
    receive
	{Parent,Name}->
	    worker(Name,Repository)
    end.
    

worker(Name,Repository)->
    receive
%	{From,X,Y}->
%	    io:format("~p~n",[{node(),?MODULE,?LINE,X,Y}]),
%	    Reply={test,node(),?MODULE,?LINE};

	{From,get_file_list,[]}->
	    case rpc:call(node(),sw_mgr_lib,get_file_list,[Repository]) of
		{badrpc,Reason}->
		    Reply={error,{badrpc,Reason}};
		R->
		    Reply={ok,R}
	    end;
	
	{From,get_file,[FileName]}->
	    case rpc:call(node(),sw_mgr_lib,get_file,[FileName,Repository]) of
		{badrpc,Reason}->
		    Reply={error,{badrpc,Reason}};
		R->
		    Reply={ok,R}
	    end;

	{From,upgrade,[Module,Node]}->
	    case rpc:call(node(),sw_mgr_lib,upgrade,[Module,Node]) of
		{badrpc,Reason}->
		    Reply={error,{badrpc,Reason}};
		R->
		    Reply={ok,R}
	    end;

	{From,exit}->
	    Reply=exit;		 
	{From,X,Y}->
	    Reply={error,not_defined,X,Y};
	Err->
	    From=self(),
	    Reply={error,Err}
    end,
    case Reply of
	exit->
	    exit(self(),{From,time()});
	_->
	    
	    From!{Name,Reply},
	    worker(Name,Repository)
    end.
