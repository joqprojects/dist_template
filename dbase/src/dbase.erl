%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase).
-behaviour(gen_server).
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------
%% External exports

-export([connect/0]).
%-export([add/2,divi/2]).
-export([start/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name}).

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
    Parent=self(),
    Pid=spawn(fun()-> loop(Parent) end),
    NameStr=pid_to_list(Pid)++"-"++atom_to_list(node()),
    Name=list_to_atom(NameStr),
    Pid!{Parent,Name},
    true=register(Name,Pid),
    erlang:monitor(process,Pid),
    io:format("Application Started ~p~n",[{?MODULE,?LINE}]),
    {ok, #state{name=Name}}.

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
    Name=State#state.name,
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
handle_info({'DOWN',_Ref,process,KilledPid,Reason}, State) ->
    io:format(" ~p~n",[{?MODULE,?LINE,'DOWN',_Ref,process,KilledPid,Reason}]),
    Name=State#state.name,
    Pid=spawn(fun()-> worker(Name) end),
    true=register(Name,Pid),
    erlang:monitor(process,Pid),
    {noreply, State};

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
%% Function: get_all/0
%% Description:if needed creates dets file with name ?MODULE, and
%% initates the debase
%% Returns: non
%% --------------------------------------------------------------------
loop(Parent)->
    receive
	{Parent,Name}->
	    worker(Name)
    end.
worker(Name)->
    receive
	{From,delete,[File]}->
	    Reply=rpc:call(node(),dbase_lib,delete,[File]);
	{From,exists,[File]}->
	    Reply=rpc:call(node(),dbase_lib,exists,[File]);
	{From, create,[Type,File]}->
	    Reply=rpc:call(node(),dbase_lib,create,[Type,File]);
	{From,store,[Key,Value,File]}->
	    Reply=rpc:call(node(),dbase_lib,store,[Key,Value,File]);
	{From,get,[Key,File]}->
	    Reply=rpc:call(node(),dbase_lib,get,[Key,File]);
	{From,all,[File]}->
	    Reply=rpc:call(node(),dbase_lib,all,[File]);
	{From,remove,[Key,File]}->
	    Reply=rpc:call(node(),dbase_lib,remove,[Key,File]);
	{From,test,[]}->
	    Reply=killed,
	    exit(self(),{From,time()});
	{From,X,Y}->
	    Reply={error,not_defined,X,Y};
	Err->
	    From=Name,
	    Reply={error,Err}
    end,
    case Reply of
	exit->
	    do_nothing_gen_server;
	_->
	    From!{Name,Reply}
    end,
    timer:sleep(1), % dont know why this is needed - race conditions?
    worker(Name).
