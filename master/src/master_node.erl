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
-module(master_node).

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
-export([get_config/1]).
-export([start/0,
	 stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {masterConfigList}).

%% ====================================================================
%% External functions
%% ====================================================================
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

get_config(BoardId)->
    gen_server:call(?MODULE, {get_config,BoardId},infinity).

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
   
    spawn(fun()->start_board_mgr() end),
 %   application:load(board_mgr),
  %  application:start(board_mgr),
    {ok, #state{masterConfigList=MasterConfigList}}.

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
handle_call({get_config,BoardId},_From, State) ->
    ConfigList=State#state.masterConfigList,
    {BoardId,{providedServices,ProvidedServices},{neededServices,NeededServices}}
	=lists:keyfind(BoardId,1,ConfigList),
    Reply={ok,ProvidedServices,NeededServices},
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
start_board_mgr()->
   % timer:sleep(2000),
    application:load(board_mgr),
    application:start(board_mgr).

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
