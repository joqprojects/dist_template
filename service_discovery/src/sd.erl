%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : based on resource discovery in OTP in Action 
%%% 1.Exposes local services to other SD and hold inventory
%%%   target services
%%% 2. Garbage collections of target services 
%%% 3. Support calls or cast
%%% 4. Interacts with connection pool
%%%  Key data types
%%%  Type = module 
%%%  Instance = {name/pid,Node}
%%%  ResourceTuples =
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(sd).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
 
%% --------------------------------------------------------------------

-define(TRADE_PERIOD,10*60*1000).


%% External exports

%% Service discovery functions
-export([add_needed_service/1,add_provided_service/2,delete_found_instance/1,
	 fetch_service/1,
	 trade_services/0,trade_services/1,
	 get_provided/0,get_needed/0,get_found/0
	]).
%% support functions
-export([connect/1,call/4]).


%% gen_server callbacks

-export([start/0,stop/0]).

-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {needed_services,  % I want part
	        provided_services_tuples,  % I have part
	        found_services_tuples  % Local cache of found resources
	       }).

%% ====================================================================
%% External functions
%% ====================================================================
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


%%%-----------------------------------------------------------------
connect(Service)->
    Reply=sd:fetch_service(Service), 
    Reply.


call(Instance,Cmd,Args,TimeOut)->
    Parent=self(),
    Pid=spawn(fun()->call(Parent,{Instance,Cmd,Args}) end),
    receive
	{Pid,R}->
	    Reply=R
    after TimeOut ->
	    sd:delete_found_instance(Instance),
	    Reply={error,{timeout,Instance,Cmd,Args}}
    end,		     
    Reply.

call(Parent,{Instance,Cmd,Args})->
    Instance!{self(),Cmd,Args}, %check i name must be use
    {Name,_Node}=Instance,
    receive
	{Name,R}->
	    Reply=R;
	X->
	    Reply=X
    end,
    Parent!{self(),Reply}.


%%%-----------------------------------------------------------------
add_needed_service(Service)->
    gen_server:cast(?MODULE, {add_needed_service,Service}). 
add_provided_service(Service,Instance)->
    gen_server:cast(?MODULE, {add_provided_service,Service,Instance}). 
	 
fetch_service(Service)->
    gen_server:call(?MODULE, {fetch_service,Service},infinity).

trade_services()->
    gen_server:cast(?MODULE, {trade_services}). 
trade_services({ReplyTo,Remotes})->
    gen_server:cast(?MODULE, {trade_services,{ReplyTo,Remotes}}).

delete_found_instance(Instance)->
    gen_server:cast(?MODULE, {delete_found_instance,Instance}).


%%%----------------------------------------------------------------
get_provided()->
    gen_server:call(?MODULE, {get_provided},infinity).
get_needed()->
    gen_server:call(?MODULE, {get_needed},infinity).
get_found()->
    gen_server:call(?MODULE, {get_found},infinity).

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
    io:format("Starting ~p~n",[{?MODULE,?LINE,time()}]),
    spawn(fun()->periodic_trade(?TRADE_PERIOD) end), % should die if sd restarts
    {ok, #state{needed_services = [],
	        provided_services_tuples = dict:new(),
	        found_services_tuples = dict:new()}
    }.

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

handle_call({fetch_service,Service},_From, State) ->
    case dict:find(Service,State#state.found_services_tuples) of
	{ok,ListOfServices}->
	    [Instance|T]=ListOfServices, 
	    NewDict=dict:store(Service,lists:append([T,[Instance]]),State#state.found_services_tuples),
	    NewState=State#state{found_services_tuples=NewDict}, %Round Robin
	    Reply={ok,Instance};
	X->
	    Reply={error,X},
	    NewState=State
    end,
    {reply, Reply, NewState};


handle_call({get_needed},_From, State) ->
    Reply=State#state.needed_services,
    {reply, Reply, State};
handle_call({get_provided},_From, State) ->
    Reply=dict:to_list(State#state.provided_services_tuples),
    {reply, Reply, State};
handle_call({get_found},_From, State) ->
    Reply=dict:to_list(State#state.found_services_tuples),
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

handle_cast({delete_found_instance,Instance}, State) ->
    ListOfServices=dict:fetch_keys(State#state.found_services_tuples),
    NewDict=delete_found_instance(Instance,ListOfServices,State#state.found_services_tuples,false),
    NewState=State#state{found_services_tuples=NewDict},
    {noreply,NewState};


handle_cast({add_needed_service,Service}, State) ->
   % io:format("~p~n",[{?MODULE,?LINE,time()}]),
    NeededServices=State#state.needed_services,
    NewNeededServices=[Service|lists:delete(Service,NeededServices)],
    {noreply, State#state{needed_services=NewNeededServices}};


handle_cast( {add_provided_service,Service,Instance}, State) ->
    ProvidedTuples=State#state.provided_services_tuples,
    NewProvidedTuples=add_resource(Service,Instance,ProvidedTuples),
    {noreply, State#state{provided_services_tuples=NewProvidedTuples}};


handle_cast({trade_services}, State) ->
    ProvidedTuples=State#state.provided_services_tuples,
    AllNodes=[node()|nodes()],
    lists:foreach(fun(Node) ->
			  gen_server:cast({?MODULE,Node},
					  {trade_services, {node(),ProvidedTuples}})
      end,
      AllNodes),
    {noreply, State};

handle_cast({trade_services, {ReplyTo,Remotes}},
	    #state{provided_services_tuples=ProvidedTuples,
		   needed_services=TargetTypes,
		   found_services_tuples = OldFound} =State) ->
    
    FilteredRemotes=resources_for_types(TargetTypes,Remotes),
    NewFound= add_resources(FilteredRemotes,OldFound),
    case ReplyTo  of
        noreply ->
	    ok;
        _ ->
	    gen_server:cast({?MODULE,ReplyTo},
			    {trade_services, {noreply, ProvidedTuples}})
    end,
   % io:format("tradeservices ~p~n",[{?MODULE,?LINE,self(),NewFound}]),
    {noreply, State#state{found_services_tuples=NewFound}};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,?LINE,time()}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
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
%%% Exported functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
periodic_trade(TimeOut)->
    sd:trade_services(),
    timer:sleep(TimeOut),
    periodic_trade(TimeOut).
    

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
delete_found_instance(_Instance,[],Dict,_Quit)->
    Dict;    
delete_found_instance(_Instance,_ListOfServices,Dict,true) ->
    Dict;
delete_found_instance(Instance,[Service|T],Dict,false)->
    case dict:find(Service,Dict) of
	{ok,ListOfInstances}->
	    case lists:member(Instance,ListOfInstances) of
		true->
		    ListWithoutInstance=lists:delete(Instance,ListOfInstances), 
		    NewDict=dict:store(Service,ListWithoutInstance,Dict),
		    Quit=true;
		false->
		    NewDict=Dict,
		    Quit=false
	    end;    
	_X->
	    NewDict=Dict,
	    Quit=false
    end,
    delete_found_instance(Instance,T,NewDict,Quit).  
%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------


add_resource(Type,Resource,ResourceTuples)->
    case dict:find(Type,ResourceTuples) of
	{ok,ResourceList}->
	    NewList=[Resource|lists:delete(Resource,ResourceList)],
	    dict:store(Type,NewList,ResourceTuples);
	error ->
	    dict:store(Type,[Resource],ResourceTuples)
    end.

add_resources([{Type,Resource}|T],ResourceTuples)->
    add_resources(T,add_resource(Type,Resource,ResourceTuples));
add_resources([],ResourceTuples) ->
    ResourceTuples.

resources_for_types(Types,ResourceTuples)->
    Fun =
	fun(Type,Acc) ->
		case dict:find(Type,ResourceTuples) of
		    {ok,List}->
			[{Type,Instance}||Instance <- List] ++Acc;
		    error ->
			Acc
		end
	end,
    lists:foldl(Fun,[],Types).
