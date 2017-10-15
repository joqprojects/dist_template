%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : 
%%% Manage the boards (workers) and are the interface to master_node
%%% 1. Connects to master node
%%% 2. Starts/stops applications
%%% 3. resource discovery
%%% 4. SW upgrade
%%% Uses distributed erlang
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(board_mgr).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
-include_lib("kernel/include/file.hrl").
%% --------------------------------------------------------------------
-define(POLL_TIME,20000).
-define(NUM_CONNECT_TRIES,10).
-define(CONNECT_INTERVAL,3000).
-define(WAIT_FOR_RESOURCES,3000).
-define(MASTER_NODE,'masternode@home.joqhome.eu').
-define(NUM_TRADE_TRIES,10).
-define(TRADE_SLEEP_TIME,3000).
-define(EBIN,"ebin").
-define(UPGRADE_SLEEP,1000).

%% External exports
-export([execute/3,
	 check_upgrade/0,
	 add/2,divi/2]).


-export([start/0,
	 stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

add(A,B)->
    A+B.
divi(A,B)->
    A/B.


execute(M,F,A)-> 
    gen_server:call(?MODULE, {execute,M,F,A},infinity).
check_upgrade()-> 
    gen_server:call(?MODULE, {check_upgrade},infinity).

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
    io:format("init start ~p~n",[{?MODULE,?LINE}]),
    {ok,connected}=ensure_contact(?MASTER_NODE,?NUM_CONNECT_TRIES,false), 
    timer:sleep(3000), %wait for distributed erlang  
    {ProvidedServices,NeededServices}=get_services(?MASTER_NODE),

    ProvidedServicesResources=start_provided_services(ProvidedServices),    

    application:load(sd),
    application:start(sd),
    ok=store_provided_services(ProvidedServicesResources),
    ok=store_needed_services(NeededServices),

   % _Pid=spawn_link(fun()->poll_tick(?POLL_TIME) end),
    spawn(fun()->local_check_upgrade(?EBIN,10000) end),
    ok=trade_services(),

    io:format("provided ~p~n",[{?MODULE,?LINE,sd:get_provided()}]), 
    io:format("needed ~p~n",[{?MODULE,?LINE,sd:get_needed()}]),   
    io:format("found ~p~n",[{?MODULE,?LINE,sd:get_found()}]),    
    
    io:format("init end ~p~n",[{?MODULE,?LINE}]),
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


% --------------------------------------------------------------------
%% Function: stop/0
%% Description:
%% 
%% Returns: non
%% --------------------------------------------------------------------
handle_call({execute, M,F,A}, _From, State) ->
    io:format(" ~p~n",[{time(),?MODULE,?LINE,{M,F,A}}]),
    case {M,F} of
	{os,cmd}->
	    Reply={error,os,cmd,'not implemented'};
	{os_cmd,_}->
	    Reply=apply(os,F,A);
	_->
	    Reply=apply(M,F,A)
    end,
    {reply, Reply, State};


handle_call({check_upgrade}, _From, State) ->
    Reply=local_check_upgrade(?EBIN),
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
    io:format("unmatched signal  ~p~n",[{?MODULE,?LINE,Info}]),    
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
%% Function: ensure_contact
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

ensure_contact(_MasterNode,0,false)->
    {error,failed_to_connect};
ensure_contact(_MasterNode,_N,true)->
    {ok,connected};
ensure_contact(MasterNode,N,false) ->
    case net_adm:ping(MasterNode) of
       pong->
	    Connected=true,
	    N1=N;
	pang->
	    Connected=false,
	    N1=N-1
    end,
    ensure_contact(MasterNode,N1,Connected). 

%% --------------------------------------------------------------------
%% Function: get_provided_services
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
get_services(?MASTER_NODE)->
    case rpc:call(?MASTER_NODE,master_node,get_config,[node()]) of
	{ok,ProvidedServices,NeededServices}->
	    Reply={ProvidedServices,NeededServices};
	Err->
	    Reply={error,Err}
    end,
    Reply.   


%% --------------------------------------------------------------------
%% Function: store_provided_services
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
store_provided_services([])->
    ok;
store_provided_services([{Service,Instance}|T])->
    ok=sd:add_provided_service(Service,Instance),
    store_provided_services(T).

store_needed_services([])->
    ok;
store_needed_services([Service|T])->
    ok=sd:add_needed_service(Service),
    store_needed_services(T).
   

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

trade_services()->  
    Reply=trade_services(do_trade,?NUM_TRADE_TRIES),
    Reply.
trade_services(do_trade,0)->
    {error,'missing_needed_services'};
trade_services(quit,_N)->
    ok;
trade_services(do_trade,N)->
    io:format(" ~p~n",[{?MODULE,?LINE,do_trade,N}]),
    sd:trade_services(),
    timer:sleep(?TRADE_SLEEP_TIME),
    Needed=sd:get_needed(),
    Found=sd:get_found(),
    Diff=lists:flatlength(Needed)-lists:flatlength(Found),
    if 
	Diff==0->       % Got all needed services
	    Action=quit,
	    N1=0;
	true->          % Missing some needed services 
	    Action=do_trade,
	    N1=N-1
    end,
    trade_services(Action,N1).
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

start_provided_services([])->
    [];
start_provided_services(ProvidedServices)->
    ProvidedServicesResources=start_provided_services(ProvidedServices,[]),
    ProvidedServicesResources.

start_provided_services([],Acc)->
    Acc;
start_provided_services([Service|T],Acc) ->    
    ok=application:load(Service),
    ok=application:start(Service),
   
    Instance=Service:connect(),
    Acc1=[{Service,Instance}|Acc],
    start_provided_services(T,Acc1).
    
    
%% --------------------------------------------------------------------
%% Function: 
%% Description:

    
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
local_check_upgrade(Ebin,TimeOut)->
 %   io:format(" ~p~n",[{?MODULE,?LINE,Ebin,TimeOut}]),
    timer:sleep(TimeOut),
    ok=local_check_upgrade(Ebin),
    spawn(fun()->local_check_upgrade(Ebin,TimeOut) end).
   
local_check_upgrade(Ebin)->
%    io:format("check_upgrade= ~p~n",[{?MODULE,?LINE,time()}]),
    {ok,CWD}=file:get_cwd(),
    EbinPath=filename:join(CWD,Ebin),
    {ok,LocalFiles}=file:list_dir(EbinPath),
    LocalFileList=create_file_list(LocalFiles,EbinPath),
    case sd:connect(sw_mgr) of
	{ok,I}->
	    case sd:call(I,get_file_list,[],10000) of
		{error,Err}->
		    io:format("Error ~p~n",[{?MODULE,?LINE,{error,Err}}]),
		    Reply={error,{?MODULE,?LINE,Err}};
		{ok,SourceFileList}->
		    
	  %  io:format("SourceFileList ~p~n",[{?MODULE,?LINE,SourceFileList}]),
		    {FilesToUpdate,FilesToDelete}=compare_files(LocalFileList,SourceFileList),
%	  io:format("{FilesToUpdate,FilesToDelete} ~p~n",[{?MODULE,?LINE,{FilesToUpdate,FilesToDelete}}]),
		    UpgradeList=createUpgradeList(FilesToUpdate,masterNode),
						%  io:format("UpgradeList= ~p~n",[{?MODULE,?LINE,UpgradeList}]),
		    done=do_upgrade(UpgradeList,Ebin),
		    done=delete_files(FilesToDelete,EbinPath),
		    Reply=ok;    
		X ->
		    io:format(" ~p~n",[{?MODULE,?LINE,X}]),
		    Reply=1/0
	    end;
	{error,Err}->
	    Reply={error,{?MODULE,?LINE,Err}}
    end,
    Reply.
%% --------------------------------------------------------------------
%% Function: ensure_contact
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


createUpgradeList([],_MasterNode)->
    [];
createUpgradeList(FilesToUpdate,MasterNode)->
    UpgradeList=createUpgradeList(FilesToUpdate,MasterNode,[]),
    UpgradeList.

createUpgradeList([],_MasterNode,Acc)->
    Acc;
createUpgradeList([{FileName,_Facts}|T],MasterNode,Acc) ->
    case sd:connect(sw_mgr) of
	{ok,I}->
	    case sd:call(I,get_file,[FileName],10000) of
		{ok,{FileName,Bin}}->
		    Acc1=[{FileName,Bin}|Acc];
		{error,_Err}->
		    Acc1=Acc
	    end;
	{error,_Err}->
	    Acc1=Acc
    end,
    createUpgradeList(T,MasterNode,Acc1). 

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
delete_files([],_EbinPath)->
    done;
delete_files([{FileName,_Facts}|T],EbinPath)->
  io:format("Delete ~p~n",[{?MODULE,?LINE,time(),FileName}]),
    FullFileName=filename:join(EbinPath,FileName),
    ok=file:delete(FullFileName),
    delete_files(T,EbinPath).


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


create_file_list([],_)->
    {error,empty_directory};
create_file_list(Files,Path) ->
    FileList=create_file_list(Files,Path,[]),
    FileList.

create_file_list([],_Path,Acc)->
    Acc;
create_file_list([BaseName|T],Path,Acc) ->
    FullName=filename:join(Path,BaseName),
    {ok,Facts}=file:read_file_info(FullName),
    Acc1=[{BaseName,Facts}|Acc],
    create_file_list(T,Path,Acc1).



compare_files(LocalFileList,SourceFileList)->
    
    {FilesToUpdate,FilesToDelete}=compare_files(LocalFileList,SourceFileList,{[],[]}), 
    {FilesToUpdate,FilesToDelete}.

compare_files([],[],{Update,Delete})->
    {Update,Delete};
compare_files(LocalFileList,SourceFileList,{Update,Delete})->  
    case {LocalFileList,SourceFileList} of
	{[],SourceFileList}-> 
	    Update1=lists:append(SourceFileList,Update),
	    Delete1=Delete,
	    SourceFileList1=[],
	    LocalFileList1=LocalFileList;
	 {LocalFileList,[]}->
	    Delete1=lists:append(LocalFileList,Delete),
	    Update1=Update,
	    LocalFileList1=[],
	    SourceFileList1=SourceFileList;
	 {[{FileName,LocalFacts}|_T],SourceFileList}->
	    case lists:keyfind(FileName,1,SourceFileList) of
		{FileName,SourceFacts}->
		    case update(LocalFacts,SourceFacts) of
			false->
			    Update1=Update,
			    Delete1=Delete;
			true->
			    Update1=[{FileName,SourceFacts}|Update],
			    Delete1=Delete
		    end,
		    LocalFileList1=lists:keydelete(FileName,1,LocalFileList),
		    SourceFileList1=lists:keydelete(FileName,1,SourceFileList);
		false ->
		    Delete1=[{FileName,LocalFacts}|Delete],
		    Update1=Update,
		    LocalFileList1=lists:keydelete(FileName,1,LocalFileList),
		    SourceFileList1=SourceFileList
	    end
    end,
    compare_files(LocalFileList1,SourceFileList1,{Update1,Delete1}).
    
update(LocalFacts,SourceFacts)->	    
    {S_date,S_time} = SourceFacts#file_info.mtime,
    {D_date,D_time} = LocalFacts#file_info.mtime,
    if
	S_date > D_date	-> Upgrade = true;
	S_date == D_date ->
	    if
		S_time > D_time -> Upgrade = true;
		S_time < D_time -> Upgrade = false;
		S_time == D_time -> Upgrade = false
	    end;
	S_date < D_date	-> Upgrade = false
    end,
    Upgrade.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%poll_tick(PollTime)->
 %   board_mgr:check_upgrade(),
  %  timer:sleep(PollTime),
   % poll_tick(PollTime).

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
do_upgrade([],_Ebin)->
    done;
do_upgrade([{FileName,Bin}|T],Ebin)->
    {ok,CWD}=file:get_cwd(),
%    {ebin_path,Ebin}= read_config(?BOARDCONFIGFILE,ebin_path),
    Path=filename:join(CWD,Ebin),
    FullFileName=filename:join(Path,FileName),
    ok=file:write_file(FullFileName,Bin),
    case filename:extension(FileName) of
	".beam"-> % purge 
	    RootName=filename:rootname(FileName),
	    Module=list_to_atom(RootName),
	    io:format(" ~p~n",[{?MODULE,?LINE,Module}]),
	    {Module,Bin,EbinFileName}=code:get_object_code(Module),
	    code:load_binary(Module, EbinFileName, Bin);
	".app"->
	    copy_file;
	_ ->
	    ignore
    end,
    do_upgrade(T,Ebin).
