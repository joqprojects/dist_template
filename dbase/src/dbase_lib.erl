%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(dbase_lib).



%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------
%% External exports

-export([delete/1,exists/1,create/2,get/2,store/3,all/1,remove/2]).


%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions
%% ====================================================================

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
delete(File) ->
    case filelib:is_file(File) of 
	true->
	    file:delete(File),
	    Reply={ok,file_deleted};
	false->
	   Reply={ok,file_not_exist}
    end,
    Reply.

exists(File) ->
    Reply=filelib:is_file(File),
    Reply.

create(Type,File) ->
    case filelib:is_file(File) of 
	true->
	    Reply={ok,file_already_exsist};
	false->
	    {ok,Descriptor}=dets:open_file(File,[{type,Type}]),
	    dets:close(Descriptor),
	    Reply={ok,file_created}
    end,
    Reply.


store(Key,Value,File) ->
    case filelib:is_file(File) of 
	true->
	    {ok,Descriptor}=dets:open_file(File),
	    ok=dets:insert(Descriptor, {Key,Value}),
	    dets:close(Descriptor),
	    Reply={ok,store};
	false->
	    Reply = {error,no_file}
    end,
    Reply.

get(Key,File) ->
    Reply=l_get(Key,File),
    Reply.

all(File) ->
    case filelib:is_file(File) of 
	true->
	    {ok,Descriptor}=dets:open_file(File),
	    Key=dets:first(Descriptor),
	    Reply=get_all(Descriptor,Key,[]),
	    dets:close(Descriptor);
	false->
	    Reply = {error,no_file}
    end,
    Reply.

remove(Key,File) ->
    case filelib:is_file(File) of 
	true->
	    {ok,Descriptor}=dets:open_file(File),
	    case dets:lookup(Descriptor, Key) of
		[]->
		    Reply = {error,no_entry};
		X->
		    ok=dets:delete(Descriptor, Key),
		    [{Key,Value}]=X,
		    Reply={ok,Value}
	    end,
	    dets:close(Descriptor);
	false->
	    Reply = {error,no_file}
    end,
    Reply.


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: get_all/0
%% Description:if needed creates dets file with name ?MODULE, and
%% initates the debase
%% Returns: non
%% --------------------------------------------------------------------
get_all(_Desc,'$end_of_table',Acc)->
    {ok,Acc};
get_all(Desc,Key,Acc)->  
    Status=dets:lookup(Desc, Key),
    Acc1=lists:append(Status,Acc),
    Key1=dets:next(Desc,Key),
    get_all(Desc,Key1,Acc1).

%% Function: l_get/0
%% Description:local get funtion used by several server functions
%% Returns: {ok,Value}|{error,Errcode}
%% --------------------------------------------------------------------
l_get(Key,File)->
    case filelib:is_file(File) of 
	true->
	    {ok,Descriptor}=dets:open_file(File),
	    Value=dets:lookup(Descriptor, Key),
	    Reply={ok,Value},
	    dets:close(Descriptor);
	false->
	    Reply = {error,no_file}
    end,
    Reply.
