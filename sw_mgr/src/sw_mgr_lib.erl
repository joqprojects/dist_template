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
-module(sw_mgr_lib).

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

-export([get_file_list/1,get_file/2,upgrade/2]).

get_file_list(Repo)->
    {ok,Files}=file:list_dir(Repo),
    Reply=create_file_list(Files,Repo),
    Reply.

get_file(FileName,Repo)->
    FullFileName=filename:join(Repo,FileName),
    {ok,Bin}=file:read_file(FullFileName),
    Reply={FileName,Bin},
    Reply.


upgrade(Module,Node)->
    {Module,Bin,FullFileName}=code:get_object_code(Module),
    rpc:call(Node, code, load_binary, [Module, FullFileName, Bin]),
    Reply=upgrade_ack,
    Reply.


%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------



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
