%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Created : 7 March 2015
%%% Revsion : 2015-06-19: 1.0.0 :  Created
%%% Description :
%%% Generic tcp server interface to internet and "middle man". Concept 
%%% described in Joe Armstrong book
%%% -------------------------------------------------------------------
-module(sd_test).

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

init_test()->
    pong=net_adm:ping('master_node@home.joqhome.eu'),
    ok=application:load(service_discovery),
    ok=application:start(service_discovery),
   % {ok,_}=service_discovery:start(),
    ok.

provided_test()->   
    %ok=service_discovery:add_target_resource_type(type1),
    ok=service_discovery:add_provided_service(service1,pid_11),
    %[type1]=service_discovery:get_target(),
    [{service1,[pid_11]}]=service_discovery:get_provided(),
    ok=service_discovery:add_provided_service(service1,pid_12),
    ok=service_discovery:add_provided_service(service2,pid_21),
    [{service1,[pid_12,pid_11]},{service2,[pid_21]}]
	=service_discovery:get_provided(),
    %ok=service_discovery:add_local_resource(l1,{ipadd,port2}),
    %[{l1,[{ipadd,port2},{ipadd,port}]}] =service_discovery:get_local(),
     ok.
needed_test()->   
    ok=service_discovery:add_needed_service(service_n11),
    [service_n11]=service_discovery:get_needed(),
    ok=service_discovery:add_needed_service(service_n12),
    ok=service_discovery:add_needed_service(service_n13),
    [service_n13,service_n12,service_n11]=service_discovery:get_needed(),
    ok.
   
%t2_test()->
 %   []=service_discovery:get_found(),
  %  service_discovery:trade_resources(),
   % glurk=service_discovery:get_found(),
   % ok=ok.
    
    
    
    
%%
%% Local Functions
%%
%% --------------------------------------------------------------------
%% Function: fun/x
%% Description: fun x skeleton 
%% Returns:ok|error
%% --------------------------------------------------------------------
