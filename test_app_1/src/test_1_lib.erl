%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : mymath using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(test_1_lib).



%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%  -include("").
%% --------------------------------------------------------------------
%% External exports

-export([add/2,divi/2,sum/1,test/2]).
%-export([add/2,divi/2]).

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% -------------------------------------------------------------------
test(Base,N)->
    case sd:connect(mymath) of
	{ok,I}->
%	    io:format("~p~n",[{?MODULE,self(), Base,I}]),
	    case sd:call(I,add,[Base,1],1000) of
		{ok,Base1}->
		    N1=N-1;
		{error,_Err}->
	%	    io:format("no conatct with Instance ~p~n",[{ self(),Err}]),
		    Base1=Base,
		    N1=N
	    end;
	{error,Err}->
	    io:format("~p~n",[{ self(),error,Err}]),
	    Base1=Base,
	    N1=N
    end,
    {Base1,N1}.



add(A,B)->
   % io:format("Snart går världens bästa tjej ut gymansiet *********** ~p~n",[{}]),
   %  io:format(" fel på raden ~p~n",[{A+B}]),
   %   io:format("    ~p~n",[{A+B}]),	
    A+B.


divi(A,B)-> 
    A/B.

sum(0)->
    0;
sum(1) ->
    1;
sum(N) ->
    N+sum(N-1).


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: get_all/0
%% Description:if needed creates dets file with name ?MODULE, and
%% initates the debase
%% Returns: non
%% --------------------------------------------------------------------
