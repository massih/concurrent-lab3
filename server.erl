-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, {connect, Pid , Client}) ->
	case lists:member(Client, St#server_st.connected_nicks) of
		true ->
			{'EXIT', {error, nick_already_taken}};
		false ->
			NewSt = St#server_st{connected_pids = lists:append(St#server_st.connected_pids,[Pid]), connected_nicks = lists:append(St#server_st.connected_nicks,[Client])},
			{ok,  NewSt}			
	end;

loop(St, {disconnect, Pid , Client}) ->
		NewSt = St#server_st{connected_pids = lists:delete(Pid, St#server_st.connected_pids), connected_nicks = lists:delete(Client, St#server_st.connected_nicks)},
		{ok,  NewSt};

loop(_St,_Msg) ->
	{ok,_St}.
		
    %{result, connect}.

% loop(_St, _Msg) -> 
%     {ok, _St}. 


initial_state(Server) ->
    #server_st{server = Server, connected_nicks = [], connected_pids = []}.
