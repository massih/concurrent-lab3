-module(server).
-export([loop/2, initial_state/1,loop_channel/2, initial_state_channel/1]).
-import(helper, [start/3, request/2, request/3, requestAsync/2, timeSince/1]).

-include_lib("./defs.hrl").


loop(St, {connect, Pid , Client}) ->
	case lists:member(Client, St#server_st.connected_nicks) of
		true ->
			{'EXIT', {error, nick_already_taken}};
		false ->
			NewSt = St#server_st{connected_pids = lists:append(St#server_st.connected_pids,[Pid]), connected_nicks = lists:append(St#server_st.connected_nicks,[Client])},
			{ok,  NewSt}			
	end;


		
    %{result, connect}.

% loop(_St, _Msg) -> 
%     {ok, _St}. 

loop(St, {join, Pid, Channel, Nickname}) ->
	case (lists:member(Channel, St#server_st.channels)) of
		true ->
			case catch (request(list_to_atom(Channel), {connect, Channel, Pid, Nickname})) of
				ok ->
					{ok, St#server_st{channels = lists:append(St#server_st.channels, [Channel])}}
			end;
		
		false -> 
			genserver:start(list_to_atom(Channel), initial_state_channel(Channel), fun loop_channel/2),
			case catch genserver:request(list_to_atom(Channel), {connect, Channel, Pid, Nickname}) of
				ok ->
					{ok, St#server_st{channels = lists:append(St#server_st.channels, Channel) } };
				{'EXIT', _Reason} ->
					{ {error, could_initiate_loop_channel, _Reason}, St}
			end
	end;

loop(St,_Msg) ->
	{ok,St}.

initial_state(Server) ->
    #server_st{server = Server, connected_nicks = [], connected_pids = [], channels =[]}.


initial_state_channel(Channel) ->
	#channel_st{channel_name = Channel, joined_pids = [], joined_nicks = []}.


loop_channel(St, {connect, Channel ,Pid, Nickname}) ->
	{ok, St#channel_st{channel_name = Channel, joined_pids = lists:append( St#channel_st.joined_pids, [Pid]),
	 joined_nicks = lists:append(St#channel_st.joined_nicks, [Nickname])}};

loop_channel(_St,_Msg) ->
	{ok,_St}.

