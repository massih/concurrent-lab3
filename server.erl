-module(server).
-export([main/1, main_channel/1, initial_state/1, initial_state_channel/1]).
-import(helper, [start/3, request/2, request/3, requestAsync/2, timeSince/1]).

-include_lib("./defs.hrl").


main(State) ->
    receive
        {request, From, Ref, Request} ->
            {Response, NextState} = loop(State, Request),
            From ! {result, Ref, Response},
            main(NextState)
    end.


%%%%%%%%%%%%%%%%%%%%%%%
%%% Connect to a server
%%%%%%%%%%%%%%%%%%%%%%%
loop(St, {connect, Pid , Client}) ->
	case lists:member(Client, St#server_st.connected_nicks) of
		true ->
			{{'EXIT', {error, nick_already_taken}}, St };
		false ->
			NewSt = St#server_st{connected_pids = lists:append(St#server_st.connected_pids,[Pid]), 
			connected_nicks = lists:append(St#server_st.connected_nicks,[Client]),
			pidNick = lists:append(St#server_st.pidNick, [{Pid, Client}]) },
			{ok,  NewSt}		
	end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Disconnect from a server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(St, {disconnect, Pid , Client}) ->
	NewSt = St#server_st{connected_pids = lists:delete(Pid, St#server_st.connected_pids), 
	connected_nicks = lists:delete(Client, St#server_st.connected_nicks),
	pidNick = lists:delete([{Pid, Client}], St#server_st.pidNick)},
	{ok,  NewSt};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Handeling PING message to destination
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(St, {ping_message, Pid, Destination}) ->
	case lists:member(Destination, St#server_st.connected_nicks) of
		true ->
			{DestPid, _DestNick} = lists:keyfind(Destination, 2, St#server_st.pidNick),
			helper:requestAsync(DestPid, {ping_message, Pid}),
			{ok, St};
		false ->
			{{error, user_not_found}, St}
	end;


%%%%%%%%%%%%%%%%%%
%%% Join a channel
%%%%%%%%%%%%%%%%%%
loop(St, {join, Pid, Channel, Nickname}) ->

	case (lists:member(Channel, St#server_st.channels)) of
		true ->
			case catch (helper:request(list_to_atom(Channel), {connect, Channel, Pid, Nickname})) of
				ok ->
					{ok, St#server_st{channels = lists:append(St#server_st.channels, [Channel])}}
			end;
		
		false -> 
			helper:start(list_to_atom(Channel), server:initial_state_channel(Channel), fun server:main_channel/1),
			case catch helper:request(list_to_atom(Channel), {connect, Channel, Pid, Nickname}) of
				ok ->
					{ok, St#server_st{channels = lists:append(St#server_st.channels, [Channel]) } };
				{'EXIT', _Reason} ->
					{ {error, could_initiate_loop_channel, _Reason}, St}
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Channel %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main_channel(State) ->
    receive
        {request, From, Ref, Request} ->
            {Response, NextState} = loop_channel(State, Request),
            From ! {result, Ref, Response},
            main_channel(NextState)
    end.

%%%%%%%%%%%%%%%%%%%%%%%
%%%% Connect to channel
%%%%%%%%%%%%%%%%%%%%%%%
loop_channel(St, {connect, Channel ,Pid, Nickname}) ->
	{ok, St#channel_st{channel_name = Channel, joined_pids = lists:append( St#channel_st.joined_pids, [Pid]),
	 joined_nicks = lists:append(St#channel_st.joined_nicks, [Nickname])}};

%%%%%%%%%%%%%%%%%%%%
%%%% Leave a channel
%%%%%%%%%%%%%%%%%%%%
loop_channel(St,{leave, Pid}) ->
	{ok, St#channel_st{joined_pids = lists:delete(Pid, St#channel_st.joined_pids) } };

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Send message to all members of a channel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop_channel(St,{message_to_all, Msg, Pid, Nickname}) ->
	spawn(fun() -> msg_to_all(St#channel_st.joined_pids, Msg, Pid, Nickname, St#channel_st.channel_name) end),
	{ok, St}.

msg_to_all([], _Msg, _Pid, _Nickname, _Channel_name) ->
	ok;

msg_to_all([H|T], Msg, Pid, Nickname, Channel_name) ->
	case H == Pid of
		true ->
			msg_to_all(T, Msg, Pid, Nickname, Channel_name);
		false ->
			spawn(fun() -> helper:request(H, {incoming_msg, Channel_name, Nickname, Msg}) end),
			msg_to_all(T, Msg, Pid, Nickname, Channel_name)

	end.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Server initial state
%%%%%%%%%%%%%%%%%%%%%%%%%
initial_state(Server) ->
    #server_st{server = Server, connected_nicks = [], connected_pids = [], channels =[], pidNick = []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Channel initial state
%%%%%%%%%%%%%%%%%%%%%%%%%%
initial_state_channel(Channel) ->
	#channel_st{channel_name = Channel, joined_pids = [], joined_nicks = []}.