-module(client).
-export([main/1, initial_state/2]).
-import(helper, [start/3, request/2, request/3, requestAsync/2, timeSince/1]).
-include_lib("./defs.hrl").


main(State) ->
    receive
        {request, From, Ref, Request} ->
            {Response, NextState} = loop(State, Request),
            From ! {result, Ref, Response},
            main(NextState)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Connect to a server
%%%%%%%%%%%%%%%%%%%%%%%%
loop(St, {connect, Server}) ->
    case St#cl_st.server =/= none of
        true ->
            {{error, user_already_connected, "You are already connected"}, St};
        false ->
            case catch (helper:request(list_to_atom(Server), {connect, self() , St#cl_st.nickname})) of
                ok ->
                    {ok, St#cl_st{server = Server} } ;
                {'EXIT', {error, nick_already_taken}} ->
                    { {error, user_already_connected, "The nickname is already taken"}, St};
                {'EXIT',_Reason} ->
                    { {error, server_not_reached,"Server not found"}, St}
            end
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Disconnect from a server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(St, disconnect) ->
    case St#cl_st.server  == none of
        true ->
            {{error, user_not_connected, "You are not connected to any server"}, St};
        false ->
            case length(St#cl_st.channels) == 0 of
                true ->
                    case catch(helper:request(list_to_atom(St#cl_st.server), {disconnect, self() , St#cl_st.nickname})) of
                         ok ->
                            {ok, St#cl_st{server = none} } ;
                        {'EXIT',_Reason} ->
                            { {error, server_not_reached,"Server not found"}, St}
                    end;
                false ->
                    {{error, leave_channels_first, "You should leave all joined channels first"}, St}
            end
    end;

%%%%%%%%%%%%%%%%%%
%%% Join a channel
%%%%%%%%%%%%%%%%%%
loop(St,{join, Channel}) ->

    case St#cl_st.server == none of 
        true -> 
            {{error, user_not_connected, "You have to connect to a server first"}, St};
        false ->
            case lists:member(Channel, St#cl_st.channels) of
                true -> 
                    {{error, user_already_joined, "You are already join to this channel"}, St};
                false ->
                    case catch helper:request(list_to_atom(St#cl_st.server), {join, self(), Channel, St#cl_st.nickname}) of
                        ok ->
                            {ok, St#cl_st{ channels = lists:append(St#cl_st.channels, [Channel])}};

                        {'EXIT',_Reason} ->
                            { {error, server_not_reached, _Reason }, St}
                    end
            end        
    end;

%%%%%%%%%%%%%%%%%%%
%%% Leave a channel
%%%%%%%%%%%%%%%%%%%
loop(St, {leave, Channel}) ->  
    case St#cl_st.server == none of 
        true -> 
            {{error, user_not_connected, "You have to connect to a server first"}, St};
        false ->
            case lists:member(Channel, St#cl_st.channels) of
                true ->
                    case catch helper:request(list_to_atom(Channel), {leave, self() }) of
                        ok ->
                            {ok, St#cl_st{channels = lists:delete(Channel, St#cl_st.channels) } };
                        {'EXIT',_Reason} ->
                            { {error, server_not_reached, _Reason}, St}
                    end;
                false ->
                    {{error, user_not_joined, "You are not member of such channel"}, St}
            end
    end;

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, Channel, Msg}) ->
    case lists:member(Channel, St#cl_st.channels) of
        true ->
            case catch helper:request(list_to_atom(Channel), {message_to_all, Msg, self(), St#cl_st.nickname}) of
                ok ->
                    {ok, St};
                {'EXIT',_Reason} ->
                    { {error, server_not_reached, _Reason}, St}
            end;
        false ->
            {{error, user_not_joined, "You are not a member of this channel"}, St}
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WhoIam, Show the user's nickname
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(St, whoami) ->
    {St#cl_st.nickname, St} ;

%%%%%%%%%%%%%%%%%%%%%
%%% Change a nickname
%%%%%%%%%%%%%%%%%%%%%
loop(St,{nick, Nick}) ->
    if
        St#cl_st.server == none ->
            {ok, St#cl_st{nickname = Nick}};
        true ->
            {{error, user_already_connected, "To change the nickname execute /disconnect first"}, St}
    end;

%%%%%%%%%%%%%%%%%%%%%
%%%% Incoming message
%%%%%%%%%%%%%%%%%%%%%
loop(St = #cl_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St};

loop(St,{ping, Destination}) ->
    if
        St#cl_st.server == none ->
            {{error, user_not_connected, "You have to connect to a server first"}, St};
        true ->
            St#cl_st{pingTime = now()},
            case catch helper:request(list_to_atom(St#cl_st.server), {ping_message, self(), Destination}) of
                ok ->
                    {ok, St};
                {error, user_not_found} ->
                    {{error, user_not_found, "There is no such a user"}, St}
            end
    end;


%%%%%%%%%%%%%%%%%%%%%
%%%% Incoming PONG message
%%%%%%%%%%%%%%%%%%%%%
loop(St = #cl_st { gui = GUIName }, {incoming_pong_msg, Pid, Name, Msg}) ->

    gen_server:call(list_to_atom(GUIName), {msg_to_SYSTEM, io_lib:format("Pong ~s: ~pms", [UserNick,Diff])}),
    {ok, St}.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Client initial state
%%%%%%%%%%%%%%%%%%%%%%%%%
initial_state(Nick, GUIName) ->
    #cl_st { nickname = Nick, gui = GUIName, server = none, channels = [] , pingTime = 0}.
