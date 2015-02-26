-module(client).
-export([loop/2, initial_state/2]).
-import(helper, [start/3, request/2, request/3, requestAsync/2, timeSince/1]).
-include_lib("./defs.hrl").

%%%%%%%%%%%%%%%
%%%% Connect
%%%%%%%%%%%%%%%
loop(St, {connect, Server}) ->
    case Server == St#cl_st.server of
        true ->
            {{error, user_already_connected, "You are already connected"}, St};
        false ->
            case catch(request(list_to_atom(Server), {connect, self() , St#cl_st.nickname})) of
                ok ->
                    {ok, St#cl_st{server = Server} } ;
                {'EXIT', {error, nick_already_taken}} ->
                    { {error, user_already_connected,"The nickname is already taken"}, St};
                {'EXIT',_Reason} ->
                    { {error, server_not_reached,"Server not found"}, St}
            end
    end;

%%%%%%%%%%%%%%%
%%%% Disconnect
%%%%%%%%%%%%%%%
loop(St, disconnect) ->
    
     {ok, St} ;

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join, Channel}) ->
    case lists:member(Channel, St#cl_st.channels) of
        true -> 
            {{error, user_already_joined, "You are already join to this channel"}, St};
        false ->
            case catch request(list_to_atom(St#cl_st.server), {join, self(), Channel, St#cl_st.nickname}) of
                ok ->
                    {ok, St#cl_st{ channels = lists:append(St#cl_st.channels, [Channel])}};

                {'EXIT',_Reason} ->
                    { {error, server_not_reached, _Reason}, St}

            end
    end;

%%%%%%%%%%%%%%%
%%%% Leave
%%%%%%%%%%%%%%%
loop(St, {leave, _Channel}) ->
     {ok, St} ;

%%%%%%%%%%%%%%%%%%%%%
%%% Sending messages
%%%%%%%%%%%%%%%%%%%%%
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
     {ok, St} ;


%%%%%%%%%%%%%%
%%% WhoIam
%%%%%%%%%%%%%%
loop(St, whoiam) ->
    {St#cl_st.nickname, St} ;

%%%%%%%%%%
%%% Nick
%%%%%%%%%%
loop(St,{nick,_Nick}) ->
    {ok, St} ;

%%%%%%%%%%%%%
%%% Debug
%%%%%%%%%%%%%
loop(St, debug) ->
    {St, St} ;

%%%%%%%%%%%%%%%%%%%%%
%%%% Incoming message
%%%%%%%%%%%%%%%%%%%%%
loop(St = #cl_st { gui = GUIName }, _MsgFromClient) ->
    {Channel, Name, Msg} = decompose_msg(_MsgFromClient),
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.


% This function will take a message from the client and
% decomposed in the parts needed to tell the GUI to display
% it in the right chat room.
decompose_msg(_MsgFromClient) ->
    {"", "", ""}.


initial_state(Nick, GUIName) ->
    #cl_st { nickname = Nick, gui = GUIName , channels = [], server = none}.
