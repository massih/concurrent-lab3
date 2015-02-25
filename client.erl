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
    case St#cl_st.server of
        none ->
            {{error, user_not_connected, "You are not connected to any server"}, St};
        _ ->
            case length(St#cl_st.channels) of
                0 ->
                    case catch(request(list_to_atom(St#cl_st.server), {disconnect, self() , St#cl_st.nickname})) of
                         ok ->
                            {ok, St#cl_st{server = none} } ;
                        {'EXIT',_Reason} ->
                            { {error, server_not_reached,"Server not found"}, St}
                    end;
                _ ->
                    {{error, leave_channels_first, "You should leave all joined channels first"}, St}
            end
    end;

%%%%%%%%%%%%%%
%%% Join
%%%%%%%%%%%%%%
loop(St,{join,_Channel}) ->
    {ok, St} ;

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
loop(St,{nick, Nick}) ->
    if
        St#cl_st.server == none ->
            {ok, St#cl_st{nickname = Nick}};
        true ->
            {{error, user_already_connected, "To change the nickname enter /disconnect first"}, St}
    end;

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
    #cl_st { nickname = Nick, gui = GUIName, server = none, channels = [] }.
