% This record defines the structure of the 
% client process. 
% 
% It contains the following fields: 
%
% gui: it stores the name (or Pid) of the GUI process.
%
-record(cl_st, {nickname, gui, server, channels}).


    
% This record defines the structure of the 
% server process. 
% 
-record(server_st, {server, connected_pids, connected_nicks, channels}).


% This record defines the structure of the 
% channel process. 
% 
-record(channel_st, {channel_name, joined_pids, joined_nicks}).
