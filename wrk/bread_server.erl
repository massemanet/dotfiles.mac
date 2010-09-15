%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 10 Jun 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('bread_server').
-author('Mats Cronqvist').
-export([get_bread/0
         , stop/0
         , start/2]).
-export([rec_info/1
         , init/1
         , handle_info/2]).

%% the api
start(FilePattern,Type) ->
  gen_serv:start(?MODULE,{FilePattern,Type}).

stop() ->
  ?MODULE ! stop.

get_bread() ->
  ?MODULE ! {get_bread,self()},
  receive
    {bread,Bread} -> Bread
  end.

%% state
-record(ld,{file,files,type,bread_pid,bread_ref}).
rec_info(ld) -> record_info(fields,ld).

%% gen_serv callbacks
init({FilePattern,Type}) ->
  case filelib:wildcard(FilePattern) of
    [File|Files] -> 
      start_breader(File,Files,#ld{type=Type});
    [] ->
      exit(no_files)
  end.

handle_info({'DOWN',BR,_,BP,normal},LD = #ld{bread_pid=BP,bread_ref=BR}) ->
  case LD#ld.files of
    [File|Files] -> start_breader(File,Files,LD);
    [] -> {stop,normal,LD}
  end;
handle_info({get_bread,Pid},LD) ->
  LD#ld.bread_pid ! {get_bread,Pid},
  LD.

%% implementation
start_breader(File,Files,LD) ->
  {BP,BR} = spawn_monitor(fun() -> breader(File,LD#ld.type) end),
  LD#ld{file=File,files=Files,bread_pid=BP,bread_ref=BR}.

%% runs in its own process
breader(File,Type) -> 
  bread:fold(File,fun breaded/2,0,[Type]).
  
breaded(Blob,N) ->
  receive
    quit -> exit(N);
    {get_bread,Pid} -> Pid ! {bread,Blob}, N+1
  end.
