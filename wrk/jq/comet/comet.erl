%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  8 Jul 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('comet').
-author('Mats Cronqvist').

%% The API
-export([start/0
         , start/1
         , state/0
         , stop/0]).

%% gen_serv callbacks
-export([rec_info/1
        , init/1
        , handle_info/2]).

-include_lib("eper/src/log.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start the server
%% comet:start().
%%
%% connect from a browser
%% http://sterlett:8080/comet.html
%% 
%% send data
%% comet!{data,"load",66}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API functions
start(Args) ->
  start(),
  [?MODULE ! A || A <- Args].

start() ->
  gen_serv:start(?MODULE).

state() ->
  handle_state().

stop() ->
  gen_serv:shutdown(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_serv callbacks

%% gen_serv get_state formatting callback
-record(ld,{acceptor, sockets=[],  quiet=false,
            here=filename:dirname(code:which(?MODULE))}).

rec_info(ld) -> record_info(fields,ld).

%% gen_serv init callback
init(_) ->
  ?log({starting,now()}),
  gen_serv:unlink(),
  erlang:process_flag(trap_exit,true),
  start_timer("load"),
  #ld{acceptor=accept(self(),tcp_port(),tcp_opts())}.

%% gen_serv handle_info callback
handle_info({'EXIT',Pid,Err},LD=#ld{acceptor=Pid}) ->
  %% the acceptor died. we'll restart and hope for the best.
  ?log({acceptor_died,Err}),
  case Err of
    {listen_error,_} -> {stop,shutdown,LD#ld{acceptor=undefined}};
    _ -> LD#ld{acceptor=accept(self(),tcp_port(),tcp_opts())}
  end;

handle_info({quiet,Bool},LD) ->
  LD#ld{quiet=Bool};

handle_info(quiet,LD) ->
  LD#ld{quiet=(not LD#ld.quiet)};

handle_info({new_socket,Socket},LD) ->
  inet:setopts(Socket,[{active,once}]),
  LD#ld{sockets=[Socket|LD#ld.sockets]};

handle_info({tcp_closed,Socket},LD) -> 
  close(Socket,LD);

handle_info({http,Socket,{http_request,'GET',{abs_path,"/"++Item},_}},LD) ->
  case answer(Item,LD) of
    {reply,Answer} -> reply(Answer,Socket,LD);
    postpone -> pd_append({sockets,Item},Socket),LD
  end;

handle_info({data,Item,Data}, LD) ->
  Sockets = pd_clear({sockets,Item}),
  Json = make_json(Data),
  all([fun(L)->reply(Json,S,L)end || S <- Sockets],LD);
  
handle_info({timeout,Timer,Item}, LD) ->
  case pd_read({timer,Item}) of
    Timer -> 
      start_timer(Item),
      Sockets = pd_clear({sockets,Item}),
      all([fun(L)->close(S,L)end || S <- Sockets],LD);
    _ValidTimer -> 
      %% Timer has been cancelled; drop it
      LD
  end;

handle_info(Info,LD) ->
  ?log([{weird_message,Info}]),
  LD.

tcp_opts()->
  [binary, 
   {packet, http}, 
   {active, true},
   {reuseaddr, true}].

make_json(Data) ->
  to_binary("{data:"++to_list(Data)++"}").

start_timer(Item) ->
  case pd_store({timer,Item},erlang:start_timer(2000,self(),Item)) of
    undefined -> ok;
    OldTimer -> erlang:cancel_timer(OldTimer)
  end.

reply(Answer,Socket,LD) ->
  gen_tcp:send(Socket,http_reply(Answer)),
  close(Socket,LD).

close(Socket,LD) ->
  gen_tcp:close(Socket),
  LD#ld{sockets=LD#ld.sockets--[Socket]}.

http_reply(Status) when is_integer(Status) ->
  http_reply(Status,<<>>);
http_reply(Reply) when is_binary(Reply) ->
  http_reply(200,Reply).

http_reply(Status,Reply) ->
  ["HTTP/1.1 ",integer_to_list(Status)," ",phrase(Status),"\r\n",
   "Content-Length: ", integer_to_list(byte_size(Reply)),"\r\n",
   "Content-Type: text/html\r\n"
   "Server: Smarty\r\n",
   "\r\n",
   Reply].

phrase(200) -> "OK";
phrase(404) -> "NotFound".

all([],O)     -> O;
all([F|Fs],O) -> 
  NO = try F(O) 
       catch _:R -> ?log({R,erlang:get_stacktrace()}),O 
       end,
  all(Fs, NO).

pd_read(Key) -> 
  get(Key).

pd_store(Key,Val) ->
  put(Key,Val).

pd_append(Key,Val) -> 
  case put(Key,[Val]) of
    undefined -> ok;
    Vals -> put(Key,[Val|Vals])
  end.

pd_clear(Key) ->
  case erase(Key) of
    undefined -> [];
    Vals -> Vals
  end.

to_binary(L) when is_list(L) -> list_to_binary(L).

to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(B) when is_integer(B) -> integer_to_list(B).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% accept is blocking, so it runs in its own process
accept(Daddy,Port,Opts) ->
  spawn_link(fun() -> acceptor(Daddy,Port,Opts) end).

acceptor(Daddy,Port,Opts) ->
  case gen_tcp:listen(Port,Opts) of
    {ok,ListenSock} -> acceptor_loop(Daddy,ListenSock);
    {error,Err} -> exit({listen_error,Err})
  end.

acceptor_loop(Daddy,ListenSock) ->
  {ok,Socket} = gen_tcp:accept(ListenSock),
  gen_tcp:controlling_process(Socket,Daddy),
  Daddy ! {new_socket,Socket},
  acceptor_loop(Daddy,ListenSock).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% implementation details

handle_state() ->
  gen_serv:get_state(?MODULE).

tcp_port() ->
  8080.

answer("load",_) ->
  postpone;
answer(File,LD) ->
  try {ok,Res} = file:read_file(filename:join(LD#ld.here,File)),
      {reply,Res}
  catch _:R -> 
      ?log({not_found,R,File}),
      {reply,404}
  end.
