%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  8 Jul 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('ajax').
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
%% API functions
start(Args) ->
  start(),
  [?MODULE ! A || A <- Args].

start() ->
  gen_serv:start(?MODULE).

state() ->
  handle_state().

stop() ->
  gen_serv:stop(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_serv callbacks

%% gen_serv get_state formatting callback
-record(ld,{acceptor, sockets=[], quiet=false}).

rec_info(ld) -> record_info(fields,ld).

%% gen_serv init callback
init(_) ->
  ?log({starting,now()}),
  gen_serv:unlink(),
  erlang:process_flag(trap_exit,true),
  #ld{acceptor=accept(self(),tcp_port(),tcp_opts())}.

%% gen_serv handle_info callback
handle_info({'EXIT',Pid,_},#ld{acceptor=Pid}) ->
  %% the acceptor died. we'll restart and hope for the best.
  #ld{acceptor=accept(self(),tcp_port(),tcp_opts())};

handle_info({quiet,Bool},LD) ->
  LD#ld{quiet=Bool};

handle_info(quiet,LD) ->
  LD#ld{quiet=(not LD#ld.quiet)};

handle_info({new_socket,Socket},LD) ->
  inet:setopts(Socket,[{active,once}]),
  LD#ld{sockets=[Socket|LD#ld.sockets]};

handle_info({tcp_closed,Socket},LD) -> 
  LD#ld{sockets=LD#ld.sockets--[Socket]};

handle_info({tcp,Socket,Data},LD) -> 
  try gen_tcp:send(Socket,http_reply(answer(parse(Data),LD)))
  catch
    no_return -> ok;
    C:R -> ?log([{C,R},{bt,erlang:get_stacktrace()}])
  after gen_tcp:close(Socket)
  end,
  LD#ld{sockets=LD#ld.sockets--[Socket]}.

handle_state() -> gen_serv:get_state(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% accept is blocking, so it runs in its own process
accept(Daddy,Port,Opts) ->
  spawn_link(fun() -> acceptor(Daddy,Port,Opts) end).

acceptor(Daddy,Port,Opts) ->
  {ok,ListenSock} = gen_tcp:listen(Port,Opts),
  acceptor_loop(Daddy,ListenSock).

acceptor_loop(Daddy,ListenSock) ->
  {ok,Socket} = gen_tcp:accept(ListenSock),
  gen_tcp:controlling_process(Socket,Daddy),
  Daddy ! {new_socket,Socket},
  acceptor_loop(Daddy,ListenSock).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% implementation details

tcp_port() ->
  8080.

tcp_opts()->
  [binary, 
   {packet, 0}, 
   {reuseaddr, true},
   {active, false}].

parse(Request) -> 
  try 
    ["get","/"++Path,"http/"++_] = http_header(Request),
    {get,Path}
  catch 
    _:R -> throw({error_parsing_header,R,Request})
  end.

http_header(Request) ->
  low_string(re:split(hd(re:split(Request,"\r\n"))," ")).

low_string(Bs) -> [string:to_lower(binary_to_list(B))||B<- Bs].

http_reply(404) ->
  ["HTTP/1.1 404 NotFound\r\n",
   "Content-Length: 0\r\n",
   "Content-Type: text/html\r\n"
   "Server: Smarty\r\n",
   "\r\n"];
http_reply(Reply) ->
  ["HTTP/1.1 200 OK\r\n",
   "Content-Length: ", integer_to_list(byte_size(Reply)),"\r\n",
   "Content-Type: text/html\r\n"
   "Server: Smarty\r\n",
   "\r\n",
   Reply].

answer({get,"somedata"},_) ->
  <<"key: val;">>;
answer({get,File},_) ->
  try {ok,Res} = file:read_file(filename:join(here(),File)),
      Res
  catch _:R -> 
      ?log({not_found,R,File}),
      404
  end.

here() ->
  filename:dirname(code:which(?MODULE)).
