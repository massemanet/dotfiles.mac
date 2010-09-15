%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  8 Jan 2010 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('websocket_server').
-author('Mats Cronqvist').

%% The API
-export([start/0
         , start/1
         , stop/0]).

%% gen_serv callbacks
-export([rec_info/1
        , init/1
        , handle_info/2]).

-define(log(T),
        '?log'([process_info(self(),current_function), {line,?LINE}], T)).

'?log'(HD,T) when not is_integer(hd(T)) -> error_logger:info_report(HD++T);
'?log'(HD,T)  -> '?log'(HD,[T]).

-record(ld,{acceptor, quiet=false,
            here=filename:dirname(code:which(?MODULE))}).

rec_info(ld) -> record_info(fields,ld).

%% API functions
start(Args) ->  start(),
  [?MODULE ! A || A <- Args].

start() ->
  gen_serv:start(?MODULE).

stop() ->
  gen_serv:shutdown(?MODULE).

%% gen_serv init callback
init(_) ->
  ?log({starting,now()}),
  gen_serv:unlink(),
  erlang:process_flag(trap_exit,true),
  #ld{acceptor=accept(self(),tcp_port(),tcp_opts())}.

-define(HTTP_HDR(Sock),{http,Sock,{http_header,_,_,_,_}}).
-define(HTTP_EOH(Sock),{http,Sock,http_eoh}).
-define(HTTP_GET(Sock,I),{http,Sock,{http_request,'GET',{abs_path,"/"++I},_}}).

%% gen_serv handle_info callback
handle_info(?HTTP_HDR(_),LD) -> LD;
handle_info(?HTTP_EOH(_),LD) -> LD;
handle_info(?HTTP_GET(Socket,Item),LD) ->
  case answer(Item,LD) of
    {reply,Answer} ->
      reply(Answer,Socket,LD);
    close ->
      close(Socket,LD)
  end;

handle_info({'EXIT',Pid,Err},LD=#ld{acceptor=Pid}) ->
  %% the acceptor died. we'll restart and hope for the best.
  ?log({acceptor_died,Err}),
  case Err of
    {listen_error,_} -> {stop,shutdown,LD#ld{acceptor=undefined}};
    _ -> LD#ld{acceptor=accept(self(),tcp_port(),tcp_opts())}
  end;

handle_info({new_socket,Socket},LD) ->
  inet:setopts(Socket,[{active,true}]),
  LD;

handle_info({tcp_closed,Socket},LD) -> 
  close(Socket,LD);

handle_info(Info,LD) ->
  ?log({weird_message,Info}),
  LD.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% implementation
tcp_opts()->
  [binary, 
   {packet, http}, 
   {active, true},
   {reuseaddr, true}].

tcp_port() ->
  8081.

answer("",LD)   -> answer("index.html",LD);
answer(File,LD) ->
  case has_extension(File) of
    true -> serve_file(File,LD);
    false-> {reply,500}
  end.

serve_file(File,LD) -> 
  try {ok,Res} = file:read_file(filename:join(LD#ld.here,File)),
      {reply,Res}
  catch C:R -> 
      ?log([{C,R},{not_found,File}]),
      {reply,404}
  end.

has_extension(File) ->
  case lists:reverse(string:tokens(File,".")) of
    [Ext|_] -> lists:member(Ext,["css","html","js","ico","gif"]);
    _ -> false
  end.

reply(Answer,Socket,LD) ->
  gen_tcp:send(Socket,http_reply(Answer)),
  close(Socket,LD).

close(Socket,LD) ->
  gen_tcp:close(Socket),
  LD.

http_reply(Status) when is_integer(Status) ->
  http_reply(Status,<<>>);
http_reply(Reply) when is_list(Reply) ->
  http_reply(list_to_binary(Reply));
http_reply(Reply) when is_binary(Reply) ->
  http_reply(200,Reply).

http_reply(Status,Reply) ->
  ["HTTP/1.1 ",to_str(Status)," ",phrase(Status),"\r\n",
   "Content-Length: ", to_str(byte_size(Reply)),"\r\n",
   "Content-Type: text/html\r\n"
   "Server: Smarty\r\n",
   "\r\n",
   Reply].

phrase(200) -> "OK";
phrase(404) -> "NotFound";
phrase(500) -> "InternalServerError".

-define(is_string(S),S==[];is_integer(hd(S))).
to_str(S) when ?is_string(S) -> S;
to_str(L) when is_list(L) -> lists:append([to_str(I)||I<-L]);
to_str(P) when is_pid(P) -> pid_to_list(P);
to_str(I) when is_integer(I) -> integer_to_list(I);
to_str(A) when is_atom(A) -> atom_to_list(A).
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
