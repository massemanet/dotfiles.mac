%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  8 Jul 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('trcfile').
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

-import(proplists,
        [get_value/2]).

%-include_lib("eper/src/log.hrl").
-define(log(T),
        '?log'([process_info(self(),current_function)
                , {line,?LINE}]
               ,T)).
-define(log_bt(T),
        '?log'([process_info(self(),current_function)
                , {line,?LINE}
                , {bt,erlang:get_stacktrace()}]
               , T)).

'?log'(HD,T) when not is_integer(hd(T)) -> error_logger:info_report(HD++T);
'?log'(HD,T)  -> '?log'(HD,[T]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% config data

tcp_port() ->
  8080.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start the server
%% trcfile:start().
%%
%% connect from a browser
%% http://sterlett:8080/trcfile.html
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API functions
start(Args) ->
  start(),
  [?MODULE ! A || A <- Args].

start() ->
  gen_serv:start(?MODULE).

state() ->
  {_,Dict} = process_info(whereis(?MODULE),dictionary),
  [I || {{sockets,_},_}=I <- Dict]++
    [I || {K,_}=I <- gen_serv:get_state(?MODULE),lists:member(K,[quiet,here])].

stop() ->
  gen_serv:shutdown(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_serv callbacks

%% gen_serv get_state formatting callbacks
-record(ld,{acceptor, quiet=false,
            encoder=mochijson2:encoder([{handler,json_handler()}]),
            here=filename:dirname(code:which(?MODULE)),
            trc_file="watchdog-2010-01-07.trc"}).

rec_info(ld) -> record_info(fields,ld).

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
  inet:setopts(Socket,[{active,true}]),
  LD;

handle_info({tcp_closed,Socket},LD) -> 
  close(Socket,LD);

handle_info(?HTTP_HDR(_),LD) -> LD;
handle_info(?HTTP_EOH(_),LD) -> LD;
handle_info(?HTTP_GET(Socket,Item),LD) ->
  case answer(Item,LD) of
    {reply,Answer} ->
      reply(Answer,Socket,LD);
    close ->
      close(Socket,LD)
  end;

handle_info(Info,LD) ->
  ?log({weird_message,Info}),
  LD.

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

answer(Item,LD) ->
  case has_extension(Item) of
    true -> {reply,serve_file(Item,LD)};
    false->
      try {reply,bread(LD,Item)}
      catch close -> close
      end
  end.

bread(LD,Item) ->
  [Node,What] = string:tokens(Item,"/"),
  bread(LD,Node,What).

bread(LD,Node,"prf") ->
  case bread_server:get_bread() of
    eof ->
      bread_open(LD),
      bread(LD,Node,"prf");
    {watchdog,Nod,_TS,ticker,[{prfPrc,PrfPrc},{prfSys,PrfSys}]} -> 
      case re:run(atom_to_list(Nod),Node) of
        nomatch -> bread(LD,Node,"prf");
        _       ->
          Reply = [{procs,make_procs(PrfSys,PrfPrc)},{prfSys,PrfSys}],
          (LD#ld.encoder)(structify(Reply))
      end;
    _ -> 
      bread(LD,Node,"prf")
  end;
bread(_,_,What) ->
  ?log({bad_item,What}),
  throw(close).

bread_open(#ld{here=Dir,trc_file=File}) -> 
  bread_server:start(filename:join(Dir,File)).

make_procs(PrfSys,PrfPrc) ->
  Procs = get_value(info,PrfPrc),
  BeamUser = get_value(beam_user,PrfSys),
  Reds = get_value(reductions,PrfSys),
  [make_proc(BeamUser/Reds,[{pid,pid2str(Pid)}|Is]) || {Pid,Is} <- Procs].

pid2str(Pid) -> 
  [_,A,B] = string:tokens(pid_to_list(Pid),"<>."),
  list_to_binary(lists:append(["<0.",A,".",B,">"])).

make_proc(CpR,Is) -> 
  case proplists:get_value(name,Proc = fold(fun(I)->make_i(I,CpR)end,Is)) of
    undefined -> [{name,proplists:get_value(initial_call,Is)}|Proc];
    _ -> Proc
  end.

make_i({dreductions,DR},CpR)    -> {cpu,round(100*CpR*DR)};
make_i({dmemory,DM},_)          -> {dmemory,round(DM)};
make_i({memory,M},_)            -> {memory,round(M/1024)};  % kBytes
make_i({initial_call,_},_)      -> throw(drop);
make_i({reductions,_},_)        -> throw(drop);
make_i({registered_name,""},_)  -> throw(drop);
make_i({registered_name,RN},_)  -> {name,RN};
make_i({message_queue_len,QL},_)-> {msgs,QL};
make_i(KV,_) -> KV.

fold(_,[]) -> [];
fold(F,[I|Is]) -> 
  try [F(I)|fold(F,Is)]
  catch drop -> fold(F,Is)
  end.

serve_file(File,LD) -> 
  try {ok,Res} = file:read_file(filename:join(LD#ld.here,File)),
      Res
  catch C:R -> 
      ?log([{C,R},{not_found,File}]),
      404
  end.

has_extension(File) ->
  case lists:reverse(string:tokens(File,".")) of
    [Ext|_] -> lists:member(Ext,["css","html","js","ico","gif"]);
    _ -> false
  end.


tcp_opts()->
  [binary, 
   {packet, http}, 
   {active, true},
   {reuseaddr, true}].

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
phrase(404) -> "NotFound".

-define(are_ints(T1,T2,T3),is_integer(T1),is_integer(T2),is_integer(T3)).
-define(is_mfa(M,F,A),is_atom(M),is_atom(F),is_integer(A)).
-define(is_string(S),S==[];is_integer(hd(S))).
-define(is_pl(PL), tuple_size(hd(PL))=:=2).
json_handler() -> 
  fun({T1,T2,T3}) when ?are_ints(T1,T2,T3) -> T1*1000000000+T2*1000+T3/1000;
     ({M,F,A}) when ?is_mfa(M,F,A) -> to_bin([M,":",F,"/",A]);
     ({dets,Table}) when is_atom(Table) -> to_bin([dets," - ",Table]);
     ({dets,{Table,index,N}}) -> to_bin([dets," - ",Table,"(",N,")"]);
     (P) when is_pid(P) -> to_bin(P)
  end.

structify(PL)when ?is_pl(PL) -> {struct,[{K,structify(V)}||{K,V}<-PL]};
structify(S) when ?is_string(S) -> S;
structify(L) when is_list(L) -> [structify(E)||E<-L];
structify(X) -> X.

to_bin(X) -> list_to_binary(to_str(X)).

to_str(S) when ?is_string(S) -> S;
to_str(L) when is_list(L) -> lists:append([to_str(I)||I<-L]);
to_str(P) when is_pid(P) -> pid_to_list(P);
to_str(I) when is_integer(I) -> integer_to_list(I);
to_str(A) when is_atom(A) -> atom_to_list(A).
