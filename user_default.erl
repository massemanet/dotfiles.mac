%% -*- erlang-indent-level: 2 -*-
%%% Created :  7 Apr 2008 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('user_default').
-author('Mats Cronqvist').

-export([ineti/0,
         sig/1,sig/2,sig/3,
         print_source/1,
         ports/0,
         export_all/1,
         tab/0,
         long/1,flat/1,dump/1,
         e/2,
         kill/1,
         pi/1,pi/2,
         os/1,
         callstack/1,
         bt/1,
         pid/1,
         lm/0]).

%% recompiles M with export_all without access to the source.
export_all(M) ->
  case code:which(M) of
    non_existing -> no_such_module;
    F ->
      {ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(F,[abstract_code]),
      {ok,_,B} = compile:forms(AC,[export_all]),
      code:soft_purge(M),
      code:load_binary(M,F,B)
  end.

lm() ->
  MD5File =
    fun(F) ->
        case F of
          preloaded -> dont_load;
          _ ->
            case beam_lib:md5(F) of
              {ok,{_,MD5}} -> MD5;
              _ -> dont_load
            end
        end
    end,

  MD5Loaded =
    fun(M) ->
        case M:module_info(compile) of
          [] -> dont_load;
          _  -> M:module_info(md5)
        end
    end,

  Loadp =
    fun(M,F) ->
        Loaded = MD5Loaded(M),
        File = MD5File(F),
        Loaded =/= dont_load andalso
          File =/= dont_load andalso
          Loaded =/= File
    end,

  Load =
    fun(M,"") ->
        {cannot_load,M};
       (M,F) ->
        code:purge(M),
        {module,M} = code:load_abs(filename:rootname(F,".beam")),
        M
    end,

  [Load(M,F) || {M,F} <- code:all_loaded(), Loadp(M,F)].

tab() ->
  N=node(),
  io:setopts([{expand_fun,fun(B)->rpc:call(N,edlin_expand,expand,[B]) end}]).

print_source(Mod) ->
  {ok,{_,[{"Abst",AChunk}]}} = beam_lib:chunks(code:which(Mod),["Abst"]),
  {_,Forms} = binary_to_term(AChunk),
  io:fwrite("~s~n",[erl_prettypr:format(erl_syntax:form_list(Forms))]).

dump(Term)->
  File = filename:join([os:getenv("HOME"),"erlang.dump"]),
  {ok,FD}=file:open(File,[write]),
  try wr(FD,"~p.~n",Term),File
  after file:close(FD)
  end.

flat(Term) -> flat("~p~n",[Term]).
flat(Form,List) -> wr("~s",io_lib:format(Form,List)).

long(X) -> wr(X).

e(N,T) when is_list(T) -> lists:nth(N,T);
e(N,T) when is_tuple(T) -> element(N,T).

kill(P) -> exit(pid(P),kill).

pi(P) -> process_info(pid(P)).
pi(P,Item) -> process_info(pid(P),Item).

os(Cmd) ->
  lists:foreach(fun(X)->wr("~s~n",X)end,string:tokens(os:cmd(Cmd),"\n")).

sig(M) ->
  sig(M,'').
sig(M,F) ->
  sig(M,F,'').
sig(M,F,A) ->
  case code:get_object_code(M) of
    error -> error;
    {M,Beam,_FN} ->
      {ok,{M,[{"Abst",Chunk}]}} = beam_lib:chunks(Beam,["Abst"]),
      {_,Abst} = binary_to_term(Chunk),
      Exports = lists:append([FAs || {attribute,_,export,FAs} <- Abst]),
      Arg = fun(AA) -> string:join([erl_pp:expr(A1) || A1 <- AA],",") end,
      Grd = fun(GG) -> erl_pp:guard(GG) end,
      P = fun(M0,F0,A0,G) -> flat("~w:~w(~s) ~s~n",[M0,F0,Arg(A0),Grd(G)]) end,
      length([[P(M,Fn,AA,GG) || {clause,_,AA,GG,_} <- As]
              || {function,_,Fn,Ar,As} <- Abst,
                 A == '' orelse A == Ar,
                 F == '' orelse F == Fn,
                 lists:member({Fn,Ar},Exports)])
  end.

wr(E) -> wr("~p.~n",E).
wr(F,E) -> wr(user,F,E).
wr(FD,F,E) -> io:fwrite(FD,F,[E]).

callstack(P) ->
  [string:strip(e(2,string:tokens(L,"(+)"))) || L<- bt(P), $0 =:= hd(L)].
bt(P) ->
  string:tokens(binary_to_list(e(2,process_info(pid(P),backtrace))),"\n").

pid(Pid) when is_list(Pid) -> list_to_pid(Pid);
pid(Pid) when is_pid(Pid) -> Pid;
pid(Atom) when is_atom(Atom) -> whereis(Atom);
pid({0,I2,I3}) when is_integer(I2) -> c:pid(0,I2,I3);
pid(I2) when is_integer(I2) -> pid({0,I2,0}).

ineti() ->
  lists:foreach(fun ineti/1,ports()).

ineti(P) ->
  {_Fam,Type} = proplists:get_value(type,P),
  [Status|_]  = proplists:get_value(status,P),
  {LIP,LPort} = proplists:get_value(local,P),
  Sent        = proplists:get_value(sent,P),
  Recvd       = proplists:get_value(received,P),
  {RIP,RPort} =
    case proplists:get_value(remote,P) of
      enotconn -> {"*","*"};
      {Rip,Rp} -> {inet_parse:ntoa(Rip),integer_to_list(Rp)}
    end,
  io:fwrite("~15s:~-5w ~15s:~-5s ~7w ~9w ~w/~w~n",
            [inet_parse:ntoa(LIP),LPort,RIP,RPort,Type,Status,Sent,Recvd]).

ports() ->
  [port_info(P)++PI ||
    {P,PI}<-[{P,erlang:port_info(P)}||P<-erlang:ports()],
    lists:sublist(proplists:get_value(name,PI),4,100)=="_inet"].

port_info(P) ->
  {ok,Type} = prim_inet:gettype(P),
  {ok,Status} = prim_inet:getstatus(P),
  {ok,[{_,Sent}]} = prim_inet:getstat(P,[send_oct]),
  {ok,[{_,Recvd}]} = prim_inet:getstat(P,[recv_oct]),
  {ok,Local} = prim_inet:sockname(P),
  Remote = case prim_inet:peername(P) of
             {ok,R} -> R;
             {error,R} -> R
           end,
  [{type,Type},{status,Status},
   {sent,Sent},{received,Recvd},
   {local,Local},{remote,Remote}].
