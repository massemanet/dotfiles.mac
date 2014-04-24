%% -*- erlang-indent-level: 2 -*-
%%% Created :  7 Apr 2008 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('user_default').
-author('Mats Cronqvist').
-export([export_all/1
         ,tab/0
         ,long/1,flat/1,dump/1
         ,e/2
         ,inet/0
         ,kill/1
         ,pi/1,pi/2
         ,os/1
         ,bt/1
         ,pid/1
         ,lm/0]).

%% recompiles M with export_all without access to the source.
export_all(M) ->
  case code:which(M) of
    non_existing -> no_such_module;
    F ->
      {ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(F,[abstract_code]),
      {ok,_,B} = compile:forms(AC,[export_all]),
      code:soft_purge(M),
      code:load_binary(M,"",B)
  end.

lm() ->
  MD5File =
    fun(F) ->
        case beam_lib:md5(F) of
          {ok,{_,<<I:128>>}} -> [I];
          _ -> undefined
        end
    end,

  MD5Loaded =
    fun(M) ->
        proplists:get_value(vsn,M:module_info(attributes))
    end,

  Load =
    fun(M,F) ->
        true = code:soft_purge(M),
        code:load_abs(filename:rootname(F,".beam"))
    end,

  [Load(M,F) || {M,F} <- code:all_loaded(), MD5Loaded(M) =/= MD5File(F)].

tab() ->
  N=node(),
  io:setopts([{expand_fun,fun(B)->rpc:call(N,edlin_expand,expand,[B]) end}]).

dump(Term)->
  {ok,FD}=file:open(filename:join([os:getenv("HOME"),"erlang.dump"]),[write]),
  try wr(FD,"~p.~n",Term)
  after file:close(FD)
  end.

flat(L) -> wr("~s~n",lists:flatten(L)).

long(X) -> wr(X).

e(N,T) when is_list(T) -> lists:nth(N,T);
e(N,T) when is_tuple(T) -> element(N,T).

kill(P) -> exit(pid(P),kill).

pi(P) -> process_info(pid(P)).
pi(P,Item) -> process_info(pid(P),Item).

os(Cmd) ->
  lists:foreach(fun(X)->wr("~s~n",X)end,string:tokens(os:cmd(Cmd),"\n")).

wr(E) -> wr("~p.~n",E).
wr(F,E) -> wr(user,F,E).
wr(FD,F,E) -> io:fwrite(FD,F,[E]).

bt(P) ->
  string:tokens(binary_to_list(e(2,process_info(pid(P),backtrace))),"\n").

pid(Pid) when is_list(Pid) -> list_to_pid(Pid);
pid(Pid) when is_pid(Pid) -> Pid;
pid(Atom) when is_atom(Atom) -> whereis(Atom);
pid({0,I2,I3}) when is_integer(I2) -> c:pid(0,I2,I3);
pid(I2) when is_integer(I2) -> pid({0,I2,0}).

inet() ->
  [element(2,prim_inet:sockname(P)) ||
    P <- erlang:ports(),
    erlang:port_info(P,name) == {name,"tcp_inet"}].
