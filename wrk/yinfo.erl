%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 20 Aug 2010 by masse <masse@kreditor.se>

%% @doc
%% @end

-module('yinfo').
-author('masse').

-export([get_yaws_info/0
         , get_yaws_info/1]).

-include_lib("yaws/include/yaws_api.hrl").
-include_lib("site/include/site_state.hrl").

get_yaws_info() ->
    get_yaws_info(0).

get_yaws_info(Sleep) when is_integer(Sleep) ->
  case erlang:system_info(process_count) < 1000 of
    false-> [];
    true -> 
      Ps = get_procs(Sleep),
      [I || I <- [{P,get_yaws_info(P)} || P <- Ps], element(2,I) =/= []]
  end;

get_yaws_info(Pid) when is_pid(Pid) ->
  case is_safe(Pid) of
    {ok,Links} -> lists:flatten([yaws_info(Pid)|[yaws_info(P) || P <- Links]]);
    nok        -> []
  end.

yaws_info(Pid)->
  try
    YA = proplists:get_value(yaws_arg,pi(Pid,dictionary)),
    case (YA#arg.req)#http_request.method of
      'POST'-> {Pid,yaws_path(YA),yaws_xmlrpc(YA)};
      'GET' -> {Pid,yaws_path(YA),yaws_user(YA)};
      {undefined,_} -> io:fwrite("yaws_arg ~p~n",[YA]),[]
    end
  catch
    error:{badrecord,_} -> [];
    error:badarg        -> [];
    C:R -> io:fwrite("crash yaws_info/1 ~p~n",[{C,R}]),[]
  end.

get_procs(Sleep) ->
  P0 = [{P,pi(P,reductions)} || P <- processes()],
  case Sleep of
    0 -> [P || {P,_} <- P0];
    _ -> 
      timer:sleep(Sleep),
      Pr = [{P,proplists:get_value(P,P0),pi(P,reductions)} || P<-processes()],
      [P || {P,R1,R2} <- Pr,is_integer(R1),is_integer(R2),R1=/=R2]
  end.

yaws_xmlrpc(#arg{clidata = D}) ->
  try 
    {ok,{call,Method,Args}} = xmlrpc_decode:payload(binary_to_list(D)),
    {Method,Args}
  catch
    _:_ -> not_xml
  end.

yaws_path(#arg{req = #http_request{path = {abs_path,Path}}}) -> 
  Path.

yaws_user(#arg{headers=#headers{cookie=C}}) ->
  try
    CV = yaws_api:find_cookie_val("kreditor",C),
    {ok,SS} = yaws_api:cookieval_to_opaque(CV),
    SS#site_state.username
  catch
    _:_ -> unknown_user
  end.

pi(Pid,Item) ->
  case process_info(Pid,Item) of
    {Item,V} -> V;
    _ -> []
  end.

is_safe(Pid) ->
  case length(Links = pi(Pid,links)) < 30 andalso
    proc_lib:translate_initial_call(Pid) =/= {yaws_server,gserv,3} of
    true -> {ok,Links};
    false-> nok
  end.
