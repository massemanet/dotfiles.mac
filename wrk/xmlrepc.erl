%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 28 May 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module(xmlrepc).
-author('Mats Cronqvist').
-export([start/0
         , start/1
         , start/2
         , start/3
         , stop/0]).

-export([init/1
         , rec_info/1
         , handle_info/2]).

-ignore_xref([start/0
	      , start/1
	      , start/2
	      , send/1]).	      

%% smarty_server!{pcache_dir,"/mnt/kred_bups/pcache"}.

stop() ->
  gen_serv:stop(?MODULE).

start() ->
  start(all).

start(Count) ->
  start(Count,"/mnt/kred_logs/searchable/logs.*/*/xmlrpc.log").

start(Count,FilePattern) ->
  start(Count,FilePattern,8).

start(Count,FilePattern,RobotCount) ->
  start(Count,FilePattern,RobotCount,"http://lax:4567").

start(Count,FilePattern,RobotCount,KredURL) ->
  gen_serv:start(?MODULE,make_ld(Count,FilePattern,RobotCount,KredURL)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_serv callbacks
%% `cum'; the total number of blobs for this server instance
%% `count'; the number of blobs left in this run (counts down to 0)
-record(ld,{count=0,cum=0,acc=[],robots=[],
            robot_maker,robot_count,file_pattern}).
rec_info(ld) -> record_info(fields,ld).

init(LD = #ld{file_pattern=FP}) ->
  bread_server:start(FP,xml),
  maybe_start_robots(LD).

handle_info({'DOWN',BR,_,BP,Result},LD) ->
  handle_down(Result,update_robots(BR,BP,Result,LD));
handle_info({count,Count},LD) ->
  handle_down(next,LD#ld{count=Count}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% implementation
make_ld(Count,FilePattern,RobotCount,KredURL) ->
  #ld{count=Count
      ,file_pattern=FilePattern
      ,robot_count=RobotCount
      ,robot_maker=robot_maker(KredURL)}.

handle_down(Result,LD) ->
  case is_finished(LD) of
    true ->  LD;
    false -> handle_result(Result,maybe_start_robots(LD))
  end.

is_finished(LD) -> LD#ld.count =< 0.

maybe_start_robots(LD = #ld{robot_maker=RM,robots=Robots}) ->
  case LD#ld.robot_count-length(Robots) of
    X when 0<X -> LD#ld{robots=Robots++[RM() || _ <- lists:seq(1,X)]};
    _          -> LD
  end.

handle_result(Result,LD) ->
  case Result of
    {ok,Res}                        -> update_ld(Res,LD);
    next                            -> LD;
    What                            -> log(LD,What),LD
  end.

update_ld(Res,LD=#ld{acc=Acc,count=Count,cum=Cum}) ->
  LD#ld{acc=key_upd(what_fault(Res),Acc),count=decr(Count),cum=Cum+1}.

decr(all) -> all;
decr(I)   -> I-1.

update_robots(BR,BP,Result,LD=#ld{robots=Robots}) ->
  case Robots--[{BP,BR}] of
    Robots -> exit({got_weird_DOWN,BP,Result});
    NRobots-> LD#ld{robots=NRobots}
  end.

robot_maker(KredURL) ->
  fun() -> spawn_monitor(fun() -> robot_f(KredURL) end) end.

what_fault({fault,_,Fault}) -> Fault;
what_fault([_])             -> ok.

key_upd(Key, O) ->
  case proplists:get_value(Key,O,"") of
    "" -> [{Key,1}|O];
    Vl -> lists:keyreplace(Key,1,O,{Key,Vl+1})
  end.
      
log(LD,What) ->
  io:fwrite("error at ~p - ~p~n",[LD#ld.cum,What]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the robots... runs in their own procs. always exits. supposed to be
%% handled by monitoring.
robot_f(KredURL) ->
  RawXML = bread_server:get_bread(),
  try xmlrpc_decode:payload(binary_to_list(RawXML)) of
    {ok,{call,add_invoice,[{array,As}]}} -> 
      Eid = 2,
      {ok,Goodss} = estore_server:convert_goods(lists:nth(7,As)),
      Gs = string:join([element(3,G)|| {G,_}<-Goodss],":")++":",
      Digest = estore_verify:mk_digest_secret(secret(Eid),Gs),
      NewAs = tweak_as(As,[{3,Eid},{5,Digest}]),
      {ok,XML} = xmlrpc_encode:payload({call,add_invoice,[{array,NewAs}]}),
      exit(send(lists:flatten(XML),KredURL));
    {ok,_} -> 
      exit(next);
    {error,{fatal,{error_scanning_entity_ref,{file,_},{line,L},{col,C}}}} -> 
      exit({bad_entity,bad_entity(RawXML,L,C,30)});
    {error,R} ->
      exit({xmlrpc_decode_error,R,RawXML})
  catch 
    error:{badmatch,{error,"goods_list"}} ->
      exit(next)
  end.


bad_entity(X,L,C,CtxtSize) ->
  P = lists:max([0,C-(CtxtSize div 2)]),
  lists:sublist(binary_to_list(lists:nth(L,re:split(X,"\\R"))),P,CtxtSize).

secret(2) -> "dr.alban".

tweak_as(As,IVs) ->
  tweak_as(As,IVs,1).

tweak_as([A|As],IVs,I) ->
  [proplists:get_value(I,IVs,A)|tweak_as(As,IVs,I+1)];
tweak_as([],_,_) ->
  [].

send(Body,Url) ->
  case http_request(Body,Url) of
    {error,R} -> {http_error,R};
    {ok,{{"HTTP/1.1",200,"OK"}, _Headers,RespBody}} -> 
      case xmlrpc_decode:payload(RespBody) of
        {ok,{response,Resp}} -> {ok,Resp};
        {error,R}            -> {xmlrpc_error,R}
      end
  end.

http_request(Body,Url) ->
  http:request(post,
               {Url,[{"user-agent","inets"}],"text/xml",Body},
               [],
               []).
