%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 20 Jul 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('gsu').
-author('Mats Cronqvist').
-export([start/0,stop/0,state/0]).
-export([rec_info/1,init/1,handle_info/2]).
-include_lib("eper/src/log.hrl").

start() ->
  gen_serv:start(?MODULE).

state() ->
  gen_serv:get_state(?MODULE).

stop() ->
  gen_serv:stop(?MODULE).

-record(ld,{count=0,date=[]}).
rec_info(ld) -> record_info(fields,ld).

upgrade({ld,_,Count,[]}) -> #ld{count=Count,date=date()}.

%% gen_serv init callback
init(_) ->
  ?log({starting,now()}),
  gen_serv:unlink(),
  #ld{}.

%% gen_serv handle_info callback

handle_info(Msg,OLD) when not is_record(OLD,ld) ->
  handle_info(Msg,upgrade(OLD));

handle_info(_,LD) -> 
  LD#ld{count=LD#ld.count+1}.
