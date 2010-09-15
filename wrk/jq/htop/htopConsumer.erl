%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 14 Jul 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('htopConsumer').
-author('Mats Cronqvist').

%% prf callbacks
-export([collectors/0
        , init/1
        , terminate/1
        , config/2
        , tick/2]).

-include_lib("eper/src/log.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% prf callbacks
%% prf:start(logger,node(),loggerConsumer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%collectors() -> [prfPrc,prfSys].
collectors() -> [prfSys,prfPrc].

-record(ld, {node=[],client=[]}).
init(Node) ->
  #ld{node=Node}.

terminate(_LD) -> ok.

config(LD,{client,Client}) -> LD#ld{client=Client};
config(LD,_Data) -> ?log({loopdata,LD}), LD.

tick(LD,In) ->
  case LD#ld.client of
    [] -> ok;
    CC -> CC ! In
  end,
  LD.

