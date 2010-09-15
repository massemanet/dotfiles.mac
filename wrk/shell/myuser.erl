%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 23 Nov 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% replace the normal shell with ?MODULE:shell/0
%% use thusly;
%% erl -user myuser -pa ~/wrk/shell/
%% @end

-module('myuser').
-author('Mats Cronqvist').
-export([start/0
         ,shell/0]).

start() ->
  user_drv:start('tty_sl -c -e',{?MODULE,shell,[]}).

shell() ->
  shell:start(false,true).
