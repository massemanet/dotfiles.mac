%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 27 Jun 2012 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('tzt').
-author('mats cronqvist').
-export([ go/0
         ,do/3]).

%% starts a server that serves localhost:8989/tzt/do[?/]*
%% logs errors to /tmp/tzt/errors.log
go() ->
  inets:stop(),
  inets:start(),
  Root = "/tmp/tzt",
  inets:start(httpd, [{port, 8989},
                      {server_name,"tzt"},
                      {server_root,ensure(Root++"/")},
                      {document_root,ensure(Root++"/")},
                      {modules, [mod_esi,mod_log]},
                      {error_log,ensure(filename:join(Root,"errors.log"))},
                      {erl_script_alias, {"", [tzt]}},
                      {erl_script_nocache,true}]).

%% called when the server sees /tzt/do[/?]*
%% we can deliver the content in chunks
do(SessionID,Env,Input) ->
  mod_esi:deliver(SessionID,
                  ["Content-Type: text/html\r\n\r\n", 
                   "<html><title>I am ",
                   flat(node()),
                   "</title><body>",
                   "Input:<tt>",
                   flat(Input),
                   "</tt>"]),
  mod_esi:deliver(SessionID,
                  ["<br>Env:",
                   flat(Env),
                   "</body></html>"]).

flat(X) ->
  lists:flatten(io_lib:fwrite("~p",[X])).

ensure(X) ->
  filelib:ensure_dir(X),
  X.

