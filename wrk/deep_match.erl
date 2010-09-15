%% -*- erlang-indent-level: 2 -*-
%%% Created :  3 Nov 2008 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('deep_match').
-author('Mats Cronqvist').
-export([dm/1]).

matches() ->[{[],			fun version/1},
	     {["version"],		fun version/1},
	     {["register"],		fun register/1},
	     {["nodes"],		fun get_nodes/1},
	     {["nodes", ''],		fun nodez/1},
	     {["nodes", '', "queues"],	fun queues/1},
	     {["nodes", '', "messages"],fun messages/1}
	    ].

dm(Str) -> until(string:tokens(Str,"/"),matches()).
  
until(Toks,[]) -> exit({no_match,Toks});
until(Toks,[{CandToks,CandFun}|Cands]) ->
  try CandFun(deep_match(Toks,CandToks))
  catch _:_ -> until(Toks,Cands)
  end.

deep_match([],[]) -> [];
deep_match([T|Toks],[T|CandToks]) -> deep_match(Toks,CandToks);
deep_match([T|Toks],[T|CandToks]) -> deep_match(Toks,CandToks);
deep_match([T|Toks],[''|CandToks]) -> [T|deep_match(Toks,CandToks)].

version([])   -> version.
register([])  -> register.
get_nodes([]) -> get_nodes.
nodez(X)      -> {nodes,X}.
queues(X)     -> {queues,X}.
messages(X)   -> {messages,X}.
