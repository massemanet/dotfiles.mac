%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 20 Apr 2010 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('btree_bench').
-author('Mats Cronqvist').
-export([go/1
         , dets/1]).

go(What) ->
  Tobbe = lists:seq(1,1000),
  catch btree:destroy(foo),
  btree:new(foo),
  L = mlist(What),
  Start = now(),
  [btree:insert(foo,E,{Tobbe,E}) || E <- L],
  Mid = now(),
  [{Tobbe,E}=btree:lookup(foo,E) || E <- L],
  Stop = now(),
  {tdiff(Stop,Mid)/length(L),tdiff(Mid,Start)/length(L)}.

dets(What) ->
  Tobbe = lists:seq(1,1000),
  File = "/home/masse/foo.dets",
  file:delete(File),
  dets:open_file(foo,[{type,set},{file,File}]),
  L = mlist(What),
  Start = now(),
  [dets:insert(foo,[{E,{Tobbe,E}}]) || E <- L],
  Mid = now(),
  [[{E,{Tobbe,E}}] = dets:lookup(foo,E) || E <- L],
  Stop = now(),
  {tdiff(Stop,Mid)/length(L),tdiff(Mid,Start)/length(L)}.

mlist({rseq,N}) ->
  lists:reverse(mlist({seq,N}));
mlist({seq,N}) -> 
  lists:seq(1,N);
mlist({rand,N}) ->
  [I||{_,I}<-lists:sort([{random:uniform(),I}||I<-lists:seq(1,N)])].

tdiff(B,S) ->
  timer:now_diff(B,S)/1000000.
