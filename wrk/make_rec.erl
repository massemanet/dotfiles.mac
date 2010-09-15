%% -*- erlang-indent-level: 2 -*-
%%% Created : 19 Nov 2008 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('make_rec').
-author('Mats Cronqvist').
-export([maker/1]).

maker(Fields) ->
  Expr = {'fun',1,{clauses,clauses(Fields,[throw_clause('wot?')],2)}},
  {value,Fun,[]} = erl_eval:expr(Expr,[],none,none),
  Fun.

throw_clause(R) ->
  {clause,1,[{var,1,'_'}],[],[{call,1,
			       {remote,1,{atom,1,erlang},
				{atom,1,throw}},
			       [{atom,1,R}]}]}.

clauses([F|Fields],Tail,N) -> clauses(Fields,[c1(F,N),c2(N,F)|Tail],N+1);
clauses([],Tail,_) -> io:fwrite("~p~n",[Tail]),Tail.

c2(N,Field) ->
  {clause,1,[{integer,1,N}],[],[{atom,1,Field}]}.
c1(Field,N) ->
  {clause,1,[{atom,1,Field}],[],[{integer,1,N}]}.
