%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 12 Jan 2011 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('foo').
-author('mats cronqvist').
-export([sum_a_b/0
         , diff_a_c/0]).

-record(a,{key,val,field_bs}).
-record(b,{key,val,field_a}).
-record(c,{key,val}).

diff_a_c() ->
  {fun(A,C) ->
       A#a.val - C#c.val
   end,
  [{'A',a_table},
   {'C',c_table,ckey}]}.

sum_a_b()->
  {fun(A,Bs) ->
       A#a.val + [2*B#b.val || B<-Bs]
   end,
   [[{'A',a_table},
     {'Bs',b_table,['A',#a.field_bs]}],
    [{'B',b_table},
     {'A',a_table,['B',#b.field_a]},
     {'Bs',b_table,['A',#a.field_bs]}]]}.

%
go() ->
  receive
    a   -> a;
    aaa -> lists:sum(
  end.

