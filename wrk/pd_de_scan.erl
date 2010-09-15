%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 27 Aug 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('pd_de_scan').
-author('Mats Cronqvist').
-export([go/0,go/1]).

go() -> go("*/*/*/DK*.debitor").

go(Wild) ->
  pcache_fold:files(Wild,fun scanner/2,[]).

scanner({{_,_,_Pno},{error,_}},Acc)->Acc;
scanner({{_,_,Pno},{[A|_],_}},Acc)-> 
  try 
    R=short_address(A),
    Q=short_contact(Pno),
    case {R,Q} of 
      {_,[]}-> Acc;
      {R,R}->Acc;
      {[AF,AL|_],[AF,AL|_]}->Acc;
      X->[{Pno,X}|Acc]
    end 
  catch _:Rs->exit({Rs,Pno,A}) 
  end.

short_address(A) ->
  {_,[F,L,S,Z]} = pd_lib:extract(A,[first_name,last_name,street,zip],""), 
  (downcase([first_bit(F),L,first_bit(S),Z])).

first_bit(Str) -> 
  case string:tokens(Str," ") of 
    []    ->[];
    [H|_] ->H
  end.

downcase(Strs) -> [string:to_lower(Str) || Str <- Strs].

short_contact(Pno) ->  
  try
    [F,L,S,Z] = contact:get_fields(contact:main_delivery_contact(
                                     person:read_d(pno:pno(Pno))),
                                   [first_name,last_name,street,zip]),
    downcase([first_bit(F),L,first_bit(S),Z])
  catch _:_ -> []
  end.
