%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 12 Aug 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('pd_de_acc').
-author('Mats Cronqvist').
-export([go/0
        , check_all/0]).

-include_lib("kernel/include/file.hrl").

go() ->
  {ok,[Bads]} = file:consult("/home/masse/bads.txt"),
  length(go_bad(Bads)).

go_bad([]) -> [];
go_bad([Bad|Bads]) ->
  try [go_bad1(Bad)|go_bad(Bads)]
  catch
    {drop,quiet} -> go_bad(Bads);
    {drop,R} -> dropped(Bad,R), go_bad(Bads);
    {stop,R} -> exit({stopping,R,fetch(pno,Bad)})
  end.

dropped(Bad,R) -> io:fwrite("dropped - ~p~n~p~n",[R,fetch(real,Bad)]).

is_main(Bad) ->
  {"real",F,L,S,Z} = fetch(real,Bad),
  [F0,L0,S0,Z0] = main_contact(fetch(pno,Bad)),
  is_all_eq([{F,F0},{L,L0},{S,S0},{Z,Z0}]).

is_all_eq([]) -> true;
is_all_eq([{A,B}|ABs])  ->
  case pref(A,B) of
    true -> is_all_eq(ABs);
    false-> false
  end.

main_contact(Pno) -> 
  contact:get_fields(contact:main_delivery_contact(pno2person(Pno)),
                     [first_name,last_name,street,zip]).

go_bad1(Bad) -> 
  File = fetch(file,Bad),
  MtimeInfo = (file_info(File++"_info"))#file_info.mtime,
  MtimeRisk = (file_info(File))#file_info.mtime,
  case MtimeInfo =< MtimeRisk of
    true -> ok;
    false -> throw({drop,quiet})
  end,
  NewMtimeInfo = lists:max([now_minus_89d(),plus_2s(MtimeRisk)]),
  io:fwrite("setting time to ~p~n~p, ~p, ~p~n",[NewMtimeInfo,
                                                tdiff(NewMtimeInfo,MtimeRisk),
                                                tdiff(NewMtimeInfo,MtimeInfo),
                                                tdiff(now(),NewMtimeInfo)]),
  file:write_file_info(File++"_info",#file_info{mtime=NewMtimeInfo}),
  re_cache(fetch(pno,Bad)),
  case is_main(Bad) of
    true -> io:fwrite("fixed - ~p~n",[fetch(real,Bad)]);
    false-> throw({drop,{failed,fetch(pno,Bad)}})
  end.

plus_2s(DateTime) ->
  calendar:gregorian_seconds_to_datetime(
    calendar:datetime_to_gregorian_seconds(DateTime)
    +2).

now_minus_89d() -> 
  calendar:gregorian_seconds_to_datetime(
    calendar:datetime_to_gregorian_seconds(
      calendar:now_to_local_time(now()))
    -89*24*3600).

tdiff({_,_,_}=Now,{_,_}=DT) ->
  tdiff(calendar:now_to_local_time(Now),DT);
tdiff({_,_}=DT1,{_,_}=DT2) ->
  calendar:time_difference(DT2,DT1).

file_info(F) -> {ok,FI} = file:read_file_info(F), FI.

pno2person(Pno) -> person:read_d(pno:pno(Pno)).

re_cache(Pno) -> kula:get_delivery_addresses(4,pno2person(Pno)).

fetch(K,PL) -> proplists:get_value(K,PL).

pref(A,A) -> true;
pref(A,B) when length(A) < length(B) -> pref(B,A);
pref(A,B) -> lists:prefix(string:to_lower(B),string:to_lower(A)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_all() ->
  dets:foldl(fun check_all/2,[],person).

check_all(_,Acc) when 10<length(Acc)-> exit({Acc});
check_all(O,Acc) -> 
  Pno = element(2,O),
  case contact:all_delivery_contacts_d(Pno,4) of
    [] -> Acc;
    [_] -> Acc;
    [C|Cs] = X -> 
      case is_same_fn(get_fn(C),Cs) of
        true -> Acc;
        false-> [X|Acc]
      end
  end.

is_same_fn(_,[]) -> true;
is_same_fn(FN,[C|Cs]) ->
  case same_fn(FN,get_fn(C)) of
    true -> is_same_fn(FN,Cs);
    false-> false
  end.

get_fn({contact,_,I,_}) -> 
  try string:to_lower(fetch(first_name,I))
  catch _:_ -> undefined
  end.

same_fn(A,A) -> true;
same_fn(A,B) -> 
  same_pref(A,B) orelse same_els(string:tokens(A," -"),string:tokens(B," -")).

same_pref(A,B) when length(A) < length(B) -> same_pref(B,A);
same_pref(A,B) -> lists:prefix(B,A).

same_els(As,Bs) when length(As) < length(Bs) -> same_els(Bs,As);
same_els(As,[B]) -> lists:member(B,As);
same_els(As,Bs) -> [B||B<-Bs,lists:member(B,As)] =:= Bs.
