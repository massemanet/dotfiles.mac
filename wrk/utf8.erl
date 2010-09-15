%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  4 Jun 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('utf8').
-author('Mats Cronqvist').
-export([list/0
        , list/1
        , to_latin1/1]).

list() ->  list([0,16#C2,16#C3]).

list(C1) when is_integer(C1)-> list([C1]);
list(C1s) ->
  [try write(C1,C2,unicode_from_utf8(C1,C2))
   catch _:_-> x 
   end || C1<-C1s, C2<-lists:seq(0,16#FF)],
  ok.

unicode_from_utf8(C1,C2) ->
  case C1 of 
    0 -> xmerl_ucs:from_utf8([C2]);
    _ -> xmerl_ucs:from_utf8([C1,C2])
  end.

write(C1,C2,UNI) -> 
  io:fwrite("~2.16.0B~2.16.0B o~.8B ~w x~.16B - ~p~n",
            [C1,C2,hd(UNI),hd(UNI),hd(UNI),UNI]).

to_latin1([]) -> [];
to_latin1([16#C3,16#80|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#81|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#82|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#83|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#84|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#85|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#86|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#87|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#88|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#89|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#8A|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#8B|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#8C|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#8D|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#8E|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#8F|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#90|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#91|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#92|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#93|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#94|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#95|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#96|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#97|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#98|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#99|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#9A|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#9B|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#9C|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#9D|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#9E|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#9F|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#A0|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#A1|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#A2|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#A3|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#A4|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#A5|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#A6|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#A7|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#A8|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#A9|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#AA|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#AB|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#AC|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#AD|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#AE|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#AF|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#B0|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#B1|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#B2|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#B3|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#B4|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#B5|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#B6|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#B7|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#B8|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#B9|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#BA|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#BB|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#BC|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#BD|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#BE|R]) -> "�"++to_latin1(R);
to_latin1([16#C3,16#BF|R]) -> "�"++to_latin1(R);
to_latin1([H|T]) -> [H|to_latin1(T)].
