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
to_latin1([16#C3,16#80|R]) -> "À"++to_latin1(R);
to_latin1([16#C3,16#81|R]) -> "Á"++to_latin1(R);
to_latin1([16#C3,16#82|R]) -> "Â"++to_latin1(R);
to_latin1([16#C3,16#83|R]) -> "Ã"++to_latin1(R);
to_latin1([16#C3,16#84|R]) -> "Ä"++to_latin1(R);
to_latin1([16#C3,16#85|R]) -> "Å"++to_latin1(R);
to_latin1([16#C3,16#86|R]) -> "Æ"++to_latin1(R);
to_latin1([16#C3,16#87|R]) -> "Ç"++to_latin1(R);
to_latin1([16#C3,16#88|R]) -> "È"++to_latin1(R);
to_latin1([16#C3,16#89|R]) -> "É"++to_latin1(R);
to_latin1([16#C3,16#8A|R]) -> "Ê"++to_latin1(R);
to_latin1([16#C3,16#8B|R]) -> "Ë"++to_latin1(R);
to_latin1([16#C3,16#8C|R]) -> "Ì"++to_latin1(R);
to_latin1([16#C3,16#8D|R]) -> "Í"++to_latin1(R);
to_latin1([16#C3,16#8E|R]) -> "Î"++to_latin1(R);
to_latin1([16#C3,16#8F|R]) -> "Ï"++to_latin1(R);
to_latin1([16#C3,16#90|R]) -> "Ð"++to_latin1(R);
to_latin1([16#C3,16#91|R]) -> "Ñ"++to_latin1(R);
to_latin1([16#C3,16#92|R]) -> "Ò"++to_latin1(R);
to_latin1([16#C3,16#93|R]) -> "Ó"++to_latin1(R);
to_latin1([16#C3,16#94|R]) -> "Ô"++to_latin1(R);
to_latin1([16#C3,16#95|R]) -> "Õ"++to_latin1(R);
to_latin1([16#C3,16#96|R]) -> "Ö"++to_latin1(R);
to_latin1([16#C3,16#97|R]) -> "×"++to_latin1(R);
to_latin1([16#C3,16#98|R]) -> "Ø"++to_latin1(R);
to_latin1([16#C3,16#99|R]) -> "Ù"++to_latin1(R);
to_latin1([16#C3,16#9A|R]) -> "Ú"++to_latin1(R);
to_latin1([16#C3,16#9B|R]) -> "Û"++to_latin1(R);
to_latin1([16#C3,16#9C|R]) -> "Ü"++to_latin1(R);
to_latin1([16#C3,16#9D|R]) -> "Ý"++to_latin1(R);
to_latin1([16#C3,16#9E|R]) -> "Þ"++to_latin1(R);
to_latin1([16#C3,16#9F|R]) -> "ß"++to_latin1(R);
to_latin1([16#C3,16#A0|R]) -> "à"++to_latin1(R);
to_latin1([16#C3,16#A1|R]) -> "á"++to_latin1(R);
to_latin1([16#C3,16#A2|R]) -> "â"++to_latin1(R);
to_latin1([16#C3,16#A3|R]) -> "ã"++to_latin1(R);
to_latin1([16#C3,16#A4|R]) -> "ä"++to_latin1(R);
to_latin1([16#C3,16#A5|R]) -> "å"++to_latin1(R);
to_latin1([16#C3,16#A6|R]) -> "æ"++to_latin1(R);
to_latin1([16#C3,16#A7|R]) -> "ç"++to_latin1(R);
to_latin1([16#C3,16#A8|R]) -> "è"++to_latin1(R);
to_latin1([16#C3,16#A9|R]) -> "é"++to_latin1(R);
to_latin1([16#C3,16#AA|R]) -> "ê"++to_latin1(R);
to_latin1([16#C3,16#AB|R]) -> "ë"++to_latin1(R);
to_latin1([16#C3,16#AC|R]) -> "ì"++to_latin1(R);
to_latin1([16#C3,16#AD|R]) -> "í"++to_latin1(R);
to_latin1([16#C3,16#AE|R]) -> "î"++to_latin1(R);
to_latin1([16#C3,16#AF|R]) -> "ï"++to_latin1(R);
to_latin1([16#C3,16#B0|R]) -> "ð"++to_latin1(R);
to_latin1([16#C3,16#B1|R]) -> "ñ"++to_latin1(R);
to_latin1([16#C3,16#B2|R]) -> "ò"++to_latin1(R);
to_latin1([16#C3,16#B3|R]) -> "ó"++to_latin1(R);
to_latin1([16#C3,16#B4|R]) -> "ô"++to_latin1(R);
to_latin1([16#C3,16#B5|R]) -> "õ"++to_latin1(R);
to_latin1([16#C3,16#B6|R]) -> "ö"++to_latin1(R);
to_latin1([16#C3,16#B7|R]) -> "÷"++to_latin1(R);
to_latin1([16#C3,16#B8|R]) -> "ø"++to_latin1(R);
to_latin1([16#C3,16#B9|R]) -> "ù"++to_latin1(R);
to_latin1([16#C3,16#BA|R]) -> "ú"++to_latin1(R);
to_latin1([16#C3,16#BB|R]) -> "û"++to_latin1(R);
to_latin1([16#C3,16#BC|R]) -> "ü"++to_latin1(R);
to_latin1([16#C3,16#BD|R]) -> "ý"++to_latin1(R);
to_latin1([16#C3,16#BE|R]) -> "þ"++to_latin1(R);
to_latin1([16#C3,16#BF|R]) -> "ÿ"++to_latin1(R);
to_latin1([H|T]) -> [H|to_latin1(T)].
