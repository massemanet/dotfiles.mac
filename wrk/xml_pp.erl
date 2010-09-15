%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 17 May 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('xml_pp').
-author('Mats Cronqvist').
-export([null/0
        , null/1]).

maxlinelength() -> 77.

null() ->
  null("/mnt/kred_bups/pcache/0/7/4/DK29309396.rki_addr").

null(File) ->
  {ok,Xml} = file:read_file(File),
  SaxTokens = sax_tokens(Xml),
  pp(SaxTokens).

sax_tokens(Xml) ->
  lists:reverse(erlsom:sax(Xml,[],fun(A,B)->[A|B]end)).

pp(Ts) ->
  lists:foldl(fun ppf/2,[],Ts).

-define(PRI(Target,H), {processingInstruction,Target,H}).
-define(BEG(El,Attrs), {startElement,_,El,_,Attrs}).
-define(END(El),       {endElement,_,El,_}).
-define(ATT(Id,Val),   {attribute,Id,_,_,Val}).
-define(CHS(Cs),       {characters,Cs}).
-define(IWS(),         {ignorableWhitespace,_}).
-define(EPM(),         {endPrefixMapping,_}).
-define(SPM(),         {startPrefixMapping,_,_}).

ppf(startDocument,_)      -> [0|[]];
ppf(?PRI("xml",H),O)      -> line(["<?xml",H,"?>"],0,O);
ppf(?SPM(),S)             -> S;
ppf(?IWS(),S)             -> S;
ppf(?EPM(),S)             -> S;
ppf(?BEG(T,As),{To,Ao,O}) -> {T,As,line(["<",To,as(Ao),">"],2,O)};
ppf(?BEG(T,As),O)         -> {T,As,O};
ppf(?CHS(Cs),{T,As,O})    -> {T,As,Cs,O};
ppf(?END(T),{T,As,O})     -> line(["<",T,as(As),"/>"],0,O);
ppf(?END(T),{T,As,Cs,O})  -> line(["<",T,as(As),">",Cs,"</",T,">"],0,O);
ppf(?END(T),O)            -> line(["</",T,">"],-2,O);
ppf(endDocument,[0|Strs]) -> lists:reverse(Strs).

line(L,N,[I|O]) when N<0 -> [I+N|[maybe_break(L,I+N)|O]];
line(L,N,[I|O])          -> [I+N|[maybe_break(L,I)|O]].

maybe_break(L,I) ->
  Line = aline(L,I),
  case maxlinelength() < length(Line) of
    true -> break(L,I,Line);
    false-> Line
  end.

as(As) ->
  case string:join(lists:foldl(fun asf/2,[],As)," ") of
    "" -> "";
    AS -> " "++AS
  end.

asf(?ATT(Id,Val),O) -> [string:join([Id,Val],"=")|O].

break(["<",T,As,">",Cs,"</",T,">"],I,_) ->
  [aline(["<",T,As,">"],I),aline([Cs],I+2),aline(["</",T,">"],I)];
break(_,_,Line) ->
  Line.

aline(L,I) ->
  lists:flatten(ind(I)++string:join(L,"")++"\n").

ind(I) ->
  lists:duplicate(I,$ ).
