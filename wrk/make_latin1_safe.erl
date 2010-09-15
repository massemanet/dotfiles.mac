%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 13 Aug 2010 by masse <masse@kreditor.se>
%% @doc
%% takes a list of UCS codepoints, and returns a list of latin-1 chars.
%% translates some chars (from the CP1252 / Windows ANSI set) to a similar
%% latin-1 char, replaces all other non-latin-1 chars with space.
%% @end

-module('make_latin1_safe').
-author('masse').
-export([string/1,char/1]).
-export([unit/0]).

string(Str) when is_list(Str) ->
  [char(C) || C <- Str].

char(C) when C < 256 -> C;
%%%% UCS      latin-1 UTF-8
char(8364) -> $E ;    %% e282ac 
char(8218) -> $' ;    %% e2809a 
char(402)  -> $f ;    %% c692   
char(8222) -> $" ;    %% e2809e 
char(8230) -> $. ;    %% e280a6 
char(352)  -> $S ;    %% c5a0   
char(338)  -> $Ö ;    %% c592   
char(381)  -> $Z ;    %% c5bd   
char(8216) -> $' ;    %% e28098 
char(8217) -> $' ;    %% e28099 
char(8220) -> $" ;    %% e2809c 
char(8221) -> $" ;    %% e2809d 
char(8226) -> $. ;    %% e280a2 
char(8211) -> $- ;    %% e28093 
char(8212) -> $- ;    %% e28094 
char(732)  -> $~ ;    %% cb9c   
char(353)  -> $s ;    %% c5a1   
char(339)  -> $ö ;    %% c593   
char(382)  -> $z ;    %% c5be   
char(376)  -> $Y ;    %% c5b8   
char(_)    -> $  .

unit() ->
  UTF8s = 
    [[16#c5,16#92],
     [16#c5,16#93],
     [16#c5,16#a0],
     [16#c5,16#a1],
     [16#c5,16#b8],
     [16#c5,16#bd],
     [16#c5,16#be],
     [16#c6,16#92],
     [16#cb,16#9c],
     [16#e2,16#80,16#93],
     [16#e2,16#80,16#94],
     [16#e2,16#80,16#98],
     [16#e2,16#80,16#99],
     [16#e2,16#80,16#9a],
     [16#e2,16#80,16#9c],
     [16#e2,16#80,16#9d],
     [16#e2,16#80,16#9e],
     [16#e2,16#80,16#a2],
     [16#e2,16#80,16#a6],
     [16#e2,16#82,16#ac]],
  "ÖöSsYZzf~--'''\"\"\"..E" = 
  string(lists:append([element(1,erlsom_ucs:from_utf8(U))||U<-UTF8s])).
