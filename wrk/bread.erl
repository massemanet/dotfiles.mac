%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 25 May 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('bread').
-author('Mats Cronqvist').

-export([sax/3
         , term/3
         , line/3
         , fold/4]).

-define(BLOCK, 1048576).

%% this is an attempt to use "abstract patterns"
-define(tail_0(),             {<<>>,0,''}).
-define(tail(Bin,Offset),     {Bin,Offset,''}).
-define(tail_eof(Bin,Offset), {Bin,Offset,eof}).
-define(state(Acc,Tail),      {Acc,Tail}).

-define(log(T),error_logger:info_report(
                 [process_info(self(),current_function),{line,?LINE},T])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the API
line(Filename,Fun,Acc) ->
  fold(Filename,Fun,Acc,[line]).

sax(Filename,Fun,Acc) ->
  fold(Filename,Fun,Acc,[xml]).

term(Filename,Fun,Acc) ->
 fold(Filename,Fun,Acc,[term]).

fold(Filename,Fun,Acc,Opts) ->
  case file:open(Filename, [read, raw, binary]) of
    {ok, FD} -> 
      try fold(read(FD),FD,wrap(Fun,Opts),chunker(Opts),?state(Acc,?tail_0()))
      after file:close(FD)
      end;
    {error,R} ->
      exit({open_error, R, Filename})
  end.

wrap(Fun,Opts) ->
  Es = [xml,term,line],
  case [proplists:is_defined(T,Opts) || T <- Es] of
    [true,false,false] -> xml_f(Fun);
    [false,true,false] -> term_f(Fun);
    [false,false,true] -> Fun;
    [false,false,false]-> exit({valid_entities_are,Es});
    _                  -> exit({use_one_of,Es})
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% term funs
term_f(Fun) ->
  fun(TermBin,O) ->
      try Fun(to_term(TermBin),O)
      catch {parsing_failed,R} -> ?log({parse_error,R,TermBin}),O
      end
  end.
  
to_term(Bin) ->
  try
    {ok,Ts,_}= erl_scan:string(binary_to_list(Bin)),
    {ok,Term} = erl_parse:parse_term(Ts),
    Term
  catch
    _:R -> throw({parsing_failed,R})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SAX funs
xml_f(Fun) ->
  fun(RawXML,O) -> 
      try [erlsom:sax(encoding(RawXML),[],sax_f(Fun))|O]
      catch error:R -> ?log([{erlang:get_stacktrace(),RawXML}]),exit(R)
      end
  end.

-define(BEG(El,Attrs), {startElement,_,El,_,Attrs}).
-define(END(El),       {endElement,_,El,_}).
-define(CHS(Cs),       {characters,Cs}).
-define(ATT(Id,Val),   {attribute,Id,_,_,Val}).
sax_f(Fun) ->
  fun(startDocument,_)   -> Fun(startDoc,[]);
     (?BEG(El,As),State) -> Fun({startEl,El,[{Id,V}||?ATT(Id,V)<-As]},State);
     (?END(El),State)    -> Fun({endEl,El},State);
     (?CHS(Cs),State)    -> Fun({chs,Cs},State);
     (endDocument,State) -> Fun(endDoc,State);
     (_,State)           -> State
  end.

%% here I assume that the input file is encoded in latin-1
%% If the XML claims to be utf-8, we convert latin-1 -> utf-8
%% This will fail silently if the input file is utf-8 :<
encoding(Bin) ->
  case re:run(Bin,"<\\?xml.*iso-8859-1.*\\?>",[caseless,ungreedy,anchored]) of
    nomatch  -> unicode:characters_to_binary(Bin,latin1);
    {match,_}-> binary_to_list(Bin)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% implementation
fold(eof,_,Fun,Chunker,State) ->
  ?state(Acc,Tail) = folder(Chunker,Fun,state_eof(State)),
  [?log({trailing,Tail}) || not is_empty(Tail)],
  Acc;
fold({ok,Chunk},FD,Fun,Chunker,State) -> 
  fold(read(FD),FD,Fun,Chunker,folder(Chunker,Fun,add_chunk(Chunk,State))).

folder(Chunker,Fun,?state(Acc,Tail)) ->
  case Chunker(Tail) of
    {ok,Term,NTail} -> folder(Chunker,Fun,?state(Fun(Term,Acc),NTail));
    {cont,NTail} -> ?state(Acc,NTail)
  end.

read(FD) ->
  file:read(FD, ?BLOCK).

add_chunk(Chunk,?state(Acc,?tail(Cnt,Off))) ->
  erlang:garbage_collect(),
  <<_:Off/binary,C/binary>> = Cnt,
  ?state(Acc,?tail(<<C/binary,Chunk/binary>>,0)).

is_empty(?tail(Bin,Off)) ->
  byte_size(Bin) == Off.

state_eof(?state(Acc,?tail(Bin,Off))) ->
  ?state(Acc,?tail_eof(Bin,Off)).

chunker_patt(term) ->
  re:compile("(.*\\.\\R)",[dotall,ungreedy]);
chunker_patt(xml) ->
  re:compile("(<\\?xml.*\\?>.*)<\\?xml.*\\?>",[dotall,caseless,ungreedy]);
chunker_patt(line) ->
  re:compile("\\R*(.+)\\R").

-define(is_empty(X), X=:=<<>>; X=:=<<"\n">>; X=:=<<"\n\r">>).
chunker(Opts) ->
  {ok,Patt} = chunker_patt(take_first(Opts,[line,xml,term])),
  fun(?tail_eof(Bin,Off)) ->
      case split_binary(Bin,Off) of
        {_,T} when ?is_empty(T) -> {cont,?tail_0()};
        {_,Trail}      -> {ok,Trail,?tail_0()}
      end;
     (Tail=?tail(Bin,Off)) -> 
      case re:run(Bin,Patt,[{offset,Off},{capture,[1]}]) of
        nomatch         -> {cont,Tail};
        {match,[{O,L}]} -> {ok,snip(Bin,O,L),?tail(Bin,O+L)}
      end
  end.

snip(Bin,B,L) ->
  <<_:B/binary,R:L/binary,_/binary>> = Bin,
  R.

take_first(Opts,[Alt|Alts]) ->
  case proplists:is_defined(Alt,Opts) of
    true -> Alt;
    false-> take_first(Opts,Alts)
  end.
