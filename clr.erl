%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 16 Feb 2012 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('clr').
-author('mats cronqvist').
-export([go/0]).
-export([octree_put/2,octree_put/3
         ,octree_get/2
         ,octree_clr/2]).

go() ->
  mk_table(),
  fill_table().

mk_table() ->
  spawn(fun()->
            try 
              register(?MODULE,self()),
              ets:new(?MODULE,[named_table,ordered_set,public]),
              receive _ -> ok end
            catch
              _:_ -> ok
            end
        end).

fill_table() -> fill_table("/usr/X11/share/X11/rgb.txt").
fill_table(F) ->
  parse(binary_to_list(element(2,file:read_file(F)))).

parse(Str) -> lists:reverse(parse(Str,ws,[])).

-define(is_ws(C), (C==$  orelse C==$\t orelse C==$\n orelse C==$\r)).
-define(is_digit(C), ($0 =< C andalso C =< $9)).
-define(is_str(S), is_integer(hd(S))).
parse([X|R],ws,O) when ?is_ws(X)                  -> parse(R,ws,O);
parse([X|R],ws,O) when ?is_digit(X)               -> parse(R,ii,[[X]|O]);
parse([X|R],ws,O)                                 -> parse(R,wd,[[X]|O]);
parse([X|R],ii,[P|O]) when ?is_ws(X)              -> parse(R,ws,[i(r(P))|O]);
parse([X|R],ii,[P|O]) when ?is_digit(X)           -> parse(R,ii,[[X|P]|O]);
parse([X|R],ii,[P|O])                             -> parse(R,wd,[[X|P]|O]);
parse([X|R],wd,[P,Q|O]) when ?is_ws(X),?is_str(Q) -> parse(R,ws,[j(r(P),Q)|O]);
parse([X|R],wd,[P|O]) when ?is_ws(X)              -> parse(R,ws,[r(P)|O]);
parse([X|R],wd,[P|O])                             -> parse(R,wd,[[X|P]|O]);
parse([],ii,[P|O])                                -> [i(r(P))|O];
parse([],_,O)                                     -> O.

i(S) -> list_to_integer(S).
r(S) -> lists:reverse(S).
j(P,Q) -> string:join([Q,P]," ").

octree_clr([],Tree) -> Tree;
octree_clr([I|K],Tree) ->
  case element(I,Tree) of
    {}      -> Tree;
    {K,_}   -> set(I,Tree,{});
    {_,_}   -> Tree;
    SubTree -> set(I,Tree,octree_clr(K,SubTree))
  end.

octree_get([],_) ->
  error(no_such_key);
octree_get([I|K],Tree) ->
  case element(I,Tree) of
    {}      -> error(no_such_key);
    {K,V}   -> V;
    {_,_}   -> error(no_such_key);
    SubTree -> octree_get(K,SubTree)
  end.

%% K is a list of length 8, where each element is an integer 0-7
octree_put(K,V) ->
  octree_put(K,V,octree_new()).

octree_put([],V,_) ->
  %% a final leaf
  {[],V};
octree_put([I|K],V,Tree) ->
  %% sub_tree can be a non-final leaf
  case element(I,Tree) of
    {}      -> set(I,Tree,{K,V});
    {K,_}   -> set(I,Tree,{K,V});
    {KO,VO} -> set(I,Tree,octree_put(K,V,octree_put(KO,VO,octree_new())));
    SubTree -> set(I,Tree,octree_put(K,V,SubTree))
  end.

set(I,T,V) -> setelement(I,T,V).

octree_new() -> erlang:make_tuple(8,{}).
