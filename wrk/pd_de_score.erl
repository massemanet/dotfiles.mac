%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 10 Aug 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('pd_de_score').
-author('Mats Cronqvist').
-export([go/0
         , go/1
         , go/2]).

go() -> go("/mnt/kred_bups/pcache/*/*/*/DK*.debitor").

go(Wild) -> go(Wild,"").

go(Wild,OutFile) -> 
  case OutFile of 
    "" -> FD = standard_io;
    _ -> {ok,FD} = file:open(OutFile,[write])
  end,
  Is = files(filelib:wildcard(Wild)),
  try length([pp(FD,Pno,Real,Bs)||[_,{pno,Pno},{real,Real},{bads,Bs}]<-Is])
  after file:close(FD)
  end.

pp(FD,Pno,Real,Bs)->
  io:fwrite(FD,"~s - ~s~n",[Pno,pp_addr(Real)]),
  [io:fwrite(FD,"               ~s~n",[pp_addr(B)])||B<-Bs].

pp_addr({_,F,L,S,Z}) -> L++", "++F++" - "++S++", "++Z.

files([]) -> [];
files([F|Fs]) -> 
  try [[{file,F}|file(F)]|files(Fs)]
  catch 
    drop -> files(Fs);
    {{_,"empty document"},_} -> files(Fs);
    C:R -> io:fwrite("~p:~p ~s~n~p~n",[C,R,F,erlang:get_stacktrace()]),
           files(Fs)
  end.

file(F) ->
  case f(F) of
    [] -> throw(drop);
    R -> 
      Real = hd(f(F++"_info")),
      chk_contact(mk_pno(F),Real,filter(Real,R,[]))
  end.

chk_contact(Pno,Real,Bads) ->
  case chk_contact(Pno,Bads) of
    true -> [{pno,Pno},{real,Real},{bads,Bads}];
    false-> throw(drop)
  end.

mk_pno(F) ->
  pd_lib:snip(F,"\(DK[0-9]+\)").

filter(_,[],[]) -> throw(drop);
filter(_,[],Bads) -> Bads;
filter(Real,[Other|Others],Bads) ->
  case member(Other,Bads) of
    true -> filter(Real,Others,Bads);
    false -> 
      case is_good(Real,Other) of
        true -> filter(Real,Others,Bads);
        false-> filter(Real,Others,[Other|Bads])
      end
  end.

member(_,[]) -> false;
member({_,A,B,C,D},[{_,A,B,C,D}|_]) -> true;
member(X,[_|R]) -> member(X,R).

is_good(A,B) -> 
  (setelement(1,A,x) =:= setelement(1,B,x))
    orelse (97 < list_to_integer(element(1,B))).

f(F) ->
  try
    {ok,Raw} = file:read_file(F),
    saxy(Raw)
  catch
    R -> throw({R,F})
  end.

saxy(Raw) -> 
  coag(erlsom:sax(Raw,[],fun saxy/2),[]).

mk_street({S,F,L,S1,S2,S3,Z}) -> 
  {S,F,L,S1++" "++S2++S3,Z}.

coag([],O) -> lists:map(fun mk_street/1,O);
coag([{score,S}|R],O)            -> coag(R,[{S,"","","","","",""}|O]);
coag([{firstName,X}|R],[O|Os])   -> coag(R,[setelement(2,O,X)|Os]);
coag([{lastName,X}|R],[O|Os])    -> coag(R,[setelement(3,O,X)|Os]);
coag([{street,X}|R],[O|Os])      -> coag(R,[setelement(4,O,X)|Os]);
coag([{houseNumber,X}|R],[O|Os]) -> coag(R,[setelement(5,O,X)|Os]);
coag([{houseLetter,X}|R],[O|Os]) -> coag(R,[setelement(6,O,X)|Os]);
coag([{zip,X}|R],[O|Os])         -> coag(R,[setelement(7,O,X)|Os]).

saxy({endElement,_,"customerReference",_},S)       ->[{score,"real"}|S];
saxy({endElement,_,"activityClass",_},S)           ->[{score,"real"}|S];
saxy({startElement,_,"score",_,_},S)               ->[score|S];
saxy({startElement,_,"firstName",_,_},S)           ->[firstName|S];
saxy({startElement,_,"firstAndMiddleName",_,_},S)  ->[firstName|S];
saxy({startElement,_,"lastName",_,_},S)            ->[lastName|S];
saxy({startElement,_,"street",_,_},S)              ->[street|S];
saxy({startElement,_,"zip",_,_},S)                 ->[zip|S];
saxy({startElement,_,"houseNumber",_,_},S)         ->[houseNumber|S];
saxy({startElement,_,"houseLetter",_,_},S)         ->[houseLetter|S];
saxy({endElement,_,_,_},[Tag|S]) when is_atom(Tag) ->S;
saxy({characters,Cs},[Tag|S]) when is_atom(Tag)    ->[{Tag,strip(Cs)}|S];
saxy(_,S)                                          ->S.

strip(Cs) ->
  string:strip(string:strip(Cs),left,$0).

chk_contact(Pno, Bads) ->
  Fields = [first_name, last_name, zip],
  Cs     = contact:all_delivery_contacts_d(pno:pno(Pno),4),
  BadCs  = [contact:new(lists:zip(Fields,[F,L,Z])) || {_,F,L,_,Z} <- Bads],
  lists:any(fun(BadC) ->
                lists:any(fun(C) ->
                              contact_compare:eq(BadC, C, Fields)
                          end, Cs)
            end, BadCs).
