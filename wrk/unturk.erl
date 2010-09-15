%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 16 Aug 2010 by masse <masse@kreditor.se>

%% @doc
%%  "unturk" is terminology for "ad-hoc crappy unicode -> latin-1 coercion".
%%  takes a list of UCS codepoints, and returns a list of latin-1 chars.
%%  UCS == latin-1 below 256.
%%  translates all utf-8 0xC4** and 0xC5** characters by removing umlauts or 
%% taking the first letter of a ligature.
%%  translates some chars from the CP1252 / Windows ANSI to similar latin-1.
%%  replaces all other non-latin-1 chars with space.
%%  conserves string length by mishandling ligatures.
%%  reference: http://www.utf8-chartable.de/unicode-utf8-table.pl
%% @end

-module('unturk').
-author('masse').
-export([string/1,char/1]).
-export([unit/0]).

string(Bin) when is_binary(Bin) -> string(binary_to_list(Bin));
string(Str) when is_list(Str)   -> [char(C) || C <- Str].

char(C) when C < 256 -> C;
%%%% UCS         latin-1 UTF-8
char(16#0100) -> $A ; %  c480    LATIN CAPITAL LETTER A WITH MACRON
char(16#0101) -> $a ; %  c481    LATIN SMALL LETTER A WITH MACRON
char(16#0102) -> $A ; %  c482    LATIN CAPITAL LETTER A WITH BREVE
char(16#0103) -> $a ; %  c483    LATIN SMALL LETTER A WITH BREVE
char(16#0104) -> $A ; %  c484    LATIN CAPITAL LETTER A WITH OGONEK
char(16#0105) -> $a ; %  c485    LATIN SMALL LETTER A WITH OGONEK
char(16#0106) -> $C ; %  c486    LATIN CAPITAL LETTER C WITH ACUTE
char(16#0107) -> $c ; %  c487    LATIN SMALL LETTER C WITH ACUTE
char(16#0108) -> $C ; %  c488    LATIN CAPITAL LETTER C WITH CIRCUMFLEX
char(16#0109) -> $c ; %  c489    LATIN SMALL LETTER C WITH CIRCUMFLEX
char(16#010A) -> $C ; %  c48a    LATIN CAPITAL LETTER C WITH DOT ABOVE
char(16#010B) -> $c ; %  c48b    LATIN SMALL LETTER C WITH DOT ABOVE
char(16#010C) -> $C ; %  c48c    LATIN CAPITAL LETTER C WITH CARON
char(16#010D) -> $c ; %  c48d    LATIN SMALL LETTER C WITH CARON
char(16#010E) -> $D ; %  c48e    LATIN CAPITAL LETTER D WITH CARON
char(16#010F) -> $d ; %  c48f    LATIN SMALL LETTER D WITH CARON
char(16#0110) -> $D ; %  c490    LATIN CAPITAL LETTER D WITH STROKE
char(16#0111) -> $d ; %  c491    LATIN SMALL LETTER D WITH STROKE
char(16#0112) -> $E ; %  c492    LATIN CAPITAL LETTER E WITH MACRON
char(16#0113) -> $e ; %  c493    LATIN SMALL LETTER E WITH MACRON
char(16#0114) -> $E ; %  c494    LATIN CAPITAL LETTER E WITH BREVE
char(16#0115) -> $e ; %  c495    LATIN SMALL LETTER E WITH BREVE
char(16#0116) -> $E ; %  c496    LATIN CAPITAL LETTER E WITH DOT ABOVE
char(16#0117) -> $e ; %  c497    LATIN SMALL LETTER E WITH DOT ABOVE
char(16#0118) -> $E ; %  c498    LATIN CAPITAL LETTER E WITH OGONEK
char(16#0119) -> $e ; %  c499    LATIN SMALL LETTER E WITH OGONEK
char(16#011A) -> $E ; %  c49a    LATIN CAPITAL LETTER E WITH CARON
char(16#011B) -> $e ; %  c49b    LATIN SMALL LETTER E WITH CARON
char(16#011C) -> $G ; %  c49c    LATIN CAPITAL LETTER G WITH CIRCUMFLEX
char(16#011D) -> $g ; %  c49d    LATIN SMALL LETTER G WITH CIRCUMFLEX
char(16#011E) -> $G ; %  c49e    LATIN CAPITAL LETTER G WITH BREVE
char(16#011F) -> $g ; %  c49f    LATIN SMALL LETTER G WITH BREVE
char(16#0120) -> $G ; %  c4a0    LATIN CAPITAL LETTER G WITH DOT ABOVE
char(16#0121) -> $g ; %  c4a1    LATIN SMALL LETTER G WITH DOT ABOVE
char(16#0122) -> $G ; %  c4a2    LATIN CAPITAL LETTER G WITH CEDILLA
char(16#0123) -> $g ; %  c4a3    LATIN SMALL LETTER G WITH CEDILLA
char(16#0124) -> $H ; %  c4a4    LATIN CAPITAL LETTER H WITH CIRCUMFLEX
char(16#0125) -> $h ; %  c4a5    LATIN SMALL LETTER H WITH CIRCUMFLEX
char(16#0126) -> $H ; %  c4a6    LATIN CAPITAL LETTER H WITH STROKE
char(16#0127) -> $h ; %  c4a7    LATIN SMALL LETTER H WITH STROKE
char(16#0128) -> $I ; %  c4a8    LATIN CAPITAL LETTER I WITH TILDE
char(16#0129) -> $i ; %  c4a9    LATIN SMALL LETTER I WITH TILDE
char(16#012A) -> $I ; %  c4aa    LATIN CAPITAL LETTER I WITH MACRON
char(16#012B) -> $i ; %  c4ab    LATIN SMALL LETTER I WITH MACRON
char(16#012C) -> $I ; %  c4ac    LATIN CAPITAL LETTER I WITH BREVE
char(16#012D) -> $i ; %  c4ad    LATIN SMALL LETTER I WITH BREVE
char(16#012E) -> $I ; %  c4ae    LATIN CAPITAL LETTER I WITH OGONEK
char(16#012F) -> $i ; %  c4af    LATIN SMALL LETTER I WITH OGONEK
char(16#0130) -> $I ; %  c4b0    LATIN CAPITAL LETTER I WITH DOT ABOVE
char(16#0131) -> $i ; %  c4b1    LATIN SMALL LETTER DOTLESS I
char(16#0132) -> $i ; %  c4b2    LATIN CAPITAL LIGATURE IJ
char(16#0133) -> $i ; %  c4b3    LATIN SMALL LIGATURE IJ
char(16#0134) -> $J ; %  c4b4    LATIN CAPITAL LETTER J WITH CIRCUMFLEX
char(16#0135) -> $j ; %  c4b5    LATIN SMALL LETTER J WITH CIRCUMFLEX
char(16#0136) -> $K ; %  c4b6    LATIN CAPITAL LETTER K WITH CEDILLA
char(16#0137) -> $k ; %  c4b7    LATIN SMALL LETTER K WITH CEDILLA
char(16#0138) -> $k ; %  c4b8    LATIN SMALL LETTER KRA
char(16#0139) -> $L ; %  c4b9    LATIN CAPITAL LETTER L WITH ACUTE
char(16#013A) -> $l ; %  c4ba    LATIN SMALL LETTER L WITH ACUTE
char(16#013B) -> $L ; %  c4bb    LATIN CAPITAL LETTER L WITH CEDILLA
char(16#013C) -> $l ; %  c4bc    LATIN SMALL LETTER L WITH CEDILLA
char(16#013D) -> $L ; %  c4bd    LATIN CAPITAL LETTER L WITH CARON
char(16#013E) -> $l ; %  c4be    LATIN SMALL LETTER L WITH CARON
char(16#013F) -> $L ; %  c4bf    LATIN CAPITAL LETTER L WITH MIDDLE DOT
char(16#0140) -> $l ; %  c580    LATIN SMALL LETTER L WITH MIDDLE DOT
char(16#0141) -> $L ; %  c581    LATIN CAPITAL LETTER L WITH STROKE
char(16#0142) -> $l ; %  c582    LATIN SMALL LETTER L WITH STROKE
char(16#0143) -> $N ; %  c583    LATIN CAPITAL LETTER N WITH ACUTE
char(16#0144) -> $n ; %  c584    LATIN SMALL LETTER N WITH ACUTE
char(16#0145) -> $N ; %  c585    LATIN CAPITAL LETTER N WITH CEDILLA
char(16#0146) -> $n ; %  c586    LATIN SMALL LETTER N WITH CEDILLA
char(16#0147) -> $N ; %  c587    LATIN CAPITAL LETTER N WITH CARON
char(16#0148) -> $n ; %  c588    LATIN SMALL LETTER N WITH CARON
char(16#0149) -> $n ; %  c589    LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
char(16#014A) -> $N ; %  c58a    LATIN CAPITAL LETTER ENG
char(16#014B) -> $n ; %  c58b    LATIN SMALL LETTER ENG
char(16#014C) -> $O ; %  c58c    LATIN CAPITAL LETTER O WITH MACRON
char(16#014D) -> $o ; %  c58d    LATIN SMALL LETTER O WITH MACRON
char(16#014E) -> $O ; %  c58e    LATIN CAPITAL LETTER O WITH BREVE
char(16#014F) -> $o ; %  c58f    LATIN SMALL LETTER O WITH BREVE
char(16#0150) -> $O ; %  c590    LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
char(16#0151) -> $o ; %  c591    LATIN SMALL LETTER O WITH DOUBLE ACUTE
char(16#0152) -> $O ; %  c592    LATIN CAPITAL LIGATURE OE
char(16#0153) -> $o ; %  c593    LATIN SMALL LIGATURE OE
char(16#0154) -> $R ; %  c594    LATIN CAPITAL LETTER R WITH ACUTE
char(16#0155) -> $r ; %  c595    LATIN SMALL LETTER R WITH ACUTE
char(16#0156) -> $R ; %  c596    LATIN CAPITAL LETTER R WITH CEDILLA
char(16#0157) -> $r ; %  c597    LATIN SMALL LETTER R WITH CEDILLA
char(16#0158) -> $R ; %  c598    LATIN CAPITAL LETTER R WITH CARON
char(16#0159) -> $r ; %  c599    LATIN SMALL LETTER R WITH CARON
char(16#015A) -> $S ; %  c59a    LATIN CAPITAL LETTER S WITH ACUTE
char(16#015B) -> $s ; %  c59b    LATIN SMALL LETTER S WITH ACUTE
char(16#015C) -> $S ; %  c59c    LATIN CAPITAL LETTER S WITH CIRCUMFLEX
char(16#015D) -> $s ; %  c59d    LATIN SMALL LETTER S WITH CIRCUMFLEX
char(16#015E) -> $S ; %  c59e    LATIN CAPITAL LETTER S WITH CEDILLA
char(16#015F) -> $s ; %  c59f    LATIN SMALL LETTER S WITH CEDILLA
char(16#0160) -> $S ; %  c5a0    LATIN CAPITAL LETTER S WITH CARON
char(16#0161) -> $s ; %  c5a1    LATIN SMALL LETTER S WITH CARON
char(16#0162) -> $T ; %  c5a2    LATIN CAPITAL LETTER T WITH CEDILLA
char(16#0163) -> $t ; %  c5a3    LATIN SMALL LETTER T WITH CEDILLA
char(16#0164) -> $T ; %  c5a4    LATIN CAPITAL LETTER T WITH CARON
char(16#0165) -> $t ; %  c5a5    LATIN SMALL LETTER T WITH CARON
char(16#0166) -> $T ; %  c5a6    LATIN CAPITAL LETTER T WITH STROKE
char(16#0167) -> $t ; %  c5a7    LATIN SMALL LETTER T WITH STROKE
char(16#0168) -> $U ; %  c5a8    LATIN CAPITAL LETTER U WITH TILDE
char(16#0169) -> $u ; %  c5a9    LATIN SMALL LETTER U WITH TILDE
char(16#016A) -> $U ; %  c5aa    LATIN CAPITAL LETTER U WITH MACRON
char(16#016B) -> $u ; %  c5ab    LATIN SMALL LETTER U WITH MACRON
char(16#016C) -> $U ; %  c5ac    LATIN CAPITAL LETTER U WITH BREVE
char(16#016D) -> $u ; %  c5ad    LATIN SMALL LETTER U WITH BREVE
char(16#016E) -> $U ; %  c5ae    LATIN CAPITAL LETTER U WITH RING ABOVE
char(16#016F) -> $u ; %  c5af    LATIN SMALL LETTER U WITH RING ABOVE
char(16#0170) -> $U ; %  c5b0    LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
char(16#0171) -> $u ; %  c5b1    LATIN SMALL LETTER U WITH DOUBLE ACUTE
char(16#0172) -> $U ; %  c5b2    LATIN CAPITAL LETTER U WITH OGONEK
char(16#0173) -> $u ; %  c5b3    LATIN SMALL LETTER U WITH OGONEK
char(16#0174) -> $W ; %  c5b4    LATIN CAPITAL LETTER W WITH CIRCUMFLEX
char(16#0175) -> $w ; %  c5b5    LATIN SMALL LETTER W WITH CIRCUMFLEX
char(16#0176) -> $Y ; %  c5b6    LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
char(16#0177) -> $y ; %  c5b7    LATIN SMALL LETTER Y WITH CIRCUMFLEX
char(16#0178) -> $Y ; %  c5b8    LATIN CAPITAL LETTER Y WITH DIAERESIS
char(16#0179) -> $Z ; %  c5b9    LATIN CAPITAL LETTER Z WITH ACUTE
char(16#017A) -> $z ; %  c5ba    LATIN SMALL LETTER Z WITH ACUTE
char(16#017B) -> $Z ; %  c5bb    LATIN CAPITAL LETTER Z WITH DOT ABOVE
char(16#017C) -> $z ; %  c5bc    LATIN SMALL LETTER Z WITH DOT ABOVE
char(16#017D) -> $Z ; %  c5bd    LATIN CAPITAL LETTER Z WITH CARON
char(16#017E) -> $z ; %  c5be    LATIN SMALL LETTER Z WITH CARON
char(16#017F) -> $s ; %  c5bf    LATIN SMALL LETTER LONG S

%% some chars that appear in CP1252 (Windows ANSI)
char(16#0191) -> $F ; %  c691    LATIN CAPITAL LETTER F WITH HOOK
char(16#0192) -> $f ; %  c692    LATIN SMALL LETTER F WITH HOOK
char(16#02DC) -> $~ ; %  cb9c    SMALL TILDE
char(16#2010) -> $- ; %  e28090  HYPHEN
char(16#2011) -> $- ; %  e28091  NON-BREAKING HYPHEN
char(16#2012) -> $- ; %  e28092  FIGURE DASH
char(16#2013) -> $- ; %  e28093  EN DASH
char(16#2014) -> $- ; %  e28094  EM DASH
char(16#2015) -> $- ; %  e28095  HORIZONTAL BAR
char(16#2016) -> $  ; %  e28096  DOUBLE VERTICAL LINE
char(16#2017) -> $  ; %  e28097  DOUBLE LOW LINE
char(16#2018) -> $' ; %  e28098  LEFT SINGLE QUOTATION MARK
char(16#2019) -> $' ; %  e28099  RIGHT SINGLE QUOTATION MARK
char(16#201A) -> $' ; %  e2809a  SINGLE LOW-9 QUOTATION MARK
char(16#201B) -> $' ; %  e2809b  SINGLE HIGH-REVERSED-9 QUOTATION MARK
char(16#201C) -> $" ; %  e2809c  LEFT DOUBLE QUOTATION MARK
char(16#201D) -> $" ; %  e2809d  RIGHT DOUBLE QUOTATION MARK
char(16#201E) -> $" ; %  e2809e  DOUBLE LOW-9 QUOTATION MARK
char(16#201F) -> $" ; %  e2809f  DOUBLE HIGH-REVERSED-9 QUOTATION MARK
char(16#2020) -> $  ; %  e280a0  DAGGER
char(16#2021) -> $  ; %  e280a1  DOUBLE DAGGER
char(16#2022) -> $- ; %  e280a2  BULLET
char(16#2023) -> $- ; %  e280a3  TRIANGULAR BULLET
char(16#2024) -> $. ; %  e280a4  ONE DOT LEADER
char(16#2025) -> $. ; %  e280a5  TWO DOT LEADER
char(16#2026) -> $. ; %  e280a6  HORIZONTAL ELLIPSIS
char(16#2027) -> $- ; %  e280a7  HYPHENATION POINT
char(16#20AC) -> $E ; %  e282ac  EURO SIGN

char(_) -> $  . %  default

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
  "OoSsYZzf~--'''\"\"\"-.E" = 
  string(lists:append([element(1,erlsom_ucs:from_utf8(U))||U<-UTF8s])).
