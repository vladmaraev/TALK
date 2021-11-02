/******************************************************

    TALK Program

******************************************************/


/*----------------------------------------------------- 
    Lexical Items
-----------------------------------------------------*/

relpron( that ).
relpron( who ).
relpron( whom ).

whpron( who ).
whpron( whom ).
whpron( what ).

det( every, (XˆS1)ˆ(XˆS2)ˆ   all(X, S1=>S2) ).
det( a,     (XˆS1)ˆ(XˆS2)ˆexists(X,S1&S2)   ).
det( some,  (XˆS1)ˆ(XˆS2)ˆexists(X,S1&S2)   ).

n( author	, Xˆ ‘author(X)		).
n( book		, Xˆ ‘book(X)		).
n( professor	, Xˆ ‘professor(X)	).
n( program	, Xˆ ‘program(X)	).
n( programmer	, Xˆ ‘programmer(X)	).
n( student	, Xˆ ‘student(X)	).
pn( begriffsschrift	, begriffsschrift	).
pn( bertrand		, bertrand		).
pn( bill		, bill			).
pn( gottlob		, gottlob		).
pn( lunar		, lunar			).
pn( principia		, principia		).
pn( shrdlu		, shrdlu		).
pn( terry               , terry                 ).

iv( halt, halts, halted,
    halted, halting, Xˆ ‘halt(X) ). % VM: halts(X)?

tv( write, writes, wrote,
    written, writing, XˆYˆ ‘writes(X,Y) ).

tv( meet, meets, met,
    met, meeting, XˆYˆ ‘meets(X,Y) ).

tv( concern, concerns, concerned,
    concerned, concerning, XˆYˆ ‘concerns(X,Y) ).


tv( run, run, runs, ran, running,
    XˆYˆ ‘runs(X,Y) ).

rov( want,
     wanted,
     wants, wanted, wanting,
     % semantics is partial execution of
     % NP ˆ VP ˆ Y ˆ NP( Xˆwant(Y,X,VP(X)) )
     ((Xˆ ‘want(Y,X,Comp))ˆS) ˆ (XˆComp) ˆ Y ˆ S,
     % form of VP required:
     infinitival).

aux( to, infinitival/nonfinite, VPˆ VP ).
aux( does, finite/nonfinite, VPˆ VP ).
aux( did, finite/nonfinite, VPˆ VP ).

/*===================================================== 
        Auxiliary Predicates
=====================================================*/

%%% conc(List1, List2, List)
%%% ========================
%%%
%%%   List1 ==> a list
%%%   List2 ==> a list 
%%%   List <== the concatenation of the two lists

conc([], List, List).
conc([Element|Rest], List, [Element|LongRest]) :-
    conc(Rest, List, LongRest).


%%% read_sent(Words)
%%% ================
%%%    Words ==> set of words read from the standard input
%%%
%%%    Words are delimited by spaces and the 
%%%    line is ended by a newline. Case is not folded; 
%%%    punctuation is not stripped.

read_sent(Words) :-
    get_code(Char), % prime the lookahead
    read_sent(Char, Words). % get the words


% Newlines end the input.
read_sent(C, []) :- newline(C), !.

% Spaces are ignored.
read_sent(C, Words) :- space(C), !,
		       get_code(Char),
		       read_sent(Char, Words).

% Everything else starts a word.
read_sent(Char, [Word|Words]) :-
    read_word(Char, Chars, Next), % get the word
    name(Word, Chars), % pack the characters into an atom
    read_sent(Next, Words). % get some more words


%%% read_word(Chars)
%%% ================
%%%
%%% Chars ==> list of characters read from standard input
%%%               and delimited by spaces or newlines

% Space and newline end a word.
read_word(C, [], C) :- space(C), !.
read_word(C, [], C) :- newline(C), !.


% All other chars are added to the list.
read_word(Char, [Char|Chars], Last) :-
    get_code(Next),
    read_word(Next, Chars, Last).


%%% space(Char)
%%% ================
%%%
%%% Char === the ASCII code for the space character
%%%               

space(32).

%%% newline(Char)
%%% ================
%%%
%%% Char === the ASCII code for the newline character
%%%

newline(10).
