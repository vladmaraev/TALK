/******************************************************

    TALK Program

******************************************************/

/*===================================================== 
    Operators
=====================================================*/

:- op(500,xfy,&).
:- op(510,xfy,=>).
:- op(100,fx,~).   % '

%%% print_answer(Answers)
%%% =====================
%%% Answers ==> nonempty list of answers to be printed
%%%             to the standard output separated
%%%             by commas.
%%%

print_answers([Answer]) :- !,
    write(Answer), write('.'), nl.

print_answers([Answer|Rest]) :- write(Answer),
				write(', '),
				print_answers(Rest).

%%% parse(Sentence, LF, Type)
%%% =========================
%%% Sentence ==> sentence to parse
%%% LF <== logical form (in FOL) of sentence 
%%% Type <== type of sentence (query or assertion)
%%%

% Parsing an assertion: a finite sentence without gaps.
parse(Sentence, LF, assertion) :-
    s(LF, nogap, Sentence, []).

% Parsing a query: a question.
parse(Sentence, LF, query) :-
    q(LF, Sentence, []).


/*===================================================== 
     Clausifier
=====================================================*/

%%% clausify(FOL, Clause, FreeVars)
%%% ===============================
%%%
%%% FOL ==> FOL expression to be converted to clause form
%%% Clause <== clause form of FOL expression
%%% FreeVars <== free variables in clause

% Universals: variable is left implicitly scoped.
clausify(all(X,F0),F,[X|V]) :- clausify(F0,F,V).

% Implications: consequent must be a literal,
% antecedent is clausified specially.
clausify(A0=>C0,(C:-A),V) :-
    clausify_literal(C0,C), clausify_antecedent(A0,A,V).

% Literals: left unchanged (except literal marker is removed)
clausify(C0,C,[]) :- clausify_literal(C0,C).

% Note that conjunctions and existentials are
% disallowed, since they can’t form Horn clauses.

%%% clausify_antecedent(FOL, Clause, FreeVars)
%%% ==========================================
%%%
%%% FOL ==> FOL expression to be converted to clause form
%%% Clause <== clause form of FOL expression
%%% FreeVars ==> list of free variables in clause

% Literals: left unchanged (except literal
% marker is removed).
clausify_antecedent(L0,L,[]) :- clausify_literal(L0,L).

% Conjunctions: each conjunct is clausified separately.
clausify_antecedent(E0&F0,(E,F),V) :-
    clausify_antecedent(E0,E,V0), clausify_antecedent(F0,F,V1), conc(V0,V1,V).

% Existentials: variable is left implicitly scoped.
clausify_antecedent(exists(X,F0),F,[X|V]) :-
    clausify_antecedent(F0,F,V).

%%% clausify_literal(Literal,Clause).
%%% =================================
%%%
%%% Literal ==> FOL literal to be converted to clause form
%%% Clause <== clause form of FOL expression
%%%
% Literal is left unchanged (except literal % marker is removed).
clausify_literal(~L,L).

/*===================================================== 
    Grammar

Nonterminal names:

	 q	 Question
	 sinv	 INVerted Sentence
	 s	 noninverted Sentence
	 np	 Noun Phrase
	 vp	 Verb Phrase
	 iv	 Intransitive Verb
	 tv	 Transitive Verb
	 aux	 AUXiliary verb
	 rov	 subject-Object Raising Verb 
         optrel  OPTional RELative clause 
         relpron RELative PRONoun
	 whpron	 WH PRONoun
	 det	 DETerminer
	 n	 Noun
	 pn	 Proper Noun

Typical order of and values for arguments: 

    1. verb form:
        (main verbs) finite, nonfinite, etc.
        (auxiliaries and raising verbs) Form1-Form2
            where Form1 is form of embedded VP Form2 is form of verb itself)

    2. FOL logical form
    
    3. gap information:
        nogap or gap(Nonterm, Var)
            where Nonterm is nonterminal for gap
                  Var is the LF variable
                       that the filler will bind
=====================================================*/


%%% Questions

q(S => ~answer(X)) -->
    whpron, vp(finite, X^S, nogap).
q(S => ~answer(X)) -->
    whpron, sinv(S, gap(np, X)).
q(S => ~answer(yes)) -->
    sinv(S, nogap).
q(S => ~answer(yes)) -->
    [is],
    np((X^S0)^S, nogap),
    np((X^true)^exists(X,S0&true), nogap).


%%% Declarative Sentences

s(S, GapInfo) -->
   np(VP^S, nogap),
   vp(finite, VP, GapInfo).


%%% Inverted Sentences

sinv(S, GapInfo) -->
    aux(finite/Form, VP1^VP2), np(VP2^S, nogap),
    vp(Form, VP1, GapInfo).


%%% Noun Phrases

np(NP, nogap) -->
    det(N2^NP), n(N1), optrel(N1^N2).
np(NP, nogap) --> pn(NP).
np((X^S)^S, gap(np, X)) --> [].


%%% Verb Phrases

vp(Form, X^S, GapInfo) -->
   tv(Form, X^VP),
   np(VP^S, GapInfo).
vp(Form, VP, nogap) -->
   iv(Form, VP).
vp(Form1, VP2, GapInfo) -->
    aux(Form1/Form2, VP1^VP2),
    vp(Form2, VP1, GapInfo).


vp(Form1, VP2, GapInfo) -->
    rov(Form1/Form2, NP^VP1^VP2),
    np(NP, GapInfo),
    vp(Form2, VP1, nogap).
vp(Form2, VP2, GapInfo) -->
    rov(Form1/Form2, NP^VP1^VP2),
    np(NP, nogap),
    vp(Form1, VP1, GapInfo).
vp(finite, X^S, GapInfo) -->
    [is],
    np((X^P)^exists(X,S&P), GapInfo).

%%% Relative Clauses
optrel((X^S1)^(X^(S1&S2))) -->
    relpron, vp(finite,X^S2, nogap).
optrel((X^S1)^(X^(S1&S2))) -->
    relpron, s(S2, gap(np, X)).
optrel(N^N) --> [].

/*===================================================== 
    Dictionary
=====================================================*/

/*----------------------------------------------------- 
    Preterminals
-----------------------------------------------------*/

det(LF) --> [D], {det(D, LF)}. % LF = logical form 
n(LF) --> [N], {n(N, LF)}.
pn((E^S)^S) --> [PN], {pn(PN, E)}.


aux(Form, LF) --> [Aux], {aux(Aux, Form, LF)}.
relpron --> [RP], {relpron(RP)}.
whpron --> [WH], {whpron(WH)}.

% Verb entry arguments:
% 1. nonfinite form of the verb
% 2. third person singular present tense form of the verb
% 3. past tense form of the verb
% 4. past participle form of the verb
% 5. pres participle form of the verb
% 6. logical form of the verb

iv(nonfinite	  , LF) --> [IV], {iv(IV, _, _, _, _, LF)}.
iv(finite	  , LF) --> [IV], {iv(_, IV, _, _, _, LF)}.
iv(finite	  , LF) --> [IV], {iv(_, _, IV, _, _, LF)}.
iv(past_participle, LF) --> [IV], {iv(_, _, _, IV, _, LF)}.
iv(pres_participle, LF) --> [IV], {iv(_, _, _, _, IV, LF)}.

tv(nonfinite	  , LF) --> [TV], {tv(TV, _, _, _, _, LF)}.
tv(finite	  , LF) --> [TV], {tv(_, TV, _, _, _, LF)}.
tv(finite	  , LF) --> [TV], {tv(_, _, TV, _, _, LF)}.
tv(past_participle, LF) --> [TV], {tv(_, _, _, TV, _, LF)}.
tv(pres_participle, LF) --> [TV], {tv(_, _, _, _, TV, LF)}.

rov(nonfinite	   /Requires, LF) --> [ROV], {rov(ROV, _, _, _, _, LF, Requires)}.
rov(finite	   /Requires, LF) --> [ROV], {rov(_, ROV, _, _, _, LF, Requires)}.
rov(finite	   /Requires, LF) --> [ROV], {rov(_, _, ROV, _, _, LF, Requires)}.
rov(past_participle/Requires, LF) --> [ROV], {rov(_, _, _, ROV, _, LF, Requires)}.
rov(pres_participle/Requires, LF) --> [ROV], {rov(_, _, _, _, ROV, LF, Requires)}.

/*----------------------------------------------------- 
    Lexical Items
-----------------------------------------------------*/

relpron( that ).
relpron( who ).
relpron( whom ).

whpron( who ).
whpron( whom ).
whpron( what ).

det( every, (X^S1)^(X^S2)^   all(X, S1=>S2) ).
det( a,     (X^S1)^(X^S2)^exists(X,S1&S2)   ).
det( some,  (X^S1)^(X^S2)^exists(X,S1&S2)   ).

n( author	, X^ ~author(X)		).
n( book		, X^ ~book(X)		).
n( professor	, X^ ~professor(X)	).
n( program	, X^ ~program(X)	).
n( programmer	, X^ ~programmer(X)	).
n( student	, X^ ~student(X)	).
pn( begriffsschrift	, begriffsschrift	).
pn( bertrand		, bertrand		).
pn( bill		, bill			).
pn( gottlob		, gottlob		).
pn( lunar		, lunar			).
pn( principia		, principia		).
pn( shrdlu		, shrdlu		).
pn( terry               , terry                 ).

iv( halt, halts, halted,
    halted, halting, X^ ~halt(X) ). % VM: halts(X)?

tv( write, writes, wrote,
    written, writing, X^Y^ ~writes(X,Y) ).

tv( meet, meets, met,
    met, meeting, X^Y^ ~meets(X,Y) ).

tv( concern, concerns, concerned,
    concerned, concerning, X^Y^ ~concerns(X,Y) ).


tv( run, run, runs, ran, running,
    X^Y^ ~runs(X,Y) ).

rov( want,
     wanted,
     wants, wanted, wanting,
     % semantics is partial execution of
     % NP ^ VP ^ Y ^ NP( X^want(Y,X,VP(X)) )
     ((X^ ~want(Y,X,Comp))^S) ^ (X^Comp) ^ Y ^ S,
     % form of VP required:
     infinitival).
     % Note: is it really true?

aux( to, infinitival/nonfinite, VP^ VP ).
aux( does, finite/nonfinite, VP^ VP ).
aux( did, finite/nonfinite, VP^ VP ).

/*===================================================== 
        Auxiliary Predicates
=====================================================*/

%%% conc(List1, List2, List)
%%% ========================
%%%
%%%   List1 ==> a list
%%%   List2 ==> a list 
%%%   List <== the concatenation of the two lists
%%%
%%% Note: seems to be the same as append/3

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
