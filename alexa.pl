:- dynamic(noun1/1).
:- dynamic(fact/3).

a(R):-alexa(R).
alexa(REQ):-
	split(REQ,S),
	process(S,RES),
	nl,write(RES),nl.

process([alexa],'yes? how can i help you today.').
process(S,R):-
	wake_word(S,WAKE),
	( WAKE = 'yes' -> remove_wake_words(S,W),find_intent(W,INTENT),process_intent(INTENT,S,R) ; match(R,'Are you talking to me? to start conversation try using alexa, hi alexa, hey alexa,') ).

wake_word([W1,W2|_],WAKE):-
	( (W1 = 'alexa,') ; (W1 = 'hi'),(W2 = 'alexa,') ; ( W1='hey' ),( W2='alexa,' ) -> match(WAKE,'yes') ; match(WAKE,'no') ).

process_intent('qmark',S,R):-
	remove_wake_words(S,W),
	remove_qmark(W,[W1,W2,W3]),
	( fact(W2,W1,W3) -> appendstrings(['Yes, I know the fact that ',W2,W1,W3],R) ; R='I am sorry, I do not have sufficent information to provide answer for this.' ).

process_intent('insert',S,R):-
	( skip_till('that',S,W) -> (check_if(W) -> R='I am sorry, I am still learning to remember "if then" facts, I can not remember this sentense now .' ; process_insert(W,R)) ; R='I do not understand it, try using "that" in your input.'), !.

process_intent('retract',S,R):-
	( skip_till('that',S,W) -> process_retract(W,R) ; R='If you want me to delete any facts, use "that" in your message' ), !.

process_intent('describe',S,R):-
	(skip_till('about',S,[NOUN]) -> (fact(NOUN,X,Y) -> appendstrings([NOUN,X,Y],R) ; appendstrings(['I am sorry I do not have any information related to ',NOUN],R) ) ; skip_till('describe',S,[NOUN]) -> (fact(NOUN,X,Y) -> appendstrings([NOUN,X,Y],R) ; appendstrings(['I am sorry I do not have any information related to ',NOUN],R) ) ), !.

process_intent('reason',S,R):-
	(skip_till('that',S,W) -> process_prove(W,R) ; R='I do not understand it, try using "that" in your input.' ), !.

process_intent(I,_,R):-
	R=I.

check_if([W1|_]):- W1='if'.

skip_till(WORD,[H|T],WORDS):-
	H=WORD -> WORDS=T ; skip_till(WORD,T,WORDS).

process_insert([W1,W2,W3],RES):-
	noun1(W1) -> check_consistency(W1,W2,W3,RES) ; assert(noun1(W1)), assert(fact(W1,W2,W3)), RES='okay, I got it.'.

process_retract([W1,W2,W3],RES):-
	fact(W1,W2,W3) -> (retract(fact(W1,W2,W3)),RES='okay, I have deleted that fact from my database.') ; (RES='I am sorry, I do not have any such information.').

process_prove([W1,W2,W3],R):-
	prove_helper(W1,W3,R), !.

prove_helper(W1,W3,R):- 
	derived_fact2(W1,_,W3,R1) -> convert_proof_verbal(R1,R2),appendstrings(['Yes, this can be proved with the following fact(s) ',R2],R) ; R='I am sorry, I do not have sufficient information to prove this'.

convert_proof_verbal([H],R):-
	fact_verbal(H,R).
convert_proof_verbal([H|T],R):-
	convert_proof_verbal(T,RT),
	fact_verbal(H,RH),
	appendstrings([RH,';',RT],R).

fact_verbal([W1,W2,W3],R):-
	appendstrings([W1,W2,W3],R).

derived_fact2(W1,_,W3,R):- fact(W1,X,W3),append([],[[W1,X,W3]],R).
derived_fact2(W1,_,W3,R):-
	fact(W1,X,W2),fact(W2,Y,W3),append([[W1,X,W2]],[[W2,Y,W3]],R).
derived_fact2(W1,_,W3,R):-
	fact(W1,X,W2),derived_fact2(W2,_,W3,R1),append([[W1,X,W2]],R1,R).

derived_fact(W1,_,W3):- fact(W1,_,W3).
derived_fact(W1,_,W3):-
	fact(W1,_,W2),fact(W2,_,W3).
derived_fact(W1,_,W3):-
	fact(W1,_,W2),derived_fact(W2,_,W3).

check_consistency(W1,W2,W3,RES):-
	(fact(W1,W2,X) -> (match_str(X,W3) -> RES='Yup, I already know that.' ; appendstrings(['but someone said that ',W1,W2,X],RES)) ; assert(fact(W1,W2,W3)), RES='okay, I got it.' ), !.

check_consistency2(W1,W2,W3,RES):-
	fact(W1,X,Y),write('Test Test'),nl,write(X),nl,write(W2),nl,
	( match_str(X,W2) -> ( write('Test'),nl,match_str(Y,W3) -> RES='Yup, I already knew that!' ; appendstrings(['but someone said that ',W1,X,Y],RES)) ; ( match_str(Y,W3) -> appendstrings(['but someone said that ',W1,X,Y],RES) ; assert(fact(W1,W2,W3)), RES='okay, I got it.') ).

appendstrings([],'').
appendstrings([H|T],RES):-
	appendstrings(T,R),
	atom_codes(R,C22),
	append(C22,[32],C2),
	atom_codes(H,C11),
	append(C11,[32],C1),
	append(C1,C2,C3),
	atom_codes(RES,C3).

find_intent([W1,W2|T],INTENT):-
	is_qmark([W1,W2|T],INTE),
	( INTE = 'qmark' -> match(I,INTE) ; intent(W1,I) ),
	( I='unknown' -> intent2(W1,W2,INTENT) ; INTENT = I ).

is_qmark(WORDS,I):-
	last_word(WORDS,WORD),
	atom_codes(WORD,CODES),
	last_code(CODES,CODE),
	( CODE = 63 -> match(I,'qmark') ; match(I,'unknown') ).

intent('how','question'):- !.
intent('what','question'):- !.
intent('where','question'):- !.
intent('when','question'):- !.
intent('why','question'):- !.
intent('remember','insert'):- !.
intent('forget','retract'):- !.
intent('delete','retract'):- !.
intent('prove','reason'):- !.
intent('describe','describe'):- !.
intent('tell','describe'):- !.
intent(_,'unknown'):- !.

intent2('can','you','question'):- !.
intent2('do','you','question'):- !.
intent2(_,_,'unknown'):- !.

process_intent(INTENT,RES):-
	match(INTENT,RES).

remove_qmark([W1,W2,W3],[W1,W2,W4]):-
	atom_codes(W3,C3),
	remove_last_code(C3,C),
	atom_codes(W4,C).

remove_last_code([H],[]).
remove_last_code([H|T],C):-
	remove_last_code(T,C1),
	append([H],C1,C).

match(X,X).
match_str(S1,S2):-
	atom_codes(S1,C1),
	atom_codes(S2,C2),
	C1=C2.

last_word([H],H):- !.
last_word([H|T],W):-
	!, last_word(T,W).

last_code([H],H):- !.
last_code([H|T],C):-
	!, last_code(T,C).

remove_wake_words([W1,W2|W3],W):-
	( W1='alexa,' -> append([W2],W3,W) ; match(W3,W) ).

split(STR,LIST):-
	atom_codes(STR,CODES),
	convert(CODES,LIST).

convert(C,L):-
	convert_helper(C,L1,L2),
	reverse(L2,L3,[]),
	append([L1],L3,W),
	to_word(W,L).

convert_helper([],[],[]).
convert_helper([C1|C2],LIST,WORD_LIST):-
	convert_helper(C2,L,W),
	( C1=32 -> append(W,[L],WORD_LIST),append([],[],LIST) ; append([C1],L,LIST),append([],W,WORD_LIST)).

to_word([],[]).
to_word([W1|W2],L):-
	to_word(W2,L2),
	atom_codes(W,W1),
	append([W],L2,L).

append([],L,L).
append([H|T],M,[H|N]):-
	append(T,M,N).

 reverse([],L,L).
 reverse([H|T],L,Acc) :- 
 	reverse(T,L,[H|Acc]).