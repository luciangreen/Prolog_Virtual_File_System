% convert List Prolog to VFS
%:-include('../listprologinterpreter/listprolog.pl').
vfs(P0,P1s):-findall(P1,(%
%test(_,Q,P,R),
member(P,P0),
subterm_wa([[n,_]|_],P,
%[[[n,a],":-",[[[n,write],[[v,c],[v,d]]]]]],
Insts1),
findall(Item,(member([Add,[[n,B]|Rest]],Insts1),
((fio_command(B),Rest=[[_,_]])->(atom_concat(B,'_fio_vfs',B1),Rest1=Rest);
(

((Rest=[Args,":-",Lines]->true;Rest=[Args,"->",Lines])->true;
(Rest=[Args]->Lines=[];
(((Rest=[":-",Lines]->true;Rest=["->",Lines]))->Args=[];
(Rest=[],Lines=[],Args=[])))),

subterm_wa([[n,_]|_],Lines,Insts10),
findall(Item2,(member([Add0,[[n,B0]|Args0]],Insts10),
convert_f(Args0,B0,B10),

(Args0=[]->Item2=[Add0,[[n,B10]]];Item2=[Add0,[[n,B10]|Args0]])

),Insts2),
foldr(put_sub_term_wa_ae,Insts2, Lines, Lines1),
replace_term(Rest,Lines,Lines1,Rest1)),
convert_f(Args,B,B1)),
((Rest1=[]->Item=[Add,[[n,B1]]];Item=[Add,[[n,B1]|Rest1]]))
),Insts21),

foldr(put_sub_term_wa_ae,Insts21, P,
%[[[n,a],":-",[[[n,write],[[v,c],[v,d]]]]]],
 P1)),P1s)%,

%term_to_atom(P1s,P1s1)%,writeln(P1s1),!
.


fio_command(open).
fio_command(write).
fio_command(read).

current_predicate1(F/Arity) :-
(string(F)->true;current_predicate(F,Arity)),!.

% is Rest=[[_,_] correct, whether Args is _

convert_f(_,A,B) :- (A=''->B='';(A=','->B=',';(is_punct(A)->B=A)
)),!.
convert_f(Args0,B0,B10) :-
length(Args0,Arity0),(%not(Arity0=0),
atom_concat(A,_,B0),atom_length(A,1),atom_number(A,A1),number(A1)->true; current_predicate1(B0/Arity0))->
B0=B10;(length(Args0,Arity0),not(Arity0=0),atom_concat(B0,'_vfs',B10)),!.
convert_f(_,A,A) :-!.

write_vfs_s(FZ,write,T11) :-
	vfs(VFS),
	retractall(vfs(_)),
	append(VFS,[[FZ,T11]],VFS2),
	assertz(vfs(VFS2)),!.
	
	
	
% Convert regular Prolog code to VFS-compatible code
vfs2(Input, Output) :-
    convert_predicates(Input, Output).

% Convert predicates recursively
convert_predicates(Var, Var) :- var(Var), !.
convert_predicates([], []) :- !.
convert_predicates(Term, ConvertedTerm) :-
    %Term =.. [Name|Args],
    %(not(Args=[])%convert_f(Args, Name, NewName) 
    %->
        convert_predicates2(Term,ConvertedTerm)%,%maplist(convert_predicates, Args, NewArgs),
        %ConvertedTerm =.. [NewName|NewArgs]
    %; %maplist(convert_predicates, Args, NewArgs),
       % ConvertedTerm =.. [Name|NewArgs]
    %)
    .
	
%/*	
convert_predicates2(A,B) :-
	A=..[B0|Args0],
	convert_f(Args0,B0,B10),
	        findall(X,(member(X1,Args0),convert_predicates(X1,X)),X2),

	B=..[B10|X2],!.
convert_predicates2(A,A) :- !.
%*/