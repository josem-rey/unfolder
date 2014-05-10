:- module('unfolding',[rule/6,fact/6,hiddenfact/6,unfolding_operator/0, show_int/0,clean/0,unfold_rule/2,match/3,constructors_uppercase/2,constructors_uppercase_list/2,vars2letters/1,pretty_fact/2,reclean/1,lesser/2,remove_overlappings/1,eval_pf/3,eval/3]).
:- use_module(library(terms_check)). %% variant/2
:- use_module(library(write)). %% numbervars/3
:- use_module(library(terms_vars)). %% varset/2


:- dynamic fact/6, hiddenfact/6.
:- op(700,yfx,[@]). %% Partial and higher order applications
:- op(675,yfx,[:]). %% Adition of a new argument to a partial application

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Usage:
%% ?- [unfolding].
%%
%% yes
%% ?- unfolding_operator.
%%
%% yes
%% ?- show_int.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% removable_fact(Fact,Flag) removes redundant or useless facts. It has a flag
%% that can take the value 'variant' or 'novariant'. The first value is used 
%% when removing redundant facts after applying the unfolding operator since
%% we dont want a fact to be removed because it is compared to itself (every
%% fact is a variant of itself). The second value ('novariant') is used after
%% the unfolding operator generates new facts to prevent repeated facts from
%% entering the new interpretation since the old facts are not reviewd in this
%% process.
%%
%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RULES AND FACTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% rule(<function_name>,<pattern>,<guard>,<body>,<where>,<store>)
%%    <store> records the name of the rule
%% fact(<function_name>,<pattern>,<guard>,<body>,<where>,<store>) 
%%    <where> is a list of as 'matchings' p(Pattern,Exp)
%%    <store> records the list of rules applied to generate the fact
%% hiddenfact(<function_name>,<pattern>,_,bot,[],<store>)
%% is a list of facts that have been previously erased by removable_fact/2
%% but need to be 'reinjected' as an extended interpretation for the following
%% unfolding step 

% rule(even,[zero],_,true,[],[]).
% rule(even,[suc(zero)],_,false,[],[]).
% rule(even,[suc(suc(N))],_,even(N),[],[]).
% rule(main15,[P,N],_,even(suc(P@[N])),[],[]).
% rule(main15,[P,N],_,true,[p(P@[N],zero)],[]).
% rule(main15,[P,N],_,false,[p(P@[N],suc(zero))],[]).
% rule(main15,[P,N],_,even(Y),[p(P@[N],suc(suc(Y)))],[]).

% rule(add,[X,Y], flexeq(X,zero),Y,[],[]).
% rule(add,[X,Y],flexeq(X,suc(X2)),suc(add(X2,Y)),[],[]).
% rule(mul,[X,_], flexeq(X,zero),zero,[],[]).
% rule(mul,[X,Y],flexeq(X,suc(X2)),add(mul(X2,Y),Y),[],[]).

% rule(rev,[nil],_,nil,[],[rule('R1')]).
% rule(rev,[cons(X,Xs)],_,append(rev(Xs),cons(X,nil)),[],[rule('R2')]).
% rule(pal,[X],flexeq(X,rev(X)),true,[],[rule('P1')]).
% rule(append,[nil,X],_,X,[],[rule('A1')]).
% rule(append,[cons(X,Xs),Ys],_,cons(X,append(Xs,Ys)),[],[rule('A2')]).
% rule(main22,[L],_,rev(L),[],[rule('M22')]).

% rule(app,[nil,X],_,X,[],[]).
% rule(app,[cons(X,Xs),Y],_,cons(X,app(Xs,Y)),[],[]).
% rule(index,[P,cons(L,_Ls)],flexeq(P,zero),L,[],[]).
% rule(index,[P,cons(_L,Ls)],flexeq(P,suc(N)),index(N,Ls),[],[]).

% rule(ones,[],_,cons(1,ones),[],[]).
% rule(first,[cons(X,_Xs)],_,X,[],[]).
% rule(main,[],_,first(ones),[],[]).

% rule(quad,[X],_,double(X)+double(X),[],[]).
% rule(main2,[X],_,quad(X),[],[]).
% rule(f,[2],_,3,[],[]).
% rule(main3,[X],_,quad(f(X)),[],[]).
% double(X,Y):- Y is X*2.

% rule(hd,[cons(X,_Xs)],_,X,[],[rule('H1')]).
% rule(loop,[],_,loop,[],[rule('L1')]).
% rule(main4,[],_,hd(cons(1,loop)),[],[rule('M1')]).

% rule(ones,[],_,cons(1,ones),[],[]).
% rule(take_n,[zero,_],_,nil,[],[]).
% rule(take_n,[suc(N),cons(X,Xs)],_,cons(X,take_n(N,Xs)),[],[]).
% rule(main5,[],_,take_n(suc(suc(zero)),ones),[],[]).

% rule(map,[_F,nil],_,nil,[],[]).
% rule(map,[F,cons(X,Xs)],_,cons(F@[X],map(F,Xs)),[],[]).
% rule(g,[1],_,20,[],[]).
% rule(h,[2,N],_,N+1,[],[]).
% % rule(main6,[X],_,map(g,X),[],[]).
% rule(main6b,[X],_,map(h@[2],X),[],[]).

% rule(uncurry,[F,X],_,F@[X],[],[]).
% rule(h,[2],_,37,[],[]).
% rule(main7,[X],_,uncurry(h,X),[],[]).

% rule(app,[F,X],_,F@[X],[],[]). 
% rule(j,[2,5],_,7,[],[]).       
% rule(main8,[X],_,(j@[2]):X,[],[]).

% rule(app,[F,X],_,F@[X],[],[]).
% rule(k,[2,2],_,4,[],[]).
% rule(main9,[X],_,app(app(k@[],X),X),[],[]).
% rule(main10,[X],_,app(k@[],X),[],[]).
% rule(main11,[],_,k@[2],[],[]).

% rule(f2,[2],_,3,[],[]).
% rule(main12,[],_,f2(1+1),[],[]).

% rule(addr,[zero,N],_,N,[],[]).
% rule(addr,[suc(M),N],_,suc(addr(M,N)),[],[]).
% rule(addl,[zero,N],_,N,[],[]).
% rule(addl,[suc(M),N],_,addl(M,suc(N)),[],[]).

% rule(ack,[zero,N],_,suc(N),[],[]).
% rule(ack,[suc(M),zero],_,ack(M,suc(zero)),[],[]).
% rule(ack,[suc(M),suc(N)],_,ack(M,ack(suc(M),N)),[],[]).

% divide(X,Y,Answer):-
%  \+var(X),\+var(Y),!,
%  N is Y mod X,
%  (N=0 -> Answer=true;Answer=false).
% divide(X,Y,divide(X,Y)).
% rule(ite,[true,T,_E],_,T,[],[]).
% rule(ite,[false,_T,E],_,E,[],[]).
% rule(gen,[N],_,cons(N,gen(N+1)),[],[]).
% rule(first,[suc(zero),cons(X,_)],_,cons(X,nil),[],[]).
% rule(first,[suc(suc(N)),cons(X,L)],_,cons(X,first(suc(N),L)),[],[]).
% rule(pfilter,[_P,nil],_,nil,[],[]).
% rule(pfilter,[P,cons(X,R)],_,ite(P@[X],pfilter(P,R),cons(X,pfilter(P,R))),[],[]).
% rule(pfilter,[P,cons(X,R)],P:X,pfilter(P,R),[],[]).
% rule(pfilter,[P,cons(X,R)],not(P:X),cons(X,pfilter(P,R)),[],[]).
% rule(divideFS,[X,Y],_,divide(X,Y),[],[]).
% rule(sieve,[cons(X,R)],_,cons(X,sieve(pfilter(divideFS@[X],R))),[],[]).
% rule(main13,[],_,first(suc(suc(zero)),pfilter(divideFS@[2],gen(2))),[],[]).
% rule(main13,[],_,sieve(cons(2,nil)),[],[]).
% rule(main13,[],_,gen(2),[],[]).
% rule(main13,[],_,pfilter(divideFS@[2],gen(2)),[],[]).
% rule(main13,[],_,first(suc(suc(suc(zero))),sieve(gen(2))),[],[]).
% rule(main13,[],_,sieve(cons(2,cons(3,cons(4,bot)))),[],[]).
% rule(main13,[],_,first(suc(suc(suc(zero))),gen(2)),[],[]).
% rule(main13,[],_,sieve(gen(2)),[],[]).
% rule(main13,[],_,first(suc(suc(suc(zero))),cons(1,cons(3,bot))),[],[]).
% rule(main13,[],_,first(suc(suc(suc(zero))),cons(2,bot)),[],[]).

% rule(insert,[empty,N],_,node(empty,N,empty),[],[]).
% rule(insert,[node(Left,X,Right),N],_,ite(N<X,node(insert(Left,N),X,Right),node(Left,X,insert(Right,N))),[],[]).
% rule(ite,[true,T,_E],_,T,[],[]).
% rule(ite,[false,_T,E],_,E,[],[]).
% rule(main18,[P,X],_,ite(P@[X],1,2),[],[]).

% rule(coin,[],_,0,[],[]).
% rule(coin,[],_,1,[],[]).
% rule(twocoins,[],_,coin+coin,[],[]).

% rule(from_n,[N],_,cons(N,from_n(N+1)),[],[]).
% rule(first,[cons(X,_Xs)],_,X,[],[]).
% rule(app_first,[F,N],_,first(F@[N]),[],[]).
% rule(main14,[N],_,app_first(from_n@[],N),[],[]).

% rule(append,[nil,X],_,X,[],[]).
% rule(append,[cons(X,Xs),Y],_,cons(X,append(Xs,Y)),[],[]).
% rule(last,[cons(X,Xs)],flexeq(append(_,cons(B,nil)),cons(X,Xs)),B,[],[]).
% rule(main16,[],_,last(cons(1,cons(2,nil))),[],[]).

% rule(length,[nil],_,zero,[],[]).
% rule(length,[cons(_X,Xs)],_,suc(length(Xs)),[],[]).
% rule(main17,[X],_,length(X),[],[]).

% rule(f,[nil],_,f(nil),[],[]).
% rule(f,[cons(X,Xs)],_,cons(X,Xs),[],[]).

% rule(main18,[X],_,X+(1+1),[],[]).

% rule(m,[cons(1,cons(1,cons(1,_)))],_,1,[],[]).
% rule(ones,[],_,cons(1,ones),[],[]).
% rule(main19,[],_,m(ones),[],[]).

% rule(loop,[],_,loop,[],[]).
% rule(por,[true,_Y],_,true,[],[]).
% rule(por,[_X,true],_,true,[],[]).
% rule(por,[false,false],_,false,[],[]).
% rule(main20,[],_,por(loop,true),[],[]).

% rule(loop,[],_,loop,[],[]).
% rule(h,[_X,1,2],_,3,[],[]).
% rule(h,[0,_X,3],_,4,[],[]).
% rule(main21,[],_,h(0,loop,3),[],[]).

% rule(leq,[zero,_],_,true,[],[]).
% rule(leq,[suc(_),zero],_,false,[],[]).
% rule(leq,[suc(X),suc(Y)],_,leq(X,Y),[],[]).
% rule(g,[zero],_,zero,[],[]).
% rule(main22b,[X],_,leq(zero,g(X)),[],[]).
% rule(loop,[],_,loop,[],[]).
% rule(main22,[],_,leq(suc(zero),suc(loop)),[],[]).

% rule(ite,[true,T,_E],_,T,[],[]).
% rule(ite,[false,_T,E],_,E,[],[]).
% rule(loop,[],_,loop,[],[]).
% rule(invoca_loop,[_],_,loop,[],[]).
% rule(test,[F,N],_,ite(F@[N],1,0),[],[]).
% rule(main23,[],_,test(invoca_loop,0),[],[]).

% rule(addbad,[zero,_N],_,zero,[],[rule('AB1')]).
% rule(addbad,[suc(M),N],_,suc(addbad(M,N)),[],[rule('AB2')]).

%% Case proposed by reviewer 2 of FLOPS14. h does indeed have 2 facts and they
%% both are created in the same interpretation: unfold returns a set of facts,
%% not a single fact.
% rule(f,[0],_,0,[],[]).
% rule(f,[1],_,1,[],[]).
% rule(h,[X],_,f(X),[],[]).

%% Another example by the second reviewer in FLOPS14. Observe how the scope 
%% for g decreases from I1 to I2 because f is only partially defined. If all
%% the functions are totally defined, there is no problem.
% rule(f,[zero],_,zero,[],[]).
% rule(f,[X],neq(X,zero),bot,[],[]).
% rule(g,[X],_,suc(f(X)),[],[]).
% rule(h,[suc(_X)],_,suc(zero),[],[]).
% rule(h,[X],neq(X,suc(_)),bot,[],[]).
% rule(main30,[],_,h(g(suc(zero))),[],[]).

% rule(f,[zero],_,zero,[],[]).
% % rule(f,[suc(_)],_,zero,[],[]).
% rule(g,[X],_,suc(f(X)),[],[]).
% rule(h,[suc(_X)],_,suc(zero),[],[]).
% rule(main30b,[],_,h(g(suc(zero))),[],[]).

%% One more example, similar to that of main30, but that generates infinite
%% overlapping pairs. f is also incomplete, which causes g to 'shrink' from
%% one iteration to the next too...
% rule(f,[suc(zero)],_,zero,[],[]).
% rule(f,[suc(suc(X))],_,suc(f(X)),[],[]).
% rule(g,[X],_,suc(f(X)),[],[]).




%% Two examples taken from papers by Alpuente's team
% rule(test,[X,Y],_,h(f(X,g(Y))),[],[]).
% rule(f,[zero,zero],_,suc(f(zero,zero)),[],[]).
% rule(f,[suc(N),X],_,suc(f(N,X)),[],[]).
% % rule(f,[zero,suc(_)],_,ff,[],[]).
% rule(g,[zero],_,g(zero),[],[]).
% % rule(g,[suc(_)],_,ff,[],[]).
% rule(h,[suc(_)],_,zero,[],[]).
% % rule(h,[zero],_,ff,[],[]).
% rule(main31,[],_,test(suc(zero),zero),[],[]).

% rule(test,[X,Y],_,h(f(X,g(Y))),[],[rule('T1')]).
% rule(f,[zero,zero],_,zero,[],[rule('F1')]).
% rule(f,[suc(N),X],_,suc(f(N,X)),[],[rule('F2')]).
% rule(f,[zero,suc(_)],_,zero,[],[rule('F3')]).
% rule(g,[zero],_,g(zero),[],[rule('G1')]).
% rule(g,[suc(_)],_,zero,[],[rule('G2')]).
% rule(h,[suc(_X)],_,zero,[],[rule('H1')]).
% rule(main31,[],_,test(suc(zero),zero),[],[]).

% rule(berry,[_X,false,true],_,1,[],[]).
% rule(berry,[true,_Y,false],_,2,[],[]).
% rule(berry,[false,true,_Z],_,3,[],[]).
% rule(loop2,[X],_,loop2(X),[],[]).
% rule(main32,[X,Z],_,berry(X,loop2(false),Z),[],[]).

% rule(zip,[nil,nil],_,nil,[],[]).
% rule(zip,[cons(X,Xs),cons(Y,Ys)],_,cons(tup(X,Y),zip(Xs,Ys)),[],[]).
% rule(main33,[],_,zip(cons(1,nil),cons(11,cons(12,nil))),[],[]).

% rule(ziptup,[tup(nil,nil)],_,nil,[],[]).
% rule(ziptup,[tup(cons(X,Xs),cons(Y,Ys))],_,cons(tup(X,Y),ziptup(tup(Xs,Ys))),[],[]).

rule(ite,[true,T,_E],_,T,[],[rule('I1')]).
rule(ite,[false,_T,E],_,E,[],[rule('I2')]).
rule(gen,[N],_,cons(N,gen(N+1)),[],[rule('G1')]).
rule(senior,[Age],_,ite(Age>64,true,false),[],[rule('S1')]).
rule(map,[_F,nil],_,nil,[],[rule('M1')]).
rule(map,[F,cons(X,Xs)],_,cons(F@[X],map(F,Xs)),[],[rule('M2')]).
rule(main50,[],_,map(senior@[],gen(64)),[],[rule('MAIN')]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Syntactic SETS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pf([double,divide,ifthenelse,'+','-','<','>']). %% PF=Predefined Functions
cf([]). %% CF=Constraint Functions
cp([flexeq,eq]). %% CP=Constraint Predicates
lc([and,or,not]). %% LC=Logical Connectives

find_fs(Fs):- %% Returns the set of User-defined function names
  findall(F,rule(F,_,_,_,_,_),Fs).

is_fs(F):-find_fs(FSs),member(F,FSs),!.
is_pf(F):- pf(PFs),member(F,PFs),!.

is_dc(DC):- %% Returns whether DC is a Data Constructor or not
  pf(PFs),\+member(DC,PFs),
  cf(CFs),\+member(DC,CFs),
  cp(CPs),\+member(DC,CPs),
  lc(LCs),\+member(DC,LCs),
  find_fs(FSs),\+member(DC,FSs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Given a rule body or guard, this predicate returns whether that body or guard
%% has any symbol belonging to FS.

has_fs(Expr,Pos,F):-
  has_fs_aux(Expr,[],Pos,F).

has_fs_aux(Expr,Ac,AcPos,F):-
   \+var(Expr),
   Expr=..[FName|_FArgs],
   is_fs(FName),
   AcPos=Ac,F=Expr.
has_fs_aux(Expr,Ac,AcPos,F):-
   \+var(Expr),
   Expr=..[_FName|FArgs],
   has_fs_list(FArgs,1,Ac,AcPos,F).

has_fs_list([Arg|_Args],ArgPos,Ac,AcPos,F):-
  append(Ac,[ArgPos],AcPosAux),has_fs_aux(Arg,AcPosAux,AcPos,F).
has_fs_list([_Arg|Args],ArgPos,Ac,AcPos,F):-
    ArgPos1 is ArgPos+1,has_fs_list(Args,ArgPos1,Ac,AcPos,F).

%% Given an expression rooted by a symbol in FS, succeed if and only if that
%% expression is a full application
full_application(Exp):-
  \+var(Exp),
  Exp=..[F|Args],
  rule(F,Pat,_,_,_,_),!,
  length(Pat,PatLength),
  length(Args,ArgsLength),
  PatLength=ArgsLength.

%% The same as has_fs/3 but returning full applications only:
has_full_application(Expr,Pos,F):-
  has_fs(Expr,Pos,F),full_application(F).

%% Given an expression E, a position within the expression P, a term T
%% return E[T]|_P (that is, expression E where the subexpression at position P
%% has been replaced by T.
replace(_Expr,[],Term,Term). %% Replacement of root position.
replace(Expr,[P|Ps],Term,Expr2):-
  Expr=..[F|Args],
  get_nth_element(Args,P,NthArg),
  replace(NthArg,Ps,Term,NthArg2),
  replace_list(Args,P,NthArg2,Args2),
  Expr2=..[F|Args2].

%% Given a list L, a position in the list N (N>=1) and a term T, return the
%% same list L except that the N-th element has been replaced by T.
replace_list([_E|Es],1,T,[T|Es]):-!.
replace_list([E|Es],N,T,[E|Es2]):-
  N>1,
  N1 is N-1,
  replace_list(Es,N1,T,Es2).

%% Given a list L, a position in the list N (N>=1), return the N-th element
%% of the list
get_nth_element([E|_Es],1,E):-!.
get_nth_element([_|Es],N,E):-
  N1 is N-1,
  get_nth_element(Es,N1,E).

%% Given two terms T1 and T2, succeeds if and only if T1<=T2l
lesser(X,_):- \+var(X),X=bot,!. %% Bottom is the lowest element in this order
lesser(_,X):- \+var(X),X=bot,!,fail.
lesser(_,X):-var(X),!. %% Vars are the greatest element in the term order.
lesser(X,_):-var(X),!,fail.
lesser(T1,T2):-
  T1=..[C|Args1],T2=..[C|Args2],
  lesser_list(Args1,Args2).

lesser_list([],[]).
lesser_list([T1|T1s],[T2|T2s]):-
  lesser(T1,T2),lesser_list(T1s,T2s).

show_int:-
  findall(fact(F,H,G,B,W,S),fact(F,H,G,B,W,S),Facts),
  write_list(Facts,'*').

write_list([],_).
write_list([Fact|Facts],Prefix):-
  pretty_fact(Fact,Prefix),nl,write_list(Facts,Prefix).

%% Succeed if and only if a guard is a just the and of free variables:
guard_success(V):-var(V),!.
guard_success(and(G1,G2)):-
  guard_success(G1),guard_success(G2).

%% Change notation from Prolog to Haskell:
%% 1.- Constructors must start with a capital letter
%% 2.- Variables start with a lowercase letter.

constructors_uppercase(X,X):-var(X),!.
constructors_uppercase(N,N):-number(N),!.
constructors_uppercase(T,Tupper):-
  T=..[Constructor|Terms],
  is_dc(Constructor),
  name(Constructor,ConstructorList),
  ConstructorList=[L|Ls],
  (L>96 -> Lupper is L-32 ; Lupper=L), %% Only letters are uppercased
  name(ConstructorUpper,[Lupper|Ls]),
  constructors_uppercase_list(Terms,TermsUppercase),
  Tupper=..[ConstructorUpper|TermsUppercase].  
constructors_uppercase(T,Tupper):-
  T=..[Constructor|Terms],
  \+is_dc(Constructor),
  constructors_uppercase_list(Terms,TermsUppercase),
  Tupper=..[Constructor|TermsUppercase].  

constructors_uppercase_list([],[]).
constructors_uppercase_list([T|Ts],[Tu|Tus]):-
  constructors_uppercase(T,Tu),constructors_uppercase_list(Ts,Tus).

vars2letters(T):-
  varset(T,Tvars),
  vars2letters_aux(Tvars,98).

vars2letters_aux([],_).
vars2letters_aux([V|Vs],N):-
  name(V,[N]),
  N1 is N+1,
  vars2letters_aux(Vs,N1).

%% Pretty Print a fact:
pretty_fact(fact(Function,Pattern,Guard,Body,Where,Store),Prefix):-
  constructors_uppercase_list(Pattern,PatternU),
  constructors_uppercase(Body,BodyU),
  constructors_uppercase(Where,WhereU),
  Head=..[Function|PatternU],
  copy_term(Guard,GuardAux),
  (\+guard_success(GuardAux) -> 
    constructors_uppercase(Guard,GuardU)
  ;
    GuardU=Guard),
  vars2letters(dummy(PatternU,GuardU,BodyU,WhereU)),
  write(Prefix),write(' '),write(Head),
  ((\+guard_success(GuardAux) ; Where\=[]) -> write(' | ') ; true),
  ((\+guard_success(GuardAux),Where=[]) -> write(GuardU) ; true), %% G only
  ((\+guard_success(GuardAux),Where\=[]) -> write('and('),write(GuardU),write(','),pretty_where(WhereU),write(')') ; true) , % G,W
  ((guard_success(GuardAux),Where\=[]) -> pretty_where(WhereU) ; true), % W only
  write(' = '),
  write(BodyU),
  pretty_store(Store).

pretty_store([]).
pretty_store([S|Ss]):-
  write('  <'),pretty_store_aux([S|Ss]),write('>').

pretty_store_aux([]).
pretty_store_aux([rule(R)|Ss]):-!,
  write(R),
  (Ss\=[]->write(',');true),
  pretty_store_aux(Ss).

pretty_where([]).
pretty_where([W|Ws]):-
  W='P'(Pattern,Exp), %% constructors_uppercase will have changed this: p -> P
  write('snd(match('),write(Pattern),write(','),write(Exp),write('))'),
  (Ws=[]->true;write(',')),
  pretty_where(Ws).

%% Given an expression, search for any applications with shape f@[Args] and
%% replace full applications with f(Args).
adapt_partial_apps(Var,Var):-var(Var),!.
adapt_partial_apps(Expr,AdaptedExpr):-
  Expr=..['@'|Args],!, %% Partial Application
  Args=[F,ArgList],
  (var(F)->AdaptedExpr=Expr %% (app f x = (f x))
  ;
    rule(F,FPat,_,_,_,_),!,
    length(FPat,FArity),
    length(ArgList,LengthArgList),
    adapt_partial_apps_list(ArgList,AdaptedArgList),
    (FArity=LengthArgList -> % Full Application: F(AdaptedArgList) is returned
      AdaptedExpr=..[F|AdaptedArgList]
    ;   %% Still a partial app: F@AdaptedArgList is returned 
      AdaptedExpr=..['@',F,AdaptedArgList])).
adapt_partial_apps(Expr,AdaptedExpr):-
  Expr=..[F|Args],
  F\='@', %% Unnecessary due to the cut above
  adapt_partial_apps_list(Args,AdaptedArgs),
  AdaptedExpr=..[F|AdaptedArgs],!.
adapt_partial_apps(Expr,Expr).

adapt_partial_apps_list([],[]).
adapt_partial_apps_list([E|Es],[AdaptedE|AdaptedEs]):-
  adapt_partial_apps(E,AdaptedE),adapt_partial_apps_list(Es,AdaptedEs).

%% Given a expression, search it for subexpressions that add arguments to 
%% partial applications ((F@[X]):NewArg) and return that expression with the
%% new arguments added (F@[X,NewArg])

add_arguments(Var,Var):-var(Var),!.
add_arguments(Expr,ExprOut):- %% <func>:<Arg>
  \+var(Expr),
  Expr=..[':',PartialApp,NewArg],
  (var(PartialApp) ->
     ExprOut=..['@',PartialApp,[NewArg]]
   ;
     PartialApp=..['@',F,Args],
     append(Args,[NewArg],Args2),!,
     ExprOut=..['@',F,Args2]).
add_arguments(Expr,ExprOut):- %% F@[Args1]@[Args2] --> F@[Args1|Args2]
  \+var(Expr),
  Expr=..['@',FPlusArgs1,Args2],
  \+var(FPlusArgs1), %% I dont want to match F@[Args1]
  FPlusArgs1=..['@',F,Args1],!,
  append(Args1,Args2,Args12),
  add_arguments_list(Args12,Args12Out),
  ExprIter=..['@',F,Args12Out],
  (F=..['@',_,_] ->
    add_arguments(ExprIter,ExprOut)
  ;
    ExprOut=ExprIter).
    
add_arguments(Expr,ExprOut):-
  \+var(Expr),
  Expr=..[F|Args],
  F\=':',
  add_arguments_list(Args,ArgsOut),
  ExprOut=..[F|ArgsOut],!.
add_arguments(Expr,Expr).

add_arguments_list([],[]).
add_arguments_list([A|As],[AOut|AOuts]):-
  add_arguments(A,AOut),add_arguments_list(As,AOuts).

%% Given a pattern and an expression returns that very same expression 
%% with applications of predefined functions (PFs) replaced by different free 
%% vars and the list of pairs (Var,PF Application) and only if pattern does 
%% not already unify with expression 

remove_pfs(X,X,[]):-var(X),!.
remove_pfs(N,N,[]):-number(N),!.
remove_pfs(Exp,NewExp,Pairs):-
  Exp=..[DC|Args],DC\='@',is_dc(DC),!,
  remove_pfs_list(Args,NewArgs,Pairs),
  NewExp=..[DC|NewArgs].
remove_pfs(Exp,V,[p(V,Exp)]):-
  Exp=..[PF|_Args],(is_pf(PF);PF='@'),!.
remove_pfs(Exp,Exp,[]).

remove_pfs_list([],[],[]).
remove_pfs_list([Exp|Exps],[NewExp|NewExps],Pairs):-
  remove_pfs(Exp,NewExp,PairsExp),
  remove_pfs_list(Exps,NewExps,PairsExps),
  append(PairsExp,PairsExps,Pairs).

remove_pfs_pattern([],[],[],[]).
remove_pfs_pattern([P|Ps],[E|Es],[NewExp|NewExps],ProvThats):-
  (P=E -> (NewExp=E,PTAux=[]) ; remove_pfs(E,NewExp,PTAux)),
  remove_pfs_pattern(Ps,Es,NewExps,PTAux2),
  append(PTAux,PTAux2,ProvThats).

%% Match a pattern against expressions, generating any 'Provided that' 
%% condition that is necessary
match(Pattern,Exps,ProvidedThats):-
  remove_pfs_pattern(Pattern,Exps,NewExps,ProvidedThats),
  Pattern=NewExps.  
    





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

%% Given a rule, unfold it in all the ways possible until its body and guard
%% have no symbols belonging to FS.
unfold_rule(Rule,NewFact):-
  Rule=rule(RuleF,RulePattern,RuleGuard,RuleBodyPre,Where,Store),
  add_arguments(RuleBodyPre,AdaptedRuleBody),
  (adapt_partial_apps(AdaptedRuleBody,RuleBody)->true;RuleBody=AdaptedRuleBody),
  unfold_rule_aux(rule(RuleF,RulePattern,RuleGuard,RuleBody,Where,Store),NewFactAux),
  NewFactAux=fact(F,P,G,B,W,S),
  add_arguments(B,AdaptedB),
  (adapt_partial_apps(AdaptedB,AdaptedB2)->true;AdaptedB2=AdaptedB),
  eval(AdaptedB2,EvaluationG,AdaptedB3),
%%  eval(and(G,EvaluationG),EvaledG,ConstG), %%%% OJO
  NewFact=fact(F,P,and(G,EvaluationG),AdaptedB3,W,S).

unfold_rule_aux(rule(RuleF,RulePattern,RuleGuard,RuleBody,RuleWhere,RuleStore),NewFact):-
  \+has_full_application(dummy(RuleGuard,RuleBody),_Pos,_Exp),!,
  NewFact=fact(RuleF,RulePattern,RuleGuard,RuleBody,RuleWhere,RuleStore).
  %% A rule without full applications of FS is returned as it is.
unfold_rule_aux(rule(RuleF,RulePattern,RuleGuard,RuleBody,RuleWhere,RuleStore),NewFact):-
  has_full_application(dummy(RuleGuard,RuleBody),Pos,Exp),
  eval(Exp,GuardExp,EvaledExp),
  (guard_success(GuardExp)->GuardExp2=_ ; GuardExp2=GuardExp),
  EvaledExp=..[ExpF|ExpArgs],
  fact(ExpF,FactPattern,FactGuard,FactBody,FactWhere,FactStore),
  match(FactPattern,ExpArgs,MatchWheres),
  replace(dummy(RuleGuard,RuleBody),Pos,FactBody,dummy(NewFactGuard,NewFactBody)),
  GuardExp3=and(and(FactGuard,NewFactGuard),GuardExp2),
  append(RuleWhere,FactWhere,Where2),
  append(Where2,MatchWheres,Where3),
  append(RuleStore,FactStore,Store3),
  unfold_rule(rule(RuleF,RulePattern,GuardExp3,NewFactBody,Where3,Store3),NewFact).
unfold_rule_aux(rule(RuleF,RulePattern,RuleGuard,RuleBody,RuleWhere,RuleStore),NewFact):-
  has_full_application(dummy(RuleGuard,RuleBody),Pos,Exp),
  eval(Exp,_GuardExp,EvaledExp),
  EvaledExp=..[ExpF|ExpArgs],
  \+fact(ExpF,ExpArgs,_FactGuard,_FactBody,_FactWhere,_FactStore),!, %% No fact matches the rule
  replace(dummy(RuleGuard,RuleBody),Pos,bot,dummy(NewFactGuard,NewFactBody)),
  unfold_rule(rule(RuleF,RulePattern,NewFactGuard,NewFactBody,RuleWhere,RuleStore),NewFact).

%% Applies the unfolding operator to current interpretation  
unfolding_operator:-
  findall(hiddenfact(F,P,G,B,W,S),hiddenfact(F,P,G,B,W,S),HiddenFacts),
  assert_hidden_facts(HiddenFacts),
  findall(NewFact,(rule(Function,Head,Guard,Body,Where,Store),unfold_rule(rule(Function,Head,Guard,Body,Where,Store),NewFact)),NewFacts),
  assert_facts(NewFacts),
  clean.

%% Records a list of facts so they become part of the new interpretation:
assert_facts([]).
assert_facts([F|Fs]):- assert_fact(F),assert_facts(Fs).

%% Takes a list of hidden facts (those removable) and asserts them as regular
%% facts
assert_hidden_facts([]).
assert_hidden_facts([hiddenfact(F,P,G,B,W,S)|Fs]):-
  (fact(F,P,G,B,W,S)->true ; assert(fact(F,P,G,B,W,S))),
  retract(hiddenfact(F,P,G,B,W,S)), 
  assert_hidden_facts(Fs).

%% Records a single fact as a member of the new interpretation unless the new
%% fact is subsumed by an existing one. That happens if an existing fact 
%% fulfills the following conditions:
%% fact(F,NewH,NewG,NewB,NewW,NewS) is not asserted if some existing fact fact(F,H,G,B,W,S)
%% is such that:
%% - NewB is bottom or
%% - There exists a unificator mu between H and NewH and mu(NewB)<B.
%% - The last condition above should apply only when both guards are satisfiable
%%   at the same time but we cannot check that condition at this time since
%%   there is not any underlying solver.

assert_fact(fact(F,Hnew,Gnew,Bnew,Wnew,_Snew)):- %% Do not assert already existing facts.
  fact(F,Hf,Gf,Bf,Wf,_Sf),
  simplify_guard(Gnew,GnewSimp),
  simplify_guard(Wnew,WnewSimp),
  variant(fact(F,Hnew,GnewSimp,Bnew,WnewSimp,_),fact(F,Hf,Gf,Bf,Wf,_)),!.
assert_fact(Fact):-
  Fact=fact(F,H,G,B,W,S),
  simplify_guard(G,SimplifiedG),
  simplify_guard(W,SimplifiedW),
  (removable_fact(Fact,variant)->true;assert(fact(F,H,SimplifiedG,B,SimplifiedW,S))).

%% Succeeds if it is safe to assert the fact provided into the set of hidden
%% facts (i.e. if there is no hidden fact which is smaller than the one given)
is_safe_to_assert_hidden(fact(F,NewH,_NewG,_NewB,_NewW,_NewS)):-
  hiddenfact(F,Haux,_,bot,[],_),
  copy_term(NewH,NewHcopy),copy_term(Haux,Hauxcopy),
  NewHcopy=Hauxcopy,
  lesser_list(Haux,NewH),!,fail.
is_safe_to_assert_hidden(fact(_F,_NewH,_NewG,_NewB,_NewW,_NewS)).

removable_fact(fact(F,NewH,NewG,NewB,NewW,NewS),_):-
    \+var(NewB),
     NewB=bot,guard_success(NewG),NewW=[],!,
     simplify_guard(NewG,NewGsimp),
     (is_safe_to_assert_hidden(fact(F,NewH,NewGsimp,NewB,NewW,NewS))->
       assert(hiddenfact(F,NewH,NewGsimp,NewB,NewW,NewS))
     ;
       true).
removable_fact(fact(F,NewH,NewG,NewB,NewW,_NewS),VariantFlag):-
  fact(F,H,G,B,W,_S), %% Existing fact for the same function as the new one
  (VariantFlag=novariant -> 
      \+variant(fact(F,H,G,B,W,_S),fact(F,NewH,NewG,NewB,NewW,_NewS))
  ;
      true),
  H=NewH,
  % variant(H,NewH),
  variant(G,NewG), %% Until we have constraint solvers, we do a fake test
  variant(W,NewW), %% for guards
  lesser(NewB,B). %% NewB<=B.

clean:-
  findall(fact(F,H,G,B,W,S),fact(F,H,G,B,W,S),Facts),
  clean_aux(Facts).

clean_aux([]).
clean_aux([Fact|Facts]):-
  (removable_fact(Fact,novariant)->retract(Fact);true), 
  clean_aux(Facts).

%% Given a expression, detect whether it is made only of symbols that do not
%% need rules an evaluate it if that happens. Leave it untouched if that
%% criterion is not met.
%%
%% eval(+Expression,-Constraint,-Value)

eval(X,_,X):-var(X),!.
eval(N,_,N):-number(N),!.
eval(ifthenelse(If,Then,_Else),Constraint,EvaledIf):-
  eval(If,ConstraintIf,ValueIf),
  \+var(If),ValueIf=true,!,
  eval(Then,ConstraintThen,ValueThen),
  Constraint=and(ConstraintIf,ConstraintThen),
  EvaledIf=ValueThen.
eval(ifthenelse(If,_Then,Else),Constraint,EvaledIf):-
  eval(If,ConstraintIf,ValueIf),
  \+var(If),ValueIf=false,!,
  eval(Else,ConstraintElse,ValueElse),
  Constraint=and(ConstraintIf,ConstraintElse),
  EvaledIf=ValueElse.
eval(ifthenelse(If,Then,_Else),Constraint,EvaledIf):-
  eval(If,ConstraintIf,ValueIf),
  eval(Then,ConstraintThen,ValueThen),
  Constraint=and(and(ConstraintIf,eq(ValueIf,true)),ConstraintThen),
  EvaledIf=ValueThen.
eval(ifthenelse(If,_Then,Else),Constraint,EvaledIf):-
  eval(If,ConstraintIf,ValueIf),
  eval(Else,ConstraintElse,ValueElse),
  Constraint=and(and(ConstraintIf,eq(ValueIf,false)),ConstraintElse),
  EvaledIf=ValueElse.
eval(App,Constraint,EvaledApp):-
  App=..[RootSymbol|Args],
  RootSymbol\=ifthenelse,
  eval_list(Args,AndedConstraint,EvaledArgs),
  Aux=..[RootSymbol|EvaledArgs],
  (is_pf(RootSymbol)->
    eval_pf(Aux,PFConstraint,EvaledApp)
  ;
    (EvaledApp=Aux,PFConstraint=_)),
  Constraint=and(PFConstraint,AndedConstraint).

eval_list([],_,[]).
eval_list([E|Es],and(Constraint,Constraints),[E2|E2s]):-
  eval(E,Constraint,E2),eval_list(Es,Constraints,E2s).

%% Given a expression headed by a predefined function, evaluate it while
%% capturing exceptions: If an exception arises return the expression as it was

% eval_pf(Expr,PFConstraint,EvaledExpr):-
%   Expr=..[F|_Args],
%   member(F,['<','>']),!,
%   (catch(call(Expr),Error,_Res=Expr)->
%     (var(Error)->EvaledExpr=true;EvaledExpr=Expr)
%   ;
%     EvaledExpr=false
%   ),
%   PFConstraint=_.
eval_pf(Expr,PFConstraint,EvaledExpr):-
  Expr=..[F|Args],
  ( \+ member(F,['+','-','*','/','<','>']) 
    ->
    append(Args,[Res],ArgsRes),
    Expr2=..[F|ArgsRes],
    catch(call(Expr2),_,Res=Expr),
    EvaledExpr=Res
  ;
    catch(call(EvaledExpr is Expr),_,EvaledExpr=Expr)),
  PFConstraint=_.

%% Given a constraint, simplify it as much as possible (and(Success,X)=X):
simplify_guard(G,_):-guard_success(G),!.
simplify_guard(and(A,B),SimpB):-
  guard_success(A),!,simplify_guard(B,SimpB).
simplify_guard(and(A,B),SimpA):-
  guard_success(B),!,simplify_guard(A,SimpA).
simplify_guard(and(A,B),and(ASimp,BSimp)):-!, %% Neither A nor B are Success
  simplify_guard(A,ASimp),simplify_guard(B,BSimp).
simplify_guard(G,G).

%% Removes overlapping facts:
%% If Action is bound and unifies with 'yes', the interpretation is indeed
%% modified. Otherwise, overlapping sets are displayed but nothing is modified
%% inside current interpretation
reclean(Action):-
  findall(F,rule(F,_,_,_,_,_),FsWithDups),
  remove_dups(FsWithDups,Fs),
  reclean_aux(Fs,Action).

remove_dups([],[]).
remove_dups([X|Xs],Ys):-
  member(X,Xs),!,remove_dups(Xs,Ys).
remove_dups([X|Xs],[X|Ys]):-
  \+member(X,Xs),!,remove_dups(Xs,Ys).

%% Given a fact and alist of facts that overlap with the first and are more 
%% specific than the first, return a new fact that does not apply whenever any
%% of the overlapping does and applies whenever none of the overlappings does.
calculate_new_fact(Fact,Overlappings,NewFact):-
  calculate_new_fact_aux(Fact,Overlappings,Fact,NewFact).

calculate_new_fact_aux(_,[],AccumFact,AccumFact).
calculate_new_fact_aux(Fact,[Overlapping|Overlappings],AccumFact,NewFact):-
  Fact=fact(F,P,_G,_B,_W,_Seq),
  Overlapping=fact(F,OvP,OvG,_OvB,_OvW,_OvSeq),
  AccumFact=fact(F,AcP,AcG,AcB,AcW,AcSeq),
  NewGNunif=nunif(P,OvP),
  (guard_success(OvG)
   ->
   NewGOr=NewGNunif
   ;
   NewGOr=or(NewGNunif,not(OvG))),
  (guard_success(AcG)
   ->
   NewG=NewGOr
   ;
   NewG=and(AcG,NewGOr)),
  AccumFact2=fact(F,AcP,NewG,AcB,AcW,AcSeq),
  calculate_new_fact_aux(Fact,Overlappings,AccumFact2,NewFact).

%% Eliminates any fact within current interpretation which is overlapped by
%% some more specific fact an replaces it with a new fact that does not overlap
%% any fact within the interpretation.
remove_overlappings([]).
remove_overlappings([_Fact-[]|FactFactsList]):- %% A fact with no overlappings is untouched
  remove_overlappings(FactFactsList).
remove_overlappings([Fact-[Overlap|Overlaps]|FactFacts]):-
  calculate_new_fact(Fact,[Overlap|Overlaps],NewFact),
  retract(Fact),
  assert(NewFact),
  remove_overlappings(FactFacts).

reclean_aux([],_Action).
reclean_aux([F|Fs],Action):-
  reclean_f(F,FactFactList),
  (\+var(Action),Action=yes ->
    remove_overlappings(FactFactList)
  ;
    nl,prettyprint_factfactlist(FactFactList)),
  reclean_aux(Fs,Action).

%% Given two facts for the same function, returns whether the first one is more
%% specific than the second or not
f1_more_specific_than_f2(F1,F2):-
  F1=fact(F,Pattern1,_Guard1,_Body1,_Where1,_Id1),
  F2=fact(F,Pattern2,_Guard2,_Body2,_Where2,_Id2),
  \+variant(Pattern1,Pattern2),
  lesser(Pattern1,Pattern2).

%% Given a fact, returns the list of facts that are more specific than 
%% the one given
facts_more_specific_than(Fact,FactList):-
  Fact=fact(F,_,_,_,_,_),
  findall(fact(F,P,G,B,W,I),(fact(F,P,G,B,W,I),f1_more_specific_than_f2(fact(F,P,G,B,W,I),Fact)),FactList).

reclean_f(F,FactsFactList):-
  findall(fact(F,P,G,B,W,I)-FactList,(fact(F,P,G,B,W,I),facts_more_specific_than(fact(F,P,G,B,W,I),FactList)),FactsFactList).

prettyprint_factfactlist([]).
prettyprint_factfactlist([Fact-FactList|Fs]):-
  pretty_fact(Fact,'*'),nl,
  write_list(FactList,'***'),
  prettyprint_factfactlist(Fs).
