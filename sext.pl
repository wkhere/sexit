%%-*-prolog-*-
%%
%% Seductively Efficient Xenophilic Translator [of objective-c to ruby]
%% (c) 2012 Wojciech Kaczmarek frk@wisdio.com
%%
:- use_module(library(dcg/basics)).

%% util

+dl(Xs-Ys, Ys-Zs, Xs-Zs).

list_to_dl(S,DL-E) :- append(S,E,DL).

empty_dl(H-T) :- unify_with_occurs_check(H,T).

%% join list of code_lists using separator:
join(_Sep, [H], H).
join( Sep, [H|T], Acc) :-
    join(Sep,T,S), append(H,Sep, S1), append(S1,S, Acc).

append_nl(L, Acc) :- append(L, "\n", Acc).

%% lexer

dquote --> "\"".

eat(C, [C|Xs]) --> C, eat(C, Xs).
eat(C, [C]) --> C.

split([X|Xs]) --> nonblanks(X), {X\=[]}, whites, split(Xs).
split([]) --> [].

split_with1(Delimiter, [X|Xs]) -->
    string_without(Delimiter,X), {X\=[]}, Delimiter,
    split_with1(Delimiter, Xs).
split_with1(Delimiter, [LastX]) -->
    string_without(Delimiter,LastX), {LastX\=[]}.
split_with1(_Delimiter, []) --> [].

const(const(num(V))) --> number(V).
const(const(str(S))) --> dquote, string_without("\"",S), dquote.

ident_c1(C) --> [C], {code_type(C, alpha)}.
ident_c1('_') --> "_".

ident_c(C) --> [C], {code_type(C, alnum)}.
ident_c('_') --> "_".

ident([C]) --> ident_c1(C).     % this way ident can be '_' - bit ugly
ident([H|T]) --> ident_c1(H), ident2(T).
ident2([C]) --> ident_c(C).
ident2([H|T]) --> ident_c(H), ident2(T).

blanks1 --> blank, blanks.

%% parser

var(ident(V)) --> ident(V).

meth_arg(arg(name(N),noval)) --> ident(N).
meth_arg(arg(name(N),val(V))) --> ident(N), ":", blanks, exp(V).

meth([A]) --> meth_arg(A).
meth([H|T]) --> meth_arg(H), blanks1, meth(T).

msg(msg(rcv(X), meth(M))) -->
    "[", exp(X), {X\=[]}, blanks1, meth(M), blanks, "]".

attr_var([X]) --> var(X).
attr_var([H|T]) --> var(H), ".", attr_var(T).

exp(V) --> const(V).
exp(V) --> var(V).
exp(V) --> msg(V).
exp(attr(V)) --> attr_var(V).
exp(V) --> "(", exp(V), ")".
exp(V) --> blank, blanks, exp(V).

stmt(msg_stmt(V)) --> msg(V), ";".
stmt(asgn(dest(D),val(V))) -->
    attr_var(D), blanks, "=", exp(V), blanks, ";".


%% translator


foldl_pred_example((N,SV), ([],[]), ([N],[SV])).
foldl_pred_example((N,SV), ([H1|T1],[H2|T2]), ( [H1|[N|T1]], [H2|[SV|T2]] ) ).
%% usage: foldl(foldl_pred_example,  [(1,a), (2,b)],  ([],[]),  V).

fold_args((N,SV), "", Acc) :-
    swritef(Buf, '%s:%s', [N,SV]),
    string_to_list(Buf, Acc).
fold_args((N,SV), L0, Acc) :-
    swritef(Buf, ', %s:%s', [N,SV]),
    string_to_list(Buf, L1), append(L0,L1, Acc).

trans(const(C), S0, SAll) :-
    C =.. [_Type, V],
    swritef(S, '%t', [V]), string_to_list(S,L),
    append(S0, L, SAll).
trans(ident(V), S0, SAll) :-
    append(S0,V,SAll).
trans(attr(L), S0, SAll) :-
    findall(I, member(ident(I),L), Is),
    join(".", Is, S),
    append(S0, S, SAll).

trans(asgn(dest(D), val(V)), L0, Acc) :-
    trans(attr(D), "", LD),
    trans(V, "", LV),
    append(L0, LD, L1), append(L1, " = ", L2), append(L2, LV, Acc).

trans(msg_stmt(Msg), S0, SAll) :-
    trans(Msg, S0, SAll).

trans(msg(rcv(Rcv), meth( [arg(name(MethName),noval)] )), S0, SAll) :-
    %% arity0 case
    trans(Rcv, "", SRcv),
    swritef(S, '%s.%s', [SRcv, MethName]),
    string_to_list(S,L),
    append(S0, L, SAll).
trans(msg(rcv(Rcv), meth(L)), S0, SAll) :-
    %% arity>0 case; please note that while parser currently allows bad objc form where 
    %% noval method arg is at any position (like "[rcv foo:bar baz]"),
    %% we do not catch such case here so it will not be translated
    L=[LH|LT],
    LH= arg(name(MethName),val(Val1)),
    trans(Rcv, "", SRcv),
    trans(Val1, "", SVal1),
    ( LT==[] ->
      swritef(S, '%s.%s(%s)', [SRcv, MethName, SVal1])
    ; 
      findall((N,SV), (member(arg(name(N),val(V)), LT), trans(V,"",SV)), RestArgs),
      foldl(fold_args, RestArgs, "", SVals2Plus),
      swritef(S, '%s.%s(%s, %s)', [SRcv, MethName, SVal1, SVals2Plus])
    ),
    string_to_list(S,L1),
    append(S0, L1, SAll).

%% test

ex1(S) :- S="[Foo alloc]".
ex2(S) :- S="[[PhotoPickerController alloc] init]".
ex3(S) :- S="[[[PhotoPickerController alloc] initWithDelegate:self] autorelease];".
ex4(S) :- S="self.photoPickerController = [[[PhotoPickerController alloc] initWithDelegate:self] autorelease];".

:- begin_tests(messages).

test(arity0_message, [nondet]) :-
    phrase(exp(
               msg(rcv(_),
                   meth( [ arg(name("alloc"), noval) ] ))
              ),
           "[foo alloc]").
test(arity1_message, [nondet]) :-
    phrase(exp(msg(_,_)),
           "[foo bar:42]").
test(arity2_message, [nondet]) :-
    phrase(exp(msg(_,_)),
           "[foo bar:42 quux:23]").
test(message_rcv_nesting, [nondet]) :-
    phrase(exp(
               msg(rcv(msg(rcv(ident("foo")), meth( [ arg(_, noval) ] ))),
                   meth(_)
                  )),
           "[[foo bar] quux:1 boo: 2]").
test(message_meth_arg_nesting, [nondet]) :-
    phrase(exp(
               msg(rcv(ident("foo")),
                   meth([
                         arg(name("quux"), val( msg(_,_) )),
                         arg(name("zzz"),  val( msg(_,_) ))
                        ])
                  )),
           "[foo quux: [bar foo] zzz:[xxx yyy]]").
test(ex1, [nondet]) :- ex1(S), phrase(exp(msg(_,_)), S).
test(ex2, [nondet]) :- ex2(S), phrase(exp(msg(_,_)), S).
test(ex3, [nondet]) :- ex3(S), phrase(stmt(msg_stmt(msg(_,_))), S).
test(ex4, [nondet]) :- ex4(S), phrase(stmt(asgn(_,_)), S).
:- end_tests(messages).

:- begin_tests(attrs).

test(attr1, [nondet]) :-
    phrase(exp(attr(_)),
           "foo.bar").
test(attr2, [nondet]) :-
    phrase(exp(attr(L)),
           "foo.bar.quux"),
    nth1(2, L, ident("bar")).
:- end_tests(attrs).

:- begin_tests(corner_cases).

test(disallow_empty_ident, [fail]) :-
    phrase(ident(_), "").
test(disallow_ident_ending_with_blank, [fail]) :-
    phrase(ident(_), "foo ").
test(disallow_empty_var, [fail]) :-
    phrase(exp(var(_)), "").
test(disallow_empty_method_name, [fail]) :-
    phrase(exp(msg(_, meth([ arg(name([]),_) ]))), "[foo ]").
test(disallow_empty_rcv, [fail]) :-
    phrase(exp(msg(_,_)), "[foo]").
test(allow_1char_idents, [nondet]) :-
    phrase(ident("x"), "x").
test(disallow_digit_at_start_of_ident, [fail]) :-
    phrase(ident(_), "1foo").
test(allow_blanks_before_exp, [nondet]) :-
    phrase(exp(msg(rcv(msg(_,_)), _)), " [  [foo z] x ]").
test(method_arg_in_parens, [nondet]) :-
    phrase(exp(msg(_, meth([arg(_, val(const(num(42))))]))),
           "[foo quux:(42)]").
test(method_with_nonzero_arity_should_have_all_args_with_values, [fail, fixme(in_the_future)]) :-
    phrase(exp(msg(_,_)),
           "[[obiekt dupa] bla foo: bar xxx]").
:- end_tests(corner_cases).

:- begin_tests(trans).
test(trans_var, [nondet]) :-
    S="foo",
    phrase(exp(P), S), trans(P,"",S).
test(trans_int_const, [nondet]) :-
    S="42", 
    phrase(exp(P), S), trans(P,"",S).
test(trans_attr, [nondet]) :-
    S="foo.bar.quux", 
    phrase(exp(P), S), trans(P,"",S).
test(trans_arity0_meth, [nondet]) :-
    S="[xxx zzz]", 
    phrase(exp(P), S), trans(P,"", S2),
    S2="xxx.zzz".
test(trans_arity0_meth_nested_receiver, [nondet]) :-
    S="[[xxx yyy] zzz]", 
    phrase(exp(P), S), trans(P,"", S2),
    S2="xxx.yyy.zzz".
test(trans_arity1_meth, [nondet]) :-
    S="[xxx yyy:42]",
    phrase(exp(P), S), trans(P,"", S2),
    S2="xxx.yyy(42)".
test(trans_arity1_meth_nested_receiver, [nondet]) :-
    S="[[xxx yyy] zzz:42]", 
    phrase(exp(P), S), trans(P,"", S2),
    S2="xxx.yyy.zzz(42)".
test(trans_arity1_meth_nested_arg, [nondet]) :-
    S="[xxx yyy:[bar foo]]",
    phrase(exp(P), S), trans(P,"", S2),
    S2="xxx.yyy(bar.foo)".
test(trans_arity2_meth, [nondet]) :-
    S="[foo bar:1 baz:2]",
    phrase(exp(P), S), trans(P,"", S2),
    S2="foo.bar(1, baz:2)".
test(trans_arity2_meth_nested_all_args, [nondet]) :-
    S="[foo bar:[x y:1] baz:[a b:2]]",
    phrase(exp(P), S), trans(P,"", S2),
    S2="foo.bar(x.y(1), baz:a.b(2))".
test(trans_arity3_meth_nested_all_args, [nondet]) :-
    S="[foo bar:[x y:1] baz:[a b:2] gdc:[foo x:1]]",
    phrase(exp(P), S), trans(P,"", S2),
    S2="foo.bar(x.y(1), baz:a.b(2), gdc:foo.x(1))".
test(trans_ex1, [nondet]) :-
    ex1(S),
    phrase(exp(P), S), trans(P,"", S2),
    S2="Foo.alloc".
test(trans_ex2, [nondet]) :-
    ex2(S),
    phrase(exp(P), S), trans(P,"", S2),
    S2="PhotoPickerController.alloc.init".
test(trans_ex3, [nondet]) :-
    ex3(S),
    phrase(stmt(P), S), trans(P,"", S2),
    S2="PhotoPickerController.alloc.initWithDelegate(self).autorelease".
test(trans_ex4, [nondet]) :-
    ex4(S),
    phrase(stmt(P), S), trans(P,"", S2),
    S2="self.photoPickerController = PhotoPickerController.alloc.initWithDelegate(self).autorelease".
:- end_tests(trans).

%% run

repl :- run_tests.
start :- true.
main(_) :- start.
