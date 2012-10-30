%%-*-prolog-*-
%%
%% Seductively Efficient Xenophobic Translator [of objective-c to ruby]
%% (c) 2012 Wojciech Kaczmarek frk@wisdio.com
%
:- use_module(library(http/dcg_basics)).

%% util

+dl(Xs-Ys, Ys-Zs, Xs-Zs).

list_to_dl(S,DL-E) :- append(S,E,DL).

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

ident_c(C) --> [C], {code_type(C, alnum)}.
ident_c('_') --> "_".

ident([H|T]) --> ident_c(H), ident(T).
ident([H]) --> [H].

%% parser

var(V) --> ident(V).

meth_arg(arg(name(N),noval)) --> ident(N).
meth_arg(arg(name(N),val(V))) --> ident(N), ":", blanks, exp(V).

meth([A]) --> meth_arg(A).
meth([H|T]) --> meth_arg(H), blank, blanks, meth(T).

msg(msg(sender(X), meth(M))) -->
    "[", exp(X), {X\=[]}, blank, blanks, meth(M), "]".
    

attr_exp([X]) --> var(X).
attr_exp([H|T]) --> var(H), ".", attr_exp(T).

exp(V) --> const(V).
exp(V) --> var(V).
exp(V) --> msg(V).
exp(attr(V)) --> attr_exp(V).
exp(V) --> blank, blanks, exp(V).

stmt(V) --> msg(V), ";".
stmt(asgn(dest(D),val(V))) -->
    attr_exp(D), blanks, "=", exp(V), blanks, ";".


%% translator


%% test
ex1(S) :- S="[Foo alloc]".
ex2(S) :- S="[[PhotoPickerController alloc] init]".
ex3(S) :- S="[[[PhotoPickerController alloc] initWithDelegate:self] autorelease];".
ex4(S) :- S="self.photoPickerController = [[[PhotoPickerController alloc] initWithDelegate:self] autorelease];".

:- begin_tests(messages).

test(ex1, [nondet]) :- ex1(S), phrase(exp(msg(_,_)), S).
test(ex2, [nondet]) :- ex2(S), phrase(exp(msg(_,_)), S).
test(ex3, [nondet]) :- ex3(S), phrase(stmt(msg(_,_)), S).
test(ex4, [nondet]) :- ex4(S), phrase(stmt(asgn(_,_)), S).
test(disallow_empty_ident, [fail]) :-
    phrase(ident(_), "").
test(disallow_empty_var, [fail]) :-
    phrase(exp(var(_)), "").
test(disallow_empty_method_name, [fail]) :-
    phrase(exp(msg(_, meth([ arg(name([]),_) ]))), "[foo ]").
test(arity0_message, [nondet]) :-
    phrase(exp(
               msg(sender(_),
                   meth( [ arg(name("alloc"), noval) ] ))
              ),
           "[foo alloc]").
test(arity1_message, [nondet]) :-
    phrase(exp(msg(_,_)),
           "[foo bar:42]").
test(arity2_message, [nondet]) :-
    phrase(exp(msg(_,_)),
           "[foo bar:42 quux:23]").
test(message_sender_nesting, [nondet]) :-
    phrase(exp(
               msg(sender(msg(sender("foo"), meth( [ arg(_, noval) ] ))),
                   meth(_)
                  )),
           "[[foo bar] quux:1 boo: 2]").
test(message_meth_arg_nesting, [nondet]) :-
    phrase(exp(
               msg(sender("foo"),
                   meth([
                         arg(name("quux"), val( msg(_,_) )),
                         arg(name("zzz"),  val( msg(_,_) ))
                        ])
                  )),
           "[foo quux: [bar foo] zzz:[xxx yyy]]").
test(disallow_empty_sender, [fail]) :-
    phrase(exp(msg(_,_)), "[foo]").
test(allow_blanks_before_exp, [nondet]) :-
    phrase(exp(msg(sender(msg(_,_)), _)), " [  [foo z] x ]").
test(attr1, [nondet]) :-
    phrase(exp(attr(_)),
           "foo.bar").
test(attr2, [nondet]) :-
    phrase(exp(attr(L)),
           "foo.bar.quux"),
    nth1(2, L, "bar").

:- end_tests(messages).
