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
ident([]) --> [].

%% parser

var(V) --> ident(V).

meth_arg(arg(name(N),noval)) --> ident(N).
meth_arg(arg(name(N),val(V))) --> ident(N), ":", blanks, const(V).

meth([A]) --> meth_arg(A).
meth([H|T]) --> meth_arg(H), blank, blanks, meth(T).

msg(msg(sender(X), meth(M))) -->
    "[", exp(X), blank, blanks, meth(M), "]".
    

exp(V) --> const(V).
exp(V) --> var(V).
exp(V) --> msg(V).
exp([]) --> [].


%% translator


%% test
ex1(S) :- S="[Foo alloc]".
ex2(S) :- S="[[PhotoPickerController alloc] init]".
ex3(S) :- S="[[[PhotoPickerController alloc] initWithDelegate:self] autorelease];".
ex4(S) :- S="self.photoPickerController = [[[PhotoPickerController alloc] initWithDelegate:self] autorelease];".

:- begin_tests(messages).

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

:- end_tests(messages).
