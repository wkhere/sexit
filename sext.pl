%%-*-prolog-*-
%%
%% Seductively Efficient Xenophilic Translator [of objective-c to ruby]
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

var(V) --> ident(V).

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

stmt(V) --> msg(V), ";".
stmt(asgn(dest(D),val(V))) -->
    attr_var(D), blanks, "=", exp(V), blanks, ";".


%% translator



%% test

:- begin_tests(messages).
ex1(S) :- S="[Foo alloc]".
ex2(S) :- S="[[PhotoPickerController alloc] init]".
ex3(S) :- S="[[[PhotoPickerController alloc] initWithDelegate:self] autorelease];".
ex4(S) :- S="self.photoPickerController = [[[PhotoPickerController alloc] initWithDelegate:self] autorelease];".

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
               msg(rcv(msg(rcv("foo"), meth( [ arg(_, noval) ] ))),
                   meth(_)
                  )),
           "[[foo bar] quux:1 boo: 2]").
test(message_meth_arg_nesting, [nondet]) :-
    phrase(exp(
               msg(rcv("foo"),
                   meth([
                         arg(name("quux"), val( msg(_,_) )),
                         arg(name("zzz"),  val( msg(_,_) ))
                        ])
                  )),
           "[foo quux: [bar foo] zzz:[xxx yyy]]").
:- end_tests(messages).

:- begin_tests(attrs).

test(attr1, [nondet]) :-
    phrase(exp(attr(_)),
           "foo.bar").
test(attr2, [nondet]) :-
    phrase(exp(attr(L)),
           "foo.bar.quux"),
    nth1(2, L, "bar").
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

