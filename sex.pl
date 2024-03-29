%%-*-prolog-*-
%%
%% Seductively Efficient Xenophilic Intelligent Translator
%% of objective-c to ruby
%% (c) 2012 Wojciech Kaczmarek <wojtekk@kofeina.net>

:- use_module(library(dcg/basics)).

%% util

+dl(Xs-Ys, Ys-Zs, Xs-Zs).

list_to_dl(S,DL-E) :- append(S,E,DL).

empty_dl(H-T) :- unify_with_occurs_check(H,T).

%% join list of code_lists using separator:
join(_Sep, [H], H).
join( Sep, [H|T], S) :-
    join(Sep,T, S2), append(H,Sep, S1), append(S1,S2, S).

count_lspaces(L, N) :- count_lspaces(L,0, N).
count_lspaces([], N, N).
count_lspaces([32|T], I, N) :- !,
    I1 is I+1,
    count_lspaces(T, I1, N).
count_lspaces([_|_], N, N).

ltrim([], []).
ltrim([H|T], Res) :-
	( code_type(H, space) ->
	  ltrim(T, Res)
	; Res=[H|T] ).

rtrim(S0, S) :-
	reverse(S0, R),
	ltrim(R, R2),
	reverse(R2, S).
	

%% lexer

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

dquote --> "\"".

lpar --> "(", whites.
rpar --> whites, ")".

lbracket --> "[", whites.
rbracket --> whites, "]".

whites1 --> white, whites.
blanks1 --> blank, blanks.

lf --> blanks.
lf --> whites, "\r\n".

semicolon --> whites, ";", lf, whites.
semicolon --> whites, ";", whites.
sc --> semicolon.

const_num(const(num(V))) --> number(V), "ll", !.
const_num(const(num(V))) --> number(V), "LL", !.
const_num(const(num(V))) --> number(V), [Mod],
    { member(Mod, [0'f, 0'F, 0'u, 0'U, 0'l, 0'L]) }.
const_num(const(num(V))) --> number(V).

const_str(const(str(S))) --> "@", dquote, string_without("\"",S), dquote.
const_str(const(str(S))) --> dquote, string_without("\"",S), dquote.

const(V) --> const_str(V), !.
const(V) --> const_num(V).

ident_c1('_') --> "_".
ident_c1(C) --> [C], {code_type(C, alpha)}.

ident_c('_') --> "_".
ident_c(C) --> [C], {code_type(C, alnum)}.

ident("nil") --> "NULL", !.
ident("true") --> "YES", !.
ident("false") --> "NO", !.
ident([C]) --> ident_c1(C).     % this way ident can be '_' - bit ugly
ident([H|T]) --> ident_c1(H), ident2(T).
ident2([C]) --> ident_c(C).
ident2([H|T]) --> ident_c(H), ident2(T).

%% parser

meth_arg1(arg(name(N),noval)) --> ident(N).
meth_arg1(X) --> meth_arg(X).

meth_arg(arg(name(N),val(const(str(V))))) -->
    ident(N), ":@selector(", !, ident(V), ":)".
meth_arg(arg(name(N),val(V))) --> ident(N), ":", whites, exp(V).

meth([A]) --> meth_arg1(A).
meth([H|T]) --> meth_arg(H), whites1, meth(T).

msg(msg(rcv(X), meth(M))) -->
    lbracket, !, exp(X), {X\=[]}, whites1, meth(M), rbracket.


args([]) --> [].
args([H]) --> exp(H).
args([H|T]) --> exp(H), whites, ",", whites, args(T).

funcall(fun(name(F), args(A))) -->
    ident(F), lpar, !, args(A), rpar.

array(array(Xs)) -->
    "@", lbracket, args(Xs), whites1, rbracket.

type --> ident(_).

type_ptr --> type, whites1, "*", whites.
type_ptr --> type, whites, "*", whites1.

type_decl --> type.
type_decl --> type_ptr.


var(ident(V)) --> ident(V).

attr_var([X]) --> var(X).
attr_var([H|T]) --> var(H), ".", attr_var(T).

casted_var(V) --> lpar, type_decl, rpar, whites, var(V).
casted_var(attr(V)) --> lpar, type_decl, rpar, whites, attr_var(V).

attr_exp([X]) --> var(X).
attr_exp([H|T]) --> exp(H), ".", attr_exp(T).

lambda_args([]) --> [].
lambda_args([H]) --> ident(_Type), whites1, ident(H).
%%lambda_args([H|T]) --> lambda_args(H), whites, ",", whites, lambda_args(T).
%% above line is only a guess how lambdas with arity>1 would look in obj-c

lambda_body(L) -->
    "{", blanks, code_list(L), blanks, "}".
lambda(lambda(args([]), body(V))) -->
    "^", whites, lambda_body(V).
lambda(lambda(args(A), body(V))) -->
    "^", lpar, whites, lambda_args(A), whites, rpar, whites, lambda_body(V).


exp(V) --> msg(V).
exp(V) --> const(V).
exp(V) --> var(V).
exp(V) --> casted_var(V).
exp(V) --> funcall(V).
exp(V) --> lambda(V).
exp(V) --> array(V).
exp(attr(V)) --> attr_var(V).
%%exp(attr(V)) --> attr_exp(V). %% BUG, hangs with this
exp(V) --> lpar, exp(V), rpar.
exp(V) --> whites1, exp(V).

asgn_op("=")  --> "=".
asgn_op("+=") --> "+=".
asgn_op("-=") --> "-=".

stmt(msg_stmt(V)) --> msg(V), sc.
stmt(asgn(dest(D), op(O), val(V))) -->
    attr_var(D), whites, asgn_op(O), exp(V), sc.
stmt(asgn(dest([D]), op(O), val(V))) -->
    type_decl, whites, var(D), whites, asgn_op(O), exp(V), sc.
stmt(S) --> whites1, stmt(S).

code_list([]) --> [].
code_list([H]) --> stmt(H).
code_list([H|T]) --> stmt(H), code_list(T).

code(code(L)) --> code_list(L).

guess_stmt(S) :-
    S==""; member(59,S), not(ltrim(S, [94|_])).
%% 59 is semicolon, 94 is caret
%% very crude heuristics ;)

parse(S, P) :-
	( guess_stmt(S) ->
	  phrase(code(P), S);
	  phrase(exp(P), S)
	).


%% translator


foldl_pred_example((N,SV), ([],[]), ([N],[SV])).
foldl_pred_example((N,SV), ([H1|T1],[H2|T2]), ( [H1|[N|T1]], [H2|[SV|T2]] ) ).
%% usage: foldl(foldl_pred_example,  [(1,a), (2,b)],  ([],[]),  V).

fold_msg_args((N,SV), "", Acc) :-
    swritef(Buf, '%s:%s', [N,SV]),
    string_to_list(Buf, Acc).
fold_msg_args((N,SV), L0, Acc) :-
    swritef(Buf, ', %s:%s', [N,SV]),
    string_to_list(Buf, L1), append(L0,L1, Acc).

trans(code([]), S0, S0).
trans(code([H|T]), S0, SAll) :-
	trans(H, SH),
	append(S0, SH, S2), append(S2, "\n", S3),
	trans(code(T), S3, SAll).

trans(lambda(args([]), body(C)), L0, Acc) :-
    append(L0, "(lambda do\n", Header),
    trans(code(C), Body),
    append(Header, Body, L1),
    append(L1, "end)\n", Acc).

trans(lambda(args([A]), body(C)), L0, Acc) :-
    append(L0, "(lambda do |", H1),
    append(H1, A, H2), append(H2, "|\n", Header),
    trans(code(C), Body),
    append(Header, Body, L1),
    append(L1, "end)\n", Acc).

trans(const(C), S0, SAll) :-
    C =.. [Type, V],
    ( Type==str -> Mod='"%s"'; Mod='%q' ),
    swritef(S, Mod, [V]), string_to_list(S,L),
    append(S0, L, SAll).
trans(ident(V), S0, SAll) :-
    append(S0,V,SAll).
trans(attr(L), S0, SAll) :-
    findall(I, member(ident(I),L), Is),
    join(".", Is, S),
    append(S0, S, SAll).

trans(asgn(dest(D), op(O), val(V)), L0, Acc) :-
    trans(attr(D), LD),
    trans(V, LV),
    append([32|O], " ", Op),
    append(L0, LD, L1), append(L1, Op, L2), append(L2, LV, Acc).

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
      foldl(fold_msg_args, RestArgs, "", SVals2Plus),
      swritef(S, '%s.%s(%s, %s)', [SRcv, MethName, SVal1, SVals2Plus])
    ),
    string_to_list(S,L1),
    append(S0, L1, SAll).

trans(fun(name(N), args([])), L0, Acc) :-
    append(N,"()",F),
    append(L0, F, Acc).
trans(fun(name(N), args(Args)), L0, Acc) :-
    append(N, "(", F1),
    findall(LV,  (member(A, Args), trans(A, LV)),  LVs),
    join(", ", LVs, L),
    append(F1, L, F2),
    append(F2, ")", F),
    append(L0, F, Acc).

trans(ParseTree, Acc) :- trans(ParseTree, "", Acc).

%% test

ex1(S) :- S="[Foo alloc]".
ex2(S) :- S="[[PhotoPickerController alloc] init]".
ex3(S) :- S="[[[PhotoPickerController alloc] initWithDelegate:self] autorelease];".
ex4(S) :- S="self.photoPickerController = [[[PhotoPickerController alloc] initWithDelegate:self] autorelease];".

:- begin_tests(utils).
test(ltrim) :-
	ltrim("\r\t\n \n \r \t foo ", "foo ").
test(rtrim) :-
	rtrim(" foo \r\t\n \n \r \t", " foo").
test(count_lspaces_empty) :- count_lspaces("", 0).
test(count_lspaces_0) :- count_lspaces("foo", 0).
test(count_lspaces_1) :- count_lspaces(" foo", 1).
test(count_lspaces_1_only_space) :- count_lspaces(" ", 1).
test(count_lspaces_5) :- count_lspaces("     foo", 5).
test(count_lspaces_5_only_spaces) :- count_lspaces("     ", 5).
test(count_lspaces_is_oneway, [error(instantiation_error)]) :-
    count_lspaces(" ", _I, 1).
:- end_tests(utils).

:- begin_tests(literals).
test(int_const, [nondet]) :-
    S="42", 
    parse(S, P),
    P=const(num(42)).
test(float_const, [nondet]) :-
    S="42.23", 
    parse(S, P),
    P=const(num(42.23)).
test(float_const_with_f, [nondet]) :- S="42.5f", parse(S, P), P=const(num(42.5)).
test(float_const_with_F, [nondet]) :- S="42.5F", parse(S, P), P=const(num(42.5)).
test(int_const_with_u, [nondet]) :- S="42u", parse(S, P), P=const(num(42)).
test(int_const_with_U, [nondet]) :- S="42U", parse(S, P), P=const(num(42)).
test(int_const_with_l, [nondet]) :- S="42l", parse(S, P), P=const(num(42)).
test(int_const_with_L, [nondet]) :- S="42L", parse(S, P), P=const(num(42)).
test(int_const_with_ll, [nondet]) :- S="42ll", parse(S, P), P=const(num(42)).
test(int_const_with_LL, [nondet]) :- S="42ll", parse(S, P), P=const(num(42)).
test(str_const, [nondet]) :-
    S="\"a42\"", 
    parse(S,P),
    P=const(str("a42")).
test(str_const_with_atsign, [nondet]) :-
    S="@\"a42\"", 
    parse(S,P),
    P=const(str("a42")).
test(trans_str_const_having_only_a_number, [nondet]) :-
    S="\"42\"", 
    parse(S,P),
    P=const(str("42")).
:- end_tests(literals).


:- begin_tests(determinism).
test(const_num) :-
    phrase(const(const(num(42.3))), "42.3").
test(const_str) :-
    phrase(const(const(str(_))), "\"42\"").
test(ident_c) :-
    forall( member(C, ["_","a","b"]),
            phrase(ident_c(_), C) ).
test(ident) :-
    forall( member(C, ["_aa","aa","aa2", "aa_"]),
            phrase(ident(_), C) ).
test(meth_arg1_1, fixme(should_be_det)) :-
    phrase(meth_arg1(arg(_,noval)), "arg").
test(meth_arg1_2, fixme(should_be_det)) :-
    phrase(meth_arg1(arg(_,val(_))), "arg:2").
:- end_tests(determinism).

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
test(ex4, [nondet]) :- ex4(S), phrase(stmt(asgn(_,_,_)), S).
:- end_tests(messages).


:- begin_tests(selectors).
test(selector_arity1, [nondet]) :-
    S="[[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handleTapGesture:)]",
    parse(S, P),
    P=msg(rcv(_), meth([ _, arg(name("action"), val(const(str("handleTapGesture")))) ])).
:- end_tests(selectors).

:- begin_tests(attrs).

test(attr_var, [nondet]) :-
    phrase(exp(attr(_)),
           "foo.bar").
test(attr_var_nested, [nondet]) :-
    phrase(exp(attr(L)),
           "foo.bar.quux"),
    nth1(2, L, ident("bar")).
test(attr_exp, [nondet, fixme(wip)]) :-
    S="[UIColor clearColor].CGColor",
    parse(S, P),
    P=attr(msg(_,_), ident("CGColor")).
:- end_tests(attrs).

:- begin_tests(assignments).
test(asgn_to_attr, [nondet]) :-
    S="foo.bar = [x y:z];",
    parse(S, P),
    P=code([ asgn(dest([ident("foo"),ident("bar")]), op("="), val(msg(_,_))) ]).
test(asgn_with_type_decl, [nondet]) :-
    S="UIView bar = [x y:z];",
    parse(S, P),
    P=code([ asgn(dest([ident("bar")]), op("="), val(msg(_,_))) ]).
test(asgn_with_type_pointer_decl, [nondet]) :-
    foreach(member(S,
                   ["UIView *bar = [x y:z];", "UIView* bar = [x y:z];"
                    ]),
            (parse(S, P),
             P=code([ asgn(dest([ident("bar")]), op("="), val(msg(_,_))) ]) )).
:- end_tests(assignments).

:- begin_tests(lambdas).
test(lambda_arg1, [nondet]) :-
    S="BOOL finished",
    phrase(lambda_args(P), S),
    P=[_].
test(lambda_body, [nondet]) :-
    S="{\r\n\n\n\na=2;\nb=3;\n  }",
    phrase(lambda_body(P), S),
    P=[asgn(_,_,_), asgn(_,_,_)].
test(lambda_arity0, [nondet]) :-
    S="^{\na=2;\nb=3;\n}",
    parse(S, P),
    P=lambda(args([]), body([asgn(_,_,_), asgn(_,_,_)])),
    trans(P, S2),
    S2="(lambda do\na = 2\nb = 3\nend)\n".
test(lambda_arity1, [nondet]) :-
    S="^(BOOL x) {\na=2;\nb=3;\n}",
    parse(S, P),
    P=lambda(args([_]), body([asgn(_,_,_), asgn(_,_,_)])),
    trans(P, S2),
    S2="(lambda do |x|\na = 2\nb = 3\nend)\n".
:- end_tests(lambdas).

:- begin_tests(funcalls).
test(fun_arity0, [nondet]) :-
    S="foo()",
    parse(S, P),
    P=fun(name("foo"), args([])).
test(fun_arity1, [nondet]) :-
    S="foo(42)",
    parse(S, P),
    P=fun(name("foo"), args([const(num(42))])).
test(fun_arity1_nested_with_msg, [nondet]) :-
    S="foo([x y:3])",
    parse(S, P),
    P=fun(name("foo"), args([ msg(rcv(_), meth([arg(name("y"), val(_))])) ])).
test(fun_arity2_simple, [nondet]) :-
    S="foo(41, bar)",
    parse(S, P),
    P=fun(name("foo"), args([ const(_), ident(_) ])).
test(fun_arity3_nested, [nondet]) :-
    S="foo(41, baz(10), bar)",
    parse(S, P),
    P=fun(name("foo"), args([ const(_), fun(name("baz"), args([const(_)])), ident(_) ])).
:- end_tests(funcalls).

:- begin_tests(casts).
test(cast_simple, [nondet]) :-
    S="(id)kCTFontAttributeName",
    parse(S,P),
    P=ident("kCTFontAttributeName").
test(cast_method_args, [nondet]) :-
    S="[self.attributedString addAttribute:(id)kCTFontAttributeName value:(id)ctFontRef range:range]",
    parse(S,P),
    P=msg(_, meth( [arg(name("addAttribute"), val(ident("kCTFontAttributeName"))),
                    arg(name("value"), val(ident("ctFontRef"))),
                    _] )).
test(cast_attr, [nondet]) :-
    S="(id) foo.bar",
    parse(S,P),
    P=attr([_,_]).
:- end_tests(casts).

:- begin_tests(many_statements).
test(two_statements, [nondet]) :-
    S=" [foo bar]; a=3;",
    phrase(code(_), S).
test(two_statements_with_lfs, [nondet]) :-
    S=" [foo bar];\n a=3;\n",
    phrase(code(_), S).
test(two_statements_with_many_lfs, [nondet]) :-
    S=" [foo bar];\n\n\n a=3;\n\n",
    phrase(code(_), S).
:- end_tests(many_statements).

:- begin_tests(many_statements_as_code_block).

test(two_statements_without_whitespaces, [nondet]) :-
    S=" [foo bar]; a=3;",
    phrase(code(P), S),
	P=code([_,_]).
test(two_statements_with_space_between, [nondet]) :-
    S=" [foo bar]; a=3;",
    phrase(code(P), S),
	P=code([_,_]).
test(two_statements_with_lf_between, [nondet]) :-
    S=" [foo bar];\na=3;",
    phrase(code(P), S),
	P=code([_,_]).
test(two_statements_with_many_lfs, [nondet]) :-
    S=" [foo bar];\n\n\n a=3;\n\n",
    phrase(code(P), S),
	P=code([_,_]).
:- end_tests(many_statements_as_code_block).

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
test(noval_method_arg_cant_occur_in_the_middle, [fail]) :-
    phrase(exp(msg(_,_)),
           "[[obiekt dupa] bla foo: bar xxx]").
test(parent_exp_with_white_left, [nondet]) :-
    S="( 42)",
    phrase(exp(_), S).
test(parent_exp_with_white_right, [nondet]) :-
    S="(42 )",
    phrase(exp(_), S).
test(msg_space_simecolon, [nondet]) :-
    S="[foo bar] ;",
    phrase(stmt(_), S).
test(lf_at_the_end_of_stmt, [nondet]) :-
    S="foo=42;\n",
    phrase(stmt(_), S).
test(disallow_spaces_at_the_end_of_exp, [fail]) :-
    S="foo    ",
    parse(S, _).
test(trim_spaces_at_the_end_of_exp_then_parse, [nondet]) :-
    S="foo    \t\r\n",
    rtrim(S,S1),
    parse(S1, _).
:- end_tests(corner_cases).

:- begin_tests(trans).
test(trans_var, [nondet]) :-
    S="foo",
    phrase(exp(P), S), trans(P,"",S).
test(trans_int_const, [nondet]) :-
    S="42", 
    phrase(exp(P), S), trans(P,"",S).
test(trans_str_const, [nondet]) :-
    S="\"a42\"", 
    phrase(exp(P), S), trans(P,"",S).
test(trans_str_const_with_atsign, [nondet]) :-
    S="@\"a42\"", 
    phrase(exp(P), S), trans(P,"", "\"a42\"").
test(trans_str_const_having_only_a_number, [nondet]) :-
    S="\"42\"", 
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
test(trans_immutable_array, [nondet, fixme(write_actual_trans)]) :-
    S="@[ @\"SO_SWEET\", 2 ]",
    phrase(array(P), S),
    P=array([_,_]), trans(P, "[\"SO_SWEET\", 2]").
:- end_tests(trans).

:- begin_tests(trans_as_code_block).
test(trans_empty_code_block_gives_empty_string, [nondet]) :-
	trans(code([]), "").
test(trans_ex4, [nondet]) :-
	ex4(S),
	phrase(code(P), S), trans(P, S2),
    S2="self.photoPickerController = PhotoPickerController.alloc.initWithDelegate(self).autorelease\n".

:- end_tests(trans_as_code_block).

:- begin_tests(trans_funcalls).
test(trans_fun_arity0, [nondet]) :-
    S="foo()",
    parse(S, P), trans(P, S).
test(trans_fun_arity1, [nondet]) :-
    S="foo(42)",
    parse(S, P),
    trans(P, S).
test(trans_fun_arity1_nested_with_msg, [nondet]) :-
    S="foo([x y:3])",
    parse(S, P), trans(P, S2),
    S2="foo(x.y(3))".
test(trans_fun_arity2_simple, [nondet]) :-
    S="foo(41, bar)",
    parse(S, P), trans(P, S).
test(trans_fun_arity3_nested, [nondet]) :-
    S="foo(41, baz(10), bar)",
    parse(S, P), trans(P, S).
:- end_tests(trans_funcalls).

:- begin_tests(trans_selectors).
test(trans_selector_arity1, [nondet]) :-
    S="[[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(handleTapGesture:)]",
    parse(S, P), trans(P, S2),
    S2="UITapGestureRecognizer.alloc.initWithTarget(self, action:\"handleTapGesture\")".
:- end_tests(trans_selectors).

:- begin_tests(trans_corner_cases).
test(nil) :-
    phrase(var(P), "NULL"), trans(P, "nil").
test(nil_in_funcall, fixme(should_be_det)) :-
    phrase(funcall(P), "foo(NULL)"), trans(P, "foo(nil)").
test(yes) :-
    %% if the whole exp were det, the following line could be written
    %% using parse/2 and still be det
    phrase(var(P), "YES"), trans(P, "true").
test(no) :-
    phrase(var(P), "NO"), trans(P, "false").
test(spaaaces_and_lf_at_the_end_of_stmt, [nondet]) :-
    S="foo=42;      \n",
    phrase(code(P), S), trans(P, _).
test(spaaaces_and_crlf_at_the_end_of_stmt, [nondet]) :-
    S="foo=42;      \r\n",
    phrase(stmt(P), S), trans(P, _).
test(lf_at_the_end_of_stmt, [nondet]) :-
    S="foo=42;\n",
    phrase(stmt(P), S), trans(P, _).
test(lf_twice_at_the_end_of_stmt, [nondet]) :-
    S="foo=42;\n\n",
    phrase(stmt(P), S), trans(P, _).
:- end_tests(trans_corner_cases).

:- begin_tests(trans_as_code_or_exp).
test(trans_exp, [nondet]) :-
	parse("foo", P),
	trans(P, "foo").
test(trans_empty_code_block_gives_empty_string, [nondet]) :-
	parse("", P),
	trans(P, "").
test(trans_ex4, [nondet]) :-
	ex4(S),
	parse(S, P), trans(P, S2),
    S2="self.photoPickerController = PhotoPickerController.alloc.initWithDelegate(self).autorelease\n".

:- end_tests(trans_as_code_or_exp).

%% i/o

eat(Buf) :-
    read_stream_to_codes(user_input, Buf).

digest(Buf) :-
    rtrim(Buf, Buf2),
    parse(Buf2, P),
    trans(P, Out),
    writef(Out).
digest(Buf) :-
    writef('/* %s NOT SEXY */', [Buf]).

process :- eat(X), digest(X).

%% run

repl :- run_tests.
start :- prompt(_,''), process.
main(_) :- start.
