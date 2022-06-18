:- load_library('alice.tuprolog.lib.DCGLibrary').

% base

variable(Name, variable(Name)).
const(Value, const(Value)).

op_add(A, B, operation(op_add, A, B)).
op_subtract(A, B, operation(op_subtract, A, B)).
op_multiply(A, B, operation(op_multiply, A, B)).
op_divide(A, B, operation(op_divide, A, B)).
op_negate(A, operation(op_negate, A)).
op_sinh(A, operation(op_sinh, A)).
op_cosh(A, operation(op_cosh, A)).

operation(op_add, A, B, R) :- R is A + B.
operation(op_subtract, A, B, R) :- R is A - B.
operation(op_multiply, A, B, R) :- R is A * B.
operation(op_divide, A, B, R) :- R is A / B.
operation(op_negate, A, R) :- R is -A.
operation(op_sinh, A, R) :- R is (exp(A) - exp(-A)) / 2.
operation(op_cosh, A, R) :- R is (exp(A) + exp(-A)) / 2.

lookup(K, [(K, V) | _], V).
lookup(K, [_ | T], V) :- lookup(K, T, V).

evaluate(const(V), _, V).
evaluate(variable(Name), Vars, R) :- atom_chars(Name, [H | _]), lookup(H, Vars, R).
evaluate(operation(Op, A), Vars, R) :-
    evaluate(A, Vars, AV),
    operation(Op, AV, R).
evaluate(operation(Op, A, B), Vars, R) :-
    evaluate(A, Vars, AV),
    evaluate(B, Vars, BV),
    operation(Op, AV, BV, R).

nonvar(V, _) :- var(V).
nonvar(V, T) :- nonvar(V), call(T).

% stringify expression

expr_p(variable(Name)) -->
    {nonvar(Name, atom_chars(Name, Name2))},
    var_p(Name2),
    {not(empty(Name2)), atom_chars(Name, Name2)}.

var_p([]) --> [].
var_p([H | T]) -->
  {member(H, ['x','y','z','X','Y','Z'])},
  [H], var_p(T).

expr_p(const(Value)) -->
  {nonvar(Value, number_chars(Value, Chars))},
  digits_p_init(Chars),
  {number_chars(Value, Chars)}.

expr_p(operation(Op, A)) --> op_p(Op), ['('], expr_p(A), [')'].
expr_p(operation(Op, A, B)) --> ['('], expr_p(A), [' '], op_p(Op), [' '], expr_p(B), [')'].

% parse trimmed string

expr_p2(variable(Name)) --> expr_p(variable(Name)).
expr_p2(const(Value)) --> expr_p(const(Value)).

expr_p2(operation(Op, A)) --> op_p(Op), ['('], expr_p2(A), [')'].
expr_p2(operation(Op, A, B)) --> ['('], expr_p2(A), op_p(Op), expr_p2(B), [')'].

% parse number chars

empty([]).

digits_p_init(['-' | T]) -->
    ['-'], digits_p(T),
    {not(empty(T))}.
digits_p_init(L) -->
    digits_p(L),
    {not(empty(L))}.

digits_p([]) --> [].
digits_p([H | T]) -->
  {member(H, ['.','0','1','2','3','4','5','6','7','8','9'])},
  [H], digits_p(T).

% grammar of operations

op_p(op_add) --> ['+'].
op_p(op_subtract) --> ['-'].
op_p(op_multiply) --> ['*'].
op_p(op_divide) --> ['/'].
op_p(op_negate) --> ['n','e','g','a','t','e'].
op_p(op_sinh) --> ['s','i','n','h'].
op_p(op_cosh) --> ['c','o','s','h'].

% main

expr_str(E, A) :- ground(E), phrase(expr_p(E), C), atom_chars(A, C).
expr_str(E, A) :- atom(A), atom_chars(A, C), trim(C, D), phrase(expr_p2(E), D).

trim([], []).
trim([' ' | T], R) :- trim(T, R), !.
trim([H | T], [H | TR]) :- trim(T, TR).

infix_str(E, R) :-
    expr_str(E, R).
