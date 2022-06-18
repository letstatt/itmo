last_prime(1).

eratosthenes(P, N) :- P > N, !.
eratosthenes(P, N) :- \+ composite(P), last_prime(Last), retract(last_prime(Last)), assert(next_prime(Last, P)), assert(last_prime(P)), P2 is P * P, sieve_loop(P2, N, P).
eratosthenes(P, N) :- P < N, P2 is P + 1, eratosthenes(P2, N).

sieve_loop(I, N, STEP) :- assert(composite(I)), I < N, I2 is I + STEP, !, sieve_loop(I2, N, STEP).

init(MAX_N) :-
    eratosthenes(2, MAX_N).

prime(N) :- \+ composite(N).

get_min_factor(D, N, R) :- D * D =< N, 0 is mod(N, D), R is D, !. % no further calcs
get_min_factor(D, N, R) :- D * D =< N, next_prime(D, D2), get_min_factor(D2, N, R).

prime_divisors(1, []) :- !.
prime_divisors(P, [P]) :- prime(P), !.

prime_divisors(N, [H | T]) :-
    number(N),
    N > 1,
    get_min_factor(2, N, H),
    N1 is div(N, H),
    prime_divisors(N1, T).

ascending_primes([]) :- !.
ascending_primes([H]) :- prime(H), !.
ascending_primes([H1, H2 | T]) :-
    H1 =< H2,
    prime(H1),
    ascending_primes([H2 | T]).

mul([], 1) :- !.
mul([H | T], R) :- mul(T, R2), R is R2 * H.

prime_divisors(N, LIST) :-
    ascending_primes(LIST),
    mul(LIST, N).

intersect([], _, T) :- !, T = [].
intersect(_, [], T) :- !, T = [].
intersect([H | T1], [H | T2], [H | T]) :- intersect(T1, T2, T).
intersect([H1 | T1], [H2 | T2], T) :- H1 < H2, intersect(T1, [H2 | T2], T). % lists are reversed
intersect([H1 | T1], [H2 | T2], T) :- H1 > H2, intersect([H1 | T1], T2, T).

gcd(A, B, R) :-
    prime_divisors(A, X),
    prime_divisors(B, Y),
    intersect(X, Y, T),
    mul(T, R).