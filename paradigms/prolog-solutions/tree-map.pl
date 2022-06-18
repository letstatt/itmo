% node(key, val, h, l, r)

node(K, V, H, L, R, node(K, V, H, L, R)).
extract(node(K, V, H, L, R), K, V, H, L, R).

get_height(node(_, _, Res, _, _), Res) :- !.
get_height(nil, Res) :- Res is 0.

get_balance_factor(node(_, _, _, L, R), Res) :- get_height(L, H1), get_height(R, H2), Res is H2 - H1.

get_max(A, B, Res) :- A >= B, Res is A, !.
get_max(_, Res, Res).

update_height(node(K, V, _, L, R), Res) :-
    get_height(L, H1),
    get_height(R, H2),
    get_max(H1, H2, H3),
    H4 is H3 + 1,
    node(K, V, H4, L, R, Res).

node_recalced(K, V, H, L, R, Res) :-
    update_height(node(K, V, H, L, R), Res), !.

right_rotation(node(P, PVal, PH, PL, PR), Res) :-
    extract(PL, Q, QVal, QH, QL, QR),
    node_recalced(P, PVal, 0, QR, PR, P_2),
    node_recalced(Q, QVal, 0, QL, P_2, Res).

left_rotation(node(Q, QVal, QH, QL, QR), Res) :-
    extract(QR, P, PVal, PH, PL, PR),
    node_recalced(Q, QVal, 0, QL, PL, Q_2),
    node_recalced(P, PVal, 0, Q_2, PR, Res).

balance(Node, Res) :-
    get_balance_factor(Node, BF),
    BF == 2,
    balance2long(Node, Node2),
    left_rotation(Node2, Res), !.

balance(Node, Res) :-
    get_balance_factor(Node, BF),
    BF == -2,
    balance_2long(Node, Node2),
    right_rotation(Node2, Res), !.

balance(Res, Res).

balance2long(node(K, V, H, L, R), Res) :-
    get_balance_factor(R, BF),
    BF < 0,
    right_rotation(R, R2),
    node(K, V, H, L, R2, Res), !.
balance2long(Res, Res).

balance_2long(node(K, V, H, L, R), Res) :-
    get_balance_factor(L, BF),
    BF > 0,
    left_rotation(L, L2),
    node(K, V, H, L2, R, Res), !.
balance_2long(Res, Res).

node_balanced(K, V, H, L, R, Res) :-
    node_recalced(K, V, H, L, R, Node),
    balance(Node, Res), !.

insert(nil, K, V, Res) :-
    node(K, V, 1, nil, nil, Res), !.

insert(node(K, _, H, L, R), K, V, Res) :-
    node(K, V, H, L, R, Res), !.

insert(node(Key, Val, H, L, R), K, V, Res) :-
    K < Key,
    insert(L, K, V, L2),
    node_balanced(Key, Val, H, L2, R, Res), !.

insert(node(Key, Val, H, L, R), K, V, Res) :-
    insert(R, K, V, R2),
    node_balanced(Key, Val, H, L, R2, Res), !.

get_min(nil, nil) :- !.
get_min(Node, Node) :- extract(Node, K, V, H, L, R), L == nil, !.
get_min(node(K, V, H, L, R), Res) :- get_min(L, Res), !.

get_max(nil, nil) :- !.
get_max(Node, Node) :- extract(Node, K, V, H, L, R), R == nil, !.
get_max(node(K, V, H, L, R), Res) :- get_max(R, Res), !.

remove_min(node(_, _, _, nil, Res), Res) :- !.
remove_min(node(K, V, H, L, R), Res) :-
    remove_min(L, L2),
    node_balanced(K, V, H, L2, R, Res), !.

remove_max(nil, nil) :- !.
remove_max(node(_, _, _, Res, nil), Res) :- !.
remove_max(node(K, V, H, L, R), Res) :-
    remove_max(R, R2),
    node_balanced(K, V, H, L, R2, Res), !.

remove(nil, K, nil) :- !.
remove(node(K, _, _, L, nil), K, L) :- !.
remove(node(K, _, _, L, R), K, Res) :-
    get_min(R, Min),
    remove_min(R, R2),
    extract(Min, M, MVal, MH, ML, MR),
    node_balanced(M, MVal, MH, L, R2, Res), !.

remove(node(Key, Val, H, L, R), K, Res) :-
    K < Key,
    remove(L, K, L2),
    node_balanced(Key, Val, H, L2, R, Res), !.

remove(node(Key, Val, H, L, R), K, Res) :-
    remove(R, K, R2),
    node_balanced(Key, Val, H, L, R2, Res).

map_build([], nil) :- !.
map_build([(K, V) | L], T) :-
    map_build(L, T2),
    insert(T2, K, V, T).

map_get(node(K, V, _, _, _), K, V) :- !.
map_get(node(Key, _, _, L, _), K, V) :- K < Key, map_get(L, K, V).
map_get(node(Key, _, _, _, R), K, V) :- map_get(R, K, V).

map_put(T, K, V, R) :- insert(T, K, V, R), nl.
map_remove(T, K, R) :- remove(T, K, R).

map_getLast(T, (K, V)) :- get_max(T, Max), Max = node(K, V, _, _, _).
map_removeLast(T, R) :- remove_max(T, R).