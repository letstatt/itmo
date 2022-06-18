#pragma GCC optimize("Ofast")
#pragma GCC target("avx","avx2")

#include <iostream>
#include <random>
#include <vector>
#include <cmath>
#include <set>

using namespace std;

#define nl << '\n'
#define sp << ' ' <<
#define sq << ' '

typedef long long ll;

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

mt19937 gen;
const int inf = 1e7 + 7;

struct node {
    int closest_zero;
	int key, prior, sz = 1;
	node *l = nullptr, *r = nullptr;
	node (ll key) : key(key), prior(gen()) {
        closest_zero = (key ? inf : 0);
	}
};

int get_size(node* t) {
    return t ? t->sz : 0;
}

int get_closest_zero(node* t) {
    return t ? t->closest_zero : inf;
}

int safe_sum(int x, int y) {
    return min(x + y, inf); // to avoid overflow by arithmetics with inf
}

void recalc(node* t) {
    if (t) {
        t->sz = 1 + get_size(t->l) + get_size(t->r);
        t->closest_zero = min(
            min(get_closest_zero(t->l), (t->key ? inf : get_size(t->l))),
            safe_sum(get_closest_zero(t->r), get_size(t->l) + 1)
        );
    }
}

void implicit_split(node* t, int key, node* &l, node* &r) {
    if (!t) {
        l = r = nullptr;
    } else if (key < get_size(t->l) + 1) {
		implicit_split(t->l, key, l, t->l), r = t;
	} else {
		implicit_split(t->r, key - get_size(t->l) - 1, t->r, r), l = t;
	}
	recalc(r);
	recalc(l);
}

node* merge(node* l, node* r) {
	if (!l || !r) {
        return l ? l : r;
	} else if (l->prior > r->prior) {
		l->r = merge(l->r, r);
		recalc(l);
		return l;
	} else {
		r->l = merge(l, r->l);
		recalc(r);
		return r;
	}
}

void dump(node* t, vector<int> &d) {
    if (!t) return;
    dump(t->l, d);
    d.push_back(t->key);
    dump(t->r, d);
}

int get_max(node *t) {
    return (t ? max(t->key, get_max(t->r)) : -inf);
}

node* t = nullptr;
int n, k;

void solve() {
    cin >> n >> k;

    for (int i = 0; i <= n + k; ++i) {
        t = merge(t, new node(0));
    }

    for (int i = 1, x; i <= n; ++i) {
        node* l = nullptr, *m = nullptr, *r = nullptr;
        cin >> x;

        implicit_split(t, x, t, r);
        implicit_split(t, x - 1, t, m);

        int key = get_max(m);

        if (key > 0) {
            int closest_zero = get_closest_zero(r);

            node* to_delete = nullptr;
            implicit_split(r, closest_zero + 1, l, r);
            implicit_split(l, closest_zero, l, to_delete);

            if (to_delete != nullptr) { // but it is always true
                delete to_delete;
            }

            r = merge(l, r);
            m = merge(new node(i), m);

        } else if (m) {
            m->key = i;
            m->closest_zero = inf;
        }

        t = merge(t, merge(m, r));
    }

    vector<int> tree;
    dump(t, tree);

    while (tree.size() > 0 && tree.back() == 0) {
        tree.pop_back();
    }

    cout << tree.size() nl;
    for (auto &i: tree) {
        cout << i sq;
    }
}


