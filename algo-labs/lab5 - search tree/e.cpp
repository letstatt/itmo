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
const int inf = 1e9 + 7;

struct node {
	int key, prior, sz = 1;
	node *l = nullptr, *r = nullptr;
	node (ll key) : key(key), prior(gen()) {}
};

int get_size(node* t) {
    return t ? t->sz : 0;
}

void recalc(node* t) {
    if (t) t->sz = 1 + get_size(t->l) + get_size(t->r);
}

void split(node* t, int key, node* &l, node* &r) {
	if (!t) {
        l = r = nullptr;
	} else if (key < t->key) {
		split(t->l, key, l, t->l), r = t;
	} else {
		split(t->r, key, t->r, r), l = t;
	}
	recalc(r);
	recalc(l);
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

void traverse(node* t) {
    if (!t) return;
    traverse(t->l);
    cout << "{" << t->key << "} ";
    traverse(t->r);
}

int get_max(node *t) {
    return (t ? max(t->key, get_max(t->r)) : -inf);
}

int c, k, n;

void solve() {
    cin >> n;

    node* t = nullptr;

    while (n--) {
        cin >> c >> k;

        if (c == +1) {
            node *l = nullptr, *r = nullptr;
            split(t, k, l, r);

            if (l && get_max(l) == k) {
                t = merge(l, r);
            }
            else {
                node* q = new node(k);
                t = merge(l, merge(q, r));
            }
        } else if (c == -1) {
            node *l = nullptr, *r = nullptr;
            split(t, k, t, r);
            split(t, k - 1, l, t);
            t = merge(l, r);
        } else {
            node *l = nullptr, *r = nullptr;
            implicit_split(t, get_size(t) - k + 1, l, r);

            /*traverse(l);
            cout nl;
            traverse(r);
            cout nl;*/

            cout << get_max(l) nl;
            t = merge(l, r);
        }

        //traverse(t);
    }
}


