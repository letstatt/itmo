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
    bool rev = false;
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

void reverse(node* t) {
    if (t) t->rev ^= 1;
}

void push(node* t) {
    if (t) {
        if (t->rev) {
            swap(t->l, t->r);
            reverse(t->l);
            reverse(t->r);
            reverse(t);
        }
    }
}

void split(node* t, int key, node* &l, node* &r) {
    push(t);
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
    push(t);
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
    push(l);
    push(r);
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
    push(t);
    traverse(t->l);
    cout << "{" << t->key << "} ";
    traverse(t->r);
}

void dump(node* t) {
    if (!t) return;
    push(t);
    dump(t->l);
    cout << t->key sq;
    dump(t->r);
}

int get_max(node *t) {
    push(t);
    return (t ? max(t->key, get_max(t->r)) : -inf);
}

node* t = nullptr;
int n, k;
string q;

void solve() {
    cin >> n >> k;

    for (int i = 1; i <= n; ++i) {
        t = merge(t, new node(i));
    }

    while (k--) {
        int x, y;
        cin >> x >> y;
        node *l = nullptr, *m = nullptr, *r = nullptr;
        implicit_split(t, y, m, r);
        implicit_split(m, x - 1, l, m);
        reverse(m);
        t = merge(l, merge(m, r));

        /*traverse(t);
        cout nl;*/
    }

    dump(t);
}


