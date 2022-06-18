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
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    solve();
}

mt19937 gen;

struct node {
	ll key, prior, sum;
	node *l = nullptr, *r = nullptr;
	node (ll key) : key(key), prior(gen()), sum(key) {}
};

ll get_sum(node* t) {
    return t ? t->sum : 0;
}

void recalc(node* t) {
    if (t) t->sum = t->key + get_sum(t->l) + get_sum(t->r);
}

void split(node* t, ll key, node* &l, node* &r) {
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
    cout << "{" << t->key << " " << t->sum << "} ";
    traverse(t->r);
}

ll get_max(node *t) {
    return (t ? max(t->key, get_max(t->r)) : 0);
}

char c;
ll x, y, n;

void solve() {
    cin >> n;

    node* t = nullptr;
    ll last = 0;

    while (n--) {
        cin >> c >> x;
        if (c == '?') cin >> y;

        if (c == '+') {
            node *l = nullptr, *r = nullptr;
            x = (x + last) % ll(1e9);
            split(t, x, l, r);

            if (l && get_max(l) == x) {
                t = merge(l, r);
            }
            else {
                node* q = new node(x);
                t = merge(l, merge(q, r));
            }
            last = 0;
        } else {
            node *l = nullptr, *r = nullptr;
            split(t, x - 1, t, l);
            split(l, y, l, r);
            last = get_sum(l);
            t = merge(t, merge(l, r));
            cout << last nl;
        }

        //traverse(t);
    }
}

