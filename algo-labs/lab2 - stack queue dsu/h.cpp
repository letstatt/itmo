#pragma GCC target("sse,sse2,sse3,ssse3,sse4,popcnt,abm,mmx,avx2,tune=native")

#include <iostream>

using namespace std;

typedef long long ll;
#define sp << " " <<
#define nl << "\n"
#define sq << ""

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);
    solve();
}

struct node {
    int length = 8;
    int sz = 0;
    int clan_sz = 1;
    int *a = nullptr;

    node() {
        a = new int[length];
        a[0] = 0;
        sz = 1;
    }

    void push(int i) {
        a[sz++] = i;

        if (sz == length) {
            int *b = new int[length * 2];
            for (int j = 0; j < length; ++j) {
                b[j] = a[j];
            }
            length *= 2;
            a = b;
        }
    }

    int operator [] (int i) {
        return a[i];
    }

    int back() {
        return a[sz - 1];
    }

    int size() {
        return sz;
    }
};

int* p;
pair<int, node>* b;
int n, m;

int getp(int i) {
    int j;

    if (p[i] != i) {
        j = getp(p[i]);
    } else {
        return i;
    }

    int accumulated = b[p[i]].second.back() - b[p[i]].second[b[i].first];
    // pref_suff

    if (accumulated > 0) {
        b[i].second.push(b[i].second.back() + accumulated);
    }

    /*
    We are talking about implementing a PUSH operation similar to the implementation of a segment tree.
    All the advantages of the DSU are preserved.
    */

    b[i].first = b[j].second.size() - 1;
    return (p[i] = j);
}

void solve() {
    cin >> n >> m;
    string q;
    int x, y;

    p = new int[n + 1];
    b = new pair<int, node>[n + 1];

    for (int i = 1; i <= n; ++i) {
        b[i] = {-1, node()};
        p[i] = i;
    }

    while (cin >> q) {
        if (q == "join") {
            cin >> x >> y; // unite
            x = getp(x);
            y = getp(y);
            if (x == y) continue;
            if (b[x].second.clan_sz < b[y].second.clan_sz) {
                swap(x, y); // x - leader
            }
            p[y] = x;
            b[x].second.clan_sz += b[y].second.clan_sz;
            b[y].first = b[x].second.size() - 1;
        } else if (q == "add") {
            cin >> x >> y;
            int t = getp(x); // push to pref_suff
            b[t].second.push(b[t].second.back() + y);
        } else {
            cin >> x;
            getp(x); // get
            cout << b[x].second.back() nl;
        }
    }
}
