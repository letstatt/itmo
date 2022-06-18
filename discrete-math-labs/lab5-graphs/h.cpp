#include <unordered_set>
#include <algorithm>
#include <iostream>
#include <vector>
#include <set>

using namespace std;

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);
    solve();
}

int n, m, t;

struct polynomial {
    vector<int> a;

    polynomial() = default;

    polynomial(int deg) { // = empty graph
        a.resize(deg);
    }

    polynomial& operator+=(const polynomial& other) {
        a.resize(max(a.size(), other.size()));
        for (int i = 0; i < other.size(); ++i) {
            a[i] += other[i];
        }
        return *this;
    }

    polynomial& operator-=(const polynomial& other) {
        a.resize(max(a.size(), other.size()));
        for (int i = 0; i < other.size(); ++i) {
            a[i] -= other[i];
        }
        return *this;
    }

    int& operator[](int i) {
        return a[i];
    }

    int const& operator[](int i) const {
        return a[i];
    }

    size_t size() const {
        return a.size();
    }
};

struct edge {
    int a, b;

    bool operator==(const edge& e) const {
        return a == e.a && b == e.b;
    }
};

namespace std {
    template<>
    struct hash<edge> {
        size_t operator()(edge const &e) const {
            size_t h = e.a;
            h += 10000000 * e.b;
            return h;
        }
    };
}

polynomial rec(unordered_set<edge> e, int n) {
    if (n == 0) {
        return polynomial();
    }
    if (e.size() == 0) {
        polynomial ret(n + 1);
        ret[n] = 1;
        return ret;
    }

    edge uv = *e.begin();

    e.erase(uv);
    e.erase({uv.b, uv.a});

    polynomial p = rec(e, n);
    int x = t++;

    unordered_set<edge> e2;

    for (auto &i: e) {
        int u = (i.a == uv.a ? x : (i.a == uv.b ? x : i.a));
        int v = (i.b == uv.b ? x : (i.b == uv.a ? x : i.b));
        e2.insert({u, v});
    }

    p -= rec(e2, n - 1);
    return p;
}

void solve() {
    cin >> n >> m;
    unordered_set<edge> e;
    t = n + 1;

    for (int i = 0; i < m; ++i) {
        int a, b;
        cin >> a >> b;
        e.insert({a, b});
        e.insert({b, a});
    }

    polynomial ans = rec(e, n);

    cout << ans.size()-1 << endl;

    for (int i = (int) ans.size() - 1; i >= 0; --i) {
        cout << ans[i] << " ";
    }
    cout << endl;
}
