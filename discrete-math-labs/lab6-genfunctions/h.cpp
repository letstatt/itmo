#include <algorithm>
#include <iostream>
#include <cassert>
#include <vector>
#include <cmath>
#include <set>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);
    solve();
}

const ll MOD = 998244353;

struct Polynomial {
    vector<ll> c;

    Polynomial() : c(1, 1) {}

    Polynomial(size_t deg) : c(deg + 1, 0) {}

    Polynomial(vector<ll>&& c, bool norm = true) : c(move(c)) {
        if (norm) {
            normalize();
        }
    }

    void normalize() {
        size_t deg = 0;
        for (size_t i = 0; i < c.size(); ++i) {
            if (c[i] != 0) {
                deg = i + 1;
            }
        }
        c.resize(deg);
    }

    ll operator[](size_t i) const {
        return (i >= 0 && i < c.size() ? c[i] : 0);
    }

    size_t size() const {
        return c.size();
    }

    Polynomial add(Polynomial const& t) const {
        vector<ll> p(max(size(), t.size()));
        for (size_t i = 0; i < size(); ++i) {
            p[i] += c[i];
        }
        for (size_t i = 0; i < t.size(); ++i) {
            p[i] += t[i];
            p[i] %= MOD;
        }
        return Polynomial(move(p));
    }

    Polynomial multiply(Polynomial const& t) const {
        vector<ll> p(size() + t.size());
        for (size_t i = 0; i < p.size(); ++i) {
            for (size_t j = 0; j <= i; ++j) {
                if (i - j >= t.size()) continue;
                p[i] += ((*this)[j] * t[i - j]) % MOD;
                p[i] = (p[i] + MOD * MOD) % MOD;
            }
        }
        return Polynomial(move(p));
    }

    Polynomial div(Polynomial const& t, size_t d) const {
        assert(t[0] == 1);
        vector<ll> p(d);

        for (size_t i = 0; i < d; ++i) {
            p[i] = (*this)[i];
            if (i == 0) continue;
            for (size_t j = 0; j <= i - 1; ++j) {
                p[i] -= (p[j] * t[i - j]) % MOD;
                p[i] = (p[i] + MOD * MOD) % MOD;
            }
        }
        return Polynomial(move(p), false);
    }

    friend ostream& operator << (ostream& stream, Polynomial const& p) {
        for (auto i: p.c) {
            stream << i sq;
        }
        stream nl;
        return stream;
    }
};

int k, n;
vector<vector<ll>> C;

void solve() {
    cin >> k >> n;
    C.resize(k + 10, vector<ll>(k + 10));

    for (int i = 0; i < C.size(); ++i) {
        C[i][0] = C[i][i] = 1;
        for (int j = 1; j < i; ++j) {
            C[i][j] = (C[i - 1][j - 1] + C[i - 1][j]) % MOD;
            C[i][j] = (C[i][j] + MOD * MOD) % MOD;
        }
    }

    Polynomial A(k);
    Polynomial B(k);

    for (int i = 0; i < A.size(); ++i) {
        if (k - i - 2 < 0) {
            break;
        }
        A.c[i] = C[k - i - 2][i];
        A.c[i] *= (i & 1) ? -1 : 1;
        A.c[i] = (A.c[i] + MOD * MOD) % MOD;
    }

    for (int i = 0; i < B.size(); ++i) {
        if (k - i - 1 < 0) {
            break;
        }
        B.c[i] = C[k - i - 1][i];
        B.c[i] *= (i & 1) ? -1 : 1;
        B.c[i] = (B.c[i] + MOD * MOD) % MOD;
    }

    /*for (auto &i: C) {
        for (auto &j: i) cout << j << " ";
        cout nl;
    }*/

    //cout << A;
    //cout << B;

    Polynomial C = A.div(B, n + 1);

    //cout << C;

    for (int i = 0; i < n; ++i) {
        cout << C[i] nl;
    }
}
