#include <algorithm>
#include <iostream>
#include <iomanip>
#include <vector>
#include <cassert>
#include <set>
#include <map>

using namespace std;

typedef long long ll;
typedef long double ld;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);*/
    solve();
}

const ll MOD = 998244353;

struct polynome {
    vector<ll> c;
    size_t deg = 0;

    polynome() : deg(1), c(1, 1) {}

    polynome(size_t deg) : deg(deg + 1) {
        c.resize(deg + 1);
    }

    polynome(vector<ll>&& c, bool norm = true) : c(move(c)) {
        if (norm) {
            normalize();
        }
    }

    void normalize() {
        deg = 0;
        for (size_t i = 0; i < c.size(); ++i) {
            if (c[i] != 0) {
                deg = i + 1;
            }
        }
        c.resize(deg);
    }

    size_t d() const {
        return deg ? deg - 1 : 0;
    }

    ll operator[] (size_t i) const {
        return (i < c.size() && i >= 0 ? c[i] : 0);
    }

    polynome add(polynome const& o) const {
        vector<ll> p(max(deg, o.deg));
        for (size_t i = 0; i < deg; ++i) {
            p[i] += c[i];
        }
        for (size_t i = 0; i < o.deg; ++i) {
            p[i] += o[i];
            p[i] %= MOD;
        }
        return polynome(move(p));
    }

    polynome multiply(polynome const& o) const {
        vector<ll> p(deg + o.deg);
        for (size_t i = 0; i < p.size(); ++i) {
            for (size_t j = 0; j <= i; ++j) {
                if (i - j >= o.deg) continue;
                p[i] += ((*this)[j] * o[i - j]) % MOD;
                p[i] %= MOD;
            }
        }
        return polynome(move(p));
    }

    polynome div(polynome const& o, size_t d) const {
        assert(o[0] == 1);
        vector<ll> p(d);

        for (size_t i = 0; i < d; ++i) {
            p[i] = (*this)[i];
            if (i == 0) continue;
            for (size_t j = 0; j <= i - 1; ++j) {
                p[i] -= (p[j] * o[i - j]) % MOD;
                p[i] = (p[i] + MOD * MOD) % MOD;
            }
        }
        return polynome(move(p), false);
    }

    polynome mod(size_t n) const {
        vector<ll> p(c.begin(), c.begin() + min(deg, n));
        return polynome(move(p));
    }

    friend ostream& operator << (ostream& stream, polynome const& p) {
        for (auto i: p.c) {
            stream << i sq;
        }
        stream nl;
        return stream;
    }
};

ll revmod (ll a) {
    ll n = MOD - 2;
    ll res = 1;
    while (n) {
        if (n & 1) {
            res *= a;
            res %= MOD;
        }
        a *= a;
        a %= MOD;
        n >>= 1;
    }
    return res;
}

int n, m;

void solve() {
    cin >> n >> m;

    polynome p(n);
    for (auto &i: p.c) cin >> i;

    vector<polynome> powers_of_p(m);

    for (int i = 1; i < m; ++i) {
        powers_of_p[i] = powers_of_p[i-1].multiply(p);
    }

    // sqrt(1 + x) = 1 + x/2 - x^2/8 + x^3/16 ... + (-1)^n*(2n)!*x^n/(1-2n)/(n!)^2/4^n

    polynome sqrt_series(m - 1);
    sqrt_series.c[0] = 1;

    for (ll i = 1; i < m; ++i) {
        ll & c = sqrt_series.c[i];
        c = -sqrt_series.c[i - 1];
        c = (c * (1 - 2 * (i - 1))) % MOD;
        c = (c * 2 * i * (2 * i - 1)) % MOD;
        c = (c * revmod((1 - 2 * i) * i * i * 4)) % MOD;
        c = (c + MOD * MOD) % MOD;
    }

    for (int i = 0; i < m; ++i) {
        ll ans = 0;
        for (int j = 0; j < m; ++j) {
            ans += (powers_of_p[j][i] * sqrt_series[j]) % MOD;
            ans %= MOD;
        }
        cout << (ans + MOD * MOD) % MOD sq;
    }
    cout nl;

    // exp = 1 + x/1 + x^2/2 + x^3/6 + ... + x^n/n!

    polynome e_series(m - 1);
    e_series.c[0] = 1;

    for (int i = 1; i < m; ++i) {
        ll & c = e_series.c[i];
        c = e_series[i - 1];
        c *= revmod(i);
        c %= MOD;
    }

    for (int i = 0; i < m; ++i) {
        ll ans = 0;
        for (int j = 0; j < m; ++j) {
            ans += (powers_of_p[j][i] * e_series[j]) % MOD;
            ans %= MOD;
        }
        cout << (ans + MOD * MOD) % MOD sq;
    }
    cout nl;

    // ln(1 + x) = x - x^2/2 + x^3/3 + ... + (-1)^(n-1)*x^n/n

    polynome ln_series(m - 1);
    ln_series.c[0] = 0;

    for (int i = 1; i < m; ++i) {
        ll & c = ln_series.c[i];
        c = (i & 1 ? 1 : -1);
        c = (c * revmod(i)) % MOD;
    }

    for (int i = 0; i < m; ++i) {
        ll ans = 0;
        for (int j = 0; j < m; ++j) {
            ans += (powers_of_p[j][i] * ln_series[j]) % MOD;
            ans %= MOD;
        }
        cout << (ans + MOD * MOD) % MOD sq;
    }
    cout nl;


}
