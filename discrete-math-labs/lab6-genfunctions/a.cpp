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

    friend ostream& operator << (ostream& stream, polynome const& p) {
        for (auto i: p.c) {
            stream << i sq;
        }
        stream nl;
        return stream;
    }
};

int n, m;

void solve() {
    cin >> n >> m;
    polynome p(n);
    polynome q(m);

    for (auto &i: p.c) cin >> i;
    for (auto &i: q.c) cin >> i;

    polynome a = p.add(q);
    cout << a.d() nl << a;

    polynome b = p.multiply(q);
    cout << b.d() nl << b;

    polynome c = p.div(q, 1000);
    cout << c;


}
