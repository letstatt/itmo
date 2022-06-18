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
    /*ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);*/
    solve();
}

const ll MOD = 104857601;

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

ll n, k;

// check this
// neerc.ifmo.ru/wiki/index.php?title=%D0%91%D1%8B%D1%81%D1%82%D1%80%D0%BE%D0%B5_%D0%B2%D1%8B%D1%87%D0%B8%D1%81%D0%BB%D0%B5%D0%BD%D0%B8%D0%B5_%D1%87%D0%BB%D0%B5%D0%BD%D0%BE%D0%B2_%D0%BB%D0%B8%D0%BD%D0%B5%D0%B9%D0%BD%D0%BE%D0%B9_%D1%80%D0%B5%D0%BA%D1%83%D1%80%D1%80%D0%B5%D0%BD%D1%82%D0%BD%D0%BE%D0%B9_%D0%BF%D0%BE%D1%81%D0%BB%D0%B5%D0%B4%D0%BE%D0%B2%D0%B0%D1%82%D0%B5%D0%BB%D1%8C%D0%BD%D0%BE%D1%81%D1%82%D0%B8

void solve() {
    cin >> k >> n;
    --n;

    Polynomial A(2*k - 1);
    for (int i = 0; i < k; ++i) cin >> A.c[i];

    Polynomial Q(k);
    Q.c[0] = 1;

    for (int i = 1; i <= k; ++i) {
        cin >> Q.c[i];
        Q.c[i] = (MOD * MOD - Q[i]) % MOD;
    }

    while (n >= k) {
        // мое деление тут почему-то ****ь не работает
        for (int i = k; i < 2 * k; ++i) {
            A.c[i] = 0;
            for (int j = 1; j <= k; ++j) {
                A.c[i] += (-Q[j] * A[i - j]) % MOD;
            }
            A.c[i] = (A[i] + MOD*MOD) % MOD;
        }

        for (int i = 0, j = 0; i < 2*k; ++i) {
            if ((i & 1) == (n & 1)) {
                A.c[j++] = A[i];
            }
        }

        Polynomial negQ(Q);
        for (int i = 1; i <= k; ++i) {
            if (i & 1) {
                negQ.c[i] = (MOD * MOD - negQ[i]) % MOD;
            }
        }

        Polynomial R = Q.multiply(negQ);
        for (int i = 0; i < 2*k + 1; i += 2) {
            Q.c[i / 2] = R[i];
        }

        n /= 2;
    }

    cout << (A[n] + MOD * MOD) % MOD << endl;
}
