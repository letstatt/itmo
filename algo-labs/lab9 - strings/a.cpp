#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <set>

using namespace std;

typedef long long ll;
typedef unsigned long long ull;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    solve();
}

const ull mod = 1e9 + 7;
const ull p = 211;

vector<ull> h, pp;
string s;
int n;

ull calc(int i, int j) {
    return (h[j + 1] - (h[i] * pp[j - i + 1]) % mod + mod*mod) % mod;
}

void solve() {
    cin >> s;
    n = s.size();
    pp.resize(n + 1);
    h.resize(n + 1);
    h[0] = 0;

    pp[0] = 1;

    for (int i = 1; i < n; ++i) {
        pp[i] = pp[i - 1] * p;
        pp[i] %= mod;
    }

    for (int i = 0; i < n; ++i) {
        h[i + 1] = (h[i] * p) % mod;
        h[i + 1] = (h[i + 1] + ull(s[i])) % mod;
    }

    int m;
    cin >> m;

    while (m--) {
        int a, b, c, d;
        cin >> a >> b >> c >> d;
        a--, b--, c--, d--;

        ull hash1 = calc(a, b);
        ull hash2 = calc(c, d);
        //cout << hash1 sp hash2 nl;

        if (hash1 == hash2) {
            cout << "Yes" nl;
        } else {
            cout << "No" nl;
        }
    }

}

