#include <algorithm>
#include <iostream>
#include <numeric>
#include <vector>
#include <cmath>

using namespace std;

typedef long long ll;
typedef long double ld;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

int n, m;
vector<vector<ll>> d;

inline bool match(int j, int k) {
    int s = (j & 1) + (k & 1);
    for (int i = 1; i < n; ++i) {
        s += ((j >> i) & 1);
        s += ((k >> i) & 1);
        if (s == 0 || s == 4) return false;
        s -= ((j >> (i - 1)) & 1);
        s -= ((k >> (i - 1)) & 1);
    }
    return true;
}

void solve() {
    cin >> n >> m;
    if (m < n) swap(n, m);

    d.resize(m, vector<ll>(1 << n));

    for (auto &i: d[0]) i = 1;

    for (int i = 0, r = 1 << n; i < m - 1; ++i) {
        for (int j = 0; j < r; ++j) {
            for (int k = 0; k < r; ++k) {
                if (match(j, k)) {
                    d[i + 1][k] += d[i][j];
                }
            }
        }
    }

    cout << accumulate(d[m - 1].begin(), d[m - 1].end(), 0ll) nl;
}
