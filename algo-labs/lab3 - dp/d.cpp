#include <algorithm>
#include <iostream>
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

const int MOD = 1e9;

int n;
vector<vector<ll>> d;

inline void calc(int i, int j, int k) {
    if (i == 2 && (k == 8 || k == 0)) return;
    d[i][j] += d[i - 1][k];
    d[i][j] %= MOD;
}

void solve() {
    cin >> n;
    d.resize(n + 1, vector<ll>(10));

    for (int i = 1; i < 8; ++i) {
        d[1][i] = 1;
    }

    d[1][9] = 1;

    for (int i = 2; i <= n; ++i) {
        calc(i, 0, 4); // to, where from?
        calc(i, 0, 6);

        calc(i, 1, 6);
        calc(i, 1, 8);

        calc(i, 2, 7);
        calc(i, 2, 9);

        calc(i, 3, 8);
        calc(i, 3, 4);

        calc(i, 4, 0);
        calc(i, 4, 3);
        calc(i, 4, 9);

        calc(i, 6, 0);
        calc(i, 6, 1);
        calc(i, 6, 7);

        calc(i, 7, 2);
        calc(i, 7, 6);

        calc(i, 8, 1);
        calc(i, 8, 3);

        calc(i, 9, 2);
        calc(i, 9, 4);
    }

    ll ans = 0;
    for (auto &i: d.back()) ans = (ans + i) % MOD;

    cout << ans nl;
}
