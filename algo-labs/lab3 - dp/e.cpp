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

int n, m;
string s, t;
vector<vector<ll>> d;

void solve() {
    cin >> s >> t;
    n = s.size(), m = t.size();

    d.resize(n + 1, vector<ll>(m + 1, 1e9));

    for (int i = 0; i <= n; ++i) {
        d[i][0] = i;
    }

    for (int i = 0; i <= m; ++i) {
        d[0][i] = i;
    }

    for (int i = 1; i <= n; ++i) {
        for (int j = 1; j <= m; ++j) {
            if (s[i - 1] == t[j - 1]) {
                d[i][j] = min(d[i][j], d[i - 1][j - 1]);
            }

            d[i][j] = min(d[i][j], d[i - 1][j - 1] + 1);
            d[i][j] = min(d[i][j], d[i - 1][j] + 1);
            d[i][j] = min(d[i][j], d[i][j - 1] + 1);
        }
    }

    cout << d[n][m] nl;
}
