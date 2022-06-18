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

int n;
vector<int> a;
vector<vector<ll>> d;
vector<vector<pair<int, int>>> p;

void solve() {
    cin >> n;
    a.resize(n + 1);
    d.resize(n + 1, vector<ll>(n + 1, 1e10));
    p.resize(n + 1, vector<pair<int, int>>(n + 1, {-1, -1}));

    for (int i = 1; i <= n; ++i) {
        cin >> a[i];
    }

    d[0][0] = 0;

    for (int i = 1; i <= n; ++i) {
        for (int j = 0; j <= n; ++j) {
            if (j != n && d[i][j] > d[i - 1][j + 1]) {
                d[i][j] = d[i - 1][j + 1];
                p[i][j] = {i - 1, j + 1};
            }
            if (a[i] <= 100) {
                if (d[i - 1][j] + a[i] < d[i][j]) {
                    d[i][j] = d[i - 1][j] + a[i];
                    p[i][j] = {i - 1, j};
                }
            } else if (j > 0) {
                if (d[i - 1][j - 1] + a[i] < d[i][j]) {
                    d[i][j] = d[i - 1][j - 1] + a[i];
                    p[i][j] = {i - 1, j - 1};
                }
            }
        }
    }

    vector<int> path;
    int i = n, j = 0, r;
    ll best = 1e10;

    for (int k = 0; k <= n; ++k) {
        if (d[n][k] <= best) {
            best = d[n][k];
            j = r = k;
        }
    }

    while (i != -1) {
        int x = p[i][j].first;
        int y = p[i][j].second;
        if (y > j) {
            path.push_back(i);
        }
        i = x, j = y;
    }

    reverse(path.begin(), path.end());

    cout << best nl;
    cout << r sp path.size() nl;
    for (auto &i: path) cout << i nl;
    cout nl;
}
