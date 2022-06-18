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
vector<vector<int>> a;
vector<vector<vector<ll>>> d;

void solve() {
    cin >> n >> m;
    swap(n, m);

    a.resize(m, vector<int>(n));
    d.resize(m + 1, vector<vector<ll>>(n + 1, vector<ll>(1 << n)));
    d[0][0][0] = 1;

    for (auto &i: a) {
        char x;
        for (auto &j: i) (cin >> x), j = (x == '.');
    }

    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            for (int p = 0, t = 1 << n; p < t; ++p) {
                if (!a[i][j] && (p & (1 << j)) == 0) {
                    d[i][j + 1][p] += d[i][j][p];
                    continue; // € не знаю, почему это работает. по интуиции написал.
                }

                if ((p & (1 << j)) && a[i][j]) {
                    int p2 = p - (1 << j);
                    d[i][j + 1][p2] += d[i][j][p];

                } else if (a[i][j]) {
                    int p2 = p + (1 << j);
                    d[i][j + 1][p2] += d[i][j][p];

                    if (j < n - 1 && (p & (1 << (j + 1))) == 0 && a[i][j + 1]) {
                        int p2 = p + (1 << (j + 1));
                        d[i][j + 1][p2] += d[i][j][p];
                    }
                }
            }
        }

        for (int p = 0, t = 1 << n; p < t; ++p) {
            d[i + 1][0][p] = d[i][n][p];
        }
    }

    /*for (int k = 0; k <= m; ++k) {
        cout << "i" sp k nl;
        for (int i = 0; i <= n; ++i) {
            for (auto &j: d[k][i]) cout << j sq;
            cout nl;
        }
    }*/

    cout << d[m][0][0] nl;
}
