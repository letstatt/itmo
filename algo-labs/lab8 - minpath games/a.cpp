#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

typedef long long ll;

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);*/
    solve();
}

vector<vector<int>> g;
int n;

void solve() {
    cin >> n;
    g.resize(n, vector<int>(n));

    for (auto &i: g) {
        for (auto &j: i) {
            cin >> j;
        }
    }

    for (int k = 0; k < n; ++k) {
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) {
                g[i][j] = min(g[i][j], g[i][k] + g[k][j]);
            }
        }
    }

    for (auto &i: g) {
        for (auto &j: i) {
            cout << j sq;
        }
        cout nl;
    }
    cout nl;
}
