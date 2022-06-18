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
vector<vector<int>> d;
vector<vector<pair<int, int>>> p;

void solve() {
    cin >> n >> m;
    d.resize(n, vector<int>(m));
    p.resize(n, vector<pair<int, int>>(m));

    for (auto &i: d) {
        for (auto &j: i) cin >> j;
    }

    p[0][0] = {-1, -1};

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (i == 0 && j == 0) continue;

            if ((i > 0 ? d[i - 1][j] : -1e9) > (j > 0 ? d[i][j - 1] : -1e9)) {
                d[i][j] += d[i - 1][j];
                p[i][j] = {i - 1, j};
            } else {
                d[i][j] += d[i][j - 1];
                p[i][j] = {i, j - 1};
            }
        }
    }

    vector<char> path;
    int i = n - 1, j = m - 1;

    while (i != -1) {
        //path.push_back(v + 1);
        int x = p[i][j].first;
        int y = p[i][j].second;
        if (x != -1) {
            path.push_back(x != i ? 'D' : 'R');
        }
        i = x, j = y;
    }

    reverse(path.begin(), path.end());

    cout << d[n - 1][m - 1] nl;
    for (auto &i: path) cout << i;
    cout nl;
}
