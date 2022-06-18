#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <unordered_map>
#include <unordered_set>
#include <cassert>
#include <queue>
#include <set>
#include <map>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
#ifndef LOCAL
    //freopen("input.txt", "r", stdin);
    //freopen("output.txt", "w", stdout);
#endif
    //ios_base::sync_with_stdio(0);
    //cin.tie(0);
    solve();
}

int n, logn;
vector<vector<int>> p;

void solve() {
    cin >> n;
    logn = ceil(log2(n) + 5);
    p.resize(n + 1, vector<int>(logn));

    for (int i = 1; i <= n; ++i) {
        cin >> p[i][0];
    }

    for (int j = 1; j < logn; ++j) {
        for (int i = 1; i <= n; ++i) {
            p[i][j] = p[p[i][j - 1]][j - 1];
        }
    }

    for (int i = 1; i <= n; ++i) {
        cout << i << ": ";
        for (int j = 0; j < logn; ++j) {
            if (p[i][j] == 0) break;
            cout << p[i][j] << " ";
        }
        cout nl;
    }
}