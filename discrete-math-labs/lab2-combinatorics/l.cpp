#pragma GCC optimize("O3,unroll-loops")
#pragma GCC target("avx2")

#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <set>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    //freopen("part2sets.in", "r", stdin);
    //freopen("part2sets.out", "w", stdout);
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    solve();
}

int n, k;
vector<int> u;
vector<vector<int>> b;
bool f = false;

void print() {
    for (int i = 0; i < k; ++i) {
        for (int j = 0; j < b[i].size(); ++j) {
            cout << b[i][j] sq;
        }
        cout nl;
    }
    cout nl;
}

void rec2(int i, int j, int last) {
    if (i == k) {
        print();
        return;
    }

    for (int p = last + 1; p <= n; ++p) {
        if (f && i == 0 && j == 0 && p >= n / 2) {
            break;
        }

        if (u[p]) continue;
        b[i][j] = p;
        u[p] = true;

        if (j == b[i].size() - 1) {
            rec2(i + 1, 0, 0);
        } else {
            rec2(i, j + 1, p);
        }

        b[i][j] = 0;
        u[p] = false;
    }
}

void rec(int i, int s, int last) {
    if (i >= k) {
        if (s == 0) {
            f = true;
            for (int j = 1; j < k; ++j) {
                f &= (b[j].size() == b[0].size());
            }
            rec2(0, 0, 0);
        }
        return;
    }

    for (int j = last; j <= s - (last * (k - i - 1)); ++j) {
        b[i].resize(j);
        rec(i + 1, s - j, j);
    }
}

void solve() {
    cin >> n >> k;
    u.resize(n + 1);
    b.resize(k, vector<int>(n));
    rec(0, n, 1);
}
