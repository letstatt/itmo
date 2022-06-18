#include <algorithm>
#include <iostream>
#include <vector>
#include <set>

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

__int128 INF = 1;

vector<vector<pair<int, __int128>>> g;
vector<int> used;
int n, m, s;

void dfs(int i) {
    used[i] = 1;

    for (auto [j, w]: g[i]) {
        if (!used[j]) {
            dfs(j);
        }
    }
}

void print(__int128 x) {
    if (x < 0) {
        cout << '-';
        x = -x;
    }
    if (x > 9) {
        print(x / 10);
    }
    cout << char(char(x % 10) + '0');
}

void solve() {
    cin >> n >> m >> s;
    g.resize(n);
    used.resize(n);
    s--;

    for (int k = 0; k < 20; ++k) {
        INF = INF * (__int128) 10;
    }

    for (int i = 0; i < m; ++i) {
        int x, y;
        ll w;
        cin >> x >> y >> w;
        x--, y--;
        g[x].push_back({y, w});
    }

    vector<__int128> d(n, INF);
    d[s] = 0;



    for (int k = 0; k < n; ++k) {
        for (int i = 0; i < n; ++i) {
            if (d[i] != INF) {
                for (auto [j, w]: g[i]) {
                    if (d[j] > d[i] + w) {
                        d[j] = max(-INF, d[i] + w);
                    }
                }
            }
        }
    }


    for (int i = 0; i < n; ++i) {
        if (d[i] != INF) {
            for (auto [j, w]: g[i]) {
                if (d[j] > d[i] + w) {
                    if (!used[i]) dfs(i);
                }
            }
        }
    }

    for (int i = 0; i < n; ++i) {
        if (used[i]) {
            cout << '-' nl;
        } else if (d[i] == INF) {
            cout << '*' nl;
        } else {
            print(d[i]);
            cout nl;
        }
    }
}
