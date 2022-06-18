#include <algorithm>
#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.tie(0);*/
    solve();
}

vector<pair<int, int>> coords;
vector<int> min_edge, selected_edge;
vector<int> used;
int n;

ll distsqr(int i, int j) {
    ll a = (coords[i].first - coords[j].first);
    ll b = (coords[i].second - coords[j].second);
    return a * a + b * b;
}

void solve() {
    cin >> n;
    selected_edge.resize(n, -1);
    min_edge.resize(n, 1e9);
    coords.resize(n);
    used.resize(n);

    for (int i = 0; i < n; ++i) {
        int x, y;
        cin >> x >> y;
        x--, y--;
        coords[i] = {x, y};
    }

    min_edge[0] = 0;
    long double ans = 0;

    for (int i = 0; i < n; ++i) {
        int v = -1;
        for (int j = 0; j < n; ++j) {
            if (!used[j] && (v == -1 || min_edge[j] < min_edge[v])) {
                v = j;
            }
        }

        used[v] = true;
        if (selected_edge[v] != -1) {
            ans += sqrtl(distsqr(v, selected_edge[v]));
        }

        for (int u = 0; u < n; ++u) {
            if (distsqr(v, u) < min_edge[u]) {
                min_edge[u] = distsqr(v, u);
                selected_edge[u] = v;
            }
        }
    }

    cout << setprecision(10) << fixed << ans nl;
}
