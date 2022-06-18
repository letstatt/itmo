#include <algorithm>
#include <iostream>
#include <vector>

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

vector<vector<int>> g;
vector<int> used;
int n, m;

bool topsort(int v, vector<int>& order) {
    used[v] = 1;
    for (auto &i: g[v]) {
        if (used[i] == 1) {
            return false;
        } else if (used[i] == 0) {
            if (!topsort(i, order)) {
                return false;
            }
        }
    }
    used[v] = 2;
    order.push_back(v);
    return true;
}

void solve() {
    cin >> n >> m;
    g.resize(n);
    used.resize(n);

    for (int i = 0; i < m; ++i) {
        int x, y;
        cin >> x >> y;
        x--, y--;
        g[x].push_back(y);
    }

    vector<int> order;

    for (int i = 0; i < n; ++i) {
        if (!used[i]) {
            if (!topsort(i, order)) {
                cout << -1 nl;
                exit(0);
            }
        }
    }

    reverse(order.begin(), order.end());

    for (auto &i: order) {
        cout << i+1 sq;
    }
    cout nl;
}
