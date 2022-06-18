#include <unordered_set>
#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

#define nl << '\n'

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cout.tie(0);
    cin.cie(0);*/
    solve();
}

vector<vector<int>> g, gr;
vector<unordered_set<int>> used_numbers;
vector<int> used, order, mex;
int n, m;

void topsort(int v) {
    used[v] = 1;
    for (auto &i: g[v]) {
        if (!used[i]) {
            topsort(i);
        }
    }
    order.push_back(v);
}

void solve() {
    cin >> n >> m;

    g.resize(n);
    gr.resize(n);
    mex.resize(n);
    used.resize(n);
    used_numbers.resize(n);

    for (int i = 0; i < m; ++i) {
        int x, y;
        cin >> x >> y;
        x--, y--;
        g[x].push_back(y);
        gr[y].push_back(x);
    }

    for (int i = 0; i < n; ++i) {
        if (!used[i]) {
            topsort(i);
        }
    }

    for (auto &i: order) {
        for (int j = 0; ; ++j) {
            if (!used_numbers[i].count(j)) {
                mex[i] = j;
                for (auto &v: gr[i]) {
                    used_numbers[v].insert(mex[i]);
                }
                break;
            }
        }
    }

    for (auto &i: mex) {
        cout << i nl;
    }
}
