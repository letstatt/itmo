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
    cin.tie(0);*/
    solve();
}

vector<vector<pair<int, int>>> g;
vector<int> grundy;
int n, r;

void dfs(int v, int p) {
    for (auto &i: g[v]) {
        int j = i.first;
        if (j != p) {
            dfs(j, v);
            grundy[v] ^= (grundy[j] + 1);
        }
    }
}

void dfs2(int v, int p, int doubt) {
    for (auto &i: g[v]) {
        int j = i.first;
        if (j != p) {
            int doubt_if_remove_in_that_subtree = grundy[v] ^ (grundy[j] + 1);
            if (doubt == doubt_if_remove_in_that_subtree) {
                cout << 1 nl << i.second nl;
                exit(0);
            }
            int new_doubt = doubt ^ doubt_if_remove_in_that_subtree;
            dfs2(j, v, new_doubt - 1);
        }
    }
}

void solve() {
    cin >> n >> r;

    g.resize(n);
    grundy.resize(n);

    for (int i = 1; i < n; ++i) {
        int x, y;
        cin >> x >> y;
        x--, y--;
        g[x].push_back({y, i});
        g[y].push_back({x, i});
    }

    dfs(r-1, -1);

    if (!grundy[r-1]) {
        cout << 2 nl;
        return;
    }

    dfs2(r-1, -1, 0);
}
