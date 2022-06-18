#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
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

char x;
int n, m, a, b, c = 0;
vector<vector<int>> g;
vector<int> used;
vector<int> matching;

bool kuhn(int v) {
    if (used[v]) {
        return false;
    }

    used[v] = true;

    for (auto &i: g[v]) {
        if (matching[i] == -1 || kuhn(matching[i])) {
            matching[i] = v;
            return true;
        }
    }

    return false;
}

void solve() {
    cin >> n >> m >> a >> b;

    g.resize(n * m);
    used.resize(n * m);
    matching.resize(n * m, -1);

    vector<vector<int>> board(n, vector<int>(m));

    for (auto &i: board) {
        for (auto &j: i) {
            if ((cin >> x), x == '*') {
                j = 1;
                c++;
            }
        }
    }

    if (a >= 2 * b) {
        // it's easy fact, try to prove it by yourself
        cout << c * b nl;
        return;
    }

    // build bipartite graph.
    // the board is black and white by sum of coords

    vector<pair<int, int>> shifts = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (board[i][j] && ((i + j) % 2) == 0) {
                for (auto [di, dj]: shifts) {
                    int x = i + di;
                    int y = j + dj;
                    if (0 <= x && x < n && 0 <= y && y < m && board[x][y]) {
                        g[i * m + j].push_back(x * m + y);
                    }
                }
            }
        }
    }

    for (int i = 0; i < g.size(); ++i) {
        if (kuhn(i)) {
            fill(used.begin(), used.end(), 0);
        }
    }

    ll ans = 0;

    for (auto &i: matching) {
        ans += (i != -1);
    }

    cout << (ans * a + (c - 2 * ans) * b) << endl;
}
