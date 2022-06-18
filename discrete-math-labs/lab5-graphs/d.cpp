#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cout.tie();
    cin.tie();*/
    solve();
}

vector<vector<int>> g, g2;
vector<int> used, order;
int n;

void topsort(int i) {
    used[i] = 1;
    for (auto &j: g[i]) {
        if (!used[j]) {
            topsort(j);
        }
    }
    order.push_back(i);
}

void solve() {
    cin >> n;
    g.resize(n);
    g2.resize(n, vector<int>(n));
    used.resize(n, 0);

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < i; ++j) {
            char x;
            cin >> x;
            if (x == '1') {
                g[i].push_back(j);
                g2[i][j] = 1;
            } else {
                g[j].push_back(i);
                g2[j][i] = 1;
            }
        }
    }

    for (int i = 0; i < n; ++i) {
        fill(used.begin(), used.end(), 0);
        order.clear();
        topsort(i);
        reverse(order.begin(), order.end());

        if (!g2[order[n - 1]][order[0]]) {
            continue;
        }

        bool success = true;

        for (int j = 1; j < n; ++j) {
            if (!g2[order[j - 1]][order[j]]) {
                success = false;
                break;
            }
        }

        if (success) {
            break;
        }
    }

    for (auto &i: order) {
        cout << i+1 << " ";
    }
    cout << endl;
}

