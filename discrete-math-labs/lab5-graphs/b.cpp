#include <algorithm>
#include <iostream>
#include <numeric>
#include <vector>
#include <deque>
#include <random>

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
int n;

inline bool check(deque<int>& a) {
    if (!g[a.back()][a.front()]) {
        return false;
    }

    for (int i = 1; i < n; ++i) {
        if (!g[a[i - 1]][a[i]]) {
            return false;
        }
    }
    return true;
}

void solve() {
    cin >> n;
    g.resize(n, vector<int>(n));

    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < i; ++j) {
            char x;
            cin >> x;
            g[i][j] = g[j][i] = x - '0';
        }
    }

    deque<int> a(n);
    iota(a.begin(), a.end(), 0);

    while (true) {
        for (int k = 0; k < n * (n - 1); ++k) {
            if (!g[a[0]][a[1]]) {
                int i = 2;
                while (i + 1 < n && (!g[a[0]][a[i]] || !g[a[1]][a[i + 1]])) ++i;
                if (i == n - 1) {
                    break;
                }
                reverse(a.begin() + 1, a.begin() + i + 1);
            }
            a.push_back(a.front());
            a.pop_front();
        }

        if (check(a)) {
            break;
        }

        random_shuffle(a.begin(), a.end());
    }

    for (auto &i: a) {
        cout << i+1 sq;
    }
    cout nl;
}
