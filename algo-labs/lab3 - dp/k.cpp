#include <algorithm>
#include <iostream>
#include <numeric>
#include <vector>
#include <cmath>

using namespace std;

typedef long long ll;
typedef long double ld;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    /*ios_base::sync_with_stdio(0);
    cin.tie(0);*/
    solve();
}

int n, s;
int used = 0;
vector<int> a;
vector<vector<int>> ans;

void solve() {
    cin >> n >> s;
    a.resize(n);

    for (auto &i: a) cin >> i;

    int m = (1 << n) - 1;

    while (used != m) {
        int res = 0;
        int mask = 0;

        for (int i = 0; i <= m; ++i) {
            if ((used & i) == 0) {
                int b = 0;
                for (int t = 0; t < n; ++t) {
                    if (i & (1 << t)) {
                        b += a[t];
                        if (b > s) break;
                    }
                }

                if (b <= s && b > res) {
                    res = b;
                    mask = i;
                }
            }
        }

        ans.push_back({});

        for (int i = 0; i < n; ++i) {
            if (mask & (1 << i)) {
                ans.back().push_back(i + 1);
            }
        }

        used |= mask;
    }

    cout << ans.size() nl;

    for (auto &i: ans) {
        cout << i.size() sq;
        for (auto &j: i) cout << j sq;
        cout nl;
    }
}
