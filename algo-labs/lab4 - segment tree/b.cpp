#pragma GCC optimize("Ofast")
#pragma GCC target("avx,fma")
#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    ios_base::sync_with_stdio(0);
    cout.tie(0);
    solve();
}

const int logn = 131072;

int n, m;
pair<int, int> a[2 * logn];

inline void update(int i) {
    int mn = min(a[i * 2].first, a[i * 2 + 1].first);
    a[i] = {mn, 0};

    if (mn == a[i * 2].first) {
        a[i].second += a[i * 2].second;
    }

    if (mn == a[i * 2 + 1].first) {
        a[i].second += a[i * 2 + 1].second;
    }
}

void solve() {
    cin >> n >> m;

    for (int i = 0; i < n; ++i) {
        cin >> a[logn + i].first;
        a[logn + i].second = 1;
    }

    for (int i = logn - 1; i > 0; --i) {
        update(i);
    }

    int x, l, r;

    while (m--) {
        cin >> x >> l >> r;

        if (x == 1) {
            l += logn;
            a[l] = {r, 1};

            do {
                l /= 2;
                update(l);

            } while (l > 0);

        } else {
            pair<int, int> res = {1e9, 0};
            l += logn;
            r += logn - 1;

            while (l <= r) {
                if (l & 1) {
                    if (a[l].first < res.first) {
                        res = a[l];
                    } else if (a[l].first == res.first) {
                        res.second += a[l].second;
                    }
                    l += 1;
                }
                if (!(r & 1)) {
                    if (a[r].first < res.first) {
                        res = a[r];
                    } else if (a[r].first == res.first) {
                        res.second += a[r].second;
                    }
                    r -= 1;
                }
                l >>= 1;
                r >>= 1;
            }

            cout << res.first sp res.second nl;
        }
    }
}
