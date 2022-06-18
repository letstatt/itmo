#pragma GCC optimize("Ofast")
#pragma GCC target("avx,fma")
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

ll a[2 * 131072];
int n, m, logn = 131072;

void solve() {
    cin >> n >> m;

    for (int i = 0; i < n; ++i) {
        cin >> a[logn + i];
    }

    for (int i = logn - 1; i > 0; --i) {
        a[i] = a[i * 2] + a[i * 2 + 1];
    }

    int x, l, r;

    while (m--) {
        cin >> x >> l >> r;

        if (x == 1) {
            l += logn;
            a[l] = r;

            do {
                l /= 2;
                a[l] = a[2 * l] + a[2 * l + 1];

            } while (l > 0);

        } else {
            ll sum = 0;
            l += logn;
            r += logn - 1;

            while (l <= r) {
                if (l & 1) {
                    sum += a[l++];
                }
                if (!(r & 1)) {
                    sum += a[r--];
                }
                l >>= 1;
                r >>= 1;
            }

            cout << sum nl;
        }
    }
}
