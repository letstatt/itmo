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

int n, m, logn;
vector<int> t;

void update(int i) {
    i += logn;
    t[i] = !t[i];
    i /= 2;

    while (i > 0) {
        t[i] = t[2 * i] + t[2 * i + 1];
        i /= 2;
    }
}

void solve() {
    cin >> n >> m;
    logn = 1 << int(log2(n - 1) + 1);

    t.resize(2 * logn);

    for (int i = 0, x; i < n; ++i) {
        cin >> x;
        t[logn + i] = x;
    }

    for (int i = logn - 1; i > 0; --i) {
        t[i] = t[2 * i] + t[2 * i + 1];
    }

    int q, i;

    while (m--) {
        cin >> q >> i;

        if (q == 1) {
            update(i);

        } else {
            int v = 1;
            i += 1;

            while (v < logn) {
                if (t[2 * v] >= i) {
                    v = v * 2;
                } else {
                    i -= t[2 * v];
                    v = 2 * v + 1;
                }
            }

            cout << (v - logn) nl;
        }
    }
}
