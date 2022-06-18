#pragma GCC target("avx2")
#pragma GCC optimization("O3")
#pragma GCC optimization("unroll-loops")

#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

typedef long long ll;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

int n, k, x;
vector<int> a;

inline int lower_bound(int x) {
    int l = -1, r = n;

    while (r - l > 1) {
        int m = (l + r) / 2;
        if (a[m] < x) l = m;
        else r = m;
    }

    return r;
}

inline bool f(int l, int m, int r) {
    return abs(a[l] - x) <= abs(min(a[m], a[r]) - x);
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> n >> k;
    a = vector<int>(n);
    for (auto &i: a) cin >> i;

    while (k--) {
        cin >> x;
        //cout << lower_bound(x) nl;

        int l = max(lower_bound(x) - 1, 0);
        int m = lower_bound(x);
        int r = min(lower_bound(x), n - 1);

        if (f(l, m, r)) {
            cout << a[l] nl;
        } else if (f(r, m, l)) {
            cout << a[r] nl;
        } else {
            cout << a[m] nl;
        }
    }
}
