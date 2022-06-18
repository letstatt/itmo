#pragma GCC target("avx2")
#pragma GCC optimization("O3")
#pragma GCC optimization("unroll-loops")

#include <algorithm>
#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>

using namespace std;

typedef long long ll;
typedef long double ld;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

int n, k;
vector<ll> a;

bool f(ll x) {
    int j = 1;
    ll s = 0;

    for (auto &i: a) {
        if (i > x) return false;

        if (s + i > x) {
            if (s == 0) return false;
            j += 1;
            s = i;
        } else {
            s += i;
        }
    }

    return (j <= k);
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> n >> k;

    a = vector<ll>(n);
    for (auto &i: a) cin >> i;

    //sort(a.rbegin(), a.rend());

    ll l = 0, r = 1e15;

    while (r - l > 1) {
        ll m = (l + r) / 2;
        (f(m) ? r : l) = m;
    }

    cout << r nl;
}
