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

int n, k;
vector<int> a;

int lower_bound(int x) {
    int l = -1, r = n;

    while (r - l > 1) {
        int m = (l + r) / 2;
        if (a[m] < x) l = m;
        else r = m;
    }

    return r;
}

int upper_bound(int x) {
    int l = -1, r = n;

    while (r - l > 1) {
        int m = (l + r) / 2;
        if (a[m] <= x) l = m;
        else r = m;
    }

    return r;
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> n;
    a = vector<int>(n);
    for (auto &i: a) cin >> i;

    sort(a.begin(), a.end());

    int l, r;
    cin >> k;

    //for (auto &i: a) cout <<i sq;
    //cout nl;

    while (k--) {
        cin >> l >> r;
        //cout << upper_bound(r) sp lower_bound(l) nl;
        cout << upper_bound(r) - lower_bound(l) sq;
    }
}
