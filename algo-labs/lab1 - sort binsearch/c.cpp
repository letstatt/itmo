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

int n;
ll c = 0;
vector<int> a;

void mergesort(int l, int r) {
    if (r - l <= 1) {
        return;
    }

    int m = (l + r) / 2;
    mergesort(l, m);
    mergesort(m, r);

    vector<int> b(r - l);
    //inplace_merge(a.begin() + l, a.begin() + m, a.begin() + r);

    int i = l;
    int j = m;
    int k = 0;

    while (i < m || j < r) {
        if (j == r || i < m && a[i] <= a[j]) {
            b[k++] = a[i++];
        } else {
            c += (m - i);
            b[k++] = a[j++];
        }
    }

    for (int i = 0; i < k; ++i) {
        a[l + i] = b[i];
    }
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> n;
    a = vector<int>(n);
    for (auto &i: a) cin >> i;
    mergesort(0, n);

    cout << c nl;
}
