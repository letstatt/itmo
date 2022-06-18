#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

int n;
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

    for (auto &i: a) cout << i sq;
}
