#include <algorithm>
#include <iostream>
#include <vector>
#include <cmath>
#include <set>

using namespace std;

#define sp << ' ' <<
#define nl << '\n'
#define sq << ' '

void solve();

int main() {
    freopen("nextperm.in", "r", stdin);
    freopen("nextperm.out", "w", stdout);
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    solve();
}

int n;
vector<int> a;

void print() {
    for (int i = 0; i < n; ++i) {
        cout << a[i] sq;
    }
    cout nl;
}

void solve() {
    cin >> n;
    a.resize(n);
    for (auto &i: a) cin >> i;

    if (prev_permutation(a.begin(), a.end())) {
        print();
    } else {
        for (int i = 0; i < n; ++i) {
            cout << 0 sq;
        }
        cout nl;
    }

    next_permutation(a.begin(), a.end());

    if (next_permutation(a.begin(), a.end())) {
        print();
    } else {
        for (int i = 0; i < n; ++i) {
            cout << 0 sq;
        }
        cout nl;
    }
}
