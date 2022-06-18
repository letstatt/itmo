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
    freopen("subsets.in", "r", stdin);
    freopen("subsets.out", "w", stdout);
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    solve();
}

int n;
vector<int> a;

void print(int d) {
    for (int i = 0; i < d; ++i) {
        cout << a[i] sq;
    }
    cout nl;
}

void rec(int i, int last) {
    print(i);

    for (int j = last + 1; j <= n; ++j) {
        a[i] = j;
        rec(i + 1, j);
    }
}

void solve() {
    cin >> n;
    a.resize(n);
    rec(0, 0);
}
