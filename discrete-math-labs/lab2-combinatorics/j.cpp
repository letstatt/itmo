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
    freopen("partition.in", "r", stdin);
    freopen("partition.out", "w", stdout);
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    solve();
}

int n;
vector<int> a;

void print(int d) {
    cout << a[0];
    for (int i = 1; i < d; ++i) {
        cout << '+' << a[i];
    }
    cout nl;
}

void rec(int i, int s, int last) {
    if (s == 0) {
        print(i);
        return;
    }
    for (int j = last; j <= s; ++j) {
        a[i] = j;
        rec(i + 1, s - j, j);
    }

    a[i] = 0;
}

void solve() {
    cin >> n;
    a.resize(n);
    rec(0, n, 1);
}
